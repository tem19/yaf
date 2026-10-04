#' Id цели из параметров стратегии
#'
#' GoalId лежит в блоке параметров конкретной стратегии
#' (AverageCpa$GoalId, PayForConversion$GoalId и т. д.).
#'
#' @param x Стратегия площадки (BiddingStrategy$Search / $Network)
#'   или объект пакетной стратегии.
#'
#' @return Число или NA, если у стратегии нет цели.
#' @keywords internal
yaf_find_goal_id <- function(x) {
  for (b in x) {
    if (is.list(b) && !is.null(b$GoalId)) return(as.numeric(b$GoalId))
  }
  NA_real_
}

#' Цели, на которых обучается стратегия
#'
#' @param type Тип стратегии (BiddingStrategyType или Type пакетной стратегии).
#' @param goal_id GoalId из параметров стратегии или NA.
#' @param priority_goals Числовой вектор id из PriorityGoals.
#'
#' @return NULL, если площадка выключена (SERVING_OFF); numeric(0), если
#'   стратегия не обучается на конверсиях; иначе вектор id целей.
#'   0 означает «все цели кампании».
#' @keywords internal
yaf_learning_goals <- function(type, goal_id, priority_goals) {
  if (is.null(type) || is.na(type) || type == "SERVING_OFF") return(NULL)
  # 13 — «ключевые цели»: оптимизация по PriorityGoals
  if (grepl("MULTIPLE_GOALS|MAX_PROFIT", type) || identical(goal_id, 13)) {
    return(as.numeric(priority_goals))
  }
  if (is.na(goal_id)) return(numeric(0))
  goal_id
}

#' Id целей из PriorityGoals
#'
#' @param x Объект с полем PriorityGoals = list(Items = list(list(GoalId, ...))).
#'
#' @return Числовой вектор id целей.
#' @keywords internal
yaf_priority_goal_ids <- function(x) {
  items <- x$PriorityGoals$Items
  if (length(items) == 0) return(numeric(0))
  vapply(items, function(i) as.numeric(i$GoalId), numeric(1))
}

#' Конверсии за период по кампаниям, площадкам и целям
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Id кампаний для фильтра отчёта.
#' @param goal_ids Id целей; 0 — конверсии по всем целям (без Goals).
#' @param date_from,date_to Период отчёта.
#' @param attribution Модель атрибуции.
#'
#' @return data.frame с колонками CampaignId, Platform (SEARCH / NETWORK),
#'   GoalId, Conversions.
#' @keywords internal
yaf_learning_conversions <- function(login, campaign_ids, goal_ids,
                                     date_from, date_to, attribution) {
  filter <- paste("CampaignId IN", paste(format(campaign_ids, scientific = FALSE,
                                                trim = TRUE), collapse = ","))
  fields <- c("CampaignId", "AdNetworkType", "Conversions")
  platform <- function(x) ifelse(x == "SEARCH", "SEARCH", "NETWORK")

  out <- list()
  if (0 %in% goal_ids) {
    r <- yaf_get_report(login, date_from = date_from, date_to = date_to,
                        fields = fields, filter = filter)
    if (nrow(r) > 0) {
      out[[length(out) + 1]] <- data.frame(
        CampaignId = as.numeric(r$CampaignId),
        Platform = platform(r$AdNetworkType),
        GoalId = 0,
        Conversions = tidyr::replace_na(suppressWarnings(as.numeric(r$Conversions)), 0)
      )
    }
  }

  goal_ids <- setdiff(goal_ids, 0)
  # В одном отчёте не больше 10 целей
  for (chunk in split(goal_ids, ceiling(seq_along(goal_ids) / 10))) {
    r <- yaf_get_report(login, date_from = date_from, date_to = date_to,
                        fields = fields, goals = chunk, attribution = attribution,
                        filter = filter)
    conv_cols <- grep("^Conversions_\\d+_", names(r), value = TRUE)
    for (col in conv_cols) {
      if (nrow(r) == 0) next
      out[[length(out) + 1]] <- data.frame(
        CampaignId = as.numeric(r$CampaignId),
        Platform = platform(r$AdNetworkType),
        GoalId = as.numeric(sub("^Conversions_(\\d+)_.*$", "\\1", col)),
        Conversions = tidyr::replace_na(suppressWarnings(as.numeric(r[[col]])), 0)
      )
    }
  }

  if (length(out) == 0) {
    return(data.frame(CampaignId = numeric(0), Platform = character(0),
                      GoalId = numeric(0), Conversions = numeric(0)))
  }
  dplyr::bind_rows(out)
}

#' Проверка обучения стратегий по числу конверсий
#'
#' API Директа не отдаёт статус обучения стратегии, поэтому функция
#' оценивает его сама: считает конверсии по целям стратегии за последние
#' days дней и сравнивает с порогом min_conversions. По правилам Директа
#' обучение останавливается, если за 7 дней набралось меньше 10 конверсий.
#' Это приближение: «обучается» и «обучилась» функция не различает.
#'
#' Цели берутся из стратегии кампании: GoalId из параметров стратегии,
#' для GoalId = 13 («ключевые цели») и стратегий с несколькими целями —
#' PriorityGoals. GoalId = 0 означает все цели кампании: тогда считаются
#' все конверсии. Стратегия на поиске и в сетях проверяется отдельно.
#'
#' Для кампаний с пакетной стратегией цели и тип берутся из самой
#' стратегии (сервис strategies), а конверсии суммируются по всем
#' кампаниям этой стратегии и всем площадкам — обучается стратегия
#' целиком. У таких строк Platform = "ALL".
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Id кампаний для проверки. Если NULL — все кампании
#'   в состояниях states.
#' @param states Состояния кампаний (State), которые попадут в результат.
#'   Игнорируется, если указаны campaign_ids.
#' @param goal_ids Id целей вручную. Если задано, используются для всех
#'   кампаний вместо целей из стратегии.
#' @param min_conversions Порог конверсий за период.
#' @param days Длина периода в днях: от Sys.Date() - days до вчера.
#' @param attribution Модель атрибуции для конверсий. AUTO — модель из
#'   настроек кампании.
#'
#' @return data.frame, одна строка на кампанию и площадку: CampaignId,
#'   CampaignName, CampaignType, State, Platform (SEARCH / NETWORK / ALL),
#'   StrategyId (Id пакетной стратегии или NA), StrategyType, GoalIds
#'   (через "; "), Conversions, Status: "ok" — конверсий не меньше порога,
#'   "low_data" — меньше порога, "not_applicable" — стратегия не обучается
#'   на конверсиях (оплата за клики, ручные ставки) или цели неизвестны.
#'   Атрибуты date_from и date_to — период подсчёта.
#' @export
#'
#' @examples
#' \dontrun{
#' learning <- yaf_strategy_learning_check("my_login")
#' learning[learning$Status == "low_data", ]
#'
#' # Своя цель и порог
#' yaf_strategy_learning_check("my_login", campaign_ids = 123,
#'                             goal_ids = 456789, min_conversions = 20)
#' }
yaf_strategy_learning_check <- function(login,
                                        campaign_ids = NULL,
                                        states = "ON",
                                        goal_ids = NULL,
                                        min_conversions = 10,
                                        days = 7,
                                        attribution = "AUTO") {
  date_from <- Sys.Date() - days
  date_to <- Sys.Date() - 1

  campaigns <- yaf_api_get(login, "campaigns", list(
    SelectionCriteria = list(
      Statuses = list("ACCEPTED", "DRAFT", "MODERATION", "REJECTED")
    ),
    FieldNames = c("Id", "Name", "Type", "State"),
    TextCampaignFieldNames = c("BiddingStrategy", "PriorityGoals", "PackageBiddingStrategy"),
    UnifiedCampaignFieldNames = c("BiddingStrategy", "PriorityGoals", "PackageBiddingStrategy"),
    MobileAppCampaignFieldNames = c("BiddingStrategy", "PackageBiddingStrategy"),
    CpmBannerCampaignFieldNames = c("BiddingStrategy")
  ), progress = FALSE)

  # Пакетные стратегии
  strategy_of <- function(x) {
    block <- x[[grep("Campaign$", names(x), value = TRUE)[1]]]
    id <- block$PackageBiddingStrategy$StrategyId
    if (is.null(id)) NA_real_ else as.numeric(id)
  }
  strategy_ids <- vapply(campaigns, strategy_of, numeric(1))
  package_ids <- unique(stats::na.omit(strategy_ids))
  packages <- list()
  if (length(package_ids) > 0) {
    items <- yaf_api_get(login, "strategies", list(
      SelectionCriteria = list(),
      FieldNames = c("Id", "Name", "Type", "PriorityGoals"),
      StrategyMaximumConversionRateFieldNames = "GoalId",
      StrategyAverageCpaFieldNames = "GoalId",
      StrategyPayForConversionFieldNames = "GoalId",
      StrategyAverageCrrFieldNames = "GoalId",
      StrategyPayForConversionCrrFieldNames = "GoalId"
    ), batch_ids = package_ids, batch_field = "Ids", batch_size = 1000,
    progress = FALSE)
    for (s in items) packages[[as.character(s$Id)]] <- s
  }

  # Единицы обучения: кампания + площадка или пакетная стратегия целиком
  rows <- list()
  for (i in seq_along(campaigns)) {
    x <- campaigns[[i]]
    block <- x[[grep("Campaign$", names(x), value = TRUE)[1]]]
    base <- list(CampaignId = as.numeric(x$Id), CampaignName = x$Name,
                 CampaignType = x$Type, State = x$State)

    if (!is.na(strategy_ids[i])) {
      s <- packages[[as.character(strategy_ids[i])]]
      type <- if (is.null(s)) NA_character_ else s$Type
      goals <- if (is.null(s)) numeric(0) else
        yaf_learning_goals(type, yaf_find_goal_id(s), yaf_priority_goal_ids(s))
      rows[[length(rows) + 1]] <- c(base, list(
        Platform = "ALL", StrategyId = strategy_ids[i], StrategyType = type,
        Unit = paste0("strategy_", strategy_ids[i]), Goals = list(goals)
      ))
      next
    }

    for (p in c("Search", "Network")) {
      st <- block$BiddingStrategy[[p]]
      type <- st$BiddingStrategyType
      goals <- yaf_learning_goals(type, yaf_find_goal_id(st), yaf_priority_goal_ids(block))
      if (is.null(goals)) next
      rows[[length(rows) + 1]] <- c(base, list(
        Platform = toupper(p), StrategyId = NA_real_, StrategyType = type,
        Unit = paste0("campaign_", x$Id, "_", toupper(p)), Goals = list(goals)
      ))
    }
  }

  empty <- data.frame(CampaignId = numeric(0), CampaignName = character(0),
                      CampaignType = character(0), State = character(0),
                      Platform = character(0), StrategyId = numeric(0),
                      StrategyType = character(0), GoalIds = character(0),
                      Conversions = numeric(0), Status = character(0))
  if (length(rows) == 0) return(empty)

  units <- dplyr::bind_rows(lapply(rows, function(r) {
    r$Goals <- NULL
    r
  }))
  units$Goals <- lapply(rows, function(r) r$Goals[[1]])
  if (!is.null(goal_ids)) {
    units$Goals <- rep(list(as.numeric(goal_ids)), nrow(units))
  }

  keep <- if (!is.null(campaign_ids)) {
    units$CampaignId %in% as.numeric(campaign_ids)
  } else {
    units$State %in% states
  }
  if (!any(keep)) return(empty)

  # В отчёт идут проверяемые кампании и все кампании их пакетных стратегий
  needed_units <- unique(units$Unit[keep])
  applicable <- lengths(units$Goals) > 0
  report_campaigns <- unique(units$CampaignId[units$Unit %in% needed_units & applicable])
  report_goals <- unique(unlist(units$Goals[units$Unit %in% needed_units]))

  conv <- if (length(report_campaigns) > 0 && length(report_goals) > 0) {
    yaf_learning_conversions(login, report_campaigns, report_goals,
                             date_from, date_to, attribution)
  } else {
    data.frame(CampaignId = numeric(0), Platform = character(0),
               GoalId = numeric(0), Conversions = numeric(0))
  }

  res <- units[keep, ]
  res$Conversions <- vapply(seq_len(nrow(res)), function(i) {
    goals <- res$Goals[[i]]
    if (length(goals) == 0) return(NA_real_)
    camp <- units$CampaignId[units$Unit == res$Unit[i]]
    sel <- conv$CampaignId %in% camp & conv$GoalId %in% goals
    if (res$Platform[i] != "ALL") sel <- sel & conv$Platform == res$Platform[i]
    sum(conv$Conversions[sel])
  }, numeric(1))

  res$GoalIds <- vapply(res$Goals, function(g) paste(format(g, scientific = FALSE, trim = TRUE),
                                                     collapse = "; "), character(1))
  res$GoalIds[res$GoalIds == ""] <- NA_character_
  res$Status <- ifelse(is.na(res$Conversions), "not_applicable",
                       ifelse(res$Conversions >= min_conversions, "ok", "low_data"))

  res <- as.data.frame(res[, names(empty)])
  rownames(res) <- NULL
  attr(res, "date_from") <- date_from
  attr(res, "date_to") <- date_to
  res
}
