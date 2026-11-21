#' Приведение момента времени к формату сервиса changes
#'
#' Сервис changes принимает время в UTC в формате ISO 8601:
#' "YYYY-MM-DDThh:mm:ssZ".
#'
#' @param timestamp Строка в формате "YYYY-MM-DDThh:mm:ssZ" или POSIXct.
#'
#' @return Строка в формате "YYYY-MM-DDThh:mm:ssZ".
#' @keywords internal
yaf_format_timestamp <- function(timestamp) {
  if (inherits(timestamp, "POSIXt")) {
    return(format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  if (!is.character(timestamp) || length(timestamp) != 1 ||
      !grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", timestamp)) {
    stop("timestamp \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C POSIXct \u0438\u043B\u0438 \u0441\u0442\u0440\u043E\u043A\u043E\u0439 \u0432\u0438\u0434\u0430 \"2025-01-31T23:59:59Z\".",
         call. = FALSE)
  }
  timestamp
}

#' Вызов метода сервиса changes
#'
#' Обёртка над yaf_api_request(): у сервиса changes свои методы
#' (check, checkCampaigns, checkDictionaries) и нет постраничной выдачи,
#' поэтому yaf_api_get() здесь не подходит.
#'
#' @param login Логин в Яндексе.
#' @param method Метод сервиса: "check", "checkCampaigns", "checkDictionaries".
#' @param params Список params запроса.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return Список с элементами result (поле result ответа API)
#'   и units (баллы, см. yaf_parse_units()).
#' @keywords internal
yaf_changes_request <- function(login, method, params = list(), token = NULL,
                                timeout = yaf_default_timeout()) {
  token <- token %||% get_yaf_token(login)
  # Пустой params должен уйти объектом {}, а не массивом []
  if (length(params) == 0) params <- stats::setNames(list(), character(0))
  yaf_api_request(login, "changes", list(method = method, params = params), token,
                  timeout = timeout)
}

#' Текущее время сервера API для отслеживания изменений
#'
#' Возвращает время сервера Яндекс Директа (метод checkDictionaries без
#' Timestamp). Сохраните его и передайте в yaf_check_campaign_changes()
#' или yaf_check_changes() при следующем запуске, чтобы получить всё,
#' что изменилось с этого момента.
#'
#' @param login Логин в Яндексе.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return Строка в формате "YYYY-MM-DDThh:mm:ssZ" (UTC).
#' @export
#'
#' @examples
#' \dontrun{
#' ts <- yaf_changes_timestamp("my_login")
#' # ... через какое-то время
#' yaf_check_campaign_changes("my_login", ts)
#' }
yaf_changes_timestamp <- function(login, token = NULL, timeout = yaf_default_timeout()) {
  yaf_changes_request(login, "checkDictionaries", token = token,
                      timeout = timeout)$result$Timestamp
}

#' Какие кампании изменились с заданного момента
#'
#' Метод checkCampaigns сервиса changes: возвращает кампании, в которых
#' что-то изменилось после timestamp, и где именно. Сами изменения
#' не возвращаются: чтобы узнать, что поменялось, перевыгрузите эти
#' кампании (yaf_get_campaigns(), yaf_get_ads() и т. д.) и сравните
#' со снимком.
#'
#' @param login Логин в Яндексе.
#' @param timestamp Момент, с которого искать изменения: POSIXct или
#'   строка "YYYY-MM-DDThh:mm:ssZ" (UTC). Обычно это атрибут "timestamp"
#'   результата предыдущего вызова. Если NULL, функция только получает
#'   текущее время сервера и возвращает пустую таблицу — так удобно
#'   делать первый запуск.
#' @param raw Если TRUE, вернуть поле Campaigns ответа API без разбора.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками CampaignId, ChangesIn (значения через
#'   запятую) и логическими ChangedSelf (параметры кампании),
#'   ChangedChildren (группы, объявления, фразы), ChangedStat
#'   (корректировка статистики). Атрибут "timestamp" — время сервера,
#'   его нужно передать при следующем вызове; "units" — баллы.
#' @export
#'
#' @examples
#' \dontrun{
#' ch <- yaf_check_campaign_changes("my_login", "2025-01-31T00:00:00Z")
#' next_ts <- attr(ch, "timestamp")
#' changed <- ch$CampaignId[ch$ChangedSelf | ch$ChangedChildren]
#' ads <- yaf_get_ads("my_login", campaign_ids = changed)
#' }
yaf_check_campaign_changes <- function(login, timestamp = NULL, raw = FALSE,
                                       token = NULL, timeout = yaf_default_timeout()) {
  empty <- data.frame(
    CampaignId = numeric(0), ChangesIn = character(0),
    ChangedSelf = logical(0), ChangedChildren = logical(0), ChangedStat = logical(0)
  )

  if (is.null(timestamp)) {
    ts <- yaf_changes_timestamp(login, token = token, timeout = timeout)
    cli::cli_alert_info("timestamp \u043D\u0435 \u0437\u0430\u0434\u0430\u043D. \u0422\u0435\u043A\u0443\u0449\u0435\u0435 \u0432\u0440\u0435\u043C\u044F \u0441\u0435\u0440\u0432\u0435\u0440\u0430: {ts}")
    return(structure(if (raw) list() else empty, timestamp = ts))
  }

  timestamp <- yaf_format_timestamp(timestamp)
  res <- yaf_changes_request(
    login, "checkCampaigns",
    list(Timestamp = timestamp),
    token = token, timeout = timeout
  )
  items <- res$result$Campaigns %||% list()

  out <- if (raw) {
    items
  } else if (length(items) == 0) {
    empty
  } else {
    changes <- lapply(items, function(x) unlist(x$ChangesIn))
    data.frame(
      CampaignId      = vapply(items, function(x) as.numeric(x$CampaignId), numeric(1)),
      ChangesIn       = vapply(changes, paste, "", collapse = ","),
      ChangedSelf     = vapply(changes, function(x) "SELF" %in% x, logical(1)),
      ChangedChildren = vapply(changes, function(x) "CHILDREN" %in% x, logical(1)),
      ChangedStat     = vapply(changes, function(x) "STAT" %in% x, logical(1)),
      stringsAsFactors = FALSE
    )
  }

  if (!raw) cli::cli_alert_success("\u0418\u0437\u043C\u0435\u043D\u0438\u043B\u043E\u0441\u044C \u043A\u0430\u043C\u043F\u0430\u043D\u0438\u0439: {.val {length(items)}}")

  server_ts <- res$result$Timestamp
  if (is.null(server_ts) || !nzchar(server_ts)) {
    # Без времени сервера безопаснее вернуть исходный timestamp: повторная
    # проверка не пропустит изменения, хотя может выдать их ещё раз.
    cli::cli_warn("API \u043D\u0435 \u0432\u0435\u0440\u043D\u0443\u043B Timestamp; \u0434\u043B\u044F \u0441\u043B\u0435\u0434\u0443\u044E\u0449\u0435\u0439 \u043F\u0440\u043E\u0432\u0435\u0440\u043A\u0438 \u043E\u0441\u0442\u0430\u0432\u043B\u0435\u043D \u0438\u0441\u0445\u043E\u0434\u043D\u044B\u0439 timestamp.")
    server_ts <- timestamp
  }
  structure(out, timestamp = server_ts, units = res$units)
}

#' Какие кампании, группы и объявления изменились с заданного момента
#'
#' Метод check сервиса changes. Проверяет изменения внутри заданных
#' кампаний, групп или объявлений (указывается только один вид id) и
#' возвращает id изменившихся объектов. Id автоматически режутся на батчи
#' по лимитам API: 3000 кампаний, 10000 групп или 50000 объявлений.
#'
#' Если API не обработал часть объектов (поле Unprocessed), функция
#' повторно проверяет их отдельными запросами: до max_followups раундов.
#' Что осталось необработанным после этого, попадает в элемент
#' unprocessed результата, и выводится предупреждение.
#'
#' @param login Логин в Яндексе.
#' @param timestamp Момент, с которого искать изменения: POSIXct или
#'   строка "YYYY-MM-DDThh:mm:ssZ" (UTC). Если NULL, функция только получает
#'   текущее время сервера и возвращает пустой результат.
#' @param campaign_ids Вектор ID кампаний. Если не задан ни один вид id,
#'   берутся все кампании аккаунта.
#' @param adgroup_ids Вектор ID групп (приоритетнее campaign_ids).
#' @param ad_ids Вектор ID объявлений (приоритетнее adgroup_ids).
#' @param fields Что возвращать: любые из "CampaignIds", "AdGroupIds",
#'   "AdIds", "CampaignsStat".
#' @param max_followups Сколько раундов дополнительных запросов делать
#'   для необработанных объектов.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return Список:
#'   \describe{
#'     \item{modified}{data.frame с колонками Type ("Campaign", "AdGroup",
#'       "Ad") и Id — изменившиеся объекты.}
#'     \item{campaigns_stat}{data.frame с колонками CampaignId и BorderDate —
#'       кампании, у которых скорректирована статистика, и дата, начиная
#'       с которой её нужно перевыгрузить.}
#'     \item{not_found}{data.frame Type, Id — объекты из запроса, которые
#'       не найдены.}
#'     \item{unprocessed}{data.frame Type, Id — объекты, которые так и не
#'       удалось обработать.}
#'     \item{timestamp}{Время сервера для следующего вызова. Если запросов
#'       было несколько, берётся самое раннее, чтобы не пропустить
#'       изменения между батчами.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' ch <- yaf_check_changes("my_login", "2025-01-31T00:00:00Z",
#'                         campaign_ids = c(123, 456))
#' ch$modified
#' next_ts <- ch$timestamp
#' }
yaf_check_changes <- function(login, timestamp = NULL,
                              campaign_ids = NULL, adgroup_ids = NULL, ad_ids = NULL,
                              fields = c("CampaignIds", "AdGroupIds", "AdIds", "CampaignsStat"),
                              max_followups = 3, token = NULL,
                              timeout = yaf_default_timeout()) {
  empty_ids <- data.frame(Type = character(0), Id = numeric(0), stringsAsFactors = FALSE)
  empty_stat <- data.frame(CampaignId = numeric(0), BorderDate = character(0),
                           stringsAsFactors = FALSE)

  if (is.null(timestamp)) {
    ts <- yaf_changes_timestamp(login, token = token, timeout = timeout)
    cli::cli_alert_info("timestamp \u043D\u0435 \u0437\u0430\u0434\u0430\u043D. \u0422\u0435\u043A\u0443\u0449\u0435\u0435 \u0432\u0440\u0435\u043C\u044F \u0441\u0435\u0440\u0432\u0435\u0440\u0430: {ts}")
    return(list(modified = empty_ids, campaigns_stat = empty_stat,
                not_found = empty_ids, unprocessed = empty_ids, timestamp = ts))
  }

  timestamp <- yaf_format_timestamp(timestamp)
  token <- token %||% get_yaf_token(login)
  # Лимиты числа id в одном запросе check
  batch_size <- c(CampaignIds = 3000, AdGroupIds = 10000, AdIds = 50000)

  if (!is.null(ad_ids)) {
    queue <- list(AdIds = ad_ids)
  } else if (!is.null(adgroup_ids)) {
    queue <- list(AdGroupIds = adgroup_ids)
  } else {
    queue <- list(CampaignIds = campaign_ids %||%
                    yaf_all_campaign_ids(login, token = token, timeout = timeout))
  }

  modified <- list()
  stat <- list()
  not_found <- list()
  timestamps <- character(0)
  round <- 0L

  repeat {
    unprocessed <- list()

    for (field in names(queue)) {
      ids <- unique(as.numeric(queue[[field]]))
      if (length(ids) == 0) next
      batches <- split(ids, ceiling(seq_along(ids) / batch_size[[field]]))

      for (batch in batches) {
        params <- list(Timestamp = timestamp, FieldNames = as.list(fields))
        params[[field]] <- as.list(batch)
        res <- yaf_changes_request(login, "check", params, token,
                                   timeout = timeout)$result

        if (!is.null(res$Timestamp)) timestamps <- c(timestamps, res$Timestamp)
        modified[[length(modified) + 1]] <- yaf_changes_ids(res$Modified)
        not_found[[length(not_found) + 1]] <- yaf_changes_ids(res$NotFound)
        stat <- c(stat, res$Modified$CampaignsStat)

        for (f in names(res$Unprocessed)) {
          unprocessed[[f]] <- c(unprocessed[[f]], unlist(res$Unprocessed[[f]]))
        }
      }
    }

    if (length(unprocessed) == 0 || round >= max_followups) break
    round <- round + 1L
    queue <- unprocessed
  }

  unprocessed_df <- yaf_changes_ids(unprocessed)
  if (nrow(unprocessed_df) > 0) {
    cli::cli_warn(c(
      "!" = "\u041D\u0435 \u0443\u0434\u0430\u043B\u043E\u0441\u044C \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u0430\u0442\u044C \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432: {nrow(unprocessed_df)}.",
      "i" = "\u041E\u043D\u0438 \u043F\u0435\u0440\u0435\u0447\u0438\u0441\u043B\u0435\u043D\u044B \u0432 \u044D\u043B\u0435\u043C\u0435\u043D\u0442\u0435 {.field unprocessed} \u0440\u0435\u0437\u0443\u043B\u044C\u0442\u0430\u0442\u0430."
    ))
  }

  modified_df <- unique(dplyr::bind_rows(empty_ids, modified))
  cli::cli_alert_success("\u0418\u0437\u043C\u0435\u043D\u0438\u043B\u043E\u0441\u044C \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432: {.val {nrow(modified_df)}}")

  list(
    modified = modified_df,
    campaigns_stat = if (length(stat) == 0) empty_stat else unique(data.frame(
      CampaignId = vapply(stat, function(x) as.numeric(x$CampaignId), numeric(1)),
      BorderDate = vapply(stat, function(x) as.character(x$BorderDate), character(1)),
      stringsAsFactors = FALSE
    )),
    not_found = unique(dplyr::bind_rows(empty_ids, not_found)),
    unprocessed = unprocessed_df,
    # Строки ISO 8601 в UTC сравниваются лексикографически.
    # Если проверять было нечего — время сервера; если API не вернул
    # Timestamp — исходный timestamp, чтобы не пропустить изменения.
    timestamp = if (length(timestamps) > 0) {
      min(timestamps)
    } else if (all(vapply(queue, length, integer(1)) == 0L)) {
      yaf_changes_timestamp(login, token = token, timeout = timeout)
    } else {
      timestamp
    }
  )
}

#' Разбор блока id из ответа changes.check
#'
#' Превращает объект вида list(CampaignIds = list(...), AdGroupIds = ...,
#' AdIds = ...) в длинную таблицу. Поле CampaignsStat пропускается.
#'
#' @param x Блок Modified, NotFound или Unprocessed из ответа API.
#'
#' @return data.frame с колонками Type ("Campaign", "AdGroup", "Ad") и Id.
#' @keywords internal
yaf_changes_ids <- function(x) {
  types <- c(CampaignIds = "Campaign", AdGroupIds = "AdGroup", AdIds = "Ad")
  parts <- lapply(intersect(names(types), names(x)), function(f) {
    ids <- as.numeric(unlist(x[[f]]))
    data.frame(Type = rep(types[[f]], length(ids)), Id = ids, stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(
    data.frame(Type = character(0), Id = numeric(0), stringsAsFactors = FALSE),
    parts
  )
}
