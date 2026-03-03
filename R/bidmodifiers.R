#' Получение корректировок ставок из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param adgroup_ids Вектор ID групп (необязательно).
#'
#' @export
yaf_get_bid_modifiers <- function(login, campaign_ids = NULL, adgroup_ids = NULL) {

  token <- get_yaf_token(login)

  # 1. Если фильтры не заданы, берем все кампании аккаунта
  if (is.null(campaign_ids) && is.null(adgroup_ids)) {
    cli::cli_inform("i Поиск кампаний для выгрузки корректировок...")
    camps <- yaf_get_campaigns(login)

    col_idx <- grep("^id$", names(camps), ignore.case = TRUE)
    if (length(col_idx) == 0) stop("Колонка 'id' не найдена в списке кампаний.")
    campaign_ids <- as.numeric(camps[[col_idx]])
  }

  # 2. Настройка батчинга по 10
  ids_to_process <- if (!is.null(adgroup_ids)) adgroup_ids else campaign_ids
  id_field <- if (!is.null(adgroup_ids)) "AdGroupIds" else "CampaignIds"

  batch_size <- 10
  n <- length(ids_to_process)
  num_batches <- ceiling(n / batch_size)

  all_modifiers <- list()

  pb <- cli::cli_progress_bar(
    name = paste0("Выгрузка корректировок ", login),
    total = num_batches,
    clear = FALSE
  )

  for (i in 1:num_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n)
    current_batch <- ids_to_process[start_idx:end_idx]

    offset <- 0
    has_next_page <- TRUE

    while(has_next_page) {
      selection_criteria <- list(
        Levels = as.list(c("CAMPAIGN", "AD_GROUP"))
      )
      selection_criteria[[id_field]] <- as.list(as.numeric(current_batch))

      body <- list(
        method = "get",
        params = list(
          SelectionCriteria = selection_criteria,
          FieldNames = as.list(c("Id", "CampaignId", "AdGroupId", "Level", "Type")),
          MobileAdjustmentFieldNames = as.list(c("BidModifier")),
          DemographicsAdjustmentFieldNames = as.list(c("Gender", "Age", "BidModifier")),
          RegionalAdjustmentFieldNames = as.list(c("RegionId", "BidModifier")),
          VideoAdjustmentFieldNames = as.list(c("BidModifier")),
          SmartAdAdjustmentFieldNames = as.list(c("BidModifier")),
          AdGroupAdjustmentFieldNames = as.list(c("BidModifier")),
          RetargetingAdjustmentFieldNames = as.list(c("RetargetingConditionId", "BidModifier")),
          Page = list(Offset = jsonlite::unbox(offset))
        )
      )

      res <- httr2::request("https://api.direct.yandex.com/json/v5/bidmodifiers") |>
        httr2::req_headers(
          Authorization = paste0("Bearer ", token),
          'Client-Login' = login,
          'Accept-Language' = "ru"
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      if (!is.null(res$error)) {
        cli::cli_inform("\n! Батч {i} пропущен: {res$error$error_detail}")
        has_next_page <- FALSE
        next
      }

      if (!is.null(res$result$BidModifiers)) {
        all_modifiers <- c(all_modifiers, res$result$BidModifiers)
      }

      if (!is.null(res$result$LimitedBy)) {
        offset <- res$result$LimitedBy
      } else {
        has_next_page <- FALSE
      }
    }
    cli::cli_progress_update(id = pb)
  }
  cli::cli_progress_done(id = pb)

  # 3. Парсинг результатов
  if (length(all_modifiers) == 0) {
    cli::cli_alert_warning("Корректировки не найдены.")
    return(data.frame())
  }

  cli::cli_inform("i Парсинг данных...")

  df <- purrr::map_dfr(all_modifiers, function(x) {

    # Супер-безопасное извлечение данных
    safe_val <- function(obj, field) {
      if (is.null(obj)) return(NA)

      # Случай 1: Это список списков (и он не пустой)
      if (is.list(obj) && length(obj) > 0 && is.list(obj[[1]])) {
        val <- obj[[1]][[field]]
      }
      # Случай 2: Это простой именованный список (атомарный вектор после jsonlite)
      else if (!is.null(obj[[field]])) {
        val <- obj[[field]]
      }
      else {
        val <- NA
      }
      return(if(is.null(val)) NA else val)
    }

    out <- data.frame(
      modifier_id = x$Id,
      campaign_id = if(!is.null(x$CampaignId)) x$CampaignId else NA,
      adgroup_id  = if(!is.null(x$AdGroupId)) x$AdGroupId else NA,
      level       = x$Level,
      type        = x$Type,
      value       = NA,
      condition   = NA,
      stringsAsFactors = FALSE
    )

    if (x$Type == "MOBILE_ADJUSTMENT") {
      out$value <- safe_val(x$MobileAdjustment, "BidModifier")

    } else if (x$Type == "DEMOGRAPHICS_ADJUSTMENT") {
      out$value <- safe_val(x$DemographicsAdjustment, "BidModifier")
      g <- safe_val(x$DemographicsAdjustment, "Gender")
      a <- safe_val(x$DemographicsAdjustment, "Age")
      out$condition <- paste0(g, " ", a)

    } else if (x$Type == "REGIONAL_ADJUSTMENT") {
      out$value <- safe_val(x$RegionalAdjustment, "BidModifier")
      out$condition <- as.character(safe_val(x$RegionalAdjustment, "RegionId"))

    } else if (x$Type == "RETARGETING_ADJUSTMENT") {
      out$value <- safe_val(x$RetargetingAdjustment, "BidModifier")
      out$condition <- as.character(safe_val(x$RetargetingAdjustment, "RetargetingConditionId"))

    } else if (x$Type == "AD_GROUP_ADJUSTMENT") {
      out$value <- safe_val(x$AdGroupAdjustment, "BidModifier")

    } else if (x$Type == "VIDEO_ADJUSTMENT") {
      out$value <- safe_extract(x$VideoAdjustment, "BidModifier")

    } else if (x$Type == "SMART_AD_ADJUSTMENT") {
      out$value <- safe_extract(x$SmartAdAdjustment, "BidModifier")
    }

    return(out)
  })

  # Очищаем результаты от совсем пустых строк (если вдруг проскочили)
  df <- df[!is.na(df$modifier_id), ]

  cli::cli_alert_success("Успешно выгружено корректировок: {.val {nrow(df)}}")
  return(df)
}
