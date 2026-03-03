#' Получение объявлений из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param adgroup_ids Вектор ID групп (необязательно).
#' @param ad_ids Вектор ID конкретных объявлений (необязательно).
#'
#' @export
yaf_get_ads <- function(login, campaign_ids = NULL, adgroup_ids = NULL, ad_ids = NULL) {

  token <- get_yaf_token(login)

  # 1. Логика фильтрации: определяем, что именно мы будем батчить
  if (!is.null(ad_ids)) {
    ids_to_process <- as.numeric(ad_ids)
    id_field <- "Ids"
  } else if (!is.null(adgroup_ids)) {
    ids_to_process <- as.numeric(adgroup_ids)
    id_field <- "AdGroupIds"
  } else if (!is.null(campaign_ids)) {
    ids_to_process <- as.numeric(campaign_ids)
    id_field <- "CampaignIds"
  } else {
    # Если ничего не указано, выгружаем все кампании аккаунта
    cli::cli_inform("i Параметры не указаны. Поиск всех кампаний аккаунта...")
    camps <- yaf_get_campaigns(login)

    # Ищем колонку с ID (игнорируя регистр)
    id_col <- grep("(?i)^id$", names(camps), value = TRUE)

    if (length(id_col) > 0) {
      ids_to_process <- as.numeric(camps[[id_col[1]]])
      id_field <- "CampaignIds"
    } else {
      cli::cli_alert_danger("Не удалось получить список ID кампаний.")
      return(data.frame())
    }
  }

  # 2. Настройка батчинга
  batch_size <- 10
  n <- length(ids_to_process)
  num_batches <- ceiling(n / batch_size)

  all_ads <- list()

  pb <- cli::cli_progress_bar(
    name = paste0("Выгрузка объявлений ", login),
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
      # Формируем SelectionCriteria динамически
      current_criteria <- list()
      current_criteria[[id_field]] <- as.list(as.numeric(current_batch))

      body <- list(
        method = "get",
        params = list(
          SelectionCriteria = current_criteria,
          FieldNames = as.list(c("Id", "CampaignId", "AdGroupId", "Type", "Status", "State")),
          TextAdFieldNames = as.list(c("Title", "Title2", "Text", "Href", "DisplayUrlPath")),
          DynamicTextAdFieldNames = as.list(c("Text")),
          MobileAppAdFieldNames = as.list(c("Title", "Text")),
          Page = list(Offset = jsonlite::unbox(offset))
        )
      )

      res <- tryCatch({
        httr2::request("https://api.direct.yandex.com/json/v5/ads") |>
          httr2::req_headers(
            Authorization = paste0("Bearer ", token),
            'Client-Login' = login,
            'Accept-Language' = "ru"
          ) |>
          httr2::req_body_json(body) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
      }, error = function(e) NULL)

      if (is.null(res) || !is.null(res$error)) {
        has_next_page <- FALSE
        next
      }

      if (!is.null(res$result$Ads)) {
        all_ads <- c(all_ads, res$result$Ads)
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
  if (length(all_ads) == 0) {
    cli::cli_alert_warning("Объявления не найдены.")
    return(data.frame())
  }

  cli::cli_inform("i Парсинг объявлений...")

  df <- purrr::map_dfr(all_ads, function(x) {
    out <- data.frame(
      ad_id       = x$Id,
      campaign_id = x$CampaignId,
      adgroup_id  = x$AdGroupId,
      type        = x$Type,
      status      = x$Status,
      state       = x$State,
      stringsAsFactors = FALSE
    )

    # Определяем, из какого объекта тянуть контент
    content <- if (!is.null(x$TextAd)) {
      x$TextAd
    } else if (!is.null(x$DynamicTextAd)) {
      x$DynamicTextAd
    } else if (!is.null(x$MobileAppAd)) {
      x$MobileAppAd
    } else {
      NULL
    }

    out$title  <- if (!is.null(content$Title)) content$Title else NA
    out$title2 <- if (!is.null(content$Title2)) content$Title2 else NA
    out$text   <- if (!is.null(content$Text)) content$Text else NA
    out$href   <- if (!is.null(content$Href)) content$Href else NA

    return(out)
  })

  cli::cli_alert_success("Успешно выгружено объявлений: {.val {nrow(df)}}")
  return(df)
}
