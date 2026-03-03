#' Получение списка групп объявлений из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param adgroup_ids Вектор ID конкретных групп (необязательно).
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#'
#' @export
yaf_get_adgroups <- function(login, campaign_ids = NULL) {

  token <- get_yaf_token(login)

  # 1. Сбор ID кампаний (Делаем ДО инициализации бара)
  if (is.null(campaign_ids)) {
    cli::cli_inform("i Поиск кампаний для {login}...")
    camps <- yaf_get_campaigns(login)
    col_idx <- grep("^id$", names(camps), ignore.case = TRUE)
    if (length(col_idx) == 0) stop("Колонка 'id' не найдена.")
    campaign_ids <- as.numeric(camps[[col_idx]])
  }

  # 2. Настройка батчинга
  batch_size <- 10
  n <- length(campaign_ids)
  num_batches <- ceiling(n / batch_size)

  all_adgroups <- list()

  # Инициализируем бар (без лишних спецсимволов, для стабильности)
  pb <- cli::cli_progress_bar(
    name = paste0("Выгрузка ", login),
    total = num_batches,
    clear = FALSE
  )

  for (i in 1:num_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n)
    current_batch <- campaign_ids[start_idx:end_idx]

    offset <- 0
    has_next_page <- TRUE

    while(has_next_page) {
      body <- list(
        method = "get",
        params = list(
          SelectionCriteria = list(
            CampaignIds = as.list(as.numeric(current_batch))
          ),
          FieldNames = c("Id", "CampaignId", "Name", "Status", "Type"),
          Page = list(Offset = jsonlite::unbox(offset))
        )
      )

      # Делаем запрос абсолютно молчаливым
      res <- httr2::request("https://api.direct.yandex.com/json/v5/adgroups") |>
        httr2::req_headers(Authorization = paste0("Bearer ", token), 'Client-Login' = login) |>
        httr2::req_body_json(body) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      if (!is.null(res$error)) {
        # Если ошибка есть, выводим её аккуратно, чтобы не убить бар
        cli::cli_inform("\n! Батч {i} пропущен: {res$error$error_detail}")
        has_next_page <- FALSE
        next
      }

      if (!is.null(res$result$AdGroups)) {
        all_adgroups <- c(all_adgroups, res$result$AdGroups)
      }

      if (!is.null(res$result$LimitedBy)) {
        offset <- res$result$LimitedBy
      } else {
        has_next_page <- FALSE
      }
    }

    # Обновляем бар
    cli::cli_progress_update(id = pb)
  }

  # Закрываем бар
  cli::cli_progress_done(id = pb)

  # 3. Финальная сборка
  if (length(all_adgroups) == 0) return(data.frame())

  df <- purrr::map_dfr(all_adgroups, function(x) {
    list(
      adgroup_id = x$Id,
      campaign_id = x$CampaignId,
      adgroup_name = x$Name,
      status = x$Status,
      type = x$Type
    )
  })

  cli::cli_alert_success("Готово! Выгружено групп: {.val {nrow(df)}}")
  return(df)
}
