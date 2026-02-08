#' Получение популярных запросов из Wordstat с автоматическим углублением
#'
#' @param phrase Фраза или вектор фраз для парсинга.
#' @param token Токен Wordstat API (обязателен).
#' @param region_id ID региона (по умолчанию 225 - Россия).
#' @param top_n Количество фраз для выгрузки (максимум 2000).
#' @param depth_limit Числовой порог частотности. Если указан, фразы с частотностью выше этого порога будут пропарсены повторно.
#'
#' @export
yaf_ws_top <- function(phrase,
                       token,
                       region_id = 225,
                       top_n = 2000,
                       depth_limit = NULL) {

  if (missing(token) || is.null(token) || token == "") {
    stop("Ошибка: Аргумент 'token' обязателен.")
  }

  api_url <- "https://api.wordstat.yandex.net/v1/topRequests"

  # Внутренняя функция для базового запроса
  get_single <- function(p) {
    body <- list(
      numPhrases = top_n,
      phrase = p,
      regions = list(as.numeric(region_id)),
      devices = list("all")
    )

    resp <- httr2::request(api_url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Content-Type" = "application/json;charset=utf-8",
        "Authorization" = paste("Bearer", token)
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_perform()

    res_data <- httr2::resp_body_json(resp)$topRequests

    if (length(res_data) == 0) return(NULL)

    df <- as.data.frame(do.call(rbind, res_data)) |>
      lapply(unlist) |>
      as.data.frame() |>
      dplyr::mutate(
        count = as.numeric(count),
        source_phrase = p
      )
    return(df)
  }

  # Этап 1
  message("Этап 1: Сбор базовых фраз...")
  res_step1 <- purrr::map_df(phrase, function(x) {
    message("Парсим маску: ", x)
    Sys.sleep(0.2) # Ускоренная пауза (5 запросов в сек)
    get_single(x)
  })

  if (is.null(res_step1) || nrow(res_step1) == 0) return(NULL)

  # Этап 2 (углубление)
  if (!is.null(depth_limit)) {
    fat_phrases <- res_step1 |>
      dplyr::filter(count >= depth_limit) |>
      dplyr::pull(phrase)

    if (length(fat_phrases) > 0) {
      message("Этап 2: Углубляемся в ", length(fat_phrases), " фраз...")

      res_step2 <- purrr::map_df(fat_phrases, function(x) {
        message("Парсим вложенность для: ", x)
        Sys.sleep(0.2) # Ускоренная пауза (5 запросов в сек)
        get_single(x)
      })

      res_final <- dplyr::bind_rows(res_step1, res_step2) |>
        dplyr::distinct(phrase, .keep_all = TRUE) |>
        dplyr::arrange(desc(count)) |>
        dplyr::select(-source_phrase)

      return(res_final)
    }
  }

  return(res_step1 |>
           dplyr::arrange(desc(count)) |>
           dplyr::select(-source_phrase))
}
