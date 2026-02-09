#' Получение популярных запросов из Wordstat с автоматическим углублением
#'
#' @param phrase Вектор базовых фраз (масок).
#' @param token Ваш OAuth-токен Яндекс Wordstat (обязателен).
#' @param region_id ID региона (213 - Мск, 225 - РФ, по умолчанию 225).
#' @param top_n Лимит фраз на один запрос (макс 2000).
#' @param depth_limit Порог частотности для парсинга вглубь. Если частота фразы выше этого числа, скрипт соберет вложенные запросы для нее.
#'
#' @export
yaf_ws_top <- function(phrase,
                       token,
                       region_id = 225,
                       top_n = 2000,
                       depth_limit = NULL) {

  # 1. Проверка входных данных
  if (missing(token) || is.null(token) || token == "") {
    stop("Ошибка: Аргумент 'token' обязателен для авторизации в API.")
  }

  api_url <- "https://api.wordstat.yandex.net/v1/topRequests"

  # 2. Внутренняя функция для одного запроса к API
  get_single <- function(p) {
    body <- list(
      numPhrases = top_n,
      phrase = p,
      regions = list(as.numeric(region_id)),
      devices = list("all")
    )

    # Выполняем запрос с отключенным автоматическим выбросом ошибки
    resp <- httr2::request(api_url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Content-Type" = "application/json;charset=utf-8",
        "Authorization" = paste("Bearer", token)
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = ~ FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(resp)

    # ОБРАБОТКА ОШИБОК
    if (status != 200) {
      error_msg <- httr2::resp_body_string(resp)

      if (status == 429) {
        # Извлекаем количество секунд из текста ошибки Яндекса
        seconds_left <- stringi::stri_extract_first_regex(error_msg, "(?<=refill: )\\d+")

        detail_msg <- "Время ожидания не распознано."
        if (!is.na(seconds_left)) {
          wait_sec <- as.numeric(seconds_left)
          wait_h <- round(wait_sec / 3600, 1)
          detail_msg <- paste0("Нужно подождать: ", wait_sec, " сек. (~", wait_h, " ч.)")
        }

        msg <- paste0("\n[ПРЕВЫШЕН ЛИМИТ (429)]\n",
                      "Яндекс говорит: ", error_msg, "\n",
                      "АНАЛИЗ: ", detail_msg, "\n",
                      "------------------------------------------")
      } else {
        msg <- paste0("\n[ОШИБКА API (", status, ")]\n",
                      "Текст ответа: ", error_msg)
      }
      stop(msg, call. = FALSE)
    }

    # Парсим результат
    res_list <- httr2::resp_body_json(resp)$topRequests

    if (length(res_list) == 0) return(NULL)

    # Превращаем список в аккуратный data.frame
    df <- as.data.frame(do.call(rbind, res_list)) |>
      lapply(unlist) |>
      as.data.frame() |>
      dplyr::mutate(
        count = as.numeric(count),
        source_phrase = p # Временная метка
      )
    return(df)
  }

  # 3. ЭТАП 1: Сбор данных по основным маскам
  message("Этап 1: Сбор базовых фраз для ", length(phrase), " масок...")
  res_step1 <- purrr::map_df(phrase, function(x) {
    message("Парсим: ", x)
    Sys.sleep(0.2) # 5 запросов в секунду
    get_single(x)
  })

  if (is.null(res_step1) || nrow(res_step1) == 0) {
    message("Данные не найдены.")
    return(NULL)
  }

  # 4. ЭТАП 2: Углубление (если задан порог depth_limit)
  if (!is.null(depth_limit)) {
    fat_phrases <- res_step1 |>
      dplyr::filter(count >= depth_limit) |>
      dplyr::pull(phrase)

    if (length(fat_phrases) > 0) {
      message("Этап 2: Углубляемся в ", length(fat_phrases), " популярных фраз...")

      res_step2 <- purrr::map_df(fat_phrases, function(x) {
        message("Парсим вложенность для: ", x)
        Sys.sleep(0.2)
        get_single(x)
      })

      # Склеиваем, удаляем дубликаты, убираем технический столбец
      final_res <- dplyr::bind_rows(res_step1, res_step2) |>
        dplyr::distinct(phrase, .keep_all = TRUE) |>
        dplyr::arrange(desc(count)) |>
        dplyr::select(-source_phrase)

      message("Готово! Собрано ", nrow(final_res), " уникальных фраз.")
      return(final_res)
    }
  }

  # Если углубления не было
  return(res_step1 |>
           dplyr::arrange(desc(count)) |>
           dplyr::select(-source_phrase))
}
