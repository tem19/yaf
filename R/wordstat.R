#' Получение популярных запросов из Wordstat с автоматическим углублением
#'
#' Использует метод GetTop API Wordstat из Yandex Search API (AI Studio).
#' Возвращает данные за последние 30 дней.
#'
#' @param phrase Вектор базовых фраз (масок), до 400 символов каждая.
#' @param api_key API-ключ сервисного аккаунта с областью действия
#'   `yc.search-api.execute`. Аккаунту нужна роль `search-api.webSearch.user`.
#'   По умолчанию берётся из переменной окружения `YANDEX_SEARCH_API_KEY`.
#' @param iam_token IAM-токен. Можно передать вместо `api_key`.
#' @param folder_id ID каталога в Yandex Cloud, в котором создан сервисный
#'   аккаунт. По умолчанию берётся из переменной окружения `YANDEX_FOLDER_ID`.
#' @param region_id Вектор ID регионов (213 - Мск, 225 - РФ, по умолчанию 225).
#'   `NULL` — все регионы.
#' @param devices Устройства: `"all"`, `"desktop"`, `"phone"`, `"tablet"`.
#' @param top_n Лимит фраз на один запрос (от 1 до 2000).
#' @param depth_limit Порог частотности для парсинга вглубь. Если частота фразы выше этого числа, скрипт соберет вложенные запросы для нее.
#'
#' @return data.frame с колонками phrase и count, отсортированный по
#'   убыванию частоты. Если данных нет — NULL.
#'
#' @section Ограничения:
#' Квоты API: 10 запросов в секунду и 100 запросов в час. Каждая фраза
#' (и каждая фраза при углублении) — отдельный запрос.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' top <- yaf_ws_top("купить слона", api_key = "AQVN...",
#'                   folder_id = "b1g...", region_id = 213)
#' }
yaf_ws_top <- function(phrase,
                       api_key = Sys.getenv("YANDEX_SEARCH_API_KEY"),
                       iam_token = NULL,
                       folder_id = Sys.getenv("YANDEX_FOLDER_ID"),
                       region_id = 225,
                       devices = "all",
                       top_n = 2000,
                       depth_limit = NULL) {

  # 1. Проверка входных данных
  if (!is.null(iam_token) && nzchar(iam_token)) {
    auth <- paste("Bearer", iam_token)
  } else if (!is.null(api_key) && nzchar(api_key)) {
    auth <- paste("Api-Key", api_key)
  } else {
    stop("Нужен 'api_key' (или переменная окружения YANDEX_SEARCH_API_KEY) ",
         "либо 'iam_token'.", call. = FALSE)
  }
  if (is.null(folder_id) || !nzchar(folder_id)) {
    stop("Нужен 'folder_id' (или переменная окружения YANDEX_FOLDER_ID).",
         call. = FALSE)
  }
  devices <- match.arg(devices, c("all", "desktop", "phone", "tablet"),
                       several.ok = TRUE)

  api_url <- "https://searchapi.api.cloud.yandex.net/v2/wordstat/topRequests"

  # 2. Внутренняя функция для одного запроса к API
  get_single <- function(p) {
    body <- list(
      phrase = p,
      numPhrases = as.character(top_n),
      devices = as.list(paste0("DEVICE_", toupper(devices))),
      folderId = folder_id
    )
    if (!is.null(region_id)) body$regions <- as.list(as.character(region_id))

    # Выполняем запрос с отключенным автоматическим выбросом ошибки
    resp <- httr2::request(api_url) |>
      httr2::req_headers(Authorization = auth) |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = ~ FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(resp)

    # ОБРАБОТКА ОШИБОК
    if (status != 200) {
      error_msg <- httr2::resp_body_string(resp)
      parsed <- tryCatch(jsonlite::fromJSON(error_msg), error = function(e) NULL)
      if (is.list(parsed) && !is.null(parsed$message)) error_msg <- parsed$message

      hint <- switch(as.character(status),
        "401" = "Проверьте API-ключ или IAM-токен.",
        "403" = "Проверьте роль search-api.webSearch.user и folder_id.",
        "429" = "Превышена квота: 10 запросов в секунду, 100 запросов в час.",
        NULL
      )
      stop(yaf_api_error(
        paste0("Ошибка API Wordstat (", status, "): ", error_msg,
               if (!is.null(hint)) paste0("\n", hint)),
        code = status, service = "wordstat"
      ))
    }

    # Парсим результат
    res_list <- httr2::resp_body_json(resp)$results

    if (length(res_list) == 0) return(NULL)

    data.frame(
      phrase = purrr::map_chr(res_list, "phrase"),
      count = as.numeric(purrr::map_chr(res_list, "count")),
      source_phrase = p # Временная метка
    )
  }

  # 3. ЭТАП 1: Сбор данных по основным маскам
  message("Этап 1: Сбор базовых фраз для ", length(phrase), " масок...")
  res_step1 <- dplyr::bind_rows(lapply(phrase, function(x) {
    message("Парсим: ", x)
    yaf_sleep(0.2) # не больше 10 запросов в секунду
    get_single(x)
  }))

  if (is.null(res_step1) || nrow(res_step1) == 0) {
    message("Данные не найдены.")
    return(NULL)
  }

  # 4. ЭТАП 2: Углубление (если задан порог depth_limit)
  if (!is.null(depth_limit)) {
    fat_phrases <- res_step1 |>
      dplyr::filter(count >= depth_limit) |>
      dplyr::pull(phrase) |>
      setdiff(phrase)

    if (length(fat_phrases) > 0) {
      message("Этап 2: Углубляемся в ", length(fat_phrases), " популярных фраз...")

      res_step2 <- dplyr::bind_rows(lapply(fat_phrases, function(x) {
        message("Парсим вложенность для: ", x)
        yaf_sleep(0.2)
        get_single(x)
      }))

      # Склеиваем, удаляем дубликаты, убираем технический столбец
      final_res <- dplyr::bind_rows(res_step1, res_step2) |>
        dplyr::distinct(phrase, .keep_all = TRUE) |>
        dplyr::arrange(dplyr::desc(count)) |>
        dplyr::select(-source_phrase)

      message("Готово! Собрано ", nrow(final_res), " уникальных фраз.")
      return(final_res)
    }
  }

  # Если углубления не было
  return(res_step1 |>
           dplyr::distinct(phrase, .keep_all = TRUE) |>
           dplyr::arrange(dplyr::desc(count)) |>
           dplyr::select(-source_phrase))
}
