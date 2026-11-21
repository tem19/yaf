#' Получение популярных запросов из Wordstat с автоматическим углублением
#'
#' Использует метод GetTop API Wordstat из Yandex Search API (AI Studio).
#' Возвращает данные за последние 30 дней.
#'
#' @param phrase Вектор базовых фраз (масок), до 400 символов каждая.
#' @param api_key API-ключ сервисного аккаунта с областью действия
#'   `yc.search-api.execute`. Аккаунту нужна роль `search-api.webSearch.user`.
#'   По умолчанию берётся из переменной окружения `YANDEX_SEARCH_API_KEY`.
#' @param folder_id ID каталога в Yandex Cloud, в котором создан сервисный
#'   аккаунт. По умолчанию берётся из переменной окружения `YANDEX_FOLDER_ID`.
#' @param region_id Вектор ID регионов (213 - Мск, 225 - РФ, по умолчанию 225).
#'   `NULL` — все регионы.
#' @param devices Устройства: `"all"`, `"desktop"`, `"phone"`, `"tablet"`.
#' @param top_n Лимит фраз на один запрос (от 1 до 2000).
#' @param depth_limit Порог частотности для парсинга вглубь. Если частота фразы выше этого числа, скрипт соберет вложенные запросы для нее.
#' @param timeout Таймаут одного HTTP-запроса в секундах.
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
# Счётчик часовой квоты Wordstat в рамках сессии R. Сбрасывается через час.
yaf_ws_quota <- new.env(parent = emptyenv())
yaf_ws_quota$start <- NULL
yaf_ws_quota$count <- 0L

yaf_ws_take_quota <- function() {
  now <- Sys.time()
  if (is.null(yaf_ws_quota$start) ||
      difftime(now, yaf_ws_quota$start, units = "secs") >= 3600) {
    yaf_ws_quota$start <- now
    yaf_ws_quota$count <- 0L
  }
  if (yaf_ws_quota$count >= 100L) {
    stop(yaf_api_error(
      "\u041F\u0440\u0435\u0432\u044B\u0448\u0435\u043D\u0430 \u0447\u0430\u0441\u043E\u0432\u0430\u044F \u043A\u0432\u043E\u0442\u0430 Wordstat (100 \u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432). \u041F\u043E\u0432\u0442\u043E\u0440\u0438\u0442\u0435 \u043F\u043E\u0437\u0436\u0435 \u0438\u043B\u0438 \u0443\u043C\u0435\u043D\u044C\u0448\u0438\u0442\u0435 depth_limit.",
      service = "wordstat"
    ))
  }
  yaf_ws_quota$count <- yaf_ws_quota$count + 1L
  invisible(NULL)
}

yaf_ws_top <- function(phrase,
                       api_key = Sys.getenv("YANDEX_SEARCH_API_KEY"),
                       folder_id = Sys.getenv("YANDEX_FOLDER_ID"),
                       region_id = 225,
                       devices = "all",
                       top_n = 2000,
                       depth_limit = NULL,
                       timeout = yaf_default_timeout()) {

  # 1. Проверка входных данных
  if (!is.character(phrase) || length(phrase) == 0 || anyNA(phrase) ||
      any(!nzchar(trimws(phrase)))) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'phrase' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043D\u0435\u043F\u0443\u0441\u0442\u044B\u043C \u0432\u0435\u043A\u0442\u043E\u0440\u043E\u043C \u0441\u0442\u0440\u043E\u043A.", call. = FALSE)
  }
  if (is.null(api_key) || !nzchar(api_key)) {
    stop("\u041D\u0443\u0436\u0435\u043D 'api_key' (\u0438\u043B\u0438 \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u0430\u044F \u043E\u043A\u0440\u0443\u0436\u0435\u043D\u0438\u044F YANDEX_SEARCH_API_KEY).",
         call. = FALSE)
  }
  auth <- paste("Api-Key", api_key)
  if (is.null(folder_id) || !nzchar(folder_id)) {
    stop("\u041D\u0443\u0436\u0435\u043D 'folder_id' (\u0438\u043B\u0438 \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u0430\u044F \u043E\u043A\u0440\u0443\u0436\u0435\u043D\u0438\u044F YANDEX_FOLDER_ID).",
         call. = FALSE)
  }
  devices <- match.arg(devices, c("all", "desktop", "phone", "tablet"),
                       several.ok = TRUE)

  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
      top_n < 1 || top_n > 2000) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'top_n' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u0447\u0438\u0441\u043B\u043E\u043C \u043E\u0442 1 \u0434\u043E 2000.", call. = FALSE)
  }
  top_n <- as.integer(top_n)

  if (!is.null(depth_limit) && (!is.numeric(depth_limit) || length(depth_limit) != 1L ||
                                is.na(depth_limit))) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'depth_limit' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u0447\u0438\u0441\u043B\u043E\u043C \u0438\u043B\u0438 NULL.", call. = FALSE)
  }

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
    send <- function() {
      yaf_ws_take_quota()
      httr2::request(api_url) |>
        httr2::req_headers(Authorization = auth) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = ~ FALSE) |>
        httr2::req_perform()
    }

    resp <- send()
    status <- httr2::resp_status(resp)

    # 429 — временное превышение квоты: ждём Retry-After и повторяем
    retries <- 0L
    while (status == 429 && retries < 3L) {
      retries <- retries + 1L
      wait <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
      if (length(wait) != 1L || is.na(wait) || wait <= 0) wait <- 10 * retries
      yaf_sleep(wait)
      resp <- send()
      status <- httr2::resp_status(resp)
    }

    # ОБРАБОТКА ОШИБОК
    if (status != 200) {
      error_msg <- httr2::resp_body_string(resp)
      parsed <- tryCatch(jsonlite::fromJSON(error_msg), error = function(e) NULL)
      if (is.list(parsed) && !is.null(parsed$message)) error_msg <- parsed$message

      hint <- switch(as.character(status),
        "401" = "\u041F\u0440\u043E\u0432\u0435\u0440\u044C\u0442\u0435 API-\u043A\u043B\u044E\u0447.",
        "403" = "\u041F\u0440\u043E\u0432\u0435\u0440\u044C\u0442\u0435 \u0440\u043E\u043B\u044C search-api.webSearch.user \u0438 folder_id.",
        "429" = "\u041F\u0440\u0435\u0432\u044B\u0448\u0435\u043D\u0430 \u043A\u0432\u043E\u0442\u0430: 10 \u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0432 \u0441\u0435\u043A\u0443\u043D\u0434\u0443, 100 \u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0432 \u0447\u0430\u0441.",
        NULL
      )
      stop(yaf_api_error(
        paste0("\u041E\u0448\u0438\u0431\u043A\u0430 API Wordstat (", status, "): ", error_msg,
               if (!is.null(hint)) paste0("\n", hint)),
        code = status, service = "wordstat"
      ))
    }

    # Парсим результат
    res_list <- httr2::resp_body_json(resp)$results

    if (length(res_list) == 0) return(NULL)

    # Ответ Wordstat иногда приходит с неполными строками: без phrase или
    # без count. Такие строки пропускаем, а не роняем всю выгрузку.
    has_fields <- vapply(res_list, function(x) {
      !is.null(x$phrase) && !is.null(x$count)
    }, logical(1))
    skipped <- sum(!has_fields)
    if (skipped > 0) {
      cli::cli_warn("\u041F\u0440\u043E\u043F\u0443\u0449\u0435\u043D\u043E \u0441\u0442\u0440\u043E\u043A Wordstat \u0431\u0435\u0437 phrase \u0438\u043B\u0438 count: {skipped}.")
    }
    res_list <- res_list[has_fields]
    if (length(res_list) == 0) return(NULL)

    data.frame(
      phrase = vapply(res_list, function(x) as.character(x$phrase), character(1)),
      count = as.numeric(vapply(res_list, function(x) as.character(x$count), character(1))),
      source_phrase = p # Временная метка
    )
  }

  # 3. ЭТАП 1: Сбор данных по основным маскам
  message("\u042D\u0442\u0430\u043F 1: \u0421\u0431\u043E\u0440 \u0431\u0430\u0437\u043E\u0432\u044B\u0445 \u0444\u0440\u0430\u0437 \u0434\u043B\u044F ", length(phrase), " \u043C\u0430\u0441\u043E\u043A...")
  res_step1 <- dplyr::bind_rows(lapply(phrase, function(x) {
    message("\u041F\u0430\u0440\u0441\u0438\u043C: ", x)
    yaf_sleep(0.2) # не больше 10 запросов в секунду
    get_single(x)
  }))

  if (is.null(res_step1) || nrow(res_step1) == 0) {
    message("\u0414\u0430\u043D\u043D\u044B\u0435 \u043D\u0435 \u043D\u0430\u0439\u0434\u0435\u043D\u044B.")
    return(NULL)
  }

  # 4. ЭТАП 2: Углубление (если задан порог depth_limit)
  if (!is.null(depth_limit)) {
    fat_phrases <- res_step1 |>
      dplyr::filter(count >= depth_limit) |>
      dplyr::pull(phrase) |>
      setdiff(phrase)

    if (length(fat_phrases) > 0) {
      message("\u042D\u0442\u0430\u043F 2: \u0423\u0433\u043B\u0443\u0431\u043B\u044F\u0435\u043C\u0441\u044F \u0432 ", length(fat_phrases), " \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0444\u0440\u0430\u0437...")

      res_step2 <- dplyr::bind_rows(lapply(fat_phrases, function(x) {
        message("\u041F\u0430\u0440\u0441\u0438\u043C \u0432\u043B\u043E\u0436\u0435\u043D\u043D\u043E\u0441\u0442\u044C \u0434\u043B\u044F: ", x)
        yaf_sleep(0.2)
        get_single(x)
      }))

      # Склеиваем, удаляем дубликаты, убираем технический столбец
      final_res <- dplyr::bind_rows(res_step1, res_step2) |>
        dplyr::distinct(phrase, .keep_all = TRUE) |>
        dplyr::arrange(dplyr::desc(count)) |>
        dplyr::select(-source_phrase)

      message("\u0413\u043E\u0442\u043E\u0432\u043E! \u0421\u043E\u0431\u0440\u0430\u043D\u043E ", nrow(final_res), " \u0443\u043D\u0438\u043A\u0430\u043B\u044C\u043D\u044B\u0445 \u0444\u0440\u0430\u0437.")
      return(final_res)
    }
  }

  # Если углубления не было
  return(res_step1 |>
           dplyr::distinct(phrase, .keep_all = TRUE) |>
           dplyr::arrange(dplyr::desc(count)) |>
           dplyr::select(-source_phrase))
}
