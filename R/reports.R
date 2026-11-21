#' Получение статистики из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param date_from Дата начала (ГГГГ-ММ-ДД).
#' @param date_to Дата окончания (ГГГГ-ММ-ДД).
#' @param date_range_type Пресет периода (например: TODAY, YESTERDAY, LAST_7_DAYS, LAST_30_DAYS, LAST_MONTH, ALL_TIME).
#' @param fields Список полей для выгрузки.
#' @param goals Список ID целей Метрики (необязательно).
#' @param attribution Модели атрибуции (FC, LC, LSC, LYDC, FCCD, LSCCD, LYDCCD, AUTO).
#' @param filter Строка для фильтрации данных.
#' @param numeric_fields_as_numeric Менять ли тип числовых полей на numeric.
#' @param search_query_report TRUE для отчета по поисковым запросам.
#' @param include_vat Логический аргумент: если \code{TRUE}, в API уходит IncludeVAT = "YES", иначе "NO".
#' @param save_report Логическое значение. Если TRUE, сохраняет CSV.
#' @param save_dir Путь к папке для сохранения отчета (по умолчанию "reports").
#' @param max_tries Максимальное количество попыток опроса отчета до остановки.
#'   Попытки идут не чаще раза в 10 секунд (лимит API): 120 попыток — около
#'   20 минут ожидания.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах. Скачивание большого
#'   отчёта может занимать больше времени, чем обычный запрос.
#'
#' @return data.frame с запрошенными полями отчета.
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- yaf_get_report("my_login", date_range_type = "LAST_7_DAYS",
#'                         fields = c("Date", "CampaignName", "Clicks", "Cost"))
#' }
yaf_get_report <- function(login,
                           date_from = Sys.Date()-7,
                           date_to = Sys.Date()- 1,
                           date_range_type = NULL,
                           fields = c("Date","Impressions","Clicks"),
                           goals = NULL,
                           attribution = NULL,
                           filter = NULL,
                           search_query_report = FALSE,
                           numeric_fields_as_numeric = TRUE,
                           include_vat = TRUE,
                           save_report = FALSE,
                           save_dir = "reports",
                           max_tries = 120L, token = NULL,
                           timeout = yaf_default_timeout()) {

  # Валидация аргументов
  if (missing(login) || is.null(login) || !is.character(login) || length(login) != 1L || !nzchar(login)) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'login' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043D\u0435\u043F\u0443\u0441\u0442\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u043E\u0439.", call. = FALSE)
  }

  if (!is.null(date_range_type)) {
    allowed_presets <- c(
      "TODAY", "YESTERDAY",
      "LAST_3_DAYS", "LAST_5_DAYS", "LAST_7_DAYS", "LAST_14_DAYS",
      "LAST_30_DAYS", "LAST_90_DAYS", "LAST_365_DAYS",
      "THIS_WEEK_MON_TODAY", "THIS_WEEK_SUN_TODAY",
      "LAST_WEEK", "LAST_BUSINESS_WEEK", "LAST_WEEK_SUN_SAT",
      "THIS_MONTH", "LAST_MONTH",
      "ALL_TIME"
    )
    if (!date_range_type %in% allowed_presets) {
      stop(
        sprintf(
          "\u041D\u0435\u043A\u043E\u0440\u0440\u0435\u043A\u0442\u043D\u044B\u0439 'date_range_type': %s. \u0414\u043E\u043F\u0443\u0441\u0442\u0438\u043C\u044B\u0435 \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F: %s",
          date_range_type,
          paste(allowed_presets, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (!is.logical(include_vat) || length(include_vat) != 1L) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'include_vat' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043B\u043E\u0433\u0438\u0447\u0435\u0441\u043A\u0438\u043C \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u0435\u043C \u0434\u043B\u0438\u043D\u044B 1.", call. = FALSE)
  }

  if (!is.logical(save_report) || length(save_report) != 1L) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'save_report' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043B\u043E\u0433\u0438\u0447\u0435\u0441\u043A\u0438\u043C \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u0435\u043C \u0434\u043B\u0438\u043D\u044B 1.", call. = FALSE)
  }

  if (!is.character(save_dir) || length(save_dir) != 1L || !nzchar(save_dir)) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'save_dir' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043D\u0435\u043F\u0443\u0441\u0442\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u043E\u0439.", call. = FALSE)
  }

  if (!is.numeric(max_tries) || length(max_tries) != 1L || max_tries <= 0) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'max_tries' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043F\u043E\u043B\u043E\u0436\u0438\u0442\u0435\u043B\u044C\u043D\u044B\u043C \u0447\u0438\u0441\u043B\u043E\u043C.", call. = FALSE)
  }

  max_tries <- as.integer(max_tries)

  # Определение типа отчета
  ReportType <- if (search_query_report) "SEARCH_QUERY_PERFORMANCE_REPORT" else "CUSTOM_REPORT"

  # 1. Получаем токен
  token <- token %||% get_yaf_token(login)

  # 2. Логика определения периода
  if (!is.null(date_range_type)) {
    PeriodType <- date_range_type
    selection_criteria <- stats::setNames(list(), character(0))
  } else {
    date_from <- tryCatch(as.Date(date_from), error = function(e) NA)
    date_to <- tryCatch(as.Date(date_to), error = function(e) NA)
    if (is.na(date_from) || is.na(date_to)) {
      stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442\u044B date_from \u0438 date_to \u0434\u043E\u043B\u0436\u043D\u044B \u0431\u044B\u0442\u044C \u0434\u0430\u0442\u0430\u043C\u0438.", call. = FALSE)
    }
    if (date_from > date_to) {
      stop("date_from \u043D\u0435 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u043F\u043E\u0437\u0436\u0435 date_to.", call. = FALSE)
    }
    PeriodType <- "CUSTOM_DATE"
    selection_criteria <- list(
      DateFrom = as.character(date_from),
      DateTo = as.character(date_to)
    )
  }

  if (!is.null(goals)) {
    goals_num <- suppressWarnings(as.numeric(goals))
    if (anyNA(goals_num) || any(!is.finite(goals_num))) {
      stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'goals' \u0434\u043E\u043B\u0436\u0435\u043D \u0441\u043E\u0434\u0435\u0440\u0436\u0430\u0442\u044C \u0447\u0438\u0441\u043B\u043E\u0432\u044B\u0435 Id \u0446\u0435\u043B\u0435\u0439.", call. = FALSE)
    }
    goals <- goals_num
  }

  if (!is.null(attribution) && is.null(goals)) {
    cli::cli_warn("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'attribution' \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0441\u044F \u0442\u043E\u043B\u044C\u043A\u043E \u0432\u043C\u0435\u0441\u0442\u0435 \u0441 'goals' \u0438 \u0441\u0435\u0439\u0447\u0430\u0441 \u043F\u0440\u043E\u0438\u0433\u043D\u043E\u0440\u0438\u0440\u043E\u0432\u0430\u043D.")
    attribution <- NULL
  }

  # 3. Формируем тело запроса
  body <- list(
    params = list(
      SelectionCriteria = selection_criteria,
      # as.list: иначе одно поле уйдёт строкой, а не массивом, и API вернёт ошибку
      FieldNames = as.list(fields),
      Page = list(Limit = 2000000L),
      ReportName = paste0("yaf_", gsub("[^A-Za-z0-9_-]", "_", login), "_",
                          format(Sys.time(), "%Y%m%d%H%M%S")),
      ReportType = ReportType,
      DateRangeType = PeriodType,
      Format = "TSV"
    )
  )

  body$params$IncludeVAT <- if(include_vat) "YES" else "NO"

  if(!is.null(goals)) {
    body$params$Goals <- as.list(as.numeric(goals))
    body$params$AttributionModels <- if(!is.null(attribution)) as.list(attribution) else list("AUTO")
  }

  # 4. Обработка фильтра
  if (!is.null(filter)) {
    parts <- unlist(strsplit(trimws(filter), "\\s+"))
    if (length(parts) < 3) stop("\u041E\u0448\u0438\u0431\u043A\u0430: \u0444\u0438\u043B\u044C\u0442\u0440 \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u0432 \u0444\u043E\u0440\u043C\u0430\u0442\u0435 '\u041F\u043E\u043B\u0435 \u041E\u043F\u0435\u0440\u0430\u0442\u043E\u0440 \u0417\u043D\u0430\u0447\u0435\u043D\u0438\u0435'")

    filter_list <- list(list(
      Field = parts[1],
      Operator = parts[2],
      Values = as.list(unlist(strsplit(paste(parts[3:length(parts)], collapse = " "), ",\\s*")))
    ))
    body$params$SelectionCriteria$Filter <- filter_list
  }

  # 5. Внутренняя функция для отправки
  send_request <- function() {
    httr2::request("https://api.direct.yandex.com/json/v5/reports") |>
      httr2::req_headers(
        Authorization = paste0("Bearer ", token),
        'Accept-Language' = "ru",
        'Client-Login' = login,
        returnMoneyInMicros = "false",
        skipReportSummary = "true",
        skipReportHeader = "true"
      ) |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(timeout) |>
      # Не даём httr2 падать на 4xx/5xx: иначе теряется текст ошибки от API
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }

  # Ошибка из JSON-ответа API ({"error": {...}}) или NULL. Content-Type у
  # ответа reports не всегда JSON, поэтому разбираем тело как строку
  api_error <- function(resp) {
    parsed <- tryCatch(
      jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(parsed)) parsed$error else NULL
  }

  # Текст ошибки API или тело ответа как есть
  api_error_message <- function(resp) {
    status <- httr2::resp_status(resp)
    err <- api_error(resp)
    msg <- if (!is.null(err)) {
      sprintf("[reports] \u041E\u0448\u0438\u0431\u043A\u0430 API %s: %s. %s",
              err$error_code %||% "?",
              err$error_string %||% "",
              err$error_detail %||% "")
    } else {
      sprintf("[reports] HTTP %s: %s", status, httr2::resp_body_string(resp))
    }
    if (status == 401 || identical(as.integer(err$error_code), 53L)) {
      msg <- paste0(msg, " \u041F\u0435\u0440\u0435\u0432\u044B\u043F\u0443\u0441\u0442\u0438\u0442\u0435 \u0442\u043E\u043A\u0435\u043D: yaf_get_token('", login, "').")
    }
    msg
  }

  cat("\u0417\u0430\u043F\u0440\u043E\u0441 \u043E\u0442\u043F\u0440\u0430\u0432\u043B\u0435\u043D. \u041F\u0435\u0440\u0438\u043E\u0434:", PeriodType, "| \u041B\u043E\u0433\u0438\u043D:", login, "\n")
  start_time <- proc.time()

  req <- send_request()
  resp_status <- httr2::resp_status(req)
  attempt <- 1L

  while(resp_status != 200 && attempt < max_tries) {
    # 201/202 — отчёт формируется, 502-504 — временный сбой,
    # ошибка 56 — превышен лимит запросов метода, остальное — ошибка
    rate_limited <- identical(as.integer(api_error(req)$error_code), 56L)
    if (!resp_status %in% c(201, 202, 502, 503, 504) && !rate_limited) {
      stop(yaf_api_error(api_error_message(req), service = "reports"))
    }
    # API разрешает опрашивать reports не чаще раза в 10 секунд;
    # заголовок retryIn подсказывает, когда отчёт может быть готов
    retry_in <- suppressWarnings(as.numeric(httr2::resp_header(req, "retryIn")))
    yaf_sleep(max(10, retry_in, na.rm = TRUE))
    req <- send_request()
    resp_status <- httr2::resp_status(req)
    attempt <- attempt + 1L

    elapsed <- round((proc.time() - start_time)[["elapsed"]], 1)
    cat("\r\u041F\u0440\u043E\u0448\u043B\u043E \u0432\u0440\u0435\u043C\u0435\u043D\u0438:", elapsed, "\u0441\u0435\u043A. | \u0421\u0442\u0430\u0442\u0443\u0441:", resp_status, "    ")
    utils::flush.console()
  }

  if (resp_status != 200) {
    stop(
      sprintf(
        "\u041E\u0442\u0447\u0435\u0442 \u043D\u0435 \u0431\u044B\u043B \u0433\u043E\u0442\u043E\u0432 \u043F\u043E\u0441\u043B\u0435 %d \u043F\u043E\u043F\u044B\u0442\u043E\u043A. \u041F\u043E\u0441\u043B\u0435\u0434\u043D\u0438\u0439 HTTP-\u0441\u0442\u0430\u0442\u0443\u0441: %s",
        max_tries,
        resp_status
      ),
      call. = FALSE
    )
  }

  cat("\n\u041E\u0442\u0447\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D. \u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u0434\u0430\u043D\u043D\u044B\u0445...\n")
  tsv <- tryCatch(httr2::resp_body_string(req), error = function(e) "")

  # У отчёта без строк тело пустое: это не ошибка, а «нет данных»
  if (!nzchar(trimws(tsv))) {
    cli::cli_alert_info("\u041E\u0442\u0447\u0451\u0442 \u043D\u0435 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u0442 \u0441\u0442\u0440\u043E\u043A \u0437\u0430 \u0432\u044B\u0431\u0440\u0430\u043D\u043D\u044B\u0439 \u043F\u0435\u0440\u0438\u043E\u0434.")
    empty <- stats::setNames(rep(list(logical(0)), length(fields)), fields)
    return(as.data.frame(empty, stringsAsFactors = FALSE))
  }

  result <- utils::read.table(
    text = tsv, header = TRUE, sep = "\t",
    stringsAsFactors = FALSE, quote = "", comment.char = "", encoding = "UTF-8"
  )

  # ПРЕОБРАЗОВАНИЕ ТИПОВ ДАННЫХ

  # Числа
  if (numeric_fields_as_numeric) {
    numeric_fields <- "Conversions_|Clicks|Impressions|Cost|Bounces|Profit|Revenue|Sessions|AvgImpressionPosition"
    result <- result |>
      dplyr::mutate(dplyr::across(dplyr::matches(numeric_fields), ~ suppressWarnings(tidyr::replace_na(as.numeric(.), 0))))
  }

  # Даты (Date, Month, Quarter, Year)
  # Яндекс присылает Month как "ГГГГ-ММ-01", Quarter как "ГГГГ-КВ-01", Year как "ГГГГ-01-01"
  date_cols <- intersect(names(result), c("Date", "Month", "Quarter", "Year"))

  if (length(date_cols) > 0) {
    result <- result |>
      dplyr::mutate(dplyr::across(dplyr::all_of(date_cols), ~ as.Date(.)))
  }

  rows_count <- nrow(result)
  if (rows_count >= body$params$Page$Limit) {
    cli::cli_warn(c(
      "!" = "\u041E\u0442\u0447\u0451\u0442 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u0442 \u043D\u0435 \u043C\u0435\u043D\u044C\u0448\u0435 {body$params$Page$Limit} \u0441\u0442\u0440\u043E\u043A \u0438 \u043C\u043E\u0433 \u0431\u044B\u0442\u044C \u0443\u0441\u0435\u0447\u0451\u043D API.",
      "i" = "\u0421\u043E\u043A\u0440\u0430\u0442\u0438\u0442\u0435 \u043F\u0435\u0440\u0438\u043E\u0434 \u0438\u043B\u0438 \u0434\u043E\u0431\u0430\u0432\u044C\u0442\u0435 \u0444\u0438\u043B\u044C\u0442\u0440, \u0447\u0442\u043E\u0431\u044B \u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C \u043F\u043E\u043B\u043D\u044B\u0435 \u0434\u0430\u043D\u043D\u044B\u0435."
    ))
  }
  cat("\u041F\u0440\u043E\u0446\u0435\u0441\u0441 \u0437\u0430\u0432\u0435\u0440\u0448\u0435\u043D. \u0412\u044B\u0433\u0440\u0443\u0436\u0435\u043D\u043E \u0441\u0442\u0440\u043E\u043A:", rows_count, "\n")

  if (save_report) {
    if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    file_name <- paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    full_path <- file.path(save_dir, file_name)
    utils::write.csv2(result, file = full_path, row.names = FALSE, fileEncoding = "UTF-8")
    cat("\u0424\u0430\u0439\u043B \u0443\u0441\u043F\u0435\u0448\u043D\u043E \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D \u0432 \u043F\u043E\u0434\u043F\u0430\u043F\u043A\u0443: ", full_path, "\n")
  }

  return(result)
}
