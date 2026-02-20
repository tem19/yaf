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
#'
#' @export
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
                           max_tries = 60L) {

  # Валидация аргументов
  if (missing(login) || is.null(login) || !is.character(login) || length(login) != 1L || !nzchar(login)) {
    stop("Аргумент 'login' должен быть непустой строкой.", call. = FALSE)
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
          "Некорректный 'date_range_type': %s. Допустимые значения: %s",
          date_range_type,
          paste(allowed_presets, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (!is.logical(include_vat) || length(include_vat) != 1L) {
    stop("Аргумент 'include_vat' должен быть логическим значением длины 1.", call. = FALSE)
  }

  if (!is.logical(save_report) || length(save_report) != 1L) {
    stop("Аргумент 'save_report' должен быть логическим значением длины 1.", call. = FALSE)
  }

  if (!is.character(save_dir) || length(save_dir) != 1L || !nzchar(save_dir)) {
    stop("Аргумент 'save_dir' должен быть непустой строкой.", call. = FALSE)
  }

  if (!is.numeric(max_tries) || length(max_tries) != 1L || max_tries <= 0) {
    stop("Аргумент 'max_tries' должен быть положительным числом.", call. = FALSE)
  }

  max_tries <- as.integer(max_tries)

  # Определение типа отчета
  ReportType <- if (search_query_report) "SEARCH_QUERY_PERFORMANCE_REPORT" else "CUSTOM_REPORT"

  # 1. Получаем токен
  token <- get_yaf_token(login)

  # 2. Логика определения периода
  if (!is.null(date_range_type)) {
    PeriodType <- date_range_type
    selection_criteria <- setNames(list(), character(0))
  } else {
    PeriodType <- "CUSTOM_DATE"
    selection_criteria <- list(
      DateFrom = as.character(date_from),
      DateTo = as.character(date_to)
    )
  }

  # 3. Формируем тело запроса
  body <- list(
    params = list(
      SelectionCriteria = selection_criteria,
      FieldNames = fields,
      Page = list(Limit = 2000000L),
      ReportName = paste0("yaf_", login, "_", format(Sys.time(), "%Y%m%d%H%M%S")),
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
    if (length(parts) < 3) stop("Ошибка: фильтр должен быть в формате 'Поле Оператор Значение'")

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
      httr2::req_perform()
  }

  cat("Запрос отправлен. Период:", PeriodType, "| Логин:", login, "\n")
  start_time <- proc.time()

  req <- send_request()
  resp_status <- httr2::resp_status(req)
  attempt <- 1L

  while(resp_status != 200 && attempt < max_tries) {
    if (resp_status %in% c(400, 401, 500)) {
      stop(paste("Ошибка API:", resp_status, httr2::resp_body_string(req)))
    }
    Sys.sleep(2)
    req <- send_request()
    resp_status <- httr2::resp_status(req)
    attempt <- attempt + 1L

    elapsed <- round((proc.time() - start_time)[["elapsed"]], 1)
    cat("\rПрошло времени:", elapsed, "сек. | Статус:", resp_status, "    ")
    utils::flush.console()
  }

  if (resp_status != 200) {
    stop(
      sprintf(
        "Отчет не был готов после %d попыток. Последний HTTP-статус: %s",
        max_tries,
        resp_status
      ),
      call. = FALSE
    )
  }

  cat("\nОтчет получен. Обработка данных...\n")
  tsv <- httr2::resp_body_string(req)
  result <- utils::read.table(text = tsv, header = TRUE, sep = "\t", stringsAsFactors = FALSE, quote = "", comment.char = "", encoding = "UTF-8")

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
  cat("Процесс завершен. Выгружено строк:", rows_count, "\n")

  if (save_report) {
    if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    file_name <- paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    full_path <- file.path(save_dir, file_name)
    utils::write.csv2(result, file = full_path, row.names = FALSE, fileEncoding = "UTF-8")
    cat("Файл успешно сохранен в подпапку: ", full_path, "\n")
  }

  return(result)
}
