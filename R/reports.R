#' Получение статистики из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param date_from Дата начала (ГГГГ-ММ-ДД).
#' @param date_to Дата окончания (ГГГГ-ММ-ДД).
#' @param fields Список полей для выгрузки.
#' @param goals Список ID целей Метрики (необязательно).
#' @param attribution Модели атрибуции.
#' @param filter Строка для фильтрации данных.
#' @param numeric_fields_as_numeric Не менять тип полей.
#' @param search_query_report TRUE для отчета по поисковым запросам.
#' @param include_vat Включать ли НДС (YES/NO).
#' @param save_report Логическое значение. Если TRUE, сохраняет CSV в папку /reports.
#'
#' @export
yaf_get_report <- function(login,
                           date_from = Sys.Date()-7,
                           date_to = Sys.Date()- 1,
                           fields = c("Date","Impressions","Clicks"),
                           goals = NULL,
                           atribution = NULL,
                           filter = NULL,
                           search_query_report = FALSE,
                           numeric_fields_as_numeric = TRUE,
                           include_vat = TRUE,
                           save_report = FALSE) {

  # Определение типа отчета
  ReportType <- if (search_query_report) "SEARCH_QUERY_PERFORMANCE_REPORT" else "CUSTOM_REPORT"

  # 1. Получаем токен
  token <- get_yaf_token(login)

  # 2. Формируем тело запроса
  body <- list(
    params = list(
      SelectionCriteria = list(
        DateFrom = as.character(date_from),
        DateTo = as.character(date_to)
      ),
      FieldNames = fields,
      Page = list(Limit = 2000000L),
      ReportName = paste0("yaf_", login, "_", format(Sys.time(), "%Y%m%d%H%M%S")),
      ReportType = ReportType,
      DateRangeType = "CUSTOM_DATE",
      Format = "TSV"
    )
  )

  body$params$IncludeVAT <- if(include_vat) "YES" else "NO"

  if(!is.null(goals)) {
    body$params$Goals <- as.list(as.numeric(goals))
    body$params$AttributionModels <- if(!is.null(atribution)) as.list(atribution) else list("AUTO")
  }

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

  # 3. Внутренняя функция для отправки запроса
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

  # 4. Цикл ожидания отчета
  cat("Запрос отправлен. Ожидание формирования отчета для:", login, "\n")
  start_time <- proc.time()

  req <- send_request()
  resp_status <- httr2::resp_status(req)

  while(resp_status != 200) {
    if (resp_status %in% c(400, 401, 500)) {
      stop(paste("Ошибка API:", resp_status, httr2::resp_body_string(req)))
    }
    Sys.sleep(2)
    req <- send_request()
    resp_status <- httr2::resp_status(req)

    elapsed <- round((proc.time() - start_time)[["elapsed"]], 1)
    cat("\rПрошло времени:", elapsed, "сек. | Статус:", resp_status, "    ")
    utils::flush.console()
  }

  cat("\nОтчет получен. Обработка данных...\n")

  # 5. Чтение данных
  tsv <- httr2::resp_body_string(req)
  result <- utils::read.table(
    text = tsv, header = TRUE, sep = "\t",
    stringsAsFactors = FALSE, quote = "",
    comment.char = "", encoding = "UTF-8"
  )

  # 6. Приведение числовых типов
  if (numeric_fields_as_numeric == TRUE) {
    numeric_fields <- "Conversions_|Clicks|Impressions|Cost|Bounces|Profit|Revenue|Sessions|AvgImpressionPosition"
    result <- result |>
      dplyr::mutate(dplyr::across(
        dplyr::matches(numeric_fields),
        ~ suppressWarnings(tidyr::replace_na(as.numeric(.), 0))
      ))
  }

  # 7. Финальное сообщение
  rows_count <- nrow(result)
  cat("Процесс завершен. Выгружено строк:", rows_count, "\n")

  # 8. Автоматическое сохранение в папку /reports
  if (save_report == TRUE) {
    if (!dir.exists("reports")) {
      dir.create("reports")
    }

    file_name <- paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    full_path <- file.path("reports", file_name)

    utils::write.csv2(result, file = full_path, row.names = FALSE, fileEncoding = "UTF-8")
    cat("Файл успешно сохранен в подпапку: ", full_path, "\n")
  }

  return(result)
}
