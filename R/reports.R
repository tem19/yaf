#' Получение статистики из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param date_from Дата начала (ГГГГ-ММ-ДД). (необязательный, если не указать, установится дата неделю назад).
#' @param date_to Дата окончания (ГГГГ-ММ-ДД). (необязательный, если не указать, установится вчрешня дата).
#' @param fields Список полей для выгрузки (необязательный, если не указать, выгрузятся поля Date, Impressions, Clicks).
#' @param goals Список ID целей Метрики (необязательно).
#' @param search_query_report Логическое значение TRUE означает, что запрашивается отчет по поисковым запросам. (необязательно, по-умоляанию выгрузится CUSTOM_REPORT).
#' @export
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_body_string
#' @importFrom dplyr mutate across matches
#' @importFrom tidyr replace_na
#' @importFrom utils read.table flush.console
#'

#' @examples
#' \dontrun{
#' # Базовая выгрузка
#' report <- yaf_get_report(
#'   login = "my_login",
#'   fields = c("Date", "Clicks", "Cost")
#' )
#'
#' # Выгрузка поисковых запросов
#' queries <- yaf_get_report(
#'   login = "my_login",
#'   fields = c("Date", "Query", "Clicks"),
#'   search_query_report = TRUE
#' )
#' }

yaf_get_report <- function(login,
                           date_from = Sys.Date()-7,
                           date_to = Sys.Date()- 1,
                           fields = c("Date","Impressions","Clicks"),
                           goals = NULL,
                           filter = NULL,
                           search_query_report = FALSE) {

  # если search_query_report = TRUE, то меняем тип отчета на "SEARCH_QUERY_REPORT"
  if (search_query_report == TRUE) {
    ReportType <- "SEARCH_QUERY_PERFORMANCE_REPORT"
  } else {
    ReportType <- "CUSTOM_REPORT"
  }

  # 1. Получаем токен
  token <- get_yaf_token(login)

  # 2. Формируем тело запроса
  body <- list(
    params = list(
      SelectionCriteria = list(
        DateFrom = date_from,
        DateTo = date_to
      ),
      FieldNames = fields,
      Page = list(
        Limit = 2000000L
      ),
      ReportName = paste0("yaf_", login, "_", format(Sys.time(), "%Y%m%d%H%M%S")),
      ReportType = ReportType,
      DateRangeType = "CUSTOM_DATE",
      Format = "TSV",
      IncludeVAT = "YES"
    )
  )

  # добавляем цели
  if(!is.null(goals)) {
    body$params$Goals <- as.list(as.numeric(goals))
    body$params$AttributionModels <- list("AUTO")
  }

  # добавляем фильтр
  if (!is.null(filter)) {
    # Используем регулярку, чтобы делить по любому количеству пробелов
    parts <- unlist(strsplit(trimws(filter), "\\s+"))

    if (length(parts) < 3) stop("Ошибка: фильтр должен быть в формате 'Поле Оператор Значение'")

    field <- parts[1]
    operator <- parts[2]

    # Собираем значения и чистим их
    value_raw <- paste(parts[3:length(parts)], collapse = " ")
    values <- unlist(strsplit(value_raw, ",\\s*"))

    # Важно: API часто требует строки в Values, даже если это числа,
    # но в JSON они должны уходить как массив.
    # auto_unbox = TRUE в req_body_json может превратить массив из 1 элемента в строку.
    # Чтобы этого избежать, используем I()
    filter_list <- list(
      list(
        Field = field,
        Operator = operator,
        Values = as.list(values)
      )
    )

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
  message("Запрос отправлен. Ожидание формирования отчета для: ", login)
  start_time <- proc.time()

  req <- send_request()
  resp_status <- httr2::resp_status(req)

  while(resp_status != 200) {
    Sys.sleep(2)
    req <- send_request()
    resp_status <- httr2::resp_status(req)

    elapsed <- round((proc.time() - start_time)[["elapsed"]], 1)
    cat("\rПрошло времени:", elapsed, "сек. | Статус:", resp_status, "   ")
    utils::flush.console()
  }

  cat("\nОтчет получен. Обработка данных...\n")

  # 5. Чтение данных
  tsv <- httr2::resp_body_string(req)

  result <- utils::read.table(
    text = tsv,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    quote = "",
    comment.char = "",
    encoding = "UTF-8"
  )

  # 6. Приведение числовых типов
  numeric_fields <- "Conversions_|Clicks|Impressions|Cost|Bounces|Profit|Revenue|Sessions|AvgImpressionPosition"

  result <- result |>
    dplyr::mutate(dplyr::across(
      dplyr::matches(numeric_fields),
      ~ suppressWarnings(tidyr::replace_na(as.numeric(.), 0))
    ))

  return(result)
}
