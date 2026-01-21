#' Получение статистики из Яндекс Директа
#'
#' @param login Character. Логин в Яндексе.
#' @param date_from Character. Дата начала (ГГГГ-ММ-ДД).
#' @param date_to Character. Дата окончания (ГГГГ-ММ-ДД).
#' @param fields Character vector. Список полей для выгрузки (обязательный).
#' @param goals Numeric vector. Список ID целей Метрики (необязательно).
#' @export
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_body_string
#' @importFrom dplyr mutate across matches
#' @importFrom tidyr replace_na
#' @importFrom utils read.table flush.console
yaf_get_report <- function(login,
                           date_from,
                           date_to,
                           fields,
                           goals = NULL) {

  # Проверка обязательного аргумента fields
  if (missing(fields) || is.null(fields)) {
    stop("Ошибка: необходимо указать вектор полей в аргументе 'fields'.")
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
      ReportType = "CUSTOM_REPORT",
      DateRangeType = "CUSTOM_DATE",
      Format = "TSV",
      IncludeVAT = "YES"
    )
  )

  if(!is.null(goals)) {
    body$params$Goals <- as.list(as.numeric(goals))
    body$params$AttributionModels <- list("AUTO")
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
