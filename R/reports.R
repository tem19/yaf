#' Получение статистики из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param date_from Дата начала (ГГГГ-ММ-ДД). (необязательный, если не указать, установится дата неделю назад).
#' @param date_to Дата окончания (ГГГГ-ММ-ДД). (необязательный, если не указать, установится вчрешня дата).
#' @param fields Список полей для выгрузки (необязательный, если не указать, выгрузятся поля Date, Impressions, Clicks).
#' @param goals Список ID целей Метрики (необязательно).
#' @param attribution Character vector. Модели атрибуции. Можно указать одну или несколько.
#' По умолчанию используется "AUTO".
#'
#' Доступные модели:
#' \itemize{
#'   \item \code{FC} — Первый переход.
#'   \item \code{LC} — Последний переход.
#'   \item \code{LSC} — Последний значимый переход.
#'   \item \code{LYDC} — Последний переход из Яндекс Директа.
#'   \item \code{FCCD} — Первый переход кросс-девайс.
#'   \item \code{LSCCD} — Последний значимый переход кросс-девайс.
#'   \item \code{LYDCCD} — Последний переход из Яндекс Директа кросс-девайс.
#'   \item \code{AUTO} — Автоматическая атрибуция.
#' }
#'
#' @details
#' Модели атрибуции работают только при указании аргумента \code{goals}.
#' Если вы указываете несколько моделей, API вернет данные по каждой из них
#' (строки в отчете могут дублироваться для разных моделей, если не добавлен срез AttributionModel).
#'
#' @param filter Строка для фильтрации данных.
#' Должна быть оформлена в формате "Поле Оператор Значение".
#'
#' Разрешенные операторы:
#' \itemize{
#'   \item \code{EQUALS}, \code{NOT_EQUALS} — точное совпадение.
#'   \item \code{IN}, \code{NOT_IN} — вхождение в список (значения через запятую).
#'   \item \code{GREATER_THAN}, \code{LESS_THAN} — больше или меньше.
#'   \item \code{STARTS_WITH}, \code{ENDS_WITH} — текстовые фильтры.
#' }
#' @details
#' Фильтр передается в виде одной строки. Если значений несколько (для оператора IN),
#' их следует разделять запятой.
#' @param search_query_report Логическое значение TRUE означает, что запрашивается отчет по поисковым запросам. (необязательно, по-умоляанию выгрузится CUSTOM_REPORT).
#' @export
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_body_string
#' @importFrom dplyr mutate across matches
#' @importFrom tidyr replace_na
#' @importFrom utils read.table flush.console
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
#' # Фильтр по названию кампании
#' yaf_get_report(login, fields = c("CampaignName", "Clicks"),
#'                filter = "CampaignName STARTS_WITH Лето")
#'
#' # Фильтр по количеству кликов
#' yaf_get_report(login, fields = c("CampaignName", "Clicks"),
#'                filter = "Clicks GREATER_THAN 10")
#'
#' # Фильтр по списку ID кампаний
#' yaf_get_report(login, fields = c("CampaignName", "Clicks"),
#'                filter = "CampaignId IN 123456, 789012")
#' }

yaf_get_report <- function(login,
                           date_from = Sys.Date()-7,
                           date_to = Sys.Date()- 1,
                           fields = c("Date","Impressions","Clicks"),
                           goals = NULL,
                           atribution = NULL,
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
    if(!is.null(atribution)) {
      body$params$AttributionModels <- as.list(atribution)
    } else
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
