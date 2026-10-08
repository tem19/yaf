#' Справочник полей отчётов API Яндекс Директа
#'
#' Таблица полей сервиса reports: где поле можно использовать и в каких
#' типах отчётов оно доступно. Загружается командой `data(yaf_fields_info)`.
#'
#' @format data.frame (tibble), 84 строки, 12 колонок:
#' \describe{
#'   \item{Имя поля}{Название поля для аргумента `fields` в [yaf_get_report()].}
#'   \item{FieldNames}{"+", если поле можно запросить в отчёте.}
#'   \item{Filter.Field}{"+", если по полю можно фильтровать (аргумент `filter`).}
#'   \item{OrderBy.Field}{"+", если по полю можно сортировать.}
#'   \item{ACCOUNT_PERFORMANCE_REPORT, AD_PERFORMANCE_REPORT,
#'     ADGROUP_PERFORMANCE_REPORT, CAMPAIGN_PERFORMANCE_REPORT,
#'     CRITERIA_PERFORMANCE_REPORT, CUSTOM_REPORT,
#'     REACH_AND_FREQUENCY_PERFORMANCE_REPORT,
#'     SEARCH_QUERY_PERFORMANCE_REPORT}{Роль поля в отчёте этого типа:
#'     "сегмент", "атрибут" или "–" (поле недоступно).}
#' }
#' @source <https://yandex.ru/dev/direct/doc/ru/reports/fields-list>
"yaf_fields_info"
