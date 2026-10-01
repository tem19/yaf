#' Получение списка рекламных кампаний
#'
#' Функция возвращает таблицу со списком всех кампаний рекламодателя.
#'
#' @param login Character. Логин в Яндексе.
#' @param fields Вектор полей из FieldNames сервиса.
#' @param type_fields Именованный список тип-специфичных полей,
#'   например list(TextCampaignFieldNames = c("BiddingStrategy")).
#' @param raw Если TRUE, вернуть сырой список объектов API без разбора.
#'
#' @return A data frame с ID кампаний, их названиями, статусами и типами.
#' @export
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_body_json resp_body_string
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' my_campaigns <- yaf_get_campaigns("my_login")
#' }
yaf_get_campaigns <- function(login,
                              fields = c("Id", "Name", "Status", "State", "Type",
                                         "StartDate", "Statistics"),
                              type_fields = list(
                                UnifiedCampaignFieldNames = c("BiddingStrategy",
                                                              "AttributionModel",
                                                              "TrackingParams")
                              ),
                              raw = FALSE) {
  params <- c(
    list(
      SelectionCriteria = list(
        Statuses = list("ACCEPTED", "DRAFT", "MODERATION", "REJECTED")
      ),
      FieldNames = fields
    ),
    type_fields
  )

  items <- yaf_api_get(login, "campaigns", params, progress = FALSE)

  if (raw) return(items)

  # Прежний формат: вложенные поля разворачиваются через unlist
  purrr::map_dfr(items, ~as.data.frame(t(unlist(.))))
}
