#' Получение списка рекламных кампаний
#'
#' Функция возвращает таблицу со списком всех кампаний рекламодателя.
#'
#' Поля из тип-специфичных блоков (TextCampaign, UnifiedCampaign и т. д.)
#' выводятся без префикса типа, чтобы у кампаний разных типов были одни
#' и те же колонки: BiddingStrategy.Search.BiddingStrategyType,
#' AttributionModel и т. д. Тип кампании — в колонке Type.
#'
#' @param login Character. Логин в Яндексе.
#' @param fields Вектор полей из FieldNames сервиса.
#' @param type_fields Именованный список тип-специфичных полей,
#'   например list(TextCampaignFieldNames = c("BiddingStrategy")).
#' @param raw Если TRUE, вернуть сырой список объектов API без разбора.
#'
#' @return data.frame, одна строка на кампанию. Вложенные поля развёрнуты
#'   в колонки с именами через точку (Statistics.Clicks,
#'   BiddingStrategy.Search.BiddingStrategyType), списки
#'   (NegativeKeywords, NegativeKeywordSharedSetIds) склеены через "; ",
#'   настройки Settings — в виде "OPTION=VALUE; ...". При raw = TRUE —
#'   список объектов API.
#' @export
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_body_json resp_body_string
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' my_campaigns <- yaf_get_campaigns("my_login")
#' my_campaigns[, c("Name", "BiddingStrategy.Search.BiddingStrategyType",
#'                  "NegativeKeywords")]
#' }
yaf_get_campaigns <- function(login,
                              fields = c("Id", "Name", "Status", "State", "Type",
                                         "StartDate", "Statistics", "NegativeKeywords"),
                              type_fields = list(
                                TextCampaignFieldNames = c("BiddingStrategy",
                                                           "AttributionModel",
                                                           "NegativeKeywordSharedSetIds"),
                                UnifiedCampaignFieldNames = c("BiddingStrategy",
                                                              "AttributionModel",
                                                              "TrackingParams",
                                                              "NegativeKeywordSharedSetIds"),
                                MobileAppCampaignFieldNames = c("BiddingStrategy",
                                                                "NegativeKeywordSharedSetIds"),
                                CpmBannerCampaignFieldNames = c("BiddingStrategy")
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
  if (length(items) == 0) return(data.frame())

  rows <- lapply(items, function(x) {
    # Блок TextCampaign / UnifiedCampaign / ... поднимаем на верхний уровень
    block <- grep("Campaign$", names(x), value = TRUE)
    for (b in block) {
      x <- c(x[setdiff(names(x), b)], x[[b]])
    }
    yaf_flatten_object(x)
  })

  dplyr::bind_rows(rows)
}
