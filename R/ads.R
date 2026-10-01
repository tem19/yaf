#' Получение объявлений из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param adgroup_ids Вектор ID групп (необязательно).
#' @param ad_ids Вектор ID конкретных объявлений (необязательно).
#' @param fields Вектор полей из FieldNames сервиса.
#' @param type_fields Именованный список тип-специфичных полей,
#'   например list(TextAdFieldNames = c("Title", "Href")).
#' @param raw Если TRUE, вернуть сырой список объектов API без разбора.
#'
#' @export
yaf_get_ads <- function(login, campaign_ids = NULL, adgroup_ids = NULL, ad_ids = NULL,
                        fields = c("Id", "CampaignId", "AdGroupId", "Type", "Status", "State"),
                        type_fields = list(
                          TextAdFieldNames = c("Title", "Title2", "Text", "Href", "DisplayUrlPath"),
                          DynamicTextAdFieldNames = c("Text"),
                          MobileAppAdFieldNames = c("Title", "Text")
                        ),
                        raw = FALSE) {
  if (!is.null(ad_ids)) {
    ids <- ad_ids; id_field <- "Ids"; batch_size <- 10000
  } else if (!is.null(adgroup_ids)) {
    ids <- adgroup_ids; id_field <- "AdGroupIds"; batch_size <- 1000
  } else {
    ids <- campaign_ids %||% yaf_all_campaign_ids(login)
    id_field <- "CampaignIds"; batch_size <- 10
  }

  items <- yaf_api_get(
    login, "ads",
    params = c(list(FieldNames = fields), type_fields),
    batch_ids = ids, batch_field = id_field, batch_size = batch_size
  )

  if (raw) return(items)

  if (length(items) == 0) {
    cli::cli_alert_warning("Объявления не найдены.")
    return(data.frame())
  }

  df <- purrr::map_dfr(items, function(x) {
    content <- x$TextAd %||% x$DynamicTextAd %||% x$MobileAppAd
    data.frame(
      ad_id            = x$Id,
      campaign_id      = x$CampaignId,
      adgroup_id       = x$AdGroupId,
      type             = x$Type %||% NA,
      status           = x$Status %||% NA,
      state            = x$State %||% NA,
      title            = content$Title %||% NA,
      title2           = content$Title2 %||% NA,
      text             = content$Text %||% NA,
      href             = content$Href %||% NA,
      display_url_path = content$DisplayUrlPath %||% NA,
      stringsAsFactors = FALSE
    )
  })

  cli::cli_alert_success("Успешно выгружено объявлений: {.val {nrow(df)}}")
  df
}
