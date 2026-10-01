#' Получение списка групп объявлений из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param fields Вектор полей из FieldNames сервиса.
#' @param type_fields Именованный список тип-специфичных полей,
#'   например list(TextAdGroupFeedParamsFieldNames = c("FeedId")).
#' @param raw Если TRUE, вернуть сырой список объектов API без разбора.
#'
#' @export
yaf_get_adgroups <- function(login, campaign_ids = NULL,
                             fields = c("Id", "CampaignId", "Name", "Status", "Type"),
                             type_fields = list(),
                             raw = FALSE) {
  campaign_ids <- campaign_ids %||% yaf_all_campaign_ids(login)

  items <- yaf_api_get(
    login, "adgroups",
    params = c(list(FieldNames = fields), type_fields),
    batch_ids = campaign_ids, batch_field = "CampaignIds", batch_size = 10
  )

  if (raw) return(items)
  if (length(items) == 0) return(data.frame())

  df <- yaf_items_to_df(items) |>
    dplyr::rename(dplyr::any_of(c(
      adgroup_id = "Id", campaign_id = "CampaignId", adgroup_name = "Name",
      status = "Status", type = "Type"
    ))) |>
    dplyr::relocate(dplyr::any_of(c("adgroup_id", "campaign_id", "adgroup_name",
                                    "status", "type")))

  cli::cli_alert_success("Готово! Выгружено групп: {.val {nrow(df)}}")
  df
}
