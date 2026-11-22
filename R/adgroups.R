#' Получение списка групп объявлений из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param fields Вектор полей из FieldNames сервиса.
#' @param type_fields Именованный список тип-специфичных полей,
#'   например list(TextAdGroupFeedParamsFieldNames = c("FeedId")).
#' @param raw Если TRUE, вернуть сырой список объектов API без разбора.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками adgroup_id, campaign_id, adgroup_name,
#'   status, type и остальными запрошенными полями. Вложенные объекты
#'   развёрнуты в колонки через точку, списки (RegionIds, NegativeKeywords)
#'   склеены через "; ". При raw = TRUE — список объектов API.
#' @export
#'
#' @examples
#' \dontrun{
#' groups <- yaf_get_adgroups("my_login", campaign_ids = c(123, 456))
#' }
yaf_get_adgroups <- function(login, campaign_ids = NULL,
                             fields = c("Id", "CampaignId", "Name", "Status", "Type"),
                             type_fields = list(),
                             raw = FALSE, token = NULL,
                             timeout = yaf_default_timeout()) {
  campaign_ids <- campaign_ids %||% yaf_all_campaign_ids(login, token = token, timeout = timeout)

  items <- yaf_api_get(
    login, "adgroups",
    params = c(list(FieldNames = fields), type_fields),
    batch_ids = campaign_ids, batch_field = "CampaignIds", batch_size = 10,
    token = token, timeout = timeout
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

  cli::cli_alert_success("\u0413\u043E\u0442\u043E\u0432\u043E! \u0412\u044B\u0433\u0440\u0443\u0436\u0435\u043D\u043E \u0433\u0440\u0443\u043F\u043F: {.val {nrow(df)}}")
  df
}
