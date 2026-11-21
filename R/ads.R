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
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками ad_id (character: Id объявлений больше
#'   2^53 и не помещаются в numeric без потери точности), campaign_id,
#'   adgroup_id, type, status, state, title, title2, text, href,
#'   display_url_path, image_hash, video_creative_id.
#'   При raw = TRUE — список объектов API.
#' @export
#'
#' @examples
#' \dontrun{
#' ads <- yaf_get_ads("my_login", campaign_ids = c(123, 456))
#' }
yaf_get_ads <- function(login, campaign_ids = NULL, adgroup_ids = NULL, ad_ids = NULL,
                        fields = c("Id", "CampaignId", "AdGroupId", "Type", "Status", "State"),
                        type_fields = list(
                          TextAdFieldNames = c("Title", "Title2", "Text", "Href",
                                               "DisplayUrlPath", "AdImageHash", "VideoExtension"),
                          TextImageAdFieldNames = c("AdImageHash", "Href"),
                          ResponsiveAdFieldNames = c("Titles", "Texts", "Href",
                                                     "AdImages", "VideoExtensions"),
                          DynamicTextAdFieldNames = c("Text"),
                          MobileAppAdFieldNames = c("Title", "Text")
                        ),
                        raw = FALSE, token = NULL,
                        timeout = yaf_default_timeout()) {
  if (!is.null(ad_ids)) {
    ids <- ad_ids; id_field <- "Ids"; batch_size <- 10000
  } else if (!is.null(adgroup_ids)) {
    ids <- adgroup_ids; id_field <- "AdGroupIds"; batch_size <- 1000
  } else {
    ids <- campaign_ids %||% yaf_all_campaign_ids(login, token = token, timeout = timeout)
    id_field <- "CampaignIds"; batch_size <- 10
  }

  items <- yaf_api_get(
    login, "ads",
    params = c(list(FieldNames = fields), type_fields),
    batch_ids = ids, batch_field = id_field, batch_size = batch_size,
    token = token, timeout = timeout
  )

  if (raw) return(items)

  if (length(items) == 0) {
    cli::cli_alert_warning("\u041E\u0431\u044A\u044F\u0432\u043B\u0435\u043D\u0438\u044F \u043D\u0435 \u043D\u0430\u0439\u0434\u0435\u043D\u044B.")
    return(data.frame())
  }

  # Поля различаются по типу объявления. Неизвестные типы не разбираем,
  # но сообщаем о них, чтобы данные не терялись молча.
  ad_content <- function(x) {
    if (!is.null(x$TextAd) || !is.null(x$DynamicTextAd) || !is.null(x$MobileAppAd)) {
      content <- x$TextAd %||% x$DynamicTextAd %||% x$MobileAppAd
      return(list(
        title = content$Title %||% NA,
        title2 = content$Title2 %||% NA,
        text = content$Text %||% NA,
        href = content$Href %||% NA,
        display_url_path = content$DisplayUrlPath %||% NA,
        image_hash = content$AdImageHash %||% NA,
        video_creative_id = content$VideoExtension$CreativeId %||% NA
      ))
    }
    if (!is.null(x$ResponsiveAd)) {
      content <- x$ResponsiveAd
      paste_or_na <- function(v) {
        if (is.null(v) || length(v) == 0) return(NA_character_)
        paste(unlist(v), collapse = "; ")
      }
      video_items <- content$VideoExtensions$Items %||%
        content$VideoExtensions %||% list()
      video_ids <- vapply(video_items, function(v) {
        if (is.list(v)) as.character(v$CreativeId %||% NA_character_) else as.character(v)
      }, character(1))
      video_ids <- video_ids[!is.na(video_ids) & nzchar(video_ids)]
      return(list(
        title = paste_or_na(content$Titles),
        title2 = NA_character_,
        text = paste_or_na(content$Texts),
        href = content$Href %||% NA,
        display_url_path = content$DisplayUrlPath %||% NA,
        image_hash = paste_or_na(content$AdImages),
        video_creative_id = if (length(video_ids)) paste(video_ids, collapse = "; ") else NA_character_
      ))
    }
    if (!is.null(x$TextImageAd)) {
      content <- x$TextImageAd
      return(list(
        title = NA_character_, title2 = NA_character_, text = NA_character_,
        href = content$Href %||% NA, display_url_path = NA_character_,
        image_hash = content$AdImageHash %||% NA,
        video_creative_id = NA_character_
      ))
    }
    list(title = NA_character_, title2 = NA_character_, text = NA_character_,
         href = NA_character_, display_url_path = NA_character_,
         image_hash = NA_character_, video_creative_id = NA_character_)
  }

  unknown <- unique(vapply(items, function(x) {
    if (!is.null(x$TextAd) || !is.null(x$DynamicTextAd) ||
        !is.null(x$MobileAppAd) || !is.null(x$ResponsiveAd) ||
        !is.null(x$TextImageAd)) {
      return(NA_character_)
    }
    as.character(x$Type %||% "UNKNOWN")
  }, character(1)))
  unknown <- unknown[!is.na(unknown)]
  if (length(unknown) > 0) {
    cli::cli_warn(c(
      "!" = "\u0422\u0438\u043F\u044B \u043E\u0431\u044A\u044F\u0432\u043B\u0435\u043D\u0438\u0439 \u0431\u0435\u0437 \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E: {paste(unknown, collapse = ', ')}.",
      "i" = "\u0412\u0435\u0440\u043D\u0438\u0442\u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0447\u0435\u0440\u0435\u0437 raw = TRUE, \u0447\u0442\u043E\u0431\u044B \u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C \u0438\u0441\u0445\u043E\u0434\u043D\u044B\u0435 \u043F\u043E\u043B\u044F API."
    ))
  }

  rows <- lapply(items, function(x) {
    content <- ad_content(x)
    data.frame(
      ad_id            = as.character(x$Id),
      campaign_id      = x$CampaignId,
      adgroup_id       = x$AdGroupId,
      type             = x$Type %||% NA,
      status           = x$Status %||% NA,
      state            = x$State %||% NA,
      title            = content$title,
      title2           = content$title2,
      text             = content$text,
      href             = content$href,
      display_url_path = content$display_url_path,
      image_hash       = as.character(content$image_hash),
      video_creative_id = as.character(content$video_creative_id),
      stringsAsFactors = FALSE
    )
  })
  df <- dplyr::bind_rows(rows)

  cli::cli_alert_success("\u0423\u0441\u043F\u0435\u0448\u043D\u043E \u0432\u044B\u0433\u0440\u0443\u0436\u0435\u043D\u043E \u043E\u0431\u044A\u044F\u0432\u043B\u0435\u043D\u0438\u0439: {.val {nrow(df)}}")
  df
}
