#' Получение ключевых фраз из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно). Если не задан
#'   вместе с adgroup_ids, берутся все кампании аккаунта.
#' @param adgroup_ids Вектор ID групп (необязательно, приоритетнее campaign_ids).
#' @param fields Поля из FieldNames сервиса keywords.
#' @param raw Если TRUE, вернуть сырой список объектов API.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с полями из fields и колонками KeywordPhrase
#'   (фраза без минус-слов) и KeywordMinusWords (минус-слова через "; ").
#'   Ставки Bid (на поиске) и ContextBid (в сетях) переведены из
#'   микроединиц API в валюту аккаунта: 12.5 означает 12,50 руб.
#'   При автоматической стратегии ставки не применяются, но API их
#'   всё равно возвращает. При raw = TRUE — список объектов API
#'   (ставки в микроединицах).
#' @export
#'
#' @examples
#' \dontrun{
#' kw <- yaf_get_keywords("my_login", campaign_ids = c(123, 456))
#' }
yaf_get_keywords <- function(login, campaign_ids = NULL, adgroup_ids = NULL,
                             fields = c("Id", "CampaignId", "AdGroupId", "Keyword",
                                        "State", "Status", "ServingStatus",
                                        "Bid", "ContextBid", "StrategyPriority",
                                        "UserParam1", "UserParam2"),
                             raw = FALSE, token = NULL,
                             timeout = yaf_default_timeout()) {
  if (!is.null(adgroup_ids)) {
    batch_ids <- adgroup_ids
    batch_field <- "AdGroupIds"
    batch_size <- 1000
  } else {
    batch_ids <- campaign_ids %||% yaf_all_campaign_ids(login, token = token, timeout = timeout)
    batch_field <- "CampaignIds"
    batch_size <- 10
  }

  items <- yaf_api_get(
    login, "keywords",
    params = list(FieldNames = fields),
    batch_ids = batch_ids, batch_field = batch_field, batch_size = batch_size,
    token = token, timeout = timeout
  )

  if (raw) return(items)

  df <- yaf_items_to_df(items)

  # API отдаёт ставки в микроединицах: 1 000 000 = 1 единица валюты
  for (col in intersect(c("Bid", "ContextBid"), names(df))) {
    df[[col]] <- as.numeric(df[[col]]) / 1e6
  }

  if ("Keyword" %in% names(df)) {
    parts <- yaf_split_keyword(df$Keyword)
    df$KeywordPhrase <- parts$phrase
    df$KeywordMinusWords <- parts$minus
  }

  cli::cli_alert_success("\u0412\u044B\u0433\u0440\u0443\u0436\u0435\u043D\u043E \u0444\u0440\u0430\u0437: {.val {nrow(df)}}")
  df
}

#' Разделение фразы на саму фразу и минус-слова
#'
#' "---autotargeting" (автотаргетинг) минус-словом не считается.
#'
#' @param keyword Вектор фраз в формате API: "фраза -минус -слова".
#'
#' @return Список из двух векторов той же длины: phrase (фраза без
#'   минус-слов) и minus (минус-слова без "-", по алфавиту, через "; ").
#' @keywords internal
yaf_split_keyword <- function(keyword) {
  keyword[is.na(keyword)] <- ""
  res <- lapply(strsplit(trimws(keyword), "\\s+"), function(tokens) {
    is_minus <- startsWith(tokens, "-") & !startsWith(tokens, "---")
    c(
      phrase = paste(tokens[!is_minus], collapse = " "),
      minus  = paste(sort(sub("^-", "", tokens[is_minus])), collapse = "; ")
    )
  })
  list(
    phrase = vapply(res, `[[`, "", "phrase"),
    minus  = vapply(res, `[[`, "", "minus")
  )
}
