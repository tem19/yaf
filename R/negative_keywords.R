#' Получение наборов минус-фраз
#'
#' @param login Логин в Яндексе.
#' @param raw Если TRUE, вернуть сырой список объектов API.
#'
#' @return data.frame: Id, Name, NegativeKeywords (через "; "), Associated.
#' @export
#'
#' @examples
#' \dontrun{
#' sets <- yaf_get_negative_keyword_sets("my_login")
#' }
yaf_get_negative_keyword_sets <- function(login, raw = FALSE) {
  items <- yaf_api_get(
    login, "negativekeywordsharedsets",
    params = list(FieldNames = c("Id", "Name", "NegativeKeywords", "Associated"))
  )

  if (raw) return(items)
  if (length(items) == 0) return(data.frame())

  purrr::map_dfr(items, function(x) {
    data.frame(
      Id = x$Id,
      Name = x$Name %||% NA,
      NegativeKeywords = paste(sort(unlist(x$NegativeKeywords)), collapse = "; "),
      Associated = x$Associated %||% NA,
      stringsAsFactors = FALSE
    )
  })
}
