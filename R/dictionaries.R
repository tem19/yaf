#' Получение справочника регионов Яндекс Директа
#'
#' Возвращает дерево регионов (ID, название, родительский ID, тип).
#' https://yandex.ru/dev/direct/doc/ru/dictionaries/get
#'
#' @param login Логин в Яндексе.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками region_id, parent_id, region_name,
#'   region_type. Если справочник пуст — пустой data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' regions <- yaf_get_regions("my_login")
#' }
yaf_get_regions <- function(login, token = NULL, timeout = yaf_default_timeout()) {
  token <- token %||% get_yaf_token(login)

  cli::cli_inform("i \u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0441\u043F\u0440\u0430\u0432\u043E\u0447\u043D\u0438\u043A\u0430 \u0440\u0435\u0433\u0438\u043E\u043D\u043E\u0432...")

  res <- yaf_api_request(
    login, "dictionaries",
    list(method = "get", params = list(DictionaryNames = as.list("GeoRegions"))),
    token,
    timeout = timeout
  )

  regions <- res$result$GeoRegions %||% list()

  df <- yaf_items_to_df(regions)
  if (nrow(df) > 0) {
    df <- data.frame(
      region_id   = df$GeoRegionId,
      parent_id   = if (!is.null(df$ParentId)) df$ParentId else NA,
      region_name = df$GeoRegionName,
      region_type = df$GeoRegionType,
      stringsAsFactors = FALSE
    )
  } else {
    df <- data.frame(
      region_id = numeric(0), parent_id = numeric(0),
      region_name = character(0), region_type = character(0),
      stringsAsFactors = FALSE
    )
  }

  cli::cli_alert_success("\u0421\u043F\u0440\u0430\u0432\u043E\u0447\u043D\u0438\u043A \u0440\u0435\u0433\u0438\u043E\u043D\u043E\u0432 \u0437\u0430\u0433\u0440\u0443\u0436\u0435\u043D: {.val {nrow(df)}} \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432.")
  df
}
