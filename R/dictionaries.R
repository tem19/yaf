#' Получение справочника регионов Яндекс Директа
#'
#' Возвращает дерево регионов (ID, название, родительский ID, тип).
#' https://yandex.ru/dev/direct/doc/ru/dictionaries/get
#'
#' @param login Логин в Яндексе.
#'
#' @export
yaf_get_regions <- function(login) {

  token <- get_yaf_token(login)

  body <- list(
    method = "get",
    params = list(
      DictionaryNames = as.list("GeoRegions")
    )
  )

  cli::cli_inform("i Загрузка справочника регионов...")

  res <- httr2::request("https://api.direct.yandex.com/json/v5/dictionaries") |>
    httr2::req_headers(
      Authorization = paste0("Bearer ", token),
      'Client-Login' = login,
      'Accept-Language' = "ru"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (!is.null(res$error)) {
    cli::cli_alert_danger("Ошибка API: {res$error$error_detail}")
    return(NULL)
  }

  regions <- res$result$GeoRegions

  # Парсинг в удобный data.frame
  df <- purrr::map_dfr(regions, function(x) {
    data.frame(
      region_id   = x$GeoRegionId,
      parent_id   = if (!is.null(x$ParentId)) x$ParentId else NA,
      region_name = x$GeoRegionName,
      region_type = x$GeoRegionType,
      stringsAsFactors = FALSE
    )
  })

  cli::cli_alert_success("Справочник регионов загружен: {.val {nrow(df)}} объектов.")
  return(df)
}
