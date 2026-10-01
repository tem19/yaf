#' Текущее время сервера Яндекс Директа
#'
#' Возвращает текущее время на сервере API (метод `changes.checkDictionaries`
#' без параметров). Значение удобно сохранить и использовать как `timestamp`
#' при следующем вызове [yaf_check_campaigns()] или [yaf_check_changes()].
#'
#' @param login Character. Логин в Яндексе.
#'
#' @return Character. Время в формате `YYYY-MM-DDThh:mm:ssZ` (UTC).
#' @export
#'
#' @examples
#' \dontrun{
#' ts <- yaf_get_server_timestamp("my_login")
#' }
yaf_get_server_timestamp <- function(login) {
  res <- yaf_changes_request(login, "checkDictionaries", structure(list(), names = character(0)))
  res$Timestamp
}

#' Проверка изменений в кампаниях аккаунта
#'
#' Обертка над методом `changes.checkCampaigns`. Возвращает кампании, в которых
#' после указанного времени произошли изменения: в параметрах самой кампании
#' (`SELF`), в ее группах, объявлениях, фразах и т. п. (`CHILDREN`) или в
#' статистике (`STAT`).
#'
#' @param login Character. Логин в Яндексе.
#' @param timestamp Время, начиная с которого искать изменения. Строка в
#'   формате `YYYY-MM-DDThh:mm:ssZ` (UTC), `POSIXct` или `Date`.
#'   Обычно используется значение атрибута `timestamp` из предыдущего вызова.
#'
#' @return A data frame с колонками `campaign_id`, `changes_in` (через запятую),
#'   `self`, `children`, `stat` (логические). Время сервера на момент запроса
#'   сохраняется в атрибуте `timestamp` — его следует передать в следующий вызов.
#' @export
#'
#' @examples
#' \dontrun{
#' changed <- yaf_check_campaigns("my_login", Sys.time() - 86400)
#' next_ts <- attr(changed, "timestamp")
#' }
yaf_check_campaigns <- function(login, timestamp) {
  ts <- yaf_format_timestamp(timestamp)

  res <- yaf_changes_request(login, "checkCampaigns", list(Timestamp = ts))

  camps <- res$Campaigns
  if (length(camps) == 0) {
    df <- data.frame(
      campaign_id = numeric(0), changes_in = character(0),
      self = logical(0), children = logical(0), stat = logical(0)
    )
  } else {
    df <- purrr::map_dfr(camps, function(x) {
      changes <- unlist(x$ChangesIn)
      data.frame(
        campaign_id = as.numeric(x$CampaignId),
        changes_in = paste(changes, collapse = ","),
        self = "SELF" %in% changes,
        children = "CHILDREN" %in% changes,
        stat = "STAT" %in% changes
      )
    })
  }

  attr(df, "timestamp") <- res$Timestamp
  cli::cli_alert_success("Кампаний с изменениями: {.val {nrow(df)}}. Время сервера: {res$Timestamp}")
  df
}

#' Проверка изменений в кампаниях, группах и объявлениях
#'
#' Обертка над методом `changes.check`. Возвращает ID измененных кампаний,
#' групп и объявлений, а также даты, начиная с которых изменилась статистика
#' кампаний. Нужно указать ровно один из параметров `campaign_ids`,
#' `adgroup_ids` или `ad_ids`. Большие списки ID автоматически разбиваются на
#' пакеты по лимитам API (3000 кампаний, 10000 групп, 50000 объявлений).
#'
#' @param login Character. Логин в Яндексе.
#' @param timestamp Время, начиная с которого искать изменения. Строка в
#'   формате `YYYY-MM-DDThh:mm:ssZ` (UTC), `POSIXct` или `Date`.
#' @param campaign_ids Вектор ID кампаний. Если не задан ни один из
#'   `campaign_ids`, `adgroup_ids`, `ad_ids`, берутся все кампании аккаунта.
#' @param adgroup_ids Вектор ID групп.
#' @param ad_ids Вектор ID объявлений.
#' @param field_names Какие данные вернуть: любые из `"CampaignIds"`,
#'   `"AdGroupIds"`, `"AdIds"`, `"CampaignsStat"`.
#'
#' @return Список:
#'   * `modified` — список векторов `campaign_ids`, `adgroup_ids`, `ad_ids`
#'     с объектами, измененными после `timestamp`;
#'   * `not_found` — ID из запроса, которые не найдены;
#'   * `unprocessed` — ID, которые не удалось обработать (например, из-за
#'     слишком большого числа изменений) — их стоит запросить повторно;
#'   * `campaigns_stat` — data frame `campaign_id`, `border_date`: дата, начиная
#'     с которой изменилась статистика кампании;
#'   * `timestamp` — время сервера на момент запроса, для следующего вызова.
#' @export
#'
#' @examples
#' \dontrun{
#' ch <- yaf_check_changes("my_login", "2026-09-01T00:00:00Z")
#' ch$modified$ad_ids
#' ch$campaigns_stat
#' }
yaf_check_changes <- function(login,
                              timestamp,
                              campaign_ids = NULL,
                              adgroup_ids = NULL,
                              ad_ids = NULL,
                              field_names = c("CampaignIds", "AdGroupIds", "AdIds", "CampaignsStat")) {
  ts <- yaf_format_timestamp(timestamp)
  field_names <- match.arg(field_names, several.ok = TRUE)

  given <- !c(is.null(campaign_ids), is.null(adgroup_ids), is.null(ad_ids))
  if (sum(given) > 1) {
    cli::cli_abort("Укажите только один из параметров: {.arg campaign_ids}, {.arg adgroup_ids} или {.arg ad_ids}.")
  }

  if (sum(given) == 0) {
    cli::cli_inform("i Поиск кампаний для {login}...")
    camps <- yaf_get_campaigns(login)
    col_idx <- grep("^id$", names(camps), ignore.case = TRUE)
    if (length(col_idx) == 0) stop("Колонка 'id' не найдена.")
    campaign_ids <- camps[[col_idx]]
  }

  if (!is.null(campaign_ids)) {
    ids <- campaign_ids; id_field <- "CampaignIds"; batch_size <- 3000
  } else if (!is.null(adgroup_ids)) {
    ids <- adgroup_ids; id_field <- "AdGroupIds"; batch_size <- 10000
  } else {
    ids <- ad_ids; id_field <- "AdIds"; batch_size <- 50000
  }

  ids <- unique(as.numeric(ids))
  batches <- split(ids, ceiling(seq_along(ids) / batch_size))

  empty_ids <- function() list(campaign_ids = numeric(0), adgroup_ids = numeric(0), ad_ids = numeric(0))
  out <- list(
    modified = empty_ids(),
    not_found = empty_ids(),
    unprocessed = empty_ids(),
    campaigns_stat = data.frame(campaign_id = numeric(0), border_date = as.Date(character(0))),
    timestamp = NULL
  )

  append_ids <- function(acc, block) {
    if (is.null(block)) return(acc)
    acc$campaign_ids <- c(acc$campaign_ids, as.numeric(unlist(block$CampaignIds)))
    acc$adgroup_ids  <- c(acc$adgroup_ids,  as.numeric(unlist(block$AdGroupIds)))
    acc$ad_ids       <- c(acc$ad_ids,       as.numeric(unlist(block$AdIds)))
    acc
  }

  for (batch in batches) {
    params <- list(FieldNames = as.list(field_names), Timestamp = ts)
    params[[id_field]] <- as.list(batch)

    res <- yaf_changes_request(login, "check", params)

    out$modified    <- append_ids(out$modified, res$Modified)
    out$not_found   <- append_ids(out$not_found, res$NotFound)
    out$unprocessed <- append_ids(out$unprocessed, res$Unprocessed)

    if (length(res$CampaignsStat) > 0) {
      out$campaigns_stat <- rbind(out$campaigns_stat, purrr::map_dfr(res$CampaignsStat, function(x) {
        data.frame(campaign_id = as.numeric(x$CampaignId), border_date = as.Date(x$BorderDate))
      }))
    }

    # Берем время первого пакета: так при следующем вызове не будут пропущены
    # изменения, произошедшие во время обработки остальных пакетов
    if (is.null(out$timestamp)) out$timestamp <- res$Timestamp
  }

  n_unprocessed <- length(unlist(out$unprocessed))
  if (n_unprocessed > 0) {
    cli::cli_warn("Не обработано объектов: {n_unprocessed}. Запросите их повторно (см. {.field unprocessed}).")
  }

  cli::cli_alert_success(
    "Изменено: кампаний {.val {length(out$modified$campaign_ids)}}, групп {.val {length(out$modified$adgroup_ids)}}, объявлений {.val {length(out$modified$ad_ids)}}."
  )
  out
}

#' Internal: перевод времени в формат API `YYYY-MM-DDThh:mm:ssZ` (UTC)
#' @param x Character, POSIXct или Date.
#' @keywords internal
#' @noRd
yaf_format_timestamp <- function(x) {
  if (missing(x) || is.null(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("Аргумент {.arg timestamp} обязателен (строка 'YYYY-MM-DDThh:mm:ssZ', POSIXct или Date).")
  }
  if (inherits(x, "Date")) x <- as.POSIXct(format(x), tz = "UTC")
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  x <- as.character(x)
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", x)) {
    cli::cli_abort("Неверный формат {.arg timestamp}: {.val {x}}. Ожидается 'YYYY-MM-DDThh:mm:ssZ'.")
  }
  x
}

#' Internal: запрос к сервису changes
#' @keywords internal
#' @noRd
yaf_changes_request <- function(login, method, params) {
  token <- get_yaf_token(login)

  res <- httr2::request("https://api.direct.yandex.com/json/v5/changes") |>
    httr2::req_headers(
      Authorization = paste0("Bearer ", token),
      'Client-Login' = login,
      'Accept-Language' = "ru"
    ) |>
    httr2::req_body_json(list(method = method, params = params)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (!is.null(res$error)) {
    cli::cli_abort(c(
      "Ошибка API Яндекс Директа ({res$error$error_code}): {res$error$error_string}",
      "i" = "{res$error$error_detail}"
    ))
  }

  res$result
}
