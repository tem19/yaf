`%||%` <- function(x, y) if (is.null(x)) y else x

yaf_sleep <- function(seconds) Sys.sleep(seconds)

#' Ошибка API с кодом
#' @keywords internal
yaf_api_error <- function(message, code = NA_integer_, service = NA_character_,
                          request_id = NA_character_) {
  structure(
    class = c("yaf_api_error", "error", "condition"),
    list(message = message, call = NULL, code = code,
         service = service, request_id = request_id)
  )
}

#' Разбор заголовка Units: "потрачено/осталось/лимит"
#' @keywords internal
yaf_parse_units <- function(header) {
  if (is.null(header) || is.na(header)) {
    return(c(spent = NA_real_, rest = NA_real_, limit = NA_real_))
  }
  v <- as.numeric(strsplit(header, "/", fixed = TRUE)[[1]])
  stats::setNames(v[1:3], c("spent", "rest", "limit"))
}

#' Подготовка params к сериализации
#'
#' httr2 сериализует тело с auto_unbox = TRUE, поэтому вектор из одного
#' элемента (FieldNames = "Id") уйдёт строкой, а не массивом, и API вернёт
#' ошибку. Все *FieldNames принудительно превращаем в list().
#' Пустой SelectionCriteria должен уйти как {}, а не [].
#' @keywords internal
yaf_prepare_params <- function(params) {
  for (nm in grep("FieldNames$", names(params), value = TRUE)) {
    params[[nm]] <- as.list(params[[nm]])
  }
  if (length(params$SelectionCriteria) == 0) {
    params$SelectionCriteria <- stats::setNames(list(), character(0))
  }
  params
}

#' Один запрос к API с ретраями
#' @keywords internal
yaf_api_request <- function(login, service, body, token,
                            max_retries = 5,
                            retry_codes = c(52, 506, 1000, 1001, 1002)) {
  attempt <- 0L

  repeat {
    attempt <- attempt + 1L

    resp <- tryCatch(
      httr2::request(paste0("https://api.direct.yandex.com/json/v5/", service)) |>
        httr2::req_headers(
          Authorization = paste0("Bearer ", token),
          `Client-Login` = login,
          `Accept-Language` = "ru"
        ) |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(),
      error = function(e) e
    )

    retry_reason <- NULL

    if (inherits(resp, "error")) {
      retry_reason <- paste("Сетевая ошибка:", conditionMessage(resp))
    } else {
      status <- httr2::resp_status(resp)
      parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
      err <- parsed$error

      if (!is.null(err)) {
        code <- as.integer(err$error_code)
        msg <- sprintf("[%s] Ошибка API %s: %s. %s",
                       service, code, err$error_string, err$error_detail %||% "")
        if (identical(code, 53L)) {
          msg <- paste0(msg, " Перевыпустите токен: yaf_get_token('", login, "').")
        }
        if (!code %in% retry_codes) {
          stop(yaf_api_error(msg, code, service, err$request_id %||% NA_character_))
        }
        retry_reason <- msg
      } else if (status >= 500) {
        retry_reason <- paste("HTTP", status)
      } else if (status != 200 || is.null(parsed)) {
        stop(yaf_api_error(
          sprintf("[%s] HTTP %s: %s", service, status, httr2::resp_body_string(resp)),
          service = service
        ))
      } else {
        return(list(
          result = parsed$result,
          units  = yaf_parse_units(httr2::resp_header(resp, "Units"))
        ))
      }
    }

    if (attempt > max_retries) {
      stop(yaf_api_error(
        sprintf("[%s] Запрос не выполнен за %d попыток. Последняя ошибка: %s",
                service, attempt, retry_reason),
        service = service
      ))
    }

    wait <- min(2^(attempt - 1), 60)
    # retry_reason подставляется как значение, фигурные скобки в тексте ошибки
    # не интерпретируются cli
    cli::cli_inform(c("!" = "{retry_reason} Повтор через {wait} сек. (попытка {attempt}/{max_retries})"))
    yaf_sleep(wait)
  }
}

#' Универсальный get-запрос к API Яндекс Директа
#'
#' Выполняет метод get любого сервиса API v5: разбивает id на батчи,
#' проходит все страницы через LimitedBy, повторяет запрос при временных
#' ошибках. При неустранимой ошибке останавливается (класс yaf_api_error),
#' неполный результат не возвращается.
#'
#' @param login Логин в Яндексе.
#' @param service Имя сервиса: "campaigns", "adgroups", "ads", "keywords",
#'   "bidmodifiers", "negativekeywordsharedsets" и т. д.
#' @param params Список params запроса: SelectionCriteria, FieldNames,
#'   {Type}FieldNames. Массивы внутри SelectionCriteria передавайте как list().
#' @param batch_ids Вектор id для разбиения на батчи (необязательно).
#' @param batch_field Поле SelectionCriteria для батчей, например "CampaignIds".
#' @param batch_size Размер батча. Лимит зависит от сервиса и поля.
#' @param page_limit Объектов на страницу (максимум 10000).
#' @param max_retries Максимум повторов при временных ошибках.
#' @param progress Показывать прогресс-бар.
#'
#' @return Список объектов в том виде, в каком их вернул API. Атрибут
#'   "units" — баллы после последнего запроса, "requests" — число запросов.
#' @export
#'
#' @examples
#' \dontrun{
#' kw <- yaf_api_get(
#'   "my_login", "keywords",
#'   params = list(FieldNames = c("Id", "AdGroupId", "Keyword")),
#'   batch_ids = c(123, 456), batch_field = "CampaignIds"
#' )
#' attr(kw, "units")
#' }
yaf_api_get <- function(login, service, params = list(),
                        batch_ids = NULL, batch_field = "CampaignIds",
                        batch_size = 10, page_limit = 10000,
                        max_retries = 5, progress = interactive()) {
  token <- get_yaf_token(login)
  params <- yaf_prepare_params(params)

  if (!is.null(batch_ids)) {
    batch_ids <- unique(as.numeric(batch_ids))
    if (length(batch_ids) == 0) {
      return(structure(list(), units = yaf_parse_units(NULL), requests = 0L))
    }
    batches <- unname(split(batch_ids, ceiling(seq_along(batch_ids) / batch_size)))
  } else {
    batches <- list(NULL)
  }

  pages <- list()
  units <- yaf_parse_units(NULL)
  n_requests <- 0L

  show_pb <- progress && length(batches) > 1
  if (show_pb) {
    pb <- cli::cli_progress_bar(paste(service, login), total = length(batches), clear = FALSE)
  }

  for (batch in batches) {
    offset <- 0

    repeat {
      p <- params
      if (!is.null(batch)) p$SelectionCriteria[[batch_field]] <- as.list(batch)
      p$Page <- list(Limit = page_limit, Offset = offset)

      res <- yaf_api_request(login, service, list(method = "get", params = p),
                             token, max_retries)
      n_requests <- n_requests + 1L
      units <- res$units

      data_field <- setdiff(names(res$result), "LimitedBy")
      if (length(data_field) > 0) {
        pages[[length(pages) + 1]] <- res$result[[data_field[1]]]
      }

      if (is.null(res$result$LimitedBy)) break
      offset <- res$result$LimitedBy
    }

    if (show_pb) cli::cli_progress_update(id = pb)
  }

  if (show_pb) cli::cli_progress_done(id = pb)

  items <- unlist(pages, recursive = FALSE)
  if (is.null(items)) items <- list()
  attr(items, "units") <- units
  attr(items, "requests") <- n_requests
  items
}

#' Плоская таблица из объектов API
#'
#' Скалярные поля остаются как есть, вложенные (списки, объекты)
#' сериализуются в JSON-строку.
#' @keywords internal
yaf_items_to_df <- function(items) {
  if (length(items) == 0) return(data.frame())
  rows <- lapply(items, function(x) {
    lapply(x, function(v) {
      if (is.null(v)) NA
      else if (is.atomic(v) && length(v) == 1) v
      else as.character(jsonlite::toJSON(v, auto_unbox = TRUE))
    })
  })
  dplyr::bind_rows(rows)
}

#' Все Id кампаний аккаунта
#' @keywords internal
yaf_all_campaign_ids <- function(login) {
  items <- yaf_api_get(login, "campaigns", list(FieldNames = "Id"), progress = FALSE)
  vapply(items, function(x) as.numeric(x$Id), numeric(1))
}
