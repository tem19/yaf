#' Значение по умолчанию для NULL
#'
#' @param x Значение.
#' @param y Что вернуть, если x равен NULL.
#'
#' @return x, если он не NULL, иначе y.
#' @name grapes-or-or-grapes
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Пауза между повторами запроса
#'
#' Обёртка над Sys.sleep(): в тестах её подменяют, чтобы не ждать.
#'
#' @param seconds Длительность паузы в секундах.
#'
#' @return NULL, невидимо.
#' @keywords internal
yaf_sleep <- function(seconds) Sys.sleep(seconds)

#' Ошибка API с кодом
#'
#' Создаёт условие класса yaf_api_error, чтобы ошибки API можно было
#' ловить отдельно: tryCatch(..., yaf_api_error = function(e) e$code).
#'
#' @param message Текст ошибки.
#' @param code Код ошибки API Директа (error_code).
#' @param service Сервис API, в котором произошла ошибка.
#' @param request_id Идентификатор запроса из ответа API.
#'
#' @return Объект условия классов yaf_api_error, error, condition.
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
#'
#' @param header Значение заголовка Units или NULL.
#'
#' @return Именованный числовой вектор spent, rest, limit. Если заголовка
#'   нет, все значения NA.
#' @keywords internal
yaf_parse_units <- function(header) {
  empty <- c(spent = NA_real_, rest = NA_real_, limit = NA_real_)
  if (is.null(header) || length(header) != 1L || is.na(header)) {
    return(empty)
  }
  v <- suppressWarnings(as.numeric(strsplit(header, "/", fixed = TRUE)[[1]]))
  v <- c(v, rep(NA_real_, 3))[1:3]
  stats::setNames(v, c("spent", "rest", "limit"))
}

#' Таймаут по умолчанию для одного HTTP-запроса, секунды
#'
#' httr2 не задаёт таймаут сам: без него зависшее соединение блокирует
#' R-процесс навсегда.
#'
#' @return Число секунд.
#' @keywords internal
yaf_default_timeout <- function() {
  t <- getOption("yaf.timeout", Sys.getenv("YAF_TIMEOUT", "60"))
  t <- suppressWarnings(as.numeric(t))
  if (length(t) != 1L || is.na(t) || t <= 0) 60 else t
}

#' Приведение Id к строкам без потери точности
#'
#' Id объявлений в Директе больше 2^53 и не помещаются в double без потери
#' точности. API принимает Id строками, поэтому внутри пакета они хранятся
#' как character.
#'
#' @param ids Вектор Id (numeric, character или NULL).
#'
#' @return Уникальный вектор строк.
#' @keywords internal
yaf_ids_chr <- function(ids) {
  if (is.null(ids) || length(ids) == 0) return(character(0))
  if (is.numeric(ids)) {
    ids <- format(ids, scientific = FALSE, trim = TRUE)
  }
  ids <- trimws(as.character(ids))
  if (anyNA(ids) || any(!grepl("^-?[0-9]+$", ids))) {
    stop("\u0410\u0440\u0433\u0443\u043c\u0435\u043d\u0442 batch_ids/ids \u0434\u043e\u043b\u0436\u0435\u043d \u0441\u043e\u0434\u0435\u0440\u0436\u0430\u0442\u044c \u0446\u0435\u043b\u044b\u0435 \u0447\u0438\u0441\u043b\u0430 \u0431\u0435\u0437 NA.", call. = FALSE)
  }
  unique(ids)
}

#' Подготовка params к сериализации
#'
#' httr2 сериализует тело с auto_unbox = TRUE, поэтому вектор из одного
#' элемента (FieldNames = "Id") уйдёт строкой, а не массивом, и API вернёт
#' ошибку. Все *FieldNames принудительно превращаем в list().
#' Пустой SelectionCriteria должен уйти как {}, а не []. Если
#' SelectionCriteria не передан, он не добавляется: у некоторых сервисов
#' (negativekeywordsharedsets) пустой SelectionCriteria — ошибка.
#'
#' @param params Список params запроса.
#'
#' @return Тот же список с исправленными FieldNames и SelectionCriteria.
#' @keywords internal
yaf_prepare_params <- function(params) {
  for (nm in grep("FieldNames$", names(params), value = TRUE)) {
    params[[nm]] <- as.list(params[[nm]])
  }
  if ("SelectionCriteria" %in% names(params) && length(params$SelectionCriteria) == 0) {
    params$SelectionCriteria <- stats::setNames(list(), character(0))
  }
  params
}

#' Один запрос к API с ретраями
#'
#' Отправляет запрос и разбирает ответ. Временные ошибки (коды из
#' retry_codes, HTTP 5xx, сетевые сбои) повторяются с паузой 1, 2, 4...
#' секунд, но не больше 60. Остальные ошибки сразу останавливают
#' выполнение с ошибкой класса yaf_api_error.
#'
#' @param login Логин в Яндексе (заголовок Client-Login).
#' @param service Сервис API, например "campaigns".
#' @param body Тело запроса: list(method = ..., params = ...).
#' @param token OAuth-токен.
#' @param max_retries Максимум повторов при временных ошибках.
#' @param retry_codes Коды ошибок API, при которых запрос повторяется.
#' @param version Версия API в адресе: "v5" или "v501" (единые перфоманс-кампании).
#' @param retry_network Повторять ли запрос при сетевой ошибке и HTTP 5xx.
#'   Для изменяющих методов (add и т. п.) FALSE: запрос мог выполниться,
#'   и повтор создал бы дубли.
#' @param timeout Таймаут одного HTTP-запроса в секундах. По умолчанию
#'   `getOption("yaf.timeout", 60)` или переменная окружения `YAF_TIMEOUT`.
#'
#' @return Список с элементами result (поле result ответа API)
#'   и units (см. yaf_parse_units()).
#' @keywords internal
yaf_api_request <- function(login, service, body, token,
                            max_retries = 5,
                            retry_codes = c(52, 506, 1000, 1001, 1002),
                            version = "v5", retry_network = TRUE,
                            timeout = yaf_default_timeout()) {
  yaf_check_login(login)
  attempt <- 0L

  repeat {
    attempt <- attempt + 1L

    resp <- tryCatch(
      httr2::request(paste0("https://api.direct.yandex.com/json/", version, "/", service)) |>
        httr2::req_headers(
          Authorization = paste0("Bearer ", token),
          `Client-Login` = login,
          `Accept-Language` = "ru"
        ) |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(),
      error = function(e) e
    )

    retry_reason <- NULL

    if (inherits(resp, "error")) {
      retry_reason <- paste("\u0421\u0435\u0442\u0435\u0432\u0430\u044F \u043E\u0448\u0438\u0431\u043A\u0430:", conditionMessage(resp))
      if (!retry_network) {
        stop(yaf_api_error(sprintf("[%s] %s. \u0417\u0430\u043F\u0440\u043E\u0441 \u043D\u0435 \u043F\u043E\u0432\u0442\u043E\u0440\u044F\u0435\u0442\u0441\u044F: \u043E\u043D \u043C\u043E\u0433 \u0432\u044B\u043F\u043E\u043B\u043D\u0438\u0442\u044C\u0441\u044F",
                                   service, retry_reason), service = service))
      }
    } else {
      status <- httr2::resp_status(resp)
      parsed <- tryCatch(httr2::resp_body_json(resp, bigint_as_char = TRUE),
                         error = function(e) NULL)
      err <- parsed$error

      if (!is.null(err)) {
        # error_code может отсутствовать или быть нечисловым: не роняем разбор
        code <- tryCatch(
          suppressWarnings(as.integer(err$error_code %||% NA_integer_)[1]),
          error = function(e) NA_integer_
        )
        msg <- sprintf("[%s] \u041E\u0448\u0438\u0431\u043A\u0430 API %s: %s. %s",
                       service,
                       if (is.na(code)) "?" else code,
                       err$error_string %||% "",
                       err$error_detail %||% "")
        if (identical(code, 53L)) {
          msg <- paste0(msg, " \u041F\u0435\u0440\u0435\u0432\u044B\u043F\u0443\u0441\u0442\u0438\u0442\u0435 \u0442\u043E\u043A\u0435\u043D: yaf_get_token('", login, "').")
        }
        retry_api <- !is.na(code) && code %in% retry_codes
        # HTTP 5xx может прийти с неретраибельным кодом: для чтения это
        # временный сбой, для записи повтор опасен.
        retry_http <- !retry_api && status >= 500 && isTRUE(retry_network)
        if (!retry_api && !retry_http) {
          stop(yaf_api_error(msg, code, service, err$request_id %||% NA_character_))
        }
        retry_reason <- msg
      } else if (status >= 500) {
        retry_reason <- paste("HTTP", status)
        if (!retry_network) {
          stop(yaf_api_error(sprintf("[%s] %s. \u0417\u0430\u043F\u0440\u043E\u0441 \u043D\u0435 \u043F\u043E\u0432\u0442\u043E\u0440\u044F\u0435\u0442\u0441\u044F: \u043E\u043D \u043C\u043E\u0433 \u0432\u044B\u043F\u043E\u043B\u043D\u0438\u0442\u044C\u0441\u044F",
                                     service, retry_reason), service = service))
        }
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
        sprintf("[%s] \u0417\u0430\u043F\u0440\u043E\u0441 \u043D\u0435 \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D \u0437\u0430 %d \u043F\u043E\u043F\u044B\u0442\u043E\u043A. \u041F\u043E\u0441\u043B\u0435\u0434\u043D\u044F\u044F \u043E\u0448\u0438\u0431\u043A\u0430: %s",
                service, attempt, retry_reason),
        service = service
      ))
    }

    wait <- min(2^(attempt - 1), 60)
    # Текст ошибки API подставляем как значение: cli не должен
    # интерпретировать фигурные скобки из ответа провайдера
    cli::cli_inform(c("!" = paste0(retry_reason, " \u041F\u043E\u0432\u0442\u043E\u0440 \u0447\u0435\u0440\u0435\u0437 ", wait,
                                   " \u0441\u0435\u043A. (\u043F\u043E\u043F\u044B\u0442\u043A\u0430 ", attempt, "/", max_retries, ")")))
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
#'   <Type>FieldNames. Массивы внутри SelectionCriteria передавайте как list().
#'   Если сервис требует SelectionCriteria даже для выборки всех объектов
#'   (например, campaigns), передайте SelectionCriteria = list().
#' @param batch_ids Вектор id для разбиения на батчи (необязательно).
#' @param batch_field Поле SelectionCriteria для батчей, например "CampaignIds".
#' @param batch_size Размер батча. Лимит зависит от сервиса и поля.
#' @param page_limit Объектов на страницу (максимум 10000).
#' @param max_retries Максимум повторов при временных ошибках.
#' @param progress Показывать прогресс-бар.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
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
                        max_retries = 5, progress = interactive(),
                        token = NULL, timeout = yaf_default_timeout()) {
  token <- token %||% get_yaf_token(login)
  params <- yaf_prepare_params(params)

  if (!is.null(batch_ids)) {
    batch_ids <- yaf_ids_chr(batch_ids)
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
                             token, max_retries, timeout = timeout)
      n_requests <- n_requests + 1L
      units <- res$units

      data_fields <- setdiff(names(res$result), "LimitedBy")
      for (field in data_fields) {
        pages[[length(pages) + 1]] <- res$result[[field]]
      }

      limited_by <- res$result$LimitedBy
      if (is.null(limited_by)) break
      # LimitedBy — новый Offset. Если он не растёт, API зациклит выдачу.
      if (length(limited_by) != 1L || is.na(limited_by) || limited_by <= offset) {
        stop(yaf_api_error(
          sprintf("[%s] \u041F\u0430\u0433\u0438\u043D\u0430\u0446\u0438\u044F \u043D\u0435 \u043F\u0440\u043E\u0434\u0432\u0438\u0433\u0430\u0435\u0442\u0441\u044F: LimitedBy = %s \u043F\u0440\u0438 Offset = %s.",
                  service, paste(limited_by, collapse = ","), offset),
          service = service
        ))
      }
      offset <- limited_by
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
#' Скалярные поля остаются как есть, вложенные разворачиваются через
#' yaf_flatten_object(): объекты — в колонки с именами через точку,
#' списки — в строку через "; ".
#'
#' @param items Список объектов API, например результат yaf_api_get().
#'
#' @return data.frame: одна строка на объект, колонки по полям объектов.
#'   Для пустого списка — пустой data.frame.
#' @keywords internal
yaf_items_to_df <- function(items) {
  if (length(items) == 0) return(data.frame())
  dplyr::bind_rows(lapply(items, yaf_flatten_object))
}

#' Все Id кампаний аккаунта
#'
#' @param login Логин в Яндексе.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return Числовой вектор Id кампаний.
#' @keywords internal
yaf_all_campaign_ids <- function(login, token = NULL, timeout = yaf_default_timeout()) {
  items <- yaf_api_get(login, "campaigns",
                       list(SelectionCriteria = list(), FieldNames = "Id"),
                       progress = FALSE, token = token, timeout = timeout)
  vapply(items, function(x) as.character(x$Id), character(1))
}

#' Плоский список полей из вложенного объекта API
#'
#' Вложенные объекты разворачиваются в поля с именами через точку
#' (BiddingStrategy.Search.BiddingStrategyType). Списки строк вида
#' list(Items = ...) и массивы скаляров склеиваются через "; ".
#' Массивы настроек list(Option, Value) превращаются в "OPTION=VALUE; ...".
#' Остальные массивы объектов сериализуются в JSON-строку.
#'
#' @param x Объект API (именованный список).
#' @param prefix Префикс имён полей для рекурсивного вызова.
#'
#' @return Именованный список скаляров.
#' @keywords internal
yaf_flatten_object <- function(x, prefix = NULL) {
  is_scalar <- function(e) is.atomic(e) && length(e) == 1
  out <- list()
  for (nm in names(x)) {
    v <- x[[nm]]
    key <- if (is.null(prefix)) nm else paste(prefix, nm, sep = ".")
    if (is.null(v)) {
      out[[key]] <- NA
    } else if (is_scalar(v)) {
      out[[key]] <- v
    } else if (identical(names(v), "Items")) {
      out[[key]] <- paste(unlist(v$Items), collapse = "; ")
    } else if (!is.null(names(v))) {
      out <- c(out, yaf_flatten_object(v, key))
    } else if (all(vapply(v, is_scalar, logical(1)))) {
      out[[key]] <- paste(unlist(v), collapse = "; ")
    } else if (all(vapply(v, function(e) all(c("Option", "Value") %in% names(e)), logical(1)))) {
      out[[key]] <- paste(vapply(v, function(e) paste0(e$Option, "=", e$Value), ""),
                          collapse = "; ")
    } else {
      out[[key]] <- as.character(jsonlite::toJSON(v, auto_unbox = TRUE))
    }
  }
  out
}
