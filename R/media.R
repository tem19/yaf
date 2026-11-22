#' Загрузка изображений в Яндекс Директ
#'
#' Синхронно загружает изображения методом `AdImages.add` и возвращает
#' `AdImageHash`, который затем передаётся в `TextImageAd` или
#' `ResponsiveAd$AdImageHashes`.
#'
#' @param login Логин в Яндексе.
#' @param paths Пути к файлам JPG, PNG или GIF.
#' @param names Имена изображений. По умолчанию — имена файлов.
#' @param type Тип изображения: `"REGULAR"`, `"WIDE"`, `"FIXED_IMAGE"`,
#'   `"AUTO"` или `NULL` (определить автоматически).
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками Name, AdImageHash, Errors, Warnings.
#' @export
#'
#' @examples
#' \dontrun{
#' img <- yaf_upload_images("my_login", "banner.png", type = "FIXED_IMAGE")
#' img$AdImageHash
#' }
yaf_upload_images <- function(login, paths, names = NULL, type = NULL,
                              token = NULL, timeout = yaf_default_timeout()) {
  if (!is.character(paths) || length(paths) == 0) {
    stop("paths \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043D\u0435\u043F\u0443\u0441\u0442\u044B\u043C \u0432\u0435\u043A\u0442\u043E\u0440\u043E\u043C \u043F\u0443\u0442\u0435\u0439.", call. = FALSE)
  }
  paths <- normalizePath(paths, mustWork = TRUE)
  if (is.null(names)) names <- basename(paths)
  if (!is.character(names) || length(names) != length(paths)) {
    stop("names \u0434\u043E\u043B\u0436\u0435\u043D \u0441\u043E\u0432\u043F\u0430\u0434\u0430\u0442\u044C \u043F\u043E \u0434\u043B\u0438\u043D\u0435 \u0441 paths.", call. = FALSE)
  }
  if (!is.null(type)) {
    type <- match.arg(type, c("REGULAR", "WIDE", "FIXED_IMAGE", "AUTO"))
  }
  if (length(paths) > 100) {
    stop("\u0417\u0430 \u043E\u0434\u0438\u043D \u0432\u044B\u0437\u043E\u0432 \u043C\u043E\u0436\u043D\u043E \u0437\u0430\u0433\u0440\u0443\u0437\u0438\u0442\u044C \u043D\u0435 \u0431\u043E\u043B\u044C\u0448\u0435 100 \u0438\u0437\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u0438\u0439.", call. = FALSE)
  }

  items <- lapply(seq_along(paths), function(i) {
    size <- file.info(paths[i])$size
    if (is.na(size) || size > 10 * 1024^2) {
      stop("\u0424\u0430\u0439\u043B ", paths[i], " \u0431\u043E\u043B\u044C\u0448\u0435 10 \u041C\u0411 \u0438\u043B\u0438 \u043D\u0435\u0434\u043E\u0441\u0442\u0443\u043F\u0435\u043D.", call. = FALSE)
    }
    item <- list(
      Name = names[i],
      ImageData = jsonlite::base64_enc(readBin(paths[i], "raw", n = size))
    )
    if (!is.null(type)) item$Type <- type
    item
  })

  res <- yaf_api_call(login, "adimages", "add", list(AdImages = items),
                      token = token, timeout = timeout)
  results <- res$AddResults %||% list()
  n <- length(items)
  hash <- rep(NA_character_, n)
  errors <- rep(NA_character_, n)
  warnings <- rep(NA_character_, n)
  for (i in seq_len(min(n, length(results)))) {
    hash[i] <- results[[i]]$AdImageHash %||% NA_character_
    errors[i] <- yaf_format_notifications(results[[i]]$Errors)
    warnings[i] <- yaf_format_notifications(results[[i]]$Warnings)
  }

  data.frame(Name = names, AdImageHash = hash, Errors = errors,
             Warnings = warnings, stringsAsFactors = FALSE)
}

#' Удаление изображений
#'
#' Удалить можно только изображение, не привязанное ни к одному объявлению.
#'
#' @param login Логин в Яндексе.
#' @param hashes Вектор `AdImageHash`.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками AdImageHash, Errors, Warnings.
#' @export
yaf_delete_images <- function(login, hashes, token = NULL,
                              timeout = yaf_default_timeout()) {
  hashes <- unique(as.character(hashes))
  if (length(hashes) == 0) {
    return(data.frame(AdImageHash = character(0), Errors = character(0),
                      Warnings = character(0), stringsAsFactors = FALSE))
  }
  res <- yaf_api_call(
    login, "adimages", "delete",
    list(SelectionCriteria = list(AdImageHashes = as.list(hashes))),
    token = token, timeout = timeout
  )
  results <- res$DeleteResults %||% list()
  data.frame(
    AdImageHash = vapply(results, function(r) {
      as.character(r$AdImageHash %||% NA_character_)
    }, character(1)),
    Errors = vapply(results, function(r) yaf_format_notifications(r$Errors), character(1)),
    Warnings = vapply(results, function(r) yaf_format_notifications(r$Warnings), character(1)),
    stringsAsFactors = FALSE
  )
}

#' Загрузка видео в Яндекс Директ
#'
#' Загружает видео по ссылке или из файла. Обработка видео асинхронная:
#' после загрузки проверьте статус через `yaf_wait_video_ready()`.
#'
#' Через API видео нельзя удалить: оно остаётся в библиотеке кабинета.
#'
#' @param login Логин в Яндексе.
#' @param paths Пути к файлам видео. За один вызов можно загрузить один файл.
#' @param urls Ссылки на видео. Не более 10 за вызов. Взаимоисключающе с `paths`.
#' @param names Имена видео. Для файлов по умолчанию — имена файлов.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками Id, Errors, Warnings.
#' @export
yaf_upload_videos <- function(login, paths = NULL, urls = NULL, names = NULL,
                              token = NULL, timeout = yaf_default_timeout()) {
  if (is.null(paths) && is.null(urls)) {
    stop("\u0423\u043A\u0430\u0436\u0438\u0442\u0435 paths \u0438\u043B\u0438 urls.", call. = FALSE)
  }
  if (!is.null(paths) && !is.null(urls)) {
    stop("paths \u0438 urls \u043D\u0435\u043B\u044C\u0437\u044F \u0443\u043A\u0430\u0437\u044B\u0432\u0430\u0442\u044C \u043E\u0434\u043D\u043E\u0432\u0440\u0435\u043C\u0435\u043D\u043D\u043E.", call. = FALSE)
  }

  if (!is.null(paths)) {
    if (length(paths) != 1) {
      stop("\u0417\u0430 \u043E\u0434\u0438\u043D \u0432\u044B\u0437\u043E\u0432 \u043C\u043E\u0436\u043D\u043E \u0437\u0430\u0433\u0440\u0443\u0437\u0438\u0442\u044C \u043E\u0434\u0438\u043D \u0444\u0430\u0439\u043B \u0432\u0438\u0434\u0435\u043E.", call. = FALSE)
    }
    path <- normalizePath(paths, mustWork = TRUE)
    size <- file.info(path)$size
    if (is.na(size) || size > 100 * 1024^2) {
      stop("\u0412\u0438\u0434\u0435\u043E \u0431\u043E\u043B\u044C\u0448\u0435 100 \u041C\u0411 \u0438\u043B\u0438 \u043D\u0435\u0434\u043E\u0441\u0442\u0443\u043F\u043D\u043E.", call. = FALSE)
    }
    name <- if (is.null(names)) basename(path) else as.character(names[1])
    items <- list(list(
      Name = name,
      VideoData = jsonlite::base64_enc(readBin(path, "raw", n = size))
    ))
  } else {
    urls <- as.character(urls)
    if (length(urls) == 0 || length(urls) > 10) {
      stop("urls \u0434\u043E\u043B\u0436\u0435\u043D \u0441\u043E\u0434\u0435\u0440\u0436\u0430\u0442\u044C \u043E\u0442 1 \u0434\u043E 10 \u0441\u0441\u044B\u043B\u043E\u043A.", call. = FALSE)
    }
    items <- lapply(urls, function(u) list(Url = u))
  }

  res <- yaf_api_call(login, "advideos", "add", list(AdVideos = items),
                      token = token, timeout = timeout)
  yaf_action_results(res$AddResults %||% list())
}

#' Статус обработки видео
#'
#' @param login Логин в Яндексе.
#' @param ids Вектор Id видео (строки).
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками Id и Status: `"NEW"`, `"CONVERTING"`,
#'   `"READY"` или `"ERROR"`.
#' @export
yaf_get_video_status <- function(login, ids, token = NULL,
                                 timeout = yaf_default_timeout()) {
  ids <- yaf_ids_chr(ids)
  if (length(ids) == 0) {
    return(data.frame(Id = character(0), Status = character(0),
                      stringsAsFactors = FALSE))
  }
  items <- yaf_api_get(
    login, "advideos",
    params = list(
      SelectionCriteria = list(Ids = as.list(ids)),
      FieldNames = c("Id", "Status")
    ),
    progress = FALSE, token = token, timeout = timeout
  )
  df <- yaf_items_to_df(items)
  if (nrow(df) == 0) {
    return(data.frame(Id = character(0), Status = character(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(Id = as.character(df$Id), Status = as.character(df$Status),
             stringsAsFactors = FALSE)
}

#' Дождаться готовности видео
#'
#' @param login Логин в Яндексе.
#' @param ids Вектор Id видео.
#' @param wait_seconds Максимальное время ожидания.
#' @param poll_every Интервал опроса в секундах.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame Id, Status. Если время вышло, возвращается последний статус
#'   с предупреждением.
#' @export
yaf_wait_video_ready <- function(login, ids, wait_seconds = 300,
                                 poll_every = 5, token = NULL,
                                 timeout = yaf_default_timeout()) {
  deadline <- Sys.time() + wait_seconds
  repeat {
    status <- yaf_get_video_status(login, ids, token = token, timeout = timeout)
    if (nrow(status) == 0 ||
        all(status$Status %in% c("READY", "ERROR"))) {
      return(status)
    }
    if (Sys.time() >= deadline) {
      cli::cli_warn("\u0412\u0438\u0434\u0435\u043E \u043D\u0435 \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u0430\u043D\u043E \u0437\u0430 {wait_seconds} \u0441\u0435\u043A\u0443\u043D\u0434.")
      return(status)
    }
    yaf_sleep(poll_every)
  }
}

#' Создание креативов для видеодополнений
#'
#' @param login Логин в Яндексе.
#' @param video_ids Id видео, полученные из `yaf_upload_videos()`.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками Id (Id креатива), Errors, Warnings.
#' @export
yaf_add_video_creatives <- function(login, video_ids, token = NULL,
                                    timeout = yaf_default_timeout()) {
  ids <- yaf_ids_chr(video_ids)
  if (length(ids) == 0) {
    return(data.frame(Id = character(0), Errors = character(0),
                      Warnings = character(0), stringsAsFactors = FALSE))
  }
  if (length(ids) > 10) {
    stop("\u0412 \u043E\u0434\u043D\u043E\u043C \u0432\u044B\u0437\u043E\u0432\u0435 \u043C\u043E\u0436\u043D\u043E \u0441\u043E\u0437\u0434\u0430\u0442\u044C \u043D\u0435 \u0431\u043E\u043B\u044C\u0448\u0435 10 \u043A\u0440\u0435\u0430\u0442\u0438\u0432\u043E\u0432.", call. = FALSE)
  }
  items <- lapply(ids, function(id) {
    list(VideoExtensionCreative = list(VideoId = id))
  })
  res <- yaf_api_call(login, "creatives", "add", list(Creatives = items),
                      token = token, timeout = timeout)
  yaf_action_results(res$AddResults %||% list())
}
