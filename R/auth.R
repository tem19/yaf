#' Проверка логина
#'
#' Логин попадает в имя файла токена и в заголовок Client-Login, поэтому
#' запрещаем пустые значения, NA и разделители пути.
#'
#' @param login Логин в Яндексе.
#'
#' @return NULL, невидимо; при ошибке останавливается.
#' @keywords internal
yaf_check_login <- function(login) {
  if (missing(login) || !is.character(login) || length(login) != 1L ||
      is.na(login) || !nzchar(login)) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'login' \u0434\u043E\u043B\u0436\u0435\u043D \u0431\u044B\u0442\u044C \u043D\u0435\u043F\u0443\u0441\u0442\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u043E\u0439.", call. = FALSE)
  }
  if (grepl("[/\\\\]", login)) {
    stop("\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442 'login' \u043D\u0435 \u0434\u043E\u043B\u0436\u0435\u043D \u0441\u043E\u0434\u0435\u0440\u0436\u0430\u0442\u044C \u0440\u0430\u0437\u0434\u0435\u043B\u0438\u0442\u0435\u043B\u0438 \u043F\u0443\u0442\u0438.", call. = FALSE)
  }
  invisible(NULL)
}

#' Получение и сохранение токена Яндекс Директа
#'
#' Открывает в браузере страницу авторизации Яндекса, просит вставить
#' полученный OAuth-токен в консоль и сохраняет его в файл `<login>.rds`
#' в папке `tools::R_user_dir("yaf", "config")`. Остальные функции пакета
#' читают токен из этого файла. Работает только в интерактивной сессии.
#'
#' @param login Логин в Яндексе.
#'
#' @return Токен (строка), невидимо.
#' @export
#'
#' @examples
#' \dontrun{
#' yaf_get_token("my_login")
#' }
yaf_get_token <- function(login) {
  yaf_check_login(login)

  # Формируем URL с login_hint для удобства пользователя
  auth_url <- paste0(
    "https://oauth.yandex.ru/authorize?response_type=token&",
    "client_id=", "d0e4dc1f57df4f35854da60979e58283",
    "&login_hint=", utils::URLencode(login)
  )

  message("\u041E\u0442\u043A\u0440\u044B\u0432\u0430\u044E \u0431\u0440\u0430\u0443\u0437\u0435\u0440 \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u0440\u0438\u0437\u0430\u0446\u0438\u0438 \u043B\u043E\u0433\u0438\u043D\u0430: ", login)
  utils::browseURL(auth_url)

  message("\u0415\u0441\u043B\u0438 \u0431\u0440\u0430\u0443\u0437\u0435\u0440 \u043D\u0435 \u043E\u0442\u043A\u0440\u044B\u043B\u0441\u044F, \u043F\u0435\u0440\u0435\u0439\u0434\u0438\u0442\u0435 \u043F\u043E \u0441\u0441\u044B\u043B\u043A\u0435 \u0432\u0440\u0443\u0447\u043D\u0443\u044E:")
  message(auth_url)

  # Запрашиваем токен из консоли
  token <- readline("\u0412\u0441\u0442\u0430\u0432\u044C\u0442\u0435 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u043D\u044B\u0439 \u0442\u043E\u043A\u0435\u043D \u0437\u0434\u0435\u0441\u044C: ")

  if (token == "" || is.na(token)) {
    stop("\u041E\u0448\u0438\u0431\u043A\u0430: \u0422\u043E\u043A\u0435\u043D \u043D\u0435 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u043F\u0443\u0441\u0442\u044B\u043C.")
  }

  # Определяем кроссплатформенный путь к папке настроек (Windows/Mac/Linux)
  conf_dir <- tools::R_user_dir("yaf", which = "config")

  # Создаем папку, если она еще не существует
  if (!dir.exists(conf_dir)) {
    dir.create(conf_dir, recursive = TRUE, mode = "0700")
  }

  # Формируем путь к файлу (например, .../yaf/config/my_login.rds)
  token_path <- file.path(conf_dir, paste0(login, ".rds"))

  # Сохраняем токен и закрываем файл от других пользователей
  saveRDS(token, token_path)
  try(Sys.chmod(token_path, mode = "0600"), silent = TRUE)

  message("\u2714 \u0422\u043E\u043A\u0435\u043D \u0443\u0441\u043F\u0435\u0448\u043D\u043E \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D!")
  message("\u041F\u0443\u0442\u044C \u043A \u0444\u0430\u0439\u043B\u0443: ", token_path)

  return(invisible(token))
}

#' Чтение сохранённого токена
#'
#' Читает токен, сохранённый yaf_get_token(). Если файла нет, в
#' интерактивной сессии запускает yaf_get_token(), в неинтерактивной
#' (Rscript, cron) останавливается с ошибкой.
#'
#' @param login Логин в Яндексе.
#'
#' @return Токен (строка).
#' @keywords internal
get_yaf_token <- function(login) {
  yaf_check_login(login)
  conf_dir <- tools::R_user_dir("yaf", which = "config")
  token_path <- file.path(conf_dir, paste0(login, ".rds"))

  # Если файла нет — запускаем процесс получения
  if (!file.exists(token_path)) {
    if (!interactive()) {
      stop(
        "\u0422\u043E\u043A\u0435\u043D \u0434\u043B\u044F \u043B\u043E\u0433\u0438\u043D\u0430 '", login, "' \u043D\u0435 \u043D\u0430\u0439\u0434\u0435\u043D: ", token_path, ". ",
        "\u041F\u043E\u043B\u0443\u0447\u0438\u0442\u0435 \u0435\u0433\u043E \u0432 \u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0439 \u0441\u0435\u0441\u0441\u0438\u0438 R: yaf_get_token('", login, "').",
        call. = FALSE
      )
    }
    message("\u0424\u0430\u0439\u043B \u0442\u043E\u043A\u0435\u043D\u0430 \u043D\u0435 \u043D\u0430\u0439\u0434\u0435\u043D \u0434\u043B\u044F '", login, "'. \u0417\u0430\u043F\u0443\u0441\u043A\u0430\u044E yaf_get_token()...")
    yaf_get_token(login = login)
  }

  # Читаем токен из файла
  token <- tryCatch(
    readRDS(token_path),
    error = function(e) {
      stop("\u041E\u0448\u0438\u0431\u043A\u0430 \u043F\u0440\u0438 \u0447\u0442\u0435\u043D\u0438\u0438 \u0444\u0430\u0439\u043B\u0430 \u0442\u043E\u043A\u0435\u043D\u0430 ", token_path, ": ",
           conditionMessage(e), ". \u041F\u043E\u043F\u0440\u043E\u0431\u0443\u0439\u0442\u0435 \u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C \u0442\u043E\u043A\u0435\u043D \u0437\u0430\u043D\u043E\u0432\u043E.",
           call. = FALSE)
    }
  )

  if (!is.character(token) || length(token) != 1L || is.na(token) || !nzchar(token)) {
    stop("\u0424\u0430\u0439\u043B \u0442\u043E\u043A\u0435\u043D\u0430 ", token_path, " \u043F\u043E\u0432\u0440\u0435\u0436\u0434\u0451\u043D: \u043E\u0436\u0438\u0434\u0430\u043B\u0430\u0441\u044C \u043D\u0435\u043F\u0443\u0441\u0442\u0430\u044F \u0441\u0442\u0440\u043E\u043A\u0430. ",
         "\u041F\u043E\u043B\u0443\u0447\u0438\u0442\u0435 \u0442\u043E\u043A\u0435\u043D \u0437\u0430\u043D\u043E\u0432\u043E: yaf_get_token('", login, "').", call. = FALSE)
  }

  token
}

