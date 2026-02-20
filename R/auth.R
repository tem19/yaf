#' Get and save Yandex Direct Token to RDS
#'
#' @param login Character. Your Yandex login.
#' @description Opens a browser to get an OAuth token and saves it to a local RDS file for future use.
#' @export
yaf_get_token <- function(login) {
  if (missing(login) || is.null(login)) {
    stop("Аргумент 'login' обязателен для получения и сохранения токена.")
  }

  # Формируем URL с login_hint для удобства пользователя
  auth_url <- paste0(
    "https://oauth.yandex.ru/authorize?response_type=token&",
    "client_id=", "d0e4dc1f57df4f35854da60979e58283",
    "&login_hint=", utils::URLencode(login)
  )

  message("Открываю браузер для авторизации логина: ", login)
  utils::browseURL(auth_url)

  message("Если браузер не открылся, перейдите по ссылке вручную:")
  message(auth_url)

  # Запрашиваем токен из консоли
  token <- readline("Вставьте полученный токен здесь: ")

  if (token == "" || is.na(token)) {
    stop("Ошибка: Токен не может быть пустым.")
  }

  # Определяем кроссплатформенный путь к папке настроек (Windows/Mac/Linux)
  conf_dir <- tools::R_user_dir("yaf", which = "config")

  # Создаем папку, если она еще не существует
  if (!dir.exists(conf_dir)) {
    dir.create(conf_dir, recursive = TRUE)
  }

  # Формируем путь к файлу (например, .../yaf/config/my_login.rds)
  token_path <- file.path(conf_dir, paste0(login, ".rds"))

  # Сохраняем токен
  saveRDS(token, token_path)

  message("✔ Токен успешно сохранен!")
  message("Путь к файлу: ", token_path)

  return(invisible(token))
}

#' Internal function to retrieve token
#' @param login Character. Yandex login.
#' @keywords internal
get_yaf_token <- function(login) {
  conf_dir <- tools::R_user_dir("yaf", which = "config")
  token_path <- file.path(conf_dir, paste0(login, ".rds"))

  # Если файла нет — запускаем процесс получения
  if (!file.exists(token_path)) {
    message("Файл токена не найден для '", login, "'. Запускаю yaf_get_token()...")
    yaf_get_token(login = login)
  }

  # Читаем токен из файла
  token <- tryCatch({
    readRDS(token_path)
  }, error = function(e) {
    stop("Ошибка при чтении файла токена. Попробуйте получить его заново.")
  })

  return(token)
}

