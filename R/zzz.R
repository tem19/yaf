# Переменные, используемые внутри dplyr::filter()/select() без qualifying
utils::globalVariables(c("count", "source_phrase"))

#' Сообщение при подключении пакета
#'
#' В интерактивной сессии выводит версию пакета и ссылку на репозиторий.
#'
#' @param libname Путь к библиотеке (передаётся R).
#' @param pkgname Имя пакета (передаётся R).
#'
#' @return NULL, невидимо.
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Проверяем, что сессия интерактивна (чтобы не спамить в логах серверов)
  if (interactive()) {
    packageStartupMessage(paste0(
      "--------------------------------------------------\n",
      "\u041F\u0430\u043A\u0435\u0442 yaf v", utils::packageVersion("yaf"), " \u0443\u0441\u043F\u0435\u0448\u043D\u043E \u0437\u0430\u0433\u0440\u0443\u0436\u0435\u043D.\n",
      "\u0410\u0432\u0442\u043E\u0440: \u0410\u0440\u0442\u0435\u043C \u0420\u043E\u0433\u043E\u0436\u0438\u043D (https://github.com/tem19/yaf)\n",
      "--------------------------------------------------"
    ))
  }
}
