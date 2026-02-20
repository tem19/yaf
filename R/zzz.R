.onAttach <- function(libname, pkgname) {
  # Проверяем, что сессия интерактивна (чтобы не спамить в логах серверов)
  if (interactive()) {
    packageStartupMessage(paste0(
      "--------------------------------------------------\n",
      "Пакет yaf v", packageVersion("yaf"), " успешно загружен.\n",
      "Автор: Артем Рогожин (https://github.com/tem19/yaf)\n",
      "--------------------------------------------------"
    ))
  }
}
