# Подменяет токен и паузу между повторами: тестам не нужны сеть и реальный токен
mock_env <- function(env = parent.frame()) {
  local_mocked_bindings(
    get_yaf_token = function(login) "test-token",
    yaf_sleep = function(seconds) invisible(NULL),
    .env = env
  )
}
