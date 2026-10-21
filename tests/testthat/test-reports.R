test_that("yaf_get_report() показывает текст ошибки API", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 400L, body = list(error = list(
      error_code = 8000, error_string = "Неверный запрос",
      error_detail = "Неизвестное поле"
    )))
  })

  expect_error(
    capture.output(yaf_get_report("login")),
    "8000.*Неизвестное поле",
    class = "yaf_api_error"
  )
})

test_that("yaf_get_report() отправляет одно поле массивом", {
  mock_env()
  sent <- NULL
  httr2::local_mocked_responses(function(req) {
    sent <<- req$body$data$params
    httr2::response(status_code = 200, body = charToRaw("Clicks\n5"))
  })

  invisible(capture.output(res <- yaf_get_report("login", fields = "Clicks")))

  expect_type(sent$FieldNames, "list")
  expect_equal(res$Clicks, 5)
})

test_that("yaf_get_report() ждёт 10 секунд и повторяет запрос при ошибке 56", {
  mock_env()
  waits <- c()
  local_mocked_bindings(yaf_sleep = function(seconds) waits <<- c(waits, seconds))

  n <- 0
  httr2::local_mocked_responses(function(req) {
    n <<- n + 1
    switch(n,
      httr2::response(status_code = 201, headers = list(retryIn = "15")),
      httr2::response(status_code = 400, body = charToRaw(
        '{"error":{"error_string":"Превышен лимит запросов метода","error_code":"56"}}'
      )),
      httr2::response(status_code = 200, body = charToRaw("Clicks\n5"))
    )
  })

  invisible(capture.output(res <- yaf_get_report("login", fields = "Clicks")))

  expect_equal(n, 3)
  expect_equal(waits, c(15, 10))
  expect_equal(res$Clicks, 5)
})
