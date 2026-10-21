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
