test_that("yaf_ws_top отправляет запрос в новом формате и разбирает ответ", {
  mock_env()
  req_seen <- NULL
  httr2::local_mocked_responses(function(req) {
    req_seen <<- req
    httr2::response_json(body = list(
      totalCount = "300",
      results = list(
        list(phrase = "купить слона", count = "100"),
        list(phrase = "купить слона дешево", count = "200")
      ),
      associations = list(list(phrase = "продать слона", count = "5"))
    ))
  })

  res <- suppressMessages(yaf_ws_top("купить слона", api_key = "key",
                                     folder_id = "folder", region_id = 213,
                                     devices = c("phone", "tablet"), top_n = 10))

  expect_equal(req_seen$url, "https://searchapi.api.cloud.yandex.net/v2/wordstat/topRequests")
  expect_equal(httr2::req_get_headers(req_seen, "reveal")$Authorization, "Api-Key key")
  expect_equal(
    as.character(jsonlite::toJSON(req_seen$body$data, auto_unbox = TRUE)),
    paste0('{"phrase":"купить слона","numPhrases":"10",',
           '"devices":["DEVICE_PHONE","DEVICE_TABLET"],',
           '"folderId":"folder","regions":["213"]}')
  )
  expect_equal(res, data.frame(phrase = c("купить слона дешево", "купить слона"),
                               count = c(200, 100)))
})

test_that("yaf_ws_top: ошибка API", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 403,
                         body = list(code = 7, message = "Permission denied"))
  })

  err <- tryCatch(
    suppressMessages(yaf_ws_top("слон", api_key = "key",
                                folder_id = "folder")),
    yaf_api_error = function(e) e
  )
  expect_s3_class(err, "yaf_api_error")
  expect_match(conditionMessage(err), "Permission denied")
  expect_equal(err$code, 403)
})

test_that("yaf_ws_top требует ключ и каталог", {
  expect_error(yaf_ws_top("слон", api_key = "", folder_id = "f"), "api_key")
  expect_error(yaf_ws_top("слон", api_key = "k", folder_id = ""), "folder_id")
})
