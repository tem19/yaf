test_that("add режет объекты на батчи и собирает результаты по порядку", {
  mock_env()
  sizes <- c()
  urls <- c()
  httr2::local_mocked_responses(function(req) {
    items <- req$body$data$params$Campaigns
    sizes <<- c(sizes, length(items))
    urls <<- c(urls, req$url)
    httr2::response_json(body = list(result = list(AddResults = lapply(items, function(x) {
      if (identical(x$Name, "bad")) {
        list(Errors = list(list(Code = 5005, Message = "Поле задано неверно", Details = "Name")))
      } else {
        list(Id = as.numeric(sub("c", "", x$Name)))
      }
    }))))
  })

  campaigns <- c(lapply(1:11, function(i) list(Name = paste0("c", i))), list(list(Name = "bad")))
  res <- yaf_add_campaigns("login", campaigns)
  expect_equal(sizes, c(10, 2))
  expect_true(all(grepl("/json/v501/campaigns$", urls)))
  expect_equal(res$Id, c(1:11, NA))
  expect_equal(res$Errors[12], "5005: Поле задано неверно (Name)")
  expect_true(all(is.na(res$Errors[1:11])))
})

test_that("массив из одного объекта уходит массивом", {
  mock_env()
  body <- NULL
  httr2::local_mocked_responses(function(req) {
    body <<- req$body$data
    httr2::response_json(body = list(result = list(AddResults = list(list(Id = 1)))))
  })
  yaf_add_keywords("login", list(list(AdGroupId = 1, Keyword = "уролог")))
  json <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
  expect_match(json, '"Keywords":\\[\\{"AdGroupId":1,"Keyword":"уролог"\\}\\]')
  expect_match(json, '"method":"add"')
})

test_that("add не повторяется при сетевой ошибке, get — повторяется", {
  mock_env()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    stop("timeout")
  })
  expect_error(yaf_add_ads("login", list(list(AdGroupId = 1))), class = "yaf_api_error")
  expect_equal(calls, 1)

  calls <- 0
  expect_error(suppressMessages(yaf_api_get("login", "ads", list(FieldNames = "Id"),
                                            max_retries = 2, progress = FALSE)),
               class = "yaf_api_error")
  expect_equal(calls, 3)
})

test_that("временная ошибка API повторяется и при add", {
  mock_env()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    if (calls == 1) {
      httr2::response_json(body = list(error = list(error_code = 52, error_string = "Сервер временно недоступен")))
    } else {
      httr2::response_json(body = list(result = list(AddResults = list(list(Id = 7)))))
    }
  })
  res <- suppressMessages(yaf_add_sitelinks("login", list(list(Sitelinks = list(list(Title = "a", Href = "https://a.ru"))))))
  expect_equal(res$Id, 7)
  expect_equal(calls, 2)
})

test_that("модерация отправляет Id массивом и читает ModerateResults", {
  mock_env()
  body <- NULL
  httr2::local_mocked_responses(function(req) {
    body <<- req$body$data
    httr2::response_json(body = list(result = list(ModerateResults = list(list(Id = 5)))))
  })
  res <- yaf_moderate_ads("login", 5)
  expect_equal(body$method, "moderate")
  expect_match(as.character(jsonlite::toJSON(body, auto_unbox = TRUE)), '"Ids":\\[5\\]')
  expect_equal(res$Id, 5)
})

test_that("пустой список не делает запросов", {
  mock_env()
  httr2::local_mocked_responses(function(req) stop("не должно быть запросов"))
  expect_equal(nrow(yaf_add_ads("login", list())), 0)
  expect_equal(nrow(yaf_delete_campaigns("login", numeric(0))), 0)
})
