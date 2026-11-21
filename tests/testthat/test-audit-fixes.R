test_that("ошибка API без error_code не роняет разбор", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(error = list(error_string = "Bad request")))
  })

  err <- tryCatch(
    yaf_api_get("login", "campaigns", list(FieldNames = "Id")),
    yaf_api_error = function(e) e
  )
  expect_s3_class(err, "yaf_api_error")
  expect_match(conditionMessage(err), "Ошибка API \\?")
  expect_match(conditionMessage(err), "Bad request")
})

test_that("таймаут берётся из опции", {
  old <- options(yaf.timeout = 7)
  on.exit(options(old), add = TRUE)
  expect_equal(yaf_default_timeout(), 7)

  options(yaf.timeout = NULL)
  expect_equal(yaf_default_timeout(), 60)
})

test_that("таймаут передаётся в HTTP-запрос", {
  mock_env()
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req$options$timeout_ms
    httr2::response_json(body = list(result = list(Campaigns = list())))
  })

  yaf_api_get("login", "campaigns", list(FieldNames = "Id"), timeout = 5)
  expect_equal(seen, 5000)
})

test_that("пагинация без прогресса останавливается", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(
      Campaigns = list(list(Id = 1)), LimitedBy = 0
    )))
  })

  expect_error(
    yaf_api_get("login", "campaigns", list(FieldNames = "Id")),
    class = "yaf_api_error"
  )
})

test_that("нечисловые batch_ids отклоняются до запроса", {
  mock_env()
  httr2::local_mocked_responses(function(req) stop("запроса быть не должно"))
  expect_error(
    yaf_api_get("login", "ads", list(FieldNames = "Id"),
                batch_ids = c(1, NA), batch_field = "CampaignIds"),
    "batch_ids"
  )
})

test_that("HTTP 5xx с неретраибельным кодом повторяется только для чтения", {
  mock_env()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    httr2::response_json(status_code = 503, body = list(error = list(
      error_code = 8000, error_string = "Server error"
    )))
  })
  expect_error(
    suppressMessages(yaf_api_get("login", "campaigns", list(FieldNames = "Id"),
                                 max_retries = 2)),
    class = "yaf_api_error"
  )
  expect_equal(calls, 3)

  calls <- 0
  expect_error(
    yaf_api_call("login", "campaigns", "suspend",
                 list(SelectionCriteria = list(Ids = list(1)))),
    class = "yaf_api_error"
  )
  expect_equal(calls, 1)
})

test_that("пустой отчёт возвращает пустую таблицу с колонками", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw(""))
  })

  df <- suppressMessages(yaf_get_report("login", fields = c("Date", "Clicks")))
  expect_equal(nrow(df), 0)
  expect_named(df, c("Date", "Clicks"))
})

test_that("отчёт проверяет даты и goals до запроса", {
  mock_env()
  httr2::local_mocked_responses(function(req) stop("запроса быть не должно"))

  expect_error(
    yaf_get_report("login", date_from = "2025-02-01", date_to = "2025-01-01"),
    "date_from"
  )
  expect_error(yaf_get_report("login", goals = c("a")), "goals")
  expect_error(yaf_get_report("login", date_from = "not-a-date"), "датами")
})

test_that("attribution без goals предупреждает", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw("Date\tClicks\n2025-01-01\t1\n"))
  })

  expect_warning(
    suppressMessages(yaf_get_report("login", attribution = "FC")),
    "attribution"
  )
})

test_that("yaf_get_ads разбирает ResponsiveAd", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Ads = list(
      list(Id = 1, CampaignId = 2, AdGroupId = 3, Type = "RESPONSIVE_AD",
           Status = "ACCEPTED", State = "ON",
           ResponsiveAd = list(Titles = list("Первый", "Второй"),
                               Texts = list("Текст 1", "Текст 2"),
                               Href = "https://a.ru"))
    ))))
  })

  df <- suppressMessages(yaf_get_ads("login", campaign_ids = 2))
  expect_equal(df$type, "RESPONSIVE_AD")
  expect_equal(df$title, "Первый; Второй")
  expect_equal(df$text, "Текст 1; Текст 2")
  expect_equal(df$href, "https://a.ru")
})

test_that("yaf_get_ads предупреждает о неизвестном типе", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Ads = list(
      list(Id = 1, CampaignId = 2, AdGroupId = 3, Type = "CPM_BANNER_AD",
           Status = "ACCEPTED", State = "ON")
    ))))
  })

  expect_warning(
    suppressMessages(yaf_get_ads("login", campaign_ids = 2)),
    "CPM_BANNER_AD"
  )
})

test_that("Wordstat пропускает неполные строки", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(results = list(
      list(phrase = "купить слона", count = "10"),
      list(count = "5")
    )))
  })

  expect_warning(
    res <- yaf_ws_top("слон", api_key = "k", folder_id = "f"),
    "Пропущено строк"
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$phrase, "купить слона")
  expect_equal(res$count, 10)
})

test_that("Wordstat проверяет top_n", {
  expect_error(yaf_ws_top("слон", api_key = "k", folder_id = "f", top_n = 0), "top_n")
  expect_error(yaf_ws_top("слон", api_key = "k", folder_id = "f", top_n = 2001), "top_n")
})

test_that("yaf_get_regions возвращает yaf_api_error", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 400, body = list(error = list(
      error_code = 8000, error_string = "Bad request"
    )))
  })

  expect_error(
    suppressMessages(yaf_get_regions("login")),
    class = "yaf_api_error"
  )
})

test_that("yaf_get_regions разбирает справочник", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(GeoRegions = list(
      list(GeoRegionId = 225, GeoRegionName = "Россия", GeoRegionType = "COUNTRY"),
      list(GeoRegionId = 213, ParentId = 225, GeoRegionName = "Москва",
           GeoRegionType = "REGION")
    ))))
  })

  df <- suppressMessages(yaf_get_regions("login"))
  expect_equal(nrow(df), 2)
  expect_named(df, c("region_id", "parent_id", "region_name", "region_type"))
  expect_equal(df$parent_id, c(NA, 225))
  expect_equal(df$region_name, c("Россия", "Москва"))
})

test_that("changes сохраняет исходный timestamp, если сервер его не вернул", {
  mock_env()
  ts <- "2025-01-31T00:00:00Z"
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(
      Campaigns = list(list(CampaignId = 1, ChangesIn = list("SELF")))
    )))
  })

  expect_warning(
    res <- yaf_check_campaign_changes("login", ts),
    "Timestamp"
  )
  expect_equal(attr(res, "timestamp"), ts)
})

test_that("явный token не читает RDS", {
  testthat::local_mocked_bindings(
    get_yaf_token = function(login) stop("RDS не должен читаться"),
    yaf_sleep = function(seconds) invisible(NULL),
    .env = asNamespace("yaf")
  )
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Campaigns = list())))
  })

  # get_yaf_token() замокан на stop(), поэтому успешный вызов доказывает,
  # что использован явный token
  expect_silent(
    yaf_api_get("login", "campaigns", list(FieldNames = "Id"), token = "secret")
  )
})

test_that("верхнеуровневый Type кампании не затирается блоком", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Campaigns = list(
      list(Id = 1, Name = "c", Type = "TEXT_CAMPAIGN",
           TextCampaign = list(Type = "WRONG",
                               BiddingStrategy = list(Search = list(
                                 BiddingStrategyType = "HIGHEST_POSITION"))))
    ))))
  })

  df <- yaf_get_campaigns("login")
  expect_equal(df$Type, "TEXT_CAMPAIGN")
})

test_that("write-функции принимают явный token", {
  testthat::local_mocked_bindings(
    get_yaf_token = function(login) stop("RDS не должен читаться"),
    yaf_sleep = function(seconds) invisible(NULL),
    .env = asNamespace("yaf")
  )
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(AddResults = list(list(Id = 10)))))
  })

  res <- yaf_add_ads("login", list(list(AdGroupId = 1)), token = "secret")
  expect_equal(res$Id, 10)
})

test_that("логин проверяется до запроса", {
  expect_error(yaf_check_login(""), "login")
  expect_error(yaf_check_login(NA_character_), "login")
  expect_error(yaf_check_login("a/b"), "разделители")
})
