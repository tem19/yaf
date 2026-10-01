test_that("timestamp приводится к формату API", {
  expect_equal(
    yaf_format_timestamp(as.POSIXct("2025-01-31 10:00:00", tz = "Europe/Moscow")),
    "2025-01-31T07:00:00Z"
  )
  expect_equal(yaf_format_timestamp("2025-01-31T07:00:00Z"), "2025-01-31T07:00:00Z")
  expect_error(yaf_format_timestamp("2025-01-31"), "timestamp")
})

test_that("yaf_changes_timestamp вызывает checkDictionaries с пустым params-объектом", {
  mock_env()
  body <- NULL
  httr2::local_mocked_responses(function(req) {
    body <<- req$body$data
    httr2::response_json(body = list(result = list(Timestamp = "2025-02-01T00:00:00Z")))
  })

  expect_equal(yaf_changes_timestamp("login"), "2025-02-01T00:00:00Z")
  expect_equal(body$method, "checkDictionaries")
  expect_equal(as.character(jsonlite::toJSON(body$params)), "{}")
})

test_that("yaf_check_campaign_changes разбирает ChangesIn", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    expect_equal(req$body$data$method, "checkCampaigns")
    expect_equal(req$body$data$params$Timestamp, "2025-01-31T00:00:00Z")
    httr2::response_json(body = list(result = list(
      Campaigns = list(
        list(CampaignId = 1, ChangesIn = list("SELF", "CHILDREN")),
        list(CampaignId = 2, ChangesIn = list("STAT"))
      ),
      Timestamp = "2025-02-01T00:00:00Z"
    )))
  })

  df <- suppressMessages(yaf_check_campaign_changes("login", "2025-01-31T00:00:00Z"))
  expect_equal(df$CampaignId, c(1, 2))
  expect_equal(df$ChangesIn, c("SELF,CHILDREN", "STAT"))
  expect_equal(df$ChangedSelf, c(TRUE, FALSE))
  expect_equal(df$ChangedChildren, c(TRUE, FALSE))
  expect_equal(df$ChangedStat, c(FALSE, TRUE))
  expect_equal(attr(df, "timestamp"), "2025-02-01T00:00:00Z")
})

test_that("без timestamp возвращается пустая таблица и текущее время сервера", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    expect_equal(req$body$data$method, "checkDictionaries")
    httr2::response_json(body = list(result = list(Timestamp = "2025-02-01T00:00:00Z")))
  })

  df <- suppressMessages(yaf_check_campaign_changes("login"))
  expect_equal(nrow(df), 0)
  expect_equal(attr(df, "timestamp"), "2025-02-01T00:00:00Z")
})

test_that("yaf_check_changes режет id на батчи и берёт самый ранний timestamp", {
  mock_env()
  sizes <- c()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    p <- req$body$data$params
    sizes <<- c(sizes, length(p$CampaignIds))
    expect_null(p$SelectionCriteria)
    httr2::response_json(body = list(result = list(
      Modified = list(
        CampaignIds = list(p$CampaignIds[[1]]),
        AdIds = list(100 + calls),
        CampaignsStat = list(list(CampaignId = p$CampaignIds[[1]], BorderDate = "2025-01-30"))
      ),
      Timestamp = if (calls == 1) "2025-02-01T00:00:05Z" else "2025-02-01T00:00:01Z"
    )))
  })

  res <- suppressMessages(yaf_check_changes("login", "2025-01-31T00:00:00Z",
                                            campaign_ids = 1:3500))
  expect_equal(sizes, c(3000, 500))
  expect_equal(res$modified$Type, c("Campaign", "Ad", "Campaign", "Ad"))
  expect_equal(res$modified$Id, c(1, 101, 3001, 102))
  expect_equal(res$campaigns_stat$CampaignId, c(1, 3001))
  expect_equal(res$timestamp, "2025-02-01T00:00:01Z")
})

test_that("необработанные объекты перепроверяются отдельными запросами", {
  mock_env()
  requests <- list()
  httr2::local_mocked_responses(function(req) {
    p <- req$body$data$params
    requests[[length(requests) + 1]] <<- p
    if (!is.null(p$CampaignIds)) {
      httr2::response_json(body = list(result = list(
        Modified = list(CampaignIds = list(1)),
        Unprocessed = list(AdGroupIds = list(20, 21)),
        Timestamp = "2025-02-01T00:00:00Z"
      )))
    } else {
      httr2::response_json(body = list(result = list(
        Modified = list(AdGroupIds = list(21)),
        Timestamp = "2025-02-01T00:00:00Z"
      )))
    }
  })

  res <- suppressMessages(yaf_check_changes("login", "2025-01-31T00:00:00Z", campaign_ids = 1))
  expect_length(requests, 2)
  expect_equal(unlist(requests[[2]]$AdGroupIds), c(20, 21))
  expect_equal(res$modified$Id, c(1, 21))
  expect_equal(nrow(res$unprocessed), 0)
})

test_that("остаток необработанных объектов возвращается с предупреждением", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(
      Unprocessed = list(AdIds = list(5)),
      Timestamp = "2025-02-01T00:00:00Z"
    )))
  })

  expect_warning(
    res <- suppressMessages(yaf_check_changes("login", "2025-01-31T00:00:00Z",
                                              ad_ids = 5, max_followups = 1)),
    "Не удалось обработать"
  )
  expect_equal(res$unprocessed$Type, "Ad")
  expect_equal(res$unprocessed$Id, 5)
})
