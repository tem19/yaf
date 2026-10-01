test_that("FieldNames из одного элемента уходит массивом, пустой SelectionCriteria объектом", {
  p <- yaf_prepare_params(list(FieldNames = "Id"))
  expect_equal(
    as.character(jsonlite::toJSON(p, auto_unbox = TRUE)),
    '{"FieldNames":["Id"],"SelectionCriteria":{}}'
  )
})

test_that("страницы склеиваются по LimitedBy", {
  mock_env()
  offsets <- c()
  httr2::local_mocked_responses(function(req) {
    offset <- req$body$data$params$Page$Offset
    offsets <<- c(offsets, offset)
    if (offset == 0) {
      httr2::response_json(body = list(result = list(
        Keywords = list(list(Id = 1), list(Id = 2)), LimitedBy = 2
      )))
    } else {
      httr2::response_json(body = list(result = list(Keywords = list(list(Id = 3)))))
    }
  })

  items <- yaf_api_get("login", "keywords", list(FieldNames = "Id"), progress = FALSE)
  expect_length(items, 3)
  expect_equal(offsets, c(0, 2))
  expect_equal(attr(items, "requests"), 2L)
})

test_that("id режутся на батчи нужного размера", {
  mock_env()
  sizes <- c()
  httr2::local_mocked_responses(function(req) {
    sizes <<- c(sizes, length(req$body$data$params$SelectionCriteria$CampaignIds))
    httr2::response_json(body = list(result = list(AdGroups = list())))
  })

  yaf_api_get("login", "adgroups", list(FieldNames = "Id"),
              batch_ids = 1:25, batch_size = 10, progress = FALSE)
  expect_equal(sizes, c(10, 10, 5))
})

test_that("временная ошибка повторяется", {
  mock_env()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    if (calls == 1) {
      httr2::response_json(body = list(error = list(
        error_code = 506, error_string = "Limit", error_detail = ""
      )))
    } else {
      httr2::response_json(body = list(result = list(Campaigns = list(list(Id = 1)))))
    }
  })

  items <- suppressMessages(yaf_api_get("login", "campaigns", list(FieldNames = "Id")))
  expect_length(items, 1)
  expect_equal(calls, 2)
})

test_that("неустранимая ошибка останавливает выгрузку", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(error = list(
      error_code = 8000, error_string = "Invalid request", error_detail = "x"
    )))
  })

  expect_error(
    yaf_api_get("login", "campaigns", list(FieldNames = "Id")),
    class = "yaf_api_error"
  )
})

test_that("ретраи ограничены max_retries", {
  mock_env()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    httr2::response_json(status_code = 503, body = list())
  })

  expect_error(
    suppressMessages(yaf_api_get("login", "campaigns", list(FieldNames = "Id"), max_retries = 2)),
    class = "yaf_api_error"
  )
  expect_equal(calls, 3)
})

test_that("Units разбирается", {
  expect_equal(yaf_parse_units("10/20828/64000"),
               c(spent = 10, rest = 20828, limit = 64000))
  expect_true(all(is.na(yaf_parse_units(NULL))))
})

test_that("фраза отделяется от минус-слов", {
  res <- yaf_split_keyword(c("купить слона -бесплатно -игрушка",
                             "санкт-петербург отель",
                             "---autotargeting"))
  expect_equal(res$phrase, c("купить слона", "санкт-петербург отель", "---autotargeting"))
  expect_equal(res$minus, c("бесплатно; игрушка", "", ""))
})

# Разбор ответов в старых функциях: формат таблиц должен совпадать с 0.7.0

test_that("yaf_get_adgroups сохраняет прежние имена колонок", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(AdGroups = list(
      list(Type = "TEXT_AD_GROUP", Id = 10, Name = "g", CampaignId = 1, Status = "ACCEPTED")
    ))))
  })

  df <- suppressMessages(yaf_get_adgroups("login", campaign_ids = 1))
  expect_equal(names(df), c("adgroup_id", "campaign_id", "adgroup_name", "status", "type"))
})

test_that("yaf_get_ads возвращает display_url_path", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Ads = list(
      list(Id = 5, CampaignId = 1, AdGroupId = 10, Type = "TEXT_AD",
           Status = "ACCEPTED", State = "ON",
           TextAd = list(Title = "t", Text = "x", Href = "https://a.ru",
                         DisplayUrlPath = "path"))
    ))))
  })

  df <- suppressMessages(yaf_get_ads("login", campaign_ids = 1))
  expect_equal(df$display_url_path, "path")
  expect_true(is.na(df$title2))
})

test_that("yaf_get_bid_modifiers разбирает видео, демографию и массив условий", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(BidModifiers = list(
      list(Id = 1, CampaignId = 1, Level = "CAMPAIGN", Type = "VIDEO_ADJUSTMENT",
           VideoAdjustment = list(BidModifier = 150)),
      list(Id = 2, CampaignId = 1, Level = "CAMPAIGN", Type = "DEMOGRAPHICS_ADJUSTMENT",
           DemographicsAdjustment = list(Gender = "GENDER_MALE", Age = "AGE_25_34",
                                         BidModifier = 120)),
      list(Id = 3, CampaignId = 1, Level = "CAMPAIGN", Type = "REGIONAL_ADJUSTMENT",
           RegionalAdjustment = list(list(RegionId = 225, BidModifier = 90),
                                     list(RegionId = 1, BidModifier = 110)))
    ))))
  })

  df <- suppressMessages(yaf_get_bid_modifiers("login", campaign_ids = 1))
  expect_equal(nrow(df), 4)
  expect_equal(df$value, c(150, 120, 90, 110))
  expect_equal(df$condition, c(NA, "GENDER_MALE AGE_25_34", "225", "1"))
})
