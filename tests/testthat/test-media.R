test_that("yaf_upload_images отправляет base64 и возвращает Hash", {
  mock_env()
  path <- tempfile(fileext = ".png")
  writeBin(charToRaw("fake-png"), path)
  on.exit(unlink(path), add = TRUE)

  body <- NULL
  httr2::local_mocked_responses(function(req) {
    body <<- req$body$data$params
    httr2::response_json(body = list(result = list(AddResults = list(
      list(AdImageHash = "hash-1")
    ))))
  })

  res <- yaf_upload_images("login", path, names = "banner", type = "FIXED_IMAGE")
  expect_equal(res$AdImageHash, "hash-1")
  expect_equal(res$Name, "banner")
  expect_true(nzchar(body$AdImages[[1]]$ImageData))
  expect_equal(body$AdImages[[1]]$Type, "FIXED_IMAGE")
  expect_equal(body$AdImages[[1]]$Name, "banner")
})

test_that("yaf_delete_images разбирает DeleteResults", {
  mock_env()
  body <- NULL
  httr2::local_mocked_responses(function(req) {
    body <<- req$body$data$params
    httr2::response_json(body = list(result = list(DeleteResults = list(
      list(AdImageHash = "hash-1")
    ))))
  })

  res <- yaf_delete_images("login", "hash-1")
  expect_equal(res$AdImageHash, "hash-1")
  expect_true(is.na(res$Errors))
  expect_equal(unlist(body$SelectionCriteria$AdImageHashes), "hash-1")
})

test_that("yaf_upload_videos загружает файл и ссылки", {
  mock_env()
  path <- tempfile(fileext = ".mp4")
  writeBin(charToRaw("fake-video"), path)
  on.exit(unlink(path), add = TRUE)

  bodies <- list()
  httr2::local_mocked_responses(function(req) {
    bodies[[length(bodies) + 1]] <<- req$body$data$params
    httr2::response_json(body = list(result = list(AddResults = list(
      list(Id = "1927660909263305805")
    ))))
  })

  res <- yaf_upload_videos("login", paths = path, names = "clip")
  expect_equal(res$Id, "1927660909263305805")
  expect_equal(bodies[[1]]$AdVideos[[1]]$Name, "clip")
  expect_true(nzchar(bodies[[1]]$AdVideos[[1]]$VideoData))

  res <- yaf_upload_videos("login", urls = "https://example.com/v.mp4")
  expect_equal(bodies[[2]]$AdVideos[[1]]$Url, "https://example.com/v.mp4")

  expect_error(
    yaf_upload_videos("login", paths = c(path, path)),
    "один файл"
  )
})

test_that("yaf_get_video_status опрашивает статусы", {
  mock_env()
  params <- NULL
  httr2::local_mocked_responses(function(req) {
    params <<- req$body$data$params
    httr2::response_json(body = list(result = list(AdVideos = list(
      list(Id = "1927660909263305805", Status = "READY")
    ))))
  })

  res <- yaf_get_video_status("login", "1927660909263305805")
  expect_equal(res$Id, "1927660909263305805")
  expect_equal(res$Status, "READY")
  expect_equal(unlist(params$SelectionCriteria$Ids), "1927660909263305805")
})

test_that("yaf_wait_video_ready ждёт READY", {
  mock_env()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    status <- if (calls < 3) "CONVERTING" else "READY"
    httr2::response_json(body = list(result = list(AdVideos = list(
      list(Id = "1", Status = status)
    ))))
  })

  res <- yaf_wait_video_ready("login", "1", wait_seconds = 10, poll_every = 0)
  expect_equal(res$Status, "READY")
  expect_equal(calls, 3)
})

test_that("yaf_add_video_creatives отправляет VideoId и возвращает Id", {
  mock_env()
  body <- NULL
  httr2::local_mocked_responses(function(req) {
    body <<- req$body$data$params
    httr2::response_json(body = list(result = list(AddResults = list(
      list(Id = "777")
    ))))
  })

  res <- yaf_add_video_creatives("login", "1927660909263305805")
  expect_equal(res$Id, "777")
  expect_equal(body$Creatives[[1]]$VideoExtensionCreative$VideoId,
               "1927660909263305805")
})

test_that("yaf_get_ads разбирает графическое объявление", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Ads = list(
      list(Id = 1, CampaignId = 2, AdGroupId = 3, Type = "IMAGE_AD",
           Status = "DRAFT", State = "OFF",
           TextImageAd = list(AdImageHash = "hash-1", Href = "https://a.ru"))
    ))))
  })

  df <- suppressMessages(yaf_get_ads("login", campaign_ids = 2))
  expect_equal(df$type, "IMAGE_AD")
  expect_equal(df$image_hash, "hash-1")
  expect_equal(df$href, "https://a.ru")
  expect_true(is.na(df$video_creative_id))
})

test_that("yaf_get_ads разбирает видеодополнение", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Ads = list(
      list(Id = 1, CampaignId = 2, AdGroupId = 3, Type = "TEXT_AD",
           Status = "DRAFT", State = "OFF",
           TextAd = list(Title = "t", Text = "x", Href = "https://a.ru",
                         VideoExtension = list(CreativeId = 777)))
    ))))
  })

  df <- suppressMessages(yaf_get_ads("login", campaign_ids = 2))
  expect_equal(df$video_creative_id, "777")
})

test_that("yaf_get_ads разбирает VideoExtensions у ResponsiveAd", {
  mock_env()
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(result = list(Ads = list(
      list(Id = 1, CampaignId = 2, AdGroupId = 3, Type = "RESPONSIVE_AD",
           Status = "DRAFT", State = "OFF",
           ResponsiveAd = list(
             Titles = list("t"), Texts = list("x"), Href = "https://a.ru",
             AdImages = list("hash-1"),
             VideoExtensions = list(Items = list(list(
               CreativeId = 777, RatioType = "HORIZONTAL", Status = "ACCEPTED"
             )))
           ))
    ))))
  })

  df <- suppressMessages(yaf_get_ads("login", campaign_ids = 2))
  expect_equal(df$image_hash, "hash-1")
  expect_equal(df$video_creative_id, "777")
})
