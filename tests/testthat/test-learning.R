test_that("цели стратегии: GoalId, ключевые цели, все цели, без целей", {
  expect_equal(yaf_learning_goals("AVERAGE_CPA", 111, c(1, 2)), 111)
  expect_equal(yaf_learning_goals("PAY_FOR_CONVERSION", 13, c(1, 2)), c(1, 2))
  expect_equal(yaf_learning_goals("AVERAGE_CPA_MULTIPLE_GOALS", NA, c(1, 2)), c(1, 2))
  expect_equal(yaf_learning_goals("WB_MAXIMUM_CONVERSION_RATE", 0, numeric(0)), 0)
  expect_equal(yaf_learning_goals("WB_MAXIMUM_CLICKS", NA, c(1, 2)), numeric(0))
  expect_null(yaf_learning_goals("SERVING_OFF", NA, numeric(0)))
})

test_that("yaf_strategy_learning_check считает конверсии по целям стратегии", {
  mock_env()
  text <- function(search, network = list(BiddingStrategyType = "SERVING_OFF"), package = NULL) {
    list(BiddingStrategy = list(Search = search, Network = network),
         PackageBiddingStrategy = package)
  }
  campaigns <- list(
    list(Id = 1, Name = "cpa", Type = "TEXT_CAMPAIGN", State = "ON",
         TextCampaign = text(list(BiddingStrategyType = "AVERAGE_CPA",
                                  AverageCpa = list(AverageCpa = 500, GoalId = 111)))),
    list(Id = 2, Name = "key goals", Type = "UNIFIED_CAMPAIGN", State = "ON",
         UnifiedCampaign = c(
           text(list(BiddingStrategyType = "PAY_FOR_CONVERSION",
                     PayForConversion = list(Cpa = 300, GoalId = 13)),
                list(BiddingStrategyType = "PAY_FOR_CONVERSION",
                     PayForConversion = list(Cpa = 300, GoalId = 13))),
           list(PriorityGoals = list(Items = list(list(GoalId = 222, Value = 1),
                                                  list(GoalId = 333, Value = 1))))
         )),
    list(Id = 3, Name = "clicks", Type = "TEXT_CAMPAIGN", State = "ON",
         TextCampaign = text(list(BiddingStrategyType = "WB_MAXIMUM_CLICKS"))),
    list(Id = 4, Name = "package on", Type = "TEXT_CAMPAIGN", State = "ON",
         TextCampaign = text(list(BiddingStrategyType = "AVERAGE_CPA"),
                             package = list(StrategyId = 900))),
    list(Id = 5, Name = "package off", Type = "TEXT_CAMPAIGN", State = "OFF",
         TextCampaign = text(list(BiddingStrategyType = "AVERAGE_CPA"),
                             package = list(StrategyId = 900)))
  )
  report <- paste(
    "CampaignId\tAdNetworkType\tConversions_111_AUTO\tConversions_222_AUTO\tConversions_333_AUTO",
    "1\tSEARCH\t12\t--\t--",
    "2\tSEARCH\t0\t3\t2",
    "2\tAD_NETWORK\t0\t4\t7",
    "4\tSEARCH\t6\t0\t0",
    "5\tAD_NETWORK\t5\t0\t0",
    sep = "\n"
  )

  report_body <- NULL
  strategy_ids <- NULL
  httr2::local_mocked_responses(function(req) {
    if (grepl("reports$", req$url)) {
      report_body <<- req$body$data$params
      return(httr2::response(status_code = 200, body = charToRaw(report)))
    }
    if (grepl("strategies$", req$url)) {
      strategy_ids <<- unlist(req$body$data$params$SelectionCriteria$Ids)
      return(httr2::response_json(body = list(result = list(Strategies = list(
        list(Id = 900, Name = "pkg", Type = "AVERAGE_CPA",
             AverageCpa = list(GoalId = 111))
      )))))
    }
    httr2::response_json(body = list(result = list(Campaigns = campaigns)))
  })

  invisible(capture.output(res <- yaf_strategy_learning_check("login")))

  expect_equal(strategy_ids, 900)
  expect_equal(sort(unlist(report_body$Goals)), c(111, 222, 333))
  expect_match(report_body$SelectionCriteria$Filter[[1]]$Field, "CampaignId")
  expect_setequal(as.numeric(unlist(report_body$SelectionCriteria$Filter[[1]]$Values)),
                  c(1, 2, 4, 5))

  expect_equal(res$CampaignId, c(1, 2, 2, 3, 4))
  expect_equal(res$Platform, c("SEARCH", "SEARCH", "NETWORK", "SEARCH", "ALL"))
  expect_equal(res$GoalIds, c("111", "222; 333", "222; 333", NA, "111"))
  expect_equal(res$Conversions, c(12, 5, 11, NA, 11))
  expect_equal(res$Status, c("ok", "low_data", "ok", "not_applicable", "ok"))
  expect_equal(res$StrategyId, c(NA, NA, NA, NA, 900))
  expect_equal(attr(res, "date_to"), Sys.Date() - 1)
})
