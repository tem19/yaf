#' Получение корректировок ставок из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param adgroup_ids Вектор ID групп (необязательно).
#' @param type_fields Именованный список тип-специфичных полей,
#'   например list(MobileAdjustmentFieldNames = c("BidModifier")).
#' @param raw Если TRUE, вернуть сырой список объектов API без разбора.
#'
#' @export
yaf_get_bid_modifiers <- function(login, campaign_ids = NULL, adgroup_ids = NULL,
                                  type_fields = list(
                                    MobileAdjustmentFieldNames       = c("BidModifier"),
                                    TabletAdjustmentFieldNames       = c("BidModifier"),
                                    DesktopAdjustmentFieldNames      = c("BidModifier"),
                                    DesktopOnlyAdjustmentFieldNames  = c("BidModifier"),
                                    DemographicsAdjustmentFieldNames = c("Gender", "Age", "BidModifier"),
                                    RetargetingAdjustmentFieldNames  = c("RetargetingConditionId", "BidModifier"),
                                    RegionalAdjustmentFieldNames     = c("RegionId", "BidModifier"),
                                    VideoAdjustmentFieldNames        = c("BidModifier"),
                                    SmartAdAdjustmentFieldNames      = c("BidModifier"),
                                    SerpLayoutAdjustmentFieldNames   = c("SerpLayout", "BidModifier"),
                                    IncomeGradeAdjustmentFieldNames  = c("Grade", "BidModifier"),
                                    AdGroupAdjustmentFieldNames      = c("BidModifier")
                                  ),
                                  raw = FALSE) {
  if (!is.null(adgroup_ids)) {
    ids <- adgroup_ids; id_field <- "AdGroupIds"; batch_size <- 1000
  } else {
    ids <- campaign_ids %||% yaf_all_campaign_ids(login)
    id_field <- "CampaignIds"; batch_size <- 10
  }

  params <- c(
    list(
      SelectionCriteria = list(Levels = list("CAMPAIGN", "AD_GROUP")),
      FieldNames = c("Id", "CampaignId", "AdGroupId", "Level", "Type")
    ),
    type_fields
  )

  items <- yaf_api_get(login, "bidmodifiers", params,
                       batch_ids = ids, batch_field = id_field, batch_size = batch_size)

  if (raw) return(items)

  if (length(items) == 0) {
    cli::cli_alert_warning("Корректировки не найдены.")
    return(data.frame())
  }

  df <- purrr::map_dfr(items, function(x) {
    adj_name <- grep("Adjustment$", names(x), value = TRUE)[1]
    adj <- if (is.na(adj_name)) list() else x[[adj_name]]

    # Если API вернул массив условий, разворачиваем в несколько строк
    conds <- if (length(adj) > 0 && is.list(adj[[1]])) adj else list(adj)

    purrr::map_dfr(conds, function(a) {
      cond_fields <- setdiff(names(a), c("BidModifier", "Enabled"))
      data.frame(
        modifier_id = x$Id,
        campaign_id = x$CampaignId %||% NA,
        adgroup_id  = x$AdGroupId %||% NA,
        level       = x$Level,
        type        = x$Type,
        value       = a$BidModifier %||% NA,
        condition   = if (length(cond_fields) > 0) {
          paste(vapply(cond_fields, function(f) as.character(a[[f]] %||% NA), ""),
                collapse = " ")
        } else NA,
        stringsAsFactors = FALSE
      )
    })
  })

  df <- df[!is.na(df$modifier_id), ]
  cli::cli_alert_success("Успешно выгружено корректировок: {.val {nrow(df)}}")
  df
}
