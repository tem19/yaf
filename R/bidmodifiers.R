#' Получение корректировок ставок из Яндекс Директа
#'
#' @param login Логин в Яндексе.
#' @param campaign_ids Вектор ID кампаний (необязательно).
#' @param adgroup_ids Вектор ID групп (необязательно).
#' @param type_fields Именованный список тип-специфичных полей,
#'   например list(MobileAdjustmentFieldNames = c("BidModifier")).
#' @param raw Если TRUE, вернуть сырой список объектов API без разбора.
#' @param token OAuth-токен. Если NULL, читается через get_yaf_token().
#' @param timeout Таймаут одного HTTP-запроса в секундах.
#'
#' @return data.frame с колонками modifier_id, campaign_id, adgroup_id,
#'   level, type, value (коэффициент в процентах) и condition (условие
#'   корректировки, например "GENDER_MALE AGE_25_34" или id региона).
#'   При raw = TRUE — список объектов API.
#' @export
#'
#' @examples
#' \dontrun{
#' mods <- yaf_get_bid_modifiers("my_login", campaign_ids = c(123, 456))
#' }
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
                                  raw = FALSE, token = NULL,
                                  timeout = yaf_default_timeout()) {
  if (!is.null(adgroup_ids)) {
    ids <- adgroup_ids; id_field <- "AdGroupIds"; batch_size <- 1000
  } else {
    ids <- campaign_ids %||% yaf_all_campaign_ids(login, token = token, timeout = timeout)
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
                       batch_ids = ids, batch_field = id_field, batch_size = batch_size,
                       token = token, timeout = timeout)

  if (raw) return(items)

  if (length(items) == 0) {
    cli::cli_alert_warning("\u041A\u043E\u0440\u0440\u0435\u043A\u0442\u0438\u0440\u043E\u0432\u043A\u0438 \u043D\u0435 \u043D\u0430\u0439\u0434\u0435\u043D\u044B.")
    return(data.frame())
  }

  df <- purrr::map_dfr(items, function(x) {
    adj_name <- grep("Adjustment$", names(x), value = TRUE)
    adj <- if (length(adj_name) == 0) list() else x[[adj_name[1]]]

    # Массив условий приходит безымянным списком именованных объектов,
    # одиночная корректировка — именованным списком полей.
    is_array <- length(adj) > 0 && is.null(names(adj)) &&
      all(vapply(adj, is.list, logical(1)))
    conds <- if (is_array) adj else list(adj)

    purrr::map_dfr(conds, function(a) {
      cond_fields <- setdiff(names(a), c("BidModifier", "Enabled"))
      data.frame(
        modifier_id = as.character(x$Id),
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

  if (anyNA(df$modifier_id)) {
    cli::cli_warn("\u0423 {sum(is.na(df$modifier_id))} \u043A\u043E\u0440\u0440\u0435\u043A\u0442\u0438\u0440\u043E\u0432\u043E\u043A \u043D\u0435\u0442 Id: \u0441\u0442\u0440\u043E\u043A\u0438 \u043E\u0441\u0442\u0430\u0432\u043B\u0435\u043D\u044B \u0441 NA.")
  }
  cli::cli_alert_success("\u0423\u0441\u043F\u0435\u0448\u043D\u043E \u0432\u044B\u0433\u0440\u0443\u0436\u0435\u043D\u043E \u043A\u043E\u0440\u0440\u0435\u043A\u0442\u0438\u0440\u043E\u0432\u043E\u043A: {.val {nrow(df)}}")
  df
}
