#' Получение списка рекламных кампаний
#'
#' Функция возвращает таблицу со списком всех кампаний рекламодателя.
#'
#' @param login Character. Логин в Яндексе.
#'
#' @return A data frame с ID кампаний, их названиями, статусами и типами.
#' @export
#'
#' @examples
#' \dontrun{
#' my_campaigns <- yaf_get_campaigns("my_login")
#' }
yaf_get_campaigns <- function(login) {
  # URL для запроса (используем стабильную v5)
  url <- "https://api.direct.yandex.com/json/v5/campaigns"

  api_token <- get_yaf_token(login)

  # Создание запроса
  response <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_token),
      `Client-Login` = login,
      `Content-Type` = "application/json; charset=utf-8"
    ) |>
    httr2::req_body_json(list(
      method = "get",
      params = list(
        SelectionCriteria = list(
          Statuses = list("ACCEPTED", "DRAFT", "MODERATION", "REJECTED")
        ),
        FieldNames = c("Id", "Name", "Status", "State", "Type", "StartDate", "Statistics"),
        UnifiedCampaignFieldNames = c("BiddingStrategy", "AttributionModel")
      )
    )) |>
    httr2::req_perform()

  # Проверка статуса ответа
  if (httr2::resp_status(response) == 200) {
    result <- httr2::resp_body_json(response)

    # Проверка на внутреннюю ошибку API Яндекс Директа
    err <- result$error

    if (!is.null(err)) {
      message("Ошибка API Яндекс Директа:")
      print(err$error_string)
      print(err$error_code)
      print(err$error_detail)
    } else {
      # Парсинг JSON-ответа с помощью purrr
      # Используем .progress для наглядности, если кампаний много
      campaigns <- purrr::map_dfr(result$result$Campaigns, ~as.data.frame(t(unlist(.))))
    }

  } else {
    message(paste("Ошибка HTTP:", httr2::resp_status(response)))
    print(httr2::resp_body_string(response))
  }

  # Финальный возврат объекта
  if (exists("campaigns")) {
    return(campaigns)
  } else {
    message("FAILED. Check an error.")
    return(NULL)
  }
}
