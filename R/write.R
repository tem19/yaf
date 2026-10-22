#' Произвольный метод API Яндекс Директа
#'
#' Один запрос к любому методу любого сервиса. Для изменяющих методов
#' (add, update, delete, moderate...) сетевые ошибки и HTTP 5xx не
#' повторяются: запрос мог выполниться, и повтор создал бы дубли.
#' Временные ошибки API (коды 52, 506, 1000-1002) повторяются всегда:
#' при них запрос не выполнен.
#'
#' @param login Логин в Яндексе.
#' @param service Сервис API, например "campaigns".
#' @param method Метод, например "add".
#' @param params Список params запроса. Массивы передавайте как list():
#'   тело сериализуется с auto_unbox = TRUE, вектор из одного элемента
#'   уйдёт скаляром.
#' @param version Версия API: "v5" или "v501". Единые перфоманс-кампании
#'   и их группы создаются только через "v501".
#' @param max_retries Максимум повторов при временных ошибках.
#' @param retry_network Повторять ли запрос при сетевой ошибке и HTTP 5xx.
#'   По умолчанию — только для get.
#'
#' @return Поле result ответа API. Атрибут "units" — баллы после запроса.
#' @export
#'
#' @examples
#' \dontrun{
#' yaf_api_call("my_login", "campaigns", "suspend",
#'              list(SelectionCriteria = list(Ids = list(123))))
#' }
yaf_api_call <- function(login, service, method, params = list(), version = "v5",
                         max_retries = 5, retry_network = identical(method, "get")) {
  token <- get_yaf_token(login)
  params <- yaf_prepare_params(params)
  res <- yaf_api_request(login, service, list(method = method, params = params), token,
                         max_retries, version = version, retry_network = retry_network)
  out <- res$result
  attr(out, "units") <- res$units
  out
}

#' Таблица из ActionResult
#'
#' @param results Список ActionResult из ответа API (AddResults и т. п.).
#'
#' @return data.frame с колонками Id (NA, если объект не создан), Errors и
#'   Warnings ("код: текст (подробности); ...", NA — если их нет).
#' @keywords internal
yaf_action_results <- function(results) {
  fmt <- function(x) {
    if (length(x) == 0) return(NA_character_)
    paste(vapply(x, function(e) {
      details <- e$Details %||% ""
      paste0(e$Code, ": ", e$Message, if (nzchar(details)) paste0(" (", details, ")") else "")
    }, ""), collapse = "; ")
  }
  data.frame(
    Id = vapply(results, function(r) as.numeric(r$Id %||% NA), numeric(1)),
    Errors = vapply(results, function(r) fmt(r$Errors), ""),
    Warnings = vapply(results, function(r) fmt(r$Warnings), ""),
    stringsAsFactors = FALSE
  )
}

#' Метод add любого сервиса с батчами
#'
#' Режет items на батчи по лимиту сервиса и отправляет их по очереди.
#' Ошибки отдельных объектов не останавливают выполнение: они в колонке
#' Errors. Ошибка всего запроса (авторизация, баллы, сеть) — останавливает
#' с классом yaf_api_error; объекты из уже отправленных батчей при этом
#' остаются созданными.
#'
#' @inheritParams yaf_api_call
#' @param items Список объектов для создания (Add-структуры API).
#' @param field Имя массива в params: "Campaigns", "AdGroups", "Ads" и т. д.
#' @param batch_size Объектов в одном запросе.
#'
#' @return data.frame Id, Errors, Warnings — по строке на объект, в порядке
#'   items. Атрибут "units" — баллы после последнего запроса.
#' @export
yaf_api_add <- function(login, service, items, field, batch_size = 1000,
                        version = "v5", max_retries = 5) {
  out <- yaf_action_results(list())
  units <- yaf_parse_units(NULL)
  if (length(items) == 0) return(structure(out, units = units))
  batches <- unname(split(items, ceiling(seq_along(items) / batch_size)))
  for (batch in batches) {
    res <- yaf_api_call(login, service, "add", stats::setNames(list(unname(batch)), field),
                        version = version, max_retries = max_retries)
    out <- rbind(out, yaf_action_results(res$AddResults))
    units <- attr(res, "units")
  }
  structure(out, units = units)
}

#' Метод по списку Id: moderate, suspend, resume, archive, delete
#'
#' @inheritParams yaf_api_call
#' @param ids Вектор Id объектов.
#' @param batch_size Id в одном запросе.
#'
#' @return data.frame Id, Errors, Warnings — по строке на Id. Атрибут
#'   "units" — баллы после последнего запроса.
#' @keywords internal
yaf_api_ids_action <- function(login, service, method, ids, batch_size = 1000,
                               version = "v5", max_retries = 5) {
  out <- yaf_action_results(list())
  units <- yaf_parse_units(NULL)
  ids <- unique(as.numeric(ids))
  if (length(ids) == 0) return(structure(out, units = units))
  field <- paste0(toupper(substring(method, 1, 1)), substring(method, 2), "Results")
  for (batch in unname(split(ids, ceiling(seq_along(ids) / batch_size)))) {
    res <- yaf_api_call(login, service, method, list(SelectionCriteria = list(Ids = as.list(batch))),
                        version = version, max_retries = max_retries)
    out <- rbind(out, yaf_action_results(res[[field]]))
    units <- attr(res, "units")
  }
  structure(out, units = units)
}

#' Создание кампаний
#'
#' Не больше 10 кампаний в запросе — батчи режутся автоматически.
#' Единые перфоманс-кампании (UnifiedCampaign) создаются только через v501.
#'
#' @param login Логин в Яндексе.
#' @param campaigns Список CampaignAddItem: Name, StartDate, NegativeKeywords,
#'   UnifiedCampaign / TextCampaign и т. д.
#' @param version Версия API, по умолчанию "v501".
#'
#' @return data.frame Id, Errors, Warnings — по строке на кампанию.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- yaf_add_campaigns("my_login", list(list(
#'   Name = "Поиск | Москва", StartDate = format(Sys.Date()),
#'   UnifiedCampaign = list(BiddingStrategy = list(
#'     Search = list(BiddingStrategyType = "WB_MAXIMUM_CLICKS",
#'                   WbMaximumClicks = list(WeeklySpendLimit = 3e9)),
#'     Network = list(BiddingStrategyType = "SERVING_OFF")
#'   ))
#' )))
#' }
yaf_add_campaigns <- function(login, campaigns, version = "v501") {
  yaf_api_add(login, "campaigns", campaigns, "Campaigns", batch_size = 10, version = version)
}

#' Создание групп объявлений
#'
#' Группы единой перфоманс-кампании (UnifiedAdGroup) создаются только через v501.
#'
#' @param login Логин в Яндексе.
#' @param adgroups Список AdGroupAddItem: Name, CampaignId, RegionIds
#'   (массив: list(213)), NegativeKeywords, UnifiedAdGroup и т. д.
#' @param version Версия API, по умолчанию "v501".
#'
#' @return data.frame Id, Errors, Warnings — по строке на группу.
#' @export
yaf_add_adgroups <- function(login, adgroups, version = "v501") {
  yaf_api_add(login, "adgroups", adgroups, "AdGroups", batch_size = 1000, version = version)
}

#' Создание ключевых фраз
#'
#' @param login Логин в Яндексе.
#' @param keywords Список KeywordAddItem: AdGroupId, Keyword, Bid и т. д.
#' @param version Версия API.
#'
#' @return data.frame Id, Errors, Warnings — по строке на фразу.
#' @export
yaf_add_keywords <- function(login, keywords, version = "v5") {
  yaf_api_add(login, "keywords", keywords, "Keywords", batch_size = 1000, version = version)
}

#' Создание объявлений
#'
#' Объявления создаются черновиками; на модерацию — yaf_moderate_ads().
#' Комбинаторные объявления (ResponsiveAd) API принимает только через v501,
#' поэтому v501 — по умолчанию.
#'
#' @param login Логин в Яндексе.
#' @param ads Список AdAddItem: AdGroupId и ResponsiveAd / TextAd и т. д.
#' @param version Версия API, по умолчанию "v501".
#'
#' @return data.frame Id, Errors, Warnings — по строке на объявление.
#' @export
yaf_add_ads <- function(login, ads, version = "v501") {
  yaf_api_add(login, "ads", ads, "Ads", batch_size = 1000, version = version)
}

#' Создание наборов быстрых ссылок
#'
#' @param login Логин в Яндексе.
#' @param sets Список SitelinksSetAddItem: list(Sitelinks = list(list(Title,
#'   Href, Description), ...)), от 1 до 8 ссылок в наборе.
#' @param version Версия API.
#'
#' @return data.frame Id, Errors, Warnings — по строке на набор.
#' @export
yaf_add_sitelinks <- function(login, sets, version = "v5") {
  yaf_api_add(login, "sitelinks", sets, "SitelinksSets", batch_size = 1000, version = version)
}

#' Создание расширений объявлений (уточнений)
#'
#' @param login Логин в Яндексе.
#' @param extensions Список AdExtensionAddItem: list(Callout = list(CalloutText = "...")).
#' @param version Версия API.
#'
#' @return data.frame Id, Errors, Warnings — по строке на расширение.
#' @export
yaf_add_adextensions <- function(login, extensions, version = "v5") {
  yaf_api_add(login, "adextensions", extensions, "AdExtensions", batch_size = 1000, version = version)
}

#' Отправка объявлений на модерацию
#'
#' @param login Логин в Яндексе.
#' @param ids Вектор Id объявлений.
#' @param version Версия API.
#'
#' @return data.frame Id, Errors, Warnings — по строке на объявление.
#' @export
yaf_moderate_ads <- function(login, ids, version = "v5") {
  yaf_api_ids_action(login, "ads", "moderate", ids, batch_size = 10000, version = version)
}

#' Остановка кампаний
#'
#' @param login Логин в Яндексе.
#' @param ids Вектор Id кампаний.
#' @param version Версия API.
#'
#' @return data.frame Id, Errors, Warnings — по строке на кампанию.
#' @export
yaf_suspend_campaigns <- function(login, ids, version = "v5") {
  yaf_api_ids_action(login, "campaigns", "suspend", ids, batch_size = 1000, version = version)
}

#' Возобновление показов кампаний
#'
#' @inheritParams yaf_suspend_campaigns
#'
#' @return data.frame Id, Errors, Warnings — по строке на кампанию.
#' @export
yaf_resume_campaigns <- function(login, ids, version = "v5") {
  yaf_api_ids_action(login, "campaigns", "resume", ids, batch_size = 1000, version = version)
}

#' Удаление кампаний
#'
#' Удалить можно только кампанию, по которой не было показов (например,
#' черновик). Остальные — архивировать.
#'
#' @inheritParams yaf_suspend_campaigns
#'
#' @return data.frame Id, Errors, Warnings — по строке на кампанию.
#' @export
yaf_delete_campaigns <- function(login, ids, version = "v5") {
  yaf_api_ids_action(login, "campaigns", "delete", ids, batch_size = 1000, version = version)
}
