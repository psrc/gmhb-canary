# Observed public Order Search contract; no browser session values are stored.
cms_page_url <- "https://eluho2022.my.site.com/casemanager/s/order-search"
cms_fields <- c("Board__c", "Name", "Case_Link__c", "Docket_Number__c",
                "Document_Date__c", "Document_Type__c", "Case__c")

cms_error <- function(message) {
  error <- simpleError(message, call = NULL)
  class(error) <- c("gmhb_error", class(error))
  stop(error)
}

valid_date <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    cms_error("Expected a date in YYYY-MM-DD format.")
  }
  parsed <- as.Date(value, format = "%Y-%m-%d")
  if (is.na(parsed) || format(parsed, "%Y-%m-%d") != value) {
    cms_error("Invalid calendar date.")
  }
  value
}

previous_month <- function(today = as.Date(format(Sys.time(), tz = "America/Los_Angeles",
                                                 format = "%Y-%m-%d"))) {
  first <- as.Date(format(today, "%Y-%m-01"))
  list(since = format(first - 1L, "%Y-%m-01"), before = format(first, "%Y-%m-%d"))
}

parse_json <- function(text, stage) {
  tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
           error = function(e) cms_error(paste(stage, "did not contain valid JSON.")))
}

scalar_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

discover_context <- function(html) {
  # The inline script URL embeds the current framework and application versions.
  # Parse that JSON as data; never evaluate scripts or reuse a HAR's runtime values.
  pattern <- '/casemanager/s/sfsites/l/([^/"<>[:space:]]+)/inline\\.js'
  match <- regmatches(html, regexec(pattern, html, perl = TRUE))[[1L]]
  if (length(match) != 2L) cms_error("CMS bootstrap changed: Aura context URL missing.")
  runtime <- parse_json(utils::URLdecode(match[2L]), "CMS bootstrap")
  app_key <- "APPLICATION@markup://siteforce:communityApp"
  if (!is.list(runtime) || !scalar_text(runtime$mode) ||
      !scalar_text(runtime$fwuid) || !identical(runtime$app, "siteforce:communityApp") ||
      !is.list(runtime$loaded) || !scalar_text(runtime$loaded[[app_key]])) {
    cms_error("CMS bootstrap changed: required Aura context values missing.")
  }
  list(mode = runtime$mode, fwuid = runtime$fwuid, app = runtime$app,
       loaded = runtime$loaded, dn = list(), globals = setNames(list(), character()),
       uad = FALSE)
}

order_message <- function(since, before = NULL) {
  since <- valid_date(since)
  where <- paste0("RecordType.Name = 'Order' AND Case__r.RecordType.Name = 'GMHB'",
                  " AND Document_Date__c >= ", since, "T00:00:00Z")
  if (!is.null(before)) {
    before <- valid_date(before)
    if (before <= since) cms_error("The exclusive end date must be after the start date.")
    # Reproduce the observed UI's inclusive upper timestamp. The local filter
    # still excludes this date to keep reports within the previous month.
    where <- paste0(where, " AND Document_Date__c <= ", before, "T23:59:59Z")
  }
  list(actions = list(list(
    id = "1;a", descriptor = "aura://ApexActionController/ACTION$execute",
    callingDescriptor = "UNKNOWN",
    params = list(namespace = "", classname = "DecisionSearchController",
                  method = "getRequestedRecords",
                  params = list(apiName = "ELUHO_Document__c", fields = cms_fields,
                                whereClause = where, eluhoFields = cms_fields),
                  cacheable = FALSE, isContinuation = FALSE))))
}

parse_orders <- function(text) {
  response <- parse_json(text, "CMS response")
  if (!is.list(response) || !is.list(response$actions) ||
      length(response$actions) != 1L || !is.list(response$actions[[1L]])) {
    cms_error("CMS contract changed: expected one Aura action.")
  }
  action <- response$actions[[1L]]
  if (!identical(action$id, "1;a") || !identical(action$state, "SUCCESS")) {
    # Do not include raw server errors: they may contain session details.
    cms_error("CMS action failed or returned an unexpected ID; check access and the Aura contract.")
  }
  if (!is.list(action$returnValue) || !("returnValue" %in% names(action$returnValue))) {
    cms_error("CMS contract changed: missing nested returnValue.")
  }
  records <- action$returnValue$returnValue
  if (!is.list(records) || !is.null(names(records))) {
    cms_error("CMS contract changed: records must be a JSON array.")
  }
  columns <- c("Id", cms_fields, "RecordTypeId")
  empty <- as.data.frame(setNames(rep(list(character()), length(columns)), columns))
  if (!length(records)) return(empty)
  required <- c("Id", "Board__c", "Name", "Document_Date__c", "Case__c")
  rows <- lapply(records, function(record) {
    if (!is.list(record) || !all(required %in% names(record)) ||
        !all(vapply(record[required], scalar_text, logical(1)))) {
      cms_error("CMS contract changed: an order is missing required text fields.")
    }
    if (!identical(record$Board__c, "GMHB") ||
        !grepl("^[A-Za-z0-9]{18}$", record$Id) ||
        !grepl("^[A-Za-z0-9]{18}$", record$Case__c)) {
      cms_error("CMS contract changed: unexpected board or record identifiers.")
    }
    stamp <- record$Document_Date__c
    if (!grepl(paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}T([01][0-9]|2[0-3]):",
                      "[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?Z$"), stamp) ||
        is.na(as.POSIXct(stamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))) {
      cms_error("CMS contract changed: unexpected document timestamp.")
    }
    values <- lapply(columns, function(column) {
      value <- record[[column]]
      if (is.null(value)) return(NA_character_)
      if (length(value) != 1L || !(is.character(value) || is.numeric(value)) || is.na(value)) {
        cms_error("CMS contract changed: unexpected field value.")
      }
      as.character(value)
    })
    as.data.frame(setNames(values, columns), stringsAsFactors = FALSE)
  })
  orders <- do.call(rbind, rows)
  if (anyDuplicated(orders$Id)) cms_error("CMS contract changed: duplicate order IDs.")
  orders[order(orders$Document_Date__c, orders$Id), , drop = FALSE]
}

select_window <- function(orders, since, before = NULL) {
  since <- valid_date(since)
  if (!is.null(before)) {
    before <- valid_date(before)
    if (before <= since) cms_error("The exclusive end date must be after the start date.")
  }
  dates <- substr(orders$Document_Date__c, 1L, 10L)
  if (any(dates < since)) cms_error("CMS contract changed: response violates the date filter.")
  if (is.null(before)) orders else orders[dates < before, , drop = FALSE]
}

perform_cms_request <- function(request, stage) {
  tryCatch(httr2::req_perform(request), error = function(e) {
    status <- if (!is.null(e$resp)) httr2::resp_status(e$resp) else NULL
    detail <- if (is.null(status)) "network/TLS/timeout failure" else paste("HTTP", status)
    cms_error(paste0("CMS ", stage, " failed (", detail, ")."))
  })
}

fetch_orders <- function(since, before = NULL, perform = perform_cms_request) {
  # Validate everything before network access.
  select_window(data.frame(Document_Date__c = character()), since, before)
  cookie_file <- tempfile("gmhb-cookies-")
  on.exit(unlink(cookie_file), add = TRUE)
  request <- function(url) {
    httr2::request(url) |>
      httr2::req_user_agent("gmhb-canary/0.1 (public Order Search)") |>
      httr2::req_timeout(60) |>
      httr2::req_cookie_preserve(cookie_file)
  }
  page <- perform(request(cms_page_url), "page initialization")
  context <- discover_context(httr2::resp_body_string(page))
  json <- function(value) as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"))
  req <- request(sub("order-search$", "sfsites/aura", cms_page_url)) |>
    httr2::req_body_form(message = json(order_message(since, before)),
                       `aura.context` = json(context),
                       `aura.pageURI` = "/casemanager/s/order-search",
                       `aura.token` = "null")
  response <- perform(req, "order search")
  content_type <- httr2::resp_header(response, "content-type")
  if (is.null(content_type) || !grepl("application/json", content_type, fixed = TRUE)) {
    cms_error("CMS contract changed: expected a JSON response, possibly received a login page.")
  }
  orders <- parse_orders(httr2::resp_body_string(response))
  if (!is.null(before)) {
    upper <- as.POSIXct(paste0(before, "T23:59:59Z"), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    stamps <- as.POSIXct(orders$Document_Date__c, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    if (any(stamps > upper)) cms_error("CMS contract changed: response violates the upper date filter.")
  }
  retrieved <- nrow(orders)
  orders <- select_window(orders, since, before)
  attr(orders, "retrieved_count") <- retrieved
  # The user observed 50 rows per UI page. This is a conservative review
  # threshold, not a verified API limit; check the count before local filtering.
  attr(orders, "manual_review_required") <- retrieved >= 50L
  if (retrieved >= 50L) {
    warning(sprintf(paste0("CMS returned %d records (manual-review threshold: 50). ",
                           "Results may be incomplete; check the CMS for additional orders: %s"),
                    retrieved, cms_page_url), call. = FALSE, immediate. = TRUE)
  }
  orders
}
