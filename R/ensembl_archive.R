#### Archive Endpoints ####

#' Retrieve the latest version for a set of identifiers
#'
#' @param ids
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_archive = function(ids,
                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 1000)))
  v_msg("POST archive/id")
  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("id" = as.array(ids)))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "archive/id",
                           body = call_body,
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_archive.json"))
  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
