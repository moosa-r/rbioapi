#' Retrieve a List of available libraries from Enrichr
#' @description This function will retrieve a list of available libraries and
#' their statistics from Enrichr. You should call this function once per session
#' with argument 'store_in_options = TRUE' before querying data from Enrichr.
#' Nevertheless, rbioapi will do this for you in the background in the first
#' time you call any function pertinent to Enrichr.
#'
#' @param ...
#' @param store_in_options
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_info = function(store_in_options = TRUE,
                            ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "store_in_options",
                               class = "logical")))

  v_msg("Retrieving List of available libraries and statistics of Enrichr.")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "datasetStatistics"),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("enrichr_info.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  ## Save Library Names as Global Options
  if (store_in_options == TRUE) {
    options(rba_enrichr_libs = final_output[["statistics.libraryName"]])
  }
  return(final_output)
}

#' Upload your gene set to Enrichr
#'
#' @param description
#' @param ...
#' @param gene_list
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_add_list = function(gene_list,
                                description = NA,
                                ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_list",
                               class = "character"),
                          list(arg = "description",
                               class = "character")))

  v_msg("Uploading %s gene IDs to Enrichr.", length(gene_list))

  ## Build POST API Request's URL
  call_body = rba_ba_query(init = list("format" = "text",
                                       "list" = paste(unique(gene_list),
                                                      collapse = "\n")),
                           list("description",
                                !is.na(description),
                                description))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "addList"),
                           body = call_body,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("enrichr_add_list.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' View your uploaded gene list
#'
#' @param ...
#' @param user_list_id
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_view_list = function(user_list_id,
                                 ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "user_list_id",
                               class = c("numeric", "integer"),
                               len = 1)))

  v_msg("Retrieving your uploaded gene list under the ID %s.",
        user_list_id)

  ## Build GET API Request's query
  call_query = list("userListId" = user_list_id)

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "view"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file(sprintf("enrichr_view_list_%s.json",
                                                         user_list_id)))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Internal function for rba_enrichr_enrich
#'
#' @param user_list_id
#' @param gene_set_library
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_enrich_internal = function(user_list_id,
                                       gene_set_library,
                                       save_name,
                                       ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Build GET API Request's query
  call_query = list("userListId" = user_list_id,
                    "backgroundType" = gene_set_library)

  ## Build Function-Specific Call
  parser_input = quote(httr::content(response,
                                     as = "text",
                                     type = "text/tab-separated-values",
                                     encoding = "UTF-8"))

  input_call = rba_ba_httr(httr = "get",
                           rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "export"),
                           query = call_query,
                           httr::accept("text/tab-separated-values"),
                           parser = parser_input,
                           save_to = rba_ba_file(save_name))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  final_output = utils::read.delim(textConnection(final_output),
                                   sep = "\t", header = TRUE,
                                   stringsAsFactors = FALSE)
  return(final_output)
}

#' Get Enrichr enrichment results
#'
#' @param user_list_id
#' @param gene_set_library
#' @param multi_libs_progress_bar
#' @param regex_library_name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_enrich = function(user_list_id,
                              gene_set_library = "all",
                              regex_library_name = TRUE,
                              multi_libs_progress_bar = TRUE,
                              ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## get a list of available libraries
  if (is.null(getOption("rba_enrichr_libs"))){
    v_msg("Calling rba_enrichr_info() to get a list of available enricr libraries.")
    invisible(rba_enrichr_info(store_in_options = TRUE))
  }
  ## handle different gene_set_library input situations
  if (length(gene_set_library) > 1) {
    run_mode = "multiple"
  } else if (gene_set_library == "all") {
    run_mode = "multiple"
    gene_set_library = getOption("rba_enrichr_libs")
  } else {
    if (regex_library_name == FALSE) {
      run_mode = "single"
    } else {
      gene_set_library = grep(gene_set_library,
                              getOption("rba_enrichr_libs"),
                              ignore.case = TRUE, value = TRUE, perl = TRUE)
      #check the results of regex
      if (length(gene_set_library) == 0) {
        stop("Your regex pattern did not match any Enrichr library name.",
             call. = get("diagnostics"))
      } else if (length(gene_set_library) == 1) {
        run_mode = "single"
      } else if (length(gene_set_library) > 1) {
        run_mode = "multiple"
      }
    }
  } #end of if length(gene_set_library) > 1
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "user_list_id",
                               class = c("numeric", "integer"),
                               len = 1),
                          list(arg = "gene_set_library",
                               class = "character",
                               val = getOption("rba_enrichr_libs")),
                          list(arg = "multi_libs_progress_bar",
                               class = "logical")))
  ## call Enrichr API
  if (run_mode == "single") {
    v_msg("Enriching Gene set %s using Enrichr library: %s.",
          user_list_id, gene_set_library)
    final_output = rba_enrichr_enrich_internal(user_list_id = user_list_id,
                                               gene_set_library = gene_set_library,
                                               save_name = sprintf("enrichr_%s_%s.json",
                                                                   user_list_id,
                                                                   gene_set_library),
                                               ...)
    return(final_output)

  } else {
    v_msg("Enriching Gene set %s using multiple Enrichr libraries.",
          user_list_id)
    v_msg(paste0("Note: You have selected '%s' Enrichr libraries. note that for ",
                 "each library, a seperate call should be send to the Enrichr server. ",
                 "thus, this could take a while depending on the number of selected ",
                 "libraries and your network connection."), length(gene_set_library))
    ## initiate progress bar
    if (multi_libs_progress_bar == TRUE) {
      pb = utils::txtProgressBar(min = 0,
                                 max = length(gene_set_library),
                                 style = 3)
    }
    final_output = lapply(gene_set_library,
                          function(x){
                            lib_enrich_res = rba_enrichr_enrich_internal(user_list_id = user_list_id,
                                                                         gene_set_library = x,
                                                                         save_name = sprintf("enrichr_%s_%s.json",
                                                                                             user_list_id,
                                                                                             x),
                                                                         ...)
                            #advance the progress bar
                            if (multi_libs_progress_bar == TRUE) {
                              utils::setTxtProgressBar(pb, which(gene_set_library == x))
                            }
                            return(lib_enrich_res)
                          })
    close(pb)
    names(final_output) = gene_set_library
    return(final_output)
  }
}


#' Find terms that contain a given gene
#'
#' @param gene
#' @param ...
#' @param catagorize
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr_gene_map = function(gene,
                                catagorize = FALSE,
                                ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene",
                               class = "character",
                               len = 1),
                          list(arg = "catagorize",
                               class = "logical")))

  v_msg("Finding terms that contain gene: %s", gene)

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("gene" = gene,
                                        "json" = "true"),
                            list("setup",
                                 catagorize == TRUE,
                                 "true"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "genemap"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("enrichr_gene_map.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Gene Set Enrichment Using Enrichr With One Function Call
#'
#' @param gene_list
#' @param description
#' @param gene_set_library
#' @param regex_library_name
#' @param ...
#' @param multi_libs_progress_bar
#'
#' @return
#' @export
#'
#' @examples
rba_enrichr = function(gene_list,
                       description = NA,
                       gene_set_library = "all",
                       regex_library_name = TRUE,
                       multi_libs_progress_bar = TRUE,
                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_list",
                               class = "character"),
                          list(arg = "description",
                               class = "character"),
                          list(arg = "regex_library_name",
                               class = "logical"),
                          list(arg = "multi_libs_progress_bar",
                               class = "logical")))
  v_msg("--Step 1/3:")
  invisible(rba_enrichr_info(store_in_options = TRUE,
                             ...))
  v_msg("--Step 2/3:")
  list_id = rba_enrichr_add_list(gene_list = gene_list,
                                 description = description,
                                 ...)
  v_msg("--Step 3/3:")
  enriched = rba_enrichr_enrich(user_list_id = list_id$userListId,
                                gene_set_library = gene_set_library,
                                regex_library_name = regex_library_name,
                                multi_libs_progress_bar = multi_libs_progress_bar,
                                ...)
  return(enriched)
}
