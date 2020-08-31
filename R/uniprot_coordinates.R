#' Search genomic coordinates for UniProt entries
#'
#' @param accession
#' @param chromosome
#' @param ensembl
#' @param gene
#' @param protein
#' @param taxid
#' @param location
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_coordinates_search = function(accession = NA,
                                          chromosome = NA,
                                          ensembl = NA,
                                          gene = NA,
                                          protein = NA,
                                          taxid = NA,
                                          location = NA,
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = chromosome,
                                         name = "chromosome",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = ensembl,
                                         name = "ensembl",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = gene,
                                         name = "gene",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = protein,
                                         name = "protein",
                                         class = "character"),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = location,
                                         name = "location",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /coordinates Search genomic coordinates for UniProt entries")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(any(!is.na(chromosome)),
                              list("chromosome" = paste0(chromosome,
                                                         collapse = ","))),
                         list(any(!is.na(ensembl)),
                              list("ensembl" = paste0(ensembl,
                                                      collapse = ","))),
                         list(any(!is.na(gene)),
                              list("gene" = paste0(gene,
                                                   collapse = ","))),
                         list(!is.na(protein),
                              list("protein" = protein)),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ","))),
                         list(!is.na(location),
                              list("location" = location))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "coordinates"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get genome coordinate by protein sequence position/ position range
#'
#' @param accession
#' @param p_position
#' @param p_start
#' @param p_end
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_coordinates_location = function(accession,
                                            p_position = NA,
                                            p_start = NA,
                                            p_end = NA,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character"),
                                    list(arg = p_position,
                                         name = "p_position",
                                         class = "numeric"),
                                    list(arg = p_start,
                                         name = "p_start",
                                         class = "numeric"),
                                    list(arg = p_end,
                                         name = "pEnd",
                                         class = "numeric")),
                        cond = list(list(sum(!is.na(p_position), !is.na(p_start), !is.na(p_end)) == 3,
                                         "You can only provide either 'p_position' alone or 'p_start' and 'p_end' together"),
                                    list(sum(!is.na(p_position), !is.na(p_start), !is.na(p_end)) == 0,
                                         "You should provide either 'p_position' alone or 'p_start' and 'p_end' together"),
                                    list(sum(!is.na(p_start), !is.na(p_end)) == 1,
                                         "You should provide 'p_start' and 'p_end' togeather.")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /coordinates/location/{accession}:{pPosition} Get genome coordinate by protein sequence position",
            "get /coordinates/location/{accession}:{pStart}-{pEnd} Get genome coordinate by protein sequence position range")
  }

  ## make function-specific calls
  if (!is.na(p_position)) {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "coordinates/location/",
                        accession, ":",
                        p_position)
  } else {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "coordinates/location/",
                        accession, ":",
                        p_start, "-", p_end)
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = path_input,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get genomic coordinates for a UniProt accession Search UniProt entries by
#' genomic database cross reference IDs: Ensembl, CCDS, HGNC or RefSeq
#'
#' @param accession
#' @param db_type
#' @param db_id
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_coordinates = function(accession = NA,
                                   db_type = NA,
                                   db_id = NA,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character"),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = db_id,
                                         name = "db_id",
                                         class = "character")),
                        cond = list(list(sum(!is.na(accession), !is.na(db_type), !is.na(db_id)) == 3,
                                         "You can only provide either 'accession' alone or 'db_type' and 'db_id' together."),
                                    list(sum(!is.na(accession), !is.na(db_type), !is.na(db_id)) == 0,
                                         "You should provide either 'accession' alone or 'db_type' and 'db_id' together."),
                                    list(sum(!is.na(db_type), !is.na(db_id)) == 1,
                                         "You should provide 'db_type' and 'db_id' togeather.")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /coordinates/{accession} Get genomic coordinates for a UniProt accession",
            "get /coordinates/{dbtype}:{dbid} Search UniProt entries by genomic database cross reference IDs: Ensembl, CCDS, HGNC or RefSeq")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  ## make function-specific calls
  if (!is.na(accession)) {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "coordinates/",
                        accession)
  } else {
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "coordinates/",
                        db_type, ":", db_id)
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = path_input,
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Search UniProt entries by taxonomy and genomic coordinates
#'
#' @param taxid
#' @param locations
#' @param in_range
#' @param feature
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_coordinates_taxonomy = function(taxid,
                                            locations,
                                            in_range = NA,
                                            feature = FALSE,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = taxid,
                                         name = "taxid",
                                         class = "numeric"),
                                    list(arg = locations,
                                         name = "locations",
                                         class = "character"),
                                    list(arg = in_range,
                                         name = "in_range",
                                         class = "logical"),
                                    list(arg = feature,
                                         name = "feature",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /coordinates/{taxonomy}/{locations} Search UniProt entries by taxonomy and genomic coordinates",
            "get /coordinates/{taxonomy}/{locations}/feature Search UniProt entries by taxonomy and genomic coordinates")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  ## make function-specific calls
  path_input = paste0(getOption("rba_pth_uniprot"),
                      "coordinates/",
                      taxid, "/",
                      locations)

  if (feature == TRUE) {
    path_input = paste0(path_input, "/feature")
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = path_input,
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}
