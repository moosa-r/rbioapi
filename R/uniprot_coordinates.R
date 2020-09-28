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
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "chromosome",
                               class = "character",
                               max_len = 20),
                          list(arg = "ensembl",
                               class = "character",
                               max_len = 20),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "protein",
                               class = "character"),
                          list(arg = "taxid",
                               class = "character",
                               max_len = 20),
                          list(arg = "location",
                               class = "character")))

  v_msg(paste("get /coordinates Search genomic coordinates for UniProt entries"))
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("chromosome",
                                 any(!is.na(chromosome)),
                                 paste0(chromosome,
                                        collapse = ",")),
                            list("ensembl",
                                 any(!is.na(ensembl)),
                                 paste0(ensembl,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")),
                            list("protein",
                                 !is.na(protein),
                                 protein),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("location",
                                 !is.na(location),
                                 location))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "coordinates"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
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
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "p_position",
                               class = "numeric"),
                          list(arg = "p_start",
                               class = "numeric"),
                          list(arg = "pEnd",
                               class = "numeric")),
              cond = list(list(quote(sum(!is.na(p_position), !is.na(p_start), !is.na(p_end)) == 3),
                               "You can only provide either 'p_position' alone or 'p_start' and 'p_end' together"),
                          list(quote(sum(!is.na(p_position), !is.na(p_start), !is.na(p_end)) == 0),
                               "You should provide either 'p_position' alone or 'p_start' and 'p_end' together"),
                          list(quote(sum(!is.na(p_start), !is.na(p_end)) == 1),
                               "You should provide 'p_start' and 'p_end' togeather.")))

  v_msg(paste("get /coordinates/location/{accession}:{pPosition} Get genome coordinate by protein sequence position",
              "get /coordinates/location/{accession}:{pStart}-{pEnd} Get genome coordinate by protein sequence position range"))

  ## make function-specific calls
  path_input = sprintf("%scoordinates/location/%s:%s",
                       rba_ba_stg("uniprot", "pth"),
                       accession,
                       ifelse(!is.na(p_position),
                              yes = p_position,
                              no = paste0(p_start, "-", p_end)))

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
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
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "db_id",
                               class = "character")),
              cond = list(list(quote(sum(!is.na(accession), !is.na(db_type), !is.na(db_id)) == 3),
                               "You can only provide either 'accession' alone or 'db_type' and 'db_id' together."),
                          list(quote(sum(!is.na(accession), !is.na(db_type), !is.na(db_id)) == 0),
                               "You should provide either 'accession' alone or 'db_type' and 'db_id' together."),
                          list(quote(sum(!is.na(db_type), !is.na(db_id)) == 1),
                               "You should provide 'db_type' and 'db_id' togeather."))
  )

  v_msg(paste("get /coordinates/{accession} Get genomic coordinates for a UniProt accession",
              "get /coordinates/{dbtype}:{dbid} Search UniProt entries by genomic database cross reference IDs: Ensembl, CCDS, HGNC or RefSeq"))
  ## build GET API request's query
  call_query = list("size" = "-1")
  ## make function-specific calls
  path_input = sprintf("%scoordinates/%s",
                       rba_ba_stg("uniprot", "pth"),
                       ifelse(!is.na(accession),
                              yes = accession,
                              no = paste0(db_type, ":", db_id)))

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
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
  rba_ba_args(cons = list(list(arg = "taxid",
                               class = "numeric"),
                          list(arg = "locations",
                               class = "character"),
                          list(arg = "in_range",
                               class = "logical"),
                          list(arg = "feature",
                               class = "logical"))
  )

  v_msg(paste("get /coordinates/{taxonomy}/{locations} Search UniProt entries by taxonomy and genomic coordinates",
              "get /coordinates/{taxonomy}/{locations}/feature Search UniProt entries by taxonomy and genomic coordinates"))
  ## build GET API request's query
  call_query = list("size" = "-1")

  ## make function-specific calls
  path_input = sprintf("%scoordinates/%s/%s",
                       rba_ba_stg("uniprot", "pth"),
                       taxid,
                       locations)
  if (feature == TRUE) {
    path_input = paste0(path_input, "/feature")
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
