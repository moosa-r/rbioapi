#' Map a Set of Identifiers to String Identifiers
#' @description This function Calls String's API to Convert a set of identifiers
#'   to String Identifiers. Although You can call STRING with a variety of
#'   Identifiers, It is recommended by STRING's documentations that you first
#'   map Your Protein/genes Identifiers to STRING Identifiers and then proceed
#'   with other STRING API functions. This  function's Documentation is based
#'   on: \url{https://string-db.org/help/api/#mapping-identifiers}
#'
#' @param input A character or/and numeric vector containing your desired Identifiers to be mapped
#' @param species (optional) Numeric, NCBI taxon identifiers (e.g. Human is 9606, see:
#'   STRING organisms).
#' @param echo_query (optional) Logical, Insert column with your input identifier. (default = FALSE)
#' @param limit (optional) limits the number of matches per query identifier (best matches
#'   come first)
#' @param ...
#' @param caller_identity (optional) Character, your identifier for STRING servers.
#'
#' @references \url{https://string-db.org/help/api/#mapping-identifiers}
#' @return
#' @export
#'
#' @examples
rba_string_map_ids = function(input,
                              species = NA,
                              echo_query = FALSE,
                              limit = NA,
                              caller_identity = NA,
                              ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "echo_query",
                               class = "logical"),
                          list(arg = "limit",
                               class = "numeric"),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))
  v_msg("Mapping %s Input Identifiers to STRING Identifiers.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("echo_query",
                                echo_query == TRUE,
                                "1"),
                           list("limit",
                                !is.na(limit),
                                limit),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/resolve"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Getting STRING network image
#'
#' @param input
#' @param species
#' @param add_color_nodes
#' @param add_white_nodes
#' @param required_score
#' @param network_flavor
#' @param hide_node_labels
#' @param hide_disconnected_nodes
#' @param block_structure_pics_in_bubbles
#' @param caller_identity
#' @param save_file
#' @param ...
#' @param output_format
#'
#' @return
#' @export
#'
#' @examples
rba_string_network_image = function(input,
                                    output_format = "image",
                                    save_file = TRUE,
                                    species = NA,
                                    add_color_nodes = FALSE,
                                    add_white_nodes = NA,
                                    required_score = NA,
                                    network_flavor = "confidence",
                                    hide_node_labels = FALSE,
                                    hide_disconnected_nodes = FALSE,
                                    block_structure_pics_in_bubbles = FALSE,
                                    caller_identity = NA,
                                    ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "output_format",
                               val = c("image", "highres_image", "svg")),
                          list(arg = "save_file",
                               class = c("character",
                                         "logical")),
                          list(arg = "add_color_nodes",
                               class = "logical"),
                          list(arg = "add_white_nodes",
                               class = "numeric"),
                          list(arg = "required_score",
                               class = "numeric",
                               min_val = 0,
                               max_val = 1000),
                          list(arg = "network_flavor",
                               val = c("evidence", "confidence", "actions")),
                          list(arg = "hide_node_labels",
                               class = "logical"),
                          list(arg = "hide_disconnected_nodes",
                               class = "logical"),
                          list(arg = "block_structure_pics_in_bubbles",
                               class = "logical"),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))

  v_msg("Getting STRING network image of %s Input Identifiers.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("add_color_nodes",
                                add_color_nodes == TRUE,
                                "1"),
                           list("add_white_nodes",
                                !is.na(add_white_nodes),
                                add_white_nodes),
                           list("required_score",
                                !is.na(required_score),
                                required_score),
                           list("network_flavor",
                                !is.na(network_flavor),
                                network_flavor),
                           list("hide_node_labels",
                                hide_node_labels == TRUE,
                                "1"),
                           list("hide_disconnected_nodes",
                                hide_disconnected_nodes == TRUE,
                                "1"),
                           list("block_structure_pics_in_bubbles",
                                block_structure_pics_in_bubbles == TRUE,
                                "1"),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make file path
  if (output_format == "svg") {
    ext_input = "svg"
    accept_input = "image/svg+xml"
    parser_input = quote(httr::content(response))
  } else {
    ext_input = "png"
    accept_input = "image/png"
    parser_input = quote(httr::content(response,
                                       type = "image/png"))
  }
  save_file = rba_ba_file(file_ext = ext_input,
                          file_name = "string_network_image",
                          save_to = save_file)

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "image/network"),
                           accept = accept_input,
                           parser = parser_input,
                           body = call_body,
                           save_to = save_file)

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Getting the STRING network interactions
#'
#' @param input
#' @param species
#' @param required_score
#' @param add_nodes
#' @param ...
#' @param caller_identity
#'
#' @return
#' @export
#'
#' @examples
rba_string_network_interactions = function(input,
                                           species = NA,
                                           required_score = NA,
                                           add_nodes = NA,
                                           caller_identity = NA,
                                           ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "required_score",
                               class = "numeric",
                               min_val = 0,
                               max_val = 1000),
                          list(arg = "add_nodes",
                               class = "numeric",
                               min_val = 0),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))

  v_msg("Getting STRING Network interaction of %s inputs.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("required_score",
                                !is.na(required_score),
                                required_score),
                           list("add_nodes",
                                !is.na(add_nodes),
                                add_nodes),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity)
  )

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/network"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Getting all the STRING interaction partners of the protein set
#'
#' @param input
#' @param species
#' @param required_score
#' @param limit
#' @param ...
#' @param caller_identity
#'
#' @return
#' @export
#'
#' @examples
rba_string_interaction_partners = function(input,
                                           species = NA,
                                           required_score = NA,
                                           limit = NA,
                                           caller_identity = NA,
                                           ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "required_score",
                               class = "numeric",
                               min_val = 0,
                               max_val = 1000),
                          list(arg = "limit",
                               class = "numeric",
                               min_val = 1)),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))

  v_msg("Retrieving Interacting partners of ", length(input), " inputs.")

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("limit",
                                !is.na(limit),
                                limit),
                           list("required_score",
                                !is.na(required_score),
                                required_score),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "/json/interaction_partners"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}


#' Retrieving similarity scores of the protein set
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param ...
#' @param caller_identity
#'
#' @return
#' @export
#'
#' @examples
rba_string_homology = function(input,
                               species = NA,
                               caller_identity = NA,
                               ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))

  v_msg("Retrieving similarity scores between %s inputs.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/homology"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Retrieving best similarity hits between species
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param species_b
#' @param ...
#' @param caller_identity
#'
#' @return
#' @export
#'
#' @examples
rba_string_homology_best = function(input,
                                    species = NA,
                                    species_b = NA,
                                    caller_identity = NA,
                                    ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "species_b",
                               class = "numeric"),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))
  v_msg("Retrieving similarity scores between %s inputs.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("species_b",
                                !is.na(species_b),
                                paste(unique(species_b),collapse = "%0d")),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/homology_best"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}


#' Getting functional enrichment
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param caller_identity
#' @param background_string_ids
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_string_enrichment = function(input,
                                 species = NA,
                                 background_string_ids = NA,
                                 caller_identity = NA,
                                 ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg ="input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "background_string_ids",
                               class = "character"),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))
  v_msg("Retrieving similarity scores between %s inputs.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("background_string_identifiers",
                                !is.na(background_string_ids),
                                paste(unique(background_string_ids),
                                      collapse = "%0d")),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/enrichment"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}


#' Retrieving functional annotation
#'
#' @param input
#' @param species
#' @param allow_pubmed
#' @param ...
#' @param caller_identity
#'
#' @return
#' @export
#'
#' @examples
rba_string_functional_annotation = function(input,
                                            species = NA,
                                            allow_pubmed = FALSE,
                                            caller_identity = NA,
                                            ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "allow_pubmed",
                               class = "logical"),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))

  v_msg("Retrieving functional annotations of %s inputs.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("allow_pubmed",
                                allow_pubmed == TRUE,
                                1),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/functional_annotation"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}


#' Getting protein-protein interaction enrichment
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param required_score
#' @param ...
#' @param caller_identity
#'
#' @return
#' @export
#'
#' @examples
rba_string_ppi_enrichment = function(input,
                                     species = NA,
                                     required_score = NA,
                                     caller_identity = NA,
                                     ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "input",
                               class = c("character", "numeric")),
                          list(arg = "species",
                               class = "numeric"),
                          list(arg = "required_score",
                               class = "numeric",
                               min_val = 0,
                               max_val = 1000),
                          list(arg = "caller_identity",
                               class = "character")),
              cond = list(list("length(input) > 100 && is.na(species)",
                               sprintf("Input's length is %s. Please Specify the specie. (Homo Sapiens NCBI taxa ID is 9606.)",
                                       length(input)))
              ))

  v_msg("Performing PPI Enrichment of %s inputs.", length(input))

  ## build POST API request's body
  call_body = rba_ba_query(init = list("format" = "text",
                                       "identifiers" = paste(unique(input),
                                                             collapse = "%0d")),
                           list("species",
                                !is.na(species),
                                species),
                           list("required_score",
                                !is.na(required_score),
                                required_score),
                           list("caller_identity",
                                !is.na(caller_identity),
                                caller_identity))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/ppi_enrichment"),
                           body = call_body,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->df")
  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Getting current STRING version
#'
#' #' @family STRING
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_string_version = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("Retrieving Current STRING database version.")

  ## build POST API request's body
  call_query = list("format" = "text")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("string", "url"),
                           path = paste0(rba_ba_stg("string", "pth"),
                                         "json/version"),
                           body = call_query,
                           encode = "form",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}


#' Get Statistics from STRING database
#'
#' @param type
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_string_info = function(type = "statistics",
                           ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "type",
                               class = "character",
                               val = c("statistics", "organisms"))))

  if (type == "statistics") {
    v_msg("Retrieving STRNG statistics.")
    input_url = paste0("https://string-db.org/",
                       "cgi/about.pl?footer_active_subpage=statistics")
    input_html = xml2::read_html(input_url)
    input_html = rvest::html_nodes(input_html, ".footer_stats_table")
    output_table = rvest::html_table(input_html)

  } else if (type == "organisms") {
    v_msg("Retrieving STRNG Organism tubular view")
    input_url = paste0("https://string-db.org/",
                       "organism_overview.html")
    input_html = xml2::read_html(input_url)
    output_table = rvest::html_table(input_html)[[1]]
    output_table[[2]] = paste0("https://stringdb-static.org/",
                               output_table[[2]][[2]])
  }

  return(output_table)
}
