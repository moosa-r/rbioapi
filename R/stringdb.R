

#' Map a Set of Identifiers to String Identifiers
#' @description This function Calls String's API to Convert a set of identifiers
#'   to String Identifiers. Although You can call STRING with a variety of
#'   Identifiers, It is recommended by STRING's documentations that you first
#'   map Your Protein/genes Identifiers to STRING Identifiers and then proceed
#'   with other STRING API functions. This  function's Documentation is based
#'   on: \url{https://string-db.org/help/api/#mapping-identifiers}
#' @family STRING
#'
#' @param input A character or/and numeric vector containing your desired Identifiers to be mapped
#' @param species (optional) Numeric, NCBI taxon identifiers (e.g. Human is 9606, see:
#'   STRING organisms).
#' @param echo_query (optional) Logical, Insert column with your input identifier. (default = FALSE)
#' @param limit (optional) limits the number of matches per query identifier (best matches
#'   come first)
#' @param caller_identity (optional) Character, your identifier for STRING servers.
#' @param progress_bar Logical, Display Progress bar? (default = FALSE)
#' @param verbose Should the function print details? (default = FALSE)
#' @param diagnostics
#'
#' @references \url{https://string-db.org/help/api/#mapping-identifiers}
#' @return
#' @export
#'
#' @examples
rba_string_map_ids = function(input,
                               species = NA,
                               echo_query = FALSE,
                               limit = 1,
                               caller_identity = NA,
                               verbose = TRUE,
                               progress_bar = FALSE,
                               diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = echo_query,
                                                    name = "echo_query",
                                                    class = "logical"),
                                               list(arg = limit,
                                                    name = "limit",
                                                    class = "numeric"),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Mapping ", length(input),
            " Input Identifiers to STRING Identifiers.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(echo_query == TRUE,
                              list("echo_query" = "1")),
                         list(!is.na(limit),
                              list("limit" = limit)),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/resolve",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)


  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE), stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}



#' Getting STRING network image
#'
#' #' @family STRING
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
#' @param verbose
#' @param output_format
#' @param save_to
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_network_image = function(input,
                                    output_format = "image",
                                    save_to = NA,
                                    species = NA,
                                    add_color_nodes = FALSE,
                                    add_white_nodes = NA,
                                    required_score = NA,
                                    network_flavor = "confidence",
                                    hide_node_labels = FALSE,
                                    hide_disconnected_nodes = FALSE,
                                    block_structure_pics_in_bubbles = FALSE,
                                    caller_identity = NA,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = output_format,
                                                    name = "output_format",
                                                    val = c("image", "highres_image", "svg")),
                                               list(arg = save_to,
                                                    name = "save_to",
                                                    class = "character"),
                                               list(arg = add_color_nodes,
                                                    name = "add_color_nodes",
                                                    class = "logical"),
                                               list(arg = add_white_nodes,
                                                    name = "add_white_nodes",
                                                    class = "numeric"),
                                               list(arg = required_score,
                                                    name = "required_score",
                                                    class = "numeric",
                                                    min_val = 0,
                                                    max_val = 1000),
                                               list(arg = network_flavor,
                                                    name = "network_flavor",
                                                    val = c("evidence", "confidence", "actions")),
                                               list(arg = hide_node_labels,
                                                    name = "hide_node_labels",
                                                    class = "logical"),
                                               list(arg = hide_disconnected_nodes,
                                                    name = "hide_disconnected_nodes",
                                                    class = "logical"),
                                               list(arg = block_structure_pics_in_bubbles,
                                                    name = "block_structure_pics_in_bubbles",
                                                    class = "logical"),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Getting STRING network image of ", length(input),
        " Input Identifiers.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(add_color_nodes == TRUE,
                              list("add_color_nodes" = "1")),
                         list(!is.na(add_white_nodes),
                              list("add_white_nodes" = add_white_nodes)),
                         list(!is.na(required_score),
                              list("required_score" = required_score)),
                         list(!is.na(network_flavor),
                              list("network_flavor" = network_flavor)),
                         list(hide_node_labels == TRUE,
                              list("hide_node_labels" = "1")),
                         list(hide_disconnected_nodes == TRUE,
                              list("hide_disconnected_nodes" = "1")),
                         list(block_structure_pics_in_bubbles == TRUE,
                              list("block_structure_pics_in_bubbles" = "1")),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)


  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/image/network",
                                     body = call_body,
                                     httr::write_disk(save_to, overwrite = TRUE),
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(httr::content(output,
                                              type = "image/png"))

  # create file_path
  if (is.na(save_to)){
    file_name = paste0(paste0(input[seq_len(min(3, length(input)))], collapse = "_"),
                       ifelse(output_format == "svg", ".svg", ".png"))
    save_to = file.path(getwd(), "STRING_network_images", file_name)
    dir.create(dirname(save_to), showWarnings = TRUE, recursive = TRUE)
    message("No file path was provided with 'save_to' argument.",
            " Saving to:\r\n", save_to, "\r\n")
  } else {
    if (verbose == TRUE) {
      message("Saving to:\r\n", save_to, "\r\n")
    }
  }
  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}



#' Getting the STRING network interactions
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param required_score
#' @param add_nodes
#' @param caller_identity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = required_score,
                                                    name = "required_score",
                                                    class = "numeric",
                                                    min_val = 0,
                                                    max_val = 1000),
                                               list(arg = add_nodes,
                                                    name = "add_nodes",
                                                    class = "numeric",
                                                    min_val = 0),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Getting STRING Network interaction of ", length(input), " inputs.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))


  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(required_score),
                              list("required_score" = required_score)),
                         list(!is.na(add_nodes),
                              list("add_nodes" = add_nodes)),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/network",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}




#' Getting all the STRING interaction partners of the protein set
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param required_score
#' @param limit
#' @param caller_identity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
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
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = required_score,
                                                    name = "required_score",
                                                    class = "numeric",
                                                    min_val = 0,
                                                    max_val = 1000),
                                               list(arg = limit,
                                                    name = "limit",
                                                    class = "numeric",
                                                    min_val = 1),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Retrieving Interacting partners of ", length(input), " inputs.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(limit),
                              list("limit" = limit)),
                         list(!is.na(required_score),
                              list("required_score" = required_score)),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/interaction_partners",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}


#' Retrieving similarity scores of the protein set
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param caller_identity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_homology = function(input,
                               species = NA,
                               caller_identity = NA,
                               verbose = TRUE,
                               progress_bar = FALSE,
                               diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Retrieving similarity scores between ", length(input), " inputs.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/homology",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#' Retrieving best similarity hits between species
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param species_b
#' @param caller_identity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_homology_best = function(input,
                                    species = NA,
                                    species_b = NA,
                                    caller_identity = NA,
                                    verbose = TRUE,
                                    progress_bar = FALSE,
                                    diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = species_b,
                                                    name = "species_b",
                                                    class = "numeric"),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Retrieving similarity scores between ", length(input), " inputs.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(species_b),
                              list("species_b" = paste(unique(species_b),collapse = "%0d"))),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/homology_best",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}


#' Getting functional enrichment
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param background_string_identifiers
#' @param caller_identity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_enrichment = function(input,
                                 species = NA,
                                 background_string_identifiers = NA,
                                 caller_identity = NA,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = background_string_identifiers,
                                                    name = "background_string_identifiers",
                                                    class = "character"),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Retrieving similarity scores between ", length(input), " inputs.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(background_string_identifiers),
                              list("background_string_identifiers" = paste(unique(background_string_identifiers),collapse = "%0d"))),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/enrichment",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}


#' Retrieving functional annotation
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param allow_pubmed
#' @param caller_identity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_functional_annotation = function(input,
                                            species = NA,
                                            allow_pubmed = NA,
                                            caller_identity = NA,
                                            verbose = TRUE,
                                            progress_bar = FALSE,
                                            diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = allow_pubmed,
                                                    name = "allow_pubmed",
                                                    class = "logical"),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Retrieving similarity scores between ", length(input), " inputs.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(allow_pubmed == TRUE,
                              list("allow_pubmed" = 1)),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/functional_annotation",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}


#' Getting protein-protein interaction enrichment
#'
#' #' @family STRING
#'
#' @param input
#' @param species
#' @param required_score
#' @param caller_identity
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_ppi_enrichment = function(input,
                                     species = NA,
                                     required_score = NA,
                                     caller_identity = NA,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = input,
                                                    name = "input",
                                                    class = c("character", "numeric")),
                                               list(arg = species,
                                                    name = "species",
                                                    class = "numeric"),
                                               list(arg = required_score,
                                                    name = "required_score",
                                                    class = "numeric",
                                                    min_val = 0,
                                                    max_val = 1000),
                                               list(arg = caller_identity,
                                                    name = "caller_identity",
                                                    class = "character"),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  ## when querying more that 100 IDs, STRING returns a HTML page asking to specify the specie
  if (length(input) > 100 && is.na(species)) {
    stop("Input's length is ", length(input), ". Please Specify the specie. ",
         "(Homo Sapiens NCBI taxa ID is 9606)\r\n")
  }

  if (verbose == TRUE){
    message("Retrieving similarity scores between ", length(input), " inputs.\r\n")
  }

  ## build POST API request's body
  call_body = list("format" = "text",
                   "identifiers" = paste(unique(input),collapse = "%0d"))

  additional_pars = list(list(!is.na(species),
                              list("species" = species)),
                         list(!is.na(required_score),
                              list("required_score" = required_score)),
                         list(!is.na(caller_identity),
                              list("caller_identity" = caller_identity)))

  call_body = rba_ba_body_add_pars(call_body = call_body,
                                   additional_pars = additional_pars)

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/functional_annotation",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}

#' Getting current STRING version
#'
#' #' @family STRING
#'
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_version = function(verbose = TRUE,
                              progress_bar = FALSE,
                              diagnostics = FALSE){

  if (verbose == TRUE){
    message("Retrieving Current STRING database version.\r\n")
  }

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = progress_bar,
                                                    name = "progress_bar",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))
  ## build POST API request's body
  call_body = list("format" = "text")

  ## make function-specific calls
  call_func_input = quote(httr::POST(url = getOption("rba_url_string"),
                                     path = "api/json/version",
                                     body = call_body,
                                     encode = "form",
                                     httr::user_agent(getOption("rba_ua")),
                                     httr::accept_json()
                                     ))

  call_func_input = rba_ba_call_add_pars(call_func_input = call_func_input,
                                         diagnostics = diagnostics,
                                         progress_bar = progress_bar)

  response_parser_input = quote(data.frame(jsonlite::fromJSON(httr::content(output,
                                                                            as = "text",
                                                                            encoding = "UTF-8"),
                                                              flatten = TRUE),
                                           stringsAsFactors = FALSE))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = response_parser_input,
                                  verbose = verbose,
                                  diagnostics = diagnostics)
  return(final_output)
}


#' Get Statistics from STRING database
#'
#' @param what
#' @param verbose
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_string_info = function(what = "statistics",
                           verbose = TRUE,
                           diagnostics = FALSE){

  ## Check input arguments
  invisible(rba_ba_arguments_check(cons = list(list(arg = what,
                                                    name = "what",
                                                    class = "character",
                                                    val = c("statistics", "organisms")),
                                               list(arg = verbose,
                                                    name = "verbose",
                                                    class = "logical"),
                                               list(arg = diagnostics,
                                                    name = "diagnostics",
                                                    class = "logical")),
                                   diagnostics = diagnostics))

  if (what == "statistics") {
    if (verbose == TRUE){
      message("Retrieving STRNG statistics.\r\n")
    }

    input_url = paste0("https://string-db.org/",
                       "cgi/about.pl?footer_active_subpage=statistics")
    input_html = xml2::read_html(input_url)
    input_html = rvest::html_nodes(input_html, ".footer_stats_table")
    output_table = rvest::html_table(input_html)

  } else if (what == "organisms") {
    if (verbose == TRUE){
      message("Retrieving STRNG Organism tubular view\r\n")
    }

    input_url = paste0("https://string-db.org/",
                       "organism_overview.html")
    input_html = xml2::read_html(input_url)
    output_table = rvest::html_table(input_html)[[1]]
    output_table[[2]] = paste0("https://stringdb-static.org/", output_table[[2]][[2]])
  }

  return(output_table)
}
