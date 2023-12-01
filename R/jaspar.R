#### Collection Endpoints ####

#' List collections available in JASPAR
#'
#' JASPAR organizes matrix profiles into collections.
#'   Using this function, you can retrieve a list of available collections
#'   in a JASPAR release.
#'
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/collections/"
#'
#' @return A data frame with collections' names and URLs.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_collections(release = 2024)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_collections <- function(release = 2024,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024))
  ))

  .msg("Retrieving a list of collections available in JASPAR release %s.",
       release)

  ## Build GET API Request's query
  call_query <- list("release" = release,
                     "page_size" = 1000)

  ## Build Function-Specific Call
  parser_input <- list("json->list_simp",
                       function(x) x[["results"]]
  )

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = paste0(.rba_stg("jaspar", "pth"),
                                        "collections/"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("jaspar_collections.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#' List matrices available in a JASPAR collection
#'
#' Using this function you can list all matrix profiles
#' that are available in a collection from a JASPAR release.
#'
#' The results are paginated. You can control the page's size number
#'   with the function's arguments. Also, you can use \code{\link{rba_pages}}
#'   to automatically iterate over multiple pages.
#'
#' @param collection JASPAR Collection's name. See
#'   \href{https://jaspar.elixir.no/docs/}{JASPAR Collections} for
#'   information. The accepted values are: "CORE", "CNE", "PHYLOFACTS",
#'   "SPLICE", "POLII", "FAM", "PBM", "PBM_HOMEO", "PBM_HLH", and
#'   "UNVALIDATED".
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param only_last_version Logical: (default = FALSE) If TRUE, only the
#'   latest version of a matrix profile will be returned.
#' @param search Character: A search term.
#' @param order Character: A character string or a vector of character strings
#'   of field names that will be used to order the results.
#'   \cr Providing multiple field names is supported. You can alsoa use prefix
#'   "-" before a field name to indicate reverse ordering.
#' @param page_size Numeric: (default = 1000) This resource returns paginated
#'   results. What is the maximum numbers of results that you want to retrieve
#'   per a page? Accepted values are between 1 and 1000.
#' @param page Numeric: Which page of the results to retrieve? The accepted
#'   values depend on the page size and number of results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/collections/{collection}/"
#'
#' @return A list that contains a data frame with information of matrix
#'   profiles available in the collection.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_collections_matrices(collection = "CORE",
#'   release = 2024,
#'   page_size = 100,
#'   page = 2)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_collections_matrices <- function(collection,
                                            release = 2024,
                                            only_last_version = FALSE,
                                            search = NULL,
                                            order = NULL,
                                            page_size = 1000,
                                            page = 1,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "collection",
                             class = "character",
                             val = c("CORE",
                                     "CNE",
                                     "PHYLOFACTS",
                                     "SPLICE",
                                     "POLII",
                                     "FAM",
                                     "PBM",
                                     "PBM_HOMEO",
                                     "PBM_HLH",
                                     "UNVALIDATED")),
                        list(arg = "only_last_version",
                             class = "logical"),
                        list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024)),
                        list(arg = "search",
                             class = "character"),
                        list(arg = "order",
                             class = "character"),
                        list(arg = "page_size",
                             class = "numeric",
                             ran = c(1,1000)),
                        list(arg = "page",
                             class = "numeric",
                             min_val = 1)
  ))

  .msg("Retrieving a list of matrix profiles available in JASPAR %s collection release %s (page %s).",
       collection, release, page)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list("collection" = collection,
                                       "release" = release,
                                       "page_size" = page_size,
                                       "page" = page),
                           list("version",
                                isTRUE(only_last_version),
                                "latest"),
                           list("search",
                                !is.null(search),
                                search),
                           list("order",
                                !is.null(order),
                                paste0(order, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = sprintf("%s/collections/%s/",
                                         .rba_stg("jaspar", "pth"),
                                         collection),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("jaspar_collections_profiles.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### matrix Endpoints ####

#' Search matrix profiles available in JASPAR
#'
#' You can use this function to list the JASPAR matrix
#'   profiles that match your search query, or run the function without any
#'   arguments to return a list of every matrix profile available in the
#'   latest release.
#'
#' Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr The results are paginated. You can control the page's size number
#'   with the function's arguments. Also, you can use \code{\link{rba_pages}}
#'   to automatically iterate over multiple pages.
#'
#' @param term Character: A search term.
#' @param tf_name Character: Transcription factor names (Case-sensitive).
#' @param tf_class Character: Transcription factor class
#' @param tf_family Character: Transcription factor family
#' @param tax_group Character: Taxonomic group. Use
#'   \code{\link{rba_jaspar_taxons}} to get a list of supported Taxonomic
#'   groups.
#' @param tax_id Numeric: NCBI taxonomic Identifier of species. Use
#'   \code{\link{rba_jaspar_species}} to get a list of supported Species.
#' @param data_type Character: Type of the data (i.e The Methodology used
#'   for matrix construction). For example: "ChIP-seq", "PBM"
#' @param collection Character: JASPAR matrix profile collection name. USE
#'   \code{\link{rba_jaspar_collections}} to get a list of collection names.
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param only_last_version Logical: (default = FALSE) If TRUE, only the
#'   latest version of a matrix profile will be returned.
#' @param order Character: A character string or a vector of character strings
#'   of field names that will be used to order the results.
#'   \cr Providing multiple field names is supported. You can also use prefix
#'   "-" before a field name to indicate reverse ordering.
#' @param page_size Numeric: (default = 1000) This resource returns paginated
#'   results. What is the maximum numbers of results that you want to retrieve
#'   per a page? Accepted values are between 1 and 1000.
#' @param page Numeric: Which page of the results to retrieve? The accepted
#'   values depend on the page size and number of results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/api/v1/matrix/"
#'
#' @return A list that contains a data frame of matrix profiles' information.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_matrix_search(term = "FOX")
#' rba_jaspar_matrix_search(tf_name = "FOXP3")
#' rba_jaspar_matrix_search(tf_name = "FOXP3", only_last_version = TRUE)
#' rba_jaspar_matrix_search(tf_class = "Zipper-Type")
#' rba_jaspar_matrix_search(tax_group = "insects")
#' rba_jaspar_matrix_search(page_size = 100)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_matrix_search <- function(term = NULL,
                                     tf_name = NULL,
                                     tf_class = NULL,
                                     tf_family = NULL,
                                     tax_group = NULL,
                                     tax_id = NULL,
                                     data_type = NULL,
                                     collection = NULL,
                                     release = 2024,
                                     only_last_version = FALSE,
                                     order = NULL,
                                     page_size = 1000,
                                     page = 1,
                                     ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "term",
                             class = "character"),
                        list(arg = "tf_name",
                             class = "character"),
                        list(arg = "tf_class",
                             class = "character"),
                        list(arg = "tf_family",
                             class = "character"),
                        list(arg = "tax_group",
                             class = "character"),
                        list(arg = "tax_id",
                             class = "numeric"),
                        list(arg = "data_type",
                             class = "character"),
                        list(arg = "collection",
                             class = "character",
                             val = c("CORE",
                                     "CNE",
                                     "PHYLOFACTS",
                                     "SPLICE",
                                     "POLII",
                                     "FAM",
                                     "PBM",
                                     "PBM_HOMEO",
                                     "PBM_HLH",
                                     "UNVALIDATED")),
                        list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024)),
                        list(arg = "order",
                             class = "character"),
                        list(arg = "only_last_version",
                             class = "logical"),
                        list(arg = "page_size",
                             class = "numeric",
                             ran = c(1,1000)),
                        list(arg = "page",
                             class = "numeric",
                             min_val = 1)
  ))

  .msg("Retrieving a list of matrix profiles available in JASPAR release %s based on your search query.",
       release)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list("release" = release,
                                       "page_size" = page_size,
                                       "page" = page),
                           list("search",
                                !is.null(term),
                                term),
                           list("name",
                                !is.null(tf_name),
                                tf_name),
                           list("tf_class",
                                !is.null(tf_class),
                                tf_class),
                           list("tf_family",
                                !is.null(tf_family),
                                tf_family),
                           list("tax_group",
                                !is.null(tax_group),
                                tax_group),
                           list("tax_id",
                                !is.null(tax_id),
                                paste0(tax_id, collapse = ",")),
                           list("data_type",
                                !is.null(data_type),
                                data_type),
                           list("collection",
                                !is.null(collection),
                                collection),
                           list("search",
                                !is.null(term),
                                term),
                           list("search",
                                !is.null(term),
                                term),
                           list("version",
                                isTRUE(only_last_version),
                                "latest"),
                           list("order",
                                !is.null(order),
                                paste0(order, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = paste0(.rba_stg("jaspar", "pth"), "matrix/"),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("jaspar_matrix_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' List matrix profile versions associated with a base ID
#'
#' Since JASPAR release 2010, the matrix profiles
#'   are versioned; So, a matrix profile Identifier has "base_id.version"
#'   naming schema. Using this function you can retrieve a list of matrix
#'   profiles associated with a base (stable) ID.
#'
#' @param base_id Character: A base (stable) Identifier. A matrix profile
#'  identifier has "base_id.version" naming schema
#' @param order Character: A character string or a vector of character strings
#'   of field names that will be used to order the results.
#'   \cr Providing multiple field names is supported. You can also use prefix
#'   "-" before a field name to indicate reverse ordering.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/matrix/{base_id}/versions/"
#'
#' @return A data frame of matrix profiles' versions information.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_matrix_versions("MA0600")
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_matrix_versions <- function(base_id,
                                       order = NULL,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list( list(arg = "base_id",
                              class = "character"),
                         list(arg = "order",
                              class = "character")),
            cond = list(list(quote(grepl("\\.\\d+" ,base_id)),
                             "base_id cannot be versioned. "))
  )

  .msg("Retrieving a list of matrix profile versions under base ID %s.",
       base_id)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list("base_id" = base_id,
                                       "page_size" = 1000),
                           list("order",
                                !is.null(order),
                                paste0(order, collapse = ",")))

  ## Build Function-Specific Call
  parser_input <- list("json->list_simp",
                       function(x) x[["results"]]
  )

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = sprintf("%smatrix/%s/versions/",
                                         .rba_stg("jaspar", "pth"),
                                         base_id),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("jaspar_matrix_versions.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get a Position Frequency Matrices (PFM) with annotations
#'
#' Using this function you can retrieve a Position Frequency Matrices (PFM)
#'  associated with a matrix profile Identifier along with its details and
#'  annotations. If a base ID (i.e. without version suffix) was supplied,
#'  the latest version will be returned.
#'
#' @param matrix_id Character: A matrix profile
#'   Identifier. It has "base_id.version" naming schema.
#' @param file_format Character: Instead of returning a R object, you
#'   can directly download the profile matrix in file with this format.
#'   Supported formats are: "yaml", "jaspar", "transfac", "meme" and "pfm"
#' @param save_to NULL or Character:\itemize{
#'   \item NULL: (only if file_format was supplied) Save the file to an
#'     automatically-generated path.
#'   \item Character string: A valid file or directory path to save the file to.}
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/matrix/{matrix_id}/"
#'
#' @return A list that contains the PFM along with its details and
#'   annotations. If file_format was supplied, an un-parsed character string
#'   with the file's content.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_matrix("MA0600.2")
#' }
#' \dontrun{
#' rba_jaspar_matrix(matrix_id = "MA0600.2",
#'                   file_format = "meme",
#'                   save_to = "my_matrix.meme")
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_matrix <- function(matrix_id,
                              file_format = NULL,
                              save_to = NULL,
                              ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "matrix_id",
                             class = "character"),
                        list(arg = "file_format",
                             class = "character",
                             val = c("yaml",
                                     "jaspar",
                                     "transfac",
                                     "pfm",
                                     "meme")),
                        list(arg = "save_to",
                             class = "character")
  ))

  .msg("Retrieving details of matrix profile with ID %s.", matrix_id)

  ## Build Function-Specific Call
  if (is.null(file_format)) {
    accept_input <- "application/json"

    parser_input <- list("json->list_simp",
                         function(x) {
                           x$pfm <- as.matrix(t(as.data.frame(x$pfm[c("A", "C", "G", "T")])))
                           return(x)})

    save_to_input <- ifelse(isTRUE(save_to),
                            .rba_file("jaspar_matrix.json",
                                      save_to = save_to),
                            .rba_file("jaspar_matrix.json")
    )

  } else {
    accept_input <- switch(file_format,
                           "yaml" = "application/yaml",
                           "jaspar" = "text/jaspar",
                           "transfac" = "text/transfac",
                           "pfm" = "text/pfm",
                           "meme" = "text/meme")

    parser_input <- "text->chr"

    save_to_input <- .rba_file(file = sprintf("%s.%s",
                                              matrix_id, file_format),
                               save_to = ifelse(is.null(save_to) || is.na(save_to),
                                                yes = TRUE,
                                                no = save_to))
  }


  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = sprintf("%smatrix/%s/",
                                         .rba_stg("jaspar", "pth"),
                                         matrix_id),
                          accept = accept_input,
                          parser = parser_input,
                          save_to = save_to_input)

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Releases Endpoints ####

#' Get information about JASPAR database releases
#'
#' If a release number was supplied, this function will return the details
#'   of that release. Otherwise, if the function was called without "release"
#'   argument, a list of all JASPAR database releases will be returned.
#'
#' @param release_number Numeric: Which JASPAR database release number
#'   information's to retrieve? If left NULL (the default), a list of all
#'   JASPAR database releases will be returned. Available options are 1 to 8.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#' "GET "https://jaspar.elixir.no/api/v1/releases/"
#'  "GET "https://jaspar.elixir.no/api/v1/releases/{release_number}/"
#'
#' @return A list that contains all JASPAR database releases' information or
#'   details of a particular release.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_releases()
#' rba_jaspar_releases(7)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_releases  <- function(release_number = NULL,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "release_number",
                             class = "numeric",
                             ran = c(1,8))
  ))

  .msg(ifelse(is.null(release_number),
              yes = "Retrieving a list of all releases of JASPAR database.",
              no = sprintf("Retrieving a details of JASPAR database release number %s.",
                           release_number))
  )

  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("release_number",
                                !is.null(release_number),
                                release_number),
                           list("page_size",
                                is.null(release_number),
                                1000)
  )

  ## Build Function-Specific Call
  if (is.null(release_number)) {
    path_input <- paste0(.rba_stg("jaspar", "pth"), "releases/")
    parser_input <- list("json->list_simp",
                         function(x) x[["results"]])
  } else {
    path_input <- sprintf("%sreleases/%s",
                          .rba_stg("jaspar", "pth"), release_number)

    parser_input <- "json->list_simp"
  }
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = path_input,
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("jaspar_matrix.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Sites Endpoints ####

#' Get binding sites of a matrix profile
#'
#' Use this function to retrieve a list of transcription factor binding sites
#'   associated with a matrix profile.
#'
#' @param matrix_id Character: A matrix profile
#'   Identifier. It has "base_id.version" naming schema.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/sites/{matrix_id}/"
#'
#' @return A list that contains a data frame with binding sites information.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_sites("MA0600.1")
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_sites <- function(matrix_id,
                             ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list( list(arg = "matrix_id",
                              class = "character"))
  )

  .msg("Retrieving binding sites information of matrix profile with ID %s.",
       matrix_id)

  ## Build GET API Request's query
  call_query <- list("matrix_id" = matrix_id)

  ## Build Function-Specific Call

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = sprintf("%ssites/%s/",
                                         .rba_stg("jaspar", "pth"),
                                         matrix_id),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("jaspar_sites.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Species Endpoints ####

#' List available species in JASPAR
#'
#' JASPAR organizes matrix profiles from multiple species
#'   in six taxonomic groups. Use this function to retrieve a list of
#'   available species in a JASPAR database release.
#'
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param search Character: A search term.
#' @param order Character: A character string or a vector of character strings
#'   of field names that will be used to order the results.
#'   \cr Providing multiple field names is supported. You can also use prefix
#'   "-" before a field name to indicate reverse ordering.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/species/"
#'
#' @return A data frame with information of available species.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_species(release = 2024)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_species <- function(release = 2024,
                               search = NULL,
                               order = NULL,
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024)),
                        list(arg = "search",
                             class = "character"),
                        list(arg = "order",
                             class = "character")
  ))

  .msg("Retrieving a list of species available in JASPAR release %s.",
       release)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list("release" = release,
                                       "page" = 1,
                                       "page_size" = 1000),
                           list("search",
                                !is.null(search),
                                search),
                           list("order",
                                !is.null(order),
                                paste0(order, collapse = ","))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list_simp",
                       function(x) x[["results"]]
  )

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = paste0(.rba_stg("jaspar", "pth"),
                                        "species/"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("jaspar_species.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#' List matrices available in JASPAR of a species
#'
#' JASPAR curates matrix profiles from multiple species
#'   in six taxonomic groups. Using this function you can list all
#'   matrix profiles that are available in a JASPAR  release from a species.
#'
#' The results are paginated. You can control the page's size number
#'   with the function's arguments. Also, you can use \code{\link{rba_pages}}
#'   to automatically iterate over multiple pages.
#'
#' @param tax_id Numeric: NCBI taxonomic Identifier of species. Use
#'   \code{\link{rba_jaspar_species}} to get a list of supported Species.
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param only_last_version Logical: (default = FALSE) If TRUE, only the
#'   latest version of a matrix profile will be returned.
#' @param search Character: A search term.
#' @param order Character: A character string or a vector of character strings
#'   of field names that will be used to order the results.
#'   \cr Providing multiple field names is supported. You can also use prefix
#'   "-" before a field name to indicate reverse ordering.
#' @param page_size Numeric: (default = 1000) This resource returns paginated
#'   results. What is the maximum numbers of results that you want to retrieve
#'   per a page? Accepted values are between 1 and 1000.
#' @param page Numeric: Which page of the results to retrieve? The accepted
#'   values depend on the page size and number of results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/species/{tax_id}/"
#'
#' @return A list that contains a data frame with information of matrix
#'   profiles available for the species.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_species_matrices(tax_id = 9606, page_size = 100)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_species_matrices <- function(tax_id,
                                        release = 2024,
                                        only_last_version = FALSE,
                                        search = NULL,
                                        order = NULL,
                                        page_size = 1000,
                                        page = 1,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "tax_id",
                             class = "numeric"),
                        list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024)),
                        list(arg = "only_last_version",
                             class = "logical"),
                        list(arg = "search",
                             class = "character"),
                        list(arg = "order",
                             class = "character"),
                        list(arg = "page_size",
                             class = "numeric",
                             ran = c(1,1000)),
                        list(arg = "page",
                             class = "numeric",
                             min_val = 1)
  ))

  .msg("Retrieving a list of matrix profiles of species %s available in JASPAR release %s (page %s).",
       tax_id, release, page)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list("tax_id" = tax_id,
                                       "release" = release,
                                       "page" = page,
                                       "page_size" = page_size),
                           list("version",
                                isTRUE(only_last_version),
                                "latest"),
                           list("search",
                                !is.null(search),
                                search),
                           list("order",
                                !is.null(order),
                                paste0(order, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = sprintf("%sspecies/%s/",
                                         .rba_stg("jaspar", "pth"),
                                         tax_id),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("jaspar_species_matrices.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Taxon Endpoints ####

#' List available taxonomic groups in JASPAR
#'
#' JASPAR organizes matrix profiles from multiple species
#'   in six taxonomic groups. Use this function to retrieve a list of
#'   available taxonomic groups in a JASPAR database release.
#'
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/taxon/"
#'
#' @return A data frame with information of available species.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_taxons(release = 2024)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_taxons <- function(release = 2024,
                              ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024))
  ))

  .msg("Retrieving a list of taxonomic groups available in JASPAR release %s.",
       release)

  ## Build GET API Request's query
  call_query <- list("release" = release,
                     "page_size" = 1000)

  ## Build Function-Specific Call
  parser_input <- list("json->list_simp",
                       function(x) x[["results"]]
  )

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = paste0(.rba_stg("jaspar", "pth"),
                                        "taxon/"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("jaspar_taxons.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' List matrices available in JASPAR of a taxonomic group
#'
#' JASPAR organizes matrix profiles from multiple species
#'   in six taxonomic groups. Using this function you can list all
#'   matrix profiles that are available in a JASPAR release from a
#'   taxonomic group.
#'
#' The results are paginated. You can control the page's size number
#'   with the function's arguments. Also, you can use \code{\link{rba_pages}}
#'   to automatically iterate over multiple pages.
#'
#' @param tax_group Character: Taxonomic group. Use
#'   \code{\link{rba_jaspar_taxons}} to get a list of supported Taxonomic
#'   groups.
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param only_last_version Logical: (default = FALSE) If TRUE, only the
#'   latest version of a matrix profile will be returned.
#' @param search Character: A search term.
#' @param order Character: A character string or a vector of character strings
#'   of field names that will be used to order the results.
#'   \cr Providing multiple field names is supported. You can also use prefix
#'   "-" before a field name to indicate reverse ordering.
#' @param page_size Numeric: (default = 1000) This resource returns paginated
#'   results. What is the maximum numbers of results that you want to retrieve
#'   per a page? Accepted values are between 1 and 1000.
#' @param page Numeric: Which page of the results to retrieve? The accepted
#'   values depend on the page size and number of results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/taxon/{tax_group}/"
#'
#' @return A list that contains a data frame with information of matrix
#'   profiles available for the taxonomic group.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_taxons_matrices(tax_group = "plants", page_size = 100)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_taxons_matrices <- function(tax_group,
                                       release = 2024,
                                       only_last_version = FALSE,
                                       search = NULL,
                                       order = NULL,
                                       page_size = 1000,
                                       page = 1,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "tax_group",
                             class = "character",
                             val = c("plants",
                                     "vertebrates",
                                     "insects",
                                     "urochordates",
                                     "nematodes",
                                     "fungi",
                                     "trematodes",
                                     "protozoa",
                                     "cnidaria")),
                        list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024)),
                        list(arg = "only_last_version",
                             class = "logical"),
                        list(arg = "search",
                             class = "character"),
                        list(arg = "order",
                             class = "character"),
                        list(arg = "page_size",
                             class = "numeric",
                             ran = c(1,1000)),
                        list(arg = "page",
                             class = "numeric",
                             min_val = 1)
  ))

  .msg("Retrieving a list of matrix profiles of taxonomic group %s available in JASPAR release %s (page %s).",
       tax_group, release, page)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list("tax_group" = tax_group,
                                       "release" = release,
                                       "page" = page,
                                       "page_size" = page_size),
                           list("version",
                                isTRUE(only_last_version),
                                "latest"),
                           list("search",
                                !is.null(search),
                                search),
                           list("order",
                                !is.null(order),
                                paste0(order, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = sprintf("%staxon/%s/",
                                         .rba_stg("jaspar", "pth"),
                                         tax_group),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("jaspar_taxon_matrices.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### TFFM Endpoints ####

#' Search TF flexible models (TFFMs) available in JASPAR
#'
#' You can use this function to list the JASPAR TF flexible models (TFFMs)
#'   that match your search query, or run the function without any
#'   arguments to return a list of every matrix profile available in the
#'   latest release.
#'
#' Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr The results are paginated. You can control the page's size number
#'   with the function's arguments. Also, you can use \code{\link{rba_pages}}
#'   to automatically iterate over multiple pages.
#'
#' @param term Character: A search term.
#' @param release Numeric: (default = 2024) Which JASPAR database release
#'   to use? Available options are: 2024, 2022, 2020, 2018, 2016, and 2014.
#' @param tax_group Character: Taxonomic group. Use
#'   \code{\link{rba_jaspar_taxons}} to get a list of supported Taxonomic
#'   groups.
#' @param search Character: A search term.
#' @param order Character: A character string or a vector of character strings
#'   of field names that will be used to order the results.
#'   \cr Providing multiple field names is supported. You can also use prefix
#'   "-" before a field name to indicate reverse ordering.
#' @param page_size Numeric: (default = 1000) This resource returns paginated
#'   results. What is the maximum numbers of results that you want to retrieve
#'   per a page? Accepted values are between 1 and 1000.
#' @param page Numeric: Which page of the results to retrieve? The accepted
#'   values depend on the page size and number of results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/api/v1/tffm/"
#'
#' @return A list that contains a data frame with information of query hits'
#'   TFFMs.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_tffm_search(term = "FOX")
#' rba_jaspar_tffm_search(tax_group = "insects")
#' rba_jaspar_tffm_search(page_size = 100)
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_tffm_search <- function(term = NULL,
                                   release = 2024,
                                   tax_group = NULL,
                                   search = NULL,
                                   order = NULL,
                                   page_size = 1000,
                                   page = 1,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "term",
                             class = "character"),
                        list(arg = "release",
                             class = "numeric",
                             no_null = TRUE,
                             val = c(2014, 2016, 2018, 2020, 2022, 2024)),
                        list(arg = "tax_group",
                             class = "character",
                             val = c("plants",
                                     "vertebrates",
                                     "insects",
                                     "urochordates",
                                     "nematodes",
                                     "fungi",
                                     "trematodes",
                                     "protozoa",
                                     "cnidaria")),
                        list(arg = "search",
                             class = "character"),
                        list(arg = "order",
                             class = "character"),
                        list(arg = "page_size",
                             class = "numeric",
                             ran = c(1,1000)),
                        list(arg = "page",
                             class = "numeric",
                             min_val = 1)
  ))

  .msg("Retrieving a list of TFFM profiles available in JASPAR release %s based on your search query.",
       release)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list("release" = release,
                                       "page" = page,
                                       "page_size" = page_size),
                           list("search",
                                !is.null(term),
                                term),
                           list("tax_group",
                                !is.null(tax_group),
                                tax_group),
                           list("order",
                                !is.null(order),
                                paste0(order, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = paste0(.rba_stg("jaspar", "pth"),
                                        "tffm/"),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("jaspar_tffm_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get a TF flexible models (TFFMs) information
#'
#' Using this function you can retrieve details and annotations of
#'   Transcription Factor flexible models (TFFMs) associated with a TFFM
#'   ID. If a base ID (i.e. without version suffix) was supplied, the latest
#'   version will be returned.
#'
#' @param tffm_id Character: A TF flexible model (TFFM) Identifier.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET "https://jaspar.elixir.no/api/v1/fttm/{tffm_id}/"
#'
#' @return A list that contains the TFFM's information and annotations.
#'
#' @references \itemize{
#'   \item Rauluseviciute I, Riudavets-Puig R, Blanc-Mathieu R,
#'   Castro-Mondragon JA, Ferenc K, Kumar V, Lemma RB, Lucas J, Chèneby J,
#'   Baranasic D, Khan A, Fornes O, Gundersen S, Johansen M, Hovig E, Lenhard
#'   B, Sandelin A, Wasserman WW, Parcy F, Mathelier A JASPAR 2024:
#'   20th anniversary of the open-access database of transcription factor
#'   binding profiles Nucleic Acids Res. in_press; doi: 10.1093/nar/gkad1059
#'   \item Khan, A. and Mathelier, A. JASPAR RESTful API: accessing JASPAR data
#'   from any programming language. Bioinformatics, 2017,
#'   doi: 10.1093/bioinformatics/btx804
#'   \item
#'   \href{https://jaspar.elixir.no/api/v1/docs/}{JASPAR API Documentation}
#'   \item \href{https://jaspar.elixir.no/faq/}{Citations note
#'   on JASPAR website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_jaspar_tffm("TFFM0056.3")
#' }
#'
#' @family "JASPAR"
#' @export
rba_jaspar_tffm <- function(tffm_id,
                            ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list( list(arg = "tffm_id",
                              class = "character"))
  )

  .msg("Retrieving details of TFFM profile with ID %s.", tffm_id)

  ## Build GET API Request's query
  call_query <- list("tffm_id" = tffm_id)

  ## Build Function-Specific Call

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("jaspar", "url"),
                          path = sprintf("%stffm/%s/",
                                         .rba_stg("jaspar", "pth"),
                                         tffm_id),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("jaspar_tffm.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
