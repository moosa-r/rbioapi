#### Internal functions ####

#' Internal Function to Handle Different inputs of Reactome Analysis
#'
#' This function will be called within any Reactome Analysis service which
#'   requires input from the users.
#'
#' @param input Character or Numeric vector, Data frame, or Matrix: Pass on
#'   caller function's input argument to this.
#' @param type Character: (optional) Pass on caller function's input_format
#'   argument to this.
#' @param prepare_upload Logical: (default = \code{TRUE}) If TRUE, the input
#'   will be written to a temporary file when necessary to facilitate uploading
#'   to Reactome. If FALSE, the input will only be identified.
#'
#' @return If \code{prepare_upload} is FALSE, one of "table", "vector", "file", or
#'   "url". Otherwise, a list containing the handled type and the temporary or
#'   user-supplied file location.
#'
#' @noRd
.rba_reactome_input <- function(input,
                                type = NULL,
                                prepare_upload = TRUE){

  diagnostics <- get0(
    "diagnostics",
    envir = parent.frame(1),
    ifnotfound = getOption("rba_diagnostics")
  )

  ### 1 Identify Input
  input_validity <- c(
    "url" = is.character(input) &&
      length(input) == 1L &&
      isTRUE(grepl("^https?://", input, ignore.case = TRUE)),
    "file" = is.character(input) &&
      length(input) == 1L &&
      file.exists(input) &&
      !dir.exists(input),
    "table" = is.data.frame(input) || is.matrix(input),
    "vector" = is.atomic(input) &&
      is.null(dim(input)) &&
      (is.character(input) || is.numeric(input)) &&
      length(input) > 0L
  )

  if (is.null(type)) {
    type_index <- match(TRUE, input_validity)

    if (is.na(type_index)) {
      stop(
        "Could not identify your input format. Please specify it using 'input_format' argument.",
        call. = diagnostics
      )
    }

    type <- names(input_validity)[[type_index]]
  }

  ### 2 Validate Identified Input
  if (!isTRUE(input_validity[type])) {
    input_requirement <- switch(
      type,
      "file" = "a single path to an existing local file",
      "url" = "a single HTTP or HTTPS URL",
      "table" = "a data frame or matrix",
      "vector" = "a non-empty character or numeric vector",
      "compatible with a supported input format"
    )

    stop(
      sprintf(
        "`input` must be %s when `input_format = \"%s\"`.",
        input_requirement,
        type
      ),
      call. = diagnostics
    )
  }

  ### 3 Handle Input
  if (isFALSE(prepare_upload)) {

    return(type)

  }

  output <- list(type = type, file = input)

  if (type %in% c("table", "vector")) {
    output$type <- "file"
    output$file <- tempfile(pattern = "rba", fileext = ".txt")

    if (type == "table") {
      input <- as.data.frame(input, stringsAsFactors = FALSE)

      # Make sure that the first column name starts with #.
      if (!startsWith(colnames(input)[[1L]], "#")) {
        colnames(input)[[1L]] <- paste0("#", colnames(input)[[1L]])
      }
      utils::write.table(
        x = input,
        file = output$file,
        sep = "\t",
        quote = FALSE,
        row.names = FALSE,
        col.names = TRUE
      )
    } else {
      writeLines(
        text = c("#Gene names", input),
        con = output$file,
        sep = "\n"
      )
    }
  }

  return(output)
}

#' Keep Reactome Analysis Results Consistent
#'
#' Reactome's analysis, token, and species-comparison resources return the same
#'   pathway result structure. This function applies the same table handling to
#'   each response without defining, renaming, reordering, or removing Reactome
#'   fields.
#'
#' @param result A parsed Reactome Analysis response.
#'
#' @return The result with information within tables expanded into columns and
#'   an empty pathways result represented by an empty data frame. Other
#'   structures are returned unchanged.
#'
#' @noRd
.rba_reactome_analysis_result <- function(result) {
  # Preserve responses outside the expected result structure.
  if (!is.list(result)) {
    return(result)
  }

  output <- lapply(
    X = result,
    FUN = function(field) {
      # Expand information stored within a table.
      if (is.data.frame(field)) {
        field <- jsonlite::flatten(field)
      }

      return(field)
    }
  )

  # Standardize an empty pathways result.
  if (
    utils::hasName(output, "pathways") &&
    is.list(output$pathways) &&
    length(output$pathways) == 0L
  ) {
    output$pathways <- data.frame()
  }

  return(output)
}
#### Identifiers Endpoints ####

#' Reactome Over-Representation or Expression Analysis
#'
#' Using this function, you can perform Reactome Analysis In a convenient way.
#' The Analysis Type will be chosen depending on your supplied
#'   input:\enumerate{
#'   \item If you supply a vector or a single-columned table,
#'   "Over-Representation" analysis will be performed.
#'   \item If you supply a multi-column table, with the first column being
#'   molecules identifiers and the rest being numeral expression values,
#'   "Expression" analysis will be performed.}
#' See the details section for the accepted input types and format.
#'
#' You can supply your table or vector input in numerous formats:\enumerate{
#'   \item An R object which can be a data frame, matrix, or simple vector.
#'   \item A path to a local text file in your device that contains the molecules
#'   data. (The file should be formatted correctly, see below.)
#'   \item An HTTP or HTTPS URL pointing to a text file on the web that contains
#'   the molecules data. (The file should be formatted correctly, see below.)}
#' If you supply a text file (as a local file path or URL), it should be in TSV
#' (Tab-Separated Values) format; the first column name should start with "#".
#' Note that if you are providing the file for "Over-Representation" analysis
#' (i.e. Single columned-data) this header line is optional and will be used as
#' your 'Sample Name', otherwise it is required. \cr Also, form the "summary"
#' element in the function's output, you can see how Reactome Interpreted your
#' input and subsequently the type of analysis that has been performed. \cr
#' There is no strict criteria about the type of your molecules Identifiers,
#' Reactome will Map the IDs to it's internal database entities. Nevertheless,
#' You can check if all your identifiers has been found in "identifiersNotFound"
#' element in the function's output. \cr After any analysis, Reactome will
#' associate a token with your analysis. It can later be used in functions that
#' require the token (e.g. to retrieve the analysis results, download pdf). \cr
#' Note that Reactome will store your token for only 7 days. You can download
#' your full results with \code{\link{rba_reactome_analysis_download}}, and
#' re-import it anytime to reactome (using
#' \code{\link{rba_reactome_analysis_import}}) to generate a new token.
#'
#' @section Corresponding API Resources: "POST
#'   https://reactome.org/AnalysisService/identifiers/form" \cr "POST
#'   https://reactome.org/AnalysisService/identifiers/form/projection" \cr "POST
#'   https://reactome.org/AnalysisService/identifiers/url" \cr "POST
#'   https://reactome.org/AnalysisService/identifiers/url/projection"
#'
#' @param input Character or Numeric vector, Data frame, or Matrix: A vector,
#'   data frame, matrix or a local file path or URL that points to your data.
#'   See "Details section" for more information of how to organize and supply
#'   your input.
#' @param input_format Character: (optional) This function will automatically
#'   identify your supplied input's format. But in case of unexpected issues or
#'   if you want to be explicit, set this argument to one of:\itemize{
#'   \item "table": If you supplied a data frame or matrix as input.
#'   \item "vector": If you supplied a simple vector (numeric or character) as
#'   input.
#'   \item "file": If you supplied a local file path pointing to a
#'   correctly-formatted text file.
#'   \item "url": If you supplied an HTTP or HTTPS URL pointing to a
#'   correctly-formatted text file.}
#'   An explicit value takes precedence. Otherwise, HTTP and HTTPS addresses
#'   are identified first, followed by existing local files, tables, and then
#'   other non-empty character or numeric inputs as identifier vectors.
#' @param species Character or Numeric: (optional) NCBI Taxonomy identifier
#'   (Human is 9606), species name (e.g. "Homo sapiens") or Reactome DbId (e.g
#'   Homo sapiens is 48887). See \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}. Note that you cannot supply the species
#'   parameter when projection parameter is TRUE.
#' @param projection Logical: (default = \code{TRUE}) Should non-human
#'   identifiers be projected to their human equivalents? (using Reactome
#'   orthology data)
#' @param interactors Logical: (default = \code{FALSE}) Should IntAct
#'   interaction data be used to increase the analysis background?
#' @param sort_by Character: (default = \code{"ENTITIES_PVALUE"}) Sort the
#'   result based on what column? Available choices are: "NAME",
#'   "TOTAL_ENTITIES", "TOTAL_INTERACTORS", "TOTAL_REACTIONS", "FOUND_ENTITIES",
#'   "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
#'   "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO"
#' @param order Character: (default = \code{"ASC"}) Sort Order. Can be either
#'   "ASC" or "DESC".
#' @param resource Character: (default = \code{"TOTAL"}) Filter results based on
#'   the resource. Available choices are: "TOTAL", "UNIPROT", "ENSEMBL",
#'   "CHEBI", "IUPHAR", "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND" or
#'   "PUBCHEM_COMPOUND".
#' @param p_value Numeric: (default = \code{1}) Set a P value threshold. Only
#'   results with P value equal to or less than your supplied threshold will be
#'   returned (1 means no P value filtering).
#' @param include_disease Logical: (default = \code{TRUE}) Should the disease
#'   pathways be included in the results?
#' @param min Numeric: (optional) Minimum number of entities that a pathways
#'   should have to be included in the results.
#' @param max Numeric: (optional) Maximum number of entities that a pathways
#'   should have to be included in the results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#'
#' @return A list containing the results and information about the analysis.
#'   The \code{pathways} element is a data frame with information about each
#'   pathway expanded into columns; it is an empty data frame when no pathways
#'   match. The token in \code{results$summary$token} can be used to retrieve
#'   the results later or in other Reactome analysis functions.
#'
#' @references \itemize{ \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla,
#'   C., Beavers, D., Grentner, A., ... D’Eustachio, P. (2026). The Reactome
#'   Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
#'   10.1093/nar/gkaf1223
#'   \item Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A.,
#'   & Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
#'   Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
#'   doi: 10.1074/mcp.TIR120.002155
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis(input = c("p53", "BRCA1", "cdk2", "Q99835", "CDC42"))
#' }
#' \dontrun{
#' rba_reactome_analysis(input = "c:/rbioapi/genes.txt")
#' }
#' \dontrun{
#' rba_reactome_analysis(input = "https://example.com/genes.txt")
#' }
#'
#' @family "Reactome Analysis Service"
#' @family "Enrichment/Over-representation"
#' @export
rba_reactome_analysis <- function(input,
                                  input_format = NULL,
                                  projection = TRUE,
                                  interactors = FALSE,
                                  species = NULL,
                                  sort_by = "ENTITIES_PVALUE",
                                  order = "ASC",
                                  resource = "TOTAL",
                                  p_value = 1,
                                  include_disease = TRUE,
                                  min = NULL,
                                  max = NULL,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "input",
        class = c("character", "numeric", "integer", "data.frame", "matrix"),
        min_len = 1L
      ),
      list(
        arg = "input_format", class = "character",
        len = 1L,
        val = c("table",
                "vector",
                "file",
                "url")
      ),
      list(arg = "projection", class = "logical", len = 1L),
      list(arg = "interactors", class = "logical", len = 1L),
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L),
      list(
        arg = "sort_by", class = "character", len = 1L,
        val = c("NAME",
                "TOTAL_ENTITIES",
                "TOTAL_INTERACTORS",
                "TOTAL_REACTIONS",
                "FOUND_ENTITIES",
                "FOUND_INTERACTORS",
                "FOUND_REACTIONS",
                "ENTITIES_RATIO",
                "ENTITIES_PVALUE",
                "ENTITIES_FDR",
                "REACTIONS_RATIO")
      ),
      list(arg = "order", class = "character", len = 1L, val = c("ASC", "DESC")),
      list(
        arg = "resource", class = "character", len = 1L,
        val = c("TOTAL",
                "UNIPROT",
                "ENSEMBL",
                "CHEBI",
                "IUPHAR",
                "MIRBASE",
                "NCBI_PROTEIN",
                "EMBL",
                "COMPOUND",
                "PUBCHEM_COMPOUND")
      ),
      list(arg = "p_value", class = c("numeric", "integer"), len = 1L, ran = c(0, 1)),
      list(arg = "include_disease", class = "logical", len = 1L),
      list(arg = "min", class = c("numeric", "integer"), len = 1L, min_val = 0),
      list(arg = "max", class = c("numeric", "integer"), len = 1L, min_val = 0)
    ),
    cond = list(
      list(
        quote(isTRUE(projection) && !is.null(species)),
        "You cannot supply 'species' when 'projection' argument is TRUE"
      ),
      list(
        quote(!is.null(min) && (!is.finite(min) || min != floor(min))),
        "`min` should be a finite, non-negative integer."
      ),
      list(
        quote(!is.null(max) && (!is.finite(max) || max != floor(max))),
        "`max` should be a finite, non-negative integer."
      ),
      list(
        quote(!is.null(min) && !is.null(max) && min > max),
        "`min` cannot be greater than `max`."
      )
    )
  )

  .msg(
    "Retrieving Reactome Analysis Results of your supplied Identifiers."
  )

  ## Build POST API Request's query
  call_query <- list(
    "interactors" = ifelse(interactors, "true", "false"),
    "sortBy" = sort_by,
    "order" = order,
    "resource" = resource,
    "includeDisease" = ifelse(include_disease, "true", "false")
  )

  call_query <- .rba_query(
    init = call_query, list("species", !is.null(species), species),
    list("pValue", !is.null(p_value), p_value),
    list("min", !is.null(min), min),
    list("max", !is.null(max),max)
  )

  ## Build POST API Request's URL
  # Handle supplied input
  input <- .rba_reactome_input(
    input = input,
    type = input_format
  )

  if (input$type == "file") {
    call_body <- list(
      file = httr::upload_file(path = input$file)
    )
    submission_type <- "form"
    content_type_input <- NULL
  } else {
    call_body <- input$file
    submission_type <- "url"
    content_type_input <- httr::content_type("text/plain")
  }

  ## Build Function-Specific Call
  path_input <- paste0(
    .rba_stg("reactome", "pth", "analysis"),
    "identifiers/",
    submission_type
  )

  if (isTRUE(projection)) {
    path_input <- paste0(path_input, "/projection")
  }

  parser_input <- list(
    "json->list_simp",
    .rba_reactome_analysis_result
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    body = call_body,
    query = call_query,
    content_type_input,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("reactome_analysis.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Report Endpoints ####

#' Generate PDF file with Reactome Analysis Results
#'
#' Use this function to save a detailed report of your previous analysis (That
#' you have done with \code{\link{rba_reactome_analysis}}). You need to supply a
#' 'token' associated to your previous analysis.
#'
#' Token is associated to each Reactome analysis results and kept by Reactome
#' for at least 7 days. You can locate it in
#' \code{\link{rba_reactome_analysis}}'s output, under a sub-list named
#' "summary" (i.e. results$summary$token). \cr Note that Reactome will store
#' your token for only 7 days. You can download your full results with
#' \code{\link{rba_reactome_analysis_download}}, and re-import it anytime to
#' reactome (using \code{\link{rba_reactome_analysis_import}}) to generate a new
#' token. Use \code{\link{rba_reactome_analysis_download}} to save your results
#' in other formats.
#'
#' @section Corresponding API Resources: "GET
#'   https://reactome.org/AnalysisService/report/\{token\}/\{species\}/
#'   \{filename\}.pdf"
#'
#' @param token Character: A token associated to your previous Reactome
#'   analysis.
#' @param species Character or Numeric: NCBI Taxonomy identifier (Human Taxonomy
#'   ID is 9606.) or species name (e.g. "Homo sapiens"). See
#'   \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param save_to NULL or Character: (optional) \itemize{ \item NULL: Save the
#'   file to an automatically-generated path. \item Character string: A valid
#'   file path to save the file to.}
#' @param number Numeric: (default = \code{25}) Positive integer giving the
#'   maximum number of pathways to include in the report.
#' @param resource Character: (default = \code{"TOTAL"}) Filter results based on
#'   the resource. Available choices are: "TOTAL", "UNIPROT", "ENSEMBL",
#'   "CHEBI", "IUPHAR", "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND" or
#'   "PUBCHEM_COMPOUND".
#' @param diagram_profile Character: (default = \code{"Modern"}) Color profile
#'   of diagrams, should be either "Modern" or "Standard".
#' @param analysis_profile Character: (default = \code{"Standard"}) Color
#'   profile of analysis, should be one of: "Standard", "Strosobar" or "Copper
#'   Plus".
#' @param fireworks_profile Character: (default = \code{"Barium Lithium"}) Color
#'   profile of the overview diagram, should be one of: "Cooper", "Cooper Plus",
#'   "Barium Lithium" or "Calcium Salts".
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#'
#' @return NULL, a PDF file will be saved to disk.
#'
#' @references \itemize{ \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla,
#'   C., Beavers, D., Grentner, A., ... D’Eustachio, P. (2026). The Reactome
#'   Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
#'   10.1093/nar/gkaf1223
#'   \item Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A.,
#'   & Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
#'   Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
#'   doi: 10.1074/mcp.TIR120.002155
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis_pdf(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM%3D",
#'     species = 9606, save_to = "my_analysis.pdf")
#' }
#'
#' @family "Reactome Analysis Service"
#' @seealso \code{\link{rba_reactome_analysis_download}}
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_analysis_pdf <- function(token,
                                      species,
                                      save_to = NULL,
                                      number  = 25,
                                      resource = "TOTAL",
                                      diagram_profile = "Modern",
                                      analysis_profile = "Standard",
                                      fireworks_profile = "Barium Lithium",
                                      ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "token", class = "character", len = 1L),
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L),
      list(arg = "save_to", class = "character", len = 1L, no_na = FALSE),
      list(arg = "number", class = c("numeric", "integer"), len = 1L, min_val = 1),
      list(
        arg = "resource", class = "character", len = 1L,
        val = c("TOTAL",
                "UNIPROT",
                "ENSEMBL",
                "CHEBI",
                "IUPHAR",
                "MIRBASE",
                "NCBI_PROTEIN",
                "EMBL",
                "COMPOUND",
                "PUBCHEM_COMPOUND")
      ),
      list(
        arg = "diagram_profile", class = "character", len = 1L,
        val = c("Modern", "Standard")
      ),
      list(
        arg = "analysis_profile", class = "character", len = 1L,
        val = c("Standard", "Strosobar", "Copper Plus")
      ),
      list(
        arg = "fireworks_profile", class = "character", len = 1L,
        val = c("Cooper", "Cooper Plus", "Barium Lithium", "Calcium Salts")
      )
    ),
    cond = list(
      list(
        quote(!is.finite(number) || number != floor(number)),
        "`number` should be a finite, positive integer."
      )
    )
  )

  .msg(
    "Downloading a pdf report of Reactome analysis result with token %s.",
    token
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("number", number != 25, number),
    list("resource", resource != "TOTAL", resource),
    list("diagramProfile", diagram_profile != "Modern", diagram_profile),
    list("analysisProfile", analysis_profile != "Standard", analysis_profile),
    list("fireworksProfile", fireworks_profile != "Barium Lithium", fireworks_profile)
  )

  # create file_path
  save_to <- .rba_file(
    file = paste0(token, ".pdf"),
    save_to = ifelse(
      is.null(save_to) || is.na(save_to),
      yes = TRUE,
      no = save_to
    )
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = sprintf(
      "%sreport/%s/%s/%s.pdf",
      .rba_stg("reactome", "pth", "analysis"), token, species, token
    ),
    query = call_query,
    accept = "application/pdf",
    parser = NULL,
    save_to = save_to
  )

  ## Call API
  invisible(.rba_skeleton(input_call))
}

#### Download Endpoints ####

#' Download Different Reactome Analysis Results
#'
#' Based on the "request" argument, you can download different analysis results
#' data associated with a given token.
#'
#' Token is associated to each Reactome analysis results and kept by Reactome
#' for at least 7 days. You can locate it in
#' \code{\link{rba_reactome_analysis}}'s output, under a sub-list named
#' "summary" (i.e. results$summary$token). \cr Use
#' \code{\link{rba_reactome_analysis_pdf}} to save a full report in PDF format.
#'
#' @section Corresponding API Resources: GET
#'   https://reactome.org/AnalysisService/download/\{token\}/entities/
#'   found/\{resource\}/\{filename\}.csv" \cr GET
#'   https://reactome.org/AnalysisService/download/\{token\}/entities/
#'   notfound/\{filename\}.csv" \cr GET
#'   https://reactome.org/AnalysisService/download/\{token\}/pathways/
#'   \{resource\}/\{filename\}.csv" \cr GET
#'   https://reactome.org/AnalysisService/download/\{token\}/result.json" \cr
#'   GET https://reactome.org/AnalysisService/download/\{token\}/result.json.gz"
#'
#' @param token Character: A token associated to your previous Reactome
#'   analysis.
#' @param request Character: What to download? Should be one of:\itemize{ \item
#'   "found_ids": Download a CSV file containing the found user-supplied
#'   identifiers in the analysis associated with your supplied token and
#'   resource. \item "not_found_ids"" Download a CSV file containing the
#'   user-supplied Identifiers which has not been found in the analysis
#'   associated with your supplied token. \item "pathways": Download a CSV file
#'   containing Pathway analysis results of the analysis associated with your
#'   supplied token and resource. \item "results": Download a JSON file
#'   containing the complete analysis results associated with your supplied
#'   token. \item "results_gz" Same as "results", but the output will be
#'   compress (gzipped).}
#' @param save_to NULL or Character: (optional) \itemize{ \item NULL: Save the
#'   file to an automatically-generated path. \item Character string: A valid
#'   file path to save the file to.}
#' @param resource Character: (default = \code{"TOTAL"}) (Only when request is
#'   "found_ids" or "pathways") Filter results based on the resource. Available
#'   choices are:"TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR", "MIRBASE",
#'   "NCBI_PROTEIN", "EMBL", "COMPOUND" or "PUBCHEM_COMPOUND".
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#'
#' @return NULL, a CSV,JSON or Gzipped JSON file will be saved to disk based on
#'   your input.
#'
#' @references \itemize{ \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla,
#'   C., Beavers, D., Grentner, A., ... D’Eustachio, P. (2026). The Reactome
#'   Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
#'   10.1093/nar/gkaf1223
#'   \item Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A.,
#'   & Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
#'   Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
#'   doi: 10.1074/mcp.TIR120.002155
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis_download(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM",
#'     request = "pathways", save_to = "found_ids.csv")
#' }
#' \dontrun{
#' rba_reactome_analysis_download(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM",
#'     request = "found_ids", save_to = "found_ids.csv")
#' }
#'
#' @family "Reactome Analysis Service"
#' @seealso \code{\link{rba_reactome_analysis_pdf}}
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_analysis_download <- function(token,
                                           request,
                                           save_to = NULL,
                                           resource = "TOTAL",
                                           ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "token", class = "character", len = 1L),
      list(
        arg = "request", class = "character", len = 1L,
        val = c("found_ids", "not_found_ids", "pathways", "results", "results_gz")
      ),
      list(arg = "save_to", class = "character", len = 1L, no_na = FALSE),
      list(
        arg = "resource", class = "character", len = 1L, no_null = TRUE,
        val = c("TOTAL",
                "UNIPROT",
                "ENSEMBL",
                "CHEBI",
                "IUPHAR",
                "MIRBASE",
                "NCBI_PROTEIN",
                "EMBL",
                "COMPOUND",
                "PUBCHEM_COMPOUND")
      )
    ),
    cond = list(
      list(
        quote(request %in% c("not_found_ids", "results", "results_gz") && resource != "TOTAL"),
        c("You cannot supply 'resource' with ", request, " request. ignoring resource.")
      )
    ),
    cond_warning = TRUE
  )

  .msg(
    "Saving %s of the Reactome analysis associated with token: %s",
    switch(
      request,
      "found_ids" = "found identifiers",
      "not_found_ids" = "not-found identifiers",
      "pathways" = "pathway results",
      "results" = "full results",
      "results_gz" = "compressed full results"),
    token
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%sdownload/%s/",
    .rba_stg("reactome", "pth", "analysis"),
    token
  )

  path_input <- switch(
    request,
    "found_ids" = sprintf("%sentities/found/%s/%s.csv", path_input, resource, token),
    "not_found_ids" = sprintf("%sentities/notfound/%s.csv", path_input, token),
    "pathways" = sprintf("%spathways/%s/%s.csv", path_input, resource, token),
    "results" = paste0(path_input, "result.json"),
    "results_gz" = paste0(path_input, "result.json.gz")
  )

  if (request == "results") {
    output_format <- "json"
    accept_input <- "application/json"
  } else if (request == "results_gz") {
    output_format <- "json.gz"
    accept_input <- "application/x-gzip"
  } else {
    output_format <- "csv"
    accept_input <- "text/csv"
  }

  # create file_path
  save_to <- .rba_file(
    file = paste0(request, "_", token, ".", output_format),
    save_to = ifelse(
      is.null(save_to) || is.na(save_to),
      yes = TRUE, no = save_to
    )
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    accept = accept_input,
    save_to = save_to,
    parser = NULL
  )

  ## Call API
  invisible(.rba_skeleton(input_call))
}

#### Import Endpoints ####
#' Import Saved Analysis JSON to Reactome
#'
#' If you have a JSON file of analysis results (only obtained via
#'   \code{\link{rba_reactome_analysis_download}} with the result argument
#'   set to "results", or "results_gz"), you can import the results back to
#'   Reactome and retrieve a token.
#'   \cr This is useful when you want to use other Reactome services which
#'   require a token but you do not have a token or your token has been
#'   expired (i.e. more than 7 days passed from your analysis).
#'
#' @section Corresponding API Resources:
#' "POST https://reactome.org/AnalysisService/import/form"
#' \cr "POST https://reactome.org/AnalysisService/import/url"
#'
#' @param input Character: A local file path or HTTP or HTTPS URL that points
#'   to plain or gzipped saved analysis results.
#' @param input_format Character: (optional) This function will automatically
#'  identify your supplied input's format. To be explicit, set this argument to
#'  one of:\itemize{
#'   \item "file": If you supplied a local file path pointing to the saved
#'   results file.
#'   \item "url": If you supplied an HTTP or HTTPS URL pointing to the saved
#'   results file.}
#'   An explicit value takes precedence. Otherwise, HTTP and HTTPS addresses
#'   are identified before existing local files. Other inputs are rejected.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the new token and other information of your
#'   imported results.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A.,
#'   & Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
#'   Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
#'   doi: 10.1074/mcp.TIR120.002155
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis_import("c:/rbioapi/res.json")
#' }
#' \dontrun{
#' rba_reactome_analysis_import("https://qaz.com/res.json.gz")
#' }
#'
#' @family "Reactome Analysis Service"
#' @export
rba_reactome_analysis_import <- function(input,
                                         input_format = NULL,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "input", class = "character", len = 1L),
      list(
        arg = "input_format", class = "character", len = 1L,
        val = c("file", "url")
      )
    )
  )

  .msg(
    "Importing saved Reactome analysis results."
  )

  ## Build Function-Specific Call
  # Handle supplied input
  input_type <- .rba_reactome_input(
    input = input,
    type = input_format,
    prepare_upload = FALSE
  )

  if (!input_type %in% c("file", "url")) {
    stop(
      "`input` must be an existing local file or an HTTP or HTTPS URL.",
      call. = get("diagnostics")
    )
  }

  input <- list(type = input_type, file = input)

  if (input$type == "url") {

    path_input <- paste0(
      .rba_stg("reactome", "pth", "analysis"),
      "import/url"
    )
    call_body <- input$file
    content_type_input <- httr::content_type("text/plain")

  } else {

    path_input <- paste0(
      .rba_stg("reactome", "pth", "analysis"),
      "import/form"
    )
    call_body <- list(
      file = httr::upload_file(path = input$file)
    )
    content_type_input <- NULL

  }

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    body = call_body,
    content_type_input,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("reactome_analysis_import.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Mapping Endpoints ####
#' Maps Molecule Identifiers
#'
#' Use this function to map molecule identifiers of different species to
#'   Reactome Identifiers.
#'
#' @section Corresponding API Resources:
#' "POST https://reactome.org/AnalysisService/mapping/form"
#' \cr "POST https://reactome.org/AnalysisService/mapping/form/projection"
#' \cr "POST https://reactome.org/AnalysisService/mapping/url"
#' \cr "POST https://reactome.org/AnalysisService/mapping/url/projection"
#'
#' @param input Character or Numeric vector: A vector, local file path or URL
#'   that points to your identifiers list.
#' @param input_format Character: (optional) This function will automatically identify
#'   your supplied input's format. To be explicit, set this argument to one
#'   of:\itemize{
#'   \item "vector": If you supplied a simple vector (numeric or character) as
#'   input.
#'   \item "file": If you supplied a local file path pointing to a
#'   correctly-formatted text file.
#'   \item "url": If you supplied an HTTP or HTTPS URL pointing to a
#'   correctly-formatted text file.}
#'   An explicit value takes precedence. Otherwise, HTTP and HTTPS addresses
#'   are identified first, followed by existing local files, and then other
#'   non-empty character or numeric inputs as identifier vectors.
#' @param projection Logical: (default = \code{TRUE}) Should non-human identifiers
#'   be projected to their human equivalents? (using Reactome orthology data)
#' @param interactors Logical: (default = \code{FALSE}) Should IntAct interaction data
#'   be included?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List containing your identifiers and the IDS and resources they
#'   are mapped to.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A.,
#'   & Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
#'   Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
#'   doi: 10.1074/mcp.TIR120.002155
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis_mapping(c("Q8SQ34", "cd40"))
#' }
#'
#' @family "Reactome Analysis Service"
#' @export
rba_reactome_analysis_mapping <- function(input,
                                          input_format = NULL,
                                          projection = TRUE,
                                          interactors = FALSE,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "input", class = c("character", "numeric", "integer"),
        min_len = 1L
      ),
      list(
        arg = "input_format", class = "character",
        len = 1L,
        val = c("vector", "file", "url")
      ),
      list(arg = "projection", class = "logical", len = 1L),
      list(arg = "interactors", class = "logical", len = 1L)
    )
  )

  .msg(
    "Mapping your supplied input identifiers."
  )

  ## Build POST API Request's query
  call_query <- list("interactors" = ifelse(interactors, "true", "false"))

  ## Build POST API Request's URL
  # Handle supplied input
  input <- .rba_reactome_input(
    input = input,
    type = input_format
  )

  if (input$type == "file") {
    call_body <- list(
      file = httr::upload_file(path = input$file)
    )
    submission_type <- "form"
    content_type_input <- NULL
  } else {
    call_body <- input$file
    submission_type <- "url"
    content_type_input <- httr::content_type("text/plain")
  }

  ## Build Function-Specific Call
  path_input <- paste0(
    .rba_stg("reactome", "pth", "analysis"),
    "mapping/",
    submission_type
  )

  if (isTRUE(projection)) {
    path_input <- paste0(path_input, "/projection")
  }

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("reactome", "url"),
    path = path_input,
    body = call_body,
    query = call_query,
    content_type_input,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("reactome_analysis_mapping.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Species Endpoints ####
#' Compare Human Pathways with with Other Species
#'
#' Use This function to Compare human's manually-curated pathways
#'   and computationally inferred pathways (orthologous) in other species.
#'
#' Reactome incorporate manually curated human reactions and PANTHER's
#'   protein homology data to Computationally infer events in other eukaryotic
#'   species.
#' \cr Reactome uses an orthology-based approach to project curated human
#'   events to supported non-human species. See
#'   \href{https://reactome.org/documentation/inferred-events/}{
#'   Reactome Computationally Inferred Events} for more information.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/AnalysisService/species/homoSapiens/\{species\}"
#'
#' @param species_dbid Numeric: Reactome DbId (e.g  Mus musculus is 48892) of
#'   the species you want to compare with Homo sapiens. See
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param sort_by Character: (default = \code{"ENTITIES_PVALUE"}) Sort the
#'   result based on what column? Available choices are: "NAME",
#'   "TOTAL_ENTITIES", "TOTAL_INTERACTORS", "TOTAL_REACTIONS", "FOUND_ENTITIES",
#'   "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
#'   "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO".
#' @param order Character: (default = \code{"ASC"}) Sort Order. Can be either
#'   "ASC" or "DESC".
#' @param resource Character: (default = \code{"TOTAL"}) Filter results based on
#'   the resource. Available choices are: "TOTAL", "UNIPROT", "ENSEMBL",
#'   "CHEBI", "IUPHAR", "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND" or
#'   "PUBCHEM_COMPOUND".
#' @param p_value Numeric: (default = \code{1}) Set a P value threshold. Only
#'   results with P value equal to or less than your supplied threshold will be
#'   returned (1 means no P value filtering).
#' @param min Numeric: (optional) Minimum number of entities that a pathways
#'   should have to be included in the results.
#' @param max Numeric: (optional) Maximum number of entities that a pathways
#'   should have to be included in the results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the comparison results. The \code{pathways}
#'   element is a data frame with information about each pathway expanded into
#'   columns; it is an empty data frame when no pathways match.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A.,
#'   & Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
#'   Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
#'   doi: 10.1074/mcp.TIR120.002155
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_analysis_species(species_dbid = 48892)
#' }
#'
#' @family "Reactome Analysis Service"
#' @seealso
#' \code{\link{rba_reactome_orthology}}
#' @export
rba_reactome_analysis_species <- function(species_dbid,
                                          sort_by = "ENTITIES_PVALUE",
                                          order = "ASC",
                                          resource = "TOTAL",
                                          p_value = 1,
                                          min = NULL,
                                          max = NULL,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "species_dbid", class = c("numeric", "integer"),
        len = 1L, min_val = 0
      ),
      list(
        arg = "sort_by",
        class = "character", len = 1L,
        val = c("NAME",
                "TOTAL_ENTITIES",
                "TOTAL_INTERACTORS",
                "TOTAL_REACTIONS",
                "FOUND_ENTITIES",
                "FOUND_INTERACTORS",
                "FOUND_REACTIONS",
                "ENTITIES_RATIO",
                "ENTITIES_PVALUE",
                "ENTITIES_FDR",
                "REACTIONS_RATIO")
      ),
      list(arg = "order", class = "character", len = 1L, val = c("ASC", "DESC")),
      list(
        arg = "resource",
        class = "character", len = 1L,
        val = c("TOTAL",
                "UNIPROT",
                "ENSEMBL",
                "CHEBI",
                "IUPHAR",
                "MIRBASE",
                "NCBI_PROTEIN",
                "EMBL",
                "COMPOUND",
                "PUBCHEM_COMPOUND")
      ),
      list(arg = "p_value", class = c("numeric", "integer"), len = 1L, ran = c(0, 1)),
      list(arg = "min", class = c("numeric", "integer"), len = 1L, min_val = 0),
      list(arg = "max", class = c("numeric", "integer"), len = 1L, min_val = 0)
    ),
    cond = list(
      list(
        quote(!is.finite(species_dbid) || species_dbid != floor(species_dbid)),
        "`species_dbid` should be a finite, non-negative integer."
      ),
      list(
        quote(!is.null(min) && (!is.finite(min) || min != floor(min))),
        "`min` should be a finite, non-negative integer."
      ),
      list(
        quote(!is.null(max) && (!is.finite(max) || max != floor(max))),
        "`max` should be a finite, non-negative integer."
      ),
      list(
        quote(!is.null(min) && !is.null(max) && min > max),
        "`min` cannot be greater than `max`."
      )
    )
  )

  .msg(
    "Comparing human pathways and computationally inferred pathways of species %s.",
    species_dbid
  )

  ## Build POST API Request's query
  call_query <- .rba_query(
    init = list("sortBy" = sort_by, "order" = order, "resource" = resource),
    list("pValue", !is.null(p_value), p_value),
    list("min", !is.null(min), min),
    list("max", !is.null(max), max)
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list_simp",
    .rba_reactome_analysis_result
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = paste0(
      .rba_stg("reactome", "pth", "analysis"),
      "species/homoSapiens/",
      species_dbid
    ),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("reactome_analysis_species.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Token Endpoints ####
#' Return the Results Associated with a Token
#'
#' Use a token generated After a Reactome analysis
#'   (via \code{\link{rba_reactome_analysis}}) to Retrieve the analysis results.
#'   The output format is identical to the returned object of
#'   \code{\link{rba_reactome_analysis}}.
#'
#' After any analysis, Reactome will associate a token with your analysis. It
#'   can later be used in functions that require the token (e.g. to retrieve
#'   the analysis results, download pdf).
#'   \cr Note that Reactome will store your token for only 7 days. You can
#'   download your full results with
#'   \code{\link{rba_reactome_analysis_download}}, and re-import it anytime to
#'   reactome (using \code{\link{rba_reactome_analysis_import}}) to generate
#'   a new token.
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/AnalysisService/token/\{token\}"
#'
#' @param token Character: A token associated to your previous Reactome analysis.
#' @param species Character or Numeric: (optional) NCBI Taxonomy identifier
#'   (Human is 9606), species name (e.g. "Homo sapiens") or Reactome DbId
#'   (e.g. Homo sapiens is 48887). See
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species/}{Reactome
#'    Data Schema: Entries: Species}.
#' @param sort_by Character: (default = \code{"ENTITIES_PVALUE"}) Sort the
#'   result based on what column? Available choices are: "NAME",
#'   "TOTAL_ENTITIES", "TOTAL_INTERACTORS", "TOTAL_REACTIONS", "FOUND_ENTITIES",
#'   "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
#'   "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO".
#' @param order Character: (default = \code{"ASC"}) Sort Order. Can be either
#'   "ASC" or "DESC".
#' @param resource Character: (default = \code{"TOTAL"}) Filter results based on
#'   the resource. Available choices are: "TOTAL", "UNIPROT", "ENSEMBL",
#'   "CHEBI", "IUPHAR", "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND" or
#'   "PUBCHEM_COMPOUND".
#' @param p_value Numeric: (default = \code{1}) Set a P value threshold. Only
#'   results with P value equal to or less than your supplied threshold will be
#'   returned (1 means no P value filtering).
#' @param include_disease Logical: (default = \code{TRUE}) Should the disease
#'   pathways be included in the results?
#' @param min Numeric: (optional) Minimum number of entities that a pathways
#'   should have to be included in the results.
#' @param max Numeric: (optional) Maximum number of entities that a pathways
#'   should have to be included in the results.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the results and information about the analysis.
#'   The \code{pathways} element is a data frame with information about each
#'   pathway expanded into columns; it is an empty data frame when no pathways
#'   match. Its structure is the same as the output from
#'   \code{\link{rba_reactome_analysis}}.
#'
#' @references \itemize{
#'   \item Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
#'   Grentner, A., ... D’Eustachio, P. (2026). The Reactome Knowledgebase 2026.
#'   Nucleic Acids Res., 54(D1), D673–D681. doi: 10.1093/nar/gkaf1223
#'   \item Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A.,
#'   & Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
#'   Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
#'   doi: 10.1074/mcp.TIR120.002155
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   \item \href{https://reactome.org/cite}{Citations note on Reactome website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis_token(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM",
#'     species = 9606)
#' }
#'
#' @family "Reactome Analysis Service"
#' @seealso
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_analysis_token <- function(token,
                                        species = NULL,
                                        sort_by = "ENTITIES_PVALUE",
                                        order = "ASC",
                                        resource = "TOTAL",
                                        p_value = 1,
                                        include_disease = TRUE,
                                        min = NULL,
                                        max = NULL,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "token", class = "character", len = 1L),
      list(arg = "species", class = c("character", "numeric", "integer"), len = 1L),
      list(
        arg = "sort_by",
        class = "character", len = 1L,
        val = c("NAME",
                "TOTAL_ENTITIES",
                "TOTAL_INTERACTORS",
                "TOTAL_REACTIONS",
                "FOUND_ENTITIES",
                "FOUND_INTERACTORS",
                "FOUND_REACTIONS",
                "ENTITIES_RATIO",
                "ENTITIES_PVALUE",
                "ENTITIES_FDR",
                "REACTIONS_RATIO")
      ),
      list(arg = "order", class = "character", len = 1L, val = c("ASC", "DESC")),
      list(
        arg = "resource",
        class = "character", len = 1L,
        val = c("TOTAL",
                "UNIPROT",
                "ENSEMBL",
                "CHEBI",
                "IUPHAR",
                "MIRBASE",
                "NCBI_PROTEIN",
                "EMBL",
                "COMPOUND",
                "PUBCHEM_COMPOUND")
      ),
      list(arg = "p_value", class = c("numeric", "integer"), len = 1L, ran = c(0, 1)),
      list(arg = "include_disease", class = "logical", len = 1L),
      list(arg = "min", class = c("numeric", "integer"), len = 1L, min_val = 0),
      list(arg = "max", class = c("numeric", "integer"), len = 1L, min_val = 0)
    ),
    cond = list(
      list(
        quote(!is.null(min) && (!is.finite(min) || min != floor(min))),
        "`min` should be a finite, non-negative integer."
      ),
      list(
        quote(!is.null(max) && (!is.finite(max) || max != floor(max))),
        "`max` should be a finite, non-negative integer."
      ),
      list(
        quote(!is.null(min) && !is.null(max) && min > max),
        "`min` cannot be greater than `max`."
      )
    )
  )

  .msg(
    "Retrieving Reactome analysis results with token %s.",
    token
  )

  ## Build POST API Request's query
  call_query <- .rba_query(
    init = list(
      "sortBy" = sort_by,
      "order" = order,
      "resource" = resource,
      "includeDisease" = ifelse(include_disease, "true", "false")
    ),
    list("species", !is.null(species), species),
    list("pValue", !is.null(p_value), p_value),
    list("min", !is.null(min), min),
    list("max", !is.null(max), max)
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list_simp",
    .rba_reactome_analysis_result
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("reactome", "url"),
    path = paste0(.rba_stg("reactome", "pth", "analysis"), "token/", token),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("reactome_analysis_token.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
