#### Internal functions ####

#' Internal Function to Handle Different inputs of Reactome Analysis
#'
#' This function will be called within any Reactome Analysis service which
#'   requires input from the users.
#'
#' @param input Pass on caller function's input argument to this.
#' @param type Pass on caller function's input_format argument to this.
#'   (default = NA)
#' @param handle Logical: If TRUE (default), The input will be -if necessary-
#'   written to a temp file to facilitate data uploading to Reactome.
#'   If False, The Input will only be identified.
#'
#' @return If handle was FALSE, a single string with the identified file type
#'   (one of: "table", "vector", "file" or "url"), else if handle was TRUE,
#'   a list containing the file Type and a path to the tempfile containing
#'   the data or the user-provided url/file path.
#'
#' @export
.rba_reactome_input <- function(input,
                                type = NA,
                                handle = TRUE){
  diagnostics <- get0("diagnostics", envir = parent.frame(1),
                      ifnotfound = getOption("rba_diagnostics"))
  ### 1 identify input
  if (is.na(type)) {
    if (is.data.frame(input) |
        is.matrix(input)) {
      type <- "table"
    } else if (is.vector(input) &&
               length(input) > 1) {
      type <- "vector"
    } else if (is.character(input) &&
               length(input) == 1) {
      if (grepl(pattern = "^[a-zA-z]:|^\\\\\\w|^/|^\\w+\\.\\w+$",
                x = input)) {
        type <- "file"
        if (!file.exists(input)) {
          stop("You provided a file path that does not exist or it is not ",
               "accessible. Please Check your provided input. If you did not ",
               "provide a file path, kindly set 'input-type = \"file\"' to ",
               "an appropriate value and try again.",
               immediate. = TRUE,
               call. = diagnostics)
        }
      } else if (grepl(pattern = "^http\\:|^https\\:|^ftp\\:|^ftps\\:|^(\\w+)(\\.\\w+)+/\\w",
                       x = input)) {
        type <- "url"
      } else {
        type <- "vector"
      }
    } else {
      stop("Could not identify your input format. Please specify it using 'input_format' argument.",
           call. = diagnostics)
    }
  }
  ### 2 handle input
  if (isFALSE(handle)) {
    return(type)
  } else {
    if (type == "file" |
        type == "url") {
      return(list(type = type,
                  file = input))
    } else {
      temp_file <- tempfile(pattern = "rba", fileext = ".txt")

      if (type == "table") {
        input <- as.data.frame(input,
                               stringsAsFactors = FALSE)
        #make sure that every column name starts with #
        inproper_colnames <- !grepl("^#", colnames(input))
        if (any(inproper_colnames)) {
          colnames(input)[inproper_colnames] <- paste0("#",
                                                       colnames(input)[inproper_colnames])
        }
        utils::write.table(x = as.character(input),
                           file = temp_file,
                           sep = "\t",
                           row.names = FALSE,
                           col.names = TRUE)
        return(list(type = "file",
                    file = temp_file))
      } else if (type == "vector") {
        writeLines(text = input,
                   con = temp_file,
                   sep = "\n")
        return(list(type = "file",
                    file = temp_file))
      } else {
        stop("Internal error!", call. = TRUE)
      }
    }
  }
}
#### Identifiers Endpoints ####

#' Reactome Over-Representation or Expression Analysis
#'
#' Using this function, you can perform Reactome Analysis In a convenient way.
#'   The Analysis Type will be chosen depending on your provided
#'   input:\enumerate{
#'   \item If you provide a vector or a single-columned table,
#'   "Over-Representation" analysis will be performed.
#'   \item If you provide a multi-columend table, with the first column being
#'   molecules identifers and the rest being numeral expression values,
#'   "Expression" analysis will be performed.}
#'   \cr Refer to the details section for the accepted input types and format.
#'
#' You can provide your table or vector input in numerous formats:\enumerate{
#'   \item A R object which can be data frame, matrix or a simple vector.
#'   \item A path to a local text file in your device that contains the molecules
#'   data. (The file should be formatted correctly, see below.)
#'   \item A URL pointing to a text file on the web that contains the molecules
#'   data. (The file should be formatted correctly, see below.}
#'   \cr If you provide a text file (as a local file path or URL), it should be
#'   in TSV (Tab-Separated Values) format; Column names should start with "#"
#'   character. Note that if you are providing the file for
#'   "Over-Representation" analysis (i.e. Single columned-data) this header
#'   line is optional and will be used as your 'Sample Name', otherwise it is
#'   required.
#'   \cr Also, form the "summary" element in the function's output, you can see
#'   how Reactome Interpreted your input and subsequently the type of analysis
#'   that has been performed.
#'   \cr There is no strict criteria about the type of your molecules Identifiers,
#'   Reactome will Map the IDs to it's internal database entities.
#'   Nevertheless, You can check if all your identifiers has been found in
#'   "identifiersNotFound" element in the function's output.
#'   \cr After Any Analysis, Reactome will associate a token to your analysis.
#'   It can be later used to in function that requires the token (e.g to retrieve
#'   the analysis results, download pdf).
#'   \cr Note that Reactome will store your token for only 7 days. You can
#'   download your full results with
#'   \code{\link{rba_reactome_analysis_download}}, and re-import it anytime to
#'   reactome (using \code{\link{rba_reactome_analysis_import}}) to generate
#'   a new token.
#'
#' @section Corresponding API Resources:
#'  "POST https://reactome.org/AnalysisService/identifiers/form"
#'  \cr "POST https://reactome.org/AnalysisService/identifiers/url"
#'  \cr "POST https://reactome.org/AnalysisService/identifiers/form/projection"
#'  \cr "POST https://reactome.org/AnalysisService/identifiers/url/projection"
#'
#' @param input A vector, data frame, matrix or a local file path or URL
#'   that points to your data. See "Details section" for more information of
#'   how to organize and provide your input.
#' @param input_format (Optional) This function will automatically identify
#'   your provided input's format. But in case of unexpected issues or if you
#'   want to be explicit, set this argument to one of:\itemize{
#'   \item "table": If you provided a data frame or matrix as input.
#'   \item "vector": If you provided a simple vector (numeric or character) as
#'   input.
#'   \item "file": If you provided a local file path pointing to a
#'   correctly-formatted text file.
#'   \item "url": If you provided a URL pointing to a correctly-formatted
#'   text file.}
#' @param species Numeric or Character: NCBI Taxonomy identifier (Human
#'   is 9606), species name (e.g. "Homo sapiens") or Reactome DbId (e.g
#'   Homo sapiens is 48887). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param projection Logical (default = FALSE) Should non-human identifiers
#'   be projected to their human equivalents? (using Reactome orthology data)
#' @param interactors Logical (default = FALSE) Should IntAct interaction data
#'   be used to increase the analysis background?
#' @param sort_by Sort the result based on what column? available choices
#'   are: "NAME", "TOTAL_ENTITIES", "TOTAL_INTERACTORS", "TOTAL_REACTIONS",
#'   "FOUND_ENTITIES", "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
#'   "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO"
#' @param order Sort Order. Can be either "ASC" (default) or "DESC".
#' @param resource Filter results based on the resource. Default is "TOTAL",
#'   available choices are: "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR",
#'   "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND", "ENTITIES_FDR" or
#'   "PUBCHEM_COMPOUND".
#' @param p_value Set a P value threshold. Only results with P value equal to
#'   or less than your provided threshold will be returned. (default = 1,
#'   Meaning no P value filtering)
#' @param include_disease Logical (default = TRUE) Should the disease pathways
#'   be included in the results?
#' @param min (numeric) Minimum number of entities that a pathways should have
#'   to be included in the results.
#' @param max (numeric) Maximum number of entities that a pathways should have
#'   to be included in the results.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List containing the results and information of your analysis. Note
#'   that you can use the token returned in the "summary" sub-list of the
#'   results (i.e. results$summary$token) to retrieve your results later or
#'   in other Reactome analysis functions.
#'
#' @references \itemize{
#'   \item Fabregat A, Sidiropoulos K, Viteri G, Forner O, Marin-Garcia P,
#'   Arnau V, D'Eustachio P, Stein L, Hermjakob H. Reactome pathway analysis:
#'   a high-performance in-memory approach. BMC bioinformatics. 2017 Mar;18(1)
#'   142. doi: 10.1186/s12859-017-1559-2. PubMed PMID: 28249561.
#'   PubMed Central PMCID: PMC5333408.
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_analysis(input = c("p53", "BRCA1", "cdk2", "Q99835", "CDC42"))
#' }
#' \dontrun{
#' rba_reactome_analysis(input = "c:/rbioapi/genes.txt")
#' }
#' \dontrun{
#' rba_reactome_analysis(input = "https://qazwsx.com/genes.txt")
#' }
#'
#' @family "Reactome Analysis Service"
#' @export
rba_reactome_analysis <- function(input,
                                  input_format = NA,
                                  projection = FALSE,
                                  interactors = FALSE,
                                  species = NA,
                                  sort_by = "ENTITIES_PVALUE",
                                  order = "ASC",
                                  resource = "TOTAL",
                                  p_value = 1,
                                  include_disease = TRUE,
                                  min = NA,
                                  max = NA,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "input",
                             class = c("character",
                                       "numeric",
                                       "data.frame",
                                       "matrix")),
                        list(arg = "input_format",
                             class = "character",
                             val = c("table",
                                     "vector",
                                     "file",
                                     "url")),
                        list(arg = "projection",
                             class = "logical"),
                        list(arg = "interactors",
                             class = "logical"),
                        list(arg = "species",
                             class = c("character",
                                       "numeric")),
                        list(arg = "sort_by",
                             class = "character",
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
                                     "REACTIONS_RATIO")),
                        list(arg = "order",
                             class = "character",
                             val = c("ASC",
                                     "DESC")),
                        list(arg = "resource",
                             class = "character",
                             val = c("TOTAL",
                                     "UNIPROT",
                                     "ENSEMBL",
                                     "CHEBI",
                                     "IUPHAR",
                                     "MIRBASE",
                                     "NCBI_PROTEIN",
                                     "EMBL",
                                     "COMPOUND",
                                     "ENTITIES_FDR",
                                     "PUBCHEM_COMPOUND")),
                        list(arg = "p_value",
                             class = "numeric"),
                        list(arg = "include_disease",
                             class = "logical"),
                        list(arg = "min",
                             class = "numeric"),
                        list(arg = "max",
                             class = "numeric")),
            cond = list(list("sum(projection, !is.na(species)) == 2",
                             "You cannot Provide 'species' when 'projection' argument is TRUE"))
  )

  .msg("Retrieving Reactome Analysis Results of your provided Identifiers.")

  ## Build POST API Request's query
  call_query <- list("interactors" = ifelse(interactors, "true", "false"),
                     "sortBy" = sort_by,
                     "order" = order,
                     "resource" = resource,
                     "includeDisease" = ifelse(include_disease, "true", "false"))

  call_query <- .rba_query(init = call_query,
                           list("species",
                                !is.na(species),
                                species),
                           list("pValue",
                                !is.na(p_value),
                                p_value),
                           list("min",
                                !is.na(min),
                                min),
                           list("max",
                                !is.na(max),
                                max))
  ## Build POST API Request's URL
  # handle provided input
  input <- .rba_reactome_input(input = input,
                              type = input_format,
                              handle = TRUE)
  if (input$type == "file") {
    call_body <- httr::upload_file(path = input$file,
                                   type = "text/plain")
  } else if (input$type == "url") {
    call_body <- input$file
  }

  ## Build Function-Specific Call
  path_input <- paste0(.rba_stg("reactome", "pth", "analysis"),
                       "identifiers/")
  if (input$type == "url") {
    paste0(path_input, "/url")
  }
  if (isTRUE(projection)) {
    path_input <- paste0(path_input, "/projection")
  }
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          body = call_body,
                          query = call_query,
                          httr::content_type("text/plain"),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("reactome_analysis.json"))
  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Report Endpoints ####

#' Generate PDF file with Reactome Analysis Results
#'
#' Use this function to save a detailed report of your previous analysis (That
#'   you have done with \code{\link{rba_reactome_analysis}}). You need
#'   to provide a 'token' associated to your previous analysis.
#'
#' Token is associated to each Reactome analysis results and kept by Reactome
#'   for at least 7 days. You can locate it in
#'   \code{\link{rba_reactome_analysis}}'s output, under a sub-list named
#'   "summary" (i.e. results$summary$token).
#'   \cr Note that Reactome will store your token for only 7 days. You can
#'   download your full results with
#'   \code{\link{rba_reactome_analysis_download}}, and re-import it anytime to
#'   reactome (using \code{\link{rba_reactome_analysis_import}}) to generate
#'   a new token.
#'   Use \code{\link{rba_reactome_analysis_download}} to save your results
#'   in other formats.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/AnalysisService/report/{token}/{species}/
#'  {filename}.pdf"
#'
#' @param token A token associated to your previous Reactome analysis.
#' @param species Numeric or Character: NCBI Taxonomy identifier (Human Taxonomy
#'    ID is 9606.) or species name (e.g. "Homo sapiens"). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param save_to NA or Character:\itemize{
#'   \item NA: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param number Numeric: Maximum number of the reported pathways. Cannot not
#'   be greater than 50.
#' @param resource Filter results based on the resource. Default is "TOTAL",
#'   available choices are: "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR",
#'   "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND", "ENTITIES_FDR" or
#'   "PUBCHEM_COMPOUND".
#' @param diagram_profile Color profile of diagrams, should be either
#'   "Modern" (default) or "Standard".
#' @param analysis_profile Color profile of analysis, should be one of:
#'   "Standard" (default), "Strosobar" or "Copper Plus".
#' @param fireworks_profile Color profile of overview diagram, should be one of:
#'   "Copper", "Copper Plus", "Barium Lithium" or "calcium salts".
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return NULL, a PDF file will be saved to disk.
#'
#' @references \itemize{
#'   \item Fabregat A, Sidiropoulos K, Viteri G, Forner O, Marin-Garcia P,
#'   Arnau V, D'Eustachio P, Stein L, Hermjakob H. Reactome pathway analysis:
#'   a high-performance in-memory approach. BMC bioinformatics. 2017 Mar;18(1)
#'   142. doi: 10.1186/s12859-017-1559-2. PubMed PMID: 28249561.
#'   PubMed Central PMCID: PMC5333408.
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis_pdf(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM%3D",
#'     species = 9606, save_to = "my_analysis.pdf")
#' }
#'
#' @family "Reactome Analysis Service"
#' @seealso
#' \code{\link{rba_reactome_analysis_download}}
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_analysis_pdf <- function(token,
                                      species,
                                      save_to = NA,
                                      number  = 25,
                                      resource = "TOTAL",
                                      diagram_profile = "Modern",
                                      analysis_profile = "Standard",
                                      fireworks_profile = "Barium Lithium",
                                      ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "token",
                             class = "character"),
                        list(arg = "species",
                             class = c("character",
                                       "numeric")),
                        list(arg = "save_to",
                             class = "character"),
                        list(arg = "number",
                             class = "numeric"),
                        list(arg = "resource",
                             class = "character",
                             val = c("TOTAL",
                                     "UNIPROT",
                                     "ENSEMBL",
                                     "CHEBI",
                                     "IUPHAR",
                                     "MIRBASE",
                                     "NCBI_PROTEIN",
                                     "EMBL",
                                     "COMPOUND",
                                     "ENTITIES_FDR",
                                     "PUBCHEM_COMPOUND")),
                        list(arg = "diagram_profile",
                             class = "character",
                             val = c("Modern",
                                     "Standard")),
                        list(arg = "analysis_profile",
                             class = "character",
                             val = c("Standard",
                                     "Strosobar",
                                     "Copper Plus")),
                        list(arg = "fireworks_profile",
                             class = "character",
                             val = c("Copper",
                                     "Copper Plus",
                                     "Barium Lithium",
                                     "calcium salts"))))

  .msg("GET /report/{token}/{species}/{filename}.pdf",
       "Downloads a report for a given pathway analysis result")

  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("number",
                                number != 25,
                                number),
                           list("resource",
                                resource != "TOTAL",
                                resource),
                           list("token",
                                !is.na(token),
                                token),
                           list("diagramProfile",
                                diagram_profile != "Modern",
                                diagram_profile),
                           list("analysisProfile",
                                analysis_profile != "Standard",
                                analysis_profile),
                           list("fireworksProfile",
                                fireworks_profile != "Barium Lithium",
                                fireworks_profile))

  # create file_path
  save_to <- .rba_file(file = paste0(token, ".pdf"),
                       save_to = ifelse(is.na(save_to),
                                        yes = TRUE,
                                        no = save_to))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = sprintf("%sreport/%s/%s/%s.pdf",
                                         .rba_stg("reactome", "pth", "analysis"),
                                         token, species, token),
                          query = call_query,
                          accept = "application/pdf",
                          parser = NULL,
                          save_to = save_to)
  ## Call API
  invisible(.rba_skeleton(input_call))
}

#### Download Endpoints ####

#' Download Different Reactome Analysis Results
#'
#' Based on the "request" argument, you can download different analysis
#'   results data associated with a given token.
#'
#' Token is associated to each Reactome analysis results and kept by Reactome
#'   for at least 7 days. You can locate it in
#'   \code{\link{rba_reactome_analysis}}'s output, under a sub-list named
#'   "summary" (i.e. results$summary$token).
#'   \cr Use \code{\link{rba_reactome_analysis_pdf}} to save a full report
#'   in PDF format.
#'
#' @section Corresponding API Resources:
#' GET https://reactome.org/AnalysisService/download/{token}/entities/
#' found/{resource}/{filename}.csv"
#' GET https://reactome.org/AnalysisService//download/{token}/entities/
#' notfound/{filename}.csv"
#' GET https://reactome.org/AnalysisService/download/{token}/pathways/
#' {resource}/{filename}.csv"
#' GET https://reactome.org/AnalysisService/download/{token}/result.json"
#' GET https://reactome.org/AnalysisService/download/{token}/result.json.gz"
#'
#' @param token A token associated to your previous Reactome analysis.
#' @param request What to download? Should be one of:\itemize{
#'   \item "found_ids": Download a CSV file containing the found user-provided
#'   identifiers in the analysis associated with your provided token and
#'   resource.
#'   \item "not_found_ids"" Download a CSV file containing the user-provided
#'   Identifiers which has not been found in the analysis associated with your
#'   provided token.
#'   \item "pathways": Download a CSV file containing Pathway analysis results
#'   of the analysis associated with your provided token and resource.
#'   \item "results": Download a JSON file containing the complete analysis
#'   results associated with your provided token.
#'   \item "results_gz" Same as "results", but the output will be compress
#'   (gzipped).}
#' @param save_to NA or Character:\itemize{
#'   \item NA: Save the file to an automatically-generated path.
#'   \item Character string: A valid file path to save the file to.}
#' @param resource (Only when request is "found_ids" or "pathways")
#'   Filter results based on the resource. Default is "TOTAL",
#'   available choices are:"TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR",
#'   "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND", "ENTITIES_FDR" or
#'   "PUBCHEM_COMPOUND".
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return NULL, a CSV,JSON or Gzipped JSON file will be saved to disk
#'   based on your input.
#'
#' @references \itemize{
#'   \item Fabregat A, Sidiropoulos K, Viteri G, Forner O, Marin-Garcia P,
#'   Arnau V, D'Eustachio P, Stein L, Hermjakob H. Reactome pathway analysis:
#'   a high-performance in-memory approach. BMC bioinformatics. 2017 Mar;18(1)
#'   142. doi: 10.1186/s12859-017-1559-2. PubMed PMID: 28249561.
#'   PubMed Central PMCID: PMC5333408.
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_reactome_analysis_download(token = "MjAyMDEwMTYwMTI3MTNfMjY1MjM",
#'     request = "found_ids", save_to = "found_ids.csv")
#' }
#'
#' @family "Reactome Analysis Service"
#' @seealso
#' \code{\link{rba_reactome_analysis_pdf}}
#' \code{\link{rba_reactome_analysis}}
#' @export
rba_reactome_analysis_download <- function(token,
                                           request,
                                           save_to = NA,
                                           resource = "TOTAL",
                                           ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "token",
                             class = "character"),
                        list(arg = "request",
                             class = "character",
                             val = c("found_ids",
                                     "not_found_ids",
                                     "pathways",
                                     "results",
                                     "results_gz")),
                        list(arg = "save_to",
                             class = "character"),
                        list(arg = "resource",
                             class = "character",
                             val = c("TOTAL",
                                     "UNIPROT",
                                     "ENSEMBL",
                                     "CHEBI",
                                     "IUPHAR",
                                     "MIRBASE",
                                     "NCBI_PROTEIN",
                                     "EMBL",
                                     "COMPOUND",
                                     "ENTITIES_FDR",
                                     "PUBCHEM_COMPOUND"))),
            cond = list(list('grepl("^results|^not_found_ids$", request) & resource != "TOTAL"',
                             c("You cannot provide 'resource' with ",
                               request, " request. ignoring resource."))),
            cond_warning = TRUE)

  .msg("Saving %s of the Reactome Analysis asociated with token: %s",
       switch(request,
              "found_ids" = "found identifiers",
              "not_found_ids" = "not-found identifiers",
              "pathways" = "pathway results",
              "results" = "full results",
              "results_gz" = "compressed full results"),
       token)
  ## Build Function-Specific Call
  path_input <- sprintf("%sdownload/%s/",
                        .rba_stg("reactome", "pth", "analysis"),
                        token)
  path_input <- switch(request,
                       "found_ids" = sprintf("%sentities/found/%s/%s.csv",
                                             path_input, resource, token),
                       "not_found_ids" = sprintf("%sentities/notfound/%s.csv",
                                                 path_input, token),
                       "pathways" = sprintf("%sentities/pathways/%s/%s.csv",
                                            path_input, resource, token),
                       "results" = paste0(path_input, "result.json"),
                       "results_gz" = paste0(path_input, "result.json.gz"))
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
  save_to <- .rba_file(file = paste0(request, "_", token, ".", output_format),
                       save_to = ifelse(is.na(save_to),
                                        yes = TRUE,
                                        no = save_to))
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          accept = accept_input,
                          save_to = save_to,
                          parser = NULL)
  ## Call API
  invisible(.rba_skeleton(input_call))
}

#### Import Endpoints ####
#' Import Saved Analyhsis JSON to Reactome
#'
#' If you have a JSON file of analysis results (only obtained via
#'   \code{\link{rba_reactome_analysis_download}} with the result argument
#'   set to "results", or "results_gz"), you can import the results back to
#'   Reactome and retrieve a token.
#'   \cr This is useful when you want to use other Reactome services which require
#'   a token but you do not have a token or your token has been
#'   expired (i.e. more than 7 days passed from your analysis).
#'
#' @section Corresponding API Resources:
#' "GET https://reactome.org/AnalysisService/import/"
#' \cr "GET https://reactome.org/AnalysisService/import/form"
#' \cr "GET https://reactome.org/AnalysisService/import/url"
#'
#' @param input A local file path or URL that points to your -optionally
#'   gzipped- JSON file.
#' @param input_format (Optional) This function will automatically identify
#'   your provided input's format. But in case of unexpected issues or if you
#'   want to be explicit, set this argument to one of:\itemize{
#'   \item "file": If you provided a local file path pointing to the JSON file.
#'   \item "url": If you provided a URL pointing to the JSON file.}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list containing the new token and other information of your
#'   imported results.
#'
#' @references \itemize{
#'   \item Fabregat A, Sidiropoulos K, Viteri G, Forner O, Marin-Garcia P,
#'   Arnau V, D'Eustachio P, Stein L, Hermjakob H. Reactome pathway analysis:
#'   a high-performance in-memory approach. BMC bioinformatics. 2017 Mar;18(1)
#'   142. doi: 10.1186/s12859-017-1559-2. PubMed PMID: 28249561.
#'   PubMed Central PMCID: PMC5333408.
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
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
                                         input_format = NA,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "input",
                             class = "character"),
                        list(arg = "input_format",
                             class = "character",
                             val = c("file",
                                     "url"))))
  .msg("POST /import/form",
       "Imports the posted json file into the service")

  ## Build Function-Specific Call
  # handling input
  input <- .rba_reactome_input(input = input,
                               type = input_format,
                               handle = TRUE)
  if (input$type == "url") {
    path_input <- paste0(.rba_stg("reactome", "pth", "analysis"),
                         "import/url")
    call_body <- input$file
  } else {
    path_input <- paste0(.rba_stg("reactome", "pth", "analysis"),
                         "import/")
    call_body <- httr::upload_file(path = input$file,
                                   type = "application/json")
  }
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          body = call_body,
                          httr::content_type("text/plain"),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("reactome_analysis_import.json"))
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
#' "GET https://reactome.org/AnalysisService/mapping"
#' \cr "GET https://reactome.org/AnalysisService/mapping/form"
#' \cr "GET https://reactome.org/AnalysisService/mapping/form/projection"
#' \cr "GET https://reactome.org/AnalysisService/mapping"
#' \cr "GET https://reactome.org/AnalysisService/mapping/url"
#' \cr "GET https://reactome.org/AnalysisService/mapping/url/projection"
#'
#' @param input A vector, local file path or URL that points to your
#'   identifiers list.
#' @param input_format (Optional) This function will automatically identify
#'   your provided input's format. But in case of unexpected issues or if you
#'   want to be explicit, set this argument to one of:\itemize{
#'   \item "vector": If you provided a simple vector (numeric or character) as
#'   input.
#'   \item "file": If you provided a local file path pointing to a
#'   correctly-formatted text file.
#'   \item "url": If you provided a URL pointing to a correctly-formatted
#'   text file.}
#' @param projection Logical (default = FALSE) Should non-human identifiers
#'   be projected to their human equivalents? (using Reactome orthology data)
#' @param interactors Logical (default = FALSE) Should IntAct interaction data
#'   be included?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List containing your identifiers and the IDS and resources they
#'   are mapped to.
#'
#' @references \itemize{
#'   \item Fabregat A, Sidiropoulos K, Viteri G, Forner O, Marin-Garcia P,
#'   Arnau V, D'Eustachio P, Stein L, Hermjakob H. Reactome pathway analysis:
#'   a high-performance in-memory approach. BMC bioinformatics. 2017 Mar;18(1)
#'   142. doi: 10.1186/s12859-017-1559-2. PubMed PMID: 28249561.
#'   PubMed Central PMCID: PMC5333408.
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_reactome_analysis_mapping(c("Q8SQ34", "cd40"))
#' }
#'
#' @family "Reactome Analysis Service"
#' @export
rba_reactome_analysis_mapping <- function(input,
                                          input_format = NA,
                                          projection = FALSE,
                                          interactors = FALSE,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "input",
                             class = c("character",
                                       "numeric")),
                        list(arg = "input_format",
                             class = "character",
                             val = c("vector",
                                     "file",
                                     "url")),
                        list(arg = "projection",
                             class = "logical"),
                        list(arg = "interactors",
                             class = "logical")))

  .msg("Mapping your provided input identifiers.")

  ## Build POST API Request's query
  call_query <- list("interactors" = ifelse(interactors, "true", "false"))
  ## Build POST API Request's URL
  # handle provided input
  input <- .rba_reactome_input(input = input,
                               type = input_format,
                               handle = TRUE)
  if (input$type == "file") {
    call_body <- httr::upload_file(path = input$file,
                                   type = "text/plain")
  } else if (input$type == "url") {
    call_body <- input$file
  }
  ## Build Function-Specific Call
  path_input <- paste0(.rba_stg("reactome", "pth", "analysis"),
                       "mapping/")
  if (input$type == "url") {
    paste0(path_input, "/url")
  }
  if (isTRUE(projection)) {
    path_input <- paste0(path_input, "/projection")
  }
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("reactome", "url"),
                          path = path_input,
                          body = call_body,
                          query = call_query,
                          httr::content_type("text/plain"),
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("reactome_analysis_mapping.json"))
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
#' \cr Reactome incorporate manually curated human reactions and PANTHER's
#'   protein homology data to Computationally infer events in other euakaryotic
#'   species.
#' \cr In version 73 (11 June 2020), using an orthology-based approach,
#'   Homo sapiens events was projected to 18,654 orthologous pathways (with
#'   81,835 orthologous proteins) in 15 non-human species.
#'   Refer to \href{https://reactome.org/documentation/inferred-events}{
#'   Reactome Computationally Inferred Events} for more information.
#'
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/AnalysisService/species/homoSapiens/{species}"
#'
#' @param species_dbid Numeric: Reactome DbId (e.g  Mus musculus is 48892) of
#'   the species you want to compare with Homo sapiens. Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param sort_by Sort the result based on what column? available choices
#'   are: "NAME", "TOTAL_ENTITIES", "TOTAL_INTERACTORS", "TOTAL_REACTIONS",
#'   "FOUND_ENTITIES", "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
#'   "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO"
#' @param order Sort Order. Can be either "ASC" (default) or "DESC".
#' @param resource Filter results based on the resource. Default is "TOTAL",
#'   available choices are: "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR",
#'   "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND", "ENTITIES_FDR" or
#'   "PUBCHEM_COMPOUND".
#' @param p_value Set a P value threshold. Only results with P value equal to
#'   or less than your provided threshold will be returned. (default = 1,
#'   Meaning no P value filtering)
#' @param min (numeric) Minimum number of entities that a pathways should have
#'   to be included in the results.
#' @param max (numeric) Maximum number of entities that a pathways should have
#'   to be included in the results.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List with the results of the comparisson.
#'
#' @references \itemize{
#'   \item Fabregat A, Sidiropoulos K, Viteri G, Forner O, Marin-Garcia P,
#'   Arnau V, D'Eustachio P, Stein L, Hermjakob H. Reactome pathway analysis:
#'   a high-performance in-memory approach. BMC bioinformatics. 2017 Mar;18(1)
#'   142. doi: 10.1186/s12859-017-1559-2. PubMed PMID: 28249561.
#'   PubMed Central PMCID: PMC5333408.
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
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
                                          min = NA,
                                          max = NA,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "species_dbid",
                             class = "numeric"),
                        list(arg = "sort_by",
                             class = "character",
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
                                     "REACTIONS_RATIO")),
                        list(arg = "order",
                             class = "character",
                             val = c("ASC",
                                     "DESC")),
                        list(arg = "resource",
                             class = "character",
                             val = c("TOTAL",
                                     "UNIPROT",
                                     "ENSEMBL",
                                     "CHEBI",
                                     "IUPHAR",
                                     "MIRBASE",
                                     "NCBI_PROTEIN",
                                     "EMBL",
                                     "COMPOUND",
                                     "ENTITIES_FDR",
                                     "PUBCHEM_COMPOUND")),
                        list(arg = "p_value",
                             class = "numeric"),
                        list(arg = "min",
                             class = "numeric"),
                        list(arg = "max",
                             class = "numeric")))

  .msg("Comparing human's pathways and computationally inferred pathways of specie %s.",
       species_dbid)
  ## Build POST API Request's query
  call_query <- list("sortBy" = sort_by,
                     "order" = order,
                     "resource" = resource)
  call_query <- .rba_query(init = call_query,
                           list("pValue",
                                !is.na(p_value),
                                p_value),
                           list("min",
                                !is.na(min),
                                min),
                           list("max",
                                !is.na(max),
                                max))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = paste0(.rba_stg("reactome", "pth", "analysis"),
                                        "species/homoSapiens/",
                                        species_dbid),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("reactome_analysis_species.json"))
  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Token Endpoints ####
#' Return the Results Asociated with a Token
#'
#' Use a token generated After a Reactome analysis
#'   (via \code{\link{rba_reactome_analysis}}) to Retrieve the analysis results.
#'   The output format is identical to the returned object of
#'   \code{\link{rba_reactome_analysis}}.
#'
#' After Any Analysis, Reactome will associate a token to your analysis. It
#'   can be later used to in function that requires the token (e.g to retrieve
#'   the analysis results, download pdf).
#'   \cr Note that Reactome will store your token for only 7 days. You can
#'   download your full results with
#'   \code{\link{rba_reactome_analysis_download}}, and re-import it anytime to
#'   reactome (using \code{\link{rba_reactome_analysis_import}}) to generate
#'   a new token.
#' @section Corresponding API Resources:
#'  "GET https://reactome.org/AnalysisService/token/{token}"
#'
#' @param token A token associated to your previous Reactome analysis.
#' @param species Numeric or Character: NCBI Taxonomy identifier (Human
#'   is 9606), species name (e.g. "Homo sapiens") or Reactome DbId (e.g
#'   Homo sapiens is 48887). Refer to
#'    \code{\link{rba_reactome_species}} or
#'    \href{https://reactome.org/content/schema/objects/Species}{Reactome
#'    Data Schema: Entries: Species}.
#' @param sort_by Sort the result based on what column? available choices
#'   are: "NAME", "TOTAL_ENTITIES", "TOTAL_INTERACTORS", "TOTAL_REACTIONS",
#'   "FOUND_ENTITIES", "FOUND_INTERACTORS", "FOUND_REACTIONS", "ENTITIES_RATIO",
#'   "ENTITIES_PVALUE", "ENTITIES_FDR" or "REACTIONS_RATIO"
#' @param order Sort Order. Can be either "ASC" (default) or "DESC".
#' @param resource Filter results based on the resource. Default is "TOTAL",
#'   available choices are: "TOTAL", "UNIPROT", "ENSEMBL", "CHEBI", "IUPHAR",
#'   "MIRBASE", "NCBI_PROTEIN", "EMBL", "COMPOUND", "ENTITIES_FDR" or
#'   "PUBCHEM_COMPOUND".
#' @param p_value Set a P value threshold. Only results with P value equal to
#'   or less than your provided threshold will be returned. (default = 1,
#'   Meaning no P value filtering)
#' @param include_disease Logical (default = TRUE) Should the disease pathways
#'   be included in the results?
#' @param min (numeric) Minimum number of entities that a pathways should have
#'   to be included in the results.
#' @param max (numeric) Maximum number of entities that a pathways should have
#'   to be included in the results.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List containing the results and information of your analysis.
#'
#' @references \itemize{
#'   \item Fabregat A, Sidiropoulos K, Viteri G, Forner O, Marin-Garcia P,
#'   Arnau V, D'Eustachio P, Stein L, Hermjakob H. Reactome pathway analysis:
#'   a high-performance in-memory approach. BMC bioinformatics. 2017 Mar;18(1)
#'   142. doi: 10.1186/s12859-017-1559-2. PubMed PMID: 28249561.
#'   PubMed Central PMCID: PMC5333408.
#'   \item \href{https://reactome.org/AnalysisService/}{Reactome Analysis
#'   Services API Documentation}
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
                                        species,
                                        sort_by = "ENTITIES_PVALUE",
                                        order = "ASC",
                                        resource = "TOTAL",
                                        p_value = NA,
                                        include_disease = TRUE,
                                        min = NA,
                                        max = NA,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "token",
                             class = "character"),
                        list(arg = "species",
                             class = c("character",
                                       "numeric")),
                        list(arg = "sort_by",
                             class = "character",
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
                                     "REACTIONS_RATIO")),
                        list(arg = "order",
                             class = "character",
                             val = c("ASC",
                                     "DESC")),
                        list(arg = "resource",
                             class = "character",
                             val = c("TOTAL",
                                     "UNIPROT",
                                     "ENSEMBL",
                                     "CHEBI",
                                     "IUPHAR",
                                     "MIRBASE",
                                     "NCBI_PROTEIN",
                                     "EMBL",
                                     "COMPOUND",
                                     "ENTITIES_FDR",
                                     "PUBCHEM_COMPOUND")),
                        list(arg = "p_value",
                             class = "numeric"),
                        list(arg = "include_disease",
                             class = "logical"),
                        list(arg = "min",
                             class = "numeric"),
                        list(arg = "max",
                             class = "numeric")))

  .msg("GET /token/{token}",
       "Returns the result associated with the token")

  ## Build POST API Request's query
  call_query <- list("sortBy" = sort_by,
                     "order" = order,
                     "resource" = resource,
                     "includeDisease" = ifelse(include_disease, "true", "false"))

  call_query <- .rba_query(init = call_query,
                           list("pValue",
                                !is.na(p_value),
                                p_value),
                           list("min",
                                !is.na(min),
                                min),
                           list("max",
                                !is.na(max),
                                max))

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("reactome", "url"),
                          path = paste0(.rba_stg("reactome", "pth", "analysis"),
                                        "token/",
                                        token),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("reactome_analysis_token.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
