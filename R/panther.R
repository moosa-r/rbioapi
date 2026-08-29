#' Check a PANTHER Response for an Error
#'
#' PANTHER can include an error message in a response body while returning
#'   HTTP status 200. This function checks the decoded response before
#'   endpoint-specific parsing.
#'
#' @param response A decoded PANTHER response.
#'
#' @return The response unchanged, or the error message marked by
#'   .rba_api_error().
#'
#' @noRd
.rba_panther_check_response <- function(response) {
  if (!is.list(response)) {
    return(response)
  }

  search <- response[["search"]]

  if (!is.list(search) || is.null(search[["error"]])) {
    return(response)
  }

  return(.rba_api_error(search[["error"]]))
}

#' Keep PANTHER Tabular Results Consistent
#'
#' PANTHER returns one tabular record as a named object and multiple records as
#'   a table. This function only makes the outer structure consistent; it does
#'   not define, rename, reorder, or remove PANTHER fields.
#'
#' @param records Decoded PANTHER records.
#' @param field Optional field containing the records.
#'
#' @return A data frame for empty, single-record, and already-tabular results.
#'   Other structures are returned unchanged.
#'
#' @noRd
.rba_panther_data_frame <- function(records, field = NULL) {
  # Some endpoints wrap their records in a named result field.
  if (!is.null(field) && is.list(records)) {
    records <- records[[field]]
  }

  # Treat PANTHER's null and empty-string table responses as empty results.
  if (is.null(records) ||
      length(records) == 0L ||
      (is.character(records) &&
         length(records) == 1L &&
         !nzchar(records))) {
    return(data.frame())
  }

  # Preserve existing tables and structures that are not single named records.
  if (is.data.frame(records) ||
      !is.list(records) ||
      is.null(names(records))) {
    return(records)
  }

  # Wrap one named record in a one-row data frame without changing its fields.
  output <- data.frame(row.names = 1L)

  for (record_name in names(records)) {
    value <- records[[record_name]]

    if (is.data.frame(value) && nrow(value) != 1L) {
      # Keep a multi-row nested table inside the single parent record.
      output[[record_name]] <- I(list(value))
    } else if (is.list(value) && !is.null(names(value))) {
      # Normalize named nested objects without expanding the parent record.
      output[[record_name]] <- .rba_panther_data_frame(value)
    } else if (is.list(value) || length(value) > 1L) {
      # Preserve array-valued fields as list-columns rather than expanding rows.
      output[[record_name]] <- I(list(value))
    } else {
      # Preserve scalar fields as returned by PANTHER.
      output[[record_name]] <- value
    }
  }

  return(output)
}

#' Map A Gene-set to PANTHER Database
#'
#' Using this function, you can search your genes in PANTHER database and
#'   retrieve attributes and annotations associated to your genes.
#'
#' @param genes Character or Numeric: A vector of gene identifiers with maximum
#'   length of 5,000. Can be any of: Ensembl gene ID, Ensembl protein ID,
#'   Ensembl transcript ID, Entrez gene ID, gene symbol, NCBI GI, HGNC ID,
#'   International protein index ID, NCBI UniGene ID, UniProt accession and/or
#'   UniProt ID.
#' @param organism Numeric: NCBI taxon ID. run \code{\link{rba_panther_info}}
#'   with argument 'what = "organisms"' to get a list of PANTHER's
#'   supported organisms.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.pantherdb.org/services/oai/pantherdb/geneinfo"
#'
#' @return A list containing your unmapped inputs and mapped genes with
#'   pertinent information.
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_mapping(genes = c("Cd40", 7124, "ENSG00000203747", "P33681"),
#'     organism = 9606)
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_mapping <- function(genes,
                                organism,
                                ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "genes",
        class = c("character", "numeric", "integer"),
        min_len = 1L,
        max_len = 5000L
      ),
      list(
        arg = "organism", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      )
    )
  )

  .msg(
    "Mapping %s input genes from organism %s to the PANTHER database.",
    length(genes), organism
  )

  ## Build POST API Request's body
  call_body <- list(
    geneInputList =  paste(genes, collapse =  ","),
    organism = organism
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list",
    .rba_panther_check_response,
    function(x) {
      list(
        unmapped_list = x$search$unmapped_list,
        mapped_genes = x$search$mapped_genes
      )
    }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), "geneinfo"),
    encode = "form",
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_panther_mapping.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' PANTHER Over-Representation or Enrichment Analysis
#'
#' Use PANTHER services to perform over-representation enrichment analysis.
#'   You can either provide a character vector of gene IDs for
#'   over-representation analysis, or a data frame of gene IDs and expression
#'   analysis.\cr Please refer to the details section for more information on
#'   the statistical analysis.
#'
#' \strong{Over-representation Test}: It assesses whether specific gene sets are
#'   represented in your input gene list differently from what is expected by
#'   chance. It uses Fisher's exact test or Binomial test to calculate p-values.
#'   Fisher's exact test determines the probability of observing the gene
#'   counts in a category based on a hypergeometric distribution; the binomial
#'   test compares the observed proportion of genes in a category to the
#'   expected proportion based on the reference list. A significant p-value
#'   indicates over-representation or under-representation of a gene set.
#'
#' \strong{Statistical Enrichment Test}: The statistical enrichment test uses the
#'   Mann-Whitney U (Wilcoxon Rank-Sum) test to assess if the expression values
#'   associated with genes in a specific category differ significantly from the
#'   overall distribution in the input list. This non-parametric test first
#'   ranks the numerical values and computes whether the expression values
#'   were randomly drawn from the overall distribution of values. A small
#'   p-value indicates that the numerical values for the genes in the category
#'   are significantly different from the background distribution, thus
#'   non-random patterns.
#'
#' Please note that starting from rbioapi version 0.8.2, you can supply a
#'   gene expression data frame to perform statistical enrichment analysis.
#'   In earlier versions, only a character vector of gene IDs was possible,
#'   thus only over-representation analysis.
#'
#' @param genes Character or Data frame: A vector or data frame. Depending on
#'   this parameter, the analysis type is determined.
#'   \describe{
#'   \item{Character vector:}{If a character vector is supplied,
#'   over-representation analysis will be performed using either Fisher's exact
#'   test (default), or binomial.}
#'   \item{Data frame:}{If a data.frame is supplied, statistical enrichment
#'   test is performed using Mann-Whitney U (Wilcoxon Rank-Sum) test. The
#'   data frame should have two columns: the first column is a character vector
#'   with gene identifiers and the second column is a numerical vector with
#'   expression values.}
#'   }
#'   In both cases, a maximum of 100,000 identifiers can be supplied.
#'   The gene identifiers can be any of: Ensembl gene ID, Ensembl protein ID,
#'   Ensembl transcript ID, Entrez gene ID, gene symbol, NCBI GI, HGNC ID,
#'   International protein index ID, NCBI UniGene ID, UniProt accession
#'   or UniProt ID.
#' @param organism Numeric: NCBI taxon ID. run \code{\link{rba_panther_info}}
#'   with argument 'what = "organisms"' to get a list of PANTHER's
#'   supported organisms.
#' @param annot_dataset Character: A PANTHER dataset ID to test your input against it.
#'   run \code{\link{rba_panther_info}} with argument 'what = "datasets"' to
#'   get a list of PANTHER's supported datasets. Note that you should enter
#'   the "id" of the dataset, not its label (e.g. entering "biological_process"
#'   is incorrect, you should rather enter "GO:0008150").
#' @param test_type Character: (optional) Statistical test type used to calculate p-values.
#'   \itemize{
#'   \item If performing over-representation analysis (i.e. `genes` is a
#'   character vector), valid values are "FISHER" (default if NULL) or
#'   "BINOMIAL".
#'   \item If performing statistical enrichment analysis (i.e. `genes` is a
#'   data frame), the only valid value is "Mann-Whitney" (default if NULL).
#'   }
#' @param correction Character: (default = \code{"FDR"}) p value correction method. either "FDR" (default),
#'   "BONFERRONI" or "NONE".
#' @param cutoff Numeric: (optional) a threshold to filter the results.
#'   if correction is "FDR", the threshold will be applied to fdr column's
#'   values; if otherwise, the threshold will be applied to p value column.
#' @param ref_genes Character or Numeric: (optional) (only valid if genes is a character vector)
#'   A vector of genes that will be used as the test's
#'   background (reference/universe) gene set. If no value is supplied, all of
#'   the genes in the specified organism will be used. The maximum length and
#'   supported IDs are the same as the 'genes' argument.
#' @param ref_organism Numeric: (optional) (only valid if genes is a character vector)
#'   if 'ref_genes' is used, you can specify the organisms which correspond to
#'   your supplied IDs in 'ref_genes' argument. see 'organism' argument for
#'   supported values.
#' @param request_mapped_genes Character: (default = \code{"input"}) (only used if genes is a character
#'   vector, hence Over-representation test is requested) Which mapped genes
#'   should be returned for each result term. One of "input" (default),
#'   "reference", or "none". Requesting "reference" without supplying
#'   'ref_genes' may produce a large response because all genes in the
#'   specified organism are used as the reference list.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.pantherdb.org/services/oai/pantherdb/enrich/overrep"
#'  \cr "POST https://www.pantherdb.org/services/oai/pantherdb/enrich/statenrich"
#'
#' @return For a successful analysis, a list. The "result" element is a data
#'   frame with one row per returned annotation term and columns describing the
#'   term, observed counts, enrichment direction, and statistical significance.
#'   The remaining elements contain input and reference mapping summaries, when
#'   applicable, and PANTHER analysis and release metadata.
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item Mi H, Muruganujan A, Huang X, Ebert D, Mills C, Guo X, Thomas PD.
#'   (2019) Protocol Update for large-scale genome and gene function analysis
#'   with the PANTHER classification system (v.14.0). Nature Protocols, 14,
#'   703–721. https://doi.org/10.1038/s41596-019-0128-8
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_enrich(
#'   genes = c("TP53", "BRCA1", "CDK2", "Q99835", "CDC42"),
#'   organism = 9606, annot_dataset = "GO:0008150",
#'   cutoff = 0.01
#'   )
#' }
#'
#' \donttest{
#' expression_df <- data.frame(
#'   genes = c("TP53", "BRCA1", "CDK2", "CDC42", "CDK1"),
#'   expr = c(10, 8, 6, 4, 2)
#'   )
#'
#' rba_panther_enrich(
#'   genes = expression_df,
#'   organism = 9606,
#'   annot_dataset = "GO:0008150"
#'   )
#' }
#'
#' @family "PANTHER"
#' @family "Enrichment/Over-representation"
#' @export
rba_panther_enrich <- function(genes,
                               organism,
                               annot_dataset,
                               test_type = NULL,
                               correction = "FDR",
                               cutoff = NULL,
                               ref_genes = NULL,
                               ref_organism = NULL,
                               request_mapped_genes = "input",
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "genes", class = c("character", "data.frame"),
        max_len = 100000L
      ),
      list(
        arg = "organism", class = "numeric", len = 1,
        integerish = TRUE, min_val = 1
      ),
      list(arg = "annot_dataset", class = "character", len = 1),
      list(
        arg = "test_type", class = "character",  len = 1,
        val = c("FISHER", "BINOMIAL", "Mann-Whitney")
      ),
      list(
        arg = "correction", class = "character", len = 1,
        val =  c("FDR", "BONFERRONI", "NONE")
      ),
      list(arg = "cutoff", class = "numeric", len = 1, ran = c(0, 1)),
      list(arg = "ref_genes", class = c("character","numeric"), max_len = 100000L),
      list(
        arg = "ref_organism", class = "numeric", len = 1,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "request_mapped_genes",
        class = "character", len = 1, no_null = TRUE,
        val = c("input", "reference", "none")
      )
    ),
    cond = list(
      list(
        quote(xor(is.null(ref_organism), is.null(ref_genes))),
        "'ref_organism' and 'ref_genes' should be supplied together."
      ),
      list(
        quote(is.data.frame(genes) && (ncol(genes) != 2 || !inherits(genes[[1]], "character") || !is.numeric(genes[[2]]))),
        "If the `genes` parameter is a data frame, statistical enrichment analysis will be performed.\nThe gene parameter should be a data frame with 2 columns, where the first column contains the genes identifiers and the second column contains numerical expression values."
      ),
      list(
        quote(is.data.frame(genes) && nrow(genes) > 100000L),
        "The `genes` data frame can contain at most 100,000 identifiers."
      ),
      list(
        quote(is.data.frame(genes) && !is.null(test_type) && test_type != "Mann-Whitney"),
        "If `genes` is a data frame, statistical enrichment analysis will be performed.\nThus, the only valid value for `test_type` is 'Mann-Whitney'."
      ),
      list(
        quote(is.character(genes) && !is.null(test_type) && test_type == "Mann-Whitney"),
        "If `genes` is a character vector, over-representation analysis will be performed.\nThus, valid values for `test_type` are 'FISHER' and 'BINOMIAL'."
      ),
      list(
        quote(is.data.frame(genes) && any(!is.null(ref_genes), !is.null(ref_organism))),
        "If the `genes` parameter is a data frame, statistical enrichment analysis will be performed.\nProviding Reference gene list (`ref_genes` and `ref_organism`) is not possible in this mode."
      )
    )
  )

  if (is.character(genes)) {

    if (is.null(test_type)) { test_type = "FISHER" }
    # Over-representation analysis
    .msg(
      "Performing PANTHER over-representation analysis (%s test) on %s genes from `organism %s` against `%s` datasets.",
      switch(test_type, "FISHER" = "Fisher's exact", "BINOMIAL" = "Binomial"),
      length(genes), organism, annot_dataset
    )
    path_input <- "enrich/overrep"
    encode_input <- "form"

    mapped_info <- switch(
      request_mapped_genes,
      input = "COMP_LIST",
      reference = "REF_LIST",
      none = "NONE"
    )

    ## Build POST API Request's body
    call_body <- .rba_query(
      init = list(
        geneInputList =  paste(genes, collapse =  ","),
        organism = organism,
        annotDataSet = annot_dataset,
        enrichmentTestType = test_type,
        correction = correction,
        mappedInfo = mapped_info
      ),
      list("refInputList", !all(is.null(ref_genes)), paste(ref_genes, collapse =  ",")),
      list("refOrganism", !is.null(ref_organism), ref_organism)
    )

  } else {

    # Enrichment analysis
    .msg(
      "Performing PANTHER statistical enrichment analysis (Mann-Whitney U Test) on %s genes and expression values from `organism %s` against `%s` datasets.",
      nrow(genes), organism, annot_dataset
    )
    path_input <- "enrich/statenrich"
    encode_input <- "multipart"

    ## Build POST API Request's body
    temp_file <- tempfile(pattern = "rba_", fileext = ".txt")
    on.exit(unlink(temp_file), add = TRUE)

    utils::write.table(
      x = genes,
      file = temp_file,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )

    call_body <- list(
      organism = organism,
      annotDataSet = annot_dataset,
      correction = correction,
      geneExp = httr::upload_file(temp_file)
    )

  }

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list_simp",
    .rba_panther_check_response,
    function(x) {
      if (utils::hasName(x, "results")) {
        x <- x$results
        x$result <- jsonlite::flatten(
          .rba_panther_data_frame(x$result)
        )

        if (!is.null(cutoff) && nrow(x$result) > 0L) {
          if (correction == "FDR") {
            x$result <- x$result[x$result$fdr <= cutoff, ]
          } else {
            x$result <- x$result[x$result$pValue <= cutoff, ]
          }
        }
      }

      return(x)
    }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), path_input),
    encode = encode_input,
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_panther_enrich.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get PANTHER database Information
#'
#' Using this function you can retrieve a list of available organisms,
#'   annotation datasets, families, and pathways which are supported in
#'   PANTHER.
#'
#' @param what Character: what information to retrieve? should be one of: \itemize{
#' \item "organisms": Retrieve supported organisms in PANTHER.
#' \item "datasets": Retrieve available annotation datasets.
#' \item "families": Retrieve available family IDs.
#' \item "species_tree": Retrieve PANTHER's species tree.
#' \item "pathways" Retrieve available pathway IDs.}
#' @param organism_chr_loc Logical: (default = \code{FALSE}) (only when 'what = "organisms"')
#'   If TRUE, only organisms with chromosome location will be returned.
#'   If FALSE (default), all organisms will be returned.
#' @param families_page Numeric: (default = \code{1}) (only when 'what = "families"')
#'   Family information is very long, so results are returned in pages of up
#'   to 1,000 families. Use a positive whole number to define the page to
#'   retrieve.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.pantherdb.org/services/oai/pantherdb/supportedgenomes"
#'  \cr "GET https://www.pantherdb.org/services/oai/pantherdb/supportedannotdatasets"
#'  \cr "GET https://www.pantherdb.org/services/oai/pantherdb/supportedpantherfamilies"
#'  \cr "GET https://www.pantherdb.org/services/oai/pantherdb/supportedpantherpathways"
#'  \cr "GET https://www.pantherdb.org/services/oai/pantherdb/speciestree"
#'
#' @return For families, a list containing family information, the requested
#'   page, and the total number of pages. For the species tree, a list; otherwise
#'   a data frame with pertinent information.
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_info(what = "organisms")
#' }
#' \donttest{
#' rba_panther_info(what = "families", families_page = 4)
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_info <- function(what,
                             organism_chr_loc = FALSE,
                             families_page = 1,
                             ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "what", class = "character", len = 1L,
        val = c("organisms", "datasets", "families", "species_tree", "pathways")
      ),
      list(arg = "organism_chr_loc", class = "logical", len = 1),
      list(
        arg = "families_page",
        class = c("numeric", "integer"),
        len = 1L,
        integerish = TRUE,
        min_val = 1,
        no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(families_page != 1 && what != "families"),
        "'families_page' was ignored because 'what' argument is not 'families'.",
        warn = TRUE
      ),
      list(
        quote(isTRUE(organism_chr_loc) && what != "organisms"),
        "'organism_chr_loc' was ignored because 'what' argument is not 'organisms'.",
        warn = TRUE
      )
    )
  )

  .msg(
    "Retrieving %s%s.",
    switch(
      what,
      "organisms" = "supported organisms in PANTHER",
      "datasets" = "available annotation datasets",
      "families" = "available family IDs",
      "species_tree" = "phylogenetic tree of PANTHER species",
      "pathways" = "available pathway IDs"
    ),
    ifelse(what == "families", yes = sprintf(" (page %s)", families_page), no = "")
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("type", what == "organisms" && isTRUE(organism_chr_loc), "chrLoc"),
    list("startIndex", what == "families", (families_page - 1) * 1000 + 1)
  )

  ## Build Function-Specific Call
  switch(
    what,
    "organisms" = {
      path_input <- "supportedgenomes"
      parser_input <- list(
        "json->list_simp",
        .rba_panther_check_response,
        function(x) { .rba_panther_data_frame(x$search$output$genomes$genome) }
      )
    },
    "datasets" = {
      path_input <- "supportedannotdatasets"
      parser_input <- list(
        "json->list_simp",
        .rba_panther_check_response,
        function(x) {
          .rba_panther_data_frame(x$search$annotation_data_sets$annotation_data_type)
        }
      )
    },
    "families" = {
      path_input <- "supportedpantherfamilies"
      parser_input <- list(
        "json->list_simp",
        .rba_panther_check_response,
        function(x) {
          pages_count <- ceiling(x$search$number_of_families / 1000)

          if (families_page > pages_count) {
            return(.rba_api_error(sprintf(
              "Requested family page %s exceeds the available %s pages.",
              families_page,
              pages_count
            )))
          }

          list(
            family = .rba_panther_data_frame(x$search$panther_family_subfam_list$family),
            page = families_page,
            pages_count = pages_count
          )
        }
      )
    },
    "species_tree" = {
      path_input <- "speciestree"
      parser_input <- list(
        "json->list",
        .rba_panther_check_response,
        function(x) { x$species_tree }
      )
    },
    "pathways" = {
      path_input <- "supportedpantherpathways"
      parser_input <- list(
        "json->list_simp",
        .rba_panther_check_response,
        function(x) {
          .rba_panther_data_frame(x$search$output$PANTHER_pathway_list$pathway)
        }
      )
    }
  )


  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), path_input),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("panther_info.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Retrieve Genes from a PANTHER Genome
#'
#' Retrieve one page of genes and their associated information for a genome
#'   supported by PANTHER. Each page contains up to 1,000 genes.
#'
#' @param organism Numeric: NCBI taxon ID. Run
#'   \code{\link{rba_panther_info}} with argument 'what = "organisms"' to get
#'   a list of PANTHER's supported organisms.
#' @param page Numeric: The results page to retrieve. Pages contain up to
#'   1,000 genes and are numbered starting from 1.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.pantherdb.org/services/oai/pantherdb/downloadgenome"
#'
#' @return A list with the following elements: \describe{
#'   \item{gene}{A data frame with one row per returned gene. Fields are kept
#'   as returned by PANTHER, with annotation information in nested columns.}
#'   \item{page}{The retrieved page.}
#'   \item{pages_count}{The total number of available pages.}
#'   \item{number_of_genes_in_genome}{The total number of genes in the genome.}
#'   \item{product}{PANTHER product source and version information.}
#'   \item{search_type}{The search type reported by PANTHER.}
#'   }
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_genome(organism = 243273, page = 1)
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_genome <- function(organism,
                               page,
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "organism",
        class = c("numeric", "integer"),
        len = 1L,
        integerish = TRUE,
        min_val = 1
      ),
      list(
        arg = "page",
        class = c("numeric", "integer"),
        len = 1L,
        integerish = TRUE,
        min_val = 1,
        no_null = TRUE
      )
    )
  )

  .msg(
    "Retrieving page %s of genes for PANTHER organism %s.",
    page,
    organism
  )

  ## Build POST API Request's query
  call_query <- list(
    organism = organism,
    startIndex = (page - 1L) * 1000L + 1L
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list_simp",
    .rba_panther_check_response,
    function(x) {
      genes_count <- x$search$number_of_genes_in_genome
      pages_count <- ceiling(genes_count / 1000)

      if (page > pages_count) {
        return(.rba_api_error(sprintf(
          "Requested genome page %s exceeds the available %s pages.",
          page,
          pages_count
        )))
      }

      list(
        gene = .rba_panther_data_frame(x$search$gene_list, "gene"),
        page = page,
        pages_count = pages_count,
        number_of_genes_in_genome = genes_count,
        product = x$search$product,
        search_type = x$search$search_type
      )
    }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), "downloadgenome"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_panther_genome.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search PANTHER for Orthologs of Gene(s)
#'
#' Using this function you can search and retrieve orthologs of given gene(s),
#'   and optionally return the corresponding position in the target organisms'
#'   protein sequences.
#'
#' @param genes Character or Numeric: A vector of gene identifiers with maximum
#'   length of 10, or only one if \code{seq_pos} is supplied. Can be any of: Ensembl
#'   gene ID, Ensembl protein ID, Ensembl transcript ID, Entrez gene ID, gene
#'   symbol, NCBI GI, HGNC ID, International protein index ID, NCBI UniGene ID,
#'   UniProt accession and/or UniProt ID.
#' @param organism Numeric: NCBI taxon ID of the organism of your supplied
#'   genes. run \code{\link{rba_panther_info}} with argument
#'   'what = "organisms"' to get a list of PANTHER's supported organisms.
#' @param type Character: (default = \code{"all"}) Ortholog types to return. either "all" (default) or "LDO" to
#'   only return least diverged orthologs.
#' @param target_organisms Numeric: (optional) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#' @param seq_pos Numeric: (optional) A position in the protein's sequence of the
#'   supplied gene. should be in the range of the protein's length.
#' @param include_msa Logical: (optional) Only if a sequence position is supplied,
#'   should MSA (Multiple Sequence Alignment) information be included in the
#'   results?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.pantherdb.org/services/oai/pantherdb/ortholog/matchortho"
#'  \cr "POST https://www.pantherdb.org/services/oai/pantherdb/ortholog/homologpos"
#'
#' @return A data frame with Orthologs information.
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_ortholog("CD40", organism = 9606, type = "LDO")
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_ortholog <- function(genes,
                                 organism,
                                 type = "all",
                                 target_organisms = NULL,
                                 seq_pos = NULL,
                                 include_msa = NULL,
                                 ...) {

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "genes", class = c("character", "numeric"), max_len = 10),
      list(
        arg = "organism", class = "numeric", len = 1,
        integerish = TRUE, min_val = 1
      ),
      list(arg = "type", class = "character", val = c("LDO", "all"), len = 1),
      list(
        arg = "target_organisms", class = "numeric",
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "seq_pos", class = "numeric", len = 1,
        integerish = TRUE, min_val = 1
      ),
      list(arg = "include_msa", class = "logical", len = 1)
    ),
    cond = list(
      list(
        quote(!is.null(seq_pos) && length(genes) > 1),
        "When 'seq_pos' is supplied, 'genes' argument should be a single input."
      ),
      list(
        quote(!is.null(include_msa) && is.null(seq_pos)),
        "'include_msa' was ignored because no 'seq_pos' was supplied.",
        warn = TRUE
      )
    )
  )

  .msg(
    "Retrieving %s orthologs of genes %s.",
    type,
    .paste2(genes, quote_all = "'")
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(organism = organism, orthologType = type),
    list("geneInputList", is.null(seq_pos), paste(genes, collapse =  ",")),
    list("gene", !is.null(seq_pos), genes),
    list("targetOrganism", !is.null(target_organisms), paste(target_organisms, collapse =  ",")),
    list("pos", !is.null(seq_pos), seq_pos),
    list(
      "includeMsa",
      !is.null(include_msa) && !is.null(seq_pos),
      ifelse(isTRUE(include_msa), yes = "true", no = "false")
    )
  )

  ## Build Function-Specific Call
  if (is.null(seq_pos)) {
    path_input <- "matchortho"
  } else {
    path_input <- "homologpos"
  }

  parser_input <- list(
    "json->list_simp",
    .rba_panther_check_response,
    function(x) { .rba_panther_data_frame(x$search$mapping$mapped) }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), "ortholog/", path_input),
    encode = "form",
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_panther_ortholog.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search PANTHER for Homologs of Gene(s)
#'
#' Using this function you can search and retrieve homolog of given gene(s).
#'
#' @param genes Character or Numeric: A vector of gene identifiers with maximum
#'   length of 10. Can be any of: Ensembl gene ID, Ensembl protein ID, Ensembl
#'   transcript ID, Entrez gene ID, gene symbol, NCBI GI, HGNC ID,
#'   International protein index ID, NCBI UniGene ID, UniProt accession and/or
#'   UniProt ID.
#' @param organism Numeric: NCBI taxon ID of the organism of your supplied
#'   genes. run \code{\link{rba_panther_info}} with argument
#'   'what = "organisms"' to get a list of PANTHER's supported organisms.
#' @param type Character: (default = \code{"P"}) Homolog types to return. either "P" (default) for paralogs,
#'   "X" for horizontal gene transfer and "LDX" for least diverged horizontal
#'   gene transfer.
#' @param target_organisms Numeric: (optional) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#'   This argument is ignored for paralogs, which are searched within the input
#'   organism. For horizontal gene transfers, target organisms should differ
#'   from the input organism.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.pantherdb.org/services/oai/pantherdb/ortholog/homologOther"
#'
#' @return A data frame with homolog information.
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_homolog("OR4F5", organism = 9606, type = "P")
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_homolog <- function(genes,
                                organism,
                                type = "P",
                                target_organisms = NULL,
                                ...) {

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "genes", class = c("character", "numeric"), max_len = 10),
      list(
        arg = "organism", class = "numeric", len = 1,
        integerish = TRUE, min_val = 1
      ),
      list(arg = "type", class = "character", val = c("P", "X", "LDX"), len = 1),
      list(
        arg = "target_organisms", class = "numeric",
        integerish = TRUE, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(type == "P" && !is.null(target_organisms)),
        "For paralogs, 'target_organisms' was ignored because PANTHER searches within the input organism.",
        warn = TRUE
      ),
      list(
        quote(type != "P" && !is.null(target_organisms) && organism %in% target_organisms),
        "For horizontal gene transfers or least diverged horizontal gene transfers, the target organism should be different from the input organism"
      )
    )
  )

  .msg(
    "Retrieving %s homologs of genes %s.",
    type,.paste2(genes, quote_all = "'")
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      geneInputList = paste(genes, collapse =  ","),
      organism = organism,
      homologType = type
    ),
    list(
      "targetOrganism",
      type != "P" && !is.null(target_organisms),
      paste(target_organisms, collapse =  ",")
    )
  )


  ## Build Function-Specific Call
  parser_input <- list(
    "json->list_simp",
    .rba_panther_check_response,
    function(x) { .rba_panther_data_frame(x$search$mapping$mapped) }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), "ortholog/homologOther"),
    encode = "form",
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_panther_homolog.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get PANTHER Families and Sub-Families
#'
#' Using this function, you can retrieve Orthologs, MSA or Tree topology
#'   information of a given PANTHER family.
#'
#' @param id Character: Panther family id.
#' @param what Character: What to retrieve? One of: \itemize{
#' \item "ortholog": Orthologs ('LDO' for least diverged and 'O' for more
#'   diverged).
#' \item "msa": Multiple Sequence Alignment Information,
#' \item "tree": Tree topology and nodes attributes.
#' }
#' @param target_organisms Numeric: (optional) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.pantherdb.org/services/oai/pantherdb/familyortholog"
#'  \cr "POST https://www.pantherdb.org/services/oai/pantherdb/familymsa"
#'  \cr "POST https://www.pantherdb.org/services/oai/pantherdb/treeinfo"
#'
#' @return For trees a list and otherwise a data frame with the requested
#'   family's information.
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_family("PTHR10000", what = "ortholog")
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_family <- function(id,
                               what,
                               target_organisms = NULL,
                               ...) {

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "id", class = "character", len = 1),
      list(
        arg = "what", class = "character", len = 1,
        val = c("ortholog", "msa", "tree")
      ),
      list(
        arg = "target_organisms", class = "numeric",
        integerish = TRUE, min_val = 1
      )
    )
  )

  .msg(
    "Retrieving %s information of PANTHER family %s.",
    what, id
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(family = id),
    list("taxonFltr", !is.null(target_organisms), paste(target_organisms, collapse =  ","))
  )

  ## Build Function-Specific Call
  switch(
    what,
    "ortholog" = {
      path_input <- "familyortholog"
      parser_input <- list(
        "json->list_simp",
        .rba_panther_check_response,
        function(x) {
          .rba_panther_data_frame(x$search$ortholog_list, "ortholog")
        }
      )
    },
    "msa" = {
      path_input <- "familymsa"
      parser_input <- list(
        "json->list_simp",
        .rba_panther_check_response,
        function(x) {
          .rba_panther_data_frame(x$search$MSA_list, "sequence_info")
        }
      )
    },
    "tree" = {
      path_input <- "treeinfo"
      parser_input <- list(
        "json->list_simp",
        .rba_panther_check_response,
        function(x) { x$search$tree_topology }
      )
    }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), path_input),
    encode = "form",
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_panther_family.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' PANTHER Tree Grafter
#'
#' Use this function to retrieve a PANTHER family's tree topology information
#'   with a node corresponding to your sequence grafted in the best location
#'   in that tree.
#'
#' For more information, see:
#'   Haiming Tang, Robert D Finn, Paul D Thomas, TreeGrafter: phylogenetic
#'   tree-based annotation of proteins with Gene Ontology terms and other
#'   annotations, Bioinformatics, Volume 35, Issue 3, February 2019, Pages
#'   518–520, \doi{10.1093/bioinformatics/bty625}
#'
#' @param protein_seq Character: A string with the protein's sequence. Maximum
#'   allowed sequence length is 50,000 characters.
#' @param target_organisms Numeric: (optional) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.pantherdb.org/services/oai/pantherdb/graftsequence"
#'
#' @return A list containing PANTHER tree topology information.
#'
#' @references \itemize{
#'   \item Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
#'   Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to all.
#'   Protein Science, 31(1), 8–22.
#'   https://doi.org/10.1002/pro.4218
#'   \item \href{https://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   \item
#'   \href{https://www.pantherdb.org/publications.jsp#HowToCitePANTHER}{Citations
#'   note on PANTHER website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_tree_grafter("MKVLWAALLVTFLAGCQAKVEQAVETE")
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_tree_grafter <- function(protein_seq,
                                     target_organisms = NULL,
                                     ...) {

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "protein_seq", class = "character", len = 1),
      list(
        arg = "target_organisms", class = "numeric",
        integerish = TRUE, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(nchar(protein_seq) > 50000L),
        "Maximum allowed length of protein sequence is 50,000 characters.")
    )
  )

  .msg(
    "Retrieving a PANTHER family tree with your input protein grafted in it."
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(sequence  = protein_seq),
    list("taxonFltr", !is.null(target_organisms), paste(target_organisms, collapse =  ","))
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list_simp",
    .rba_panther_check_response,
    function(x) { x$search }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("panther", "url"),
    path = paste0(.rba_stg("panther", "pth"), "graftsequence"),
    encode = "form",
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_panther_tree_grafter.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
