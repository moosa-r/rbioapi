#' Map A Gene-set to PANTHER Database
#'
#' Using this function, you can search your genes in PANTHER database and
#'   retrieve attributes and annotations associated to your genes.
#'
#' @param genes Character vector of genes identifiers with maximum length of
#'   1000. Can be any of: Ensemble gene ID, Ensemble protein ID, Ensemble
#'   transcript ID, Entrez gene ID, gene symbol, NCBI GI, HGNC ID,
#'   International protein index ID, NCBI UniGene ID, UniProt accession
#'   and/or UniProt ID.
#' @param organism (numeric) NCBI taxon ID. run \code{\link{rba_panther_info}}
#'   with argument 'what = "organisms"' to get a list of PANTHER's
#'   supported organisms.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET http://www.pantherdb.org/services/oai/pantherdb/geneinfo"
#'
#' @return A list containing your unmapped inputs and mapped genes with
#'   pertinent information.
#'
#' @references \itemize{
#'   \item Mi, H., Muruganujan, A., Ebert, D., Huang, X., & Thomas, P. D.
#'   (2019). PANTHER version 14: more genomes, a new PANTHER GO-slim and
#'   improvements in enrichment analysis tools. Nucleic acids research, 47(D1),
#'   D419-D426.
#'   \item Mi, H., Muruganujan, A., Huang, X., Ebert, D., Mills, C., Guo, X.,
#'   & Thomas, P. D. (2019). Protocol Update for large-scale genome and gene
#'   function analysis with the PANTHER classification system (v. 14.0).
#'   Nature protocols, 14(3), 703-721.
#'   \item \href{http://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
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
  .rba_args(cons = list(list(arg = "genes",
                             class = c("character",
                                       "numeric"),
                             max_len = 1000),
                        list(arg = "organism",
                             class = "numeric",
                             len = 1)))
  .msg("Mapping %s input genes from organims %s to PANTHER databse.",
       length(genes), organism)

  ## Build POST API Request's body
  call_body <- list(geneInputList =  paste(genes, collapse =  ","),
                    organism = organism)

  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       function(x) {list(unmapped_list = x$search$unmapped_list,
                                         mapped_genes = x$search$mapped_genes)})
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("panther", "url"),
                          path = paste0(.rba_stg("panther", "pth"),
                                        "geneinfo"),
                          encode = "form",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_panther_mapping.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' PANTHER Over-Representation Enrichment Analysis
#'
#' Using this function you can use PANTHER services to perform
#'   over-representation enrichment analysis. This statistical test will
#'   compare your input genes to a set of defined gene lists to determine
#'   if they are over/under-represented.
#'
#' @param genes Character vector of genes identifiers with maximum length of
#'   10000. Can be any of: Ensemble gene ID, Ensemble protein ID, Ensemble
#'   transcript ID, Entrez gene ID, gene symbol, NCBI GI, HGNC ID,
#'   International protein index ID, NCBI UniGene ID, UniProt accession
#'   and/or UniProt ID.
#' @param organism (numeric) NCBI taxon ID. run \code{\link{rba_panther_info}}
#'   with argument 'what = "organisms"' to get a list of PANTHER's
#'   supported organisms.
#' @param annot_dataset A PANTHER dataset ID to test your input against it.
#'   run \code{\link{rba_panther_info}}with argument 'what = "datasets"' to
#'   get a list of PANTHER's supported datasets.
#' @param test_type statistical test type to calculate the p values. either
#'   "FISHER" (default) or "BINOMIAL".
#' @param correction p value correction method. either "FDR" (default),
#'   "BONFERRONI" or "NONE".
#' @param cutoff (Numeric) (Optional) a threshold to filter the results.
#'   if correction is "FDR", the threshold will be applied to fdr column's
#'   values; if otherwise, the threshold will be applied to p value column.
#' @param ref_genes (Optional) A set of genes that will be used as the test's
#'   background (reference/universe) gene set. If no value provided, all of
#'   the genes in specified organism will be used. maximum length and supported
#'   IDs are the same as 'genes' argument.
#' @param ref_organism (Optional) if 'ref_genes' is used, you can specify
#'   the organisms which correspond to your provided IDs in 'ref_genes'
#'   argument. see 'organism' argument for supported values.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST http://www.pantherdb.org/services/oai/pantherdb/enrich/overrep"
#'
#' @return A list with the prov
#'
#' @references \itemize{
#'   \item Mi, H., Muruganujan, A., Ebert, D., Huang, X., & Thomas, P. D.
#'   (2019). PANTHER version 14: more genomes, a new PANTHER GO-slim and
#'   improvements in enrichment analysis tools. Nucleic acids research, 47(D1),
#'   D419-D426.
#'   \item Mi, H., Muruganujan, A., Huang, X., Ebert, D., Mills, C., Guo, X.,
#'   & Thomas, P. D. (2019). Protocol Update for large-scale genome and gene
#'   function analysis with the PANTHER classification system (v. 14.0).
#'   Nature protocols, 14(3), 703-721.
#'   \item \href{http://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
#'   }
#'
#' @examples
#' \donttest{
#' rba_panther_enrich(genes = c("p53", "BRCA1", "cdk2", "Q99835", "CDC42",
#'         "CDK1", "KIF23", "PLK1", "RAC2", "RACGAP1"),
#'     organism = 9606, annot_dataset = "GO:0008150",
#'     cutoff = 0.01)
#' }
#'
#' @family "PANTHER"
#' @export
rba_panther_enrich <- function(genes,
                               organism,
                               annot_dataset,
                               test_type = "FISHER",
                               correction = "FDR",
                               cutoff = NULL,
                               ref_genes = NULL,
                               ref_organism = NULL,
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "genes",
                             class = c("character",
                                       "numeric"),
                             max_len = 100000),
                        list(arg = "organism",
                             class = "numeric",
                             len = 1),
                        list(arg = "annot_dataset",
                             class = "character",
                             len = 1),
                        list(arg = "test_type",
                             class = "character",
                             val = c("FISHER", "BINOMIAL"),
                             len = 1),
                        list(arg = "correction",
                             class = "character",
                             val =  c("FDR", "BONFERRONI", "NONE"),
                             len = 1),
                        list(arg = "cutoff",
                             class = "numeric",
                             len = 1,
                             ran = c(0, 1)),
                        list(arg = "ref_genes",
                             class = c("character",
                                       "numeric"),
                             max_len = 100000),
                        list(arg = "ref_organism",
                             class = "numeric",
                             len = 1)),
            cond = list(list(quote(!is.null(ref_organism) && is.null(ref_genes)),
                             "'ref_organism' was ignored because no 'ref_genes' was provided.")),
            cond_warning = TRUE)
  .msg("Performing over-representation enrichment analysis of %s input genes of organism %s against %s datasets.",
       length(genes), organism, annot_dataset)

  ## Build POST API Request's body
  call_body <- .rba_query(init = list(geneInputList =  paste(genes,
                                                             collapse =  ","),
                                      organism = organism,
                                      annotDataSet = annot_dataset,
                                      enrichmentTestType = test_type,
                                      correction = correction),
                          list("refInputList",
                               !all(is.null(ref_genes)),
                               paste(ref_genes, collapse =  ",")),
                          list("refOrganism",
                               !is.null(ref_organism),
                               ref_organism))

  ## Build Function-Specific Call
  parser_input <- list("json->list_simp",
                       function(x) {
                         x <- x$results
                         x$result$term_label <- x$result$term[, 1]
                         x$result$term <- x$result$term[, 2]
                         return(x)
                       })
  if (!is.null(cutoff)) {
    if (correction == "FDR") {
      parser_input <- append(parser_input,
                             list(function(x) {
                               x$result <- x$result[which(x$result$fdr <= cutoff), ]
                               return(x)
                             }))
    } else {
      parser_input <- append(parser_input,
                             list(function(x) {
                               x$result <- x$result[which(x$result$pValue <= cutoff), ]
                               return(x)
                             }))
    }
  }

  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("panther", "url"),
                          path = paste0(.rba_stg("panther", "pth"),
                                        "enrich/overrep"),
                          encode = "form",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_panther_enrich.json"))

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
#' @param what what information to retrieve? should be one of: \itemize{
#' \item "organisms": Retrieve supported organisms in PANTHER.
#' \item "datasets": Retrieve available annotation datasets.
#' \item "families" Retrieve available family IDs.
#' \item "pathways" Retrieve available pathway IDs.}
#' @param organism_chr_loc (Logical) (only when 'what = "organisms"')
#'   If TRUE, only organisms with chromosome location will be returned.
#'   If FALSE (default) every organisms will be returned.
#' @param families_page (Numeric) (only when 'what = "families"')
#'   Family information is very long, so results are paginated. Use this
#'   argument to define the page to retrieve.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET http://www.pantherdb.org/services/oai/pantherdb/supportedgenomes"
#'  \cr "GET http://www.pantherdb.org/services/oai/pantherdb/supportedannotdatasets"
#'  \cr "GET http://www.pantherdb.org/services/oai/pantherdb/supportedpantherfamilies"
#'  \cr "GET http://www.pantherdb.org/services/oai/pantherdb/supportedpantherpathways"
#'
#' @return For families, a list and otherwise a data frame with pertinent
#'   information.
#'
#' @references \itemize{
#'   \item Mi, H., Muruganujan, A., Ebert, D., Huang, X., & Thomas, P. D.
#'   (2019). PANTHER version 14: more genomes, a new PANTHER GO-slim and
#'   improvements in enrichment analysis tools. Nucleic acids research, 47(D1),
#'   D419-D426.
#'   \item Mi, H., Muruganujan, A., Huang, X., Ebert, D., Mills, C., Guo, X.,
#'   & Thomas, P. D. (2019). Protocol Update for large-scale genome and gene
#'   function analysis with the PANTHER classification system (v. 14.0).
#'   Nature protocols, 14(3), 703-721.
#'   \item \href{http://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
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
  .rba_args(cons = list(list(arg = "what",
                             class = "character",
                             val = c("organisms",
                                     "datasets",
                                     "families",
                                     "pathways")),
                        list(arg = "organism_chr_loc",
                             class = "logical",
                             len = 1),
                        list(arg = "families_page",
                             class = "numeric",
                             len = 1)),
            cond = list(list(quote(families_page != 1 && what != "families"),
                             "'families_page' was ignored because 'what' argument is not 'families'.",
                             warn = TRUE),
                        list(quote(isTRUE(organism_chr_loc)),
                             "'organism_chr_loc' was ignored because 'what' argument is not 'organisms'.",
                             warn = TRUE)))
  .msg("Retrieving %s%s.",
       switch(what,
              "organisms" = "supported organisms in PANTHER",
              "datasets" = "available annotation datasets",
              "families" = "available family IDs",
              "pathways" = "available pathway IDs"),
       ifelse(what == "families",
              yes = sprintf(" (page %s)", families_page),
              no = ""))

  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("type",
                                what == "organisms" && isTRUE(organism_chr_loc),
                                "chrLoc"),
                           list("startIndex",
                                what == "families",
                                (families_page - 1) * 1000 + 1))

  ## Build Function-Specific Call
  switch(what,
         "organisms" = {
           path_input <- "supportedgenomes"
           parser_input <- list("json->list_simp",
                                function(x) {x$search$output$genomes$genome})
         },
         "datasets" = {
           path_input <- "supportedannotdatasets"
           parser_input <- list("json->list_simp",
                                function(x) {x$search$annotation_data_sets$annotation_data_type})
         },
         "families" = {
           path_input <- "supportedpantherfamilies"
           parser_input <- list("json->list_simp",
                                function(x) {
                                  y <- list(familiy = x$search$panther_family_subfam_list$family,
                                            page = families_page,
                                            pages_count = x$search$number_of_families %/% 1000
                                  )
                                })
         },
         "pathways" = {
           path_input <- "supportedpantherpathways"
           parser_input <- list("json->list_simp",
                                function(x) {
                                  x$search$output$PANTHER_pathway_list$pathway
                                })
         })


  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("panther", "url"),
                          path = paste0(.rba_stg("panther", "pth"),
                                        path_input),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("panther_info.json"))

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
#' @param genes Character vector of genes identifiers with maximum length of
#'   10 or only one if seq_pos is provided. Can be any of: Ensemble gene ID,
#'   Ensemble protein ID, Ensemble transcript ID, Entrez gene ID, gene symbol,
#'   NCBI GI, HGNC ID, International protein index ID, NCBI UniGene ID,
#'   UniProt accession and/or UniProt ID.
#' @param organism (numeric) NCBI taxon ID of the organism of your provided
#'   genes. run \code{\link{rba_panther_info}} with argument
#'   'what = "organisms"' to get a list of PANTHER's supported organisms.
#' @param type Ortholog types to return. either "all" (default) or "LDO" to
#'   only return least diverged orthologs.
#' @param target_organisms (numeric) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#' @param seq_pos (Numeric) A position in the protein's sequence of the
#'   provided gene. should be in the range of the protein's length.
#' @param include_msa (Logical) Only if a sequence position is provided,
#'   should MSA (Multiple Sequence Alignment) information be included in the
#'   results?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST http://www.pantherdb.org/services/oai/pantherdb/ortholog/matchortho"
#'  \cr "POST http://www.pantherdb.org/services/oai/pantherdb/ortholog/homologpos"
#'
#' @return A data frame with Orthologs information.
#'
#' @references \itemize{
#'   \item Mi, H., Muruganujan, A., Ebert, D., Huang, X., & Thomas, P. D.
#'   (2019). PANTHER version 14: more genomes, a new PANTHER GO-slim and
#'   improvements in enrichment analysis tools. Nucleic acids research, 47(D1),
#'   D419-D426.
#'   \item Mi, H., Muruganujan, A., Huang, X., Ebert, D., Mills, C., Guo, X.,
#'   & Thomas, P. D. (2019). Protocol Update for large-scale genome and gene
#'   function analysis with the PANTHER classification system (v. 14.0).
#'   Nature protocols, 14(3), 703-721.
#'   \item \href{http://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
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
  .rba_args(cons = list(list(arg = "genes",
                             class = c("character",
                                       "numeric"),
                             max_len = 10),
                        list(arg = "organism",
                             class = "numeric",
                             len = 1),
                        list(arg = "type",
                             class = "character",
                             val = c("LDO",
                                     "all"),
                             len = 1),
                        list(arg = "target_organisms",
                             class = "numeric"),
                        list(arg = "seq_pos",
                             class = "numeric",
                             len = 1),
                        list(arg = "include_msa",
                             class = "logical",
                             len = 1)),
            cond = list(list(quote(!is.null(seq_pos) && length(genes) > 1),
                             "When 'seq_pos' is provided, 'genes' argument should be a single input."),
                        list(quote(!is.null(include_msa) && is.null(seq_pos)),
                             "'include_msa' was ignored because no 'seq_pos' was provided.",
                             warn = TRUE)))
  .msg("Retrieving %s orthologs of genes %s.",
       type, .paste2(genes, quote_all = "'"))

  ## Build POST API Request's body
  call_body <- .rba_query(init = list(organism = organism,
                                      orthologType = type),
                          list("geneInputList",
                               is.null(seq_pos),
                               paste(genes, collapse =  ",")),
                          list("gene",
                               !is.null(seq_pos),
                               genes),
                          list("targetOrganism",
                               !is.null(target_organisms),
                               paste(target_organisms, collapse =  ",")),
                          list("pos",
                               !is.null(seq_pos),
                               seq_pos),
                          list("includeMsa",
                               !is.null(include_msa) && !is.null(seq_pos),
                               ifelse(isTRUE(include_msa),
                                      yes = "true", no = "false"))
  )

  ## Build Function-Specific Call
  if (is.null(seq_pos)) {
    path_input <- "matchortho"
  } else {
    path_input <- "homologpos"
  }
  parser_input <- list("json->list_simp",
                       function(x) {x$search$mapping$mapped})
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("panther", "url"),
                          path = paste0(.rba_stg("panther", "pth"),
                                        "ortholog/", path_input),
                          encode = "form",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_panther_ortholog.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search PANTHER for Homologs of Gene(s)
#'
#' Using this function you can search and retrieve homolog of given gene(s).
#'
#' @param genes Character vector of genes identifiers with maximum length of
#'   10 or only one if seq_pos is provided. Can be any of: Ensemble gene ID,
#'   Ensemble protein ID, Ensemble transcript ID, Entrez gene ID, gene symbol,
#'   NCBI GI, HGNC ID, International protein index ID, NCBI UniGene ID,
#'   UniProt accession and/or UniProt ID.
#' @param organism (numeric) NCBI taxon ID of the organism of your provided
#'   genes. run \code{\link{rba_panther_info}} with argument
#'   'what = "organisms"' to get a list of PANTHER's supported organisms.
#' @param type Homolog types to return. either "P" (default) for paralogs,
#'   "X" for horizontal gene transfer and "LDX" for diverged horizontal gene
#'   transfer.
#' @param target_organisms (numeric) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#'   For Paralog, target organism and organism should be the same; Otherwise,
#'   the target organism should be different from the input organism.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET http://www.pantherdb.org/services/oai/pantherdb/ortholog/homologOther"
#'
#' @return A dataframe with homologs information.
#'
#' @references \itemize{
#'   \item Mi, H., Muruganujan, A., Ebert, D., Huang, X., & Thomas, P. D.
#'   (2019). PANTHER version 14: more genomes, a new PANTHER GO-slim and
#'   improvements in enrichment analysis tools. Nucleic acids research, 47(D1),
#'   D419-D426.
#'   \item Mi, H., Muruganujan, A., Huang, X., Ebert, D., Mills, C., Guo, X.,
#'   & Thomas, P. D. (2019). Protocol Update for large-scale genome and gene
#'   function analysis with the PANTHER classification system (v. 14.0).
#'   Nature protocols, 14(3), 703-721.
#'   \item \href{http://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
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
  .rba_args(cons = list(list(arg = "genes",
                             class = c("character",
                                       "numeric"),
                             max_len = 10),
                        list(arg = "organism",
                             class = "numeric",
                             len = 1),
                        list(arg = "type",
                             class = "character",
                             val = c("P",
                                     "X",
                                     "LDX"),
                             len = 1),
                        list(arg = "target_organisms",
                             class = "numeric")),
            cond = list(list(quote(type == "P" && !is.null(target_organisms)),
                             "For Paralog, target organism and organism should be the same. thus, 'target_organisms' was ignored.",
                             warn = TRUE),
                        list(quote(type != "P" && !is.null(target_organisms) && organism %in% target_organisms),
                             "For horizontal gene transfers or least diverged horizontal gene transfers, the target organism should be different from the input organism")))
  .msg("Retrieving %s homologs of genes %s.",
       type, .paste2(genes, quote_all = "'"))

  ## Build POST API Request's body
  call_body <- .rba_query(init = list(geneInputList = paste(genes, collapse =  ","),
                                      organism = organism,
                                      homologType = type),
                          list("targetOrganism",
                               !is.null(target_organisms),
                               paste(target_organisms, collapse =  ","))
  )


  ## Build Function-Specific Call
  parser_input <- list("json->list_simp",
                       function(x) {x$search$mapping$mapped})

  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("panther", "url"),
                          path = paste0(.rba_stg("panther", "pth"),
                                        "ortholog/homologOther"),
                          encode = "form",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_panther_homolog.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get PANTHER Families and Sub-Families
#'
#' Using this function, you can retrieve Orthologs, MSA or Tree topology
#'   information of a given PANTHER family.
#'
#' @param id Panther family id.
#' @param what What to retrieve? One of: \itemize{
#' \item "ortholog": Orthologs ('LDO' for least diverged and 'O' for more
#'   diverged).
#' \item "msa": Multiple Sequence Alignment Information,
#' \item "tree": Tree topology and nodes attributes.
#' }
#' @param target_organisms (numeric) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET http://www.pantherdb.org/services/oai/pantherdb/familyortholog"
#'  \cr "GET http://www.pantherdb.org/services/oai/pantherdb/familymsa"
#'  \cr "GET http://www.pantherdb.org/services/oai/pantherdb/treeinfo"
#'
#' @return For trees a list and otherwise a data frame with the requested
#'   family's information.
#'
#' @references \itemize{
#'   \item Mi, H., Muruganujan, A., Ebert, D., Huang, X., & Thomas, P. D.
#'   (2019). PANTHER version 14: more genomes, a new PANTHER GO-slim and
#'   improvements in enrichment analysis tools. Nucleic acids research, 47(D1),
#'   D419-D426.
#'   \item Mi, H., Muruganujan, A., Huang, X., Ebert, D., Mills, C., Guo, X.,
#'   & Thomas, P. D. (2019). Protocol Update for large-scale genome and gene
#'   function analysis with the PANTHER classification system (v. 14.0).
#'   Nature protocols, 14(3), 703-721.
#'   \item \href{http://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
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
  .rba_args(cons = list(list(arg = "id",
                             class = "character",
                             len = 1),
                        list(arg = "what",
                             class = "character",
                             val = c("ortholog",
                                     "msa",
                                     "tree"),
                             len = 1),
                        list(arg = "target_organisms",
                             class = "numeric")))
  .msg( "Retrieving %s information of PANTHER family %s.", what, id)

  ## Build POST API Request's body
  call_body <- .rba_query(init = list(family = id),
                          list("taxonFltr",
                               !is.null(target_organisms),
                               paste(target_organisms, collapse =  ","))
  )

  ## Build Function-Specific Call
  switch(what,
         "ortholog" = {
           path_input <- "familyortholog"
           parser_input <- list("json->list_simp",
                                function(x) {x$search$ortholog_list$ortholog})
         },
         "msa" = {
           path_input <- "familymsa"
           parser_input <- list("json->list_simp",
                                function(x) {x$search$MSA_list$sequence_info})},
         "tree" = {
           path_input <- "treeinfo"
           parser_input <- list("json->list_simp",
                                function(x) {x$search$tree_topology})})
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("panther", "url"),
                          path = paste0(.rba_stg("panther", "pth"),
                                        path_input),
                          encode = "form",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_panther_family.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' PANTHER Tree Grafter
#' Use this function to retrieve a PANTHER family's tree topology information
#'   with a node corresponding to your sequence grafted in the best location
#'   in that tree.
#'
#' For more information, see:
#'   \href{https://academic.oup.com/bioinformatics/article/35/3/518/5056037}{TreeGrafter:
#'   phylogenetic tree-based annotation of proteins with Gene Ontology terms
#'   and other annotations}
#'
#' @param protein_seq A character string with the protein's sequence. Maximum
#'   allowed sequence length is 50kb.
#' @param target_organisms (numeric) NCBI taxon ID(s) to filter the results.
#'   run \code{\link{rba_panther_info}} with argument 'what = "organisms"' to
#'   get a list of PANTHER's supported organisms.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET http://www.pantherdb.org/services/oai/pantherdb/graftsequence"
#'
#' @return A list containing PANTHER tree topology information.
#'
#' @references \itemize{
#'   \item Mi, H., Muruganujan, A., Ebert, D., Huang, X., & Thomas, P. D.
#'   (2019). PANTHER version 14: more genomes, a new PANTHER GO-slim and
#'   improvements in enrichment analysis tools. Nucleic acids research, 47(D1),
#'   D419-D426.
#'   \item Mi, H., Muruganujan, A., Huang, X., Ebert, D., Mills, C., Guo, X.,
#'   & Thomas, P. D. (2019). Protocol Update for large-scale genome and gene
#'   function analysis with the PANTHER classification system (v. 14.0).
#'   Nature protocols, 14(3), 703-721.
#'   \item \href{http://www.pantherdb.org/services/details.jsp}{PANTHER
#'   Services Details}
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
  .rba_args(cons = list(list(arg = "protein_seq",
                             class = "character",
                             len = 1),
                        list(arg = "target_organisms",
                             class = "numeric")),
            cond = list(list(quote(nchar(protein_seq) > 5000),
                             "Maximum allowed length of protein sequence is 50kb.")))
  .msg("Retrieving a PANTHER family tree with your input protein grated in it.")

  ## Build POST API Request's body
  call_body <- .rba_query(init = list(sequence  = protein_seq),
                          list("taxonFltr",
                               !is.null(target_organisms),
                               paste(target_organisms, collapse =  ","))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list_simp",
                       function(x) {x$search})

  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("panther", "url"),
                          path = paste0(.rba_stg("panther", "pth"),
                                        "graftsequence"),
                          encode = "form",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_panther_tree_grafter.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
