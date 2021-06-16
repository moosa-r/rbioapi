#### Proteomes Endpoints ####
#' Search Proteomes in UniProt
#'
#' UniProt collects and annotates proteomes (Protein sets expressed in an
#'   organism). Using this function you can search UniProt for available
#'   proteomes. see \href{https://www.uniprot.org/help/proteome}{What are
#'   proteomes?} for more information. You may also
#'   refine your search with modifiers such as keyword, taxon id etc. See
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/proteomes"
#'
#' @param name a keyword in proteome's name
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param keyword Limit the search to entries that contain your supplied
#'   keyword. see: \href{https://www.uniprot.org/keywords/}{UniProt Keywords}
#' @param xref Proteome cross-references such as Genome assembly ID or
#'   Biosample ID. You can supply up to 20 cross-reference IDs.
#' @param genome_acc Genome accession associated with the proteome's components.
#' @param is_ref_proteome (logical) If TRUE, only return reference proteomes; If
#'   FALSE, only returns non-reference proteomes; If NULL (default), the results
#'   will not be filtered by this criteria see
#'   \href{https://www.uniprot.org/help/reference_proteome}{'What are reference
#'   proteomes?'} for more information.
#' @param is_redundant (logical) If TRUE, only return redundant proteomes; If
#'   FALSE, only returns non-redundant proteomes; If NULL (default), the results
#'   will not be filtered by redundancy. see
#'   \href{https://www.uniprot.org/help/proteome_redundancy}{'Reducing proteome
#'   redundancy'} for more information.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list where each element is a list that corresponds to a single
#'   proteome (search hit) and contains informations pertinent to that proteome.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_proteomes_search(name = "SARS-CoV")
#' }
#' \donttest{
#' rba_uniprot_proteomes_search(name = "SARS-CoV", is_ref_proteome = TRUE)
#' }
#' \donttest{
#' rba_uniprot_proteomes_search(name = "SARS-CoV", is_ref_proteome = TRUE)
#' }
#' \donttest{
#' rba_uniprot_proteomes_search(genome_acc = "AY274119")
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_proteomes_search <- function(name = NULL,
                                         upid = NULL,
                                         taxid = NULL,
                                         keyword = NULL,
                                         xref = NULL,
                                         genome_acc = NULL,
                                         is_ref_proteome = NULL,
                                         is_redundant = NULL,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "upid",
                             class = "character",
                             max_len = 100),
                        list(arg = "name",
                             class = "character"),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "keyword",
                             class = "character"),
                        list(arg = "xref",
                             class = "character",
                             max_len = 20),
                        list(arg = "genome_acc",
                             class = "character",
                             max_len = 20),
                        list(arg = "is_ref_proteome",
                             class = "logical"),
                        list(arg = "is_redundant",
                             class = "logical"))
  )

  .msg("Searching UniProt and retrieving proteoms that match your supplied inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("name",
                                !is.null(name),
                                name),
                           list("upid",
                                !is.null(upid),
                                paste0(upid,
                                       collapse = ",")),
                           list("taxid",
                                !is.null(taxid),
                                paste0(taxid,
                                       collapse = ",")),
                           list("keyword",
                                !is.null(keyword),
                                keyword),
                           list("xref",
                                !is.null(xref),
                                paste0(xref,
                                       collapse = ",")),
                           list("genome_acc",
                                !is.null(genome_acc),
                                paste0(genome_acc,
                                       collapse = ",")),
                           list("is_ref_proteome",
                                !is.null(is_ref_proteome),
                                ifelse(is_ref_proteome,
                                       "true",
                                       "false")),
                           list("is_redundant",
                                !is.null(is_redundant),
                                ifelse(is_redundant,
                                       "true",
                                       "false")))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       function(x) {
                         x_names <- vapply(X = x,
                                          FUN = function(x) {
                                            x$upid
                                          },
                                          FUN.VALUE = character(1))
                         names(x) <- x_names
                         return(x)})

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomes"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_proteomes_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get proteome by proteome/proteins UPID
#'
#' UniProt collects and annotates proteomes(Protein sets expressed in an
#'   organism). Using this function you can search UniProt for available
#'   proteomes. see \href{https://www.uniprot.org/help/proteome}{What are
#'   proteomes?} for more information.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/proteomes/proteins/{upid}"
#'  \cr "GET https://ebi.ac.uk/proteins/api/proteomes/{upid}"
#'
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param get_proteins logical: set FALSE (default) to only return information
#'   of the proteome with supplied UPID, set TRUE to also return the proteins
#'    of the supplied proteome UPID.
#' @param reviewed Logical:  Only considered when get_proteins is TRUE.
#'   If TRUE, only return "UniProtKB/Swiss-Prot" (reviewed) proteins;
#'   If FALSE, only return TrEMBL (un-reviewed) entries. leave it as NULL if you
#'   do not want to filter proteins based on their review status.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return a list containing information of the proteome with your supplied
#'   UPID that can contain the proteomes protein entries based on the value of
#'   get_proteins argument.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_proteomes(upid = "UP000000354")
#' }
#' \donttest{
#' rba_uniprot_proteomes(upid = "UP000000354", get_proteins = TRUE)
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_proteomes <- function(upid,
                                  get_proteins = FALSE,
                                  reviewed = NULL,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "upid",
                             class = "character"),
                        list(arg = "get_proteins",
                             class = "logical"),
                        list(arg = "reviewed",
                             class = "logical")),
            cond = list(list(quote(isFALSE(get_proteins) && !is.null(reviewed)),
                             "'reviewed' argument is ignored because you supplied 'get_proteins' as FALSE.")),
            cond_warning = TRUE
  )

  .msg("Retrieving proteome %s (%s).",
       upid,
       ifelse(isTRUE(get_proteins),
              yes = sprintf("With %s proteins",
                            ifelse(test = is.null(reviewed),
                                   yes = "",
                                   no = ifelse(test = reviewed,
                                               yes = "only UniProtKB/Swiss-Prot",
                                               no =  "only TrEMBL"))),
              no = "Excluding proteins"))
  ## Build Function-Specific Call
  if (isTRUE(get_proteins)) {
    ## Build GET API Request's query
    call_query <- .rba_query(init = list(),
                             list("reviewed",
                                  !is.null(reviewed),
                                  ifelse(reviewed,
                                         "true",
                                         "false")))
    path_input <- paste0(.rba_stg("uniprot", "pth"),
                         "proteomes/proteins/",
                         upid)
  } else {
    call_query <- NULL
    path_input <- paste0(.rba_stg("uniprot", "pth"),
                         "proteomes/",
                         upid)
  }

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_proteomes.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Genecentric Endpoints ####

#' Search Gene-Centric Proteins
#'
#' Using this function you can search UniProt for available gene-centrics from
#'   proteomes. For more information,
#'   see \href{https://www.uniprot.org/help/proteome}{What are proteomes?} and
#'   \href{https://www.uniprot.org/help/gene_centric_isoform_mapping}{Automatic
#'   gene-centric isoform mapping for eukaryotic reference proteome entries.}
#'   You may also refine your search with modifiers upid, accession and gene.
#'   See "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/genecentric"
#'
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param gene unique gene identifier(s) found in MOD,
#'   \href{https://www.ensembl.org/info/genome/genebuild/gene_names.html}{Ensembl},
#'   Ensembl Genomes, \href{https://www.uniprot.org/help/gene_name}{OLN},
#'   \href{https://www.uniprot.org/help/gene_name}{ORF} or
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt Gene Name}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return a list containing gene-centric proteins search hits.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_genecentric_search(accession = "P59594")
#' }
#' \donttest{
#' rba_uniprot_genecentric_search(gene = "Spike")
#' }
#' \donttest{
#' rba_uniprot_genecentric_search(upid = "UP000000354")
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_genecentric_search <- function(upid = NULL,
                                           accession = NULL,
                                           gene = NULL,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "upid",
                             class = "character",
                             max_len = 100),
                        list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "gene",
                             class = "character",
                             max_len = 20))
  )

  .msg("Searching UniProt and retrieving Gene-Centric Proteins that match your supplied inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("upid",
                                !is.null(upid),
                                paste0(upid,
                                       collapse = ",")),
                           list("accession",
                                !is.null(accession),
                                paste0(accession,
                                       collapse = ",")),
                           list("gene",
                                !is.null(gene),
                                paste0(gene,
                                       collapse = ",")))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "genecentric"),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_genecentric_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Gene-Centric proteins by UniProt Accession
#'
#' Using this function you can retrieve gene-centrics data. For more
#'   information, see \href{https://www.uniprot.org/help/proteome}{What are
#'   proteomes?} and
#'   \href{https://www.uniprot.org/help/gene_centric_isoform_mapping}{Automatic
#'   gene-centric isoform mapping for eukaryotic reference proteome entries.}.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/genecentric/{accession}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing information of Gene-Centric proteins.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_genecentric("P29965")
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_genecentric <- function(accession,
                                    ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character"))
  )

  .msg("Retrieving Gene-Centric proteins by UniProt Accession %s.",
       accession)
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "genecentric/",
                                        accession),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("uniprot_genecentric.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
