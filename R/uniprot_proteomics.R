#### Metadata endpoints #####

#' Get UniProt Proteomics Metadata
#'
#' Retrieve information on the available Species proteomics data sources in
#'   UniProt.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/species"
#'
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with the available species as rows, and the columns
#'   indicating the proteomics data sources, separated by three main data
#'   categories: PTM (Post-Translational Modification), non-PTM, and
#'   HPP (Human Proteome Project)
#'
#' @references \itemize{
#'   \item The UniProt Consortium , UniProt: the Universal Protein
#'   Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_proteomics_species()
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_species <- function(...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args()

  .msg("Retrieving Unipropt Proteomics metadata")

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/species"),
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("uniprot_species.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#### Proteomics-non-PTM Endpoints ####

#' Search Proteomics data in UniProt
#'
#' Using this function, you can search for non-Post-Translational Modification
#'   proteomics features that has been map to UniProt proteins. You may also
#'   refine your search with modifiers such as data_source, peptide etc. See
#'   "Arguments section" for more information.
#'
#' Note that this is a search function. Thus, you are not required to fill every
#' argument; You may use whatever combinations of arguments you see fit for your
#' query.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources: "GET
#'   https://www.ebi.ac.uk/proteins/api/proteomics/nonPtm"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You
#'   can supply up to 20 taxon IDs.
#' @param data_source Proteomics data source. In addition to manual curation,
#'   UniProt also import PTM annotations from the following databases:
#'   \itemize{
#'   \item "MaxQB"
#'   \item \href{https://peptideatlas.org/}{"PeptideAtlas"}
#'   \item "EPD"
#'   \item \href{https://www.proteomicsdb.org/}{"ProteomicsDB"}
#'   }
#'   Please use `rba_uniprot_proteomics_species()` for more information on
#'   the available data sources for a given species.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Peptide sequence(s). You can supply up to 20 sequences.
#' @param unique Logical: Should the results be filtered based on the Peptide's
#'   uniqueness (the fact that a peptide maps to only 1 protein). If TRUE, Only
#'   unique peptides will be returned, if FALSE only un-unique peptides will be
#'   returned; If NULL (default) the results will not be filtered based on this.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#' @return A list Where each element correspond to a UniProt protein and
#'   proteomics data are organized under the "features"
#'   sub-list.
#'
#' @references \itemize{
#'   \item The UniProt Consortium , UniProt: the Universal Protein
#'   Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas Bursteinas,
#'   Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd Turner, Maria
#'   Martin, The Proteins API: accessing key integrated protein and genome
#'   information, Nucleic Acids Research, Volume 45, Issue W1, 3 July 2017,
#'   Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_proteomics_non_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_non_ptm_search <- function(accession = NULL,
                                                  taxid = NULL,
                                                  data_source = NULL,
                                                  upid = NULL,
                                                  peptide = NULL,
                                                  unique = NULL,
                                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "upid",
                             class = "character",
                             max_len = 100),
                        list(arg = "data_source",
                             class = "character",
                             max_len = 2,
                             val = c("MaxQB",
                                     "PeptideAtlas",
                                     "EPD",
                                     "ProteomicsDB")),
                        list(arg = "peptide",
                             class = "character",
                             max_len = 20),
                        list(arg = "unique",
                             class = "logical"))
  )

  .msg("Searching UniProt and retrieving proteomics nonPTM features of proteins that match your supplied inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                !is.null(accession),
                                paste0(accession,
                                       collapse = ",")),
                           list("taxid",
                                !is.null(taxid),
                                paste0(taxid,
                                       collapse = ",")),
                           list("upid",
                                !is.null(upid),
                                paste0(upid,
                                       collapse = ",")),
                           list("data_source",
                                !is.null(data_source),
                                paste0(data_source,
                                       collapse = ",")),
                           list("peptide",
                                !is.null(peptide),
                                paste0(peptide,
                                       collapse = ",")),
                           list("unique",
                                !is.null(unique),
                                ifelse(unique, "true", "false")))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/nonPtm"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_proteomics_non_ptm_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Proteomics data in UniProt
#'
#' UniProt maps proteomics data from different sources to the proteins'
#'   sequences. Using this function, you can retrieve all the
#'   non-post-translational modification proteomics features that has been map
#'   to a given UniProt protein's sequence.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/nonPtm/{accession}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the proteomics data features of
#' your supplied UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium , UniProt: the Universal Protein
#'   Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_proteomics_non_ptm(accession = "P04234")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_non_ptm <- function(accession,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             len = 1))
  )

  .msg("Retrieving proteomics Proteomics nonPTM features mapped to the sequence of protein %s.",
       accession)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/ptm/",
                                        accession),
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_proteomics_non_ptm.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#### Proteomics-PTM Endpoints ####

#' Search Post-Translational Modification Proteomics in UniProt
#'
#' Using this function, you can search for Post-Translational Modification
#'   proteomics features that has been map to UniProt proteins. You may also
#'   refine your search with modifiers such as data_source, peptide etc. See
#'   "Arguments section" for more information.
#'
#' Note that this is a search function. Thus, you are not required to fill every
#' argument; You may use whatever combinations of arguments you see fit for your
#' query.
#'
#' see also:
#'   \href{https://www.uniprot.org/help/post-translational_modification}{PTM /
#'   Processing section in UniProtKB}
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources: "GET
#'   https://www.ebi.ac.uk/proteins/api//proteomics/ptm"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param ptm Post-translational modification name.
#'   \href{https://www.uniprot.org/keywords/KW-9991}{Valid values} are:
#'   "Acetylation ", "ADP-ribosylation ", "Amidation ", "Autocatalytic cleavage
#'   ", "Bromination ", "Citrullination ", "Cleavage on pair of basic residues
#'   ", "Covalent protein-DNA linkage ", "Covalent protein-RNA linkage ", "CTQ
#'   ", "D-amino acid ", "Disulfide bond ", "Formylation ",
#'   "Gamma-carboxyglutamic acid ", "Glutathionylation ", "Glycoprotein ",
#'   "Lipoprotein ", "Hydroxylation ", "Hypusine ", "Iodination ", "Isopeptide
#'   bond ", "LTQ ", "Methylation ", "Nitration ", "Organic radical ",
#'   "Oxidation ", "Peptidoglycan-anchor ", "Phosphopantetheine ",
#'   "Phosphoprotein ", "Pyrrolidone carboxylic acid ", "Quinone ",
#'   "S-nitrosylation ", "Sulfation ", "Thioester bond ", "Thioether bond ",
#'   "TPQ ", "TTQ ", "Ubl conjugation ", or "Zymogen".
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You
#'   can supply up to 20 taxon IDs.
#' @param data_source Proteomics data source. In addition to manual curation,
#'   UniProt also import PTM annotations from the following databases:
#'   \itemize{
#'   \item \href{https://www.ebi.ac.uk/pride/}{"PRIDE"}
#'   \item \href{https://www.proteomexchange.org/ptmexchange}{"PTMeXchange"}
#'   }
#'   Please use `rba_uniprot_proteomics_species()` for more information on
#'   the available data sources for a given species.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Peptide sequence(s). You can supply up to 20 sequences.
#' @param unique Logical: Should the results be filtered based on the Peptide's
#'   uniqueness (the fact that a peptide maps to only 1 protein). If TRUE, Only
#'   unique peptides will be returned, if FALSE only un-unique peptides will be
#'   returned; If NULL (default) the results will not be filtered based on this.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#' @param confidence_score (Character) Valid values: "Bronze", "Silver", or
#'   "gold".\cr UniProt classifies modified residues
#'    into three categories based on its false localization rate (FLR)
#'    across multiple dataset. See
#'    \href{https://www.uniprot.org/help/mod_res_large_scale}{Large scale modified residue}
#'    for more information.
#'
#' @return A list Where each element correspond to a UniProt protein and
#'   post-translational modification are organized under the "features"
#'   sub-list.
#'
#' @references \itemize{
#'   \item The UniProt Consortium , UniProt: the Universal Protein
#'   Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas Bursteinas,
#'   Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd Turner, Maria
#'   Martin, The Proteins API: accessing key integrated protein and genome
#'   information, Nucleic Acids Research, Volume 45, Issue W1, 3 July 2017,
#'   Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_proteomics_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_ptm_search <- function(accession = NULL,
                                              ptm = NULL,
                                              taxid = NULL,
                                              data_source = NULL,
                                              upid = NULL,
                                              peptide = NULL,
                                              unique = NULL,
                                              confidence_score = NULL,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "ptm",
                             class = "character",
                             len = 1,
                             val = c("Acetylation",
                                     "ADP-ribosylation",
                                     "Amidation",
                                     "Autocatalytic cleavage",
                                     "Bromination",
                                     "Citrullination",
                                     "Cleavage on pair of basic residues",
                                     "Covalent protein-DNA linkage",
                                     "Covalent protein-RNA linkage",
                                     "CTQ",
                                     "D-amino acid",
                                     "Disulfide bond",
                                     "Formylation",
                                     "Gamma-carboxyglutamic acid",
                                     "Glutathionylation",
                                     "Glycoprotein",
                                     "Lipoprotein",
                                     "Hydroxylation",
                                     "Hypusine",
                                     "Iodination",
                                     "Isopeptide bond",
                                     "LTQ",
                                     "Methylation",
                                     "Nitration",
                                     "Organic radical",
                                     "Oxidation",
                                     "Peptidoglycan-anchor",
                                     "Phosphopantetheine",
                                     "Phosphoprotein",
                                     "Pyrrolidone carboxylic acid",
                                     "Quinone",
                                     "S-nitrosylation",
                                     "Sulfation",
                                     "Thioester bond",
                                     "Thioether bond",
                                     "TPQ",
                                     "TTQ",
                                     "Ubl conjugation",
                                     "Zymogen")),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "upid",
                             class = "character",
                             max_len = 100),
                        list(arg = "data_source",
                             class = "character",
                             max_len = 2,
                             val = c("PRIDE",
                                     "PTMExchange")),
                        list(arg = "peptide",
                             class = "character",
                             max_len = 20),
                        list(arg = "unique",
                             class = "logical"),
                        list(arg = "confidence_score",
                             class = "character",
                             max_len = 1,
                             val = c("Bronze",
                                     "Silver",
                                     "Gold")))
  )

  .msg("Searching UniProt and retrieving proteomics Post-translational modification features of proteins that match your supplied inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                !is.null(accession),
                                paste0(accession,
                                       collapse = ",")),
                           list("ptm",
                                !is.null(ptm),
                                ptm),
                           list("taxid",
                                !is.null(taxid),
                                paste0(taxid,
                                       collapse = ",")),
                           list("upid",
                                !is.null(upid),
                                paste0(upid,
                                       collapse = ",")),
                           list("data_source",
                                !is.null(data_source),
                                paste0(data_source,
                                       collapse = ",")),
                           list("peptide",
                                !is.null(peptide),
                                paste0(peptide,
                                       collapse = ",")),
                           list("unique",
                                !is.null(unique),
                                ifelse(unique, "true", "false")),
                           list("confidence_score",
                                !is.null(confidence_score),
                                confidence_score))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/ptm"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_proteomics_ptm_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Post-Translational Modification of UniProt Protein
#'
#' UniProt maps post-translational modification proteomics data from different
#'   sources to the proteins' sequences. Using this function, you can retrieve
#'   all the post-translational modification features that has been map to a
#'   given UniProt protein's sequence.
#'
#' see also:
#'   \href{https://www.uniprot.org/help/ptm_processing_section}{PTM /
#'   Processing section in UniProtKB}
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm/{accession}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param confidence_score (Character) Valid values: "Bronze", "Silver", or
#'   "gold".\cr UniProt classifies modified residues
#'    into three categories based on its false localization rate (FLR)
#'    across multiple dataset. See
#'    \href{https://www.uniprot.org/help/mod_res_large_scale}{Large scale modified residue}
#'    for more information.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the post-translational modification features of
#' your supplied UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium , UniProt: the Universal Protein
#'   Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_proteomics_ptm(accession = "P04234")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_ptm <- function(accession,
                                       confidence_score = NULL,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             len = 1))
  )

  .msg("Retrieving proteomics post-translational modification features mapped to the sequence of protein %s.",
       accession)

  ## Build GET API Request's query
  call_query <- .rba_query(init = list(),
                           list("confidence_score",
                                !is.null(confidence_score),
                                confidence_score),
                           list(arg = "confidence_score",
                                class = "character",
                                max_len = 1,
                                val = c("Bronze",
                                        "Silver",
                                        "Gold")))

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/ptm/",
                                        accession),
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_proteomics_ptm.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#### Proteomics-HPP Endpoints ####

#' Search HPP Proteomics data in UniProt
#'
#' Using this function, you can search for HPP (Human Proteome Project)
#'   proteomics features that has been map to UniProt proteins. You may also
#'   refine your search with modifiers such as data_source, peptide etc. See
#'   "Arguments section" for more information.
#'
#' Note that this is a search function. Thus, you are not required to fill every
#' argument; You may use whatever combinations of arguments you see fit for your
#' query.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources: "GET
#'   https://www.ebi.ac.uk/proteins/api/proteomics/ptm"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You
#'   can supply up to 20 taxon IDs.
#' @param data_source Proteomics data source. In addition to manual curation,
#'   UniProt also import PTM annotations from the following databases:
#'   \itemize{
#'   \item \href{https://hupo.org/human-proteome-project/}{"HPP"}
#'   }
#'   Please use `rba_uniprot_proteomics_species()` for more information on
#'   the available data sources for a given species.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Peptide sequence(s). You can supply up to 20 sequences.
#' @param unique Logical: Should the results be filtered based on the Peptide's
#'   uniqueness (the fact that a peptide maps to only 1 protein). If TRUE, Only
#'   unique peptides will be returned, if FALSE only un-unique peptides will be
#'   returned; If NULL (default) the results will not be filtered based on this.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#' @return A list Where each element correspond to a UniProt protein and
#'   proteomics data are organized under the "features"
#'   sub-list.
#'
#' @references \itemize{
#'   \item The UniProt Consortium , UniProt: the Universal Protein
#'   Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas Bursteinas,
#'   Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd Turner, Maria
#'   Martin, The Proteins API: accessing key integrated protein and genome
#'   information, Nucleic Acids Research, Volume 45, Issue W1, 3 July 2017,
#'   Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_proteomics_hpp_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_hpp_search <- function(accession = NULL,
                                              taxid = NULL,
                                              data_source = NULL,
                                              upid = NULL,
                                              peptide = NULL,
                                              unique = NULL,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "upid",
                             class = "character",
                             max_len = 100),
                        list(arg = "data_source",
                             class = "character",
                             max_len = 2,
                             val = c("MaxQB",
                                     "PeptideAtlas",
                                     "EPD",
                                     "ProteomicsDB")),
                        list(arg = "peptide",
                             class = "character",
                             max_len = 20),
                        list(arg = "unique",
                             class = "logical"))
  )

  .msg("Searching UniProt and retrieving proteomics HPP features of proteins that match your supplied inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                !is.null(accession),
                                paste0(accession,
                                       collapse = ",")),
                           list("taxid",
                                !is.null(taxid),
                                paste0(taxid,
                                       collapse = ",")),
                           list("upid",
                                !is.null(upid),
                                paste0(upid,
                                       collapse = ",")),
                           list("data_source",
                                !is.null(data_source),
                                paste0(data_source,
                                       collapse = ",")),
                           list("peptide",
                                !is.null(peptide),
                                paste0(peptide,
                                       collapse = ",")),
                           list("unique",
                                !is.null(unique),
                                ifelse(unique, "true", "false")))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/hpp"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_hpp_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get HPP Proteomics data in UniProt
#'
#' UniProt maps post-translational modification proteomics data from different
#'   sources to the proteins' sequences. Using this function, you can retrieve
#'   all the HPP (Human Proteome Project) proteomics features that has been map
#'   to a given UniProt protein's sequence.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/nonPtm/{accession}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the proteomics data features of
#' your supplied UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium , UniProt: the Universal Protein
#'   Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_proteomics_hpp(accession = "P04234")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_hpp <- function(accession,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             len = 1))
  )

  .msg("Retrieving proteomics Proteomics features mapped to the sequence of protein %s.",
       accession)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/hpp/",
                                        accession),
                          accept = "application/json",
                          parser = "json->df",
                          save_to = .rba_file("uniprot_hpp.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
