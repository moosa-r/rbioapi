#' Name UniProt search hits elements
#'
#' Every search hit in uniprot has a character element within it, named
#'   "accession". this function should be used as the second response
#'   parser in the *_search functions to set the name each search hit to
#'   it's accession
#'
#' @param x object to be parsed
#'
#' @return a list with the same structure of the input, only named.
#'
#' @export
.rba_uniprot_search_namer <- function(x) {
  x_names <- vapply(X = x,
                    FUN = function(x) {
                      x$accession
                    },
                    FUN.VALUE = character(1))
  names(x) <- x_names
  return(x)}

#### Proteins Endpoints ####

#' Search UniProt entries
#'
#' Using this function, you can search and retrieve UniProt Knowledge-base
#'   (UniProtKB) protein entries using variety of options. You may also
#'   refine your search with modifiers such as sequence length, reviews status
#'   etc. refer to "Arguments" section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.s
#'   \cr UniProt Entries are grouped in two sections:\enumerate{
#'   \item Reviewed(Swiss-Prot): Manually annotated records with information
#'   extracted from literature and curator-evaluated computational analysis.
#'   \item Unreviewed (TrEMBL): Computationally analyzed records that await
#'   full manual annotation.}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteins"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param reviewed Logical: If TRUE, only return
#'   "UniProtKB/Swiss-Prot" (reviewed) entries; If FALSE, only return TrEMBL
#'   (un-reviewed) entries.
#' @param isoform Numeric: you have three options:\itemize{
#'   \item 0: Exclude isoforms.
#'   \item 1: Return isoforms only.
#'   \item 2: Return both.}
#'   see: \href{https://www.uniprot.org/help/alternative_products}{Alternative
#'   products}
#' @param go_term Limit the search to entries associated with your provided GO
#'   (\href{https://www.uniprot.org/help/gene_ontology}{Gene Ontology}) term.
#'   You can provide Either GO ID or a character string -partially or fully-
#'   matching the term. e.g. "GO:0001776" or "leukocyte homeostasis". if You
#'   provide "leukocyte", any term containing that word will be included,
#'   e.g "leukocyte chemotaxis", "leukocyte activation".
#' @param keyword Limit the search to entries that contain your provided
#'   keyword. see: \href{https://www.uniprot.org/keywords/}{UniProt Keywords}
#' @param ec \href{https://enzyme.expasy.org/}{EC (Enzyme Commission) number(s)}.
#'   You can provide up to 20 EC numbers.
#' @param gene \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can provide up to 20 gene names. e.g. if you provide
#'   "CD40", "CD40 ligand" will also be included.
#' @param exact_gene \href{https://www.uniprot.org/help/gene_name}{UniProt
#'   exact gene name(s)}. You can provide up to 20 exact gene names. e.g.
#'   if you provide "CD40", "CD40 ligand" will not be included in the results.
#' @param protein \href{https://www.uniprot.org/help/protein_names}{UniProt
#'   protein name}
#' @param organism \href{https://www.uniprot.org/taxonomy/}{Organism name}.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can provide up to 20 taxon IDs.
#' @param pubmed Entries which \href{https://www.uniprot.org/citations/}{cite
#'   to} the article with your provided PubMed ID.
#' @param seq_length An exact sequence length (e.g. 150) or a range of sequence
#'   lengths (e.g. "130-158").
#' @param md5 Sequence md5 value.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A List where each element corresponds to one UniProt entity returned
#'   by your search query. The element itself is a sub-list containing all
#'   information that UniProt has about that entity.
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
#' rba_uniprot_proteins_search(accession = "Q99616")
#' }
#' \donttest{
#' rba_uniprot_proteins_search(gene = "cd40")
#' }
#' \donttest{
#' rba_uniprot_proteins_search(gene = "cd40 ligand")
#' }
#' \donttest{
#' rba_uniprot_proteins_search(gene = "cd40",  reviewed = TRUE)
#' }
#' \donttest{
#' rba_uniprot_proteins_search(gene = "cd40",  reviewed = TRUE, isoform = 1)
#' }
#' \donttest{
#' rba_uniprot_proteins_search(keyword = "Inhibition of host chemokines by virus")
#' }
#' \donttest{
#' rba_uniprot_proteins_search(keyword = "chemokines")
#' }
#'
#' @family "UniProt - Proteins"
#' @export
rba_uniprot_proteins_search <- function(accession = NA,
                                        reviewed = NA,
                                        isoform = NA,
                                        go_term = NA,
                                        keyword = NA,
                                        ec = NA,
                                        gene = NA,
                                        exact_gene = NA,
                                        protein = NA,
                                        organism = NA,
                                        taxid = NA,
                                        pubmed = NA,
                                        seq_length = NA,
                                        md5 = NA,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "reviewed",
                             class = "logical"),
                        list(arg = "isoform",
                             class = "numeric",
                             val = c(0, 1, 2)),
                        list(arg = "go_term",
                             class = "character"),
                        list(arg = "keyword",
                             class = "character"),
                        list(arg = "ec",
                             class = "character",
                             max_len = 20),
                        list(arg = "gene",
                             class = "character",
                             max_len = 20),
                        list(arg = "exact_gene",
                             class = "character",
                             max_len = 20),
                        list(arg = "protein",
                             class = "character"),
                        list(arg = "organism",
                             class = "character"),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "pubmed",
                             class = "character",
                             max_len = 20),
                        list(arg = "seq_length",
                             class = c("numeric",
                                       "character")),
                        list(arg = "md5",
                             class = "character")))

  .msg("Searching UniProt and retrieving proteins that match your provided inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                any(!is.na(accession)),
                                paste0(accession,
                                       collapse = ",")),
                           list("reviewed",
                                !is.na(reviewed),
                                ifelse(reviewed,
                                       "true",
                                       "false")),
                           list("isoform",
                                !is.na(isoform),
                                isoform),
                           list("goterms",
                                !is.na(go_term),
                                go_term),
                           list("keywords",
                                !is.na(keyword),
                                keyword),
                           list("ec",
                                any(!is.na(ec)),
                                paste0(ec,
                                       collapse = ",")),
                           list("gene",
                                any(!is.na(gene)),
                                paste0(gene,
                                       collapse = ",")),
                           list("exact_gene",
                                any(!is.na(exact_gene)),
                                paste0(exact_gene,
                                       collapse = ",")),
                           list("protein",
                                !is.na(protein),
                                protein),
                           list("organism",
                                !is.na(organism),
                                organism),
                           list("taxid",
                                any(!is.na(taxid)),
                                paste0(taxid,
                                       collapse = ",")),
                           list("pubmed",
                                any(!is.na(pubmed)),
                                paste0(pubmed,
                                       collapse = ",")),
                           list("seq_length",
                                !is.na(seq_length),
                                seq_length),
                           list("md5",
                                !is.na(md5),
                                md5))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteins"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_proteins_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt entry by accession
#'
#' Use this function to retrieve a UniProt Entry by it's UniProt accession.
#'   You can also use "isoform" or "interaction" arguments to retrieve
#'   isoforms or interactor proteins of that entry. Note that in one function
#'   call you can only set none or only one of "isoform" or "interaction" as
#'   TRUE, not both of them.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/proteins/{accession}"
#'  \cr "GET https://ebi.ac.uk/proteins/api/proteins/interaction/{accession}"
#'  \cr "GET https://ebi.ac.uk/proteins/api/proteins/{accession}/isoforms"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param interaction Logical: (default = FALSE) Only retrieve
#'   \href{https://www.uniprot.org/help/interaction_section}{interaction}
#'   information of your provided UniProt entity?
#' @param isoforms Logical: (default = FALSE) Only retrieve
#'   \href{https://www.uniprot.org/help/alternative_products}{isoforms} of your
#'   provided UniProt entity?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list that contains UniProt protein informations with your
#'   provided accession.
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
#' rba_uniprot_proteins(accession = "P01730")
#' }
#' \donttest{
#' rba_uniprot_proteins(accession = "P01730", interaction = TRUE)
#' }
#' \donttest{
#' rba_uniprot_proteins(accession = "Q29983", isoforms = TRUE)
#' }
#'
#' @family "UniProt - Proteins"
#' @export
rba_uniprot_proteins <- function(accession,
                                 interaction = FALSE,
                                 isoforms = FALSE,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character"),
                        list(arg = "interaction",
                             class = "logical"),
                        list(arg = "isoforms",
                             class = "logical")),
            cond = list(list(quote(sum(interaction, isoforms) == 2),
                             "You can only set only one of interaction or isoform as TRUE in one function call.")))

  .msg("Retrieving %sUniProt Entity with accession number %s.",
       if (isTRUE(interaction)) {
         "Interactions of "} else if (isTRUE(isoforms)) {
           "isoforms of "} else {
             ""},
       accession)
  ## Build Function-Specific Call
  path_input <- sprintf("%s%s/%s",
                        .rba_stg("uniprot", "pth"),
                        ifelse(isTRUE(interaction),
                               yes = "proteins/interaction",
                               no = "proteins"),
                        accession)
  if (isTRUE(isoforms)) {
    path_input <- paste0(path_input, "/isoforms")
  }
  parser_input <- ifelse(isTRUE(interaction) | isTRUE(isoforms),
                         yes = "json->list",
                         no = "json->list_simp")
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_proteins.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt Entry by UniProt Cross-Reference Database and ID
#'
#' \href{https://www.uniprot.org/database/}{UniProt Cross-Reference} links
#'   protein Entities with cross-reference (external) databases. Using this
#'   function, you can retrieve a UniProt entity using external database name
#'   and protein ID in that database.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteins/{dbtype}:{dbid}"
#'
#' @param db_id The protein ID in the cross-reference (external) database.
#' @param db_name \href{https://www.uniprot.org/database/}{cross-reference}
#'   (external database) name.
#' @param reviewed Logical: (Optional) If TRUE, only returns
#'   "UniProtKB/Swiss-Prot" (reviewed) entries; If FALSE, only returns TrEMBL
#'   (un-reviewed) entries.
#' @param isoform Numeric: (Optional) you have two options:\itemize{
#'   \item 0: Exclude isoforms.
#'   \item 1: Return isoforms only.}
#'   see: \href{https://www.uniprot.org/help/alternative_products}{Alternative
#'   products}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List which each element is a UniProt entity that correspond to
#'   your provided cross-reference database name and ID.
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
#' rba_uniprot_proteins_crossref("cd40", "hgnc")
#' }
#' \donttest{
#' rba_uniprot_proteins_crossref("cd40", "hgnc", reviewed = TRUE)
#' }
#' \donttest{
#' rba_uniprot_proteins_crossref("mica", "hgnc", isoform = 0)
#' }
#'
#' @family "UniProt - Proteins"
#' @export
rba_uniprot_proteins_crossref <- function(db_id,
                                          db_name,
                                          reviewed = NA,
                                          isoform = NA,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "db_name",
                             class = "character"),
                        list(arg = "db_id",
                             class = "character"),
                        list(arg = "reviewed",
                             class = "logical"),
                        list(arg = "isoform",
                             class = "numeric",
                             val = c(0, 1, 2))))
  .msg("Retrieving UniProt entities that correspond to ID %s in database %s.",
       db_id, db_name)
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("reviewed",
                                !is.na(reviewed),
                                ifelse(reviewed,
                                       "true",
                                       "false")),
                           list("isoform",
                                !is.na(isoform),
                                isoform))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = sprintf("%sproteins/%s:%s",
                                         .rba_stg("uniprot", "pth"),
                                         db_name,
                                         db_id),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_proteins_crossref.json"))
  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Features Endpoints ####

#' UniProt maintains \href{https://www.uniprot.org/help/sequence_annotation}{
#'   sequence annotations (features)} that describe regions
#'   in the protein sequence. Using this function, you can search and
#'   retrieve UniProt proteins' sequence annotations (features). you may also
#'   refine your search query with variety of modifiers.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr UniProt Entries are grouped in two sections:\enumerate{
#'   \item Reviewed(Swiss-Prot): Manually annotated records with information
#'   extracted from literature and curator-evaluated computational analysis.
#'   \item Unreviewed (TrEMBL): Computationally analyzed records that await
#'   full manual annotation.}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/features"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param gene \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can provide up to 20 gene names. e.g. if you provide
#'   "CD40", "CD40 ligand" will also be included.
#' @param exact_gene \href{https://www.uniprot.org/help/gene_name}{UniProt
#'   exact gene name(s)}. You can provide up to 20 exact gene names. e.g.
#'   if you provide "CD40", "CD40 ligand" will not be included in the results.
#' @param protein \href{https://www.uniprot.org/help/protein_names}{UniProt
#'   protein name}
#' @param reviewed Logical: If TRUE, only return
#'   "UniProtKB/Swiss-Prot" (reviewed) entries; If FALSE, only return TrEMBL
#'   (un-reviewed) entries.
#' @param organism \href{https://www.uniprot.org/taxonomy/}{Organism name}.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can provide up to 20 taxon IDs.
#' @param categories \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (Features)} categories (subsection). accepted values
#'   are: "MOLECULE_PROCESSING", "TOPOLOGY", "SEQUENCE_INFORMATION",
#'   "STRUCTURAL", "DOMAINS_AND_SITES", "PTM", "VARIANTS" and/or "MUTAGENESIS".
#'   You can provide up to 8 categories.
#' @param types \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (Features)} types. accepted values
#'   are: "INIT_MET", "SIGNAL", "PROPEP", "TRANSIT", "CHAIN", "PEPTIDE",
#'   "TOPO_DOM", "TRANSMEM", "DOMAIN", "REPEAT", "CA_BIND", "ZN_FING",
#'   "DNA_BIND", "NP_BIND", "REGION", "COILED", "MOTIF", "COMPBIAS",
#'   "ACT_SITE", "METAL", "BINDING", "SITE", "NON_STD", "MOD_RES", "LIPID",
#'   "CARBOHYD", "DISULFID", "CROSSLNK", "VAR_SEQ", "VARIANT", "MUTAGEN",
#'   "UNSURE", "CONFLICT", "NON_CONS", "NON_TER", "HELIX", "TURN", "STRAND"
#'   and/or "INTRAMEM". You can provide up to 20 types.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List where each element corresponds to one UniProt entity returned
#'   by your search query. The element itself is a sub-list containing all
#'   information that UniProt has about that entity.
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
#' rba_uniprot_features_search(accession = "Q99616")
#' }
#' \donttest{
#' rba_uniprot_features_search(gene = "cd40")
#' }
#' \donttest{
#' rba_uniprot_features_search(gene = "cd40 ligand")
#' }
#' \donttest{
#' rba_uniprot_features_search(gene = "cd40",  reviewed = TRUE)
#' }
#' \donttest{
#' rba_uniprot_features_search(accession = "Q99616",
#'     categories = c("MOLECULE_PROCESSING", "TOPOLOGY"))
#' }
#' \donttest{
#' rba_uniprot_features_search(accession = "Q99616", types = "DISULFID")
#' }
#'
#' @family "UniProt - Features"
#' @export
rba_uniprot_features_search <- function(accession = NA,
                                        gene = NA,
                                        exact_gene = NA,
                                        protein = NA,
                                        reviewed = NA,
                                        organism = NA,
                                        taxid = NA,
                                        categories = NA,
                                        types = NA,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "gene",
                             class = "character",
                             max_len = 20),
                        list(arg = "exact_gene",
                             class = "character",
                             max_len = 20),
                        list(arg = "protein",
                             class = "character"),
                        list(arg = "reviewed",
                             class = "logical"),
                        list(arg = "organism",
                             class = "character"),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "categories",
                             class = "character",
                             val = c("MOLECULE_PROCESSING",
                                     "TOPOLOGY",
                                     "SEQUENCE_INFORMATION",
                                     "STRUCTURAL",
                                     "DOMAINS_AND_SITES",
                                     "PTM",
                                     "VARIANTS",
                                     "MUTAGENESIS"),
                             max_len = 8),
                        list(arg = "types",
                             class = "character",
                             max_len = 20,
                             val = c("INIT_MET",
                                     "SIGNAL",
                                     "PROPEP",
                                     "TRANSIT",
                                     "CHAIN",
                                     "PEPTIDE",
                                     "TOPO_DOM",
                                     "TRANSMEM",
                                     "DOMAIN",
                                     "REPEAT",
                                     "CA_BIND",
                                     "ZN_FING",
                                     "DNA_BIND",
                                     "NP_BIND",
                                     "REGION",
                                     "COILED",
                                     "MOTIF",
                                     "COMPBIAS",
                                     "ACT_SITE",
                                     "METAL",
                                     "BINDING",
                                     "SITE",
                                     "NON_STD",
                                     "MOD_RES",
                                     "LIPID",
                                     "CARBOHYD",
                                     "DISULFID",
                                     "CROSSLNK",
                                     "VAR_SEQ",
                                     "VARIANT",
                                     "MUTAGEN",
                                     "UNSURE",
                                     "CONFLICT",
                                     "NON_CONS",
                                     "NON_TER",
                                     "HELIX",
                                     "TURN",
                                     "STRAND",
                                     "INTRAMEM")
                        ))
  )

  .msg("Searching UniProt and retrieving sequence annotations (features) of proteins that match your provided inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                any(!is.na(accession)),
                                paste0(accession,
                                       collapse = ",")),
                           list("gene",
                                any(!is.na(gene)),
                                paste0(gene,
                                       collapse = ",")),
                           list("exact_gene",
                                any(!is.na(exact_gene)),
                                paste0(exact_gene,
                                       collapse = ",")),
                           list("protein",
                                !is.na(protein),
                                protein),
                           list("reviewed",
                                !is.na(reviewed),
                                ifelse(reviewed,
                                       "true",
                                       "false")),
                           list("organism",
                                !is.na(organism),
                                organism),
                           list("taxid",
                                any(!is.na(taxid)),
                                paste0(taxid,
                                       collapse = ",")),
                           list("categories",
                                any(!is.na(categories)),
                                paste0(categories,
                                       collapse = ",")),
                           list("types",
                                any(!is.na(types)),
                                paste0(types,
                                       collapse = ",")))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "features"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_features_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

# #' Search protein sequence features of a given type in UniProt
# #'
# #' Search for term(s) that appear in feature description for your specified
# #' feature type. For example, you can search by type=DOMAIN and
# #' Term=Kinase. Comma separated values
# #'
# #' @section Corresponding API Resources:
# #'  "GET https://www.ebi.ac.uk/proteins/api"
# #'
# #' @param terms
# #' @param type
# #' @param categories
# #' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
# #'   arguments documentation for more information on available options.
# #'
# #' @return
# #'
# #' @references \itemize{
# #'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
# #'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
# #'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
# #'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
# #'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
# #'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
# #'   Documentation}
# #'   }
# #'
# # #' @examples
# #'
# #' @family "UniProt - Features"
# #' @export
# rba_uniprot_features_type <- function(terms,
#                                      type,
#                                      categories = NA,
#                                      ...) {
#   ## Load Global Options
#   .rba_ext_args(...)
#   ## Check User-input Arguments
#   .rba_args(cons = list(list(arg = "terms",
#                                class = "character",
#                                max_len = 20),
#                           list(arg = "type",
#                                class = "character",
#                                len = 1,
#                                val = c("INIT_MET",
#                                        "SIGNAL",
#                                        "PROPEP",
#                                        "TRANSIT",
#                                        "CHAIN",
#                                        "PEPTIDE",
#                                        "TOPO_DOM",
#                                        "TRANSMEM",
#                                        "DOMAIN",
#                                        "REPEAT",
#                                        "CA_BIND",
#                                        "ZN_FING",
#                                        "DNA_BIND",
#                                        "NP_BIND",
#                                        "REGION",
#                                        "COILED",
#                                        "MOTIF",
#                                        "COMPBIAS",
#                                        "ACT_SITE",
#                                        "METAL",
#                                        "BINDING",
#                                        "SITE",
#                                        "NON_STD",
#                                        "MOD_RES",
#                                        "LIPID",
#                                        "CARBOHYD",
#                                        "DISULFID",
#                                        "CROSSLNK",
#                                        "VAR_SEQ",
#                                        "VARIANT",
#                                        "MUTAGEN",
#                                        "UNSURE",
#                                        "CONFLICT",
#                                        "NON_CONS",
#                                        "NON_TER",
#                                        "HELIX",
#                                        "TURN",
#                                        "STRAND",
#                                        "INTRAMEM")),
#                           list(arg = "categories",
#                                class = "character",
#                                val = c("MOLECULE_PROCESSING",
#                                        "TOPOLOGY",
#                                        "SEQUENCE_INFORMATION",
#                                        "STRUCTURAL",
#                                        "DOMAINS_AND_SITES",
#                                        "PTM",
#                                        "VARIANTS",
#                                        "MUTAGENESIS."),
#                                max_len = 8)
#   )
#   )
#
#   .msg("get /features/type/{type} Search protein sequence features of a given type in UniProt")
#   ## Build GET API Request's query
#   call_query <- .rba_query(init = list("size" = "-1"),
#                             list("categories",
#                                  any(!is.na(categories)),
#                                  paste0(categories,
#                                         collapse = ",")),
#                             list("terms",
#                                  any(!is.na(terms)),
#                                  paste0(terms,
#                                         collapse = ",")))
#   ## Build Function-Specific Call
#   input_call <- .rba_httr(httr = "get",
#                            url = .rba_stg("uniprot", "url"),
#                            path = paste0(.rba_stg("uniprot", "pth"),
#                                          "features/type/",
#                                          type),
#                            query = call_query,
#                            accept = "application/json",
#                            parser = "json->list",
#                            save_to = .rba_file("uniprot_features_type.json"))
#
#   ## Call API
#   final_output <- .rba_skeleton(input_call)
#   return(final_output)
# }

#' Get UniProt protein sequence features by accession
#'
#' Use this function to retrieve
#' \href{https://www.uniprot.org/help/sequence_annotation}{sequence annotations
#'   (features)} of a protein by it's UniProt accession.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/features/{accession}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param types \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (Features)} types. accepted values
#'   are: "INIT_MET", "SIGNAL", "PROPEP", "TRANSIT", "CHAIN", "PEPTIDE",
#'   "TOPO_DOM", "TRANSMEM", "DOMAIN", "REPEAT", "CA_BIND", "ZN_FING",
#'   "DNA_BIND", "NP_BIND", "REGION", "COILED", "MOTIF", "COMPBIAS",
#'   "ACT_SITE", "METAL", "BINDING", "SITE", "NON_STD", "MOD_RES", "LIPID",
#'   "CARBOHYD", "DISULFID", "CROSSLNK", "VAR_SEQ", "VARIANT", "MUTAGEN",
#'   "UNSURE", "CONFLICT", "NON_CONS", "NON_TER", "HELIX", "TURN", "STRAND"
#'   and/or "INTRAMEM". You can provide up to 20 types.
#' @param categories \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (Features)} categories (subsection). accepted values
#'   are: "MOLECULE_PROCESSING", "TOPOLOGY", "SEQUENCE_INFORMATION",
#'   "STRUCTURAL", "DOMAINS_AND_SITES", "PTM", "VARIANTS" and/or "MUTAGENESIS".
#'   You can provide up to 8 categories.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list in which you can find all of your given protein's sequence
#'   annotations in a sub-list named "features".
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
#' rba_uniprot_features("Q99616")
#' }
#' \donttest{
#' rba_uniprot_features(accession = "Q99616", types = "DISULFID")
#' }
#'
#' @family "UniProt - Features"
#' @export
rba_uniprot_features <- function(accession,
                                 types = NA,
                                 categories = NA,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character"),
                        list(arg = "types",
                             class = "character",
                             len = 1,
                             val = c("INIT_MET",
                                     "SIGNAL",
                                     "PROPEP",
                                     "TRANSIT",
                                     "CHAIN",
                                     "PEPTIDE",
                                     "TOPO_DOM",
                                     "TRANSMEM",
                                     "DOMAIN",
                                     "REPEAT",
                                     "CA_BIND",
                                     "ZN_FING",
                                     "DNA_BIND",
                                     "NP_BIND",
                                     "REGION",
                                     "COILED",
                                     "MOTIF",
                                     "COMPBIAS",
                                     "ACT_SITE",
                                     "METAL",
                                     "BINDING",
                                     "SITE",
                                     "NON_STD",
                                     "MOD_RES",
                                     "LIPID",
                                     "CARBOHYD",
                                     "DISULFID",
                                     "CROSSLNK",
                                     "VAR_SEQ",
                                     "VARIANT",
                                     "MUTAGEN",
                                     "UNSURE",
                                     "CONFLICT",
                                     "NON_CONS",
                                     "NON_TER",
                                     "HELIX",
                                     "TURN",
                                     "STRAND",
                                     "INTRAMEM")),
                        list(arg = "categories",
                             class = "character",
                             val = c("MOLECULE_PROCESSING",
                                     "TOPOLOGY",
                                     "SEQUENCE_INFORMATION",
                                     "STRUCTURAL",
                                     "DOMAINS_AND_SITES",
                                     "PTM",
                                     "VARIANTS",
                                     "MUTAGENESIS."),
                             max_len = 8)
  )
  )

  .msg("Retrieving sequence annotations (features) of protein %s.", accession)
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("categories",
                                any(!is.na(categories)),
                                paste0(categories,
                                       collapse = ",")),
                           list("types",
                                any(!is.na(types)),
                                paste0(types,
                                       collapse = ",")))
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "features/",
                                        accession),
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_features.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Variation Endpoints ####

#' Search UniProt Natural Variants
#'
#' Using this function, you can search and retrieve
#'   \href{https://www.uniprot.org/help/variant}{Natural variant(s)} that
#'   has been annotated in the protein's sequences. You may also refine your
#'   search with modifiers such as source type, disease etc. refer to
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/variation"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param source_type Variation's source type. You can choose up to two of:
#'   "UniProt", "large scale study" and/or "mixed".
#' @param consequence_type Variation's consequence type. You can choose up to
#'   two of: "missense", "stop gained" or "stop lost".
#' @param wild_type Wild type amino acid. Accepted values are IUPAC
#'   single-letter amino acid (e.g. D for	Aspartic acid) and "*" for stop
#'   codon. You can provide up to 20 values.
#' @param alternative_sequence Alternative amino acid. Accepted values are IUPAC
#'   single-letter amino acid (e.g. D for	Aspartic acid) and "*" for stop codon
#'   and "-" for deletion. You can provide up to 20 values.
#' @param location A valid amino acid range (e.g. 10-25) within the sequence
#'   range where the variation occurs. You can provide up to 20 values.
#' @param disease \href{https://www.uniprot.org/diseases/}{Human disease}
#'   that are associated with a sequence variation. Accepted values are
#'   disease name (e.g. Alzheimer disease 18), partial disease name
#'   (Alzheimer) and/or disease acronym (e.g. AD). You can provide up to
#'   20 values.
#' @param omim \href{https://www.ncbi.nlm.nih.gov/omim}{OMIM} ID that is
#'   associated with a variation. You can provide up to 20 values.
#' @param evidence Pubmed ID of the variation's
#'   \href{https://www.uniprot.org/citations/}{citation} You can provide up
#'   to 20 values.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can provide up to 20 taxon IDs.
#' @param db_type cross-reference database of the variation. You can provide
#'   up to two of the following:\itemize{
#'   \item "dbSNP": \href{https://www.ncbi.nlm.nih.gov/snp/}{NIH-NCBI dbSNP
#'   database}.
#'   \item "cosmic curate": \href{https://cancer.sanger.ac.uk/cosmic/}{COSMIC
#'   (the Catalogue of Somatic Mutations in Cancer)}
#'   \item "ClinVar": \href{https://www.ncbi.nlm.nih.gov/clinvar/}{NIH-NCBI
#'   ClinVar}
#'   }
#' @param db_id The variation ID in a Cross-reference (external) database.
#'    You can provide up to 20 values.
#' @param save_peff Logical or Character:\itemize{
#'   \item FALSE: (default) Do not save PEFF file, just return as a list object.
#'   \item TRUE: Save as PEFF file to an automatically-generated path.
#'   \item Character string: A valid file path to save the PEFF file.}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List where each element corresponds to one UniProt entity returned
#'   by your search query. The element itself is a sub-list containing all
#'   information that UniProt has about that Variation.
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
#' rba_uniprot_variation_search(accession = "P05067")
#' }
#' \donttest{
#' rba_uniprot_variation_search(disease = "alzheimer disease, 18")
#' }
#' \donttest{
#' rba_uniprot_variation_search(disease = "alzheimer",
#'     wild_type = "A", alternative_sequence = "T")
#' }
#'
#' @family "UniProt - Variation"
#' @export
rba_uniprot_variation_search <- function(accession = NA,
                                         source_type = NA,
                                         consequence_type = NA,
                                         wild_type = NA,
                                         alternative_sequence = NA,
                                         location = NA,
                                         disease = NA,
                                         omim = NA,
                                         evidence = NA,
                                         taxid = NA,
                                         db_type = NA,
                                         db_id = NA,
                                         save_peff = FALSE,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "source_type",
                             class = "character",
                             val = c("uniprot",
                                     "large scale study",
                                     "mixed"),
                             max_len = 2),
                        list(arg = "consequence_type",
                             class = "character",
                             val = c("missense",
                                     "stop gained",
                                     "stop lost")),
                        list(arg = "wild_type",
                             class = "character",
                             max_len = 20),
                        list(arg = "alternative_sequence",
                             class = "character",
                             max_len = 20),
                        list(arg = "location",
                             class = "character"),
                        list(arg = "disease",
                             class = "character"),
                        list(arg = "omim",
                             class = "character",
                             max_len = 20),
                        list(arg = "evidence",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "db_type",
                             class = "character",
                             max_len = 2),
                        list(arg = "db_id",
                             class = "character",
                             max_len = 20),
                        list(arg = "save_peff",
                             class = c("logical",
                                       "character"))),
            cond = list(list(quote(all(is.na(accession), is.na(disease),
                                       is.na(omim), is.na(evidence),
                                       is.na(taxid), is.na(db_type),
                                       is.na(db_id))),
                             "You should provide at least one of: accession, disease, omim, evidence, taxid, db_type or db_id"))
  )

  .msg("Searching UniProt and retrieving natural variations of proteins that match your provided inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                any(!is.na(accession)),
                                paste0(accession,
                                       collapse = ",")),
                           list("sourcetype",
                                any(!is.na(source_type)),
                                paste0(source_type,
                                       collapse = ",")),
                           list("consequencetype",
                                any(!is.na(consequence_type)),
                                paste0(consequence_type,
                                       collapse = ",")),
                           list("wildtype",
                                any(!is.na(wild_type)),
                                paste0(wild_type,
                                       collapse = ",")),
                           list("alternativesequence",
                                any(!is.na(alternative_sequence)),
                                paste0(alternative_sequence,
                                       collapse = ",")),
                           list("location",
                                !is.na(location),
                                location),
                           list("disease",
                                !is.na(disease),
                                disease),
                           list("omim",
                                any(!is.na(omim)),
                                paste0(omim,
                                       collapse = ",")),
                           list("evidence",
                                any(!is.na(evidence)),
                                paste0(evidence,
                                       collapse = ",")),
                           list("taxid",
                                any(!is.na(taxid)),
                                paste0(taxid,
                                       collapse = ",")),
                           list("dbtype",
                                any(!is.na(db_type)),
                                paste0(db_type,
                                       collapse = ",")),
                           list("dbid",
                                any(!is.na(db_id)),
                                paste0(db_type,
                                       collapse = ",")))
  ## Build Function-Specific Call
  save_to <- ifelse(isFALSE(save_peff),
                    yes = .rba_file(file = "uniprot_variation.json"),
                    no = .rba_file(file = "uniprot_variation.peff",
                                   save_to = save_peff))
  obj_parser_input <- list("json->list",
                           .rba_uniprot_search_namer)

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "variation"),
                          query = call_query,
                          save_to = save_to,
                          file_accept = "text/x-peff",
                          file_parser = "text->chr",
                          obj_accept = "application/json",
                          obj_parser = obj_parser_input)
  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get natural variants in UniProt by NIH-NCBI SNP database identifier
#'
#' Retrieve natural variant annotations of a sequence using UniProt protein
#'   accession, dbSNP or HGVS expression.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/variation/dbsnp/{dbid}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/variation/hgvs/{hgvs}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/variation/{accession}"
#'
#' @param id An ID which can be either a
#'   \href{https://www.uniprot.org/help/accession_numbers}{UniProt primary or
#'   secondary accession}, NIH-NCBI dbSNP ID or HGVS expression.
#'   \href{https://www.ncbi.nlm.nih.gov/snp/}{NIH-NCBI dbSNP id} or
#'   \href{https://www.ncbi.nlm.nih.gov/variation/hgvs/}{HGVS Expression}.
#' @param id_type The type of provided ID argument, one of:
#'   \href{https://www.uniprot.org/help/accession_numbers}{"uniprot"},
#'   \href{https://www.ncbi.nlm.nih.gov/snp/}{"dbsnp"} or
#'   \href{https://www.ncbi.nlm.nih.gov/variation/hgvs/}{"hgvs"}
#' @param source_type Variation's source type. You can choose up to two of:
#'   "UniProt", "large scale study" and/or "mixed".
#' @param consequence_type Variation's consequence type. You can choose up to
#'   two of: "missense", "stop gained" or "stop lost".
#' @param wild_type Wild type amino acid. Accepted values are IUPAC
#'   single-letter amino acid (e.g. D for	Aspartic acid) and "*" for stop
#'   codon. You can provide up to 20 values.
#' @param alternative_sequence Alternative amino acid. Accepted values are IUPAC
#'   single-letter amino acid (e.g. D for	Aspartic acid) and "*" for stop codon
#'   and "-" for deletion. You can provide up to 20 values.
#' @param location A valid amino acid range (e.g. 10-25) within the sequence
#'   range where the variation occurs. You can provide up to 20 values.
#' @param save_peff Logical or Character:\itemize{
#'   \item FALSE: (default) Do not save PEFF file, just return as a list object.
#'   \item TRUE: Save as PEFF file to an automatically-generated path.
#'   \item Character string: A valid file path to save the PEFF file.}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list where each element is a list that corresponds to a UniProt
#'   protein entry.
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
#' rba_uniprot_variation(id = "rs121434451", id_type = "dbsnp")
#' }
#' \donttest{
#' rba_uniprot_variation(id = "NC_000008.11:g.22119227C>T", id_type = "hgvs")
#' }
#' \donttest{
#' rba_uniprot_variation(id = "O43593", id_type = "uniprot")
#' }
#'
#' @family "UniProt - Variation"
#' @export
rba_uniprot_variation <- function(id,
                                  id_type,
                                  source_type = NA,
                                  consequence_type = NA,
                                  wild_type = NA,
                                  alternative_sequence = NA,
                                  location = NA,
                                  save_peff = FALSE,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "id",
                             class = "character"),
                        list(arg = "id_type",
                             class = "character",
                             val = c("uniprot",
                                     "dbsnp",
                                     "hgvs")),
                        list(arg = "source_type",
                             class = "character",
                             val = c("uniprot",
                                     "large scale study",
                                     "mixed"),
                             max_len = 2),
                        list(arg = "consequence_type",
                             class = "character",
                             val = c("missense",
                                     "stop gained",
                                     "stop lost"),
                             max_len = 2),
                        list(arg = "wild_type",
                             class = "character",
                             max_len = 20),
                        list(arg = "alternative_sequence",
                             class = "character",
                             max_len = 20),
                        list(arg = "location",
                             class = "character"),
                        list(arg = "save_peff",
                             class = c("logical",
                                       "character"))))

  .msg("Retrieving Natural variant of %s.",
       ifelse(id_type == "uniprot",
              yes = paste0("UniProt protein ", id),
              no = paste0(id_type, " id ", id)))
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("sourcetype",
                                any(!is.na(source_type)),
                                paste0(source_type,
                                       collapse = ",")),
                           list("consequencetype",
                                any(!is.na(consequence_type)),
                                paste0(consequence_type,
                                       collapse = ",")),
                           list("wildtype",
                                any(!is.na(wild_type)),
                                paste0(wild_type,
                                       collapse = ",")),
                           list("alternativesequence",
                                any(!is.na(alternative_sequence)),
                                paste0(alternative_sequence,
                                       collapse = ",")),
                           list("location",
                                !is.na(location),
                                location))
  ## Build Function-Specific Call
  file_name <- sprintf("uniprot_variation_%s.%s",
                       id_type, ifelse(isFALSE(save_peff), "json", "peff"))
  save_to <- ifelse(isFALSE(save_peff),
                    yes = .rba_file(file = file_name),
                    no = .rba_file(file = file_name,
                                   save_to = save_peff))
  path_input <- switch(id_type,
                       "uniprot" = paste0(.rba_stg("uniprot", "pth"),
                                          "variation/", id),
                       "hgvs" = paste0(.rba_stg("uniprot", "pth"),
                                       "variation/hgvs/", id),
                       "dbsnp" = paste0(.rba_stg("uniprot", "pth"),
                                        "variation/dbsnp/", id))

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          query = call_query,
                          save_to = save_to,
                          file_accept = "text/x-peff",
                          file_parser = "text->chr",
                          obj_accept = "application/json",
                          obj_parser = "json->list")

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Proteomics Endpoints ####

#' Search Proteomics Peptides in UniProt
#'
#' UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can search for  proteomics
#'   peptides that has been map to UniProt proteins. You may also refine your
#'   search with modifiers such as data_source, peptide etc. refer to
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr see also: \href{https://www.uniprot.org/help/proteomics}{Mass
#'   spectrometry-based proteomics data in UniProtKB}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param data_source  Proteomics data source. You can choose up to two of:
#'   \itemize{
#'   \item \href{https://www.uniprot.org/database/DB-0186}{"MaxQB"}
#'   \item \href{https://www.uniprot.org/database/DB-0071}{"PeptideAtlas"}
#'   \item \href{https://www.uniprot.org/database/DB-0205}{"EPD"}
#'   \item \href{https://www.uniprot.org/database/DB-0229}{"ProteomicsDB"}
#'   }
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can provide up to 20 taxon IDs.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can provide up to 100 UPIDs.
#' @param peptide Peptide sequence(s). You can provide up to 20 sequences.
#' @param unique Logical: Should the results be filtered based on the
#'   Peptide's uniqueness (the fact that a peptide maps to only 1 protein). If
#'   TRUE, Only unique peptides will be returned, if FALSE only un-unique
#'   peptides will be returned; If NA (default) the results will not be
#'   filtered based on this.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list Where each element correspond to a UniProt protein and
#'   proteomics peptides are organized under the "features" sub-list.
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
#' rba_uniprot_proteomics_search(peptide = "MEDYTKIEK")
#' }
#' \donttest{
#' rba_uniprot_proteomics_search(peptide = "MEDYTKIEK")
#' }
#' \dontrun{
#' ### this will generate a very large response!
#'   rba_uniprot_proteomics_search(taxid = 9606,
#'   data_source = "PeptideAtlas",
#'   progress = TRUE, timeout = 999999, unique = TRUE)
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_search <- function(accession = NA,
                                          data_source = NA,
                                          taxid = NA,
                                          upid = NA,
                                          peptide = NA,
                                          unique = NA,
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
                             vals = c("MaxQB",
                                      "PeptideAtlas",
                                      "EPD",
                                      "ProteomicsDB")),
                        list(arg = "peptide",
                             class = "character",
                             max_len = 20),
                        list(arg = "unique",
                             class = "logical"))
  )

  .msg("Searching UniProt and retrieving proteomics peptides features of proteins that match your provided inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                any(!is.na(accession)),
                                paste0(accession,
                                       collapse = ",")),
                           list("taxid",
                                any(!is.na(taxid)),
                                paste0(taxid,
                                       collapse = ",")),
                           list("upid",
                                any(!is.na(upid)),
                                paste0(upid,
                                       collapse = ",")),
                           list("data_source",
                                any(!is.na(data_source)),
                                paste0(data_source,
                                       collapse = ",")),
                           list("peptide",
                                any(!is.na(peptide)),
                                paste0(peptide,
                                       collapse = ",")),
                           list("unique",
                                !is.na(unique),
                                ifelse(unique, "true", "false")))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_proteomics_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Proteomics Peptides Mapped to UniProt Protein
#'
#' UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can retrieve all the proteomics
#'   peptides features that has been map to a given UniProt protein's sequence.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/{accession}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list containing the proteomics peptides features of your provided
#'   UniProt protein's sequence.
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
#' rba_uniprot_proteomics(accession = "P25942")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics <- function(accession,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             len = 1))
  )

  .msg("Retrieving proteomics peptides features mapped to the sequence of protein %s.",
       accession)
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "proteomics/",
                                        accession),
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_proteomics.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Antigen Endpoints ####

#' Search Antigens in UniProt
#'
#' UniProt maps Antigenic (Antibody-binding) features from different sources
#'   to the proteins' sequences. Using this function, you can search for
#'   Antigenic sequences that has been map to UniProt proteins. You may also
#'   refine your search with modifiers such as score etc. refer to
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/antigen"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param antigen_sequence Protein sequence in the antigenic site.
#' @param antigen_id Human Protein Atlas (HPA) antigen ID. You can provide up to
#'   20 IDs.
#' @param ensembl_id Ensembl Stable Transcript ID. You can provide up to
#'   20 IDs.
#' @param match_score (Numeric) Minimum alignment score for the antigen
#'   sequence and the target protein sequence.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list Where each element correspond to a UniProt protein (search
#'  hit) and Antigenic features are organized under the "features" sub-list.
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
#' rba_uniprot_antigens_search(antigen_id = "HPA001060")
#' }
#'
#' @family "UniProt - Antigen"
#' @export
rba_uniprot_antigens_search <- function(accession = NA,
                                        antigen_sequence = NA,
                                        antigen_id = NA,
                                        ensembl_id = NA,
                                        match_score = NA,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "antigen_sequence",
                             class = "character"),
                        list(arg = "antigen_id",
                             class = "character",
                             max_len = 20),
                        list(arg = "ensembl_id",
                             class = "character",
                             max_len = 20),
                        list(arg = "match_score",
                             class = "numeric"))
  )

  .msg("Searching UniProt and retrieving antigenic features of proteins that match your provided inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                any(!is.na(accession)),
                                paste0(accession,
                                       collapse = ",")),
                           list("antigen_sequence",
                                !is.na(antigen_sequence),
                                antigen_sequence),
                           list("antigen_id",
                                any(!is.na(antigen_id)),
                                paste0(antigen_id,
                                       collapse = ",")),
                           list("ensembl_id",
                                any(!is.na(ensembl_id)),
                                paste0(ensembl_id,
                                       collapse = ",")),
                           list("match_score",
                                !is.na(match_score),
                                match_score))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "antigen"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_antigen_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Antigens by UniProt Accession
#'
#' UniProt maps Antigenic features from different sources to the proteins'
#'   sequences. Using this function, you can retrieve all the Antigenic
#'   features that has been map to a given UniProt protein's sequence.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/antigen/{accession}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s).
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list containing the Antigenic features of your provided
#'   UniProt protein's sequence.
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
#' rba_uniprot_antigens("P04626")
#' }
#'
#' @family "UniProt - Antigen"
#' @export
rba_uniprot_antigens <- function(accession,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             len = 1))
  )

  .msg("Retrieving Antigenic features mapped to the sequence of protein %s.",
       accession)
  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "antigen/",
                                        accession),
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_antigen.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
