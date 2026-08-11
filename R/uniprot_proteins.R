#' Name UniProt search-result elements
#'
#' Name each result with a scalar character identifier, when every result has
#'   the requested field. If the response does not have the expected
#'   structure, return it unchanged.
#'
#' @param x List: Parsed UniProt response.
#' @param field Character: Name of the identifier field.
#'
#' @return The input object, named when possible.
#' @noRd
.rba_uniprot_search_namer <- function(x, field = "accession") {
  if (!is.list(x) || length(x) == 0L) {
    return(x)
  }

  x_names <- vapply(
    X = x,
    FUN = function(search_hit) {
      if (is.list(search_hit)) {
        identifier <- search_hit[[field]]
      } else {
        identifier <- NULL
      }

      if (
        is.character(identifier) &&
        length(identifier) == 1L &&
        !is.na(identifier) &&
        nzchar(identifier)
      ) {
        return(identifier)
      }
      return(NA_character_)
    },
    FUN.VALUE = character(1)
  )

  if (!anyNA(x_names)) {
    names(x) <- x_names
  }
  return(x)
}

#### Proteins Endpoints ####

#' Search UniProt entries
#'
#' Search and retrieve UniProt Knowledgebase (UniProtKB) protein entries by
#'   accession, annotation, gene, organism, sequence properties, or other
#'   supported criteria.
#'
#' At least one primary search criterion is required. The value
#'   \code{isoform = 1} can be used by itself; \code{reviewed} and the other
#'   \code{isoform} values only refine another criterion.
#'
#' UniProtKB entries are grouped into two sections:\enumerate{
#'   \item Reviewed (Swiss-Prot): Manually annotated records with information
#'   extracted from literature and curator-evaluated computational analysis.
#'   \item Unreviewed (TrEMBL): Computationally analyzed records that await
#'   full manual annotation.}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteins"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param reviewed Logical: (optional) If \code{TRUE}, return only reviewed
#'   Swiss-Prot entries. If \code{FALSE}, return only unreviewed TrEMBL
#'   entries. This is a refining filter and cannot be the sole search
#'   criterion.
#' @param isoform Numeric: (optional) One of:\itemize{
#'   \item 0: Exclude isoforms; this only refines another criterion.
#'   \item 1: Return isoforms only; this can be a stand-alone criterion.
#'   \item 2: Return canonical entries and isoforms; this only refines another
#'   criterion.}
#'   See \href{https://www.uniprot.org/help/alternative_products}{alternative
#'   products}.
#' @param go_term Character: (optional) Limit the search to entries associated
#'   with your
#'   supplied GO
#'   (\href{https://www.uniprot.org/help/gene_ontology}{Gene Ontology}) term.
#'   Supply either a GO ID or a character string partially or fully matching
#'   the term, e.g. "GO:0001776" or "leukocyte homeostasis". If you
#'   supply "leukocyte", any term containing that word will be included,
#'   e.g. "leukocyte chemotaxis" or "leukocyte activation".
#' @param keyword Character: (optional) Limit the search to entries that contain
#'   your
#'   supplied keyword. See
#'   \href{https://www.uniprot.org/keywords/}{UniProt Keywords}.
#' @param ec Character: (optional)
#'   \href{https://enzyme.expasy.org/}{EC (Enzyme Commission) number(s)}.
#'   You can supply up to 20 EC numbers.
#' @param gene Character: (optional)
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt gene name(s)}.
#'   You can supply up to 20 gene names. For example, if you supply "CD40",
#'   "CD40 ligand" will also be included.
#' @param exact_gene Character: (optional)
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt exact gene name(s)}.
#'   You can supply up to 20 exact gene names. For example, if you supply
#'   "CD40", "CD40 ligand" will not be included in the results.
#' @param protein Character: (optional)
#'   \href{https://www.uniprot.org/help/protein_names}{UniProt protein name}.
#' @param organism Character: (optional) Organism name.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param pubmed Character or Numeric: (optional) PubMed ID(s) cited by the
#'   returned entries. You can supply up to 20 IDs.
#' @param seq_length Character or Numeric: (optional) An exact sequence length
#'   (e.g. 150)
#'   or a range of sequence lengths (e.g. "130-158").
#' @param md5 Character: (optional) A 32-character hexadecimal sequence MD5
#'   checksum.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list named by UniProt accession. Each element contains one
#'   matching UniProtKB entry.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
#' rba_uniprot_proteins_search(
#'   keyword = "Inhibition of host chemokines by virus"
#' )
#' }
#' \donttest{
#' rba_uniprot_proteins_search(keyword = "chemokines")
#' }
#'
#' @family "UniProt - Proteins"
#' @export
rba_uniprot_proteins_search <- function(accession = NULL,
                                        reviewed = NULL,
                                        isoform = NULL,
                                        go_term = NULL,
                                        keyword = NULL,
                                        ec = NULL,
                                        gene = NULL,
                                        exact_gene = NULL,
                                        protein = NULL,
                                        organism = NULL,
                                        taxid = NULL,
                                        pubmed = NULL,
                                        seq_length = NULL,
                                        md5 = NULL,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "reviewed", class = "logical", len = 1L),
      list(
        arg = "isoform", class = c("numeric", "integer"), len = 1L,
        val = c(0, 1, 2)
      ),
      list(arg = "go_term", class = "character", len = 1L),
      list(arg = "keyword", class = "character", len = 1L),
      list(arg = "ec", class = "character", max_len = 20),
      list(arg = "gene", class = "character", max_len = 20),
      list(arg = "exact_gene", class = "character", max_len = 20),
      list(arg = "protein", class = "character", len = 1L),
      list(arg = "organism", class = "character", len = 1L),
      list(
        arg = "taxid", class = c("numeric", "integer"), max_len = 20,
        min_val = 1
      ),
      list(
        arg = "pubmed", class = c("character", "numeric", "integer"),
        max_len = 20, regex = "^[1-9]\\d*$"
      ),
      list(
        arg = "seq_length", class = c("character", "numeric", "integer"),
        len = 1L, regex = "^[1-9]\\d*(?:-[1-9]\\d*)?$"
      ),
      list(
        arg = "md5", class = "character", len = 1L,
        regex = "^[[:xdigit:]]{32}$"
      )
    ),
    cond = list(
      list(
        quote(
          all(
            is.null(accession), is.null(go_term),
            is.null(keyword), is.null(ec), is.null(gene),
            is.null(exact_gene), is.null(protein), is.null(organism),
            is.null(taxid), is.null(pubmed), is.null(seq_length),
            is.null(md5)
          ) &&
            (is.null(isoform) || isoform != 1)
        ),
        paste0(
          "Supply at least one primary search criterion: accession, go_term, ",
          "keyword, ec, gene, exact_gene, protein, organism, taxid, pubmed, ",
          "seq_length, md5, or isoform = 1. `reviewed`, `isoform = 0`, and ",
          "`isoform = 2` only refine another criterion."
        )
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      ),
      list(
        quote(
          is.character(seq_length) &&
            grepl("-", seq_length, fixed = TRUE) &&
            diff(as.numeric(strsplit(seq_length, "-", fixed = TRUE)[[1]])) < 0
        ),
        "The start of a `seq_length` range cannot exceed its end."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("reviewed", !is.null(reviewed), ifelse(reviewed, "true", "false")),
    list("isoform", !is.null(isoform), isoform),
    list("goterms", !is.null(go_term), go_term),
    list("keywords", !is.null(keyword), keyword),
    list("ec", !is.null(ec), paste0(ec, collapse = ",")),
    list("gene", !is.null(gene), paste0(gene, collapse = ",")),
    list(
      "exact_gene", !is.null(exact_gene),
      paste0(exact_gene, collapse = ",")
    ),
    list("protein", !is.null(protein), protein),
    list("organism", !is.null(organism), organism),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("pubmed", !is.null(pubmed), paste0(pubmed, collapse = ",")),
    list("seqLength", !is.null(seq_length), seq_length),
    list("md5", !is.null(md5), md5)
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteins"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_proteins_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt entry by accession
#'
#' Retrieve a UniProtKB entry by accession. Alternatively, retrieve its
#'   isoforms or interaction partners by setting \code{isoforms = TRUE} or
#'   \code{interaction = TRUE}. These two modes are mutually exclusive.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteins/\{accession\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/proteins/interaction/\{accession\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/proteins/\{accession\}/isoforms"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param interaction Logical: (default = \code{FALSE}) Retrieve
#'   \href{https://www.uniprot.org/help/interaction_section}{interaction}
#'   partners instead of the entry itself?
#' @param isoforms Logical: (default = \code{FALSE}) Retrieve
#'   \href{https://www.uniprot.org/help/alternative_products}{isoforms} of your
#'   supplied UniProt entry instead of the canonical entry itself?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the requested UniProtKB entry. Isoform and
#'   interaction results are lists named by accession.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L),
      list(
        arg = "interaction", class = "logical", len = 1L,
        no_null = TRUE
      ),
      list(
        arg = "isoforms", class = "logical", len = 1L,
        no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(isTRUE(interaction) && isTRUE(isoforms)),
        "Only one of `interaction` and `isoforms` can be TRUE."
      )
    )
  )

  .msg(
    "Retrieving %sUniProt entry with accession %s.",
    if (isTRUE(interaction)) {
      "interactions for "
    } else if (isTRUE(isoforms)) {
      "isoforms of "
    } else {
      ""
    },
    accession
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%s%s/%s",
    .rba_stg("uniprot", "pth"),
    ifelse(isTRUE(interaction), yes = "proteins/interaction", no = "proteins"),
    accession
  )

  if (isTRUE(isoforms)) {
    path_input <- paste0(path_input, "/isoforms")
  }

  if (isTRUE(interaction) || isTRUE(isoforms)) {
    parser_input <- list("json->list", .rba_uniprot_search_namer)
  } else {
    parser_input <- "json->list_simp"
  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_proteins.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt Entry by UniProt Cross-Reference Database and ID
#'
#' UniProt cross-references connect protein entries with identifiers in
#'   \href{https://www.uniprot.org/database/}{external databases}. Retrieve
#'   UniProtKB entries associated with an identifier from one of these
#'   databases.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteins/\{dbtype\}:\{dbid\}"
#'
#' @param db_id Character: Protein identifier in the cross-reference database.
#' @param db_name Character:
#'   \href{https://www.uniprot.org/database/}{Cross-reference database name}.
#' @param reviewed Logical: (optional) If \code{TRUE}, return only reviewed
#'   Swiss-Prot entries. If \code{FALSE}, return only unreviewed TrEMBL entries.
#' @param isoform Numeric: (optional) One of:\itemize{
#'   \item 0: Exclude isoforms.
#'   \item 1: Return isoforms only.}
#'   See \href{https://www.uniprot.org/help/alternative_products}{alternative
#'   products}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list named by UniProt accession. Each element is a UniProtKB entry
#'   corresponding to the supplied cross-reference identifier.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
                                          reviewed = NULL,
                                          isoform = NULL,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "db_name", class = "character", len = 1L),
      list(arg = "db_id", class = "character", len = 1L),
      list(arg = "reviewed", class = "logical", len = 1L),
      list(
        arg = "isoform", class = c("numeric", "integer"), len = 1L,
        val = c(0, 1)
      )
    )
  )

  .msg(
    "Retrieving UniProt entities that correspond to ID %s in database %s.",
    db_id, db_name
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("reviewed", !is.null(reviewed), ifelse(reviewed, "true", "false")),
    list("isoform", !is.null(isoform), isoform)
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = sprintf(
      "%sproteins/%s:%s", .rba_stg("uniprot", "pth"), db_name, db_id
    ),
    query = call_query,
    accept = "application/json",
    parser = list("json->list", .rba_uniprot_search_namer),
    save_to = .rba_file("uniprot_proteins_crossref.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Features Endpoints ####

#' Search UniProt protein sequence features
#'
#' \href{https://www.uniprot.org/help/sequence_annotation}{UniProt sequence
#'   features} describe biologically relevant sites and regions within protein
#'   sequences. Search and retrieve these annotations using protein, gene,
#'   organism, and annotation criteria.
#'
#' At least one of \code{accession}, \code{gene}, \code{exact_gene},
#'   \code{protein}, \code{organism}, or \code{taxid} is required. The
#'   remaining arguments refine those primary criteria.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/features"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param gene Character: (optional)
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can supply up to 20 gene names. For example, if you supply
#'   "CD40", "CD40 ligand" will also be included.
#' @param exact_gene Character: (optional)
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt
#'   exact gene name(s)}. You can supply up to 20 exact gene names. For example,
#'   if you supply "CD40", "CD40 ligand" will not be included in the results.
#' @param protein Character: (optional)
#'   \href{https://www.uniprot.org/help/protein_names}{UniProt
#'   protein name}.
#' @param reviewed Logical: (optional) If \code{TRUE}, return only reviewed
#'   Swiss-Prot entries. If \code{FALSE}, return only unreviewed TrEMBL entries.
#' @param organism Character: (optional) Organism name.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param categories Character: (optional)
#'   \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (features)} categories. Accepted values
#'   are: "MOLECULE_PROCESSING", "TOPOLOGY", "SEQUENCE_INFORMATION",
#'   "STRUCTURAL", "DOMAINS_AND_SITES", "PTM", "VARIANTS" and/or "MUTAGENESIS".
#'   You can supply up to 20 categories.
#' @param types Character: (optional)
#'   \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (features)} types. Accepted values
#'   are: "INIT_MET", "SIGNAL", "PROPEP", "TRANSIT", "CHAIN", "PEPTIDE",
#'   "TOPO_DOM", "TRANSMEM", "DOMAIN", "REPEAT", "ZN_FING", "DNA_BIND",
#'   "REGION", "COILED", "MOTIF", "COMPBIAS", "ACT_SITE", "BINDING",
#'   "SITE", "NON_STD", "MOD_RES", "LIPID",
#'   "CARBOHYD", "DISULFID", "CROSSLNK", "VAR_SEQ", "VARIANT", "MUTAGEN",
#'   "UNSURE", "CONFLICT", "NON_CONS", "NON_TER", "HELIX", "TURN", "STRAND"
#'   and/or "INTRAMEM". You can supply up to 20 types.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list named by UniProt accession. Each element contains the entry
#'   metadata, sequence, and matching annotations in its \code{features}
#'   element.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
rba_uniprot_features_search <- function(accession = NULL,
                                        gene = NULL,
                                        exact_gene = NULL,
                                        protein = NULL,
                                        reviewed = NULL,
                                        organism = NULL,
                                        taxid = NULL,
                                        categories = NULL,
                                        types = NULL,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "gene", class = "character", max_len = 20),
      list(arg = "exact_gene", class = "character", max_len = 20),
      list(arg = "protein", class = "character", len = 1L),
      list(arg = "reviewed", class = "logical", len = 1L),
      list(arg = "organism", class = "character", len = 1L),
      list(
        arg = "taxid", class = c("numeric", "integer"), max_len = 20,
        min_val = 1
      ),
      list(
        arg = "categories", class = "character", max_len = 20,
        val = c("MOLECULE_PROCESSING",
                "TOPOLOGY",
                "SEQUENCE_INFORMATION",
                "STRUCTURAL",
                "DOMAINS_AND_SITES",
                "PTM",
                "VARIANTS",
                "MUTAGENESIS")
      ),
      list(
        arg = "types", class = "character", max_len = 20,
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
                "ZN_FING",
                "DNA_BIND",
                "REGION",
                "COILED",
                "MOTIF",
                "COMPBIAS",
                "ACT_SITE",
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
      )
    ),
    cond = list(
      list(
        quote(
          all(
            is.null(accession), is.null(gene), is.null(exact_gene),
            is.null(protein), is.null(organism), is.null(taxid)
          )
        ),
        "Supply at least one primary search criterion: accession, gene, exact_gene, protein, organism, or taxid."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving sequence annotations (features) of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("gene", !is.null(gene), paste0(gene, collapse = ",")),
    list(
      "exact_gene", !is.null(exact_gene),
      paste0(exact_gene, collapse = ",")
    ),
    list("protein", !is.null(protein), protein),
    list("reviewed", !is.null(reviewed), ifelse(reviewed, "true", "false")),
    list("organism", !is.null(organism), organism),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list(
      "categories", !is.null(categories),
      paste0(categories, collapse = ",")
    ),
    list("types", !is.null(types), paste0(types, collapse = ","))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "features"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_features_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search UniProt protein sequence features by description
#'
#' Search for terms in the descriptions of one specified
#'   \href{https://www.uniprot.org/help/sequence_annotation}{UniProt sequence
#'   annotation (feature)} type. The function returns protein entries with at
#'   least one feature of that type whose description matches a supplied term.
#'
#' The \code{type} and \code{terms} arguments determine which protein entries
#'   match the search. The optional \code{categories} and \code{types}
#'   arguments only select the annotations included in each returned entry;
#'   they do not change which entries match.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/features/type/\{type\}"
#'
#' @param terms Character: Terms to find in feature descriptions. You can
#'   supply up to 20 terms.
#' @param type Character:
#'   \href{https://www.uniprot.org/help/sequence_annotation}{Sequence
#'   annotation (feature)} type whose descriptions are searched. One of:
#'   "INIT_MET", "SIGNAL", "PROPEP", "TRANSIT", "CHAIN", "PEPTIDE",
#'   "TOPO_DOM", "TRANSMEM", "DOMAIN", "REPEAT", "ZN_FING", "DNA_BIND",
#'   "REGION", "COILED", "MOTIF", "COMPBIAS", "ACT_SITE", "BINDING",
#'   "SITE", "NON_STD", "MOD_RES", "LIPID", "CARBOHYD", "DISULFID",
#'   "CROSSLNK", "VAR_SEQ", "VARIANT", "MUTAGEN", "UNSURE", "CONFLICT",
#'   "NON_CONS", "NON_TER", "HELIX", "TURN", "STRAND", or "INTRAMEM".
#' @param categories Character: (optional)
#'   \href{https://www.uniprot.org/help/sequence_annotation}{Sequence
#'   annotation (feature)} categories to include in each returned entry.
#'   Accepted values are: "MOLECULE_PROCESSING", "TOPOLOGY",
#'   "SEQUENCE_INFORMATION", "STRUCTURAL", "DOMAINS_AND_SITES", "PTM",
#'   "VARIANTS" and/or "MUTAGENESIS". You can supply up to 20 categories.
#' @param types Character: (optional)
#'   \href{https://www.uniprot.org/help/sequence_annotation}{Sequence
#'   annotation (feature)} types to include in each returned entry. Accepted
#'   values are the same as for \code{type}. You can supply up to 20 types.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list named by UniProt accession. Each element contains the entry
#'   metadata, sequence, and the annotations selected by \code{categories} and
#'   \code{types} in its \code{features} element. Without those optional
#'   filters, all annotations of each matching entry are returned.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_features_type(
#'     terms = "Alzheimer", type = "VARIANT", types = "VARIANT")
#' }
#'
#' @family "UniProt - Features"
#' @export
rba_uniprot_features_type <- function(terms,
                                      type,
                                      categories = NULL,
                                      types = NULL,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "terms", class = "character", min_len = 1L,
        max_len = 20
      ),
      list(
        arg = "type", class = "character", len = 1L,
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
                "ZN_FING",
                "DNA_BIND",
                "REGION",
                "COILED",
                "MOTIF",
                "COMPBIAS",
                "ACT_SITE",
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
      ),
      list(
        arg = "categories", class = "character", min_len = 1L,
        max_len = 20,
        val = c("MOLECULE_PROCESSING",
                "TOPOLOGY",
                "SEQUENCE_INFORMATION",
                "STRUCTURAL",
                "DOMAINS_AND_SITES",
                "PTM",
                "VARIANTS",
                "MUTAGENESIS")
      ),
      list(
        arg = "types", class = "character", min_len = 1L,
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
                "ZN_FING",
                "DNA_BIND",
                "REGION",
                "COILED",
                "MOTIF",
                "COMPBIAS",
                "ACT_SITE",
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
      )
    )
  )

  .msg(
    "Searching UniProt for %s features with descriptions matching the supplied terms.",
    type
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(
      "size" = "-1",
      "terms" = paste0(terms, collapse = ",")
    ),
    list(
      "categories", !is.null(categories),
      paste0(categories, collapse = ",")
    ),
    list("types", !is.null(types), paste0(types, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "features/type/", type),
    query = call_query,
    accept = "application/json",
    parser = list("json->list", .rba_uniprot_search_namer),
    save_to = .rba_file("uniprot_features_type.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt protein sequence features by accession
#'
#' \href{https://www.uniprot.org/help/sequence_annotation}{UniProt sequence
#'   features} describe biologically relevant sites and regions within a
#'   protein sequence. Retrieve these annotations for one UniProtKB accession,
#'   optionally filtered by annotation type, category, or amino-acid range.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/features/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param types Character: (optional)
#'   \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (features)} types. Accepted values
#'   are: "INIT_MET", "SIGNAL", "PROPEP", "TRANSIT", "CHAIN", "PEPTIDE",
#'   "TOPO_DOM", "TRANSMEM", "DOMAIN", "REPEAT", "ZN_FING", "DNA_BIND",
#'   "REGION", "COILED", "MOTIF", "COMPBIAS", "ACT_SITE", "BINDING",
#'   "SITE", "NON_STD", "MOD_RES", "LIPID",
#'   "CARBOHYD", "DISULFID", "CROSSLNK", "VAR_SEQ", "VARIANT", "MUTAGEN",
#'   "UNSURE", "CONFLICT", "NON_CONS", "NON_TER", "HELIX", "TURN", "STRAND"
#'   and/or "INTRAMEM". You can supply up to 20 types.
#' @param categories Character: (optional)
#'   \href{https://www.uniprot.org/help/sequence_annotation}{
#'   Sequence annotation (features)} categories. Accepted values
#'   are: "MOLECULE_PROCESSING", "TOPOLOGY", "SEQUENCE_INFORMATION",
#'   "STRUCTURAL", "DOMAINS_AND_SITES", "PTM", "VARIANTS" and/or "MUTAGENESIS".
#'   You can supply up to 20 categories.
#' @param location Character: (optional) Amino-acid range in
#'   \code{"begin-end"} format,
#'   e.g. \code{"35-70"}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the entry metadata, sequence, and matching
#'   annotations in its \code{features} element.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
                                 types = NULL,
                                 categories = NULL,
                                 location = NULL,
                                 ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L),
      list(
        arg = "types", class = "character", max_len = 20,
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
                "ZN_FING",
                "DNA_BIND",
                "REGION",
                "COILED",
                "MOTIF",
                "COMPBIAS",
                "ACT_SITE",
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
      ),
      list(
        arg = "categories", class = "character", max_len = 20,
        val = c("MOLECULE_PROCESSING",
                "TOPOLOGY",
                "SEQUENCE_INFORMATION",
                "STRUCTURAL",
                "DOMAINS_AND_SITES",
                "PTM",
                "VARIANTS",
                "MUTAGENESIS")
      ),
      list(
        arg = "location", class = "character", len = 1L,
        regex = "^[1-9]\\d*-[1-9]\\d*$"
      )
    ),
    cond = list(
      list(
        quote(
          !is.null(location) &&
            diff(as.numeric(strsplit(location, "-", fixed = TRUE)[[1]])) < 0
        ),
        "The start of `location` cannot exceed its end."
      )
    )
  )

  .msg(
    "Retrieving sequence annotations (features) of protein %s.",
    accession
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list(
      "categories", !is.null(categories),
      paste0(categories, collapse = ",")
    ),
    list("types", !is.null(types), paste0(types, collapse = ",")),
    list("location", !is.null(location), location)
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "features/", accession),
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_features.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Variation Endpoints ####

#' Search UniProt Natural Variants
#'
#' Search and retrieve
#'   \href{https://www.uniprot.org/help/variant}{natural variants} annotated
#'   on protein sequences, including variants imported from supported
#'   large-scale studies.
#'
#' At least one primary criterion is required: \code{accession},
#'   \code{disease}, \code{omim}, \code{evidence}, \code{taxid},
#'   \code{db_type}, or \code{db_id}. The other arguments refine those
#'   criteria.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/variation"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param source_type Character: (optional) Up to two variant source
#'   types:
#'   "uniprot", "large scale study", "mixed", "clinvar", "nci-tcga",
#'   "cosmic curated", "ensembl", "gnomad", "topmed", or "exac".
#' @param consequence_type Character: (optional) Up to two consequence
#'   types:
#'   "missense", "stop gained", or "stop lost".
#' @param wild_type Character: (optional) Wild-type amino acid. Accepted
#'   values are IUPAC
#'   single-letter amino acid codes and "*" for a stop
#'   codon. You can supply up to 20 values.
#' @param alternative_sequence Character: (optional) Alternative amino
#'   acid. Accepted values are
#'   IUPAC single-letter amino acid codes, "*" for a stop codon, and "-" for
#'   a deletion. You can supply up to 20 values.
#' @param location Character: (optional) A valid amino acid range (e.g. 10-25)
#'   within the sequence
#'   where the variation occurs.
#' @param disease Character: (optional)
#'   \href{https://www.uniprot.org/diseases/}{Human disease}
#'   associated with a sequence variation. Accepted values are a
#'   disease name (e.g. Alzheimer disease 18), partial disease name
#'   (Alzheimer), or disease acronym (e.g. AD).
#' @param omim Character or Numeric: (optional)
#'   \href{https://www.ncbi.nlm.nih.gov/omim}{OMIM} ID that is
#'   associated with a variation. You can supply up to 20 values.
#' @param evidence Character or Numeric: (optional) PubMed ID of a variation's
#'   \href{https://www.uniprot.org/citations/}{citation}. You can supply up
#'   to 20 values.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param db_type Character: (optional) Cross-reference database of the
#'   variation.
#'   You can supply up to two values. Examples include \code{"dbSNP"},
#'   \code{"cosmic curated"}, and \code{"ClinVar"}.
#' @param db_id Character: (optional) Variation identifier in a cross-reference
#'   database. You can
#'   supply up to 20 values.
#' @param save_peff Logical or Character: (default = \code{FALSE}) \itemize{
#'   \item FALSE: Return the parsed JSON response.
#'   \item TRUE: Save the PEFF response to an automatically generated path.
#'   \item Character string: A valid file path to save the PEFF file.}
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return With \code{save_peff = FALSE}, a list named by UniProt accession.
#'   Each element contains one matching entry and its variants. Otherwise, the
#'   PEFF response is written to disk and returned as a character string.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
rba_uniprot_variation_search <- function(accession = NULL,
                                         source_type = NULL,
                                         consequence_type = NULL,
                                         wild_type = NULL,
                                         alternative_sequence = NULL,
                                         location = NULL,
                                         disease = NULL,
                                         omim = NULL,
                                         evidence = NULL,
                                         taxid = NULL,
                                         db_type = NULL,
                                         db_id = NULL,
                                         save_peff = FALSE,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(
        arg = "source_type", class = "character", max_len = 2,
        val = c(
          "uniprot", "large scale study", "mixed", "clinvar", "nci-tcga",
          "cosmic curated", "ensembl", "gnomad", "topmed", "exac"
        )
      ),
      list(
        arg = "consequence_type", class = "character", max_len = 2,
        val = c("missense", "stop gained", "stop lost")
      ),
      list(arg = "wild_type", class = "character", max_len = 20),
      list(arg = "alternative_sequence", class = "character", max_len = 20),
      list(
        arg = "location", class = "character", len = 1L,
        regex = "^[1-9]\\d*-[1-9]\\d*$"
      ),
      list(arg = "disease", class = "character", len = 1L),
      list(
        arg = "omim", class = c("character", "numeric", "integer"),
        max_len = 20, regex = "^[1-9]\\d*$"
      ),
      list(
        arg = "evidence", class = c("character", "numeric", "integer"),
        max_len = 20, regex = "^[1-9]\\d*$"
      ),
      list(
        arg = "taxid", class = c("numeric", "integer"), max_len = 20,
        min_val = 1
      ),
      list(arg = "db_type", class = "character", max_len = 2),
      list(arg = "db_id", class = "character", max_len = 20),
      list(
        arg = "save_peff", class = c("logical", "character"), len = 1L,
        no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(all(is.null(accession), is.null(disease),
                  is.null(omim), is.null(evidence),
                  is.null(taxid), is.null(db_type),
                  is.null(db_id))),
        "Supply at least one primary search criterion: accession, disease, omim, evidence, taxid, db_type, or db_id."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      ),
      list(
        quote(
          !is.null(location) &&
            diff(as.numeric(strsplit(location, "-", fixed = TRUE)[[1]])) < 0
        ),
        "The start of `location` cannot exceed its end."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving natural variations of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list(
      "sourcetype", !is.null(source_type),
      paste0(source_type, collapse = ",")
    ),
    list(
      "consequencetype", !is.null(consequence_type),
      paste0(consequence_type, collapse = ",")
    ),
    list("wildtype", !is.null(wild_type), paste0(wild_type, collapse = ",")),
    list(
      "alternativesequence", !is.null(alternative_sequence),
      paste0(alternative_sequence, collapse = ",")
    ),
    list("location", !is.null(location), location),
    list("disease", !is.null(disease), disease),
    list("omim", !is.null(omim), paste0(omim, collapse = ",")),
    list("evidence", !is.null(evidence), paste0(evidence, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("dbtype", !is.null(db_type), paste0(db_type, collapse = ",")),
    list("dbid", !is.null(db_id), paste0(db_id, collapse = ","))
  )

  ## Build Function-Specific Call
  if (isFALSE(save_peff)) {
    save_to <- .rba_file(file = "uniprot_variation_search.json")
  } else {
    save_to <- .rba_file(
      file = "uniprot_variation_search.peff",
      save_to = save_peff
    )
  }

  obj_parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "variation"),
    query = call_query,
    save_to = save_to,
    file_accept = "text/x-peff",
    file_parser = "text->chr",
    obj_accept = "application/json",
    obj_parser = obj_parser_input
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Retrieve UniProt Natural Variants by Sequence Position
#'
#' Retrieve natural variants annotated at specified amino-acid positions in
#'   UniProt protein sequences. Each supplied accession is paired with the
#'   corresponding element of \code{locations}.
#'
#' A \code{locations} element can specify one position or several
#'   comma-separated positions. The returned records are grouped by UniProt
#'   accession and include the protein sequence and variant annotations found
#'   at the requested positions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/variation/accession_locations/\{accession_locations\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{UniProtKB
#'   accession}(s). You can supply up to 100 accessions. Each accession is
#'   paired with the corresponding element of \code{locations}.
#' @param locations Character or Numeric: Amino-acid position(s) within each
#'   protein sequence. Each element should be a positive whole number or a
#'   character string of comma-separated positions, such as \code{"5,7"}.
#'   You can supply up to 100 elements, and their number should equal the
#'   number of supplied accessions.
#' @param save_peff Logical or Character: (default = \code{FALSE}) \itemize{
#'   \item FALSE: Return the parsed JSON response.
#'   \item TRUE: Save the PEFF response to an automatically generated path.
#'   \item Character string: A valid file path to save the PEFF file.}
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return With \code{save_peff = FALSE}, a list named by UniProt
#'   accession. Each element contains entry metadata, the protein sequence,
#'   and matching variants in its \code{features} element. Repeated groups
#'   for the same accession are combined by the API, and \code{features}
#'   can be empty when no variant is annotated at the requested positions.
#'   Otherwise, the PEFF response is written to disk and returned as a
#'   character string.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_variation_locations(
#'     accession = c("P05067", "Q99616"),
#'     locations = c("5,7", "5,29"))
#' }
#'
#' @family "UniProt - Variation"
#' @export
rba_uniprot_variation_locations <- function(accession,
                                            locations,
                                            save_peff = FALSE,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "accession", class = "character", min_len = 1L,
        max_len = 100
      ),
      list(
        arg = "locations", class = c("character", "numeric", "integer"),
        min_len = 1L, max_len = 100
      ),
      list(
        arg = "save_peff", class = c("logical", "character"), len = 1L,
        no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(length(accession) != length(locations)),
        "`accession` and `locations` should have the same length."
      ),
      list(
        quote(
          switch(
            typeof(locations),
            character = any(!grepl("^[1-9]\\d*(?:,[1-9]\\d*)*$", locations, perl = TRUE)),
            any(!is.finite(locations) | locations < 1 | locations %% 1 != 0)
          )
        ),
        "`locations` should be positive whole-number positions, optionally comma-separated."
      )
    )
  )

  .msg(
    "Retrieving natural variants for the supplied UniProt accession-location pairs."
  )

  if (is.numeric(locations)) {
    locations <- format(
      locations,
      scientific = FALSE,
      trim = TRUE
    )
  }

  ## Build Function-Specific Call
  if (isFALSE(save_peff)) {
    save_to <- .rba_file(file = "uniprot_variation_locations.json")
  } else {
    save_to <- .rba_file(
      file = "uniprot_variation_locations.peff",
      save_to = save_peff
    )
  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(
      .rba_stg("uniprot", "pth"),
      "variation/accession_locations/",
      paste(
        toupper(accession),
        locations,
        sep = ":",
        collapse = "|"
      )
    ),
    save_to = save_to,
    file_accept = "text/x-peff",
    file_parser = "text->chr",
    obj_accept = "application/json",
    obj_parser = list("json->list", .rba_uniprot_search_namer)
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Retrieve UniProt Natural Variants by Identifier
#'
#' Retrieve natural variant annotations by UniProt accession, dbSNP identifier,
#'   or HGVS expression.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/variation/dbsnp/\{dbid\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/variation/hgvs/\{hgvs\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/variation/\{accession\}"
#'
#' @param id Character: A single identifier: either a
#'   \href{https://www.uniprot.org/help/accession_numbers}{UniProt primary or
#'   secondary accession}, an \href{https://www.ncbi.nlm.nih.gov/snp/}{NIH-NCBI
#'   dbSNP ID}, or an \href{https://varnomen.hgvs.org/}{HGVS expression}.
#' @param id_type Character: The type of supplied ID argument, one of:
#'   \href{https://www.uniprot.org/help/accession_numbers}{"uniprot"},
#'   \href{https://www.ncbi.nlm.nih.gov/snp/}{"dbsnp"} or
#'   \href{https://varnomen.hgvs.org/}{"hgvs"}.
#' @param source_type Character: (optional) Variation's source type. You
#'   can choose up to two of:
#'   "uniprot", "large scale study", "mixed", "clinvar", "nci-tcga",
#'   "cosmic curated", "ensembl", "gnomad", "topmed", or "exac".
#' @param consequence_type Character: (optional) Variation's consequence
#'   type. You can choose up to
#'   two of: "missense", "stop gained" or "stop lost".
#' @param wild_type Character: (optional) Wild-type amino acid. Accepted
#'   values are IUPAC
#'   single-letter amino acid codes and "*" for a stop codon. You can supply up
#'   to 20 values.
#' @param alternative_sequence Character: (optional) Alternative amino
#'   acid. Accepted values are
#'   IUPAC single-letter amino acid codes, "*" for a stop codon, and "-" for a
#'   deletion. You can supply up to 20 values.
#' @param location Character: (optional) A valid amino acid range (e.g. 10-25)
#'   within the sequence
#'   where the variation occurs.
#' @param save_peff Logical or Character: (default = \code{FALSE}) \itemize{
#'   \item FALSE: Return the parsed JSON response.
#'   \item TRUE: Save as PEFF file to an automatically-generated path.
#'   \item Character string: A valid file path to save the PEFF file.}
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return If \code{save_peff = FALSE}, a list. For \code{id_type = "uniprot"},
#'   it represents the requested entry; for \code{"dbsnp"} or \code{"hgvs"},
#'   each element represents a matching entry and is named by accession when
#'   available. If PEFF output is requested, the response is written to disk
#'   and returned as a character string.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
                                  source_type = NULL,
                                  consequence_type = NULL,
                                  wild_type = NULL,
                                  alternative_sequence = NULL,
                                  location = NULL,
                                  save_peff = FALSE,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "id", class = "character", len = 1L),
      list(
        arg = "id_type", class = "character", len = 1L,
        val = c("uniprot", "dbsnp", "hgvs")
      ),
      list(
        arg = "source_type", class = "character", max_len = 2,
        val = c(
          "uniprot", "large scale study", "mixed", "clinvar", "nci-tcga",
          "cosmic curated", "ensembl", "gnomad", "topmed", "exac"
        )
      ),
      list(
        arg = "consequence_type", class = "character", max_len = 2,
        val = c("missense", "stop gained", "stop lost")
      ),
      list(arg = "wild_type", class = "character", max_len = 20),
      list(arg = "alternative_sequence", class = "character", max_len = 20),
      list(
        arg = "location", class = "character", len = 1L,
        regex = "^[1-9]\\d*-[1-9]\\d*$"
      ),
      list(
        arg = "save_peff", class = c("logical", "character"), len = 1L,
        no_null = TRUE
      )
    ),
    cond = list(
      list(
        quote(
          !is.null(location) &&
            diff(as.numeric(strsplit(location, "-", fixed = TRUE)[[1]])) < 0
        ),
        "The start of `location` cannot exceed its end."
      )
    )
  )

  .msg(
    "Retrieving natural variants for %s.",
    ifelse(
      id_type == "uniprot",
      yes = sprintf("UniProt protein %s", id),
      no = sprintf("%s ID %s", id_type, id)
    )
  )

  ## Build GET API Request's query
  if (id_type %in% c("dbsnp", "hgvs")) {
    call_query <- list("size" = "-1")
  } else {
    call_query <- list()
  }

  call_query <- .rba_query(
    init = call_query,
    list(
      "sourcetype", !is.null(source_type),
      paste0(source_type, collapse = ",")
    ),
    list(
      "consequencetype", !is.null(consequence_type),
      paste0(consequence_type, collapse = ",")
    ),
    list("wildtype", !is.null(wild_type), paste0(wild_type, collapse = ",")),
    list(
      "alternativesequence", !is.null(alternative_sequence),
      paste0(alternative_sequence, collapse = ",")
    ),
    list("location", !is.null(location), location)
  )

  ## Build Function-Specific Call
  file_name <- sprintf(
    "uniprot_variation_%s.%s",
    id_type, ifelse(isFALSE(save_peff), "json", "peff")
  )

  if (isFALSE(save_peff)) {
    save_to <- .rba_file(file = file_name)
  } else {
    save_to <- .rba_file(file = file_name, save_to = save_peff)
  }

  path_input <- switch(
    id_type,
    "uniprot" = paste0(.rba_stg("uniprot", "pth"), "variation/", id),
    "hgvs" = paste0(.rba_stg("uniprot", "pth"), "variation/hgvs/", id),
    "dbsnp" = paste0(.rba_stg("uniprot", "pth"), "variation/dbsnp/", id)
  )

  if (id_type == "uniprot") {
    parser_input <- "json->list"
  } else {
    parser_input <- list("json->list", .rba_uniprot_search_namer)
  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    query = call_query,
    save_to = save_to,
    file_accept = "text/x-peff",
    file_parser = "text->chr",
    obj_accept = "application/json",
    obj_parser = parser_input
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Antigen Endpoints ####

#' Search Antigens in UniProt
#'
#' UniProt maps antigenic (antibody-binding) features from several sources to
#'   protein sequences. Search those mappings using one or more criteria. At
#'   least one of \code{accession}, \code{antigen_sequence}, \code{antigen_id},
#'   \code{ensembl_id}, or \code{match_score} is required.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/antigen"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param antigen_sequence Character: (optional) A single antigenic protein
#'   sequence of at least four residues.
#' @param antigen_id Character: (optional) Human Protein Atlas (HPA) antigen ID.
#'   You
#'   can supply up to
#'   20 IDs.
#' @param ensembl_id Character: (optional) Ensembl stable transcript ID. You can
#'   supply up to
#'   20 IDs.
#' @param match_score Numeric: (optional) A whole number from 0 to 100 giving
#'   the minimum
#'   alignment score between the antigen sequence and target protein sequence.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list in which each element represents a matching UniProt entry,
#'   named by accession when available. Antigenic annotations are stored in the
#'   entry's \code{features} element.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_antigens_search(antigen_id = "HPA001060")
#' }
#'
#' @family "UniProt - Antigen"
#' @export
rba_uniprot_antigens_search <- function(accession = NULL,
                                        antigen_sequence = NULL,
                                        antigen_id = NULL,
                                        ensembl_id = NULL,
                                        match_score = NULL,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "antigen_sequence", class = "character", len = 1L),
      list(arg = "antigen_id", class = "character", max_len = 20),
      list(arg = "ensembl_id", class = "character", max_len = 20),
      list(
        arg = "match_score", class = c("numeric", "integer"), len = 1L,
        ran = c(0, 100)
      )
    ),
    cond = list(
      list(
        quote(
          all(
            is.null(accession), is.null(antigen_sequence),
            is.null(antigen_id), is.null(ensembl_id), is.null(match_score)
          )
        ),
        "Supply at least one search criterion: accession, antigen_sequence, antigen_id, ensembl_id, or match_score."
      ),
      list(
        quote(
          !is.null(antigen_sequence) && nchar(antigen_sequence) < 4L
        ),
        "`antigen_sequence` should contain at least four residues."
      ),
      list(
        quote(
          !is.null(match_score) &&
            (!is.finite(match_score) || match_score %% 1 != 0)
        ),
        "`match_score` should be a finite whole number from 0 to 100."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving antigenic features of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("antigen_sequence", !is.null(antigen_sequence), antigen_sequence),
    list(
      "antigen_id", !is.null(antigen_id),
      paste0(antigen_id, collapse = ",")
    ),
    list(
      "ensembl_ids", !is.null(ensembl_id),
      paste0(ensembl_id, collapse = ",")
    ),
    list("match_score", !is.null(match_score), match_score)
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "antigen"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_antigen_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Antigens by UniProt Accession
#'
#' UniProt maps antigenic (antibody-binding) features from several sources to
#'   protein sequences. Retrieve the features mapped to one UniProtKB
#'   accession.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/antigen/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the antigenic features mapped to the requested
#'   UniProt protein sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
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
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving Antigenic features mapped to the sequence of protein %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "antigen/", accession),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_antigen.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Epitopes Endpoints ####

#' Search UniProt Epitopes
#'
#' Use this function to search epitope data associated with UniProt entries,
#'   using various criteria such as UniProt accession, epitope sequence,
#'   IEDB ID, and match score. At least one search criterion is required.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/epitope"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param epitope_sequence Character: (optional) A single epitope protein
#'   sequence.
#' @param iedb_id Character or Numeric: (optional)
#'   \href{https://www.iedb.org/}{IEDB} epitope identifier(s).
#'   You can supply up to 20 identifiers.
#' @param match_score Numeric: (optional) A whole number from 0 to 100 giving
#'   the minimum
#'   alignment score between the epitope sequence and target protein sequence.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list in which each element represents a matching UniProt entry and
#'   is named by accession when available.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_epitope_search(accession = c("Q84ZX5", "P36222"))
#' }
#' \donttest{
#'   rba_uniprot_epitope_search(epitope_sequence = "DKKCIEWEKAQHGA")
#' }
#' \donttest{
#'   rba_uniprot_epitope_search(iedb_id = 20354)
#' }
#'
#' @family "UniProt - Epitopes"
#' @export
rba_uniprot_epitope_search <- function(accession = NULL,
                                       epitope_sequence = NULL,
                                       iedb_id = NULL,
                                       match_score = NULL,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "epitope_sequence", class = "character", len = 1L),
      list(
        arg = "iedb_id", class = c("character", "numeric", "integer"),
        max_len = 20, regex = "^[1-9]\\d*$"
      ),
      list(
        arg = "match_score", class = c("numeric", "integer"), len = 1L,
        ran = c(0, 100)
      )
    ),
    cond = list(
      list(
        quote(
          all(
            is.null(accession), is.null(epitope_sequence),
            is.null(iedb_id), is.null(match_score)
          )
        ),
        "Supply at least one search criterion: accession, epitope_sequence, iedb_id, or match_score."
      ),
      list(
        quote(
          !is.null(match_score) &&
            (!is.finite(match_score) || match_score %% 1 != 0)
        ),
        "`match_score` should be a finite whole number from 0 to 100."
      )
    )
  )

  .msg(
    "Searching UniProt for epitopes matching the supplied criteria."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("epitope_sequence", !is.null(epitope_sequence), epitope_sequence),
    list("iedb_id", !is.null(iedb_id), paste0(iedb_id, collapse = ",")),
    list("match_score", !is.null(match_score), match_score)
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "epitope"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_epitope_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Retrieve Epitopes by Accession
#'
#' Use this function to retrieve epitope annotations linked to a UniProt entry.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/epitope/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the UniProt epitope features details for the given
#'   accession.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_epitope(accession = "P36222")
#' }
#'
#' @family "UniProt - Epitopes"
#' @export
rba_uniprot_epitope <- function(accession, ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving epitope information for accession %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "epitope/", accession),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_epitope.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Mutagenesis Endpoints ####

#' Search Mutagenesis in UniProt
#'
#' UniProt describes how sequence mutations affect the biological properties
#'   of a protein, cell, or organism. Use this function to search for
#'   \href{https://www.uniprot.org/help/mutagen}{
#'   mutagenesis annotations} using at least one of \code{accession},
#'   \code{taxid}, or \code{db_id}.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/mutagenesis"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param db_id Character: (optional) The ID in a cross-reference database.
#'   You can supply up to 20 values.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list in which each element represents a matching UniProt entry,
#'   named by accession when available. Mutagenesis annotations are stored in
#'   the entry's \code{features} element.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' #search all mutations in COVID19 proteins
#' rba_uniprot_mutagenesis_search(taxid = 2697049)
#' }
#'
#' @family "UniProt - Mutagenesis"
#' @export
rba_uniprot_mutagenesis_search <- function(accession = NULL,
                                           taxid = NULL,
                                           db_id = NULL,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(
        arg = "taxid", class = c("numeric", "integer"), max_len = 20,
        min_val = 1
      ),
      list(arg = "db_id", class = "character", max_len = 20)
    ),
    cond = list(
      list(
        quote(all(is.null(accession), is.null(taxid), is.null(db_id))),
        "Supply at least one search criterion: accession, taxid, or db_id."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving mutagenesis description of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("dbid", !is.null(db_id), paste0(db_id, collapse = ","))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "mutagenesis"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_mutagenesis_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Mutagenesis by UniProt Accession
#'
#' UniProt describes how sequence mutations affect the biological properties
#'   of a protein, cell, or organism. Retrieve the
#'   \href{https://www.uniprot.org/help/mutagen}{
#'   mutagenesis annotations} mapped to one UniProt protein.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/mutagenesis/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param location Character: (optional) A valid amino acid range (e.g. 10-25)
#'   within the sequence
#'   of the given protein.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the mutagenesis description of your supplied
#'   UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_mutagenesis(accession = "P0DTC2", location = "300-400")
#' }
#'
#' @family "UniProt - Mutagenesis"
#' @export
rba_uniprot_mutagenesis <- function(accession,
                                    location = NULL,
                                    ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L),
      list(
        arg = "location", class = "character", len = 1L,
        regex = "^[1-9]\\d*-[1-9]\\d*$"
      )
    ),
    cond = list(
      list(
        quote(
          !is.null(location) &&
            diff(as.numeric(strsplit(location, "-", fixed = TRUE)[[1]])) < 0
        ),
        "The start of `location` cannot exceed its end."
      )
    )
  )

  .msg(
    "Retrieving mutagenesis description mapped to the sequence of protein %s.",
    accession
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("location", !is.null(location), location)
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "mutagenesis/", accession),
    accept = "application/json",
    parser = "json->list",
    query = call_query,
    save_to = .rba_file("uniprot_mutagenesis.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### RNA-editing ####

#' Search RNA Editing in UniProt
#'
#' UniProt curates \href{https://www.uniprot.org/help/rna_editing}{RNA-editing
#'   events} (conversion, insertion, deletion of nucleotides). Use this
#'   function to search RNA editing records in UniProt using various
#'   criteria such as accession, taxon ID, or protein-level variant location.
#'   At least one criterion is required.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/rna-editing"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param variant_location Character: (optional) Up to four protein-level variant
#'   locations, for example \code{"p.Leu336Pro"}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list in which each element represents a matching UniProt entry and
#'   is named by accession when available.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_rna_edit_search(accession = c("Q16851", "Q16849"))
#' }
#'
#' @family "UniProt - RNA Editing"
#' @export
rba_uniprot_rna_edit_search <- function(accession = NULL,
                                        taxid = NULL,
                                        variant_location = NULL,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(
        arg = "taxid", class = c("numeric", "integer"), max_len = 20,
        min_val = 1
      ),
      list(arg = "variant_location", class = "character", max_len = 4)
    ),
    cond = list(
      list(
        quote(
          all(
            is.null(accession), is.null(taxid), is.null(variant_location)
          )
        ),
        "Supply at least one search criterion: accession, taxid, or variant_location."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      )
    )
  )

  .msg(
    "Searching UniProt for RNA editing records matching the supplied criteria."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list(
      "variantlocation", !is.null(variant_location),
      paste0(variant_location, collapse = ",")
    )
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "rna-editing"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_rna_editing_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Retrieve UniProt RNA-Editing Annotations by Accession
#'
#' Use this function to retrieve
#'   \href{https://www.uniprot.org/help/rna_editing}{RNA-editing
#'   events} (conversion, insertion, deletion of nucleotides) annotations
#'   linked to a UniProt entry.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/rna-editing/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the UniProt RNA-editing features details for the
#'   given accession.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'   rba_uniprot_rna_edit(accession = "Q16851")
#' }
#'
#' @family "UniProt - RNA Editing"
#' @export
rba_uniprot_rna_edit <- function(accession, ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving RNA-editing information for accession %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "rna-editing/", accession),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_rna_edit.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
