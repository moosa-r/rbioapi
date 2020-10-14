#### Transcript Haplotypes Endpoints ####

#' Computes observed transcript haplotype sequences based on phased genotype data
#'
#' @param transcript_id
#' @param species
#' @param aligned_sequences
#' @param samples
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param sequence
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_transcript_haplotypes = function(transcript_id,
                                             species,
                                             aligned_sequences = FALSE,
                                             samples = FALSE,
                                             sequence = FALSE,
                                             ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "aligned_sequences",
                               class = "logical"),
                          list(arg = "samples",
                               class = "logical"),
                          list(arg = "sequence",
                               class = "logical")))

  v_msg("GET transcript_haplotypes/:species/:id")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("aligned_sequences",
                                 aligned_sequences == TRUE,
                                 "1"),
                            list("samples",
                                 samples == TRUE,
                                 "1"),
                            list("sequence",
                                 sequence == TRUE,
                                 "1"))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("transcript_haplotypes/",
                                         species, "/",
                                         transcript_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_transcript_haplotypes.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
