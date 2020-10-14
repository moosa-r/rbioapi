#### VEP Endpoints ####

#' Fetch variant consequences for multiple HGVS notations
#'
#' @param hgvs_notations
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_hgvs = function(hgvs_notations,
                                species,
                                Blosum62 = FALSE,
                                CADD = FALSE,
                                GeneSplicer = FALSE,
                                LoF = FALSE,
                                MaxEntScan = FALSE,
                                Phenotypes = FALSE,
                                SpliceAI = FALSE,
                                SpliceRegion = FALSE,
                                appris = FALSE,
                                canonical = FALSE,
                                ccds = FALSE,
                                dbNSFP = NA,
                                dbscSNV = FALSE,
                                distance = 5000,
                                domains = FALSE,
                                failed = FALSE,
                                hgvs = FALSE,
                                mane = FALSE,
                                merged = FALSE,
                                miRNA = FALSE,
                                minimal = FALSE,
                                numbers = FALSE,
                                protein = FALSE,
                                refseq = FALSE,
                                shift_3prime = FALSE,
                                shift_genomic = FALSE,
                                transcript_id = NA,
                                transcript_version = FALSE,
                                tsl = FALSE,
                                uniprot = FALSE,
                                variant_class = FALSE,
                                vcf_string = FALSE,
                                xref_refseq = FALSE,
                                ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "hgvs_notations",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("POST vep/:species/hgvs")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("hgvs_notations" = as.array(hgvs_notations)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/hgvs"),
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_vep_hgvs.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Fetch variant consequences for multiple ids
#'
#' @param ids
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_ids = function(ids,
                               species,
                               Blosum62 = FALSE,
                               CADD = FALSE,
                               GeneSplicer = FALSE,
                               LoF = FALSE,
                               MaxEntScan = FALSE,
                               Phenotypes = FALSE,
                               SpliceAI = FALSE,
                               SpliceRegion = FALSE,
                               appris = FALSE,
                               canonical = FALSE,
                               ccds = FALSE,
                               dbNSFP = NA,
                               dbscSNV = FALSE,
                               distance = 5000,
                               domains = FALSE,
                               failed = FALSE,
                               hgvs = FALSE,
                               mane = FALSE,
                               merged = FALSE,
                               miRNA = FALSE,
                               minimal = FALSE,
                               numbers = FALSE,
                               protein = FALSE,
                               refseq = FALSE,
                               shift_3prime = FALSE,
                               shift_genomic = FALSE,
                               transcript_id = NA,
                               transcript_version = FALSE,
                               tsl = FALSE,
                               uniprot = FALSE,
                               variant_class = FALSE,
                               vcf_string = FALSE,
                               xref_refseq = FALSE,
                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "ids",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("POST vep/:species/id")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("ids" = as.array(ids)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/id"),
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_vep_ids.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Fetch variant consequences for multiple regions
#'
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param variants
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_variant = function(variants,
                                   species,
                                   Blosum62 = FALSE,
                                   CADD = FALSE,
                                   GeneSplicer = FALSE,
                                   LoF = FALSE,
                                   MaxEntScan = FALSE,
                                   Phenotypes = FALSE,
                                   SpliceAI = FALSE,
                                   SpliceRegion = FALSE,
                                   appris = FALSE,
                                   canonical = FALSE,
                                   ccds = FALSE,
                                   dbNSFP = NA,
                                   dbscSNV = FALSE,
                                   distance = 5000,
                                   domains = FALSE,
                                   failed = FALSE,
                                   hgvs = FALSE,
                                   mane = FALSE,
                                   merged = FALSE,
                                   miRNA = FALSE,
                                   minimal = FALSE,
                                   numbers = FALSE,
                                   protein = FALSE,
                                   refseq = FALSE,
                                   shift_3prime = FALSE,
                                   shift_genomic = FALSE,
                                   transcript_id = NA,
                                   transcript_version = FALSE,
                                   tsl = FALSE,
                                   uniprot = FALSE,
                                   variant_class = FALSE,
                                   vcf_string = FALSE,
                                   xref_refseq = FALSE,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "variants",
                               class = "character",
                               max_len = 200),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("POST vep/:species/region")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build POST API Request's URL
  call_body = jsonlite::toJSON(list("variants" = as.array(variants)))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/region"),
                           body = call_body,
                           query = call_query,
                           accept = "application/json",
                           httr::content_type("application/json"),
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_vep_variant.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Fetch variant consequences
#'
#' @param allele
#' @param region
#' @param species
#' @param Blosum62
#' @param CADD
#' @param GeneSplicer
#' @param LoF
#' @param MaxEntScan
#' @param Phenotypes
#' @param SpliceAI
#' @param SpliceRegion
#' @param appris
#' @param canonical
#' @param ccds
#' @param dbNSFP
#' @param dbscSNV
#' @param distance
#' @param domains
#' @param failed
#' @param hgvs
#' @param mane
#' @param merged
#' @param miRNA
#' @param minimal
#' @param numbers
#' @param protein
#' @param refseq
#' @param shift_3prime
#' @param shift_genomic
#' @param transcript_id
#' @param transcript_version
#' @param tsl
#' @param uniprot
#' @param variant_class
#' @param vcf_string
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param xref_refseq
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_vep_allele = function(allele,
                                  region,
                                  species,
                                  Blosum62 = FALSE,
                                  CADD = FALSE,
                                  GeneSplicer = FALSE,
                                  LoF = FALSE,
                                  MaxEntScan = FALSE,
                                  Phenotypes = FALSE,
                                  SpliceAI = FALSE,
                                  SpliceRegion = FALSE,
                                  appris = FALSE,
                                  canonical = FALSE,
                                  ccds = FALSE,
                                  dbNSFP = NA,
                                  dbscSNV = FALSE,
                                  distance = 5000,
                                  domains = FALSE,
                                  failed = FALSE,
                                  hgvs = FALSE,
                                  mane = FALSE,
                                  merged = FALSE,
                                  miRNA = FALSE,
                                  minimal = FALSE,
                                  numbers = FALSE,
                                  protein = FALSE,
                                  refseq = FALSE,
                                  shift_3prime = FALSE,
                                  shift_genomic = FALSE,
                                  transcript_id = NA,
                                  transcript_version = FALSE,
                                  tsl = FALSE,
                                  uniprot = FALSE,
                                  variant_class = FALSE,
                                  vcf_string = FALSE,
                                  xref_refseq = FALSE,
                                  ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "variants",
                               class = "character"),
                          list(arg = "variants",
                               class = "character"),
                          list(arg = "species",
                               class = c("character",
                                         "numeric")),
                          list(arg = "Blosum62",
                               class = "logical"),
                          list(arg = "CADD",
                               class = "logical"),
                          list(arg = "GeneSplicer",
                               class = "logical"),
                          list(arg = "LoF",
                               class = "logical"),
                          list(arg = "MaxEntScan",
                               class = "logical"),
                          list(arg = "Phenotypes",
                               class = "logical"),
                          list(arg = "SpliceAI",
                               class = "logical"),
                          list(arg = "SpliceRegion",
                               class = "logical"),
                          list(arg = "appris",
                               class = "logical"),
                          list(arg = "canonical",
                               class = "logical"),
                          list(arg = "ccds",
                               class = "logical"),
                          list(arg = "dbNSFP",
                               class = "character"),
                          list(arg = "dbscSNV",
                               class = "logical"),
                          list(arg = "distance",
                               class = "numeric"),
                          list(arg = "domains",
                               class = "logical"),
                          list(arg = "failed",
                               class = "logical"),
                          list(arg = "hgvs",
                               class = "logical"),
                          list(arg = "mane",
                               class = "logical"),
                          list(arg = "merged",
                               class = "logical"),
                          list(arg = "miRNA",
                               class = "logical"),
                          list(arg = "minimal",
                               class = "logical"),
                          list(arg = "numbers",
                               class = "logical"),
                          list(arg = "protein",
                               class = "logical"),
                          list(arg = "refseq",
                               class = "logical"),
                          list(arg = "shift_3prime",
                               class = "logical"),
                          list(arg = "shift_genomic",
                               class = "logical"),
                          list(arg = "transcript_id",
                               class = "character"),
                          list(arg = "transcript_version",
                               class = "logical"),
                          list(arg = "tsl",
                               class = "logical"),
                          list(arg = "uniprot",
                               class = "logical"),
                          list(arg = "variant_class",
                               class = "logical"),
                          list(arg = "vcf_string",
                               class = "logical"),
                          list(arg = "xref_refseq",
                               class = "logical")))

  v_msg("GET vep/:species/region/:region/:allele/")

  ## Build POST API Request's query
  call_query = rba_ba_query(init = list(),
                            list("Blosum62",
                                 Blosum62 == TRUE,
                                 "1"),
                            list("CADD",
                                 CADD == TRUE,
                                 "1"),
                            list("GeneSplicer",
                                 GeneSplicer == TRUE,
                                 "1"),
                            list("LoF",
                                 LoF == TRUE,
                                 "1"),
                            list("MaxEntScan",
                                 MaxEntScan == TRUE,
                                 "1"),
                            list("Phenotypes",
                                 Phenotypes == TRUE,
                                 "1"),
                            list("SpliceAI",
                                 SpliceAI == TRUE,
                                 "1"),
                            list("SpliceRegion",
                                 SpliceRegion == TRUE,
                                 "1"),
                            list("appris",
                                 appris == TRUE,
                                 "1"),
                            list("canonical",
                                 canonical == TRUE,
                                 "1"),
                            list("ccds",
                                 ccds == TRUE,
                                 "1"),
                            list("dbNSFP",
                                 !is.na(dbNSFP),
                                 dbNSFP),
                            list("dbscSNV",
                                 dbscSNV == TRUE,
                                 "1"),
                            list("distance",
                                 distance != 5000,
                                 as.integer(distance)),
                            list("domains",
                                 domains == TRUE,
                                 "1"),
                            list("failed",
                                 failed == TRUE,
                                 "1"),
                            list("hgvs",
                                 hgvs == TRUE,
                                 "1"),
                            list("mane",
                                 mane == TRUE,
                                 "1"),
                            list("merged",
                                 merged == TRUE,
                                 "1"),
                            list("miRNA",
                                 miRNA == TRUE,
                                 "1"),
                            list("minimal",
                                 minimal == TRUE,
                                 "1"),
                            list("numbers",
                                 numbers == TRUE,
                                 "1"),
                            list("protein",
                                 protein == TRUE,
                                 "1"),
                            list("refseq",
                                 refseq == TRUE,
                                 "1"),
                            list("shift_3prime",
                                 shift_3prime == TRUE,
                                 "1"),
                            list("shift_genomic",
                                 shift_genomic == TRUE,
                                 "1"),
                            list("transcript_id",
                                 !is.na(transcript_id),
                                 transcript_id),
                            list("transcript_version",
                                 transcript_version == TRUE,
                                 "1"),
                            list("tsl",
                                 tsl == TRUE,
                                 "1"),
                            list("uniprot",
                                 uniprot == TRUE,
                                 "1"),
                            list("variant_class",
                                 variant_class == TRUE,
                                 "1"),
                            list("vcf_string",
                                 vcf_string == TRUE,
                                 "1"),
                            list("xref_refseq",
                                 xref_refseq == TRUE,
                                 "1"))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("vep/",
                                         species,
                                         "/region/",
                                         region, "/",
                                         allele),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_vep_allele.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
