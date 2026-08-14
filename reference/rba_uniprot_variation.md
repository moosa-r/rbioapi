# Retrieve UniProt Natural Variants by Identifier

Retrieve natural variant annotations by UniProt accession, dbSNP
identifier, or HGVS expression.

## Usage

``` r
rba_uniprot_variation(
  id,
  id_type,
  source_type = NULL,
  consequence_type = NULL,
  wild_type = NULL,
  alternative_sequence = NULL,
  location = NULL,
  save_peff = FALSE,
  ...
)
```

## Arguments

- id:

  Character: A single identifier: either a [UniProt primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers), an
  [NIH-NCBI dbSNP ID](https://www.ncbi.nlm.nih.gov/snp/), or an [HGVS
  expression](https://varnomen.hgvs.org/).

- id_type:

  Character: The type of supplied ID argument, one of:
  ["uniprot"](https://www.uniprot.org/help/accession_numbers),
  ["dbsnp"](https://www.ncbi.nlm.nih.gov/snp/) or
  ["hgvs"](https://varnomen.hgvs.org/).

- source_type:

  Character: (optional) Variation's source type. You can choose up to
  two of: "uniprot", "large scale study", "mixed", "clinvar",
  "nci-tcga", "cosmic curated", "ensembl", "gnomad", "topmed", or
  "exac".

- consequence_type:

  Character: (optional) Variation's consequence type. You can choose up
  to two of: "missense", "stop gained" or "stop lost".

- wild_type:

  Character: (optional) Wild-type amino acid. Accepted values are IUPAC
  single-letter amino acid codes and "\*" for a stop codon. You can
  supply up to 20 values.

- alternative_sequence:

  Character: (optional) Alternative amino acid. Accepted values are
  IUPAC single-letter amino acid codes, "\*" for a stop codon, and "-"
  for a deletion. You can supply up to 20 values.

- location:

  Character: (optional) A valid amino acid range (e.g. 10-25) within the
  sequence where the variation occurs.

- save_peff:

  Logical or Character: (default = `FALSE`)

  - FALSE: Return the parsed JSON response.

  - TRUE: Save as PEFF file to an automatically-generated path.

  - Character string: A valid file path to save the PEFF file.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

If `save_peff = FALSE`, a list. For `id_type = "uniprot"`, it represents
the requested entry; for `"dbsnp"` or `"hgvs"`, each element represents
a matching entry and is named by accession when available. If PEFF
output is requested, the response is written to disk and returned as a
character string.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/variation/dbsnp/{dbid}"  
"GET https://www.ebi.ac.uk/proteins/api/variation/hgvs/{hgvs}"  
"GET https://www.ebi.ac.uk/proteins/api/variation/{accession}"

## References

- The UniProt Consortium. (2025). UniProt: the Universal Protein
  Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
  https://doi.org/10.1093/nar/gkae1010

- Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales, L.,
  Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
  Proteins API: Accessing key integrated protein and genome information.
  Nucleic Acids Research, 45(W1), W539–W544.
  https://doi.org/10.1093/nar/gkx237

- [Proteins API Documentation](https://www.ebi.ac.uk/proteins/api/doc/)

- [Citations note on UniProt
  website](https://www.uniprot.org/help/publications)

## See also

Other "UniProt - Variation":
[`rba_uniprot_variation_locations()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_locations.md),
[`rba_uniprot_variation_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_variation(id = "rs121434451", id_type = "dbsnp")
# }
# \donttest{
rba_uniprot_variation(id = "NC_000008.11:g.22119227C>T", id_type = "hgvs")
# }
# \donttest{
rba_uniprot_variation(id = "O43593", id_type = "uniprot")
# }
```
