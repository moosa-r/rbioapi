# Search UniProt Natural Variants

Search and retrieve [natural
variants](https://www.uniprot.org/help/variant) annotated on protein
sequences, including variants imported from supported large-scale
studies.

## Usage

``` r
rba_uniprot_variation_search(
  accession = NULL,
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
  ...
)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- source_type:

  Character: (optional) Up to two variant source types: "uniprot",
  "large scale study", "mixed", "clinvar", "nci-tcga", "cosmic curated",
  "ensembl", "gnomad", "topmed", or "exac".

- consequence_type:

  Character: (optional) Up to two consequence types: "missense", "stop
  gained", or "stop lost".

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

- disease:

  Character: (optional) [Human
  disease](https://www.uniprot.org/diseases/) associated with a sequence
  variation. Accepted values are a disease name (e.g. Alzheimer disease
  18), partial disease name (Alzheimer), or disease acronym (e.g. AD).

- omim:

  Character or Numeric: (optional)
  [OMIM](https://www.ncbi.nlm.nih.gov/omim) ID that is associated with a
  variation. You can supply up to 20 values.

- evidence:

  Character or Numeric: (optional) PubMed ID of a variation's
  [citation](https://www.uniprot.org/citations/). You can supply up to
  20 values.

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- db_type:

  Character: (optional) Cross-reference database of the variation. You
  can supply up to two values. Examples include `"dbSNP"`,
  `"cosmic curated"`, and `"ClinVar"`.

- db_id:

  Character: (optional) Variation identifier in a cross-reference
  database. You can supply up to 20 values.

- save_peff:

  Logical or Character: (default = `FALSE`)

  - FALSE: Return the parsed JSON response.

  - TRUE: Save the PEFF response to an automatically generated path.

  - Character string: A valid file path to save the PEFF file.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

With `save_peff = FALSE`, a list named by UniProt accession. Each
element contains one matching entry and its variants. Otherwise, the
PEFF response is written to disk and returned as a character string.

## Details

At least one primary criterion is required: `accession`, `disease`,
`omim`, `evidence`, `taxid`, `db_type`, or `db_id`. The other arguments
refine those criteria.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/variation"

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
[`rba_uniprot_variation()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation.md),
[`rba_uniprot_variation_locations()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_locations.md)

## Examples

``` r
# \donttest{
rba_uniprot_variation_search(accession = "P05067")
# }
# \donttest{
rba_uniprot_variation_search(disease = "alzheimer disease, 18")
# }
# \donttest{
rba_uniprot_variation_search(disease = "alzheimer",
    wild_type = "A", alternative_sequence = "T")
# }
```
