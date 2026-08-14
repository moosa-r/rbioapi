# Search Proteomes in UniProt

UniProt collects and annotates proteomes (protein sets expressed in an
organism). Search available proteomes by name, identifier, taxonomy,
keyword, cross-reference, genome accession, or status. See [What are
proteomes?](https://www.uniprot.org/help/proteome) for more information.

## Usage

``` r
rba_uniprot_proteomes_search(
  name = NULL,
  upid = NULL,
  taxid = NULL,
  keyword = NULL,
  xref = NULL,
  genome_acc = NULL,
  is_ref_proteome = NULL,
  is_redundant = NULL,
  ...
)
```

## Arguments

- name:

  Character: (optional) A term in the proteome name.

- upid:

  Character: (optional) [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- keyword:

  Character: (optional) Limit the search to entries containing the
  keyword. See [UniProt Keywords](https://www.uniprot.org/keywords/).

- xref:

  Character: (optional) Proteome cross-references such as genome
  assembly ID or Biosample ID. You can supply up to 20 cross-reference
  IDs.

- genome_acc:

  Character: (optional) Genome accession associated with the proteome's
  components. You can supply up to 20 accessions.

- is_ref_proteome:

  Logical: (optional) If `TRUE`, return only reference proteomes; if
  `FALSE`, return only non-reference proteomes; if `NULL`, do not filter
  by this criterion. See ['What are reference
  proteomes?'](https://www.uniprot.org/help/reference_proteome) for more
  information.

- is_redundant:

  Logical: (optional) If `TRUE`, return only redundant proteomes; if
  `FALSE`, return only non-redundant proteomes; if `NULL`, do not filter
  by redundancy. See ['Reducing proteome
  redundancy'](https://www.uniprot.org/help/proteome_redundancy) for
  more information.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list named by UPID. Each element contains one matching proteome's
metadata.

## Details

At least one search criterion is required.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteomes"

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

Other "UniProt - Proteomes":
[`rba_uniprot_genecentric()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric.md),
[`rba_uniprot_genecentric_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric_search.md),
[`rba_uniprot_proteomes()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteomes_search(name = "SARS-CoV")
# }
# \donttest{
rba_uniprot_proteomes_search(genome_acc = "AY274119")
# }
```
