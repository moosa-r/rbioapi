# Search UniProt entries

Search and retrieve UniProt Knowledgebase (UniProtKB) protein entries by
accession, annotation, gene, organism, sequence properties, or other
supported criteria.

## Usage

``` r
rba_uniprot_proteins_search(
  accession = NULL,
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
  ...
)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- reviewed:

  Logical: (optional) If `TRUE`, return only reviewed Swiss-Prot
  entries. If `FALSE`, return only unreviewed TrEMBL entries. This is a
  refining filter and cannot be the sole search criterion.

- isoform:

  Numeric: (optional) One of:

  - 0: Exclude isoforms; this only refines another criterion.

  - 1: Return isoforms only; this can be a stand-alone criterion.

  - 2: Return canonical entries and isoforms; this only refines another
    criterion.

  See [alternative
  products](https://www.uniprot.org/help/alternative_products).

- go_term:

  Character: (optional) Limit the search to entries associated with your
  supplied GO ([Gene
  Ontology](https://www.uniprot.org/help/gene_ontology)) term. Supply
  either a GO ID or a character string partially or fully matching the
  term, e.g. "GO:0001776" or "leukocyte homeostasis". If you supply
  "leukocyte", any term containing that word will be included, e.g.
  "leukocyte chemotaxis" or "leukocyte activation".

- keyword:

  Character: (optional) Limit the search to entries that contain your
  supplied keyword. See [UniProt
  Keywords](https://www.uniprot.org/keywords/).

- ec:

  Character: (optional) [EC (Enzyme Commission)
  number(s)](https://enzyme.expasy.org/). You can supply up to 20 EC
  numbers.

- gene:

  Character: (optional) [UniProt gene
  name(s)](https://www.uniprot.org/help/gene_name). You can supply up to
  20 gene names. For example, if you supply "CD40", "CD40 ligand" will
  also be included.

- exact_gene:

  Character: (optional) [UniProt exact gene
  name(s)](https://www.uniprot.org/help/gene_name). You can supply up to
  20 exact gene names. For example, if you supply "CD40", "CD40 ligand"
  will not be included in the results.

- protein:

  Character: (optional) [UniProt protein
  name](https://www.uniprot.org/help/protein_names).

- organism:

  Character: (optional) Organism name.

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- pubmed:

  Character or Numeric: (optional) PubMed ID(s) cited by the returned
  entries. You can supply up to 20 IDs.

- seq_length:

  Character or Numeric: (optional) An exact sequence length (e.g. 150)
  or a range of sequence lengths (e.g. "130-158").

- md5:

  Character: (optional) A 32-character hexadecimal sequence MD5
  checksum.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list named by UniProt accession. Each element contains one matching
UniProtKB entry.

## Details

At least one primary search criterion is required. The value
`isoform = 1` can be used by itself; `reviewed` and the other `isoform`
values only refine another criterion.

UniProtKB entries are grouped into two sections:

1.  Reviewed (Swiss-Prot): Manually annotated records with information
    extracted from literature and curator-evaluated computational
    analysis.

2.  Unreviewed (TrEMBL): Computationally analyzed records that await
    full manual annotation.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteins"

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

Other "UniProt - Proteins":
[`rba_uniprot_proteins()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins.md),
[`rba_uniprot_proteins_crossref()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_crossref.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteins_search(accession = "Q99616")
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40")
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40 ligand")
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40",  reviewed = TRUE)
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40",  reviewed = TRUE, isoform = 1)
# }
# \donttest{
rba_uniprot_proteins_search(
  keyword = "Inhibition of host chemokines by virus"
)
# }
# \donttest{
rba_uniprot_proteins_search(keyword = "chemokines")
# }
```
