# Search Gene-Centric Proteins

UniProt gene-centric protein groups organize related protein entries
from a proteome by gene. Search these groups by proteome, accession, or
gene identifier. For more information, see [What are
proteomes?](https://www.uniprot.org/help/proteome) and [Automatic
gene-centric isoform mapping for eukaryotic reference proteome
entries.](https://www.uniprot.org/help/gene_centric_isoform_mapping)

## Usage

``` r
rba_uniprot_genecentric_search(upid = NULL, accession = NULL, gene = NULL, ...)
```

## Arguments

- upid:

  Character: (optional) [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- gene:

  Character: (optional) Unique gene identifier(s) found in MOD,
  [Ensembl](https://www.ensembl.org/info/genome/genebuild/gene_names.html),
  Ensembl Genomes, [OLN](https://www.uniprot.org/help/gene_name),
  [ORF](https://www.uniprot.org/help/gene_name) or [UniProt Gene
  Name](https://www.uniprot.org/help/gene_name).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing matching gene-centric protein groups.

## Details

At least one search criterion is required.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/genecentric"

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
[`rba_uniprot_proteomes()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes.md),
[`rba_uniprot_proteomes_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_genecentric_search(accession = "P59594")
# }
# \donttest{
rba_uniprot_genecentric_search(gene = "Spike")
# }
# \donttest{
rba_uniprot_genecentric_search(upid = "UP000000354")
# }
```
