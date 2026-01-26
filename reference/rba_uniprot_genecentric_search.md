# Search Gene-Centric Proteins

Using this function you can search UniProt for available gene-centrics
from proteomes. For more information, see [What are
proteomes?](https://www.uniprot.org/help/proteome) and [Automatic
gene-centric isoform mapping for eukaryotic reference proteome
entries.](https://www.uniprot.org/help/gene_centric_isoform_mapping) You
may also refine your search with modifiers upid, accession and gene. See
"Arguments section" for more information.

## Usage

``` r
rba_uniprot_genecentric_search(upid = NULL, accession = NULL, gene = NULL, ...)
```

## Arguments

- upid:

  [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- gene:

  unique gene identifier(s) found in MOD,
  [Ensembl](https://www.ensembl.org/info/genome/genebuild/gene_names.html),
  Ensembl Genomes, [OLN](https://www.uniprot.org/help/gene_name),
  [ORF](https://www.uniprot.org/help/gene_name) or [UniProt Gene
  Name](https://www.uniprot.org/help/gene_name).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

a list containing gene-centric proteins search hits.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/genecentric"

## References

- The UniProt Consortium , UniProt: the Universal Protein Knowledgebase
  in 2025, Nucleic Acids Research, 2024;, gkae1010,
  https://doi.org/10.1093/nar/gkae1010

- Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated
  protein and genome information, Nucleic Acids Research, Volume 45,
  Issue W1, 3 July 2017, Pages W539–W544,
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
