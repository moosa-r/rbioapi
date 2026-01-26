# Search Mutagenesis in UniProt

UniProt describes the effects of mutations in proteins' amino acid
sequence on the biological properties of the protein, cell or the
organism. Using this function, you can search for [mutagenesis
description](https://www.uniprot.org/help/mutagen) in UniProt proteins.
You may also refine your search. See "Arguments section" for more
information.

## Usage

``` r
rba_uniprot_mutagenesis_search(
  accession = NULL,
  taxid = NULL,
  db_id = NULL,
  ...
)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- db_id:

  The ID in a Cross-reference (external) database. You can supply up to
  20 values.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list Where each element correspond to a UniProt protein (search hit)
and mutagenesis description are organized under the "features" sub-list.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/mutagenesis"

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

Other "UniProt - Mutagenesis":
[`rba_uniprot_mutagenesis()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis.md)

## Examples

``` r
# \donttest{
#search all mutations in COVID19 proteins
rba_uniprot_mutagenesis_search(taxid = 2697049)
# }
```
