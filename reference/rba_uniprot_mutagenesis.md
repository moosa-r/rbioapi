# Get Mutagenesis by UniProt Accession

UniProt describes the effects of mutations in proteins' amino acid
sequence on the biological properties of the protein, cell or the
organism. Using this function, you can get the [Mutagenesis
description](https://www.uniprot.org/help/mutagen) that has been mapped
to a given UniProt protein.

## Usage

``` r
rba_uniprot_mutagenesis(accession, location = NULL, ...)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s).

- location:

  A valid amino acid range (e.g. 10-25) within the sequence range of the
  given proein.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the mutagenesis description of your supplied UniProt
protein's sequence.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/mutagenesis/{accession}"

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
[`rba_uniprot_mutagenesis_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_mutagenesis(accession = "P0DTC2", location = "300-400")
# }
```
