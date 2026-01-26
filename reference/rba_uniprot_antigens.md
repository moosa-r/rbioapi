# Get Antigens by UniProt Accession

UniProt maps Antigenic features from different sources to the proteins'
sequences. Using this function, you can retrieve all the Antigenic
features that has been map to a given UniProt protein's sequence.

## Usage

``` r
rba_uniprot_antigens(accession, ...)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the Antigenic features of your supplied UniProt
protein's sequence.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/antigen/{accession}"

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

Other "UniProt - Antigen":
[`rba_uniprot_antigens_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_antigens("P04626")
# }
```
