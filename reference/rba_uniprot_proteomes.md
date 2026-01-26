# Get proteome by proteome/proteins UPID

UniProt collects and annotates proteomes(Protein sets expressed in an
organism). Using this function you can search UniProt for available
proteomes. see [What are
proteomes?](https://www.uniprot.org/help/proteome) for more information.

## Usage

``` r
rba_uniprot_proteomes(upid, get_proteins = FALSE, reviewed = NULL, ...)
```

## Arguments

- upid:

  [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- get_proteins:

  logical: set FALSE (default) to only return information of the
  proteome with supplied UPID, set TRUE to also return the proteins of
  the supplied proteome UPID.

- reviewed:

  Logical: Only considered when get_proteins is TRUE. If TRUE, only
  return "UniProtKB/Swiss-Prot" (reviewed) proteins; If FALSE, only
  return TrEMBL (un-reviewed) entries. leave it as NULL if you do not
  want to filter proteins based on their review status.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

a list containing information of the proteome with your supplied UPID
that can contain the proteomes protein entries based on the value of
get_proteins argument.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/proteomes/proteins/{upid}"  
"GET https://ebi.ac.uk/proteins/api/proteomes/{upid}"

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
[`rba_uniprot_genecentric_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric_search.md),
[`rba_uniprot_proteomes_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteomes(upid = "UP000000354")
# }
# \donttest{
rba_uniprot_proteomes(upid = "UP000000354", get_proteins = TRUE)
# }
```
