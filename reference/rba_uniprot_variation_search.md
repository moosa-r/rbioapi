# Search UniProt Natural Variants

Using this function, you can search and retrieve [Natural
variant(s)](https://www.uniprot.org/help/variant) that has been
annotated in the protein's sequences. You may also refine your search
with modifiers such as source type, disease etc. See "Arguments section"
for more information.

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

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- source_type:

  Variation's source type. You can choose up to two of: "UniProt",
  "large scale study" and/or "mixed".

- consequence_type:

  Variation's consequence type. You can choose up to two of: "missense",
  "stop gained" or "stop lost".

- wild_type:

  Wild type amino acid. Accepted values are IUPAC single-letter amino
  acid (e.g. D for Aspartic acid) and "\*" for stop codon. You can
  supply up to 20 values.

- alternative_sequence:

  Alternative amino acid. Accepted values are IUPAC single-letter amino
  acid (e.g. D for Aspartic acid) and "\*" for stop codon and "-" for
  deletion. You can supply up to 20 values.

- location:

  A valid amino acid range (e.g. 10-25) within the sequence range where
  the variation occurs. You can supply up to 20 values.

- disease:

  [Human disease](https://www.uniprot.org/diseases/) that are associated
  with a sequence variation. Accepted values are disease name (e.g.
  Alzheimer disease 18), partial disease name (Alzheimer) and/or disease
  acronym (e.g. AD). You can supply up to 20 values.

- omim:

  [OMIM](https://www.ncbi.nlm.nih.gov/omim) ID that is associated with a
  variation. You can supply up to 20 values.

- evidence:

  Pubmed ID of the variation's
  [citation](https://www.uniprot.org/citations/) You can supply up to 20
  values.

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- db_type:

  cross-reference database of the variation. You can supply up to two of
  the following:

  - "dbSNP": [NIH-NCBI dbSNP
    database](https://www.ncbi.nlm.nih.gov/snp/).

  - "cosmic curate": [COSMIC (the Catalogue of Somatic Mutations in
    Cancer)](https://cancer.sanger.ac.uk/cosmic/)

  - "ClinVar": [NIH-NCBI ClinVar](https://www.ncbi.nlm.nih.gov/clinvar/)

- db_id:

  The variation ID in a Cross-reference (external) database. You can
  supply up to 20 values.

- save_peff:

  Logical or Character:

  - FALSE: (default) Do not save PEFF file, just return as a list
    object.

  - TRUE: Save as PEFF file to an automatically-generated path.

  - Character string: A valid file path to save the PEFF file.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List where each element corresponds to one UniProt entity returned by
your search query. The element itself is a sub-list containing all
information that UniProt has about that Variation.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/variation"

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

Other "UniProt - Variation":
[`rba_uniprot_variation()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation.md)

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
