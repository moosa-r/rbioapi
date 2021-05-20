% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrichr.R
\name{rba_enrichr_libs}
\alias{rba_enrichr_libs}
\title{Retrieve a List of available libraries from Enrichr}
\usage{
rba_enrichr_libs(store_in_options = FALSE, organism = "human", ...)
}
\arguments{
\item{store_in_options}{logical: (default = TRUE) Should a list of available
Enrichr libraries be saved as a global option?}

\item{organism}{(default = "human") Which model organism version of Enrichr
to use? Available options are: "human", (H. sapiens & M. musculus),
"fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans)
and "fish" (D. rerio).}

\item{...}{rbioapi option(s). Refer to \code{\link{rba_options}}'s
arguments documentation for more information on available options.}
}
\value{
A data frame with the names of available library in Enrichr and their
  statistics.
}
\description{
This function will retrieve a list of available libraries in Enrichr with
  their statistics. And by default, will save those names as a global option
  ("rba_enrichr_libs") to be available for other Enrichr functions that
  internally require the names of Enrichr libraries.
}
\details{
You should call this function once per R session with the argument
  'store_in_options = TRUE' before using \code{\link{rba_enrichr_enrich}}
  or \code{\link{rba_enrichr}}.
  \cr Nevertheless, rbioapi will do this for you in the background at the
  first time you call any function which requires this.
 \cr Note that using \code{\link{rba_enrichr}} is a more convenient way to
   automatically perform this and other required function calls to enrich
   your input gene-set.
}
\section{Corresponding API Resources}{

 "GET https://maayanlab.cloud/Enrichr/datasetStatistics"
}

\examples{
\donttest{
rba_enrichr_libs()
}

}
\references{
\itemize{
  \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
  Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
  enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
  \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
  Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
  Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
  analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
  \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
  Documentation}
  }
}
\seealso{
\code{\link{rba_enrichr}}

Other "Enrichr": 
\code{\link{rba_enrichr_add_list}()},
\code{\link{rba_enrichr_enrich}()},
\code{\link{rba_enrichr_gene_map}()},
\code{\link{rba_enrichr_view_list}()},
\code{\link{rba_enrichr}()}
}
\concept{"Enrichr"}