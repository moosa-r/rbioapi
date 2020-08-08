#' rbioapi: User-friendly interface to Biological Databases' APIs
#'
#' @description This package provides an easy, user-friendly and unified access
#' toBiological databases' API. rbioapi tries to provide a unified experience,
#' effortless and of course, user-friendly interface tothe most relevant
#' Biological databases. The end user is not expected to haveknowlage about
#' curl, api, http requests, etc.This is an on-going project; new databases
#' will be implemented periodically.Do you see yourself querying data from a
#' certain database which is notsupported by this package? feel free to contact
#' me and I will try toimplement that database in the future releases.
#'
#' @section STRING functions:
#' map, interactions, image
#'
#' @docType package
#' @name rbioapi
NULL

.onAttach = function(libname, pkgname) {
  packageStartupMessage("Welcome to rbioAPI.\r\n",
                        "This package provides an easy, user-friendly and ",
                        "unified access to Biological databases' API from R.\r\n",
                        "Due to nature of this package, some function could ",
                        "break after the databases changes in their API. ",
                        "if so, kindly report the broken function[s] to:\r\n",
                        "https://github.com/moosa-r/rbioapi ",
                        "or moosa.rezwani@gmail.com\r\n",
                        "Last API verbs check for currently supported Databases:\r\n",
                        "STRING: Aug 04 2020\r\n",
                        "Enrichr: Aug 04 2020"
  )
  ## save list of supported databases
  options("rba_databases" = c("STRING",
                              "Enrichr",
                              "Ensembl"))
  ## save each database names & "base URL" as an option.
  options("rba_url_string" = c("STRING" = "https://string-db.org"))
  options("rba_url_enrichr" = c("Enrichr" = "https://amp.pharm.mssm.edu"))
  options("rba_url_ensembl" = c("Ensembl" = "http://rest.ensembl.org"))
  ## save user agent for api Calls as an option
  options("rba_ua" = "rbioapi R package") #default user agent

}


genes = c("p53", "BRCA1", "cdk2", "Q99835", "CDC42","CDK1","KIF23","PLK1",
          "RAC2","RACGAP1","RHOA","RHOB", "PHF14", "RBM3", "MSL1",
          "PHF21A", "ARL10", "INSR", "JADE2",
          "P2RX7", "LINC00662", "CCDC101", "PPM1B", "KANSL1L", "CRYZL1",
          "ANAPC16", "TMCC1", "CDH8", "RBM11", "CNPY2", "HSPA1L", "CUL2",
          "PLBD2", "LARP7", "TECPR2", "ZNF302", "CUX1", "MOB2", "CYTH2",
          "SEC22C", "EIF4E3", "ROBO2", "ADAMTS9-AS2", "CXXC1", "LINC01314",
          "ATF7", "ATP5F1")
