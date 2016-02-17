#' High-poverty Medellin neighborhoods before and after construction of the MetroCable
#'
#' Neighborhood-level variables from the quasiexperimental data Cerda et al (2012) used
#' assess affects of infrastructure investments on violence and associated variables.
#' (That paper also made use of survey data collected from people in the neighborhoods.
#' This data set contains only the aggregated data, however.  It also omits three
#' treatment-group neighborhoods for which homicide data were not available.)
#' 
#'   The treatment variable
#' \itemize{
#'   \item nhTrt        Intervention neighborhood (0=no Metrocable station, 1=Metrocable station) 
#'  }
#'  Some Covariates (there are others, see the paper itself)                           
#' \itemize{                                                                                        
#'   \item nh03,         Neighborhood id                                                          
#'   \item nhGroup,      Treatment (T) or Control (C)                                             
#'   \item nhTrt,        Treatment (1) or Control (0)                                             
#'   \item nhHom,        Mean homicide rate per 100,000 population in 2003                        
#'   \item nhDistCenter, Distance to city center (km)                                             
#'   \item nhLogHom,     Log Homicide (i.e. log(nhHom))
#'   \item nhSisben,     (I forget)
#'   \item nhPopD,       Population density(?)
#'   \item nhMarDom,     ?
#'   \item nhClass,      ?.  (This may read in as a numeric variable -- convert to factor!)
#'   \item \ldots
#'  }
#'  Outcomes (Baseline versions: nhBI03, nhCE03, nhPV03, nhQP03, nhTP03, HomCount2003, Pop2003)
#' \itemize{                                                                                        
#'   \item BE      Neighborhood amenities Score 2008                                             
#'   \item CE      Collective Efficacy Score 2008                                                
#'   \item PV      Perceived Violence Score 2008                                                 
#'   \item QP      Trust in local agencies Score 2008                                            
#'   \item TP      Reliance on police Score 2008                                                 
#'   \item hom     Homicide rate per 100,000 population Score 2008-2003 (in log odds)            
#'   \item HomCount2008 Number of homicides in 2008                                              
#'   \item Pop2008      Population in 2008                                                       
#' } 
           
#' @docType data
#'
#' @usage data(meddata) (I think)
#'
#' @format A \code{data.frame} describing neighborhoods in Medellin, Colombia.
#'
#' @keywords datasets
#'
#' @references Cerda et al. (2012) Am. J. Epidemiol. 175 (10):1045--53 (10.1093/aje/kwr428).
#' (\href{http://aje.oxfordjournals.org/cgi/content/full/kwr428?ijkey=YrGfw2zq6vOY26U&keytype=ref)
