#' Get lifehistory data
#' @param lifehistorydata life history characteristics
#' @param localities FIP locations
#' @param fipinventory Inventory of FIP projects from fisheryprogress.org
#'
#' @return lifehistory.trait
#' @export
#'
#' @examples
get_lifehistory <- function(lifehistorydata, localities, lifehistorytraits, species.sm){

  fishery.species <- species.sm %>% 
    dplyr::rename(species = nombre_cientifico) %>% 
  dplyr::select(species, status)
  
  fiplocal.bc <- localities %>%
    dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN) 
  
  fiplocal.bc.id <- fiplocal.bc %>%
    dplyr::distinct(CVE_LOC)
  
   lf.traits <- lifehistorytraits %>%
    dplyr::select(-Trait,-Definition) %>%
    dplyr::filter(quant_index == 1) %>%
    tidyr::unnest(cols = c(Low, Moderate, High, `Very high`)) %>%
    tidyr::pivot_longer(cols=Low:`Very high`,names_to = "category") %>%
    dplyr::mutate(score=dplyr::if_else(category=="Low", 1,
                                       dplyr::if_else(category=="Moderate", 2,
                                                      dplyr::if_else(category=="High", 3, 4)))) %>%
    dplyr::rename(traits = col_name) %>%
    dplyr::select(-quant_index, -ideal_directionality)

  lf.data <- lifehistorydata %>% 
    dplyr::rename(species = nombre_cientifico)
    
  col.traits <- lf.traits %>%
    dplyr::distinct(traits) %>%
    dplyr::pull(traits)

  lifehistory.bc <- fishery.species %>%
    dplyr::left_join(lf.data, by=c("species")) %>%
    dplyr::filter(!is.na(nombre_comun)) %>%
    dplyr::select(species, status, all_of(col.traits)) %>%
    tidyr::pivot_longer(cols=3:ncol(.),names_to = "traits") %>%
    dplyr::left_join(lf.traits, by=c("traits","value"))  


  return(lifehistory.bc)

}

