#' Information gain
#' 
#' Calcola l'informazione che si acquisirebbe nel caso uscisse un cattivo nella squadra proposta
#' 
#' @param game la tabella della partita corrente
#' @param ... la squadra proposta
#' 
#' @return numerico con l'information gain della proposta

information_gain <- function(game, ...) {
  
  args <- list(...)
  membri <- unlist(args)
  
  informazione_prima <- game |> 
    informazione()
  
  informazione_dopo <- game |>
    dplyr::rowwise() |> 
    dplyr::mutate(
      combinazione_possibile = combinazione_possibile & (sum(dplyr::c_across(starts_with("cattivo")) %in% membri) >= 1)
    ) |> 
    dplyr::ungroup() |> 
    informazione()
  
  informazione_dopo - informazione_prima
  
}
