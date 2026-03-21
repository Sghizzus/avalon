#' Gestisce la missione fallita
#' 
#' Aggiorna la tabella della partita inseguito alle informazioni della missione fallita
#' 
#' @param game la tabella della partita corrente
#' @param ... chi ha partecipato alla missione
#' @param voti_negativi quanti voti negativi sono usciti durante la missione
#' 
#' @return tabella delle possibilità aggiornata

missione_fallita <- function(game, ..., voti_negativi = 1) {
  
  args <- list(...)
  membri <- unlist(args)
  
  game |>
    dplyr::rowwise() |> 
    dplyr::mutate(
      combinazione_possibile = combinazione_possibile & (sum(dplyr::c_across(starts_with("cattivo")) %in% membri) >= voti_negativi)
    ) |> 
    dplyr::ungroup()
}
