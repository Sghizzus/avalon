#' Numero atteso di cattivi
#' 
#' Calcola il numero atteso di cattivi
#' 
#' @param game lo stato del gioco
#' @param ... i membri della squadra
#' 
#' @return tibble 1x1 con il numero atteso di cattivi nel gruppo

numero_atteso_cattivi <- function(game, ...) {
  
  args <- list(...)
  proposta <- unlist(args)
  
  game |> 
    probabilita_marginali() |> 
    dplyr::filter(
      nome_cattivo %in% proposta
    ) |> 
    dplyr::summarise(
      numero_atteso_di_cattivi = sum(perc)
    )
  
}

