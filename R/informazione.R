#' Informazione del gioco
#' 
#' Calcola l'informazione dello stato attuale del gioco
#' 
#' @param game lo stato del gioco
#' 
#' @return valore numerico dell'informazione

informazione <- function(game) {
  game |> 
    dplyr::summarise(
      information = -log(sum(combinazione_possibile))
    ) |> 
    dplyr::pull(information)
}

