#' Calcola le probabilità marginali
#' 
#' Calcola per ogni giocatore la probabilità marginale che sia cattivo. Bisogna stare attenti perchè
#' si perde l'informazione delle probabilità congiunte. A volte se qualcuno è cattivo implica che un
#' altro non lo sia o che invece lo sia.
#' 
#' @param game lo stato attuale del gioco
#' 
#' @return una tabella riepilogativa delle probabilità marginali

probabilita_marginali <- function(game) {
  game |> 
    dplyr::mutate(numero_eventi = sum(combinazione_possibile)) |>
    tidyr::pivot_longer(
      starts_with("cattivo"),
      values_to = "nome_cattivo",
      names_to = "numero_cattivo",
      names_prefix = "cattivo_",
      names_transform = as.numeric
      )  |>
    dplyr::group_by(nome_cattivo, numero_eventi) |> 
    dplyr::summarise(
      n = sum(combinazione_possibile),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      perc = n / numero_eventi
    ) |> 
    dplyr::select(nome_cattivo, perc)
}

