#' Nuova partita di Avalon
#' 
#' Crea una tabella delle possibilità su chi sono i cattivi
#' 
#' @param ... elenco dei giocatori escluso te stesso
#' @return una tabella delle possibili combinazioni di cattivi

new_game <- function(...) {
  
  args <- list(...)
  giocatori <- unlist(args)
  n_cattivi <- ceiling((length(giocatori) + 1) / 3)
  
  eventi_possibili <- combn(giocatori, n_cattivi) |> 
    t()
  
  colnames(eventi_possibili) <- stringr::str_c("cattivo_", seq_len(n_cattivi))
  
  eventi_possibili |> 
    tibble::as_tibble() |> 
    dplyr::mutate(
      combinazione_possibile = TRUE
    )
}
