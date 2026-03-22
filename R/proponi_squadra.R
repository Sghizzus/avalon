#' Propone una squadra
#' 
#' Crea una tabella delle possibili scelte di squadre ordinate per probabilità
#' crescente che ci sia almeno un cattivo.
#' 
#' @param game stato della partita
#' @param n_membri numero di membri da proporre
#' 
#' @result tibble con le combinazioni ordinate dalla meno rischiosa alla più rischiosa


proponi_squadra <- function(game, n_membri) {
  giocatori <- game |> 
    pivot_longer(starts_with("cattivo")) |> 
    distinct(value) |> 
    pull(value)
  
  possibili_squadre <- giocatori |> 
    combn(n_membri) |> 
    t()
  
  colnames(possibili_squadre) <- str_c("membro_", seq_len(n_membri))
  
  possibili_squadre
  
  p <- double(nrow(possibili_squadre))
  ig <- double(nrow(possibili_squadre))
  
  for(i in seq_along(p)) {
    
    p[i] <- game |>
      filter(combinazione_possibile) |> 
      dplyr::rowwise() |> 
      dplyr::mutate(
        almeno_un_cattivo = any(dplyr::c_across(starts_with("cattivo")) %in% possibili_squadre[i, ])
      ) |> 
      pull(almeno_un_cattivo) |> 
      mean()
    
    ig[i] <- do.call(
      information_gain,
      c(list(game), as.list(possibili_squadre[i, ]))
    )
    
  }
  
  
  possibili_squadre |> 
    as_tibble() |> 
    mutate(
      prob = p,
      information_gain = ig) |> 
    arrange(prob)
}





