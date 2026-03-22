library(shiny)
library(bslib)
library(gt)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(forcats)

# Carica le funzioni del gioco
source("R/new_game.R")
source("R/missione_fallita.R")
source("R/proponi_squadra.R")
source("R/probabilita_marginale.R")
source("R/information_gain.R")
source("R/informazione.R")

ui <- page_navbar(
  title = "Avalon Helper",
  theme = bs_theme(bootswatch = "darkly"),
  id = "navbar",

  # Schermata 1: Setup
  nav_panel(
    "Setup Partita",
    value = "setup",
    layout_sidebar(
      sidebar = sidebar(
        title = "Giocatori",
        textAreaInput(
          "giocatori_input",
          "Inserisci i giocatori (uno per riga, escluso te stesso):",
          rows = 8,
          placeholder = "Mario\nLuigi\nPeach\nBowser\nToad"
        ),
        actionButton(
          "inizia_partita",
          "Inizia Partita",
          class = "btn-success btn-lg w-100"
        )
      ),
      card(
        card_header("Istruzioni"),
        card_body(
          h4("Benvenuto in Avalon Helper!"),
          p(
            "Questo strumento ti aiuta a tenere traccia delle probabilità durante una partita di Avalon."
          ),
          tags$ul(
            tags$li("Inserisci i nomi di tutti i giocatori tranne te stesso"),
            tags$li("Premi 'Inizia Partita' per cominciare"),
            tags$li(
              "Usa la schermata di gioco per registrare le missioni fallite"
            ),
            tags$li("Consulta le probabilità per decidere le squadre migliori")
          ),
          hr(),
          p(
            "Il numero di cattivi viene calcolato automaticamente in base al numero di giocatori."
          )
        )
      )
    )
  ),

  # Schermata 2: Gestione Partita
  nav_panel(
    "Gestione Partita",
    value = "gioco",
    layout_sidebar(
      sidebar = sidebar(
        title = "Azioni",
        width = 350,

        accordion(
          accordion_panel(
            "Missione Fallita",
            icon = icon("times-circle"),
            selectizeInput(
              "membri_missione",
              "Membri della missione:",
              choices = NULL,
              multiple = TRUE
            ),
            numericInput(
              "voti_negativi",
              "Voti negativi:",
              value = 1,
              min = 1,
              max = 5
            ),
            actionButton(
              "registra_missione",
              "Registra Missione Fallita",
              class = "btn-danger w-100"
            )
          ),
          accordion_panel(
            "Proponi Squadra",
            icon = icon("users"),
            numericInput(
              "n_membri_squadra",
              "Numero membri squadra:",
              value = 2,
              min = 2,
              max = 5
            ),
            actionButton(
              "calcola_squadre",
              "Calcola Squadre Migliori",
              class = "btn-primary w-100"
            )
          ),
          open = "Missione Fallita"
        ),

        hr(),
        actionButton(
          "nuova_partita",
          "Nuova Partita",
          class = "btn-warning w-100"
        )
      ),

      layout_columns(
        col_widths = c(4, 8),

        # Colonna sinistra: Probabilità (tabella + grafico impilati)
        layout_columns(
          col_widths = 12,
          card(
            card_header("Probabilità di essere cattivo"),
            card_body(
              gt_output("prob_marginali_gt")
            )
          ),
          card(
            card_header("Visualizzazione Probabilità"),
            card_body(
              girafeOutput("prob_plot", height = "250px")
            )
          )
        ),

        # Colonna destra: Tabs per stato e squadre (più spazio verticale)
        navset_card_tab(
          nav_panel(
            "Stato Gioco",
            card_body(
              textOutput("info_stato"),
              hr(),
              gt_output("stato_gioco_gt")
            )
          ),
          nav_panel(
            "Squadre Consigliate",
            card_body(
              gt_output("squadre_gt")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Stato reattivo
  state <- reactiveValues(
    game = NULL,
    giocatori = NULL,
    squadre = NULL
  )

  # Probabilità pre-calcolate
  prob_cached <- reactiveVal(NULL)

  # Ricalcola probabilità solo quando cambia il game
  observe({
    req(state$game)
    prob_cached(probabilita_marginali(state$game))
  }) |>
    bindEvent(state$game)

  # Inizia partita
  observeEvent(input$inizia_partita, {
    giocatori <- input$giocatori_input |>
      strsplit("\n") |>
      unlist() |>
      trimws() |>
      (\(x) x[x != ""])()

    if (length(giocatori) < 2) {
      showNotification("Inserisci almeno 2 giocatori!", type = "error")
      return()
    }

    state$giocatori <- giocatori
    state$game <- do.call(new_game, as.list(giocatori))
    state$squadre <- NULL
    prob_cached(NULL)

    updateSelectizeInput(session, "membri_missione", choices = giocatori)
    nav_select("navbar", "gioco")

    n_cattivi <- ceiling((length(giocatori) + 1) / 3)
    showNotification(
      str_glue(
        "Partita iniziata con {length(giocatori)} giocatori e {n_cattivi} cattivi"
      ),
      type = "message"
    )
  })

  # Nuova partita - mostra modal per chiedere se mantenere giocatori
  observeEvent(input$nuova_partita, {
    showModal(modalDialog(
      title = "Nuova Partita",
      "Vuoi mantenere gli stessi giocatori?",
      footer = tagList(
        actionButton(
          "nuova_stessi_giocatori",
          "Sì, stessi giocatori",
          class = "btn-success"
        ),
        actionButton(
          "nuova_giocatori_diversi",
          "No, cambia giocatori",
          class = "btn-warning"
        ),
        modalButton("Annulla")
      )
    ))
  })

  # Nuova partita con stessi giocatori
  observeEvent(input$nuova_stessi_giocatori, {
    req(state$giocatori)
    removeModal()

    state$game <- do.call(new_game, as.list(state$giocatori))
    state$squadre <- NULL
    prob_cached(NULL)

    showNotification(
      "Nuova partita iniziata con gli stessi giocatori",
      type = "message"
    )
  })

  # Nuova partita con giocatori diversi
  observeEvent(input$nuova_giocatori_diversi, {
    removeModal()

    state$game <- NULL
    state$giocatori <- NULL
    state$squadre <- NULL
    prob_cached(NULL)
    updateTextAreaInput(session, "giocatori_input", value = "")
    nav_select("navbar", "setup")

    showNotification("Partita resettata", type = "message")
  })

  # Registra missione fallita
  observeEvent(input$registra_missione, {
    req(state$game)

    if (length(input$membri_missione) == 0) {
      showNotification("Seleziona i membri della missione!", type = "error")
      return()
    }

    state$game <- do.call(
      missione_fallita,
      c(
        list(state$game),
        as.list(input$membri_missione),
        list(voti_negativi = input$voti_negativi)
      )
    )

    state$squadre <- NULL

    showNotification(
      str_glue(
        "Missione registrata: {str_c(input$membri_missione, collapse = ', ')}"
      ),
      type = "warning"
    )
  })

  # Calcola squadre
  observeEvent(input$calcola_squadre, {
    req(state$game)

    showNotification(
      "Calcolo squadre in corso...",
      id = "calc_squadre",
      duration = NULL
    )
    state$squadre <- proponi_squadra(state$game, input$n_membri_squadra)
    removeNotification("calc_squadre")
    showNotification("Squadre calcolate!", type = "message")
  })

  # Output: Tabella GT probabilità marginali
  output$prob_marginali_gt <- render_gt({
    req(prob_cached())

    prob_cached() |>
      arrange(desc(perc)) |>
      gt() |>
      cols_label(
        nome_cattivo = "Giocatore",
        perc = "P(Cattivo)"
      ) |>
      fmt_percent(columns = perc, decimals = 1) |>
      data_color(
        columns = perc,
        palette = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c"),
        domain = c(0, 1)
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = nome_cattivo)
      ) |>
      tab_options(
        table.background.color = "#2d2d2d",
        table.font.color = "white",
        column_labels.background.color = "#1a1a1a"
      )
  })

  # Output: Grafico interattivo probabilità
  output$prob_plot <- renderGirafe({
    req(prob_cached())

    df <- prob_cached() |>
      arrange(desc(perc)) |>
      mutate(
        nome_cattivo = as.character(nome_cattivo),
        nome_cattivo = fct_inorder(nome_cattivo),
        tooltip = paste0(
          nome_cattivo,
          ": ",
          scales::percent(perc, accuracy = 0.1)
        ),
        fill_color = case_when(
          perc >= 0.7 ~ "#e74c3c",
          perc >= 0.5 ~ "#e67e22",
          perc >= 0.3 ~ "#f1c40f",
          TRUE ~ "#2ecc71"
        )
      )

    p <- ggplot(df, aes(x = nome_cattivo, y = perc, fill = fill_color)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = nome_cattivo),
        width = 0.7
      ) +
      scale_fill_identity() +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = NULL, y = "Probabilità") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#2d2d2d", color = NA),
        panel.background = element_rect(fill = "#2d2d2d", color = NA),
        panel.grid.major = element_line(color = "#444444"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "fill:#3498db;cursor:pointer;"),
        opts_tooltip(
          css = "background-color:#1a1a1a;color:white;padding:8px;border-radius:4px;"
        )
      )
    )
  })

  # Output: Info stato
  output$info_stato <- renderText({
    req(state$game)
    n_possibili <- sum(state$game$combinazione_possibile)
    n_totali <- nrow(state$game)
    str_glue("Combinazioni possibili: {n_possibili} su {n_totali}")
  })

  # Output: Tabella stato gioco
  output$stato_gioco_gt <- render_gt({
    req(state$game)

    df <- state$game |>
      filter(combinazione_possibile) |>
      select(-combinazione_possibile) |>
      head(20)

    df |>
      gt() |>
      tab_options(
        table.background.color = "#2d2d2d",
        table.font.color = "white",
        column_labels.background.color = "#1a1a1a"
      )
  })

  # Helper per creare etichetta squadra
  make_squadra_label <- function(df) {
    df |>
      select(starts_with("membro_")) |>
      pmap_chr(~ str_c(c(...), collapse = ", "))
  }

  # Output: Tabella squadre
  output$squadre_gt <- render_gt({
    req(state$squadre)

    df <- state$squadre |>
      mutate(squadra = make_squadra_label(pick(starts_with("membro_")))) |>
      select(squadra, prob, information_gain)

    tbl <- df |>
      gt() |>
      cols_label(
        squadra = "Squadra",
        prob = "P(Cattivi)",
        information_gain = "Info Gain"
      ) |>
      fmt_percent(columns = prob, decimals = 1) |>
      fmt_number(columns = information_gain, decimals = 3) |>
      data_color(
        columns = prob,
        palette = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c"),
        domain = c(0, 1)
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = squadra)
      ) |>
      tab_options(
        table.background.color = "#2d2d2d",
        table.font.color = "white",
        column_labels.background.color = "#1a1a1a"
      )

    # Colora information_gain solo se possibile
    tbl <- tryCatch(
      tbl |>
        data_color(
          columns = information_gain,
          palette = c("#ecf0f1", "#3498db")
        ),
      error = function(e) tbl
    )

    tbl
  })
}

shinyApp(ui, server)
