library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

# Carica le funzioni del gioco
source("R/new_game.R")
source("R/missione_fallita.R")
source("R/proponi_squadra.R")
source("R/probabilita_marginale.R")
source("R/information_gain.R")

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

      navset_card_tab(
        nav_panel(
          "Probabilità",
          card_body(
            h5("Probabilità marginale di essere cattivo"),
            DTOutput("prob_marginali")
          )
        ),
        nav_panel(
          "Stato Gioco",
          card_body(
            h5("Combinazioni possibili"),
            textOutput("info_stato"),
            hr(),
            DTOutput("stato_gioco")
          )
        ),
        nav_panel(
          "Squadre Consigliate",
          card_body(
            h5("Squadre ordinate per rischio crescente"),
            DTOutput("squadre_consigliate")
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

  # Probabilità pre-calcolate (evita ricalcoli multipli)
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

    # Aggiorna le scelte per i membri missione
    updateSelectizeInput(session, "membri_missione", choices = giocatori)

    # Vai alla schermata di gioco
    nav_select("navbar", "gioco")

    n_cattivi <- ceiling((length(giocatori) + 1) / 3)
    showNotification(
      paste(
        "Partita iniziata con",
        length(giocatori),
        "giocatori e",
        n_cattivi,
        "cattivi"
      ),
      type = "message"
    )
  })

  # Nuova partita
  observeEvent(input$nuova_partita, {
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
      paste(
        "Missione registrata:",
        paste(input$membri_missione, collapse = ", ")
      ),
      type = "warning"
    )
  })

  # Calcola squadre
  observeEvent(input$calcola_squadre, {
    req(state$game)

    # Mostra indicatore di caricamento
    showNotification(
      "Calcolo squadre in corso...",
      id = "calc_squadre",
      duration = NULL
    )

    state$squadre <- proponi_squadra(state$game, input$n_membri_squadra)

    removeNotification("calc_squadre")
    showNotification("Squadre calcolate!", type = "message")
  })

  # Output: Probabilità marginali (usa cache)
  output$prob_marginali <- renderDT({
    req(prob_cached())

    prob <- prob_cached() |>
      arrange(desc(perc)) |>
      mutate(perc_display = scales::percent(perc, accuracy = 0.1))

    datatable(
      prob |> select(nome_cattivo, perc_display),
      colnames = c("Giocatore", "P(Cattivo)"),
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      selection = "none"
    )
  })

  # Output: Stato gioco
  output$info_stato <- renderText({
    req(state$game)
    n_possibili <- sum(state$game$combinazione_possibile)
    n_totali <- nrow(state$game)
    paste("Combinazioni possibili:", n_possibili, "su", n_totali)
  })

  output$stato_gioco <- renderDT({
    req(state$game)

    df <- state$game |>
      filter(combinazione_possibile) |>
      select(-combinazione_possibile)

    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip'
      ),
      rownames = FALSE,
      selection = "none"
    )
  })

  # Output: Squadre consigliate
  output$squadre_consigliate <- renderDT({
    req(state$squadre)

    df <- state$squadre |>
      mutate(
        prob = scales::percent(prob, accuracy = 0.1),
        information_gain = round(information_gain, 3)
      )

    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'tip'
      ),
      rownames = FALSE,
      selection = "none"
    )
  })
}

shinyApp(ui, server)
