library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(ebal)
library(ggplot2)
library(DT)

candidate_vars <- c(
  "nb_menages",
  "nb_personnes_menage",
  "nb_logements",
  "nb_RP_1_piece",
  "nb_RP_2_pieces",
  "nb_RP_3_pieces",
  "nb_RP_4_pieces",
  "nb_RP_5_piece_et_plus",
  "nb_RP_en_loc",
  "nb_RP_proprio",
  "nb_personnes_en_RP",
  "nb_personnes_en_RP_location",
  "nb_personnes_en_RP_proprio",
  "nb_residences_second_ou_occ",
  "nb_logements_vacants",
  "nb_RP",
  "nb_actifs",
  "nb_actifs_occ",
  "nb_chomeurs",
  "nb_agriculteurs",
  "nb_commercants",
  "nb_cadres",
  "nb_professions_inter",
  "nb_employes",
  "nb_ouvriers",
  "nb_etudiants"
)

safe_weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

get_existing_path <- function(upload, fallback) {
  if (!is.null(upload)) return(upload$datapath)
  fallback
}

ui <- fluidPage(
  titlePanel("Application Shiny — Entropy balancing + DID"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Fichiers"),
      p("Tu peux soit placer les CSV dans le dossier de l'application, soit les charger ici."),
      fileInput("file_communes", "communes_sans_petite_couronne.csv", accept = ".csv"),
      fileInput("file_base", "base_2006_2022.csv", accept = ".csv"),
      fileInput("file_commune_names", "commune_2022.csv", accept = ".csv"),
      hr(),
      
      h4("2. Variable principale"),
      selectInput(
        "main_var",
        "Variable principale",
        choices = candidate_vars,
        selected = "nb_RP_en_loc"
      ),
      uiOutput("match_years_ui"),
      uiOutput("post_period_ui"),
      hr(),
      
      h4("3. Calage additionnel (entropy balancing)"),
      checkboxGroupInput(
        "extra_ebal_vars",
        "Variables additionnelles à caler en niveau",
        choices = candidate_vars,
        selected = NULL
      ),
      uiOutput("extra_ebal_year_ui"),
      helpText("Ces variables sont calées en niveau à l'année choisie."),
      hr(),
      
      h4("4. Contrôles pour la régression finale"),
      checkboxInput(
        "include_pretrend_reg",
        "Inclure automatiquement la différence pré-traitement dans la régression",
        value = TRUE
      ),
      checkboxGroupInput(
        "reg_controls",
        "Variables supplémentaires dans la régression DID",
        choices = candidate_vars,
        selected = NULL
      ),
      uiOutput("reg_controls_years_ui"),
      hr(),
      
      h4("5. Sorties"),
      numericInput("top_n_communes", "Nombre de communes dans le barplot des contributions", value = 30, min = 5, max = 200, step = 5),
      actionButton("run_btn", "Lancer l'analyse", class = "btn-primary"),
      br(), br(),
      downloadButton("download_weights", "Télécharger les poids"),
      downloadButton("download_analysis", "Télécharger la base d'analyse")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Résumé",
          br(),
          h4("Paramètres et échantillon"),
          verbatimTextOutput("sample_info"),
          h4("Vérification du calage"),
          DTOutput("balance_table"),
          h4("Estimation"),
          verbatimTextOutput("ate_text"),
          verbatimTextOutput("model_text")
        ),
        tabPanel(
          "Tendances",
          br(),
          plotOutput("trend_plot", height = "420px"),
          plotOutput("trend_plot_index", height = "420px"),
          h4("Tendance annuelle entre les deux années de pré-traitement"),
          DTOutput("trend_table")
        ),
        tabPanel(
          "Poids",
          br(),
          verbatimTextOutput("weights_summary"),
          plotOutput("weights_hist", height = "320px"),
          plotOutput("weights_density", height = "320px"),
          plotOutput("weights_barplot", height = "700px"),
          DTOutput("weights_by_commune")
        ),
        tabPanel(
          "Base d'analyse",
          br(),
          DTOutput("analysis_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactive({
    communes_path <- get_existing_path(input$file_communes, "communes_sans_petite_couronne.csv")
    base_path <- get_existing_path(input$file_base, "base_2006_2022.csv")
    names_path <- get_existing_path(input$file_commune_names, "commune_2022.csv")
    
    req(file.exists(communes_path), file.exists(base_path))
    
    communes_cluster_paris <- fread(communes_path)
    data <- fread(base_path)
    commune_names <- if (file.exists(names_path)) fread(names_path) else NULL
    
    validate(
      need("COM" %in% names(communes_cluster_paris), "Le fichier communes_sans_petite_couronne.csv doit contenir la colonne COM."),
      need(all(c("COM", "IRIS", "annee") %in% names(data)), "La base principale doit contenir COM, IRIS et annee."),
      need(is.null(commune_names) || all(c("COM", "LIBELLE") %in% names(commune_names)), "Le fichier commune_2022.csv doit contenir les colonnes COM et LIBELLE.")
    )
    
    data <- data %>% mutate(COM = as.character(COM), IRIS = as.character(IRIS))
    communes_cluster_paris <- communes_cluster_paris %>% mutate(COM = as.character(COM))
    if (!is.null(commune_names)) commune_names <- commune_names %>% mutate(COM = as.character(COM))
    
    list(
      communes = communes_cluster_paris,
      data = data,
      commune_names = commune_names
    )
  })
  
  available_years <- reactive({
    yrs <- sort(unique(raw_data()$data$annee))
    yrs[!is.na(yrs)]
  })
  
  output$match_years_ui <- renderUI({
    yrs <- available_years()
    default <- intersect(c(2012, 2017), yrs)
    if (length(default) != 2 && length(yrs) >= 2) default <- yrs[1:2]
    
    selectInput(
      "match_years",
      "Période de calage pré-traitement (différence)",
      choices = yrs,
      selected = default,
      multiple = TRUE
    )
  })
  
  output$post_period_ui <- renderUI({
    yrs <- available_years()
    default <- intersect(c(2017, 2022), yrs)
    if (length(default) != 2 && length(yrs) >= 2) default <- tail(yrs, 2)
    
    selectInput(
      "post_period_years",
      "Période de traitement / évolution post-traitement",
      choices = yrs,
      selected = default,
      multiple = TRUE
    )
  })
  
  output$extra_ebal_year_ui <- renderUI({
    yrs <- available_years()
    default_year <- if (!is.null(input$match_years) && length(input$match_years) == 2) max(as.integer(input$match_years)) else max(yrs)
    
    selectInput(
      "extra_ebal_year",
      "Année de calage en niveau des variables additionnelles",
      choices = yrs,
      selected = default_year
    )
  })
  
  output$reg_controls_years_ui <- renderUI({
    req(input$reg_controls)
    if (length(input$reg_controls) == 0) return(NULL)
    
    yrs <- available_years()
    default_year <- if (!is.null(input$post_period_years) && length(input$post_period_years) == 2) min(as.integer(input$post_period_years)) else max(yrs)
    
    tagList(
      lapply(input$reg_controls, function(v) {
        selectInput(
          paste0("reg_year_", v),
          paste("Année du contrôle", v),
          choices = yrs,
          selected = default_year
        )
      })
    )
  })
  
  analysis <- eventReactive(input$run_btn, {
    raw <- raw_data()
    data <- raw$data
    donor_pool <- raw$communes
    commune_names <- raw$commune_names
    
    validate(
      need(length(input$match_years) == 2, "Sélectionne exactement 2 années pour la différence de calage pré-traitement."),
      need(length(input$post_period_years) == 2, "Sélectionne exactement 2 années pour la période de traitement."),
      need(input$main_var %in% names(data), "La variable principale n'existe pas dans la base."),
      need(all(input$extra_ebal_vars %in% names(data)), "Au moins une variable de calage additionnelle est absente de la base."),
      need(all(input$reg_controls %in% names(data)), "Au moins une variable de régression est absente de la base.")
    )
    
    pre_years <- sort(as.integer(input$match_years))
    post_years <- sort(as.integer(input$post_period_years))
    
    pre_y1 <- pre_years[1]
    pre_y2 <- pre_years[2]
    post_y1 <- post_years[1]
    post_y2 <- post_years[2]
    
    validate(
      need(pre_y2 > pre_y1, "La période de calage pré-traitement doit être une vraie différence."),
      need(post_y2 > post_y1, "La période de traitement doit être une vraie différence."),
      need(input$extra_ebal_year %in% available_years(), "L'année choisie pour le calage additionnel est invalide.")
    )
    
    paris_com <- as.character(75101:75120)
    donor_com <- as.character(donor_pool$COM)
    
    df_reduced <- data %>%
      mutate(code_com = as.character(COM)) %>%
      filter(code_com %in% donor_com | code_com %in% paris_com) %>%
      mutate(treated = as.integer(code_com %in% paris_com))
    
    validate(
      need(sum(df_reduced$treated == 1, na.rm = TRUE) > 0, "Aucune observation Paris trouvée."),
      need(sum(df_reduced$treated == 0, na.rm = TRUE) > 0, "Aucune observation contrôle trouvée.")
    )
    
    years_for_main <- unique(c(pre_y1, pre_y2, post_y1, post_y2))
    
    df_main <- df_reduced %>%
      filter(annee %in% years_for_main) %>%
      select(IRIS, code_com, treated, annee, all_of(input$main_var)) %>%
      distinct() %>%
      pivot_wider(
        names_from = annee,
        values_from = all_of(input$main_var),
        names_prefix = "y_"
      )
    
    pre_name_1 <- paste0("y_", pre_y1)
    pre_name_2 <- paste0("y_", pre_y2)
    post_name_1 <- paste0("y_", post_y1)
    post_name_2 <- paste0("y_", post_y2)
    diff_name <- paste0("d_", pre_y1, "_", pre_y2)
    outcome_name <- paste0("d_", post_y1, "_", post_y2)
    
    validate(
      need(all(c(pre_name_1, pre_name_2, post_name_1, post_name_2) %in% names(df_main)), "Impossible de construire toutes les colonnes wide nécessaires pour la variable principale.")
    )
    
    df_ebal <- df_main %>%
      mutate(
        !!diff_name := .data[[pre_name_2]] - .data[[pre_name_1]],
        !!outcome_name := .data[[post_name_2]] - .data[[post_name_1]]
      )
    
    extra_balance_names <- character(0)
    if (length(input$extra_ebal_vars) > 0) {
      df_extra <- df_reduced %>%
        filter(annee == as.integer(input$extra_ebal_year)) %>%
        select(IRIS, code_com, treated, all_of(input$extra_ebal_vars)) %>%
        distinct()
      
      rename_map <- setNames(
        paste0(input$extra_ebal_vars, "_lvl_", input$extra_ebal_year),
        input$extra_ebal_vars
      )
      names(df_extra)[match(names(rename_map), names(df_extra))] <- unname(rename_map)
      extra_balance_names <- unname(rename_map)
      
      df_ebal <- df_ebal %>%
        left_join(df_extra, by = c("IRIS", "code_com", "treated"))
    }
    
    reg_control_names <- character(0)
    if (length(input$reg_controls) > 0) {
      for (v in input$reg_controls) {
        yr <- as.integer(input[[paste0("reg_year_", v)]])
        new_name <- paste0(v, "_reg_", yr)
        
        tmp <- df_reduced %>%
          filter(annee == yr) %>%
          select(IRIS, code_com, treated, all_of(v)) %>%
          distinct()
        names(tmp)[names(tmp) == v] <- new_name
        
        reg_control_names <- c(reg_control_names, new_name)
        df_ebal <- df_ebal %>%
          left_join(tmp, by = c("IRIS", "code_com", "treated"))
      }
    }
    
    required_vars <- c(pre_name_1, pre_name_2, post_name_1, post_name_2, diff_name, outcome_name, extra_balance_names, reg_control_names)
    
    df_ebal <- df_ebal %>%
      filter(if_all(all_of(required_vars), ~ !is.na(.x)))
    
    validate(
      need(nrow(df_ebal) > 0, "Aucune observation complète après filtrage des valeurs manquantes."),
      need(length(unique(df_ebal$treated)) == 2, "Il faut des unités traitées et contrôles après filtrage."),
      need(sum(df_ebal$treated == 0) > 1, "Pas assez de contrôles pour l'entropy balancing."),
      need(sum(df_ebal$treated == 1) > 0, "Pas d'IRIS Paris après filtrage.")
    )
    
    X_names <- c(diff_name, extra_balance_names)
    X_ebal <- as.matrix(df_ebal %>% select(all_of(X_names)))
    
    eb_out <- ebalance(
      Treatment = df_ebal$treated,
      X = X_ebal
    )
    
    df_ebal <- df_ebal %>% mutate(w_ebal = 1)
    df_ebal$w_ebal[df_ebal$treated == 0] <- eb_out$w
    
    balance_table <- bind_rows(lapply(X_names, function(v) {
      data.frame(
        variable = v,
        Paris = mean(df_ebal[[v]][df_ebal$treated == 1], na.rm = TRUE),
        Controle_non_pondere = mean(df_ebal[[v]][df_ebal$treated == 0], na.rm = TRUE),
        Controle_pondere = safe_weighted_mean(df_ebal[[v]][df_ebal$treated == 0], df_ebal$w_ebal[df_ebal$treated == 0])
      )
    }))
    
    mean_treated_post <- safe_weighted_mean(df_ebal[[outcome_name]][df_ebal$treated == 1], df_ebal$w_ebal[df_ebal$treated == 1])
    mean_control_post <- safe_weighted_mean(df_ebal[[outcome_name]][df_ebal$treated == 0], df_ebal$w_ebal[df_ebal$treated == 0])
    ate <- mean_treated_post - mean_control_post
    
    rhs <- c("treated")
    if (isTRUE(input$include_pretrend_reg)) rhs <- c(rhs, diff_name)
    if (length(reg_control_names) > 0) rhs <- c(rhs, reg_control_names)
    
    formula_simple <- as.formula(paste(outcome_name, "~ treated"))
    formula_final <- as.formula(paste(outcome_name, "~", paste(rhs, collapse = " + ")))
    
    mod_simple <- lm(formula_simple, data = df_ebal, weights = w_ebal)
    mod_final <- lm(formula_final, data = df_ebal, weights = w_ebal)
    
    df_plot <- df_ebal %>%
      select(IRIS, code_com, treated, w_ebal, all_of(c(pre_name_1, pre_name_2, post_name_1, post_name_2))) %>%
      pivot_longer(
        cols = all_of(c(pre_name_1, pre_name_2, post_name_1, post_name_2)),
        names_to = "periode",
        values_to = "outcome"
      ) %>%
      mutate(
        annee = as.integer(gsub("y_", "", periode)),
        groupe = ifelse(treated == 1, "Paris", "Contrôle pondéré")
      )
    
    trend_plot <- df_plot %>%
      group_by(groupe, annee) %>%
      summarise(
        mean_outcome = safe_weighted_mean(outcome, w_ebal),
        .groups = "drop"
      ) %>%
      arrange(annee, groupe)
    
    trend_plot_index <- trend_plot %>%
      group_by(groupe) %>%
      mutate(
        base_year = min(annee),
        base_value = mean_outcome[annee == base_year][1],
        evol_depuis_base = mean_outcome - base_value
      ) %>%
      ungroup()
    
    weights_iris <- df_ebal %>%
      select(IRIS, code_com, treated, w_ebal) %>%
      distinct()
    
    df_micro <- df_reduced %>%
      filter(annee >= pre_y1, annee <= pre_y2) %>%
      left_join(weights_iris, by = c("IRIS", "code_com", "treated")) %>%
      filter(!is.na(w_ebal)) %>%
      mutate(groupe = ifelse(treated == 1, "Paris", "Contrôle pondéré"))
    
    trend_micro <- df_micro %>%
      group_by(groupe, annee) %>%
      summarise(
        mean_outcome = safe_weighted_mean(.data[[input$main_var]], w_ebal),
        .groups = "drop"
      ) %>%
      arrange(annee, groupe)
    
    weights_by_commune <- df_ebal %>%
      filter(treated == 0) %>%
      group_by(code_com) %>%
      summarise(poids_total = sum(w_ebal, na.rm = TRUE), .groups = "drop")
    
    if (!is.null(commune_names)) {
      commune_lookup <- commune_names %>%
        transmute(code_com = as.character(COM), nom_commune = LIBELLE) %>%
        distinct()
      
      weights_by_commune <- weights_by_commune %>%
        left_join(commune_lookup, by = "code_com")
    } else {
      weights_by_commune <- weights_by_commune %>%
        mutate(nom_commune = NA_character_)
    }
    
    weights_by_commune <- weights_by_commune %>%
      mutate(nom_affiche = ifelse(is.na(nom_commune) | nom_commune == "", code_com, nom_commune)) %>%
      arrange(desc(poids_total))
    
    top_communes <- weights_by_commune %>%
      slice_head(n = input$top_n_communes)
    
    list(
      df_ebal = df_ebal,
      balance_table = balance_table,
      ate = ate,
      mod_simple = mod_simple,
      mod_final = mod_final,
      trend_plot = trend_plot,
      trend_plot_index = trend_plot_index,
      trend_micro = trend_micro,
      weights_by_commune = weights_by_commune,
      top_communes = top_communes,
      diff_name = diff_name,
      outcome_name = outcome_name,
      pre_y1 = pre_y1,
      pre_y2 = pre_y2,
      post_y1 = post_y1,
      post_y2 = post_y2,
      main_var = input$main_var,
      extra_balance_names = extra_balance_names,
      reg_control_names = reg_control_names
    )
  })
  
  output$sample_info <- renderPrint({
    res <- analysis()
    cat("Variable principale :", res$main_var, "\n")
    cat("Différence de calage pré-traitement :", res$diff_name, "\n")
    cat("Différence de traitement / outcome :", res$outcome_name, "\n")
    cat("Période de calage :", res$pre_y1, "->", res$pre_y2, "\n")
    cat("Période de traitement :", res$post_y1, "->", res$post_y2, "\n\n")
    cat("Nombre d'observations dans la base d'analyse :", nrow(res$df_ebal), "\n")
    cat("Nombre d'IRIS Paris :", sum(res$df_ebal$treated == 1), "\n")
    cat("Nombre d'IRIS contrôle :", sum(res$df_ebal$treated == 0), "\n\n")
    cat("Variables additionnelles de calage :\n")
    print(if (length(res$extra_balance_names) == 0) "Aucune" else res$extra_balance_names)
    cat("\nVariables supplémentaires dans la régression :\n")
    print(if (length(res$reg_control_names) == 0) "Aucune" else res$reg_control_names)
  })
  
  output$balance_table <- renderDT({
    datatable(round(analysis()$balance_table, 4), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$ate_text <- renderPrint({
    res <- analysis()
    cat("ATE pondéré estimé sur", res$outcome_name, ":", round(res$ate, 4), "\n")
  })
  
  output$model_text <- renderPrint({
    res <- analysis()
    cat("--- Régression simple ---\n")
    print(summary(res$mod_simple))
    cat("\n--- Régression finale ---\n")
    print(summary(res$mod_final))
  })
  
  output$trend_plot <- renderPlot({
    res <- analysis()
    ggplot(res$trend_plot, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      geom_vline(xintercept = c(res$pre_y2, res$post_y1), linetype = c("dashed", "dotted")) +
      scale_x_continuous(breaks = sort(unique(res$trend_plot$annee))) +
      labs(
        title = paste0("Évolution de ", res$main_var, " : Paris vs contrôle pondéré"),
        subtitle = paste0("Calage sur ", res$diff_name, if (length(res$extra_balance_names) > 0) " + variables additionnelles" else ""),
        x = "Année",
        y = paste0("Moyenne pondérée de ", res$main_var),
        color = "Groupe"
      ) +
      theme_minimal()
  })
  
  output$trend_plot_index <- renderPlot({
    res <- analysis()
    ggplot(res$trend_plot_index, aes(x = annee, y = evol_depuis_base, color = groupe, group = groupe)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      geom_vline(xintercept = c(res$pre_y2, res$post_y1), linetype = c("dashed", "dotted")) +
      scale_x_continuous(breaks = sort(unique(res$trend_plot_index$annee))) +
      labs(
        title = "Évolution cumulée depuis l'année de base",
        subtitle = paste0("Pré-trend imposé sur ", res$diff_name),
        x = "Année",
        y = "Variation par rapport à l'année de base",
        color = "Groupe"
      ) +
      theme_minimal()
  })
  
  output$trend_table <- renderDT({
    datatable(analysis()$trend_micro, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$weights_summary <- renderPrint({
    w <- analysis()$df_ebal$w_ebal[analysis()$df_ebal$treated == 0]
    summary(w)
  })
  
  output$weights_hist <- renderPlot({
    res <- analysis()
    ggplot(res$df_ebal %>% filter(treated == 0), aes(x = w_ebal)) +
      geom_histogram(bins = 40) +
      labs(
        title = "Distribution des poids entropy balancing",
        x = "Poids",
        y = "Nombre d'observations"
      ) +
      theme_minimal()
  })
  
  output$weights_density <- renderPlot({
    res <- analysis()
    ggplot(res$df_ebal %>% filter(treated == 0), aes(x = w_ebal)) +
      geom_density(fill = "steelblue", alpha = 0.35) +
      labs(
        title = "Densité des poids entropy balancing",
        x = "Poids",
        y = "Densité"
      ) +
      theme_minimal()
  })
  
  output$weights_barplot <- renderPlot({
    res <- analysis()
    ggplot(res$top_communes, aes(x = reorder(nom_affiche, poids_total), y = poids_total)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste0("Top ", nrow(res$top_communes), " des communes contribuant au contrefactuel de Paris"),
        x = "Commune",
        y = "Poids total"
      ) +
      theme_minimal()
  })
  
  output$weights_by_commune <- renderDT({
    datatable(analysis()$weights_by_commune %>% select(code_com, nom_commune, poids_total), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$analysis_table <- renderDT({
    datatable(analysis()$df_ebal, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$download_weights <- downloadHandler(
    filename = function() paste0("weights_entropy_balancing_", Sys.Date(), ".csv"),
    content = function(file) {
      fwrite(analysis()$df_ebal %>% select(IRIS, code_com, treated, w_ebal), file)
    }
  )
  
  output$download_analysis <- downloadHandler(
    filename = function() paste0("base_analyse_entropy_balancing_", Sys.Date(), ".csv"),
    content = function(file) {
      fwrite(analysis()$df_ebal, file)
    }
  )
}

shinyApp(ui = ui, server = server)
