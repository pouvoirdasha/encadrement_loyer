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
      checkboxInput(
        "exclude_zero_logements",
        "Exclure les IRIS ayant 0 logement au moins une année",
        value = FALSE
      ),
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
      fluidRow(
        column(6, actionButton("select_all_ebal", "Tout sélectionner")),
        column(6, actionButton("clear_all_ebal", "Tout vider"))
      ),
      br(),
      checkboxGroupInput(
        "extra_ebal_vars",
        "Variables additionnelles à caler en niveau",
        choices = candidate_vars,
        selected = NULL
      ),
      selectInput(
        "extra_pretrend_var",
        "Variable additionnelle à caler en différence sur la même période pré-traitement",
        choices = c("Aucune", candidate_vars),
        selected = "Aucune"
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
      fluidRow(
        column(6, actionButton("select_all_reg", "Tout sélectionner")),
        column(6, actionButton("clear_all_reg", "Tout vider"))
      ),
      br(),
      checkboxGroupInput(
        "reg_controls",
        "Variables supplémentaires dans la régression DID",
        choices = candidate_vars,
        selected = NULL
      ),
      selectInput(
        "reg_treatment_diff_var",
        "Variable additionnelle en différence sur la période de traitement (régression)",
        choices = c("Aucune", candidate_vars),
        selected = "Aucune"
      ),
      uiOutput("reg_controls_years_ui"),
      hr(),
      
      h4("5. Sorties"),
      numericInput("top_n_communes", "Nombre de communes dans le barplot des contributions", value = 30, min = 5, max = 200, step = 5),
      actionButton("run_btn", "Lancer l'analyse", class = "btn-primary"),
      br(), br(),
      downloadButton("download_weights", "Télécharger les poids"),
      downloadButton("download_analysis", "Télécharger la base d'analyse"),
      downloadButton("download_trend_png", "Télécharger le graphique des tendances (PNG)"),
      downloadButton("download_weights_png", "Télécharger le barplot des communes (PNG)")
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
          h4("Comparatif des variables sélectionnées"),
          DTOutput("selected_vars_comparison"),
          h4("Estimation"),
          verbatimTextOutput("ate_text"),
          wellPanel(
            h4("Effet agrégé Paris"),
            verbatimTextOutput("effet_paris_text")
          ),
          verbatimTextOutput("model_text")
        ),
        tabPanel(
          "Tendances",
          br(),
          plotOutput("trend_plot", height = "420px"),
          plotOutput("trend_plot_index", height = "420px"),
          h4("Tendance complète avec toutes les années disponibles"),
          plotOutput("trend_full_plot", height = "420px"),
          plotOutput("trend_full_plot_index", height = "420px"),
          h4("Décomposition année par année — variable principale"),
          plotOutput("trend_micro_plot", height = "420px"),
          plotOutput("trend_micro_plot_index", height = "420px"),
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
    yrs <- yrs[!is.na(yrs)]
    yrs[yrs >= 2007]
  })
  
  output$match_years_ui <- renderUI({
    yrs <- available_years()
    default <- intersect(c(2012, 2017), yrs)
    if (length(default) != 2 && length(yrs) >= 2) default <- yrs[1:2]
    
    tagList(
      selectInput(
        "match_year_1",
        "Année 1 du calage pré-traitement",
        choices = yrs,
        selected = default[1]
      ),
      selectInput(
        "match_year_2",
        "Année 2 du calage pré-traitement",
        choices = yrs,
        selected = default[2]
      )
    )
  })
  
  output$post_period_ui <- renderUI({
    yrs <- available_years()
    default <- intersect(c(2017, 2022), yrs)
    if (length(default) != 2 && length(yrs) >= 2) default <- tail(yrs, 2)
    
    tagList(
      selectInput(
        "post_year_1",
        "Année 1 de la période de traitement",
        choices = yrs,
        selected = default[1]
      ),
      selectInput(
        "post_year_2",
        "Année 2 de la période de traitement",
        choices = yrs,
        selected = default[2]
      )
    )
  })
  
  output$extra_ebal_year_ui <- renderUI({
    yrs <- available_years()
    default_year <- if (!is.null(input$match_year_2)) as.integer(input$match_year_2) else max(yrs)
    
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
    default_year <- if (!is.null(input$post_year_1)) as.integer(input$post_year_1) else max(yrs)
    
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
  
  observeEvent(input$select_all_ebal, {
    updateCheckboxGroupInput(session, "extra_ebal_vars", selected = candidate_vars)
  })
  
  observeEvent(input$clear_all_ebal, {
    updateCheckboxGroupInput(session, "extra_ebal_vars", selected = character(0))
  })
  
  observeEvent(input$select_all_reg, {
    updateCheckboxGroupInput(session, "reg_controls", selected = candidate_vars)
  })
  
  observeEvent(input$clear_all_reg, {
    updateCheckboxGroupInput(session, "reg_controls", selected = character(0))
  })
  
  analysis <- eventReactive(input$run_btn, {
    raw <- raw_data()
    data <- raw$data
    donor_pool <- raw$communes
    commune_names <- raw$commune_names
    
    validate(
      need(!is.null(input$match_year_1) && !is.null(input$match_year_2), "Choisis les deux années du calage pré-traitement."),
      need(!is.null(input$post_year_1) && !is.null(input$post_year_2), "Choisis les deux années de la période de traitement."),
      need(input$main_var %in% names(data), "La variable principale n'existe pas dans la base."),
      need(all(input$extra_ebal_vars %in% names(data)), "Au moins une variable de calage additionnelle est absente de la base."),
      need(input$extra_pretrend_var == "Aucune" || input$extra_pretrend_var %in% names(data), "La variable additionnelle de pré-trend est absente de la base."),
      need(all(input$reg_controls %in% names(data)), "Au moins une variable de régression est absente de la base."),
      need(input$reg_treatment_diff_var == "Aucune" || input$reg_treatment_diff_var %in% names(data), "La variable additionnelle en différence de traitement est absente de la base.")
    )
    
    pre_y1 <- as.integer(input$match_year_1)
    pre_y2 <- as.integer(input$match_year_2)
    post_y1 <- as.integer(input$post_year_1)
    post_y2 <- as.integer(input$post_year_2)
    
    validate(
      need(pre_y2 > pre_y1, "La période de calage pré-traitement doit être une vraie différence."),
      need(post_y2 > post_y1, "La période de traitement doit être une vraie différence."),
      need(pre_y1 >= 2007 && pre_y2 >= 2007 && post_y1 >= 2007 && post_y2 >= 2007, "Les années doivent être postérieures à 2006."),
      need(input$extra_ebal_year %in% available_years(), "L'année choisie pour le calage additionnel est invalide.")
    )
    
    paris_com <- as.character(75101:75120)
    donor_com <- as.character(donor_pool$COM)
    
    df_reduced <- data %>%
      mutate(code_com = as.character(COM)) %>%
      filter(code_com %in% donor_com | code_com %in% paris_com) %>%
      mutate(treated = as.integer(code_com %in% paris_com))
    
    n_iris_initial <- df_reduced %>% distinct(IRIS) %>% nrow()
    n_iris_initial_paris <- df_reduced %>% filter(treated == 1) %>% distinct(IRIS) %>% nrow()
    n_iris_initial_control <- df_reduced %>% filter(treated == 0) %>% distinct(IRIS) %>% nrow()
    n_iris_zero_removed <- 0L
    n_iris_zero_removed_paris <- 0L
    n_iris_zero_removed_control <- 0L
    
    if (isTRUE(input$exclude_zero_logements)) {
      validate(
        need("nb_logements" %in% names(df_reduced), "La variable nb_logements est absente de la base, impossible d'exclure les IRIS avec 0 logement.")
      )
      
      iris_zero_logements <- df_reduced %>%
        group_by(IRIS) %>%
        summarise(has_zero_logements = any(nb_logements == 0, na.rm = TRUE), .groups = "drop") %>%
        filter(has_zero_logements) %>%
        pull(IRIS)
      
      n_iris_zero_removed <- length(unique(iris_zero_logements))
      
      iris_zero_detail <- df_reduced %>%
        filter(IRIS %in% iris_zero_logements) %>%
        distinct(IRIS, treated)
      
      n_iris_zero_removed_paris <- iris_zero_detail %>%
        filter(treated == 1) %>%
        distinct(IRIS) %>%
        nrow()
      
      n_iris_zero_removed_control <- iris_zero_detail %>%
        filter(treated == 0) %>%
        distinct(IRIS) %>%
        nrow()
      
      df_reduced <- df_reduced %>%
        filter(!IRIS %in% iris_zero_logements)
    }
    
    n_iris_after_zero_filter <- df_reduced %>% distinct(IRIS) %>% nrow()
    n_iris_after_zero_filter_paris <- df_reduced %>% filter(treated == 1) %>% distinct(IRIS) %>% nrow()
    n_iris_after_zero_filter_control <- df_reduced %>% filter(treated == 0) %>% distinct(IRIS) %>% nrow()
    
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
    
    extra_pretrend_name <- character(0)
    if (!is.null(input$extra_pretrend_var) && input$extra_pretrend_var != "Aucune") {
      df_pretrend_extra <- df_reduced %>%
        filter(annee %in% c(pre_y1, pre_y2)) %>%
        select(IRIS, code_com, treated, annee, all_of(input$extra_pretrend_var)) %>%
        distinct() %>%
        pivot_wider(
          names_from = annee,
          values_from = all_of(input$extra_pretrend_var),
          names_prefix = "tmp_"
        )
      
      tmp_pre_1 <- paste0("tmp_", pre_y1)
      tmp_pre_2 <- paste0("tmp_", pre_y2)
      extra_pretrend_name <- paste0("d_", input$extra_pretrend_var, "_", pre_y1, "_", pre_y2)
      
      validate(
        need(all(c(tmp_pre_1, tmp_pre_2) %in% names(df_pretrend_extra)), "Impossible de construire la différence pré-traitement de la variable additionnelle.")
      )
      
      df_pretrend_extra <- df_pretrend_extra %>%
        mutate(!!extra_pretrend_name := .data[[tmp_pre_2]] - .data[[tmp_pre_1]]) %>%
        select(IRIS, code_com, treated, all_of(extra_pretrend_name))
      
      df_ebal <- df_ebal %>%
        left_join(df_pretrend_extra, by = c("IRIS", "code_com", "treated"))
    }
    
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
    reg_treatment_diff_name <- character(0)
    if (!is.null(input$reg_treatment_diff_var) && input$reg_treatment_diff_var != "Aucune") {
      df_reg_diff <- df_reduced %>%
        filter(annee %in% c(post_y1, post_y2)) %>%
        select(IRIS, code_com, treated, annee, all_of(input$reg_treatment_diff_var)) %>%
        distinct() %>%
        pivot_wider(
          names_from = annee,
          values_from = all_of(input$reg_treatment_diff_var),
          names_prefix = "regtmp_"
        )
      
      regtmp_1 <- paste0("regtmp_", post_y1)
      regtmp_2 <- paste0("regtmp_", post_y2)
      reg_treatment_diff_name <- paste0("d_", input$reg_treatment_diff_var, "_", post_y1, "_", post_y2)
      
      validate(
        need(all(c(regtmp_1, regtmp_2) %in% names(df_reg_diff)), "Impossible de construire la différence de traitement de la variable additionnelle de régression.")
      )
      
      df_reg_diff <- df_reg_diff %>%
        mutate(!!reg_treatment_diff_name := .data[[regtmp_2]] - .data[[regtmp_1]]) %>%
        select(IRIS, code_com, treated, all_of(reg_treatment_diff_name))
      
      df_ebal <- df_ebal %>%
        left_join(df_reg_diff, by = c("IRIS", "code_com", "treated"))
    }
    
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
    
    required_vars <- c(pre_name_1, pre_name_2, post_name_1, post_name_2, diff_name, outcome_name, extra_balance_names, reg_control_names, reg_treatment_diff_name)
    
    df_ebal <- df_ebal %>%
      filter(if_all(all_of(required_vars), ~ !is.na(.x)))
    
    validate(
      need(nrow(df_ebal) > 0, "Aucune observation complète après filtrage des valeurs manquantes."),
      need(length(unique(df_ebal$treated)) == 2, "Il faut des unités traitées et contrôles après filtrage."),
      need(sum(df_ebal$treated == 0) > 1, "Pas assez de contrôles pour l'entropy balancing."),
      need(sum(df_ebal$treated == 1) > 0, "Pas d'IRIS Paris après filtrage.")
    )
    
    X_names <- c(diff_name, extra_pretrend_name, extra_balance_names)
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
    
    rhs <- c("treated")
    if (isTRUE(input$include_pretrend_reg)) rhs <- c(rhs, diff_name)
    if (length(reg_treatment_diff_name) > 0) rhs <- c(rhs, reg_treatment_diff_name)
    if (length(reg_control_names) > 0) rhs <- c(rhs, reg_control_names)
    
    formula_simple <- as.formula(paste(outcome_name, "~ treated"))
    formula_final <- as.formula(paste(outcome_name, "~", paste(rhs, collapse = " + ")))
    
    mod_simple <- lm(formula_simple, data = df_ebal, weights = w_ebal)
    mod_final <- lm(formula_final, data = df_ebal, weights = w_ebal)
    
    ate <- unname(coef(mod_final)["treated"])
    n_iris_paris <- sum(df_ebal$treated == 1, na.rm = TRUE)
    effet_total_paris <- ate * n_iris_paris
    total_paris_reference <- sum(df_ebal[[post_name_1]][df_ebal$treated == 1], na.rm = TRUE)
    effet_total_pct_paris <- if (total_paris_reference > 0) {
      (effet_total_paris / total_paris_reference) * 100
    } else {
      NA_real_
    }
    
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
    
    weights_controls <- df_ebal %>%
      filter(treated == 0) %>%
      select(IRIS, code_com, w_ebal) %>%
      distinct()
    
    df_full_trend <- df_reduced %>%
      filter(annee >= min(available_years()), annee <= max(available_years())) %>%
      left_join(weights_controls, by = c("IRIS", "code_com")) %>%
      mutate(
        w_plot = ifelse(treated == 1, 1, w_ebal),
        groupe = ifelse(treated == 1, "Paris", "Contrôle pondéré")
      ) %>%
      filter(treated == 1 | !is.na(w_plot))
    
    trend_full <- df_full_trend %>%
      group_by(groupe, annee) %>%
      summarise(
        mean_outcome = safe_weighted_mean(.data[[input$main_var]], w_plot),
        .groups = "drop"
      ) %>%
      arrange(annee, groupe)
    
    trend_full_index <- trend_full %>%
      group_by(groupe) %>%
      mutate(
        base_year = min(annee),
        base_value = mean_outcome[annee == base_year][1],
        evol_depuis_base = mean_outcome - base_value
      ) %>%
      ungroup()
    
    trend_micro <- df_micro %>%
      group_by(groupe, annee) %>%
      summarise(
        mean_outcome = safe_weighted_mean(.data[[input$main_var]], w_ebal),
        .groups = "drop"
      ) %>%
      arrange(annee, groupe)
    
    trend_micro_index <- trend_micro %>%
      group_by(groupe) %>%
      mutate(
        base_year = min(annee),
        base_value = mean_outcome[annee == base_year][1],
        evol_depuis_base = mean_outcome - base_value
      ) %>%
      ungroup()
    
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
    
    compare_vars <- unique(c(diff_name, outcome_name, extra_pretrend_name, extra_balance_names, reg_treatment_diff_name, reg_control_names))
    selected_vars_comparison <- bind_rows(lapply(compare_vars, function(v) {
      data.frame(
        variable = v,
        Paris = mean(df_ebal[[v]][df_ebal$treated == 1], na.rm = TRUE),
        Controle_non_pondere = mean(df_ebal[[v]][df_ebal$treated == 0], na.rm = TRUE),
        Controle_pondere = safe_weighted_mean(df_ebal[[v]][df_ebal$treated == 0], df_ebal$w_ebal[df_ebal$treated == 0])
      )
    }))
    
    list(
      df_ebal = df_ebal,
      n_iris_initial = n_iris_initial,
      n_iris_initial_paris = n_iris_initial_paris,
      n_iris_initial_control = n_iris_initial_control,
      n_iris_zero_removed = n_iris_zero_removed,
      n_iris_zero_removed_paris = n_iris_zero_removed_paris,
      n_iris_zero_removed_control = n_iris_zero_removed_control,
      n_iris_after_zero_filter = n_iris_after_zero_filter,
      n_iris_after_zero_filter_paris = n_iris_after_zero_filter_paris,
      n_iris_after_zero_filter_control = n_iris_after_zero_filter_control,
      balance_table = balance_table,
      ate = ate,
      n_iris_paris = n_iris_paris,
      effet_total_paris = effet_total_paris,
      total_paris_reference = total_paris_reference,
      effet_total_pct_paris = effet_total_pct_paris,
      mod_simple = mod_simple,
      mod_final = mod_final,
      trend_plot = trend_plot,
      trend_plot_index = trend_plot_index,
      trend_full = trend_full,
      trend_full_index = trend_full_index,
      trend_micro = trend_micro,
      weights_by_commune = weights_by_commune,
      top_communes = top_communes,
      selected_vars_comparison = selected_vars_comparison,
      trend_micro_index = trend_micro_index,
      extra_pretrend_name = extra_pretrend_name,
      diff_name = diff_name,
      outcome_name = outcome_name,
      pre_y1 = pre_y1,
      pre_y2 = pre_y2,
      post_y1 = post_y1,
      post_y2 = post_y2,
      main_var = input$main_var,
      extra_balance_names = extra_balance_names,
      reg_control_names = reg_control_names,
      reg_treatment_diff_name = reg_treatment_diff_name
    )
  })
  
  output$sample_info <- renderPrint({
    res <- analysis()
    cat("Variable principale :", res$main_var, "\n")
    cat("Différence de calage pré-traitement :", res$diff_name, "\n")
    cat("Différence de traitement / outcome :", res$outcome_name, "\n")
    cat("Période de calage :", res$pre_y1, "->", res$pre_y2, "\n")
    cat("Période de traitement :", res$post_y1, "->", res$post_y2, "\n\n")
    cat("Nombre d'IRIS avant éventuelle exclusion 0 logement :", res$n_iris_initial, "
")
    cat("  - Paris :", res$n_iris_initial_paris, "
")
    cat("  - Contrôle :", res$n_iris_initial_control, "
")
    cat("Nombre d'IRIS retirés pour 0 logement :", res$n_iris_zero_removed, "
")
    cat("  - Paris :", res$n_iris_zero_removed_paris, "
")
    cat("  - Contrôle :", res$n_iris_zero_removed_control, "
")
    cat("Nombre d'IRIS après ce filtre :", res$n_iris_after_zero_filter, "
")
    cat("  - Paris :", res$n_iris_after_zero_filter_paris, "
")
    cat("  - Contrôle :", res$n_iris_after_zero_filter_control, "
")
    cat("Nombre d'observations dans la base d'analyse :", nrow(res$df_ebal), "
")
    cat("Nombre d'IRIS Paris :", sum(res$df_ebal$treated == 1), "\n")
    cat("Nombre d'IRIS contrôle :", sum(res$df_ebal$treated == 0), "\n\n")
    cat("Variables additionnelles de calage :
")
    print(if (length(c(res$extra_pretrend_name, res$extra_balance_names)) == 0) "Aucune" else c(res$extra_pretrend_name, res$extra_balance_names))
    cat("
Variables supplémentaires dans la régression :
")
    reg_vars <- c(res$reg_treatment_diff_name, res$reg_control_names)
    print(if (length(reg_vars) == 0) "Aucune" else reg_vars)
  })
  
  output$balance_table <- renderDT({
    df <- analysis()$balance_table %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
    datatable(df, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$selected_vars_comparison <- renderDT({
    df <- analysis()$selected_vars_comparison %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
    datatable(df, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$ate_text <- renderPrint({
    res <- analysis()
    cat("ATE estimé (régression finale) sur", res$outcome_name, ":", round(res$ate, 4), "
")
    cat("Interprétation : effet moyen conditionnel estimé par IRIS parisien.
")
  })
  
  output$effet_paris_text <- renderPrint({
    res <- analysis()
    cat("Nombre d'IRIS parisiens :", res$n_iris_paris, "\n")
    cat("Effet total estimé sur l'ensemble de Paris :", round(res$effet_total_paris, 2), "\n")
    cat("Niveau de référence à Paris (année", res$post_y1, ") :", round(res$total_paris_reference, 2), "\n")
    cat("Effet total estimé en pourcentage du niveau parisien de référence :", round(res$effet_total_pct_paris, 4), "%
")
    cat("Ces calculs agrégés sont basés sur le coefficient treated de la régression finale.
")
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
  
  output$trend_full_plot <- renderPlot({
    res <- analysis()
    ggplot(res$trend_full, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      geom_vline(xintercept = c(res$pre_y1, res$pre_y2, res$post_y1, res$post_y2), linetype = c("dotted", "dashed", "dotted", "dashed")) +
      scale_x_continuous(breaks = sort(unique(res$trend_full$annee))) +
      labs(
        title = "Tendance complète — toutes les années disponibles",
        subtitle = paste0("Variable : ", res$main_var),
        x = "Année",
        y = paste0("Moyenne pondérée de ", res$main_var),
        color = "Groupe"
      ) +
      theme_minimal()
  })
  
  output$trend_full_plot_index <- renderPlot({
    res <- analysis()
    ggplot(res$trend_full_index, aes(x = annee, y = evol_depuis_base, color = groupe, group = groupe)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      geom_vline(xintercept = c(res$pre_y1, res$pre_y2, res$post_y1, res$post_y2), linetype = c("dotted", "dashed", "dotted", "dashed")) +
      scale_x_continuous(breaks = sort(unique(res$trend_full_index$annee))) +
      labs(
        title = "Tendance complète indexée — toutes les années disponibles",
        subtitle = paste0("Variable : ", res$main_var),
        x = "Année",
        y = "Variation par rapport à l'année de base",
        color = "Groupe"
      ) +
      theme_minimal()
  })
  
  output$trend_micro_plot <- renderPlot({
    res <- analysis()
    ggplot(res$trend_micro, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      scale_x_continuous(breaks = sort(unique(res$trend_micro$annee))) +
      labs(
        title = "Décomposition année par année — niveaux",
        subtitle = paste0("Entre ", res$pre_y1, " et ", res$pre_y2),
        x = "Année",
        y = paste0("Moyenne pondérée de ", res$main_var),
        color = "Groupe"
      ) +
      theme_minimal()
  })
  
  output$trend_micro_plot_index <- renderPlot({
    res <- analysis()
    ggplot(res$trend_micro_index, aes(x = annee, y = evol_depuis_base, color = groupe, group = groupe)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      scale_x_continuous(breaks = sort(unique(res$trend_micro_index$annee))) +
      labs(
        title = "Décomposition année par année — base 0",
        subtitle = paste0("Entre ", res$pre_y1, " et ", res$pre_y2),
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
  
  output$download_trend_png <- downloadHandler(
    filename = function() paste0("graphique_tendances_", Sys.Date(), ".png"),
    content = function(file) {
      res <- analysis()
      p <- ggplot(res$trend_plot, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        geom_vline(xintercept = c(res$pre_y2, res$post_y1), linetype = c("dashed", "dotted")) +
        scale_x_continuous(breaks = sort(unique(res$trend_plot$annee))) +
        labs(
          title = paste0("Évolution de ", res$main_var, " : Paris vs contrôle pondéré"),
          subtitle = paste0("Calage sur ", res$diff_name),
          x = "Année",
          y = paste0("Moyenne pondérée de ", res$main_var),
          color = "Groupe"
        ) +
        theme_minimal()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_weights_png <- downloadHandler(
    filename = function() paste0("barplot_communes_", Sys.Date(), ".png"),
    content = function(file) {
      res <- analysis()
      p <- ggplot(res$top_communes, aes(x = reorder(nom_affiche, poids_total), y = poids_total)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
          title = paste0("Top ", nrow(res$top_communes), " des communes contribuant au contrefactuel de Paris"),
          x = "Commune",
          y = "Poids total"
        ) +
        theme_minimal()
      ggsave(file, plot = p, width = 10, height = 12, dpi = 300)
    }
  )
}

shinyApp(ui = ui, server = server)
