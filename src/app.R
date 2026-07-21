library(shiny)
library(bslib) # themes
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT) # Data table
library(stringr) # regex
library(readr)
library(bsicons) # icons

# Target vars , manually selected a few vars out of 200+ variables
# key words like water, net, ITN, test, results, age ,sex
target_vars <- c(
  # Demographic
  "hv024", "shcounty", "shzone", "hv025", "hv270", "hv105", "hv104", "hv005",
  # ML Features
  "hv213", "hv214", "hv215", "hv206", "hv201", "hv205", "hv207",
  # Malaria Status
  "hml32", "hml35", "hml32a", "hml32b", "hml32c", "hml32d",
  # Prevention
  "hv227", "hml10", "hml4", "hml12",
  # Health
  "sb119", "sb115d"
)

# Files
dta_file <- "KEPR81FL.DTA"
var_file <- "MIS_2020_PR.txt"

# Parse Metadata
parse_variable_labels <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  pattern <- 'label\\s+variable\\s+(\\w+)\\s+"([^"]+)"'
  matches <- str_match(lines, pattern)

  metadata <- tibble::tibble(
    code = tolower(matches[, 2]),
    description = matches[, 3]
  )
  return(metadata)
}
var_desc_map <- parse_variable_labels(var_file)

# Load & Filter Data

raw_df <- read_dta(dta_file)

# Filter to target variables
scoped_df <- raw_df %>% select(all_of(target_vars))

# Decoding Haven labelled data (stata/.dta file)
# convert all the raw_data (haven labelled columns) to factor
apply_metadata <- function(data) {
  decoded <- data
  for (col in names(decoded)) {
    # dealing with haven labelled data
    decoded[[col]] <- as_factor(decoded[[col]])
  }
  return(decoded)
}

# Apply decoding
decoded_df <- apply_metadata(scoped_df)

# Pre-processing
final_df <- decoded_df %>%
  filter(!is.na(hml32) & hml32 != "sample not found in lab database") %>%
  mutate(
    Weight = as.numeric(hv005) / 1000000,

    # Malaria Positive Check
    # "Positive" in main result or "Yes" in any species indicator (hml32a-d)
    # Uses if_any with any_of to safely handle missing columns
    Malaria_Pos = case_when(
      if_any(
        any_of(c("hml32", "hml32a", "hml32b", "hml32c", "hml32d")),
        ~ str_detect(as.character(.), regex("Positive|Yes|1", ignore_case = TRUE))
      ) ~ 1,
      TRUE ~ 0
    ),
    # Slept_ITN: Person slept under an ITN last night
    # hml12 labels:
    # 0: Did not sleep under a net
    # 1: Only treated (ITN) nets
    # 2: Both treated (ITN) and untreated nets
    # 3: Only untreated nets
    Slept_ITN = case_when(
      str_detect(as.character(hml12), regex("treated \\(ITN\\)", ignore_case = TRUE)) ~ 1,
      str_detect(as.character(hml12), regex("Did not sleep under a net|Only untreated nets", ignore_case = TRUE)) ~ 0,
      TRUE ~ NA_real_
    ),
    # if a househols owns a net
    Has_Net = case_when(
      str_detect(as.character(hv227), regex("Yes", ignore_case = TRUE)) ~ 1,
      str_detect(as.character(hv227), regex("No", ignore_case = TRUE)) ~ 0,
      TRUE ~ NA_real_
    )
  )


# filtering by zone and county
unique_zones <- sort(unique(as.character(final_df$shzone)))
unique_counties <- sort(unique(as.character(final_df$shcounty)))
# Changing to title case
filter_zones <- tools::toTitleCase(unique_zones)
filter_counties <- tools::toTitleCase(unique_counties)

# meta table with all the variables used
meta_table <- var_desc_map %>%
  filter(var_desc_map$code %in% c(names(final_df)))

# logistic reg (for house hold amenities)
ml_features <- c("hv213", "hv214", "hv215", "hv206", "hv201", "hv205", "hv207")
# Only keep features that actually exist in the data
# some features in the text file mentiond do not exist in the df
ml_features <- intersect(ml_features, names(final_df))

ml_df <- final_df %>%
  select(Malaria_Pos, all_of(ml_features)) %>%
  drop_na() %>%
  apply_metadata() # using the function creted above to convert haven-labelled to factor

# instead of using col names with codes, using its label as col name attr(col , 'label')
var_labels <- sapply(ml_df, function(x) attr(x, "label"))
# output
# $Malaria_Pos
# NULL

var_labels$Malaria_Pos <- "Malaria_Pos" # setting empty target value of list


names(ml_df) <- var_labels # replacing codes with actual names/desc of codes
# column hv205 to main floor material ...

# Train Logistic Regression
ml_model <- glm(Malaria_Pos ~ ., data = ml_df, family = "binomial")

anova_res <- anova(ml_model, test = "Chisq")

# Extract deviance for each feature (excluding the first NULL row)
feature_df <- data.frame(
  Option = rownames(anova_res)[-1],
  Estimate = anova_res$Deviance[-1],
  stringsAsFactors = FALSE
)

feature_importance <- tibble(feature_df) %>%
  filter(!is.na(Estimate)) %>%
  arrange(desc(Estimate)) %>%
  mutate(Option = reorder(Option, Estimate))


# 3. UI

ui <- page_navbar(
  title = "Malaria Insights Hub",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50"),

  # Sidebar
  sidebar = sidebar(
    title = "Analysis Filters",
    selectInput("filter_zone", "Endemicity Zone", choices = c("All", unique_zones), selected = "All"),
    selectInput("filter_county", "County", choices = c("All", unique_counties), selected = "All"),
    hr(),
    p("Data source: MIS 2020"),
    p("Filters apply to all tabs.")
  ),

  # Tab 1: Overview
  nav_panel(
    "Overview & Hotspots",
    layout_columns(
      value_box(title = "Prevalence (Microscopy)", value = textOutput("kpi_prev"), showcase = bs_icon("virus"), theme = "danger"),
      value_box(title = "ITN Usage", value = textOutput("kpi_itn"), showcase = bs_icon("shield-check"), theme = "success")
    ),
    card(
      card_header("Prevalence Heatmap"),
      plotlyOutput("heatmap", height = "500px")
    )
  ),

  # Tab 2: Prevention
  nav_panel(
    "Prevention Analysis",
    layout_columns(
      card(card_header("Does ITN Usage reduce Malaria?"), plotlyOutput("itn_effect_plot")),
      card(card_header("Bed Net Ownership vs. ITN"), plotlyOutput("net_gap_plot"))
    )
  ),

  # Tab 3: Demographics
  nav_panel(
    "Social Determinants",
    layout_columns(
      card(card_header("Malaria by Wealth Quintile"), plotlyOutput("wealth_plot")),
      card(card_header("Urban vs Rural Impact"), plotlyOutput("urban_rural_plot"))
    )
  ),

  # Tab 4: Diagnosis
  nav_panel(
    "Diagnosis",
    layout_columns(
      card(card_header("Malaria Species Breakdown"), plotlyOutput("species_donut")),
      card(card_header("RDT vs Microscopy Agreement"), plotlyOutput("test_agreement_plot"))
    )
  ),

  # Tab 5: ML Predictor
  nav_panel(
    "ML Features",
    layout_columns(
      card(
        card_header("Model Insights (Logistic Regression)"),
        p("Overall importance of features influencing Malaria risk (based on Deviance Explained). Higher values indicate a stronger overall influence of the factor on predicting Malaria outcomes."),
        plotlyOutput("ml_importance_plot")
      )
    )
  ),

  # Tab 6: Scope
  nav_panel(
    "Data Scope",
    card(
      card_header("Variable Meta-Data", DTOutput("meta_table")),
      br()
    )
  )
)

# 4. Server
server <- function(input, output, session) {
  # Reactive Filter
  filtered_data <- reactive({
    df <- final_df

    if (input$filter_zone != "All") {
      df <- df %>% filter(shzone == input$filter_zone)
    }

    if (input$filter_county != "All") {
      df <- df %>% filter(shcounty == input$filter_county)
    }
    df
  })

  # KPIs
  output$kpi_prev <- renderText({
    df <- filtered_data()
 
    rate <- weighted.mean(df$Malaria_Pos, df$Weight, na.rm = T) * 100
    paste0(round(rate, 1), "%")
  })

  output$kpi_itn <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return("N/A")
    }
    rate <- weighted.mean(df$Slept_ITN, df$Weight, na.rm = T) * 100
    paste0(round(rate, 1), "%")
  })

  # Heatmap
  output$heatmap <- renderPlotly({
    # Use full data for heatmap context if filters are set.
    # If filtered to 1 county, heatmap is 1 cell.
    df <- filtered_data()

    hm_data <- df %>%
      group_by(County = shcounty, Zone = shzone) %>%
      summarise(Prev = weighted.mean(Malaria_Pos, Weight, na.rm = T) * 100, .groups = "drop")

    p <- ggplot(hm_data, aes(x = Zone, y = County, fill = Prev)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", direction = -1) +
      labs(fill = "Prev %", x = "Zone", y = "County") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # ITN Effect Bar CHart
  output$itn_effect_plot <- renderPlotly({
    df <- filtered_data()

    eff_data <- df %>%
      group_by(Slept_ITN, Has_Net) %>%
      summarise(Positivity = weighted.mean(Malaria_Pos, Weight, na.rm = T) * 100, .groups = "drop") %>%
      mutate(Status = ifelse(Slept_ITN == 1, "Used ITN", "Didnt use ITN"))

    p <- ggplot(eff_data, aes(x = factor(Has_Net), y = Positivity, fill = Status)) +
      geom_col(position = "dodge") +
      facet_wrap(~Slept_ITN, labeller = labeller(Slept_ITN = c(`0` = "Did Not Sleep Under ITN", `1` = "Slept Under ITN"))) +
      labs(
        title = "Malaria Positivity by ITN Ownership and Usage",
        x = "Has Net (0 = No, 1 = Yes)",
        y = "Positivity (%)"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # Net Gap Pie Chart
  output$net_gap_plot <- renderPlotly({
    df <- filtered_data()
    # Has_Net = 1 means household has mosquito net

    # Only include net owners with known usage
    gap_data <- df %>%
      filter(Has_Net == 1, !is.na(Slept_ITN)) %>%
      group_by(Slept_ITN) %>%
      summarise(weighted_n = sum(Weight, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Usage = ifelse(Slept_ITN == 1, "Used ITN", "Did Not Use ITN")
      )

    plot_ly(
      gap_data,
      labels = ~Usage,
      values = ~weighted_n, # use weighted totals
      type = "pie",
      hole = 0.4,
      textinfo = "label+percent"
    ) %>%
      layout(title = "ITN Usage among Net Owners", showlegend = TRUE)
  })

  # Wealth Chart
  output$wealth_plot <- renderPlotly({
    df <- filtered_data()

    summ <- df %>%
      group_by(Wealth = hv270) %>%
      summarise(Prev = weighted.mean(Malaria_Pos, Weight, na.rm = T) * 100)

    ggplot(summ, aes(x = Wealth, y = Prev, fill = Wealth)) +
      geom_col() +
      labs(title = "Malaria by Wealth Quintile", y = "Prevalence (%)", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Urban/Rural Chart
  output$urban_rural_plot <- renderPlotly({
    df <- filtered_data()

    summ <- df %>%
      group_by(Residence = hv025) %>%
      summarise(Prev = weighted.mean(Malaria_Pos, Weight, na.rm = T) * 100)

    ggplot(summ, aes(x = Residence, y = Prev, fill = Residence)) +
      geom_col(width = 0.6) +
      labs(title = "Urban vs Rural Prevalence", y = "Prevalence (%)", x = NULL) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1")
  })

  # Species Donut
  output$species_donut <- renderPlotly({
    df <- filtered_data()
    species_vars <- c("hml32a", "hml32b", "hml32c", "hml32d")

    # Pivot logic
    long_spec <- df %>%
      select(Weight, all_of(species_vars)) %>%
      mutate(across(all_of(species_vars), as.character)) %>% # Ensure char for pivot
      pivot_longer(cols = all_of(species_vars), names_to = "Species", values_to = "Result") %>%
      # Broader check for "Yes", "1", "Positive"
      filter(str_detect(Result, regex("Yes|1|Positive", ignore_case = TRUE)))

    summary_spec <- long_spec %>%
      group_by(Species) %>%
      summarise(Count = sum(Weight, na.rm = T)) %>%
      mutate(Label = case_when(
        str_detect(Species, "hml32a") ~ "P. falciparum",
        str_detect(Species, "hml32b") ~ "P. malariae",
        str_detect(Species, "hml32c") ~ "P. ovale",
        str_detect(Species, "hml32d") ~ "P. vivax",
        TRUE ~ Species
      ))

    plot_ly(summary_spec,
      labels = ~Label, values = ~Count, type = "pie", hole = 0.6,
      textinfo = "label+percent"
    )
  })

  # Agreement
  output$test_agreement_plot <- renderPlotly({
    df <- filtered_data()

    agree_data <- df %>%
      count(Microscopy = as.character(hml32), RDT = as.character(hml35)) %>%
      filter(!is.na(Microscopy), !is.na(RDT))

    ggplot(agree_data, aes(x = Microscopy, y = RDT, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c() +
      theme_minimal()
  })
  # Meta Table
  output$meta_table <- renderDT(
    {
      meta_table
    },
    options = list(pageLength = 10, scrollX = TRUE)
  )


  output$ml_importance_plot <- renderPlotly({

    p <- ggplot(feature_importance, aes(x = Option, y = Estimate)) +
      geom_col(fill = "#3498DB") +
      coord_flip() +
      labs(x = "Feature", y = "Importance (Deviance Explained)") +
      theme_minimal()

    ggplotly(p, tooltip = c("x", "y"))
  })


  # Dynamic County Update based on Zone
  observeEvent(input$filter_zone, {
    if (input$filter_zone != "All") { #if not 'all' then update filter ...
      valid <- final_df %>%
        filter(shzone == input$filter_zone) %>%
        pull(shcounty) %>%
        unique() %>%
        as.character() %>%
        sort()
      updateSelectInput(session, "filter_county", choices = c("All", valid))
    } else {
      updateSelectInput(session, "filter_county", choices = c("All", unique_counties))
    }
  })
}

shinyApp(ui, server)
