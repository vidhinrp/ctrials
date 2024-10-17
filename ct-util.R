# Load Data
##1. Go to the [clinical trials download site](https://aact.ctti-clinicaltrials.org/pipe_files)
##2. Download the "Current Month's Daily Static Copies" to a directory you will access from R.
##3. unzip the file and save it as ctrialsdata

#library(ctrialsgov) # devtools::install_github("presagia-analytics/ctrialsgov")
#con = ctgov_create_duckdb(
#    basedir = file.path("ctdata"),
#    dbdir = "ctrialsgov.duckdb"
#    )

# Load required packages
# Install bbplot from GitHub if not already installed
#devtools::install_github('bbc/bbplot')

library("bbplot")
library("dplyr")
library("DBI")
library("DT")
library("ggplot2")
library("tidyverse")
library("plotly")
library("duckdb")

# Create the connection to a database and "studies" and "sponsors" tables.
if (!exists("con")) {
  con = dbConnect(
    duckdb(
      file.path("ctrialsgov.duckdb"),
      read_only = TRUE
    )
  )

  studies = tbl(con, "studies")
  sponsors = tbl(con, "sponsors")
  conditions = tbl(con, "conditions")
  countries = tbl(con, "countries")
  inter = tbl(con, "interventions")
}

# Query keywords from a database table.
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |> gsub("'", "''", x = _)
  like <- if (ignore_case) " ilike " else " like "
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}

# Dropdown filter for sponsor type
dropdownspons <- function(d, sponsortyp) {
  if(sponsortyp != "Both")
    dplyr::filter(d, lead_or_collaborator == sponsortyp)
  else d
}

# Dropdown filter for study type
dropdownstudy <- function(d, studytyp) {
  if(studytyp != "All")
    dplyr::filter(d, study_type == studytyp)
  else d
}

# Create a histogram of the phases returned by a brief title keyword search.
plot_phase_histogram = function(d) {
  phase_summary <- d |>
    mutate(phase = ifelse(is.na(phase), "NA", phase)) |>
    group_by(phase) |>
    summarize(n = n(), .groups = "drop") |>
    mutate(n = coalesce(n, 0), phase = coalesce(phase, "Not Applicable")) |>
    filter(!(phase == "Not Applicable" & n == 0))

  ggplot(phase_summary, aes(x = phase, y = n)) +
    geom_col(fill = "#000000") +  # Customize color
    labs(x = "Phase", y = "Count") +
    bbc_style() +
    theme(
      text = element_text(size = 10), 
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 10))
}

# Create a histogram of concurrent trials for each date in a set of studies.
plot_concurrent_trials = function(d) {
  all_dates = d |> 
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |> 
    arrange(value) |>
    na.omit() |> 
    rename(date = value)

  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  all_dates$count = 
    map_dbl(
      all_dates$date, 
      ~ .x |> 
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )

  ggplot(all_dates, aes(x = date, y = count)) +
    geom_line(color = "#000000") +  
    labs(x = "Date", y = "Count") +
    bbc_style() +
    theme(text = element_text(size = 10), 
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 10))
}

# Plot histogram of top 25 conditions examined in a set of studies.
plot_conditions <- function(d) {
  conditions_pres = d |> merge(conditions, by = "nct_id", all.x=TRUE) |>
    select(downcase_name, id) |>
    group_by(downcase_name, id) |>
    mutate(downcase_name = str_remove(downcase_name, c("disease?"))) |>
    collect()

  x1 <- as.vector(conditions_pres[["downcase_name"]])
  same <- sapply(seq(length(x1)-1), 
                 function(i) any(agrep(x1[i+1], x1[1], max.distance = .7)))
  ex <- embed(x1, 2)
  conditions_pres[["name_cond"]] = c(x1[1], ifelse(same, ex[, 2], ex[, 1]))


  topcond = conditions_pres |> select(name_cond) |>
    group_by(name_cond) |>
    summarise(n=n()) |>
    top_n(n = 25, wt = n)

  ggplot(topcond, aes(x = name_cond, y = n)) +
    geom_col(fill = "#000000") + 
    labs(x = "Condition", y = "Count", title = "Top 25 Conditions") +
    bbc_style() +
    theme(text = element_text(size = 10), 
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = 70, hjust = 1),
      plot.title = element_text(size = 10))
}

# Create scatter plot of studies published by country name.
plot_countries = function(d) {
  d = d|> merge(countries, by="nct_id", all.x=TRUE) |>
          select(name) |>
          group_by(name) |>
          summarise(n = n()) |> 
          arrange(desc(n))

  x = ggplot(d,
             aes(n, 
                 y = 0, 
                 group = name, 
                 text = paste("Country:", name, "<br>Count:", n))) +  # Combine text in a single entry
      geom_point(aes(size = n), 
                 alpha = 0.6, 
                 color = "#000000", 
                 fill="#000000",
                 shape = 21) +
      labs(x = "Number of studies (use mouse to identify country)") +
      scale_x_continuous(
        name = "Number of studies (use mouse to identify country)", 
        trans = "log10", 
        breaks = c(1, 10, 100, 1000, 10000)) +
      theme(
        panel.grid.major = element_line(color = "#000000", linewidth = 0.2),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(lineheight  = 6, size = 12, family = "Helvetica"),
        axis.title.x = element_text(lineheight  = 10, margin = margin(t = 10), size = 12, family = "Helvetica"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(lineheight = 10, hjust = 0, size = 12, family = "Helvetica"),
        panel.border = element_rect(colour = "#000000", fill = NA, linewidth = 1),
        legend.position = "none",
      )

    ggplotly(x, tooltip = "text") %>%  # Use just "text" for the tooltip
    config(displayModeBar = F)
}

# Plot barplot of types of interventions done in trials
plot_intervention = function(d) {
  d = d |> merge(inter, by="nct_id", all.x=TRUE) |>
    mutate(intervention_type = replace_na(intervention_type, "Other")) |>
    select(intervention_type) |>
    group_by(intervention_type) |>
    summarise(n=n()) |>
    mutate(intervention_type = case_when(
      intervention_type %in% c("other", "Other", "OTHER") ~ "OTHER",
      TRUE ~ intervention_type))

  ggplot(d, aes(x = intervention_type, y = n)) +
    geom_col(fill = "#000000") +  
    labs(x = "Intervention Type", y = "Count") +
    theme(axis.text.x = element_text(angle = 15, hjust = 1, size=10), 
          text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 10)) +
    bbc_style()
}
