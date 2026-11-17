library(dplyr)
library(tidyr)
library(gt)
library(scales)
library(glue)
library(here)

df <- arrow::open_dataset(here("temp/exp.parquet")) |> collect()

df <- df |> 
  mutate(total_PA = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport)

# Function to calculate quantiles for columns starting with "exp" with optional grouping
calc_quantiles_grouped <- function(data, group_var = NULL) {
  # data <- df
  quantile_probs <- c(0, 0.25, 0.5, 0.75, 1)
  quantile_names <- c("0%", "25%", "50%", "75%", "100%")
  
  if (!is.null(group_var)) {
    group_vars_sym <- if (length(group_var) > 1) {
      rlang::syms(group_var)
    } else {
      rlang::sym(group_var)
    }
    grouped_df <- data |> group_by(!!!group_vars_sym)
  } else {
    grouped_df <- data
  }
  
  
  summarised <- grouped_df |>
    summarise(
      across(
        matches("^(exposure|total_PA)"),
        list(
          `0%` = ~ quantile(.x, 0),
          `25%` = ~ quantile(.x, 0.25),
          `50%` = ~ quantile(.x, 0.5),
          `75%` = ~ quantile(.x, 0.75),
          `100%` = ~ quantile(.x, 1)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -all_of(group_var),
      names_to = c("variable", "quantile"),
      names_pattern = "^(.*)_(.*)$"
    ) |>
    mutate(quantile = factor(quantile, levels = quantile_names)) |>
    arrange(across(all_of(group_var)), variable, quantile)
  return(summarised)
}

# Example usage with your dataframe `df`
overall <- calc_quantiles_grouped(df, "scen")
by_gender <- calc_quantiles_grouped(df, c("scen", "gender"))
by_ladnm <- calc_quantiles_grouped(df, c("scen", "ladnm"))
by_agegroup <- calc_quantiles_grouped(df, c("scen", "agegroup"))
# Add grouping column to differentiate
overall <- overall |> mutate(grouping = "Overall")
by_gender <- by_gender |> mutate(grouping = paste("Gender:", gender)) |> select(-gender)
by_ladnm <- by_ladnm |> mutate(grouping = paste("LADNM:", ladnm)) |> select(-ladnm)
by_agegroup <- by_agegroup |> mutate(grouping = paste("Agegroup:", agegroup)) |> select(-agegroup)
# Bind all results into one dataframe
all_quantiles <- bind_rows(overall, by_gender, by_ladnm, by_agegroup) #|>
  #select(grouping, variable, quantile, value)
# Create a wide format table for gt display
gt_table <- all_quantiles |>
  pivot_wider(names_from = quantile, values_from = value) |>
  gt(groupname_col = "grouping") |>
  tab_header(title = "Quantiles for exp Columns by Groupings") |>
  cols_label(
    grouping = "Group",
    variable = "Variable",
    `0%` = "Min",
    `25%` = "25th Percentile",
    `50%` = "Median",
    `75%` = "75th Percentile",
    `100%` = "Max"
  )

# Define the color function
col_fun <- col_numeric(palette = c("lightpink", "lightgreen"), domain = c(0, 1))

# Pivot data so that each row represents (grouping, variable, quantile)
wide_df <- all_quantiles |>
  pivot_wider(
    id_cols = c(grouping, variable, quantile),
    names_from = scen,
    values_from = value
  )

# Dynamically detect the scenario columns
scen_cols <- setdiff(names(wide_df), c("grouping", "variable", "quantile"))

# Compute normalized columns
norm_df <- wide_df |>
  rowwise() |>
  mutate(
    row_min = min(c_across(all_of(scen_cols)), na.rm = TRUE),
    row_max = max(c_across(all_of(scen_cols)), na.rm = TRUE)
  ) |>
  mutate(across(
    all_of(scen_cols),
    function(x) if_else(row_max == row_min, 0.5, (x - row_min) / (row_max - row_min)),
    .names = "{.col}_norm"
  )) |>
  ungroup()

# Now safely construct HTML columns without using glue inside across()
for (scen in scen_cols) {
  norm_col <- paste0(scen, "_norm")
  html_col <- paste0(scen, "_html")
  norm_df[[html_col]] <- mapply(function(val, norm) {
    color <- col_fun(norm)
    sprintf("<div style='background-color:%s'>%s</div>", color, round(val, 2))
  }, norm_df[[scen]], norm_df[[norm_col]])
}

html_cols <- paste0(scen_cols, "_html")

gt_tbl <- norm_df |>
  select(grouping, variable, quantile, all_of(html_cols)) |>
  gt(groupname_col = "grouping") |>
  fmt_markdown(columns = all_of(html_cols)) |>
  tab_options(table.font.size = "small") |>
  opt_interactive(use_filters = TRUE)

gt_tbl
