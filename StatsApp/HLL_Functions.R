
# Function to convert JSON string to nested dataframe
json_to_nested_df <- function(json_string) {
  if (is.na(json_string) || json_string == "" || is.null(json_string) || json_string == "{}") {
    return(tibble(Weapon = character(), Count = integer())) # Return empty df for NA/empty JSON
  }
  parsed_json <- fromJSON(json_string)
  if (length(parsed_json) == 0) {
    return(tibble(Weapon = character(), Count = integer())) # Return empty df if parsed json is empty list
  }
  nested_df <- tibble(
    Weapon = names(parsed_json),
    Count = as.integer(unname(parsed_json)) # Ensure Count is integer
  )
  return(nested_df)
}

extract_roster_names <- function(roster_csv, rows, col) {
  roster_csv %>%
    slice(rows) %>%
    pull(col) %>%
    na.omit() %>%
    as.list()
}

calculate_kd <- function(x, y) {
  round(x / y, 2)
}

DivTable <- function(data, assignment){
  data  %>%
    filter(Assignment %in% assignment) %>%
    gt() %>%
    cols_hide(c(Assignment)) %>%
    cols_align(
      align = "left",
      columns = Name
    ) %>%
    cols_align(
      align = "center",
      columns = -Name
    ) %>%
    fmt_number(
      columns = c(`K/D`, `Adj K/D`,`KPM`),
      decimals = 2
    ) %>%
    fmt_integer(
      columns = c(Kills, Deaths, CE, SE)
    ) %>%
    fmt_percent(
      columns = `% Shells`, decimals = 0
    ) %>%
    gt_add_divider(columns = "Role", style = "dashed", color = "white") %>%
    #gt_merge_stack(col1 = "Name", col2= "Clan", palette = c("white", "white")) %>%
    data_color(columns = Kills, palette = c("forestgreen","forestgreen")) %>%
    data_color(columns = Deaths, palette = c("firebrick3","firebrick3")) %>%
    data_color(columns = `Adj K/D`, palette = c("red2","green2")) %>%
    data_color(columns = `% Shells`, palette = c("lightgreen","lightcoral")) %>%
    data_color(columns = `SL`, palette = c("lightcoral","lightgreen")) %>%
    cols_width(
      Name ~ px(210),
      Role ~ px(90),
      SL ~ px(30),
      Kills ~ px(40),
      Deaths ~ px(50),
      CE ~ px(40),
      SE ~ px(40),
      `K/D` ~ px(60),
      `Adj K/D` ~ px(60),
      `KPM` ~ px(60),
      `% Shells` ~ px(60)
    ) %>%
    tab_options(
      table.font.size = 12
    ) %>%
    opt_css(
      css = "
            body { /* Or you can target .gt_table for more specific styling */
              background-color: #2d2d2d;
              color: white; /* Default text color to white */
            }
            .gt_table {
              background-color: #2d2d2d; /* Ensure table background is gray20 */
              color: white; /* Ensure table text is white */
              border: none; /* Optional: Remove default table border if you want */
            }
            .gt_table th { /* Style table header cells */
              color: white; /* Ensure header text is white */
              background-color: #2d2d2d; /* Ensure header background is gray20 */
              border-bottom: 1px solid white; /* Optional: White line under headers */
            }
            .gt_table td { /* Style table data cells */
              color: white; /* Ensure data text is white */
              background-color: #2d2d2d; /* Ensure data background is gray20 */
              border: none; /* Optional: Remove cell borders if you want */
            }
            .gt_table tfoot { /* Style table footer if you have one */
              color: white;
              background-color: #2d2d2d;
            }
            .gt_table caption { /* Style table caption if you have one */
              color: white;
              background-color: #2d2d2d;
            }
        "
    )
}
