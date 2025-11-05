# -------------------------------------------------------------------
# Table 1: Absolute number and year-to-year percentage variation by occupation
# -------------------------------------------------------------------

# Create the base table: add totals, rename variables, order, and filter
tbl1 <- df1 %>%
        adorn_totals("col") %>%  # add a "Total" column summing across all years
        rename("Classificação Brasileira de Ocupações" = cbo) %>%  # rename column for clarity (CBO = Brazilian Occupation Classification)
        arrange(desc(Total)) %>%  # order rows by descending total
        filter(Total != 0) %>%  # remove rows with total equal to zero
        adorn_totals("row")  # add a "Total" row at the bottom

# Ensure that yearly columns are numeric for calculations
tbl1_numeric <- tbl1 %>%
        mutate(across(c(`2015`, `2016`, `2017`, `2018`, 
                        `2019`, `2020`, `2021`, `2022`, `2023`, `2024`), 
                      ~ as.numeric(.)))

# -------------------------------------------------------------------
# Custom function to show absolute number and percentage variation below it
# -------------------------------------------------------------------
add_variation_below <- function(current, previous) {
        current <- as.numeric(current)
        previous <- as.numeric(previous)
        
        # No variation for the first year
        if (is.na(previous)) {
                return(as.character(current))
        }
        
        # Avoid division by zero; only show the current value
        if (previous == 0) {
                return(paste0(
                        "<div style='font-size:11px;'>",
                        formatC(current, big.mark = ".", decimal.mark = ",", format = "f", digits = 0),
                        "</div>"
                ))
        }
        
        # Calculate year-to-year percentage variation
        variation <- round(((current - previous) / previous) * 100, 0)
        variation_text <- paste0("(", formatC(variation, format = "f", digits = 0), ")")
        
        # Return formatted HTML with value (top) and variation (bottom, green/red color)
        return(paste0(
                "<div style='font-size:11px;'>", 
                formatC(current, big.mark = ".", decimal.mark = ",", format = "f", digits = 0), 
                "</div>",
                "<div style='font-size:10px; color:", 
                ifelse(variation < 0, "red", "green"), 
                ";'>", 
                variation_text, 
                "</div>"
        ))
}

# -------------------------------------------------------------------
# Apply variation calculation between consecutive years
# -------------------------------------------------------------------
var_2016 <- mapply(add_variation_below, tbl1_numeric$`2016`, tbl1_numeric$`2015`)
var_2017 <- mapply(add_variation_below, tbl1_numeric$`2017`, tbl1_numeric$`2016`)
var_2018 <- mapply(add_variation_below, tbl1_numeric$`2018`, tbl1_numeric$`2017`)
var_2019 <- mapply(add_variation_below, tbl1_numeric$`2019`, tbl1_numeric$`2018`)
var_2020 <- mapply(add_variation_below, tbl1_numeric$`2020`, tbl1_numeric$`2019`)
var_2021 <- mapply(add_variation_below, tbl1_numeric$`2021`, tbl1_numeric$`2020`)
var_2022 <- mapply(add_variation_below, tbl1_numeric$`2022`, tbl1_numeric$`2021`)
var_2023 <- mapply(add_variation_below, tbl1_numeric$`2023`, tbl1_numeric$`2022`)
var_2024 <- mapply(add_variation_below, tbl1_numeric$`2024`, tbl1_numeric$`2023`)

# -------------------------------------------------------------------
# Replace year columns with formatted (HTML) values including variation
# -------------------------------------------------------------------
tbl1_with_vars <- tbl1_numeric %>%
        mutate(
                `2016` = var_2016,
                `2017` = var_2017,
                `2018` = var_2018,
                `2019` = var_2019,
                `2020` = var_2020,
                `2021` = var_2021,
                `2022` = var_2022,
                `2023` = var_2023,
                `2024` = var_2024
        )

# -------------------------------------------------------------------
# Render the final table with the GT package
# -------------------------------------------------------------------
gt_table <- tbl1_with_vars %>%
        gt() %>%
        tab_header(
                title = "",     # optional: add main title
                subtitle = ""   # optional: add subtitle
        ) %>%
        # Enable HTML rendering (so the color and formatting appear correctly)
        fmt_markdown(
                columns = vars(`2016`, `2017`, `2018`, `2019`, 
                               `2020`, `2021`, `2022`, `2023`, `2024`)
        ) %>%
        # Set general text style for the table body
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "center"
                ),
                locations = cells_body(columns = everything())
        ) %>%
        # Style column labels
        tab_style(
                style = cell_text(font = "Times New Roman", size = px(11), weight = "normal"),
                locations = cells_column_labels(columns = everything())
        ) %>%
        # General table options
        tab_options(
                table.font.size = px(10),
                data_row.padding = px(5),
                container.width = pct(100),
                container.height = pct(100)
        ) %>%
        # Add a top spanner over year columns
        tab_spanner(
                label = "Absolute number and % change from previous year",
                columns = vars(`2015`,  `2016`, `2017`, `2018`, 
                               `2019`, `2020`, `2021`, `2022`, `2023`, `2024`)
        ) %>% 
        # Format numbers (2015 and Total columns only — other years are HTML)
        fmt_number(
                columns = vars(`2015`, Total),
                decimals = 0,
                use_seps = TRUE,
                sep_mark = ".",  # thousands separator
                dec_mark = ","
        )  %>% 
        # Bold column labels for year columns
        tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_column_labels(vars(`2015`, `2016`, `2017`, `2018`, 
                                                     `2019`, `2020`, `2021`, `2022`, 
                                                     `2023`, `2024`))
        ) %>% 
        # Align the first column (occupation) to the left
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "left"
                ),
                locations = cells_body(columns = vars(1))
        ) %>%
        # Align numeric columns to the right
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "right"
                ),
                locations = cells_body(columns = vars(`2015`, `2016`, `2017`, `2018`, 
                                                      `2019`, `2020`, `2021`, `2022`, 
                                                      `2023`, `2024`, Total))
        ) %>% 
        # Global visual settings (background colors and striping)
        tab_options(
                table.background.color = "#f9f9f9",
                column_labels.background.color = "#cccccc",
                row.striping.include_table_body = TRUE
        )

# -------------------------------------------------------------------
# Print and export the GT table as HTML
# -------------------------------------------------------------------
gt_table
gtsave(gt_table, "outputs/raw_tables/t2.html")
