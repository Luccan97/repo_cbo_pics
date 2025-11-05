# -------------------------------------------------------------------
# Table 3: Absolute number and year-to-year percentage variation for
# Integrative and Complementary Practices (PICS)
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 1. Create base table: add totals, rename variables, order, and filter
# -------------------------------------------------------------------
tbl1 <- df2 %>%
        adorn_totals("col") %>%  # add a "Total" column summing across all years
        select(-`2014`) %>%      # remove column 2014 (not used in this analysis)
        rename("Práticas Integrativas Complementares" = PICS) %>%  # rename for clarity
        arrange(desc(Total)) %>%  # order rows by descending total
        adorn_totals("row") %>%   # add a "Total" row at the bottom
        filter(Total != 0)        # remove rows with total equal to zero

# -------------------------------------------------------------------
# 2. Convert year columns to numeric for calculations
# -------------------------------------------------------------------
tbl1_numeric <- tbl1 %>%
        mutate(across(c(`2015`, `2016`, `2017`, `2018`, 
                        `2019`, `2020`, `2021`, `2022`, `2023`, `2024`), 
                      ~ as.numeric(.)))

# -------------------------------------------------------------------
# 3. Define a custom function to display the absolute number with
#    percentage variation (vs. previous year) below each value
# -------------------------------------------------------------------
add_variation_below <- function(current, previous) {
        current <- as.numeric(current)
        previous <- as.numeric(previous)
        
        # No variation for the first year
        if (is.na(previous)) {
                return(as.character(current))
        }
        
        # Avoid division by zero; show only the current value
        if (previous == 0) {
                return(paste0(
                        "<div style='font-size:11px;'>",
                        formatC(current, big.mark = ".", decimal.mark = ",", format = "f", digits = 0),
                        "</div>"
                ))
        }
        
        # Compute percentage variation compared to previous year
        variation <- round(((current - previous) / previous) * 100, 0)
        variation_text <- paste0("(", formatC(variation, format = "f", digits = 0), ")")
        
        # Return formatted HTML string with absolute value and variation (color-coded)
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
# 4. Compute year-to-year variations using the helper function
# -------------------------------------------------------------------
vars_2016 <- mapply(add_variation_below, tbl1_numeric$`2016`, tbl1_numeric$`2015`)
vars_2017 <- mapply(add_variation_below, tbl1_numeric$`2017`, tbl1_numeric$`2016`)
vars_2018 <- mapply(add_variation_below, tbl1_numeric$`2018`, tbl1_numeric$`2017`)
vars_2019 <- mapply(add_variation_below, tbl1_numeric$`2019`, tbl1_numeric$`2018`)
vars_2020 <- mapply(add_variation_below, tbl1_numeric$`2020`, tbl1_numeric$`2019`)
vars_2021 <- mapply(add_variation_below, tbl1_numeric$`2021`, tbl1_numeric$`2020`)
vars_2022 <- mapply(add_variation_below, tbl1_numeric$`2022`, tbl1_numeric$`2021`)
vars_2023 <- mapply(add_variation_below, tbl1_numeric$`2023`, tbl1_numeric$`2022`)
vars_2024 <- mapply(add_variation_below, tbl1_numeric$`2024`, tbl1_numeric$`2023`)

# -------------------------------------------------------------------
# 5. Replace year columns with formatted HTML strings containing
#    absolute numbers and variations
# -------------------------------------------------------------------
tbl1_with_vars <- tbl1_numeric %>%
        mutate(
                `2016` = vars_2016,
                `2017` = vars_2017,
                `2018` = vars_2018,
                `2019` = vars_2019,
                `2020` = vars_2020,
                `2021` = vars_2021,
                `2022` = vars_2022,
                `2023` = vars_2023,
                `2024` = vars_2024
        )

# -------------------------------------------------------------------
# 6. Render final formatted table with the GT package
# -------------------------------------------------------------------
gt_table <- tbl1_with_vars %>%
        gt() %>%
        tab_header(
                title = "",     # optional: add main title
                subtitle = ""   # optional: add subtitle
        ) %>%
        # Allow HTML rendering (so colors and formatting appear correctly)
        fmt_markdown(
                columns = vars(`2016`, `2017`, `2018`, `2019`, 
                               `2020`, `2021`, `2022`, `2023`, `2024`)
        ) %>%
        # General body text style
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
        # Table options for consistent layout
        tab_options(
                table.font.size = px(10),
                data_row.padding = px(5),
                container.width = pct(100),
                container.height = pct(100)
        ) %>%
        # Group all year columns under a spanner heading
        tab_spanner(
                label = "Absolute number and % change from previous year",
                columns = vars(`2015`, `2016`, `2017`, `2018`, 
                               `2019`, `2020`, `2021`, `2022`, `2023`, `2024`)
        ) %>% 
        # Format numeric values (2015 and Total columns only)
        fmt_number(
                columns = vars(`2015`, Total),
                decimals = 0,
                use_seps = TRUE,
                sep_mark = ".",  # thousands separator
                dec_mark = ","
        )  %>% 
        # Bold year column headers
        tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_column_labels(vars(`2015`, `2016`, `2017`, `2018`, 
                                                     `2019`, `2020`, `2021`, `2022`, 
                                                     `2023`, `2024`))
        ) %>% 
        # Left-align first column (PICS names)
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "left"
                ),
                locations = cells_body(columns = vars(1))
        ) %>%
        # Right-align all numeric columns
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "right"
                ),
                locations = cells_body(columns = vars(`2015`, `2016`, `2017`, `2018`, 
                                                      `2019`, `2020`, `2021`, `2022`, 
                                                      `2023`, `2024`, Total))
        ) %>% 
        # General visual theme: soft gray background and striped rows
        tab_options(
                table.background.color = "#f9f9f9", 
                column_labels.background.color = "#cccccc",
                row.striping.include_table_body = TRUE
        )

# -------------------------------------------------------------------
# 7. Print and export the table as an HTML file
# -------------------------------------------------------------------
gt_table
gtsave(gt_table, "outputs/raw_tables/t3.html")
