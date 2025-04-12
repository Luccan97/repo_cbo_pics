

tbl1 <- df2 %>%
        adorn_totals("col") %>%
        select(-`2014`) %>% 
        rename("Práticas Integrativas Complementares" = PICS) %>%
        arrange(desc(Total)) %>%
        adorn_totals("row") %>% 
        filter(Total != 0)

tbl1_numeric <- tbl1 %>%
        mutate(across(c(`2015`, `2016`, `2017`, `2018`, 
                        `2019`, `2020`, `2021`, `2022`, `2023`, `2024`), ~ as.numeric(.)))

# Função para formatar número absoluto e variação abaixo
add_variation_below <- function(current, previous) {
        current <- as.numeric(current)
        previous <- as.numeric(previous)
        
        if (is.na(previous)) {
                return(as.character(current))  # Sem variação para o primeiro ano
        }
        
        if (previous == 0) {
                return(paste0(
                        "<div style='font-size:11px;'>",
                        formatC(current, big.mark = ".", decimal.mark = ",", format = "f", digits = 0),
                        "</div>"
                ))  # Sem variação se o anterior = 0
        }
        
        variation <- round(((current - previous) / previous) * 100, 0)
        variation_text <- paste0("(", formatC(variation, format = "f", digits = 0), ")")
        
        # Retorna o número absoluto e variação formatados
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

# Calcular as variações

arrows_2016 <- mapply(add_variation_below, tbl1_numeric$`2016`, tbl1_numeric$`2015`)
arrows_2017 <- mapply(add_variation_below, tbl1_numeric$`2017`, tbl1_numeric$`2016`)
arrows_2018 <- mapply(add_variation_below, tbl1_numeric$`2018`, tbl1_numeric$`2017`)
arrows_2019 <- mapply(add_variation_below, tbl1_numeric$`2019`, tbl1_numeric$`2018`)
arrows_2020 <- mapply(add_variation_below, tbl1_numeric$`2020`, tbl1_numeric$`2019`)
arrows_2021 <- mapply(add_variation_below, tbl1_numeric$`2021`, tbl1_numeric$`2020`)
arrows_2022 <- mapply(add_variation_below, tbl1_numeric$`2022`, tbl1_numeric$`2021`)
arrows_2023 <- mapply(add_variation_below, tbl1_numeric$`2023`, tbl1_numeric$`2022`)
arrows_2024 <- mapply(add_variation_below, tbl1_numeric$`2024`, tbl1_numeric$`2023`)
# Criar tabela atualizada com as variações
tbl1_with_arrows <- tbl1_numeric %>%
        mutate(
               `2016` = arrows_2016,
               `2017` = arrows_2017,
               `2018` = arrows_2018,
               `2019` = arrows_2019,
               `2020` = arrows_2020,
               `2021` = arrows_2021,
               `2022` = arrows_2022,
               `2023` = arrows_2023,
               `2024` =  arrows_2024)

# Renderizar tabela com GT
gt_table <- tbl1_with_arrows %>%
        gt() %>%
        tab_header(
                title = "",
                subtitle = ""
        ) %>%
        # Permitir renderização de HTML
        fmt_markdown(
                columns = vars(`2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`)
        ) %>%
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "center"
                ),
                locations = cells_body(columns = everything())
        ) %>%
        tab_style(
                style = cell_text(font = "Times New Roman", size = px(11), weight = "normal"),
                locations = cells_column_labels(columns = everything())
        ) %>%
        tab_options(
                table.font.size = px(10),
                data_row.padding = px(5),
                container.width = pct(100),
                container.height = pct(100)
        ) %>%
        tab_spanner(
                label = "Número absoluto e variação percentual em relação ao ano anterior",
                columns = vars(`2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`)
        ) %>% 
        fmt_number(
                columns = vars(`2015`, Total),  # Columns to format
                decimals = 0,
                use_seps = TRUE,
                sep_mark = ".",  # Thousands separator
                dec_mark = ","
        )  %>% 
        tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_column_labels(vars(`2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`))
        ) %>% 
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "left"
                ),
                locations = cells_body(columns = vars(1))
        ) %>%
        # Alinhar as demais colunas à direita
        tab_style(
                style = cell_text(
                        font = "Times New Roman",
                        align = "right"
                ),
                locations = cells_body(columns = vars(`2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`,`2024`,Total))
        ) %>% 
        tab_options(
                table.background.color = "#f9f9f9", 
                
                column_labels.background.color = "#cccccc", # Cor de fundo dos rótulos das colunas
                row.striping.include_table_body = TRUE # Ativar listras alternadas
        )

# Imprimir a tabela
gt_table

gtsave(gt_table, "t2.html")
