
# 1. Filtrar os 10 CBOs definidos e calcular as 10 principais PICS
df_fig3 <- df %>%
        group_by(cbo, PICS) %>%
        summarise(n = sum(value, na.rm = TRUE), .groups = "drop") %>%
        filter(cbo %in% c("Fisioterapeuta", 
                          "Enfermeiro", 
                          "Técnico e auxiliar de enfermag",
                          "Médico",
                          "Profissional de educação físic",
                          "Farmacêutico",
                          "Terapeuta ocupacional",
                          "Nutricionista",
                          "Psicólogo",
                          "Fonoaudiólogo",
                          "Terapeuto Holístico",
                          "Assistente Social",
                          "Agente comunitário de saúde",
                          "Naturólogo",
                          "Arteterapeuta")) %>%
        drop_na()

# 2. Identificar as 10 práticas (PICS) mais realizadas
top_10_pics <- df_fig3 %>%
        group_by(PICS) %>%
        summarise(total_n = sum(n), .groups = "drop") %>%
        arrange(desc(total_n)) %>%
        slice_head(n = 15) %>%
        pull(PICS)

# 3. Reordenar os fatores para melhor apresentação
cbo_order <- df_fig3 %>%
        group_by(cbo) %>%
        summarise(total_n = sum(n)) %>%
        arrange(desc(total_n)) %>%
        pull(cbo)

pics_order <- df_fig3 %>%
        group_by(PICS) %>%
        summarise(total_n = sum(n)) %>%
        arrange(desc(total_n)) %>%
        pull(PICS)

# 4. Dataframe para o gráfico reduzido (10 principais PICS)
df_fig3_reduced <- df_fig3 %>%
        filter(PICS %in% top_10_pics) %>%
        mutate(cbo = factor(cbo, levels = cbo_order),
               PICS = factor(PICS, levels = top_10_pics),
               cbo = str_to_title(cbo))

# 1. Resumindo os CBOs (incluindo Terapeuta holístico)
library(dplyr)

df_fig3_reduced <- df_fig3_reduced %>%
        mutate(cbo = case_when(
                cbo == "Agente Comunitário de Saúde" ~ "Ag. Saúde",
                cbo == "Arteterapeuta" ~ "Arteterapeuta",
                cbo == "Assistente Social" ~ "Assist. social",
                cbo == "Enfermeiro" ~ "Enfermeiro",
                cbo == "Farmacêutico" ~ "Farmacêutico",
                cbo == "Fisioterapeuta" ~ "Fisioterapeuta",
                cbo == "Fonoaudiólogo" ~ "Fonoaudiólogo",
                cbo == "Médico" ~ "Médico",
                cbo == "Naturólogo" ~ "Naturólogo",
                cbo == "Nutricionista" ~ "Nutricionista",
                cbo == "Profissional De Educação Físic" ~ "Prof. ed. física",
                cbo == "Psicólogo" ~ "Psicólogo",
                cbo == "Terapeuta Ocupacional" ~ "Ter. ocupacional",
                cbo == "Técnico e Auxiliar De Enfermag" ~ "Téc./aux. enfermagem",
                cbo == "Terapeuto Holístico" ~ "Ter. holístico",
                TRUE ~ cbo  # 👈 Keep original value if none of the conditions match
        ))


df_fig3_reduced <- df_fig3_reduced %>%
        mutate(cat_n = case_when(
                n < 10000 ~ "< 10.000",
                n >= 10000 & n < 50000 ~ "10.000 – 50.000",
                n >= 50000 & n < 100000 ~ "50.000 – 100.000",
                n >= 100000 ~ "> 100.000"
        )) 
        

# 2. Definindo ordem e cores corrigidas para as categorias
df_fig3_reduced$cat_n <- factor(df_fig3_reduced$cat_n, 
                                levels = c("< 10.000", "10.000 – 50.000", "50.000 – 100.000", "> 100.000"))

cores_categoria <- c(
        "< 10.000" = "white",
        "10.000 – 50.000" = "yellow",
        "50.000 – 100.000" = "orange",
        "> 100.000" = "red"
)

# 3. Gráfico final com legenda organizada e vertical
ggplot(df_fig3_reduced, aes(x = cbo, y = PICS, fill = cat_n)) +
        geom_tile(color = "white") +
        scale_fill_manual(
                values = cores_categoria,
                name = "Nº de Procedimentos"  # 🏷️ Legenda ajustada
        ) +
        geom_text(
                aes(label = format(n, big.mark = ".", decimal.mark = ",")),
                color = "black", size = 3
        ) +
        labs(
                title = "",
                x = "CBO",
                y = "Prática Integrativa PICS"
        ) +
        scale_x_discrete(position = "top") +
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0, size = 10),
                axis.text.y = element_text(size = 10),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                legend.position = "right",               # 📍 Legenda na vertical
                legend.direction = "vertical",
                legend.box = "vertical",
                legend.key.width = unit(1, "cm"),
                legend.title = element_text(face = "bold")
        ) +
        guides(fill = guide_legend(ncol = 1))  # 🏹 Força a legenda em uma coluna (vertical)

ggsave("Grafico3_completo_semGrid.png", width = 15, height = 12)

