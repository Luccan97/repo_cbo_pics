# ------------------------------------------------------------
# Figure 3: Heatmap of Integrative and Complementary Practices (PICS)
# by Occupation (CBO)
# ------------------------------------------------------------

# 1. Filter selected occupations (CBOs) and calculate total PICS counts
df_fig3 <- df %>%
        group_by(cbo, PICS) %>%
        summarise(n = sum(value, na.rm = TRUE), .groups = "drop") %>%  # Aggregate total number of procedures per occupation and PICS
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
        drop_na()  # Remove missing values

# 2. Identify the top 15 most frequently performed PICS
top_10_pics <- df_fig3 %>%
        group_by(PICS) %>%
        summarise(total_n = sum(n), .groups = "drop") %>%
        arrange(desc(total_n)) %>%
        slice_head(n = 15) %>%
        pull(PICS)

# 3. Reorder factor levels for better visualization
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

# 4. Create a reduced dataset including only the top PICS
df_fig3_reduced <- df_fig3 %>%
        filter(PICS %in% top_10_pics) %>%
        mutate(cbo = factor(cbo, levels = cbo_order),     # Reorder occupations
               PICS = factor(PICS, levels = top_10_pics), # Reorder PICS
               cbo = str_to_title(cbo))                   # Capitalize names

# 5. Translate occupation names (CBOs) into English
df_fig3_reduced <- df_fig3_reduced %>%
        mutate(cbo = case_when(
                cbo == "Fisioterapeuta" ~ "Physiotherapist",
                cbo == "Enfermeiro" ~ "Nurse",
                cbo == "Técnico E Auxiliar De Enfermag" ~ "Technician and Nursing Assistant",
                cbo == "Médico" ~ "Physician",
                cbo == "Profissional De Educação Físic" ~ "Professional of Physical Education",
                cbo == "Farmacêutico" ~ "Pharmaceutical",
                cbo == "Terapeuta Ocupacional" ~ "Occupational Therapist",
                cbo == "Nutricionista" ~ "Nutritionist",
                cbo == "Psicólogo" ~ "Psychologist",
                cbo == "Fonoaudiólogo" ~ "Speech Therapist",
                cbo == "Terapeuto Holístico" ~ "Holistic Therapist",
                cbo == "Assistente Social" ~ "Social Worker",
                cbo == "Agente Comunitário De Saúde" ~ "Community Health Worker",
                cbo == "Naturólogo" ~ "Naturopath",
                cbo == "Arteterapeuta" ~ "Art Therapist",
                TRUE ~ cbo
        ))

# 6. Translate PICS descriptions into English
df_fig3_reduced <- df_fig3_reduced %>%
        mutate(PICS = case_when(
                PICS == "PRATICAS CORPORAIS EM MEDICINA TRADICIONAL CHINESA" ~ "Body practices in traditional Chinese medicine",
                PICS == "SESSAO DE ACUPUNTURA APLICACAO DE VENTOSAS / MOXA" ~ "Acupuncture session with cupping/moxa",
                PICS == "SESSAO DE ACUPUNTURA COM INSERCAO DE AGULHAS" ~ "Acupuncture session with insertion of needles",
                PICS == "SESSÃO DE ANTROPOSOFIA APLICADA À SAÚDE" ~ "Session of applied anthroposophy to health",
                PICS == "SESSÃO DE AROMATERAPIA" ~ "Session of aromatherapy",
                PICS == "SESSÃO DE AURICULOTERAPIA" ~ "Session of auriculotherapy",
                PICS == "SESSÃO DE CONSTELAÇÃO FAMILIAR" ~ "Session of family constellation",
                PICS == "SESSÃO DE ELETROESTIMULAÇÃO" ~ "Session of electrostimulation",
                PICS == "SESSÃO DE GEOTERAPIA" ~ "Geotherapy session",
                PICS == "SESSÃO DE IMPOSIÇÃO DE MÃOS" ~ "Session of hand imposition",
                PICS == "SESSÃO DE MASSOTERAPIA" ~ "Session of massotherapy",
                PICS == "SESSÃO DE MEDITAÇÃO" ~ "Meditation session",
                PICS == "TRATAMENTO EM MEDICINA TRADICIONAL CHINESA" ~ "Treatment with traditional Chinese medicine",
                PICS == "TRATAMENTO FITOTERÁPICO" ~ "Phytotherapeutic treatment",
                PICS == "YOGA" ~ "Yoga",
                TRUE ~ PICS
        ))

# 7. Create categorical variable for number of procedures
df_fig3_reduced <- df_fig3_reduced %>%
        mutate(cat_n = case_when(
                n < 10000 ~ "< 10,000",
                n >= 10000 & n < 50000 ~ "10,000 – 50,000",
                n >= 50000 & n < 100000 ~ "50,000 – 100,000",
                n >= 100000 ~ "> 100,000"
        ))

# Set ordered factor levels for categories
df_fig3_reduced$cat_n <- factor(df_fig3_reduced$cat_n, 
                                levels = c("< 10,000", "10,000 – 50,000", "50,000 – 100,000", "> 100,000"))

# 8. Define color palette for the categories
cores_categoria <- c(
        "< 10,000" = "white",
        "10,000 – 50,000" = "yellow",
        "50,000 – 100,000" = "orange",
        "> 100,000" = "red"
)

# 9. Create final heatmap
ggplot(df_fig3_reduced, aes(x = cbo, y = PICS, fill = cat_n)) +
        geom_tile(color = "white") +                                # Draw tiles for each cell
        scale_fill_manual(
                values = cores_categoria,                           # Apply custom colors
                name = "Number of Procedures"
        ) +
        geom_text(
                aes(label = format(n, big.mark = ".", decimal.mark = ",")),  # Add counts as text inside tiles
                color = "black", size = 3
        ) +
        labs(
                title = "",
                x = "Occupation (CBO)",
                y = "Integrative Practice (PICS)"
        ) +
        scale_x_discrete(position = "top") +                        # Move x-axis labels to the top
        theme_classic() +
        theme(
                axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0, size = 10), # Rotate occupation labels
                axis.text.y = element_text(size = 10),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                legend.position = "right",
                legend.direction = "vertical",
                legend.box = "vertical",
                legend.key.width = unit(1, "cm"),
                legend.title = element_text(face = "bold")
        ) +
        guides(fill = guide_legend(ncol = 1))                       # Single-column legend

# 10. Save figure as PNG

ggsave("outputs/graphics/g2.png", width = 15, height = 12)
