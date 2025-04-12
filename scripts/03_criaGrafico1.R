df_fig1 <- df %>% 
        group_by(ano) %>% 
        filter(ano != 2014) %>% 
        summarise(n = sum(value, na.rm = T))


df_fig1


# Criar o gráfico de linhas e pontos com rótulos
ggplot(df_fig1, aes(x = ano, y = n, group = 1)) +
        geom_line(color = "black", size = 1) +                    # Adicionar a linha
        geom_point(color = "black", size = 2) +                   # Adicionar os pontos
        geom_text(
                aes(label = format(n, big.mark = ".", decimal.mark = ",")),
                nudge_y = 130000,  # Nudges text upwards by 50,000 units (adjust as needed)
                nudge_x = -.1,      # Nudges text along the x-axis
                size = 2.5
        )+
        labs(title = "",
             x = "Ano", y = "Número de Procedimentos") +
        theme_classic() +   
        scale_y_continuous(
                limits = c(0, 2400000),
                breaks = seq(0, 2400000, by = 250000),
                labels = label_comma(big.mark = ".", decimal.mark = ",")
        )+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),  # Ajustar título e rótulos
              axis.title.x = element_text(size = 8),
              axis.title.y = element_text(size = 8),
              axis.text.x = element_text(hjust = 1))+
        theme_set(theme_gray(base_family = "Times New Roman"))

        # Rotacionar os rótulos dos anos

ggsave("Gráfico1_semgrid.png", width = 9, height = 4)

