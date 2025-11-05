# ------------------------------------------------------------
# Figure 1: Annual number of procedures (line chart)
# ------------------------------------------------------------

# Summarize total number of procedures by year
df_fig1 <- df %>% 
        group_by(ano) %>%                            # Group data by year
        filter(ano != 2014) %>%                      # Exclude the year 2014 from analysis
        summarise(n = sum(value, na.rm = TRUE))      # Sum all 'value' entries per year

# Display summarized table
df_fig1


# ------------------------------------------------------------
# Create a line chart with labeled points
# ------------------------------------------------------------

ggplot(df_fig1, aes(x = ano, y = n, group = 1)) +
        geom_line(color = "black", size = 1) +                     # Draw the line connecting years
        geom_point(color = "black", size = 2) +                    # Add points for each year
        geom_text(
                aes(label = format(n, big.mark = ".", decimal.mark = ",")),  # Format numbers with dots as thousands separators
                nudge_y = 130000,    # Move labels slightly upward to avoid overlap
                nudge_x = -0.1,      # Slight horizontal adjustment
                size = 2.5           # Label font size
        ) +
        labs(title = "", 
             x = "Year", 
             y = "Number of Procedures") +                         # Axis labels
        scale_y_continuous(
                limits = c(0, 2400000),                            # Define y-axis range
                breaks = seq(0, 2400000, by = 250000),             # Set tick marks every 250,000
                labels = label_comma(big.mark = ".", decimal.mark = ",")  # Use Brazilian number format
        ) +
        theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 10),  # Center and bold the title
                axis.title.x = element_text(size = 8),                             # Customize x-axis title
                axis.title.y = element_text(size = 8),                             # Customize y-axis title
                axis.text.x = element_text(hjust = 1)                              # Align x-axis labels
        ) +
        theme_set(theme_classic(base_family = "Times New Roman"))  # Use a clean classic theme with serif font


# ------------------------------------------------------------
# Save the plot as a PNG file
# ------------------------------------------------------------

ggsave("outputs/graphics/g1.png", width = 9, height = 4)
