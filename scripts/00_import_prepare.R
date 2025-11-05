
# Load packages -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(
        readxl,
        scales,
        janitor,
        gt,
        gtExtras,
        forcats,
        purr,
        tidyverse
)


# List of allowed values for the first column
allowed_values <- c(
        "SESSÃO DE HIPNOTERAPIA",
        "SESSÃO DE ARTETERAPIA",
        "SESSÃO DE APITERAPIA",
        "SESSÃO DE BIODANÇA",
        "YOGA",
        "SESSÃO DE REIKI",
        "TRATAMENTO TERMAL/CRENOTERÁPICO",
        "TRATAMENTO HOMEOPÁTICO",
        "SESSÃO DE CROMOTERAPIA",
        "SESSÃO DE ELETROESTIMULAÇÃO",
        "SESSÃO DE GEOTERAPIA",
        "SESSÃO DE MEDITAÇÃO",
        "SESSÃO DE MASSOTERAPIA",
        "TERAPIA COMUNITÁRIA",
        "SESSÃO DE OZONIOTERAPIA APLICADA À ODONTOLOGIA",
        "SESSAO DE ACUPUNTURA COM INSERCAO DE AGULHAS",
        "SESSÃO DE AURICULOTERAPIA",
        "SESSÃO DE CONSTELAÇÃO FAMILIAR",
        "OFICINA DE MASSAGEM/ AUTO-MASSAGEM",
        "TRATAMENTO EM MEDICINA TRADICIONAL CHINESA",
        "SESSÃO DE IMPOSIÇÃO DE MÃOS",
        "TRATAMENTO NATUROPÁTICO",
        "SESSÃO DE BIOENERGÉTICA",
        "SESSÃO DE AROMATERAPIA",
        "TRATAMENTO ANTROPOSÓFICO",
        "SESSÃO DE ANTROPOSOFIA APLICADA À SAÚDE",
        "SESSÃO DE MUSICOTERAPIA",
        "SESSÃO DE TERMALISMO",
        "TRATAMENTO AYURVÉDICO",
        "SESSÃO DE TERAPIA DE FLORAIS",
        "TRATAMENTO FITOTERÁPICO",
        "PRATICAS CORPORAIS EM MEDICINA TRADICIONAL CHINESA",
        "SESSÃO DE TRATAMENTO QUIROPRÁTICO",
        "SESSÃO DE TRATAMENTO OSTEOPÁTICO",
        "SESSAO DE ACUPUNTURA APLICACAO DE VENTOSAS / MOXA",
        "SESSÃO DE DANÇA CIRCULAR"
)

# Function to process individual files with verification
process_file <- function(file_path) {
        # Extract the file name without extension
        file_name <- basename(file_path)
        file_name <- tools::file_path_sans_ext(file_name)
        
        # Split the file name to get 'regiao' and 'ano'
        parts <- strsplit(file_name, "_")[[1]]
        ano <- parts[3]
        
        # Read the file with error handling
        df <- tryCatch({
                read_excel(file_path)
        }, error = function(e) {
                message(paste("Error reading file:", file_path))
                return(NULL)
        })
        
        # Check if the file was read successfully
        if (is.null(df)) {
                return(NULL)
        }
        
        # Check if the data frame has the expected number of columns (at least 2)
        if (ncol(df) < 2) {
                message(paste("Unexpected number of columns in file:", file_path))
                return(NULL)
        }
        
        # Remove rows where the first column is NA or not in allowed values
        df <- df %>% filter(!is.na(.[[1]]), .[[1]] %in% allowed_values)
        
        # Check if the data frame is not empty after filtering
        if (nrow(df) == 0) {
                message(paste("No valid data remaining after filtering in file:", file_path))
                return(NULL)
        }
        
        # Pivot longer with error handling
        df <- tryCatch({
                df %>%
                        pivot_longer(cols = -1, names_to = "variable", values_to = "value") %>%
                        mutate(ano = ano)
        }, error = function(e) {
                message(paste("Error pivoting data in file:", file_path))
                return(NULL)
        })
        
        # Check if the pivot was successful
        if (is.null(df)) {
                return(NULL)
        }
        
        # Convert the 'value' column to numeric, removing non-numeric entries
        df <- df %>%
                mutate(value = gsub("[^0-9,-]", "", value), # Remove non-numeric characters except comma and dash
                       value = gsub(",", ".", value),      # Replace comma with dot
                       value = as.numeric(value)) %>%
                filter(!is.na(value))
        
        return(df)
}

# Directory containing the files
data_dir <- "data_raw"

# List all files in the directory
files <- list.files(data_dir, full.names = TRUE, pattern = "\\.xlsx$")

# Process all files and bind the data
all_data <- files %>%
        map_dfr(process_file)


df <- all_data %>% 
        rename(cbo = variable,
               PICS = `Praticas Integrativas Compleme`)
# View the combined data
print(all_data)

saveRDS(df, "data_clean/df.RDS")


        
df1 <- df %>% 
        group_by(cbo, ano) %>% 
        filter(ano != 2014) %>% 
        summarise(n = sum(value, na.rm = T)) %>% 
        mutate(cbo = case_when(
                cbo == "Técnico e auxiliar de enfermag" ~ "Técnico e auxiliar de enfermagem",
                cbo == "Profissional de educação físic" ~ "Profissional de educação física",
                cbo == "Técnico e auxiliar de saúde bu" ~ "Técnico e auxiliar de saúde bucal",
                TRUE ~ cbo
                
        )) %>% 
        pivot_wider(id_cols = cbo, 
                    names_from = "ano", 
                    values_from = n) 
        
# 

df2 <- df %>% 
        group_by(PICS, ano) %>% 
        summarise(n = sum(value, na.rm = T)) %>% 
        pivot_wider(id_cols = PICS, 
                    names_from = "ano", 
                    values_from = n) %>% 
        mutate(PICS = str_to_sentence(PICS))

df2[is.na(df2)] <- 0

