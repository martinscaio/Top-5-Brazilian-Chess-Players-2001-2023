library(tidyverse)
library(purrr)

# Unzipando os arquivos na pasta temporario-----------------------------

diretorio_raiz <- "C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\fide_rating"

# Diretório de destino para descompactar os arquivos
diretorio_destino <- "C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\temporario"

# Lista de pastas ZIP
pastas_zip <- list.files(diretorio_raiz, pattern = "\\.zip$", full.names = TRUE)



for (pasta_zip in pastas_zip) {
  
  
  
  nome_subdiretorio <- tools::file_path_sans_ext(basename(pasta_zip))
  
  diretorio_temporario <- file.path(diretorio_destino, nome_subdiretorio)
  
  
  
  if (!file.exists(diretorio_temporario)) {
    dir.create(diretorio_temporario)
  
  }
  
  
  
  
  unzip(pasta_zip, exdir = diretorio_temporario)

  
}


# Extraindo informações--------------------------

# Função para extrair informações válidas





limpar <- function(base_dados) {
  data_rating <- stringr::str_extract(base_dados$value[1], "[A-Za-z]{3}\\d{2}")
  
  base_dados <- base_dados %>%
    dplyr::mutate(IdNumber = stringr::str_extract_all(value, "\\d{7}"),
                  Nome = stringr::str_extract_all(value, "\\w+,[[:space:]]*\\w+"),# antiga abordagem \\w+, \\w+
                  Paises = stringr::str_extract(value, "\\b[A-Z]{3}\\b"),
                  Rating = stringr::str_extract_all(value, "\\b\\d{4}\\b") %>%
                    lapply(function(x) ifelse(length(x) > 0, x, NA)) %>%
                    sapply(function(x) paste(x, collapse = " ")),
                  Flag = stringr::str_extract(value, "\\b[wi]\\b"))
  
  base_dados <- base_dados %>% dplyr::mutate(data = data_rating)
  
  return(base_dados)
}

# Função para extrair data do título do arquivo
extrair_data_do_titulo <- function(arquivo) {
  # Extrair a data do nome do arquivo usando expressões regulares
  data <- stringr::str_extract(arquivo, "([A-Za-z]{3}\\d{2}|(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)\\d{2}FRL|standard_[A-Za-z]{3}\\d{2})")
  return(data)
}





# Função para processar um arquivo
processar_arquivo <- function(arquivo) {
  texto <- readLines(arquivo)
  texto <- texto %>% as_data_frame()
  df <- limpar(texto)
  
  # Verificar se a data não foi encontrada na primeira linha
  if (is.na(df$data[1])) {
    # Extrair a data do título do arquivo
    df$data[1] <- extrair_data_do_titulo(arquivo)
  }
  
  return(df)
}

diretorio_base <- "C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\faltantes"

# Lista de pastas
pastas <- list.dirs(diretorio_base, full.names = TRUE, recursive = FALSE)

# Lista de dataframes, um para cada arquivo
lista_dataframes <- map(pastas, ~ list.files(.x, pattern = "\\.txt$|", full.names = TRUE) %>%
                          map_df(processar_arquivo))

# Juntar todos os dataframes em um único dataframe
df_final <- reduce(lista_dataframes, bind_rows)


# Função para filtrar brasil

filter_bra <- function(df){
  df <- df %>% dplyr::filter(Paises == "BRA")
  df <- df %>%  as_data_frame()
  df <- df %>% tidyr::unnest(cols = c(Nome, IdNumber))
}


temporario <- df_final %>% filter_bra()


temporario %>% view()

# Validar
temporario %>% group_by(data) %>% count() %>% view()



# Salvando os temporarios
write.csv(temporario, "C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\faltantes.csv")





# EXTRAINDO DADOS 1975-2000--------------------------------------(INCOMPLETO)



teste$data <- names(teste)[3]


teste[1]

teste <- readLines("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\Ratings_Before_2000s\\1975-01\\1975-01.txt")


teste <- teste %>% as_data_frame()

teste$data <- teste$value[1]



teste %>% 
  mutate(Nome = str_extract(value, "(^[[:alpha:],]+(?:\\s+[[:alpha:],]+)?(?:,[[:alpha:],]+)?)"),
         Nome = sapply(Nome, cortar_apos_espaco),
         Paises = str_extract_all(value,"\\b[A-Z]{3}\\b"),
         Rating = str_extract(value, "\\b\\d{4}\\b")) %>% 
  slice(5:n()) %>% view()



cortar_apos_espaco <- function(texto) {
  palavras <- stringr::str_split(texto, "\\s+")[[1]]  # Dividir o texto por espaços
  if (length(palavras) > 0) {
    return(palavras[1])  # Se houver palavras, pegar a primeira
  } else {
    return("")  # Se não houver, retornar uma string vazia
  }
}



limpar2 <- function(dados){
  dados <- dados %>% 
    dplyr::mutate(Nome = str_extract(value, "(^[[:alpha:],]+(?:\\s+[[:alpha:],]+)?(?:,[[:alpha:],]+)?)"),
                  Nome = sapply(Nome, cortar_apos_espaco),
                  Paises = str_extract_all(value,"\\b[A-Z]{3}\\b"),
                  Rating = str_extract(value, "\\b\\d{4}\\b")) %>% 
    dplyr::slice(5:n())
  
  
  return(dados)
}


leitura <- function(arquivo){
  
  texto <- readLines(arquivo)
  texto <- texto %>% as_data_frame()
  texto$data <- texto$value[1]
  
  df <- limpar2(texto)
  
  
  return(df)
  
  
}




diretorio_base <- "C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\Ratings_Before_2000s"

# Lista de pastas
pastas <- list.dirs(diretorio_base, full.names = TRUE, recursive = FALSE)

# Lista de dataframes, um para cada arquivo
lista_dataframes <- map(pastas, ~ list.files(.x, pattern = "\\.txt$|", full.names = TRUE) %>%
                          map_df(leitura))

# Juntar todos os dataframes em um único dataframe
df_final <- reduce(lista_dataframes, bind_rows)





filter_bra <- function(df){
  df <- df %>% dplyr::filter(Paises == "BRA")
  df <- df %>%  as_data_frame()
  df <- df %>% tidyr::unnest(cols = c(Nome))
}


temporario <- df_final %>% filter_bra()




temporario <- temporario %>% mutate(ano = str_extract_all(data, "\\d{4}"))

temporario %>% arrange(desc(Rating)) %>% group_by(ano) %>% slice_head(n = 10) %>% arrange(ano, desc(Rating)) %>% view()



