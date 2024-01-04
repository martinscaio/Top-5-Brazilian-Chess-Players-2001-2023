

library(tidyverse)




dados <- read_csv("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\temporario.csv")

dados2 <- read_csv("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\temporario2.csv")

dados3 <- read_csv("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\temporario3.csv")

dados4 <- read_csv("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\temporario4.csv")

dados5 <- read_csv("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\temporario5.csv")

dados_problematico <- read_csv("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\temporario_problematico.csv")

faltantes <- read_csv("C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Xadrez_Top10\\faltantes.csv")


ChessPlayersBRA <- rbind(dados, dados2,dados3,dados4,dados5,dados_problematico, faltantes)


# função que pegava tudo menos WI mas funcionava 
#arrumar <- function(df){
#  
#  df %>% dplyr::mutate(Flag = if_else(is.na(Flag),'m', Flag),
#                       Ano = stringr::str_extract_all(data, "\\d{2}"),
#                       Ano = paste0('20', Ano),
#                       Ano = lubridate::parse_date_time(Ano, orders = "y"),
#                       Ano = lubridate::year(Ano),
#                       Mes = stringr::str_extract_all(data, "\\w{3}"),
#                       Mes = lubridate::parse_date_time(Mes, orders = 'm'),
#                       MesNum = lubridate::month(Mes),
#                       Mes = lubridate::month(Mes, label = TRUE),
#                       Mes = month.name[Mes])
#  
#}








arrumar <- function(df){
  
  df %>% dplyr::mutate(Flag = if_else(!is.na(value) & grepl("\\bwi\\b", value), 'wi', 
                                      if_else(is.na(Flag), 'm', Flag)),
                       Ano = stringr::str_extract_all(data, "\\d{2}"),
                       Ano = paste0('20', Ano),
                       Ano = lubridate::parse_date_time(Ano, orders = "y"),
                       Ano = lubridate::year(Ano),
                       Mes = stringr::str_extract_all(data, "\\w{3}"),
                       Mes = lubridate::parse_date_time(Mes, orders = 'm'),
                       MesNum = lubridate::month(Mes),
                       Mes = lubridate::month(Mes, label = TRUE),
                       Mes = month.name[Mes])
  
}



# Função para selecionar o top 10 eliminando os jogadores inativos
top_dez <- function(df){
  df %>%
    filter(Flag == 'm'| Flag == 'w')%>%
    arrange(desc(Rating)) %>% 
    group_by(data) %>%
    slice_head(n = 10) %>% 
    mutate(ranking = row_number())
}




top_feminino <- function(df){
  df %>%
    filter(Flag == 'w')%>%
    arrange(desc(Rating)) %>% 
    group_by(data) %>%
    slice_head(n = 10) %>% 
    mutate(ranking = row_number())
}




ChessPlayersBRA <- ChessPlayersBRA %>% arrumar()


ChessPlayersBRA_Filtered <- ChessPlayersBRA %>% top_dez()

ChessPlayersBRA_Filtered <- ChessPlayersBRA %>% top_feminino()





ChessPlayersBRA_Filtered <- ChessPlayersBRA_Filtered %>% 
  mutate(Nome = case_when(Nome == "Fier, Alexander" ~ "Fier, Alexandr",
                          Nome == "Pinto, Renato" ~ "Quintiliano, Renato",
                          Nome == "Debs, Felipe" ~ "El Debs, Felipe",
                          Nome == "Leitao, Rafael" ~ "Leitão, Rafael",
                          Nome == "Neto, Jaime" ~ "Sunye, Jaime",
                          Nome == "Berardino, Diego" ~ "Di Berardino, Diego ",
                          Nome == "Supi, Luis" ~ "Supi, Luís Paulo",TRUE ~ Nome))


# problema com o darcy que aparentemente tá duplicado

ChessPlayersBRA_Filtered <- ChessPlayersBRA_Filtered %>% filter(Nome != "FT,IO")

ChessPlayersBRA_Filtered$Data <- paste0(ChessPlayersBRA_Filtered$Mes, '/',ChessPlayersBRA_Filtered$Ano)



# para reordenar
ChessPlayersBRA_Filtered$nova_data <- make_date(year = ChessPlayersBRA_Filtered$Ano, month = ChessPlayersBRA_Filtered$MesNum)


ChessPlayersBRA_Filtered <-  ChessPlayersBRA_Filtered %>% arrange(nova_data)



ChessPlayersBRA_Filtered <- ChessPlayersBRA_Filtered %>% select(Nome,Rating,Data)





ChessPlayersBRA_Filtered <- ChessPlayersBRA_Filtered %>% 
  separate(Nome, into = c('Sobrenome', 'Nome'), sep = ',') %>% 
  mutate(Nome = str_remove_all(Nome, " ")) %>% 
  unite("Nome", Nome:Sobrenome, sep = " ")


ChessPlayersBRA_Filtered <- ChessPlayersBRA_Filtered %>% ungroup() %>% select(Nome,Rating,Data)




ChessPlayersBRA_Filtered <- ChessPlayersBRA_Filtered %>% mutate(Nome = case_when(Nome == "LuísPaulo Supi" ~ "Luís Paulo Supi", TRUE ~ Nome))

ChessPlayersBRA_Filtered <- ChessPlayersBRA_Filtered %>% 
  pivot_wider(names_from = Data, values_from = Rating) %>% 
  replace(is.na(.),0)
  


ChessPlayersBRA %>% group_by(Ano, Flag) %>% count() %>% arrange(desc(n)) %>% view()
  
  
ChessPlayersBRA %>% filter(Flag == 'm' | Flag == 'w') %>% group_by(Ano, Flag) %>% count() %>% arrange(desc(n)) %>% view()

ChessPlayersBRA %>% filter(Nome == 'Ratcu, Tatiana') %>% view()


ChessPlayersBRA %>% filter(Flag == 'wi') %>% group_by(Ano, Flag) %>% count() %>% arrange(desc(n)) %>% view()



ChessPlayersBRA %>% filter(Ano == 2021 | Ano == 2022) %>% filter(Flag == 'w' | Flag == 'wi') %>% view()



jogadoras_w_para_wi <- ChessPlayersBRA %>%
  filter(Ano == 2021 & Flag == 'w') %>%
  semi_join(ChessPlayersBRA %>% filter(Ano == 2022 & Flag == 'wi'), by = "Nome") %>% view()


jogadoras_w_para_wi %>% arrange(desc(Rating)) %>% view()

# DADOS PRONTOS PARA SEREM EXPORTADOS PARA O FLOURISH


write.csv(ChessPlayersBRA_Filtered, "C:\\Users\\Caio\\Desktop\\Projeto Xadrez\\Top5ChessFemale.csv")


