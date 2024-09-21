library(tidyverse)   # tidyverse
library(readxl)      # para ler os arquivos
library(lubridate)   # para manipular datas
library(dplyr)       # para poder juntar tabelas

# leitura dos arquivos
tempo <- read_csv2("tempo.CSV")
janeiro <-read_excel("janeiro.xlsx", sheet = 2)
fevereiro <-read_excel("fevereiro.xlsx", sheet = 2)
marco <-read_excel("marco.xlsx", sheet = 2)
abril <-read_excel("abril.xlsx", sheet = 2)
maio <-read_excel("maio.xlsx", sheet = 2)
junho <-read_excel("junho.xlsx", sheet = 2)
julho <-read_excel("julho.xlsx", sheet = 2)
agosto <-read_excel("agosto.xlsx", sheet = 2)
setembro <-read_excel("setembro.xlsx", sheet = 2)
outubro <-read_excel("outubro.xlsx", sheet = 2)
novembro <-read_excel("novembro.xlsx", sheet = 2)
dezembro <-read_excel("dezembro.xlsx", sheet = 2)

# lista para poder ver o dia da semana
dias_da_semana <- c("dom", "seg", "ter", "qua", "qui", "sex", "sáb", "Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Sabado", "segunda-feira", "Terca", "terça-feira", "terca-feira", "quarta-feira", "quinta-feira", "sexta-feira")

# lista de cada mes para pegar os dados necessarios e usar um for ao inves de usar o mesmo codigo para cada mes
meses <- list(janeiro, fevereiro, marco, abril, maio, junho, julho, agosto, setembro, outubro, novembro, dezembro)

# lista para iniciar a data de mes
datas <- c("2022-01-01","2022-02-01","2022-03-01","2022-04-01","2022-05-01","2022-06-01","2022-07-01","2022-08-01","2022-09-01","2022-10-01","2022-11-01","2022-12-01")

# lista de variaveis para poder guardar os dados necessarios de cada mes
variaveis <- c(jan, fev, mar, abr, mai, jun, jul, ago, set, out, nov, dez)

# laco for para pegar os dados de cada mes, para aproveitar o codigo
for(i in 1:12){
  # codigo para pegar apenas os dados necessarios
  variaveis[[i]] <- meses[[i]] %>% 
      mutate(vendas = as.numeric(...5) + as.numeric(Valor), # soma das colunas D e E, para o valor total das vendas no dia
             Data = seq.Date(as.Date(datas[[i]]), by = "day", length.out = n()),    # faz a data ser sequencial pelos dias
             dia = wday(Data, label = FALSE) - 1,    #  verifica o dia da semana
             dia = ifelse(dia == 0, 7, dia),     # caso o dia for 0, então é sabado, e vai sunstituir o 0 pelo 7
             mesDia = format(Data, "%m%d"))%>%   # usado depois para juntar com outra tabela
        filter(...2 %in% dias_da_semana, vendas != 0)%>%    # filtra apenas os dias que tiveram vendas
        select(Data, dia, vendas, mesDia)
}
resultado_final <- bind_rows(variaveis)      # para colocar em uma variavel só todos os dados de cada mes 

# tabela dos dados meteorologicos
temperatura <- tempo %>%
  slice(-c(1:8)) %>%     # ignora as primeiras 8 linhas do arquivo que não serão usados
  mutate(
    data = ymd(as.Date(`REGIAO:`,format = "%d/%m/%y")))%>%     # muda o formato da data 
  group_by(data) %>%   # agrupa os dados por data
  summarise(
         temp = max(as.numeric(...8), na.rm = TRUE),    # pega a maior temperatura do dia
         chuva = sum(as.numeric(...3), na.rm = TRUE),   # soma a quantidade de chuva de cada dia
         radiacao = sum(as.numeric(...7), na.rm = TRUE) / 86400 * 1000,  # soma a radiacao do dia e converte para W/m²
         umidade = sum(as.numeric(...16), na.rm = TRUE)/24)  %>%     # faz a media da umidade
  mutate(mesDia = format(data, "%m%d")) %>%      # usado para juntar com a outra tabela
  select(data ,temp, chuva, radiacao, umidade, mesDia)

resultado <- dplyr::left_join(resultado_final, temperatura, by = "mesDia")%>%   # junta as duas tabelas
  select(Data, vendas, temp, umidade, radiacao, chuva, dia)%>%
  knitr::kable()
print(resultado, n = Inf)     # mostra o resultado final da junção das tabelas