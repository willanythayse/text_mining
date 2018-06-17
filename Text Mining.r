##################### PROJETO TEX MINING #####################

##################### STOPWORDS #####################
library(tidyverse)
library(data.table)
library(tidytext)
library(glue)
library(stringr)
library(stringi)
library(rvest)
library(ptstem)
library(wordcloud2)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

#Dicionário de Entrada
stopwordsArq <- read.csv(file.choose(), head=F, encoding = "UTF-8", sep="\t")
stopwordsArq_vet <- factor(stopwordsArq$V1)

#transformaando em um array de dicionário
stpW <- unlist(str_split(stopwordsArq_vet, '\\n'))
glimpse(stpW)
View(stpW)

#carregando o stopwords da tm
stpW2 <- stopwords('portuguese')
glimpse(stpW2)

#fazendo um merge
str(stpW)

#Unindo os dois dicionários, entrada(file) e tm
stpW_merged <- union(stpW,stpW2) 
summary(stpW_merged)

#Validando o merge para não haver duplicidade
tibble(word = stpW_merged) %>% 
  group_by(word) %>% 
  filter(n()>1)

#Removendo arquivos desnecessários...
rm(stopwordsArq)
rm(stopwordsArq_vet)
rm(stpW)
rm(stpW2)


##################### PALAVRAS DE SENTIMENTOS #####################
#Carregando arquivo com palavras de sentimento https://www.kaggle.com/rtatman/sentiment-lexicons-for-81-languages/data
posW <- read.csv(file.choose(), header = F, sep = "\t", strip.white = F, 
                 stringsAsFactors = F, encoding="UTF-8")
View(posW)
negW <- read.csv(file.choose(), header = F, sep = "\t", strip.white = F, 
                 stringsAsFactors = F, encoding="UTF-8")
View(negW)

##Criando um dataframe que salvarei os termos começando pelos adjetivos negativos
dfPolaridades <- negW %>% 
  mutate(word = V1, polaridade = -1, tipo='adjetivo', sentimento='negativo') %>%
  select(word,polaridade,tipo,sentimento) %>%
  arrange(word)
head(dfPolaridades,2)


###aqui faço um count para poder adicionar os dados corretamente
#icount <-  length(exn$V1)
#dfPolaridades <- bind_rows(dfPolaridades,list(word = exn$V1, polaridade=rep(-1,icount),tipo=rep('expressao',icount),sentimento=rep('negativo',icount)))
#dfPolaridades %>% arrange(desc(word)) %>% head(3)

icount <-  length(posW$V1)
dfPolaridades <- bind_rows(dfPolaridades,list(word = posW$V1, polaridade=rep(1,icount),tipo=rep('noclass',icount),sentimento=rep('positivo',icount)))


#visualizando como está nosso dataframe
dfPolaridades %>% group_by(word) %>% filter(n() == 1) %>% summarize(n=n())

#Fazendo um contador na quantidade total de adjetivos negativos
dfPolaridades %>% count()


#Removendo adjetivo repetidos
dfPolaridadesUnique <- dfPolaridades[!duplicated(dfPolaridades$word),]
    dfPolaridadesUnique %>% count()

rm(negW)
rm(posW)
rm(dfPolaridades)


##################### ATIVIDADE 1 - Análise de Sentimentos #####################
### Análise de sentimento Dom Casmurro ###
library(reshape2)
library(dplyr)
library(tidyverse)
library(stringr)
library(tm)
library(tidytext)
library(data.table)
library(ggplot2)
library(stringr)

### Carregando Arquivos
read_dom <- read.csv(file.choose(), header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding = "ANSI")
View(read_dom)
read_dom_vet <- c(read_dom$V1)

### Criando um dataframe
dom_df <- data_frame(line = 1:1907, text = read_dom_vet) 

glimpse(dom_df)
View(read_dom_vet)
rm(read_dom)

### Tokenizando o arquivo, para isso deve estar em vetor
tidy_books <- dom_df %>%
  unnest_tokens(word, text)
tidy_books

### Retirando STOPWORDS
tidy_books <- tidy_books %>%
  anti_join(stpW_merged)

### Quantidade de palavras  
tidy_books %>%
  count(word, sort = TRUE) 

### Gráfico barras, quantidade de palavras
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 700  ) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

### Especificando por Caítulos
tidy_books <- dom_df %>%
  group_by(text) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^CAPÍTULO", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

### Paravras de Sentimentos
neg_joy <- dfPolaridadesUnique %>% 
  filter(sentimento == "negativo")
pos_joy <- dfPolaridadesUnique %>% 
  filter(sentimento == "positivo")
tidy_books_n <- tidy_books %>%
  inner_join(pos_joy) %>%
  count(word, sort = TRUE)

### Analisando os sentimentos a cada 80 linhas 
dom_sentiment <- tidy_books %>%
  inner_join(dfPolaridadesUnique) %>%
  count(word, index = linenumber %/% 80, sentimento) %>%
  spread(sentimento, n, fill = 0) %>%
  mutate(sentimento = positivo - negativo)

### Grádico 
#ggplot(dom_sentiment, aes(index, sentimento, fill = word)) +
#  geom_col(show.legend = FALSE) +
#  facet_wrap(~word, ncol = 2, scales = "free_x")


bing_word_counts <- tidy_books %>%
  inner_join(dfPolaridadesUnique) %>%
  count(word, sentimento, sort = TRUE) %>%
  ungroup()


bing_word_counts %>%
  group_by(sentimento) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentimento)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentimento, scales = "free_y") +
  labs(y = "Contribuição to sentimento",
       x = NULL) +
  coord_flip()



tidy_books %>%
  inner_join(dfPolaridadesUnique) %>%
  count(word, sentimento, sort = TRUE) %>%
  acast(word ~ sentimento, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
				   

##################### ATIVIDADE 2 #####################
### Lei de Zipf ###
library(reshape2)
library(dplyr)
library(tidyverse)
library(stringr)
library(tm)
library(tidytext)
library(data.table)
library(ggplot2)
library(stringr)

### Carregando Arquivos ###

###O Arquipélado ###
#read_tv_arq <- read.csv(file.choose(), header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding = "ANSI")
read_tv_arq <- pdf_text("C:/Users/Willany Thayse/Documents/Workana/Text mining em R/Atividades/Atividade 1/O Tempo e o Vento  - O Arquilpe - Erico Verissimo.pdf")
OArquipelogo_sep <- strsplit(read_tv_arq, "\\W")
View(read_tv_arq)

###O Retrato ###
read_tv_ret<- read.csv(file.choose(), header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding = "ANSI")
OArquipelogo_sep <- strsplit(read_tv_arq, "\\W")
View(read_tv_ret)

### Criando um dataframe
tv_arq_df <- data_frame(line = 1:307, text = read_tv_arq_vet) 
glimpse(tv_arq_df)
rm(read_tv_arq)
tv_ret_df <- data_frame(line = 1:1907, text = read_tv_ret_vet) 
glimpse(tv_ret_df)
rm(read_tv_ret)


### Tokenizando o arquivo, para isso deve estar em vetor
tv_arq_tk <- tv_arq_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(book = "O Aqruipélado") %>%
  ungroup()
tv_arq_tk

tv_ret_tk <- tv_ret_df 
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(book = "O Retrato") %>%
  ungroup()
tv_ret_tk

#Sumarizando 
total_tv_arq_tk <- tv_arq_tk %>%
  summarize(total = sum(n))
 
total_tv_ret_tk <- tv_ret_tk %>%
  summarize(total = sum(n))



book_words
### Criando frequencia de palavras
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank