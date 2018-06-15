## ESPM - MBA Big Data Aplicado ao Marketing - 1S2018
## Análise de Redes Sociais e Text Mining - WC 4
## Prof. Gustavo Corrêa Mirapalheta

###########################################################
# 1 - Ajuste de dados com tyditext, tokenizacao, janeaustenr
# 2 - Analise de textos com stopwords, textos de Jane Austen
# 3 - Analise de textos com stopwords, Wells, Bronte x Austen 
###########################################################

###########################################################
###########################################################
#  - Aula 4 - 1a Parte
#  - Ajuste de dados com tidytext 
#  - Tokenizacao. Pacote janeaustenr
###########################################################
###########################################################

###########################################################
#Neste script iniciamos o topico Text Mining
# As tecnicas serao apresentadas tomando por base o formato
# de dados conhecido como "tidy data". 
###########################################################

###########################################################
#As aplicacoes de Text Mining (Mineracao de Texto) sao 
# variadas. Dentre elas podemos destacar:
# - Transcricoes de chamadas, discursos
# - Emails para SACs
# - Visibilidade em medias sociais
# - Relatorios de equipes de atendimento
# - Entrevistas  e pesquisas de opiniao
########################################################
########################################################
#Os topicos que iremos estudar sao:
# - Transformacao de texto no formato tidy. 
#   - Analise de sentimento. 
#   - Estatistica tf-idf (term frequency times 
#                         inverse document frequency). 
#   - Ngramas. 
# - Matrizes de termos e documentos. 
#   - Corpus objects. Modelagem de topicos. 
# - Aplicacoes: 
#   - Analise de tweets em formato tidy.  
#   - Analise de metadados (conexao entre palavras-chave, 
#                           titulo e campos descritivos). 
#   - Determinacao de padroes entre grupos de usuarios.
########################################################
########################################################
#O livro texto e Text Mining with R - Julia Silge &
#                                     David Robinson
########################################################
########################################################

#Texto nao tokenizado
texto <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
View(texto)

#Biblioteca ja carregada no .Rprofile
#library(dplyr)

#A funcao data_frame (pacote dplyr) cria tibbles
text_df <- data_frame(line = 1:4, 
                      text = texto)
View(text_df)

#Biblioteca ja carregada no .Rprofile
#library(tidytext)

#Para tokenizar o objeto text_df, indicamos que a 
# coluna com o texto de entrada em text_df chama-se
# "text" e a coluna com os tokens no dataframe de saida
# devera ser chamada de "word".
# Observe que sera gerado um dataframe com cada palavra
# como token (em uma linha) e o numero de linha no
# dataframe original (text_df)
text_df %>% 
  unnest_tokens(input="text", 
                output="word") -> text_token
View(text_token)

#Sugere-se remover os dataframes criados da memoria antes de 
# prosseguir. No entanto, as linhas sao deixadas em formato
# de comentario, pois isto fica a criterio do usuario
# rm(texto)
# rm(text_df)
# rm(text_token)

#Bibliotecas de analise de texto (ja carregadas no .Rprofile)
#library(janeaustenr)
#library(dplyr)
#library(stringr)

#Observando o formato dos textos de Jane Austen
View(austen_books())

#Agrupando Jane Austen por livro
austen_books() %>% 
  group_by(book) -> austen_grouped 
View(austen_grouped)

#Para confirmar que o dataframe foi agrupado mostre
# o mesmo na console
austen_grouped

#Inserindo os numeros de linha
# Observe o efeito do agrupamento por "book".
# A contagem do linenumber recome?a sempre que
# aparece um novo livro. Ou seja, obtivemos o linenumber
# por livro. 
austen_grouped %>% 
  mutate(linenumber=row_number()) ->
    austen_linenumber
View(austen_linenumber)  
  
#Inserindo os numeros de capitulo 
#
# Observe o mesmo efeito agora com a contagem dos capitulos
# Toda vez que o padrao de texto (definido na funcao regex)
# aparece no campo "text", e gerado um TRUE em str_detect
#
# str_detect ira gerar entao uma sequencia de TRUEs e FALSEs
# esta sequencia passara em seguida por uma soma cumulativa
# a qual na pratica ira indicar o numero do capitulo onde se
# encontra a linha do texto em um novo campo denominado 
# "chapter".
#
# O parametro ignore_case=TRUE foi utilizado para que seja
# ignorada a caixa (se o texto esta em maiusculas ou 
# minusculas) no momento da deteccao.
austen_chapters <- austen_linenumber %>%
  mutate( chapter = cumsum(
    str_detect(
      text, 
      regex("^chapter [\\divxlc]", 
            ignore_case = TRUE) ) ) )
View(austen_chapters)

#O comando View nao indica se o dataframe foi agrupado ou
# nao. Para isto precisamos mostrar o mesmo no terminal do R
austen_chapters

#Vamos explorar a regex. regex significa Regular Expression
# E uma forma de detectar um padrao de texto. Ela deve ser
# utilizada como um dos parametros de str_detect. A regex
# utilizada neste exemplo segue a logica apresenta abaixo:
#
# "^chapter " -> O caracter ^ apresentado antes da regex
# "chapter [\\divxlc]" determina que o padrao deve ser 
# procurado a partir da esquerda.
#
# O texto gra a ser procurado ? "chapter ", seguido de 
# uma sequencia de letras que representam numerais decimais 
# ou numerais romanos mais comuns nos capitulos.
#
# Os colchetes indicam que qualquer numero decimal (0 a 9) ou
# letra no conjunto i,v,x,l,c encontrada depois de "chapter " 
# deve ser utilizado na determinacao da sequencia
#
# A forma de indicar numa regexp que qualquer numero decimal
# 0 a 9 deve ser utilizado para determinar um "match" e a 
# sequencia basica "\d". 
# 
# No entanto o caracter "\" dentro de uma string e um caracter 
# de "escape". Ele manda ignorar o comportamento padrao do 
# caracter seguinte (isto e escapa-lo). Como o caracter seguinte
# em "\d" e um "d", "\d" mandaria escapar o "d", isto e ignorar
# sua caracteristica basica de ser uma string.
#
# No entanto, queremos que seja utilizado na regexp "\d" 
# portanto quem tem que ser escapado e o "\". Sendo assim 
# precisamos "\\d". "\\" ira escapar o carater de controle de 
# "\" considerando-o uma string. Em seguida vem o "d". Desta
# forma "\\d" ira indicar para a regexp que estamos passando 
# "\d", o qual este e o caracter de controle desejado. 
#
# O "\d" em si quer dizer qualquer numero (no padrao decimal,
# isto e 0,1,2,3,4,5,6,7,8 ou 9)
#
# A seguir vem "ivxlc" os quais sao os numerais romanos mais
# comuns na determinacao de capitulos. Desta forma, qualquer
# string que comece pela palavra "chapter " seguida de um 
# numero decimal (0 a 9) ou das letras i,v,x,l ou c sera 
# um match e tera como resultado TRUE.
#
# Observe os exemplos a seguir (todos em caixa baixa, isto e
# minusculas para evitar o uso de ignore_case=TRUE, e se 
# concentrar na deteccao da regexp em si):

str_detect("chapter i", regex("^chapter [ivxlc]")) #TRUE

str_detect("chapter v", regex("^chapter [ivxlc]")) #TRUE

str_detect("chapter j", regex("^chapter [ivxlc]")) #FALSE

str_detect("chapter x_retorno", 
            regex("^chapter [ivxlc]")) #TRUE

str_detect("chapter 3", regex("^chapter [ivxlc]")) #FALSE

str_detect("chapter 3", regex("^chapter [\divxlc]")) #ERRO

str_detect("chapter 3", regex("^chapter [\\divxlc]")) #TRUE

#Por ultimo desagrupamos o dataframe (neste caso um tibble)
# por?m agora temos mesmo em um dataframe nao agrupado, campos
# que sao capazes de indicar o numero da linha e o capitulo por
# livro a que se refere a string no campo "text"
austen_chapters %>% ungroup() ->
  austen_chapters_ungrouped

#Como indicado anteriormente, a forma de verificar se o 
# dataframe foi agrupado ou nao ? apresenta-lo no terminal do R
austen_chapters_ungrouped

#Vamos agora tokenizar o dataframe no formato uma palavra por
# linha. Para isto precisamos da biblioteca tidytext
# Ela j? foi carregada no .Rprofile
#library(tidytext)

#Nela a funcao que executa a tokenizacao por palavra chama-se:
# unnest_tokens. Vamos detalhar as opcoes da mesma:
#Primeiro ? necess?rio passar o dataframe, no caso 
# austen_chapters_ungrouped
#Depois precisamos indicar qual o tipo de "token" no qual o 
# texto sera quebrado. Neste caso palavras, isto ? "words". 
# Isto ? feito pela opcao token. 
#Em seguida precisamos indicar qual coluna no dataframe tem o
# texto a ser tokenizado. Neste caso a coluna chama-se "text".
# Este nome de coluna ? passado pela opcao "input=".
#to_lower=TRUE indica que o texto deve ter todas as letras
# maiusculas trocadas para minusculas antes da tokenizacao.
#Depois precisamos indicar o nome da nova coluna na qual o 
# texto tokenizado dever? ser gravado. Neste caso a coluna
# sera chamada de "word" e o seu nome ? passado pela opcao
# "output="
#drop=TRUE indica que a coluna com o texto original (neste
# caso "text") nao precisar? ser gravada no dataframe 
# resultante.
#Por ultimo observe no resultado que linhas vazias (isto ?
# sem texto sao ignoradas), pois nao h? o que ser tokenizado
# nelas, al?m de serem limpos quaisquer caracteres "nao texto".
# Isto pode ser percebido logo na linha 5, quando no dataframe
# original aparecia "(1811)" (em austen_chapters_ungrouped) e 
# no dataframe tokenizado aparece apenas "1811" (em 
# "austen_token")
unnest_tokens(austen_chapters_ungrouped,
              token="words",
              input=text,
              to_lower=TRUE,
              output=word,
              drop=TRUE) -> austen_token
View(austen_token)

#Diz-se que agora o texto esta "arrumado" isto ? no formato
# "tidy" podendo ser analisado pelas funcoes do pacote dplyr,
# tal como poder? ser visto no proximo script. 

#Para prosseguir primeiro salvamos o objeto austen_token no 
# subdiret?rio "dados", lembrando sempre que estamos 
# trabalhando no subdiret?rio "scripts". Podemos salvar o
# dataframe em formato .csv fazendo:
write.csv2(austen_token, "../dados/austen_token.csv",
           row.names = FALSE)
#OBS: Precisamos da opcao row.names=FALSE, pois caso contr?rio
# sera incluida uma coluna de nome "X" com o nome da linha o
# qual neste caso seria um numero.

#OBSII: Quando for feita a leitura, com read.csv2, devemos 
# lembrar de incluir a opcao stringsAsFactors=FALSE, caso
# contr?rio as operacoes de tokenizacao nao irao funcionar.

#Ou em formato bin?rio fazendo:
#save(austen_token, file="../dados/austen_token")
# O formato bin?rio poupa espaço em relacao ao .csv por?m
# ? menos utilizado, pois s? pode ser lido pelo R.
# Nao sera utilizado aqui, mas ? mencionado nos coment?rios
# apenas como refer?ncia.

#Por ultimo removemos os dataframes intermedi?rios da mem?ria.
# A remocao ou nao fica a crit?rio do usu?rio, por isso os
# comandos foram comentados
#rm(austen_grouped)
#rm(austen_linenumber)
#rm(austen_chapters)
#rm(austen_chapters_ungrouped)

###########################################################
###########################################################
# - Aula 4 - 2a Parte
# - Analise de Texto com stopwords 
# - Textos de Jane Austen
###########################################################
###########################################################

#Bibliotecas j? carregadas no .Rprofile:
#library(tidytext)
#library(dplyr)
#library(janeaustenr)
#library(stringr)

#Carregamento do dataframe:
# Caso queira-se ler a partir do .csv fazemos:
read.csv2("../dados/austen_token.csv",
          stringsAsFactors = FALSE)->austen_token
#OU
# Caso queira-se ler a partir do objeto bin?rio fazemos:
#load("../dados/austen_token")

#Para revisao, visualizamos novamente o dataframe:
View(austen_token)

#Existe no entanto um problema, devido a forma como os it?licos
# sao gravados. Eles utilizam o caracter de sublinhado o qual 
# permanece, mesmo depois da operacao de unnest_tokens. Isto pode
# ser visto com View(austen_token) e em seguida pedindo para mostrar
# o caracter "_". Sendo assim, por seguran?a vamos extrair da coluna 
# "word" todos os espaaos no final de cada palavra al?m do caracter 
# "_" o qual ? utilizado nos textos do projeto Gutemberg para 
# descrever um campo em it?lico. Os textos de Jane Austen que estamos
# utilizando foram obtidos do site do projeto Gutemberg, assim como
# os textos que utilizaremos na sequencia deste script.

# Vamos no entanto realizar alguns testes com str_view para 
# determinar a regex necess?ria
texto <- c("Because i could not stop for Death -",
           "he kindly stopped for me -",
           "the carriage held but just Ourselves -",
           "and Immortality")

#Detecta qualquer caracter no conjunto a at? z, apenas minusculas 
# uma ?nica vez por string 
str_view(texto, "[a-z]") #Selecionar? "e", "h", "t" e "a"

texto <- c("'Because i could not stop for Death -",
           "he kindly stopped for me -",
           "the carriage held but just Ourselves -",
           "and Immortality")
#Inclui na deteccao o caracter especial ' (ap?strofo), necess?rio
# na lingua inglesa
str_view(texto, "[a-z']") #Selecionar? "'", "h", "t" e "a"

#Selecionar? qualquer sequencia de caracteres dentro dos especificados
# at? encontrar um caracter que nao esteja na sequencia (um "B"
# mai?sculo ou um espaço por exemplo)
str_view(texto, "[a-z']+") #Selecionar? "'", "he", "the" e "and"

texto <- c("_Because i could not stop for Death -",
           "_he_ kindly stopped for me -",
           "_the carriage held but just Ourselves -",
           "and_ Immortality")
str_view(texto, "[a-z']+") #Selecionar? "'", "he", "the" e "and"

#Na pratica "[a-z']+" permite eliminar os espacos no final das 
# palavras caso tenhamos uma palavra por registro em um dataframe
# al?m de retirar caracteres como "_" os quais sao usados para 
# definir it?licos nos textos do projeto Gutenberg.

# Sendo assim, para eliminar o caracter "_" do dataframe 
# usamos a regexp [a-z']+ a qual procura qualquer combinacao dos
# caracteres de a at? z al?m do ap?strofo. Para isso fazemos:
mutate(austen_token, 
       word = str_extract(word,"[a-z']+")) -> austen_token

#Vamos primeiro olhar o dataframe com as "stopwords",
# isto ? palavras que representam preposicoes, artigos, etc...
# as quais serao retiradas do dataframe austen_token para
# Analise das ideias do texto. Este dataframe ? carregado com 
# a biblioteca tidytext.
View(stop_words)

#Se queremos retirar as stopwords do nosso dataframe
# austen_token, precisamos na verdade de uma operacao de 
# anti join, a qual podemos executar, pois temos em ambos
# o campo word, fazendo:
anti_join(austen_token,stop_words, 
          by=c("word"="word")) -> austen_words
View(austen_words)

#Podemos agora contar as palavras e determinar quais sao as
# mais utilizadas atrav?s de um count com um sort=TRUE.
# Lembre-se que o comando count gera um dataframe com duas
# colunas, a dos elementos a serem contados (neste caso a
# coluna word) e a da contagem (neste caso a coluna n)
count(austen_words, word, sort=TRUE) -> 
  austen_count
View(austen_count)

#Vamos filtrar o dataset e ficar com apenas as palavras
# que aparecem mais de 600 vezes em Jane Austen
filter(austen_count, n>600) -> austen600
View(austen600)

#Primeiro tentamos um grafico de colunas. Listamos as palavras
# da menos at? a mais mencionada, com a contagem no eixo dos x
# Por?m o grafico nao fica muito bom (vide a seguir)
ggplot(austen600, aes(x=n, y=word)) + 
  geom_col() + xlab(NULL)

#Vamos inverter os eixos. No eixo horizontal colocamos a palavra
# e no vertical a contagem da mesma em Jane Austen. Observe que
# as palavras ficaram listadas em ordem alfab?tica.
ggplot(austen600, aes(x=word, y=n)) + 
  geom_col() + xlab(NULL)

#Queremos no entanto que as palavras sejam ordenadas no eixo 
# dos x de acordo com seu valor no eixo dos y (pela contagem).
# Para isto precisamos reordenar o dataframe. A funcao reorder 
# (a qual pertence ao R gra) faz o seguinte: ela considera 
# seu primeiro argumento como uma vari?vel categ?rica, e 
# reordena seus valores tomando por base os valores da segunda 
# vari?vel, geralmente uma vari?vel num?rica.
mutate(austen600, word = reorder(word,n)) -> austen_reorder
View(austen_reorder)

#A reordenacao no entanto s? ? percebida quando pedimos
# para ser feito um grafico
ggplot(austen_reorder, aes(x=word, y=n)) + 
  geom_col() + xlab(NULL)

#Por ultimo pedimos para "flipar" os eixos de forma a tornar
# o grafico mais f?cil de ser lido
ggplot(austen_reorder, aes(x=word, y=n)) + 
  geom_col() + xlab(NULL) + coord_flip()

#Para prosseguir salvamos o dataframe austen_count, pois
# iremos utilizar o mesmo na Analise comparativa no proximo
# script
write.csv2(austen_count,"../dados/austen_count.csv",
           row.names = FALSE)
#OU
#save(austen_count,file="../dados/austen_count")

#E eliminamos os dataframes que nao iremos utilizar
# A eliminacao fica a crit?rio do usu?rio
#rm(austen_reorder)
#rm(austen_token)
#rm(austen_words)
#rm(austen600)

###########################################################
###########################################################
# - Aula 4 - 3a Parte
# - Analise de Texto com stopwords 
# - Comparacao Wells, Bronte x Austen
###########################################################
###########################################################

#Bibliotecas j? carregadas no .Rprofile
#library(tidyverse)
#library(tidytext)
#library(dplyr)
#library(janeaustenr)
#library(stringr)
#library(tidyr)

#Carregamento do dataframe:
# Caso queira-se ler a partir do .csv fazemos:
read.csv2("../dados/austen_count.csv",
          stringsAsFactors = FALSE)->austen_count
#OU
# Caso queira-se ler a partir do objeto bin?rio fazemos:
#load("../dados/austen_count")

#Para revisao, visualizamos novamente o dataframe:
View(austen_count)

#Vamos fazer agora uma Analise similar com obras de outros
# dois autores, H.G.Wells e as irmoes Bronte. As obras destes
# autores podem ser baixadas do projeto Gutenberg.
# A biblioteca j? foi carregada pelo .Rprofile
#library(gutenbergr)

#Download direto da internet (projeto Gutenberg)
# Utilize esta opcao caso a carga dos .csv armazenados
# no diret?rio /dados comece a apresentar erros inesperados
# durante a Analise
#hgwells <- gutenberg_download(c(35, 36, 5230, 159))

#Salvamento em csv, padrao decimal europeu continental
#write.csv2(hgwells,"../dados/hgwells.csv", 
#                   row.names = FALSE)

#Leitura de csv, padrao europeu continental, via tidyverse
# Nao requer stringsAsFactors = FALSE.
#read_csv2("../dados/hgwells.csv") -> hgwells

#Leitura de csv, padrao europeu continental, via R padrao
# Observe que o parametro stringsAsFactors = FALSE ? ESSENCIAL!!!
read.csv2("../dados/hgwells.csv",
          stringsAsFactors = FALSE) ->hgwells
View(hgwells)

#bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
#write.csv2(bronte,"../dados/bronte.csv", row.names = FALSE)
read.csv2("../dados/bronte.csv",
          stringsAsFactors = FALSE)->bronte
View(bronte)

#V?rias das opcoes apresentadas em unnest_tokens abaixo
# sao defaults.
#
# input= se nao for mencionado considera a coluna de nome 
# text no dataframe de entrada, neste caso hgwells
#
# output= se nao for mencionado sup?e que um nome de coluna
# nao presente no dataframe de entrada sera utilizado como 
# a coluna com o registro das palavras no dataframe de saida
#
# to_lower = TRUE ? default, nao precisa ser incluido. Ele
# garante que todas as palavras passarao para caixa baixa,
# isto ? minusculas. 
#
# drop = TRUE ? default tamb?m. Indica que a coluna com o 
# texto no dataframe de entrada deve ser omitida no dataframe
# de saida.
#
# no caso do anti_join, foram explicitamente indicadas as 
# chaves de conexao entre as tabelas atrav?s de :
# by=c("word"="word"), por?m se os dataframes que deverao
# passar pela operacao de conexao tem apenas este campo
# com nome comum, eles serao usados por default tamb?m.
hgwells %>% unnest_tokens(input=text,
                          output=word,
                          token="words",
                          to_lower = TRUE,
                          drop = TRUE) %>%
  mutate(word=str_extract(word,"[a-z']+")) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  count(word, sort=TRUE) -> hgwells_count
View(hgwells_count)

# Nesta Analise nao precisaremos mais do dataframe hgwells
# portanto podemos remove-lo, a crit?rio do usu?rio
# rm(hgwells)

bronte %>% unnest_tokens(input=text,
                         output=word,
                         token="words",
                         to_lower = TRUE,
                         drop = TRUE) %>%
  mutate(word=str_extract(word,"[a-z']+")) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  count(word, sort=TRUE) -> bronte_count
View(bronte_count)
# Idem anterior. Como nao precisaremos do dataframe bronte
# iremos remover o mesmo, a crit?rio do usu?rio.
# rm(bronte)

#Vamos agora criar um dataframe no qual iremos calcular a 
# proporcao de vezes que cada palavra foi utilizada no total 
# de palavras utilizadas por cada autor. Observe a sequencia
# de operacoes

#Primeiro vamos incluir uma coluna de nome "author" em cada
# um dos dataframes "_count".
mutate(austen_count, author="Jane Austen")-> austen_count
View(austen_count)

mutate(bronte_count, author="Bronte Sisters")-> bronte_count
View(bronte_count)

mutate(hgwells_count, author="H.G.Wells")-> hgwells_count
View(hgwells_count)

#Em seguida vamos condensar os tr?s dataframes _count
# em um ?nico dataframe denominado autores. A ligacao sera 
# feita por registro, ou seja, vamos "apendar" um dataframe 
# no outro formando um dataframe ?nico.
bind_rows(austen_count,bronte_count,hgwells_count) -> 
  autores_count
View(autores_count)

#Agrupando por autor
group_by(autores_count, author) -> 
  autores_grouped
View(autores_grouped)
autores_grouped

#Calculando a proporcao de cada palavra em cada autor
mutate(autores_grouped, proportion = n/sum(n)) -> 
  autores_proportion
View(autores_proportion)

#Eliminando a coluna "n"
select(autores_proportion,-n) -> autores_proportion2
View(autores_proportion2)

#A operacao de spread ira criar tr?s colunas com as proporcoes
# de cada palavra, uma para cada um dos tr?s autores. Caso a palavra
# nao tenha sido usada pelo autor, sera gerado um NA
spread(autores_proportion2, author,proportion) -> 
  autores_spread
View(autores_spread)

#Vamos agora condensar em duas colunas as proporcoes de cada palavra
# tal como utilizadas em Bronte e H.G.Wells. Isto criar? uma coluna
# chamada "Outros", a qual ter? o nome do autor cuja coluna sofreu
# uma operacao de gather (Bronte Sisters ou H.G.Wells) e a proporcao
# associada. Note que isto vai gerar o dobro de palavras. Por exemplo 
# a palavra "aback" ira aparecer em duas linhas, uma com a proporcao
# em Jane Austen e a proporcao em Bronte e outra com a proporcao 
# em Jane Austen novamente e a proporcao em H.G.Wells.
gather(autores_spread, Outros, proportion,
       "Bronte Sisters":"H.G.Wells") -> 
  autores_gather
View(autores_gather)

#Vamos gerar um grafico de dispersao da proporcao de palavras
# em Bronte e Wells x Jane Austen. Observe que no caso do campo
# denominado "Jane Austen" ele deve ser indicado com aspas simples
# invertidas, `Jane Austen` para que seja entendido como nome
# de campo. Vamos montar o grafico passo a passo.

#Este ? o grafico mais gra e foi feito apenas para o par
# Jane Austen e Bronte.
ggplot(filter(autores_gather, Outros=="Bronte Sisters"), 
        aes(x=proportion, y=`Jane Austen`)) + geom_point()

#Idem para o par Jane Austen e H.G.Wells
ggplot(filter(autores_gather, Outros=="H.G.Wells"), 
       aes(x=proportion, y=`Jane Austen`)) + geom_point()

#Para combinar em um mesmo grafico as duas dispersoes lancamos mao
# do recurso de facet_wrap, usando como parametro o valor do campo
# Outros
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + geom_point() +
  facet_wrap(~ Outros)

#Como os pontos estao concentrados na parte inicial das proporcoes
# vamos tentar linearizar os gráficos utilizando escalas logaritmicas
# tanto no eixo dos x quanto no eixo dos y
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + geom_point() +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros)

#Vamos agora incluir linhas de tendencia nos gráficos com abline
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + geom_point() +
  geom_abline() +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) 

#Podemos adaptar a linha de tendencia alterando sua cor para
# gray40, o que torna sua visualizacao mais facil
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + geom_point() +
  geom_abline(color = "gray40") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) 

#geom_abline traca uma reta de regressao entre os pontos.
# lty quer dizer "linetype" e e o tipo de linha que sera usado 
# para tracar a reta de regressao. Do help do ggplot2 encontramos
# que o tipo de linha pode ser especificado por nomes:
# ("blank", "solid", "dashed", "dotted", "dotdash", "longdash", 
#  "twodash") ou numeros (0, 1, 2, 3, 4, 5, 6). Vamos adotar o 
# lty=2, isto e o tracejado (dashed)
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + geom_point() +
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) 

#Vamos agora retirar o label do eixo dos x para aumentar a 
# area ?til do grafico.
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + geom_point() +
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) +
  labs(y = "Jane Austen", x = NULL)

#Em seguida vamos trocar o tipo gra de dados pois queremos ter
# uma ideia de quais palavras tem alta correlacao entre diferentes
# autores (isto é estao proximas da reta abline). Sendo assim vamos
# trocar geom_point por geom_text, sendo o mapeamento estético do 
# texto (seu "label") dado pelo valor do campo "word".
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + 
  geom_text(aes(label=word)) +
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) +
  labs(y = "Jane Austen", x = NULL)

#Para poder ver as palavras vamos evitar que ocorra sobreposicao de
# escrita nas palavras usando a opcao check_overlap = TRUE
# geom_text(aes(label=word)) escreve o texto associado ao ponto 
# no grafico. check_overlap = TRUE, nao desenha texto que sobreponha 
# textos anteriores no mesmo layer. Isto evita que os graficos virem
# um "borrao" espalhado sobre a linha de abline. 
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + 
  geom_text(aes(label=word), check_overlap = TRUE) +
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) +
  labs(y = "Jane Austen", x = NULL)

#Podemos também ajustar a justificacao vertical com vjust=1.5
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + 
  geom_text(aes(label=word), check_overlap = TRUE, vjust = 1.5) +
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) +
  labs(y = "Jane Austen", x = NULL)

#Vamos agora reintroduzir um certo ruido nos dados de forma a produzir
# novamente um borrao, mas um borrao alinhado com a reta abline
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`)) + 
  geom_text(aes(label=word), check_overlap = TRUE, vjust = 1.5) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +  
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) +
  labs(y = "Jane Austen", x = NULL)

#Em seguida vamos suavizar o borrao, fazendo a cor dos pontos ser
# dependente do quao proximo eles estao da abline.
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`,
           color = abs(`Jane Austen`-proportion))) + 
  geom_text(aes(label=word), check_overlap = TRUE, vjust = 1.5) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +  
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros) +
  labs(y = "Jane Austen", x = NULL)

#Depois para aumentar a área útil do grafico novamente vamos
# retirar a legenda
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`,
           color = abs(`Jane Austen`-proportion))) + 
  geom_text(aes(label=word), check_overlap = TRUE, vjust = 1.5) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +  
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  facet_wrap(~ Outros, ncol=2) +
  labs(y = "Jane Austen", x = NULL) +
  theme(legend.position="none")

#E por ultimo alterar o gradiente de cores e introduzir por 
# compatibilidade com o livro texto a opcao ncol=2 em facet_wrap
ggplot(autores_gather, 
       aes(x=proportion, y=`Jane Austen`,
           color = abs(`Jane Austen`-proportion))) + 
  geom_text(aes(label=word), check_overlap = TRUE, vjust = 1.5) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +  
  geom_abline(color = "gray40", lty=2) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +  
  facet_wrap(~ Outros, ncol = 2) +
  labs(y = "Jane Austen", x = NULL) +
  theme(legend.position="none")

#Vamos calcular agora a correlacao entre os autores. Primeiro
# entre Jane Austen e Bronte
# Os conjuntos para calcular a correlacao sao passados pelo
# parametro ~ proportion + `Jane Austen`. Isto ira calcular a
# correlacao entre as colunas proportion e `Jane Austen` do
# dataframe autores_gather.
# Observe que o dataframe foi filtrado para permanecer apenas
# com o autor "Bronte Sisters", logo a correlacao entre 
# proportion e `Jane Austen` sera a correlacao entre 
# "Bronte Sisters" e `Jane Austen`
cor.test(data = 
           autores_gather[
             autores_gather$Outros == "Bronte Sisters",], 
         ~ proportion + `Jane Austen`)

#E entre Jane Austen e H.G.Wells
# A mesma logica se aplica aqui para um dataframe que tenha
# sido filtrado para "H.G.Wells".
cor.test(data = 
           autores_gather[
             autores_gather$Outros == "H.G.Wells",], 
         ~ proportion + `Jane Austen`)
