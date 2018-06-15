## ESPM - MBA Big Data Aplicado ao Marketing - 1S2018
## Análise de Redes Sociais e Text Mining - WC 6
## Prof. Gustavo Corrêa Mirapalheta

######################################################
# 6.1 - Ajuste de dados com tidyr
# 6.2 - Manipulação de strings com stringr e detecção 
#      de padrões com regexps
######################################################

######################################################
######################################################
# - Aula 06 - 1a Parte
# - Ajuste de dados 
# - com tidyr
######################################################
######################################################

######################################################
#Dados tidy: uma observação por linha,
#            uma variável por coluna
#            um valor por célula
######################################################
library(tidyr)

#Este data set está "tidy"
View(table1)

#Este tem variáveis nos valores de uma coluna
View(table2)

#Vamos ajusta-lo por uma operação de spread
spread(table2, key=type, value=count) -> table2a
View(table2a)

#Este tem valores diferentes em uma mesma coluna
View(table3)

#Vamos ajusta-lo por uma operação de separate
separate(table3, rate, 
         into = c("cases","population"), sep="/") -> table3a
View(table3a)

#Este tem valores similares em duas colunas
View(table5)

#Vamos ajusta-lo pela operação de unite
unite(table5, new, century, year) -> table5a
View(table5a)

unite(table5, new, century, year, sep="") -> table5b
View(table5b)

#Estes tem a mesma variável dividida em duas colunas
#O nome da variável é "year", porém o significado é distinto
# por dataframe
View(table4a)
View(table4b)

#Vamos ajustar cada um deles por uma operação de gather
gather(table4a, "1999", "2000", 
       key="year", value="cases") -> table4a2
View(table4a2)

gather(table4b, "1999", "2000", 
       key="year", value="population") -> table4b2
View(table4b2)

#Vamos agora unir os dataframes por um left_join
left_join(table4a2, table4b2,  
          "country"="country", "year"="year") -> table5
View(table5)

#Wickham, Hadley R for Data Science, O'Reilly, 2016, cap.9
#pags 151, 156, 160, 163, 168
# OU
#Arquivo: 2.3-tidyr.docx

###########################################################
###########################################################
# - Aula 06 - 2a Parte
# - Manipulação de strings com stringr 
# - e detecção de padrões com regexps
###########################################################
###########################################################

#Bibliotecas já carregadas no script anterior:
library(tidytext)
library(dplyr)
library(stringr)

#Vamos agora introduzir o assunto regexps de maneira mais 
# extensa. Como pode ser visto, determinar um padrão a ser 
# localizado em um texto é essencial para qualquer análise 
# do tipo Text Mining.

#Vamos ver agora exemplos de manipulação de strings e seus
# caracteres especiais. As strings podem ser inseridas tanto
# com aspas duplas quanto simples.
string1 <- "This is a string"
string1

string1 <- 'This is a string'
string1

#Inserção de aspas em uma string. Se a string for definida
# com aspas simples coloque aspas duplas e vice-versa
string2 <- 'Para colocar "entre aspas" dentro de uma string, use aspas simples'

#No terminal aparecem os caracteres de escape (as "\") 
string2

#No View() não
View(string2)

#Caso queira inserir aspas duplas como caracter dentro de uma
# string definida também com aspas duplas, as aspas duplas do
# tipo caracter deverão ser "escapadas". Observe a seguir:
string3 <- "Esta string tem \"esta palavra\" entre aspas"

string3

View(string3)

#O mesmo critério se aplica a uma string definida com aspas
# simples na qual se deseja inserir uma aspa simples como
# caracter
string4 <- 'Esta string tem \'esta palavra\' entre aspas'

string4

View(string4)

#De maneira similar para inserir uma barra invertida, a qual
# normalmente é um caracter de controle, precisamos escapa-la.
string5 <- "Esta string tem aqui \\ uma barra invertida"

string5

View(string5)

#Para apresentar no terminal do R apenas as strings sem os 
# seus caracteres de controle podemos utilizar a função
# writeLines
writeLines(string1)
writeLines(string2)
writeLines(string3)
writeLines(string4)
writeLines(string5)

#A lista de caracteres de controle pode ser obtida 
# utilizando-se ?"'" ou ?'"'
?"'"

#Por exemplo para apresentar uma string "quebrada" em duas
# linhas podemos fazer:
nova_linha <- "Em uma linha \n Na outra linha"
writeLines(nova_linha)

#Ou para inserir uma tabulação:
tabulacao <- "Texto antes \t Texto depois da tabulação"
writeLines(tabulacao)

#Vamos agora conhecer algumas funções do pacote stringr

#Para obter comprimentos de string
str_length(c("a", "Mineração de Texto", NA))

#Para combinar strings
str_c("x", "y", "z")

#Para combinar strings com um separador especial
str_c("x", "y", "z", sep=",")

#Observe o efeito de NA nas strings a seguir:
x <- c("abc", NA)
str_c("|-", x, "-|") #Não é executada a operação de 
# concatenação no 2o elemento de
# x pois o mesmo é um NA

#Para tratar NA como string use str_replace_na():
str_c("|-", str_replace_na(x), "-|")

#Para unir vetores de strings em uma única string
# utilize collapse = ","
str_c(c("x", "y", "z")) #Continua-se com 3 strings
str_c(c("x", "y", "z"), collapse=",") #Uma única string
#com os elementos separados por ,

#Para extrair partes de uma string use str_sub()
x <- c("Maçã", "Banana", "Pêssego")
str_sub(x, 1, 3)

#Observe uma forma de tornar as primeiras letras de cada
# uma das strings de x em letras minúsculas:
str_sub(x,1,1) -> x2 #Produz as primeiras letras das strings
x2

str_to_lower(x2) -> x3 #Transforma os caracteres de x2 em 
# letras minúsculas
x3

str_sub(x,1,1) <- x3 #Substitui o 1o caracter de cada string
# do vetor x pelo caracter em x3

#####################################################
#Regexps
#####################################################

#Para determinar onde um padrão aparece em uma string
# utilize st_view()
x <- c("Maçã", "Banana", "Pêssego")
str_view(x, "an")

#Para detectar padrões com coringas (similar ao *) da
# linha de comando do Windows, usamos o "."
str_view(x, ".a.")

#Para determinar o próprio "." como uma string ele
# precisa ser escapado. Portanto precisamos de "\.".
# Porém para que a barra seja usada em combinação com 
# o "." e não escapada, ela própria precisa ser escapada
# portanto a sequência a ser utilizada é "\\.". Observe
# as diferenças a seguir:

#Este detecta qualquer caracter entre a e c
str_view(c("abc", "a.c", "bef"), "a.c") 

#Este dá erro
str_view(c("abc", "a.c", "bef"), "a\.c")

#Este detecta a.c como string
str_view(c("abc", "a.c", "bef"), "a\\.c")

#Suponha que você quer uma string com a barra invertida 
# como caracter.

#Isto não irá funcionar pois a barra simples é um escape
barra <- "Isto é uma \ barra"
barra
writeLines(barra)

#Precisamos escapar a própria barra, portanto precisamos
# da barra dupla
barra2 <- "Isto é uma \\ barra"
barra2
writeLines(barra2)

#Para detectar a barra precisaremos escapar duas barras
# portanto precisamos de quatro barras

#str_view(barra2,"\") #Nem funciona pois estamos mandando
# escapar a segunda aspa

str_view(barra2,"\\") #ERRO

#str_view(barra2,"\\\") #Nem funciona!

str_view(barra2,"\\\\")

#Procuras a partir da esquerda:
x <- c("abacate", "banana", "pêssego"); str_view(x, "^a")

#Procuras a partir da direita:
x <- c("abacate", "banana", "pêssego"); str_view(x, "a$")

#Para detectar a palavra "Maçã" em qualquer posição
# de uma string
x <- c("Bolo de Maçã", "Maçã", "Torta de Maçã") 
str_view(x, "Maçã")

#Para detectar apenas palavra "Maçã":
x <- c("Bolo de Maçã", "Maçã", "Torta de Maçã")
str_view(x,"^Maçã$")

x <- c("Bolo de Maçã", "Maçã Forte", "Torta de Maçã")
str_view(x,"^Maçã$")

#Outros caracteres especiais:
# "\d" encontra qualquer número.
# "\s" encontra qualquer espaço em branco 
#      (espaço, tabulação, nova linha).
# [abc] encontra a, b, ou c.
# [^abc] encontra qualquer string menos a, b, ou c.

#Operadores lógicos: &=E, |=OU, !=NÃO
# Este exemplo detecta Sao ou São
str_view(c("Sao Paulo", "São Paulo"), "S(a|ã)o")

#Detecção de repetições
x <- "1888 em romanos é: MDCCCLXXXVIII"

str_view(x,"CC+") #+: CC, 1 vez ou mais

str_view(x,"C[LX]+") #+: C e L ou X, 1 vez ou mais

str_view(x,"C{2}") # C exatamente 2 vezes 

str_view(x,"C{2,3}") # C 2 a 3 vezes

#Este script fez uma pequena introdução as regexps em R.
# Para estudo recomenda-se o site: https://regexr.com
