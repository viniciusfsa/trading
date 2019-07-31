#DETECTA DIVERGENCIA DE ALTA

rm(list = ls(all = TRUE))
require(quantmod)

#AAPL C INTC SPY IBM
#empresa = getSymbols('IBM',from='2003-01-01',to='2013-12-31', auto.assign=FALSE)


# Importa CSV
#library(readr)
#USDJPY_M1 <- read_delim("USDJPY_M1.csv",
#                        ";", escape_double = FALSE, locale = locale(decimal_mark = ","),
#                        trim_ws = TRUE)

library(readr)
USDJPY_M1 <- read_delim("Trading/diario_minuto/usdjpy_m1.csv",
                            ";", escape_double = FALSE, trim_ws = TRUE)
View(USDJPY_M1)


# cria vetor de data
library(readr)
library(lubridate)
dates <- dmy_hm(USDJPY_M1$Time)

# cria xts
xts1 <- xts(x = dates, order.by = dates)

# adiciona coluna de preco
xts1=cbind(xts1,ultimo=USDJPY_M1$Close);




#preco = Ad(empresa)
preco = xts1$ultimo
total = length(preco)
macd = MACD(xts1$ultimo)
histograma = macd[,1]-macd[,2]

diasFrente = 15
lim = 0
nDivergenciasAlta = 0
nConfirmacoesAlta = 0
nDivergenciasBaixa = 0
nConfirmacoesBaixa = 0
vetorDivergenciasAlta=c()
vetorDivergenciasBaixa=c()
indiceVetorDivergenciasAlta=1
indiceVetorDivergenciasBaixa=1


precoMatriz = data.matrix(as.data.frame(preco))
tamanhoPreco = length(preco);

#Cria Colula de Divergências de Baixa
divsBaixa = seq(0, 0, length.out=tamanhoPreco);
for(p in 1:length(divsBaixa)){
  divsBaixa[p]=NA;
}
preco=cbind(preco,divsBaixa=divsBaixa);

#Cria Coluna de Divergência de Alta
divsAlta = seq(0, 0, length.out=tamanhoPreco);
for(p in 1:length(divsAlta)){
  divsAlta[p]=NA;
}
preco=cbind(preco,divsAlta=divsAlta);




#########################################################################################################
#############################DETECTANDO DIVERGÊNCIAS DE BAIXA############################################
#########################################################################################################
i = 34 #Ponto Inicial onde tem valores validos para o Histograma
marcadorProximo = i
while(TRUE) {
    marcador = marcadorProximo
    i = marcador
    blocoA=c()
    blocoB=c()
    blocoC=c()

    #Descartando possíveis valores negativos no início
    while(histograma[i]<0){
      i=i+1
      if(i>=total)
        break();
    }
    #detectando Bloco A (Positivo)
    indexA = 1
    while(histograma[i]>0){
      blocoA[indexA]= histograma[i]
      indexA = indexA + 1
      i= i + 1
      if(i>=total)
        break();
    }
    #detectando Bloco B
    indexB = 1
    while(histograma[i]<0){
      blocoB[indexB]= histograma[i]
      indexB = indexB + 1
      i= i + 1
      if(i>=total)
        break();
    }
    marcadorProximo = i

    #detectando Bloco C
    indexC = 1
    while(histograma[i]>0){
      blocoC[indexC]= histograma[i]
      indexC = indexC + 1
      i= i + 1
      if(i>=total)
        break();
    }

    if(i>=total)
      break();

    # Definindo as posições dos máximos dos Blocos A e C
    posicaoMaxA = marcador + which.max(blocoA)-1
    posicaoMaxC = marcador + length(blocoA)+length(blocoB)+which.max(blocoC)-1

    #Cálculo dos máximos dos blocos A e C
    maxA = max(blocoA)
    maxC = max(blocoC)

    #Cálculo dos preços nos dias dos máximos de A e de C
    precoA = precoMatriz[posicaoMaxA];
    precoC = precoMatriz[posicaoMaxC];

    final = i+diasFrente
    if((maxA > maxC)&(precoA<precoC)&(length(blocoA)>lim)&(length(blocoC)>lim)) {
      nDivergenciasBaixa = nDivergenciasBaixa+1
      preco[posicaoMaxC,2]=preco[posicaoMaxC,1]
      preco1 = precoMatriz[i]
      if(final>total)
       diasFrente = total-i

      vetorDivergenciasBaixa[indiceVetorDivergenciasBaixa]=as.Date(index(preco[posicaoMaxC]));
      vetorDivergenciasBaixa[indiceVetorDivergenciasBaixa+1]=precoMatriz[posicaoMaxC,1];
      indiceVetorDivergenciasBaixa = indiceVetorDivergenciasBaixa+2;

      preco15 = precoMatriz[i+diasFrente]
      if(preco15<preco1){
        nConfirmacoesBaixa = nConfirmacoesBaixa + 1
      }
    }

    if(final>total)
      break();
}#FIM DA DETECÇÃO DE DIVERGÊNCIA DE BAIXA





#########################################################################################################
#############################DETECTANDO DIVERGÊNCIAS DE ALTA#############################################
#########################################################################################################


i = 34 #Ponto Inicial onde tem valores validos para o Histograma
marcadorProximo = i
while(TRUE) {
  marcador = marcadorProximo
  i = marcador
  blocoA=c()
  blocoB=c()
  blocoC=c()

  #Descartando possíveis valores positivos no início
  while(histograma[i]>0){
    i=i+1
    if(i>=total)
      break();
  }
  #detectando Bloco A (NEGATIVO)
  indexA = 1
  while(histograma[i]<0){
    blocoA[indexA]= histograma[i]
    indexA = indexA + 1
    i= i + 1
    if(i>=total)
      break();
  }
  #detectando Bloco B
  indexB = 1
  while(histograma[i]>0){
    blocoB[indexB]= histograma[i]
    indexB = indexB + 1
    i= i + 1
    if(i>=total)
      break();
  }
  marcadorProximo = i

  #detectando Bloco C
  indexC = 1
  while(histograma[i]<0){
    blocoC[indexC]= histograma[i]
    indexC = indexC + 1
    i= i + 1
    if(i>=total)
      break();
  }

  if(i>=total)
    break();

  # Definindo as posições dos mínimos dos Blocos A e C
  posicaoMinA = marcador + which.min(blocoA)-1
  posicaoMinC = marcador + length(blocoA)+length(blocoB)+which.min(blocoC)-1

  #Cálculo dos mínimos dos blocos A e C
  minA = min(blocoA)
  minC = min(blocoC)

  #Cálculo dos mínimos nos dias dos máximos de A e de C
  precoA = precoMatriz[posicaoMinA];
  precoC = precoMatriz[posicaoMinC];


  final = i+diasFrente
  if((minA < minC)&(precoA>precoC)&(length(blocoA)>lim)&(length(blocoC)>lim)) {
    nDivergenciasAlta = nDivergenciasAlta+1
    preco[posicaoMinC,3]=preco[posicaoMinC,1]
    preco1 = precoMatriz[i]
    if(final>total)
      diasFrente = total-i
    vetorDivergenciasAlta[indiceVetorDivergenciasAlta]=as.Date(index(preco[posicaoMinC]));
    vetorDivergenciasAlta[indiceVetorDivergenciasAlta+1]=precoMatriz[posicaoMinC,1];
    indiceVetorDivergenciasAlta = indiceVetorDivergenciasAlta+2;

    preco15 = precoMatriz[i+diasFrente]
    if(preco15>preco1){
      nConfirmacoesAlta = nConfirmacoesAlta + 1
    }
  }

  if(final>total)
    break();
  #print('')
}#FIM DA DETECÇÃO DE DIVERGÊNCIA DE ALTA



vetorDivergenciasAlta=matrix(vetorDivergenciasAlta,ncol=2,byrow=TRUE);
vetorDivergenciasBaixa=matrix(vetorDivergenciasBaixa,ncol=2,byrow=TRUE);



par(mfrow=c(2,1))
plot(preco[,1],main="Preço")
#points(preco[,2],type="b",col="red",lwd =5)
points(preco[,2],pch=19,col="red", cex = 1.5)
#points(preco[,3],type="b",col="blue",lwd = 5)
points(preco[,3],pch=19,col="blue", cex = 1.5)
plot(histograma,main="Histograma MACD")


