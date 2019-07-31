precoM = data.matrix(as.data.frame(preco))
mediaMovel = EMA(precoM[,1],n=10)
qtdePrecos = length(precoM[,1])
risco = 0.01

indexCompras = 1
indexVendas = 1

nPrevisoesVenda = length(vetorDivergenciasBaixa[,2])
nPrevisoesCompra = length(vetorDivergenciasAlta[,2])

MatrizVendaCompra <- matrix(data=0,nrow=nPrevisoesVenda,ncol=6)
MatrizCompraVenda <- matrix(data=0,nrow=nPrevisoesCompra,ncol=6)

#^GSPC(Para C e IBM)    ^NDX(Para AAPL e INTC)
#Índice NASDAQ ^NDX
#Índice NYSE ^NYA


getSymbols('^NYA',from='2003-01-01',to='2013-12-31')
fechamentoIndice = Cl(NYA)

for(x in 1:qtdePrecos) { 
  #VENDA
  if(!is.na(precoM[x,2])){
    MatrizVendaCompra[indexVendas,1] = x
    MatrizVendaCompra[indexVendas,3] = precoM[x,1]
    MatrizVendaCompra[indexVendas,5] = fechamentoIndice[x,1]
    venda = precoM[x,1]  
    minimo = precoM[x,1]  
    a = x
    
    while(TRUE){
      
      if(precoM[a,1]<minimo){
        minimo = precoM[a,1]
        #print(paste("Nova dta de mínimo",a) )
        #print(paste("Novo Minimo",minimo) )
      }
      
      diferenca = mediaMovel[a] - minimo
      perc = minimo * risco
      if(diferenca<perc){
        a = a+1
        if(a>qtdePrecos-1) 
          break()
      }
    
      else{
        MatrizVendaCompra[indexVendas,2] = a
        MatrizVendaCompra[indexVendas,4] = precoM[a,1]
        MatrizVendaCompra[indexVendas,6] = fechamentoIndice[a,1]
        indexVendas = indexVendas +1    
        break()
      }             
    }    
    #print(x)   
    
    
  }
  
  
  #COMPRA
  else if(!is.na(precoM[x,3])){
    MatrizCompraVenda[indexCompras,1] = x
    MatrizCompraVenda[indexCompras,3] = precoM[x,1]
    MatrizCompraVenda[indexCompras,5] = fechamentoIndice[x,1]
    compra = precoM[x,1]  
    maximo = precoM[x,1]  
    a = x
    
    while(TRUE){
      
      if(precoM[a,1]>maximo){
        maximo = precoM[a,1]
        #print(paste("Nova dta de maximo",a) )
        #print(paste("Novo maximo",maximo) )
      }
      
      diferenca = maximo - mediaMovel[a]
      perc = maximo * risco
      if(diferenca<perc){
        a = a+1
        if(a>qtdePrecos-1) 
          break()
      }
      
      else{
        MatrizCompraVenda[indexCompras,2] = a
        MatrizCompraVenda[indexCompras,4] = precoM[a,1]
        MatrizCompraVenda[indexCompras,6] = fechamentoIndice[a,1]
        indexCompras = indexCompras +1    
        break()
      }             
    }    
    #print(x)   
  }
  
    
  
}

