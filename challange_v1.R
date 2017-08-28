#install.packages('jsonlite')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('ggthemes')
#install.packages('corrgram')
#install.packages('corplot')
#install.packages('caTools')
#install.packages('forecast')
#install.packages('scales')
#install.packages('tseries')
#install.packages('hms')

library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)
library(forecast)
library(tseries)
library(hms)




# Function gets all products in dataset
# Return chacacters vector
# Parameter dets.frame is data frame 
get_all_products <- function(dets.frame){
  
  all.products.details <- c()
  # For each row
  for(nrow.data.frame in 1:nrow(dets.frame)){
    # For each purchase item
    for (nrow.detail in dets.frame[nrow.data.frame,][1] ){
      # Get products
      details <- (nrow.detail[,c('prod.xProd')])
      # Keep products
      all.products.details <- append(all.products.details,details)
    }
  }
  # Return unique products
  return(unique(all.products.details))
}

# Function fills matrix with purchase item and products
# Return numeric matrix
# Parameters - all.products is products vector and dets.frame is pruchase
fill_products_by_purchase_items <- function(all.products, dets.frame){
  # Create empty matrix
  matrix.products <- matrix(0,nrow = nrow(dets.frame), ncol = length(all.products) )
  # Products columns
  colnames(matrix.products) = all.products
  # For each row
  for(nrow.data.frame in 1:nrow(dets.frame)){
    # For each purchase item
    for (nrow.detail in dets.frame[nrow.data.frame,][1] ){
      # Keep product
      details <- (nrow.detail[,c('prod.xProd')])
      # Keep quantity
      qtd <- (nrow.detail[,c('prod.qCom')])
      # For each product found in dataset
      for(product in all.products){
        # If product is in purchase
        if(product %in% details){
          # Several occurrences
          if(table(details)[product] > 1){
            for(check.index in 1:length(details)){
              if(details[check.index] == product){
                # Value increase
                matrix.products[nrow.data.frame,product] =  matrix.products[nrow.data.frame,product] + qtd[check.index]
              }
            }
           # One occurrence
          }else{
            lenght.grep <- length(grep(product,details))
            # Products with same prefix
            if(lenght.grep > 1){
              for(check.index in 1:length(details)){
                if(details[check.index] == product){
                  # Value assignment
                  matrix.products[nrow.data.frame,product] =  qtd[check.index]
                }
              }
            # Simple case  
            }else{
              # Value assignment
              matrix.products[nrow.data.frame,product] = qtd[grep(product,details)]
            }
          }
        }
      }
    }
  }
  return(matrix.products)
}


# Function fills matrix with varables 
# alcoholic beverage
# non alcoholic beverage
# meal
# sweetmeats
# japanese food
# Return numeric matrix 
# Parameter all.products is products vector 
fill_vars_products <- function(prod.vars, dets.frame){
  # Create empty matrix
  matrix.prod.vars <- matrix(0,nrow = nrow(dets.frame), ncol = length(prod.vars))
  # Products columns
  colnames(matrix.prod.vars) = prod.vars
  # For each row

  for(nrow.df in 1:nrow(dets.frame)){
    # For each purchase item
    
    for (detail in dets.frame[nrow.df,][1]){
      # Keep product
      print('nrow.detail')
      print(detail)
      print('dets.frame[nrow.detail]')
      
      details.prod <- detail['prod.xProd']
      # Keep quantity
      qnt.prod <- detail['prod.qCom']
      # TODO IMPROVEMENT 
      print('nrow(details.prod)')
      print(length(detail))
      for(nrw in 1:nrow(details.prod)){
        # fills variables
        matrix.prod.vars <- fill_vars(details.prod[nrw,],qnt.prod[nrw,], matrix.prod.vars,nrow.df)
      }
    }
  }
  return(matrix.prod.vars)
}


fill_vars <- function(product, qnt, matrix.prod.vars, nrow.df){
  print(product)
  print(qnt)
  if(product == "AGUA" | product == "REFRIGERANTE"  | product ==  "SUCO"           
     | product == "CHA" | product == "LIMONADA" | product == "CAFE EXPRESSO" 
     | product == "BULE CHA"  | product ==   "SAKE" ){
    matrix.prod.vars[nrow.df,'non.alcoholic.beverage'] <- matrix.prod.vars[nrow.df,'non.alcoholic.beverage'] + qnt;
    
  } else if(product == "BUFFET"){
    matrix.prod.vars[nrow.df,'meal'] <- matrix.prod.vars[nrow.df,'meal'] + qnt;
  }else if(product == "CERVEJA"  | product == "CERVEJA LATA" | product == "WHISKY" | product == "CAIPIROSKA"
           | product == "CAIPIRINHA"  | product == "VINHO"  | product == "BACARDI"){
    matrix.prod.vars[nrow.df,'alcoholic.beverage'] <- matrix.prod.vars[nrow.df,'alcoholic.beverage'] + qnt;
  }else if(product == "DOCINHOS" | product ==  "SOBREMESA"){
    matrix.prod.vars[nrow.df,'sweetmeats'] <- matrix.prod.vars[nrow.df,'sweetmeats'] + qnt;
  }else if(product == "HARUMAKI" | product ==   "TEMAKI" | product ==  "SUSHI ESPECIAL" | product ==  "SASHIMI" | product ==  "YAKISSOBA" | product ==  "URAMAKI"){
    matrix.prod.vars[nrow.df,'jap.food'] <- matrix.prod.vars[nrow.df,'jap.food'] + qnt;
  }
  print(head(matrix.prod.vars))
  return(matrix.prod.vars)
}



## 1 - Parse and extract the data.
# Reading dataset (json file)
data.frame <- fromJSON("sample.json", flatten = TRUE)
# Get variable (dets) with purchase items 
dets.frame <- data.frame %>% select(dets)
# Get products vector 
all.products <- get_all_products(dets.frame)
# Fill matrix with purchase item and products
matrix.products <- fill_products_by_purchase_items(all.products,dets.frame)
# Add products columns by purchase
data.frame <- cbind(data.frame,matrix.products)
# remove dets feature 
data.frame$dets <- NULL

# list products features
prods.featrs <- c('non.alcoholic.beverage','alcoholic.beverage','meal','sweetmeats','jap.food')
# Fill matrix with products features
matrix.feat <- fill_vars_products(prods.featrs,dets.frame)


## 2 - Identify a pattern on any set of fields that can help predict how much a customer will spend
# 
df.prods.feat <- cbind(data.frame['complemento.valorTotal'],matrix.feat)

# Finding numerics features
num.variables.numeric <- sapply(data.frame, is.numeric)

# Finding correlation between each numeric feature
corr.prods <- cor(data.frame[,num.variables.numeric])
# Finding correlation between features products
corr.feat <- cor(df.prods.feat)

# Removes missing values
corr.prods[is.na(corr.prods)] <- 0
corr.feat[is.na(corr.feat)] <- 0


# Remove variables associated to rate ('*icms*') from correlation matrix
corr.products.clean <- corr.prods[-c(2:17),-c(2:17)]


# The table shows correlation between all features
#-------------------------------------------------------------
# Each row represents a feature (label)                      -
# Each column represents correlation with other feature      -
# How much dark blue better correlation between features     -
# How much dark orange worse correlation between features    -
#-------------------------------------------------------------
#print(corrgram(data.frame))
# The table shows correlation between purchase value and products
print(corrplot(corr.products.clean, method = 'color'))
# The main row is first and it represents purchase value (row 1). 
# The value 'complemento.valorTotal' is more closely correlated with the buffet, soft drink, water and juice (row 1).
# It is possible to notice that the drink of choice chosen with the buffet is the soft drink (row 3). 
# The people who buy beer also tend to take whisky (rows 15 and 16).

print(corrplot(corr.feat, method = 'color'))
# The main row is first and it represents purchase value (row 1). 
# Sweetmeat and Japa food tend to be purchased separately (rows 5 and 6). 
#guarda o dia da semana

# 
data.frame$dateTs <- as.Date(data.frame$ide.dhEmi)
# sales summary by day
df.temporalserie <- data.frame(data.frame %>% group_by(dateTs) %>% summarise(complemento.valorTotal = sum(complemento.valorTotal)))
# insert monday at data first week
# 6038.07 é averages os two other weeks
df.temporalserie[seq(1+1,nrow(df.temporalserie)+1),] <- df.temporalserie[seq(1,nrow(df.temporalserie)),]
df.temporalserie[1,] <- c('2016-01-04',6038.07)
# now df.temporalserie contains 3 frequency os 6 days


#decopose data
ts.value = ts(na.omit(df.temporalserie[,c('complemento.valorTotal')]), frequency=6) # 6 days by week
decomp = stl(ts.value, s.window="periodic")
# checking data
plot(decomp)

# stationary <- p-value = 0.01
print(adf.test(ts.value, alternative = "stationary"))
# arima model
arima.model <- auto.arima(as.numeric(ts.value))
tsdisplay(residuals(arima.model), lag.max=22, main='Auto Arima (0,0,0)')
#acf 6 
#pacf 3
arima.model.2 <- arima(as.numeric(ts.value), order = c(3,1,6))
tsdisplay(residuals(arima.model.2), lag.max=22, main='Seasonal Model Residuals')

fcastr2 <- forecast(arima.model.2)
plot(fcastr2)
print(fcastr2)

  