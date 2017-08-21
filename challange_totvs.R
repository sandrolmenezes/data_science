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
# Parameter all.products is products vector 
fill_products_by_purchase_items <- function(all.products){
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

## 1 - Parse and extract the data.
# Reading dataset (json file)
data.frame <- fromJSON("sample.json", flatten = TRUE)
# Get variable (dets) with purchase items 
dets.frame <- data.frame %>% select(dets)
# Get products vector 
all.products <- get_all_products(dets.frame)
# Fill matrix with purchase item and products
matrix.products <- fill_products_by_purchase_items(all.products)
# Add products columns by purchase
data.frame <- cbind(data.frame,matrix.products)
print(data.frame)
### FINISH (1 - Parse and extract the data -> data.frame)


## 2 - Identify a pattern on any set of fields that can help predict how much a customer will spend
# Keep to use prediction process (##3)
df.ts.prod.items <- cbind(data.frame['ide.dhEmi'],data.frame['complemento.valorTotal'],matrix.products)
# remove dets feature 
data.frame$dets <- NULL
# Finding numerics features
num.variables.numeric <- sapply(data.frame, is.numeric)
# Finding correlation between each numeric feature
corr.features <- cor(data.frame[,num.variables.numeric])
# Removes missing values
#-------------------------------------------------------------
# INFORMATION FOR TRESS TABLES TO FOLLOW                     -  
# Each row represents a feature (label)                      -
# Each column represents correlation with other feature      -
# How much dark blue better correlation between features     -
# How much dark orange worse correlation between features    -
#-------------------------------------------------------------
corr.features[is.na(corr.features)] <- 0
# The table shows correlation between all features
# The main row is first and it represents purchase value. 
# Variables associated to rate ('*icms*') are not relevant because their values are derived from the value of the purchase.
# The objective is to discover the correlation of quantitative product variables that are directly related to the composition of the purchase price.
print(corrgram(data.frame))
# The table shows correlation between numeric features
print(corrplot(corr.features, method = 'color'))
# Remove variables associated to rate ('*icms*') from correlation matrix
corr.features.products <- corr.features[-c(2:17),-c(2:17)]
# The table shows correlation between purchase value and products
print(corrplot(corr.features.products, method = 'color'))
# Set of fields that can help predict how much a customer will spend
# fields - {BUFFET, REFRIGERANTE, CERVEJA, WISKY, AGUA, SUCO}
# This fiels are more relevants to predict how much a customer will spend. 
# But all products (last table) features influence how much a customer will spend.
### FINISH (2 - Identify a pattern on any set of fields that can help predict how much a customer will spend)


## 3 -Calculate a sales forecast for the next week.



  