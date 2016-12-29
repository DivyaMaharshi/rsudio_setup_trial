# list of packages required for the session
#vegan -> similarity/dissimilarity, diversity
#dplyr -> data manipulation
rm(list=ls())
packages_required= c("vegan","dplyr","proxy","cba","reshape","reshape2","RMySQL")

# install from CRAN , if not already installed
install_check<- packages_required %in% installed.packages()
if(length(packages_required[!install_check])>0) install.packages(packages_required[!install_check],dependencies = TRUE)

# Load required libraries for the session
lapply(packages_required, FUN = function(X) {
  do.call("require", list(X)) 
})

###connection 
library(RMySQL)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
dbConnectNewIgp<- function(){
  igpnewConnProd <<- dbConnect(MySQL(),user='prod-ds',password='ML3Ghn@qw123fw', dbname='igpnew', host='igp-prod.cdlzhqutobeq.us-east-1.rds.amazonaws.com')  }
dbConnectNewIgp()


## db query to get distinct ptid 
distinct_ptid <- dbGetQuery(igpnewConnProd, 'select distinct(ptid) from product_cat')
datalist_rrop = list()
datalist_attributes = list()

## for this ptid do rrop wise and attributes + category wise based on attr set type 

for (i in 1:nrow(distinct_ptid))
{ 
  ptid <- distinct_ptid[i,'ptid']
  sql_query = paste("select distinct(pid) from product_cat where ptid =", ptid, sep='')
  ptid_products <- dbGetQuery(igpnewConnProd,sql_query)
  if(nrow(ptid_products) !=0)
  {
    ptid_products <- as.vector(ptid_products$pid,mode='numeric')
    ptid_products <- as.vector(ptid_products)
    
    ###rrop
    sql_query = paste("select t1.products_id,t2.label,t1.weightage from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t3.attr_set_type = 1 and t1.products_id in (",paste(ptid_products,collapse = ','),")",sep='')
    sql_query <-  gsub("c\\(","\\(" , sql_query)
    data <- dbGetQuery(igpnewConnProd,sql_query)
    if(nrow(data) != 0)
    {
      data <- unique(data)
      x <- dcast(data,  products_id ~ label,value.var='weightage')
      
      ## Addition of MPL to the output
      sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(ptid_products,collapse = ','), ')', sep='')
      sql_query <-  gsub("c\\(","\\(" , sql_query)
      products_mpl <- dbGetQuery(igpnewConnProd,sql_query)
      
      
      #merging rrop data with mpl column
      x = merge(x,products_mpl,by.x='products_id',by.y='products_id')
      x[is.na(x)] <- 0
      actual_product_ids <- as.data.frame( x$products_id)
      actual_product_ids$seq_numbers <- seq(1:nrow(x))
      x <- within(x,rm(products_id))
      rrop_mat1 <- x
      correlation_simil<-simil(rrop_mat1,method = 'correlation',upper = TRUE, diag = TRUE)
      cosine_simil<-simil(rrop_mat1,method = 'cosine',upper = TRUE, diag = TRUE)
      df1 <- melt(as.matrix(correlation_simil),varnames=c("row","col"))
      df2 <- melt(as.matrix(cosine_simil), varnames=c("row","col"))
      names(df1) <- c('product1','product2','correlation_simil')
      names(df2) <- c('product1','product2','cosine_simil')
      final_df <- cbind(df1,df2)
      final_df <- final_df[c('product1','product2','correlation_simil','cosine_simil')]
      names(final_df) <- c('product1','product2','correlation_simil','cosine_simil')
      final_df_with_productids <- merge(final_df,actual_product_ids,by.x='product1',by.y='seq_numbers')
      final_df_with_productids <- merge(final_df_with_productids,actual_product_ids,by.x='product2',by.y='seq_numbers')
      names(final_df_with_productids) <- c('p1','p2','correlation_simil','cosine_simil','product1','product2')
      final_df_with_productids <- final_df_with_productids[c('product1','product2','correlation_simil','cosine_simil')]
      final_df_with_productids$ptid <- ptid
      datalist_rrop[[i]] <- final_df_with_productids
    }
    
    
    #### category and attributes wise
    sql_query = paste("select t1.products_id,t2.label,t1.weightage from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t3.attr_set_type != 1 and t1.products_id in (",paste(ptid_products,collapse = ','),")",sep='')
    sql_query <-  gsub("c\\(","\\(" , sql_query)
    data <- dbGetQuery(igpnewConnProd,sql_query)
    if(nrow(data) != 0)
    {
      data <- unique(data)
      x <- dcast(data,  products_id ~ label,value.var='weightage')
      
      # ## Addition of MPL to the output
      # sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(ptid_products,collapse = ','), ')', sep='')
      # sql_query <-  gsub("\\\n","" , sql_query)
      # sql_query <-  gsub("c\\(","\\(" , sql_query)
      # products_mpl <- dbGetQuery(igpnewConnProd,sql_query)
      # 
      # #merging rrop data with mpl column
      # x = merge(x,products_mpl,by.x='products_id',by.y='products_id')
      
      x[is.na(x)] <- 0
      actual_product_ids <- as.data.frame( x$products_id)
      actual_product_ids$seq_numbers <- seq(1:nrow(x))
      x <- within(x,rm(products_id))
      attr_mat1 <- x
      correlation_simil<-simil(attr_mat1,method = 'correlation',upper = TRUE, diag = TRUE)
      cosine_simil<-simil(attr_mat1,method = 'cosine',upper = TRUE, diag = TRUE)
      df1 <- melt(as.matrix(correlation_simil),varnames=c("row","col"))
      df2 <- melt(as.matrix(cosine_simil), varnames=c("row","col"))
      names(df1) <- c('product1','product2','correlation_simil')
      names(df2) <- c('product1','product2','cosine_simil')
      final_df <- cbind(df1,df2)
      final_df <- final_df[c('product1','product2','correlation_simil','cosine_simil')]
      names(final_df) <- c('product1','product2','correlation_simil','cosine_simil')
      final_df_with_productids <- merge(final_df,actual_product_ids,by.x='product1',by.y='seq_numbers')
      final_df_with_productids <- merge(final_df_with_productids,actual_product_ids,by.x='product2',by.y='seq_numbers')
      names(final_df_with_productids) <- c('p1' , 'p2','correlation_simil','cosine_simil','product1','product2')
      final_df_with_productids <- final_df_with_productids[c('product1','product2','correlation_simil','cosine_simil')]
      final_df_with_productids$ptid <- ptid
      datalist_attributes[[i]] <- final_df_with_productids
    }
  }
  
}


# final_df <- merge(rrop_similarity_df,attributes_similarity_df)

rrop_similarity_df = do.call(rbind, datalist_rrop)
attributes_similarity_df = do.call(rbind, datalist_attributes)
rrop_similarity_df = rrop_similarity_df[rrop_similarity_df['product1'] == 216966,]
attributes_similarity_df = attributes_similarity_df[attributes_similarity_df['product1'] == 216966,]
# final_df <- merge(rrop_similarity_df,attributes_similarity_df)


# product similarity based on cards mapping
distinct_cards <- dbGetQuery(igpnewConnProd, 'select distinct(card_id) from cards_url ')
datalist_cards_rrop = list()

for (i in 130:140)
{
  
  #card_id <- 133
  card_id <- distinct_cards[i,'card_id']
  print(card_id)
  sql_query = paste("select distinct(prod_id) from prod_rank_ds where card_id =", card_id, sep='')
  cardid_products <- dbGetQuery(igpnewConnProd,sql_query)
  cardid_products <- as.vector(cardid_products$prod_id,mode='numeric')
  cardid_products <- as.vector(cardid_products)
  
  ###rrop
  sql_query = paste("select t1.products_id,t2.label,t1.weightage from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t3.attr_set_type = 1 and t1.products_id in (",paste(cardid_products,collapse = ','), ')', sep='')
  sql_query <-  gsub("\\\n","" , sql_query)
  sql_query <-  gsub("c\\(","\\(" , sql_query)
  data <- dbGetQuery(igpnewConnProd,sql_query)
  data <- unique(data)
  x <- dcast(data,  products_id ~ label,value.var='weightage')
  
  ## Addition of MPL to the output
  sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(cardid_products,collapse = ','), ')', sep='')
  sql_query <-  gsub("\\\n","" , sql_query)
  sql_query <-  gsub("c\\(","\\(" , sql_query)
  products_mpl <- dbGetQuery(igpnewConnProd,sql_query)
  
  #merging rrop data with mpl column
  x = merge(x,products_mpl,by.x='products_id',by.y='products_id')
  x[is.na(x)] <- 0
  actual_product_ids <- as.data.frame(x$products_id)
  actual_product_ids$seq_numbers <- seq(1:nrow(x))
  x <- within(x,rm(products_id))
  rrop_mat1 <- x
  correlation_simil<-simil(rrop_mat1,method = 'correlation',upper = TRUE, diag = TRUE)
  cosine_simil<-simil(rrop_mat1,method = 'cosine',upper = TRUE, diag = TRUE)
  df1 <- melt(as.matrix(correlation_simil),varnames=c("row","col"))
  df2 <- melt(as.matrix(cosine_simil), varnames=c("row","col"))
  names(df1) <- c('product1','product2','correlation_simil')
  names(df2) <- c('product1','product2','cosine_simil')
  final_df <- cbind(df1,df2)
  final_df <- final_df[c('product1','product2','correlation_simil','cosine_simil')]
  names(final_df) <- c('product1','product2','correlation_simil','cosine_simil')
  final_df_with_productids <- merge(final_df,actual_product_ids,by.x='product1',by.y='seq_numbers')
  final_df_with_productids <- merge(final_df_with_productids,actual_product_ids,by.x='product2',by.y='seq_numbers')
  names(final_df_with_productids) <- c('p1' , 'p2','correlation_simil','cosine_simil','product1','product2')
  final_df_with_productids <- final_df_with_productids[c('product1','product2','correlation_simil','cosine_simil')]
  final_df_with_productids$card_id <- card_id
  datalist_cards_rrop[[i]] <- final_df_with_productids
  
}


rrop_cards_similarity_df = do.call(rbind, datalist_cards_rrop)
View(rrop_cards_similarity_df)
rrop_cards_similarity_df = rrop_cards_similarity_df[rrop_cards_similarity_df['card_id'] == 133 ,]
rrop_cards_similarity_df = rrop_cards_similarity_df[rrop_cards_similarity_df['product1'] == 213840 ,]





