# list of packages required for the session
#vegan -> similarity/dissimilarity, diversity
#dplyr -> data manipulation
rm(list=ls())
.libPaths("/home/igp/R/x86_64-pc-linux-gnu-library/3.2")
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
  igpnewConnProd <<- dbConnect(MySQL(),user='root',password='password', dbname='igpnew', host='127.0.0.1', port=3306)  
}
dbConnectNewIgp()


## db query to get distinct ptid 
distinct_ptid <- dbGetQuery(igpnewConnProd, 'select distinct(ptid) from product_cat')
datalist_rrop = list()
datalist_category = list()

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
    rrop_data <- dbGetQuery(igpnewConnProd,sql_query)
    if(nrow(rrop_data) != 0)
    {
      rrop_data <- unique(rrop_data)
      rrop_data <- dcast(rrop_data,  products_id ~ label,value.var='weightage')
      
      ## Addition of MPL to the output
      sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(ptid_products,collapse = ','), ')', sep='')
      sql_query <-  gsub("c\\(","\\(" , sql_query)
      products_mpl <- dbGetQuery(igpnewConnProd,sql_query)
      
      
      #merging rrop data with mpl column
      rrop_data = merge(rrop_data,products_mpl,by.x='products_id',by.y='products_id')
      rrop_data[is.na(rrop_data)] <- 0
      
      # extract products id 
      actual_product_ids <- as.data.frame(rrop_data$products_id)
      actual_product_ids$seq_numbers <- seq(1:nrow(rrop_data))
      
      # remove products_id for simil function
      rrop_data <- within(rrop_data,rm(products_id))
      
      # simil calculation 
      rrop_mat1 <- rrop_data
      cosine_simil <-simil(rrop_mat1,method = 'cosine',upper = TRUE, diag = TRUE)
      cosine_df <- melt(as.matrix(cosine_simil), varnames=c("row","col"))
      names(cosine_df) <- c('product1','product2','cosine_simil')
      
      # merge
      final_df_with_productids <- merge(cosine_df,actual_product_ids,by.x='product1',by.y='seq_numbers')
      final_df_with_productids <- merge(final_df_with_productids,actual_product_ids,by.x='product2',by.y='seq_numbers')
      names(final_df_with_productids) <- c('p1','p2','cosine_simil','product1','product2')
      final_df_with_productids <- final_df_with_productids[c('product1','product2','cosine_simil')]
      final_df_with_productids$ptid <- ptid
      datalist_rrop[[i]] <- final_df_with_productids
    }
    
    
    ### category and attributes wise
    sql_query = paste("select t1.products_id,t2.label,t1.weightage from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t3.attr_set_type != 1 and t1.products_id in (",paste(ptid_products,collapse = ','),")",sep='')
    sql_query <-  gsub("c\\(","\\(" , sql_query)
    cat_data <- dbGetQuery(igpnewConnProd,sql_query)
    if(nrow(cat_data) != 0)
    {
      cat_data <- unique(cat_data)
      cat_data <- dcast(cat_data,  products_id ~ label,value.var='weightage')

      ## Addition of MPL to the output
      sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(ptid_products,collapse = ','), ')', sep='')
      sql_query <-  gsub("\\\n","" , sql_query)
      sql_query <-  gsub("c\\(","\\(" , sql_query)
      products_mpl <- dbGetQuery(igpnewConnProd,sql_query)

      ##merging rrop data with mpl column
      cat_data = merge(cat_data,products_mpl,by.x='products_id',by.y='products_id')
      cat_data[is.na(cat_data)] <- 0 
      
      ## extract products_id
      actual_product_ids <- as.data.frame(cat_data$products_id)
      actual_product_ids$seq_numbers <- seq(1:nrow(cat_data))
      
      ## remove products_id
      cat_data <- within(cat_data,rm(products_id))
      
      # simila calculation based on attributes data
      attr_mat1 <- cat_data
      cosine_simil<-simil(attr_mat1,method = 'cosine',upper = TRUE, diag = TRUE)
      cosine_df <- melt(as.matrix(cosine_simil), varnames=c("row","col"))
      names(cosine_df) <- c('product1','product2','cosine_simil')
  
      #merge
      final_df_with_productids <- merge(cosine_df,actual_product_ids,by.x='product1',by.y='seq_numbers')
      final_df_with_productids <- merge(final_df_with_productids,actual_product_ids,by.x='product2',by.y='seq_numbers')
      names(final_df_with_productids) <- c('p1','p2','cosine_simil','product1','product2')
      final_df_with_productids <- final_df_with_productids[c('product1','product2','cosine_simil')]
      final_df_with_productids$ptid <- ptid
      datalist_category[[i]] <- final_df_with_productids
    }
  }
}

rrop_similarity_df = do.call(rbind, datalist_rrop)
attributes_similarity_df = do.call(rbind, datalist_category)
rrop_similarity_df = rrop_similarity_df[rrop_similarity_df['product1'] == 216966,]
attributes_similarity_df = attributes_similarity_df[attributes_similarity_df['product1'] == 216966,]



