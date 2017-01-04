source("packages.R")
source("database_connection.R")
source("logging.R")

### url parser ####
url_parser <- function(url)
{ 
  url <- 'https://www.igp.com/lillies-bouquets-bunches'
  url <- str_replace(url,'https://www.igp.com/','')
  return(url)
}


#### card id for url ####
## for this ptid do rrop wise and attributes + category wise based on attr set type 
products_cosine_measure <- function(set_type,distinct_ptid)
{ 
  temp_list <- list()
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
      if (set_type == TRUE){
        sql_query = paste("select t1.products_id,t2.label,t1.weightage from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t3.attr_set_type = 1 and t1.products_id in (",paste(ptid_products,collapse = ','),")",sep='')
        sql_query <-  gsub("c\\(","\\(" , sql_query)
        rrop_data <- dbGetQuery(igpnewConnProd,sql_query)
        View(rrop_data)
      }
      else{
        sql_query = paste("select t1.products_id,t2.label,t3.label as set_type_label from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t3.attr_set_type != 1 and t1.products_id in (",paste(ptid_products,collapse = ','),")",sep='')
        sql_query <-  gsub("c\\(","\\(" , sql_query)
        rrop_data <- dbGetQuery(igpnewConnProd,sql_query)
        #browser()
        ### read data from csv
        label_weights<-read.csv(paste(getwd(),"/attributes_label_weights.csv",sep='') , header = FALSE)
        colnames(label_weights) <- c('set_type_label','weightage')
        rrop_data <- merge(rrop_data,label_weights,by.x='set_type_label',by.y='set_type_label',all.x = TRUE)[c('products_id','label','weightage')]
        
      }
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
        temp_list[[i]] <- final_df_with_productids
      }
    }
  }
  return(temp_list)
}


## db query to get distinct ptid 
distinct_ptid <- dbGetQuery(igpnewConnProd, 'select distinct(ptid) from product_cat where ptid in (133858,134134)')


# number of rows would be same in both 
rrop_similarity_df = do.call(rbind, products_cosine_measure(set_type = TRUE, distinct_ptid))
attributes_similarity_df = do.call(rbind,products_cosine_measure(set_type = FALSE, distinct_ptid))
# rrop_similarity_df = rrop_similarity_df[rrop_similarity_df['product1'] == 217766,]
# attributes_similarity_df = attributes_similarity_df[attributes_similarity_df['product1'] == 217766,]


#################   Manipulation 
# select 10 products
top_rrop_similarity_df<-rrop_similarity_df %>%
  group_by(product1,ptid) %>%
  arrange(desc(cosine_simil))


top_attributes_similarity_df <- attributes_similarity_df %>%
  group_by(product1,ptid) %>%
  arrange(desc(cosine_simil)) 



# rbind and arrange  per product 20 similar products 
top_simil_df <- rbind(top_rrop_similarity_df,top_attributes_similarity_df)

# arrange per product 20 products in descending order of simil score
top_simil_df <- top_simil_df %>%
  group_by(product1,ptid) %>%
  arrange(desc(cosine_simil)) 


# remove duplicates and keep row with high cosine simil
top_simil_df <- top_simil_df[!duplicated(top_simil_df[,c('product1','product2')]),]
View(top_simil_df)

################end ##############################################################################33


###### Hetrogenicity ####################
dbConnectNewIgp()
sql_query = "select t1.products_id,t2.label product_label,t3.label as set_label,t2.attr_set_id from products p, newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where p.products_id = t1.products_id and t1.attr_val_id = t2.id and t2.attr_set_id = t3.id and p.products_status = 1 and t3.attr_set_type != 1 "
sql_query <-  gsub("c\\(","\\(" , sql_query)
label_data <- dbGetQuery(igpnewConnProd,sql_query)

## temp_merge
label_top_simil_df <- merge(top_simil_df,label_data,by.x='product2',by.y='products_id')

### read het level data
pt_level_het_data<-read.csv(paste(getwd(),"/pt_level_attr.csv",sep='') , header = TRUE)

##merge
pt_level_het_label_data <- merge(label_top_simil_df,pt_level_het_data,by.x= c('ptid','attr_set_id'), by.y=c('pt_id','attr_set_id'))
