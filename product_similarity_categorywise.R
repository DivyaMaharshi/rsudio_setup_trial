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


### card id for url ####
url_cardid <- function(url)
{
  sql_query = "select card_id from cards_url where url = '"+url+"' "
  url_cardid <- dbGetQuery(igpnewConnProd,sql_query)
  url_cardid <- as.numeric(url_cardid[1,'card_id'])
  return(url_cardid)
}




### Main function to compute similarity
products_cosine_measure <- function(set_type,distinct_cardid)
{ 
  #url <- url_parser(url)
  #card_id <-url_cardid(url) 
  temp_list <- list()
  for (i in 1:nrow(distinct_cardid))
  {
    cardid <- distinct_cardid[i,'cardid']
    sql_query = paste("select distinct(prod_id) as pid from products p join prod_rank pr on p.products_id = pr.prod_id where p.products_status = 1  and card_id  = ", cardid, sep='')
    cardid_products <- dbGetQuery(igpnewConnProd,sql_query)
    View(cardid_products)
    if(nrow(cardid_products) !=0)
    {
      cardid_products <- as.vector(cardid_products$pid,mode='numeric')
      cardid_products <- as.vector(cardid_products)
      
      ###rrop card_id computre similarity based on  all sets
      if (set_type == TRUE){
        sql_query = paste("select t1.products_id,t2.label,t1.weightage from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t1.products_id in (",paste(cardid_products,collapse = ','),")",sep='')
        sql_query <-  gsub("c\\(","\\(" , sql_query)
        rrop_data <- dbGetQuery(igpnewConnProd,sql_query)
       
        
      }
      ### category compute similarity based on attributes only , rrop not required
      else{
        sql_query = paste("select t1.products_id,t2.label,t3.label as set_type_label from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id and t3.attr_set_type != 1 and t1.products_id in (",paste(cardid_products,collapse = ','),")",sep='')
        sql_query <-  gsub("c\\(","\\(" , sql_query)
        rrop_data <- dbGetQuery(igpnewConnProd,sql_query)
        ### read data from csv
        label_weights<-read.csv(paste(getwd(),"/attributes_label_weights.csv",sep='') , header = FALSE)
        colnames(label_weights) <- c('set_type_label','weightage')
        rrop_data <- merge(rrop_data,label_weights,by.x='set_type_label',by.y='set_type_label',all.x = TRUE)[c('products_id','label','weightage')]
        View(rrop_data)
      }
      
      if(nrow(rrop_data) != 0)
      {
        rrop_data <- unique(rrop_data)
        rrop_data <- dcast(rrop_data,  products_id ~ label,value.var='weightage')
        
        ## Addition of MPL to the output
        sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(cardid_products,collapse = ','), ')', sep='')
        sql_query <-  gsub("c\\(","\\(" , sql_query)
        products_mpl <- dbGetQuery(igpnewConnProd,sql_query)
        
      
        #merging rrop data with mpl column
        rrop_data = merge(rrop_data,products_mpl,by.x='products_id',by.y='products_id')
        rrop_data[is.na(rrop_data)] <- 0
        View(rrop_data)
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
        final_df_with_productids$cardid <- cardid
        temp_list[[i]] <- final_df_with_productids
        
        print('done')
      }
    }
  }
  return(temp_list)
}


### RROP cards
distinct_cardid <- dbGetQuery(igpnewConnProd, "select distinct(card_id) as cardid from cards_url where type != Category and card_id = 121" ) 
rrop_similarity_df = do.call(rbind, products_cosine_measure(set_type = TRUE,distinct_cardid))

####################################### Calculation ########################################3 
### arrange
top_rrop_similarity_df<-rrop_similarity_df %>%
  group_by(product1,cardid) %>%
  arrange(desc(cosine_simil))

# rbind and arrange  per product 20 similar products 
top_simil_df <- top_rrop_similarity_df

 # remove duplicates and keep row with high cosine simil
top_simil_df <- top_simil_df[!duplicated(top_simil_df[,c('product1','product2')]),]

top_simil_df <- top_simil_df %>%
  group_by(product1,cardid) %>%
  arrange(desc(cosine_simil))  %>%
  slice(1:50)

#### for hetrogeneous merge with prod rank   ####
products <- dbGetQuery(igpnewConnProd, "select prod_id,rank,ptid,ei.flag_hamper,p.products_name_for_url  from products p join prod_rank pr  on p.products_id = pr.prod_id join product_cat pc on p.products_id = pc.pid join newigp_category_extra_info ci on pc.ptid = ci.categories_id join newigp_product_extra_info ei on pr.prod_id = ei.products_id where ci.cat_type =1  and  card_id in (121,19870) order by rank")

## hampers tagged in multiple ptid ----> do unique 
products <- products[!duplicated(products$prod_id,products$ptid) ,]

top_simil_df<- merge(top_simil_df,products,by.x = 'product2', by.y='prod_id')
top_simil_df <- top_simil_df %>%
  group_by(product1,cardid) %>%
  arrange((rank))  %>%
  slice(1:50)

######################### to just have one product per pt in strip of 16 #############################################################
top_simil_df <- top_simil_df[top_simil_df['product1'] == 521298, ]
top_simil_df  <- top_simil_df %>%
  group_by(product1,cardid,ptid) %>%
  arrange((rank))  %>%
  slice(1:2)

#############################make strip of 16 products ###########################################3333
top_simil_df  <- top_simil_df %>%
  group_by(product1,cardid) %>%
  arrange((rank))  %>%
  slice(1:16) 

################end ##############################################################################



###### non-rrop 
distinct_cardid <- dbGetQuery(igpnewConnProd, "select distinct(card_id) as cardid from cards_url where type != Category and card_id = 19870" )
attributes_similarity_df = do.call(rbind,products_cosine_measure(set_type = FALSE,distinct_cardid))


#################   Manipulation
top_attributes_similarity_df <- attributes_similarity_df %>%
  group_by(product1,cardid) %>%
  arrange(desc(cosine_simil)) 
  
# rbind and arrange  per product 20 similar products 
top_simil_df <- top_attributes_similarity_df

# arrange per product 20 products in descending order of simil score
top_simil_df <- top_simil_df %>%
  group_by(product1,cardid) %>%
  arrange(desc(cosine_simil)) 
  
# remove duplicates and keep row with high cosine simil
top_simil_df <- top_simil_df[!duplicated(top_simil_df[,c('product1','product2')]),]

top_simil_df <- top_simil_df %>%
  group_by(product1,cardid) %>%
  arrange(desc(cosine_simil))  %>%
  slice(1:16)


################end ##############################################################################


################### url wise
#top_simil_df$url <- strsplit(top_simil_df$products_name_for_url,"-")[[1]][(length(strsplit(top_simil_df$products_name_for_url,"-"))-3) : (length(strsplit(top_simil_df$products_name_for_url,"-"))-1)] 
#x <- top_simil_df
#x$url <- strsplit(top_simil_df$products_name_for_url,"-")
#x$new_url = ''
#x$new_url <- x$url[[1]][(length(x$url[[1]])-3):(length(x$url[[1]])-1)]

#for (i in 1:5)
#{
#  x$new_url[i] <- x$url[i][[1]][(length(x$url[i][[1]])-3):(length(x$url[i][[1]])-1)]
#}

####### Hetrogenicity by flag_hamper ####################

# with_flag_hamper <- top_simil_df[top_simil_df['flag_hamper'] == 1 ,]
# with_flag_hamper <- with_flag_hamper[!duplicated(with_flag_hamper$ptid) ,]
# 
# without_flag_hamper <- top_simil_df[top_simil_df['flag_hamper'] == 0 ,]
# without_flag_hamper <- without_flag_hamper[!duplicated(without_flag_hamper$ptid),]
# 
# final_df <- rbind(with_flag_hamper,without_flag_hamper)
# 


################################################## het by file attr 
# dbConnectNewIgp()
# sql_query = "select t1.products_id,t2.label product_label,t3.label as set_label,t2.attr_set_id from products p, newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where p.products_id = t1.products_id and t1.attr_val_id = t2.id and t2.attr_set_id = t3.id and p.products_status = 1 and t3.attr_set_type != 1 "
# sql_query <-  gsub("c\\(","\\(" , sql_query)
# label_data <- dbGetQuery(igpnewConnProd,sql_query)
# 
# ## temp_merge
# label_top_simil_df <- merge(top_simil_df,label_data,by.x='product2',by.y='products_id')
# 
# ### read het level data
# pt_level_het_data<-read.csv(paste(getwd(),"/pt_level_attr.csv",sep='') , header = TRUE)
# 
# ##merge
# pt_level_het_label_data <- merge(label_top_simil_df,pt_level_het_data,by.x= c('ptid','attr_set_id'), by.y=c('pt_id','attr_set_id'))
