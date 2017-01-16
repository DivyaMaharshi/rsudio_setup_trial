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

create_tdm<-function(text_field){
  corpus <- Corpus(VectorSource(text_field))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  td.mat <- TermDocumentMatrix(corpus)
  #td.mat <- removeSparseTerms(dtm, 0.1)
}

lsa_m <- function (x, dims = dimcalc_share()) 
{
  SVD = svd(x)
  if (is.function(dims)) {
    dims = dims(SVD$d)
  }
  if (dims < 2) 
    dims = 2
  
  # if (any(SVD$d <= sqrt(.Machine$double.eps))) {
  #   warning("[lsa] - there are singular values which are zero.")
  # }
  space = NULL
  space$tk = SVD$u[, 1:dims]
  space$dk = SVD$v[, 1:dims]
  space$sk = SVD$d[1:dims]
  rownames(space$tk) = rownames(x)
  rownames(space$dk) = colnames(x)
  class(space) = "LSAspace"
  return(space)
}

#### text cosine computation
text_cosine_similarity <- function(td.mat,products_text){
  # MDS with LSA
  lsaSpace <- lsa_m(td.mat)  # create LSA space
  dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) 
  # compute distance matrix
  df.dist=as.matrix(dist.mat.lsa, labels=TRUE)
  lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
  # Use the `cosine` function in `lsa` package to get cosine similarities matrix
  output <- cosine(lsaMatrix)
  ## melt
  output <- melt(as.matrix(output), varnames=c("row","col"))
  names(output) <- c('product1','product2','cosine_simil')
  ### adding products id's to sequence
  output <- merge(output,products_text,by.x='product1',by.y='seq')
  output <- output[c('products_id','product2','cosine_simil','text')]
  colnames(output) <- c('product1','product2','text_cosine_simil','product1_text')
  output <- merge(output,products_text,by.x='product2',by.y='seq')
  output <- output[c('product1','products_id','text_cosine_simil','product1_text','text')]
  colnames(output) <- c('product1','product2','text_cosine_simil','product1_text','product2_text')
  return(output)
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
    ### product name context similarity code
    sql_query = paste("select  products_id,products_name as text from products p join prod_rank pr on p.products_id = pr.prod_id and pr.card_id = ",cardid,seq='')
    #sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(cardid_products,collapse = ','), ')', sep='')
    sql_query <-  gsub("c\\(","\\(" , sql_query)
    products_text <- dbGetQuery(igpnewConnProd,sql_query)
    products_text$seq <- seq(1:nrow(products_text))
    #only text required to compute textual cosine similarity
    products_text_only <- products_text[c('text')]
    
    ### method call
    td.mat<-create_tdm(products_text_only$text)
    text_cosine_simil <- text_cosine_similarity(td.mat,products_text) 
    text_cosine_simil <- text_cosine_simil[c('product1','product2','text_cosine_simil')]
    
    
    ##############################################################
    sql_query = paste("select distinct(prod_id) as pid from products p join prod_rank pr on p.products_id = pr.prod_id where p.products_status = 1  and card_id  = ", cardid, sep='')
    cardid_products <- dbGetQuery(igpnewConnProd,sql_query)
    View(cardid_products)
    if(nrow(cardid_products) !=0)
    {
      cardid_products <- as.vector(cardid_products$pid,mode='numeric')
      cardid_products <- as.vector(cardid_products)
      
      ###rrop card_id computre similarity based on  all sets
      if (set_type == TRUE){
        sql_query = paste("select t1.products_id,t2.label,t1.weightage from newigp_products_to_attr_val t1, newigp_master_attr_vals t2, newigp_master_attr_sets t3 where t1.attr_val_id = t2.id and t2.attr_set_id =t3.id   and   t1.products_id in (",paste(cardid_products,collapse = ','),")",sep='')
        sql_query <-  gsub("c\\(","\\(" , sql_query)
        rrop_data <- dbGetQuery(igpnewConnProd,sql_query)
        View(rrop_data)
        xx <- rrop_data 
        
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
        rrop_data <- dcast(rrop_data,  products_id ~ label, value.var='weightage' , fun=mean)
        
        ## Addition of MPL to the output
        sql_query = paste("select products_id,products_mrp from products where  products_id in (", paste(cardid_products,collapse = ','), ')', sep='')
        #sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(cardid_products,collapse = ','), ')', sep='')
        sql_query <-  gsub("c\\(","\\(" , sql_query)
        products_mpl <- dbGetQuery(igpnewConnProd,sql_query)
        
        
        #merging rrop data with mpl column
        rrop_data = merge(rrop_data,products_mpl,by.x='products_id',by.y='products_id')
        rrop_data[is.na(rrop_data)] <- 0
        View(rrop_data)
        
        ### append text only product score 
      
        # extract products id 
        actual_product_ids <- as.data.frame(rrop_data$products_id)
        actual_product_ids$seq_numbers <- seq(1:nrow(rrop_data))
        
        
        # remove products_id for simil function
        rrop_data <- within(rrop_data,rm(products_id))
        
        # simil calculation 
        cosine_simil <-simil(rrop_data,method = 'cosine',upper = TRUE, diag = TRUE)
        cosine_df <- melt(as.matrix(cosine_simil), varnames=c("row","col"))
        names(cosine_df) <- c('product1','product2','cosine_simil')
      
        # merge
        final_df_with_productids <- merge(cosine_df,actual_product_ids,by.x='product1',by.y='seq_numbers')
        final_df_with_productids <- merge(final_df_with_productids,actual_product_ids,by.x='product2',by.y='seq_numbers')
        names(final_df_with_productids) <- c('p1','p2','cosine_simil','product1','product2')
        final_df_with_productids <- final_df_with_productids[c('product1','product2','cosine_simil')]
        final_df_with_productids$cardid <- cardid
        final_df_with_productids <- merge(final_df_with_productids,text_cosine_simil,by.x=c('product1','product2'),by.y=c('product1','product2'))
        temp_list[[i]] <- final_df_with_productids
        
      }
    }
  }
  return(temp_list)
}


############################################## RROP cards #######################################################
card_id = 152
product_idd = 521458

sql_query = paste("select distinct(card_id) as cardid from cards_url where type != 'Category' and card_id = ",card_id,sep=' ')
distinct_cardid <- dbGetQuery(igpnewConnProd, sql_query)
rrop_similarity_df = do.call(rbind, products_cosine_measure(set_type = TRUE,distinct_cardid))
rrop_similarity_df$cosine_simil <- rrop_similarity_df$cosine_simil + rrop_similarity_df$text_cosine_simil
rrop_similarity_df <- rrop_similarity_df[-5]

### Calculation
### cosine arrange
top_rrop_similarity_df<-rrop_similarity_df %>%
group_by(product1,cardid) %>%
arrange(desc(cosine_simil))
top_simil_df <- top_rrop_similarity_df

### for perticular product check
top_simil_df <- top_simil_df[top_simil_df['product1'] ==  product_idd, ]

### select cosine based 100 products
top_simil_df <- top_simil_df %>%
group_by(product1,cardid) %>%
arrange(desc(cosine_simil))  %>%
slice(1:100)

###for hetrogeneous merge with prod rank
### products 1 price
sql_query = paste("select prod_id,p.products_mrp as product1_price  from products p join prod_rank pr  on p.products_id = pr.prod_id join product_cat pc on p.products_id = pc.pid join newigp_category_extra_info ci on pc.ptid = ci.categories_id join newigp_product_extra_info ei on pr.prod_id = ei.products_id where ci.cat_type =1 and card_id = ",card_id, "order by rank", sep=' ')
products1_price <- dbGetQuery(igpnewConnProd ,sql_query)
products1_price <- unique(products1_price)
top_simil_df    <- merge(top_simil_df,products1_price,by.x = 'product1', by.y='prod_id')

###product2 price
sql_query <- paste("select prod_id,rank,ptid,ei.flag_hamper,p.products_name_for_url,p.products_mrp as product2_price  from products p join prod_rank pr  on p.products_id = pr.prod_id join product_cat pc on p.products_id = pc.pid join newigp_category_extra_info ci on pc.ptid = ci.categories_id join newigp_product_extra_info ei on pr.prod_id = ei.products_id where ci.cat_type =1 and card_id = ", card_id, "order by rank", sep=' ')
products <- dbGetQuery(igpnewConnProd,sql_query)
###hampers tagged in multiple ptid ----> do unique
products <- products[!duplicated(products$prod_id,products$ptid) ,]


### price logic on top of cosine
top_simil_df = top_simil_df %>%
inner_join(products, by = c("product2" = "prod_id")) %>%
#filter(product2_price >= product1_price | product2_price-product1_price >= -(product1_price * .60) )
filter(product2_price >= (product1_price - (product1_price * .25)) & product2_price <= (product1_price  + (product1_price * 1.3)))

###select hetrogeneous based on simil/rank
top_simil_df <- top_simil_df %>%
group_by(product1,cardid) %>%
arrange(desc(cosine_simil))  %>%
slice(1:30)

######################### to just have count product per pt in strip of 16 (similar products should be shown) #############################################################
# top_simil_df  <- top_simil_df %>%
#   group_by(product1,cardid,ptid) %>%
#   arrange((cosine_simil))  %>%
#   slice(1:2)

#############################make strip of 16 products ###########################################
top_simil_df  <- top_simil_df %>%
  group_by(product1,cardid) %>%
  arrange(rank)  %>%
  slice(1:16)

rrop_final_df <- top_simil_df
#################end ##############################################################################


###### for categories  
distinct_cardid <- dbGetQuery(igpnewConnProd, "select distinct(card_id) as cardid from cards_url where type != Category and card_id = 10987" )
attributes_similarity_df = do.call(rbind,products_cosine_measure(set_type = FALSE,distinct_cardid))
attributes_similarity_df$cosine_simil <- attributes_similarity_df$cosine_simil + attributes_similarity_df$text_cosine_simil
attributes_similarity_df <- attributes_similarity_df[-5]


#################   calculations 
top_attributes_similarity_df <- attributes_similarity_df %>%
group_by(product1,cardid) %>%
arrange(desc(cosine_simil)) 

# rbind and arrange  per product 20 similar products 
top_simil_df <- top_attributes_similarity_df
top_simil_df <- top_simil_df[top_simil_df['product1'] ==  215504, ]

####### price calculation 
products1_price <- dbGetQuery(igpnewConnProd ,"select prod_id,p.products_mrp as product1_price  from products p join prod_rank pr  on p.products_id = pr.prod_id join product_cat pc on p.products_id = pc.pid join newigp_category_extra_info ci on pc.ptid = ci.categories_id join newigp_product_extra_info ei on pr.prod_id = ei.products_id where ci.cat_type =1  and p.products_status = 1 and card_id = 10987 order by rank")
products1_price <- unique(products1_price)
top_simil_df    <- merge(top_simil_df,products1_price,by.x = 'product1', by.y='prod_id')

###product2 price
products <- dbGetQuery(igpnewConnProd, "select prod_id,rank,ptid,ei.flag_hamper,p.products_name_for_url,p.products_mrp as product2_price  from products p join prod_rank pr  on p.products_id = pr.prod_id join product_cat pc on p.products_id = pc.pid join newigp_category_extra_info ci on pc.ptid = ci.categories_id join newigp_product_extra_info ei on pr.prod_id = ei.products_id where ci.cat_type =1  and p.products_status = 1 and card_id = 10987 order by rank")
## hampers tagged in multiple ptid ----> do unique 
products <- products[!duplicated(products$prod_id,products$ptid) ,]

#price logic on top of cosine 
top_simil_df = top_simil_df %>%
  inner_join(products, by = c("product2" = "prod_id")) %>% 
  filter(product2_price >= (product1_price - (product1_price * .25)) & product2_price <= (product1_price  + (product1_price * 1.5)))


#arrange per product 20 products in descending order of simil score
# selecting only 16 products change it to 30 if required
top_simil_df <- top_simil_df %>%
  group_by(product1,cardid) %>%
  arrange(desc(cosine_simil)) %>%
  slice(1:16)

##### based on rank hetrogenous 
top_simil_df  <- top_simil_df %>%
   group_by(product1,cardid) %>%
   arrange(rank)  %>%
   slice(1:16)
 
category_final_df <- top_simil_df
################end ##############################################################################

