# source("packages.R")
# source("database_connection.R")
# source("logging.R")
# # list of packages required for the session
# packages_required= c("tm","lsa","SnowballC","smdc")
# 
# # install from CRAN , if not already installed
# 
# install_check<- packages_required %in% installed.packages()
# if(length(packages_required[!install_check])>0) install.packages(packages_required[!install_check],dependencies = TRUE)
# 
# # Load required libraries for the session
# lapply(packages_required, FUN = function(X) {
#   do.call("require", list(X)) 
# })
# Dummy description : to be replaced by the actual product description column from the product master"---
#--------------------------------------------------------------------------------------------------------
sql_query = "select products_name from products p join prod_rank pr on p.products_id = pr.prod_id and pr.card_id = 121;"
#sql_query = paste("select products_id,mpl from newigp_product_extra_info where  products_id in (", paste(cardid_products,collapse = ','), ')', sep='')
sql_query <-  gsub("c\\(","\\(" , sql_query)
products_mpl <- dbGetQuery(igpnewConnProd,sql_query)
df <- data.frame(products_mpl, stringsAsFactors = FALSE)

#------------------------------------------------------------------------------
corpus <- Corpus(VectorSource(df$products_name))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
td.mat <- TermDocumentMatrix(corpus)
# inspect(td.mat[1:10,1:10])

#td.mat<-create_tdm(df)

#------------------------------------------------------------------------------
# MDS with raw term-document matrix compute distance matrix
dist.mat <- dist(t(as.matrix(td.mat)))
#------------------------------------------------------------------------------
# MDS with LSA
lsaSpace <- lsa(td.mat)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) 
# compute distance matrix
df.dist=as.matrix(dist.mat.lsa, labels=TRUE)
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
# (subtract from 1 to get dissimilarity matrix)
distMatrix <- cosine(lsaMatrix)


output <- merge(output,products_text,by.x='product1',by.y='seq')
output <- output[c('products_id','product2','cosine_simil')]
colnames(output) <- c('product1','product2','text_cosine_simil')
output <- merge(output,products_text,by.x='product2',by.y='seq')
output <- output[c('product1','products_id','text_cosine_simil')]