###  load libraries 
library(logging)

### logging
start_time<-Sys.time()
basicConfig(level = 'FINEST')
loginfo(paste('Editors pick products run started',Sys.time(),sep=" "))


rm(list=ls())
.libPaths("/home/igp/R/x86_64-pc-linux-gnu-library/3.2")

### list of packages required for the session
packages_required= c("RMySQL","reshape2","magrittr","dplyr","stringr","stringi")

### install from CRAN , if not already installed
install_check<- packages_required %in% installed.packages()
if(length(packages_required[!install_check])>0) install.packages(packages_required[!install_check],dependencies = TRUE)

### Load required libraries for the session
lapply(packages_required, FUN = function(X) {
  do.call("require", list(X))
})


### to disconnect all the existing connections. 
lapply( dbListConnections(dbDriver( drv = "MySQL")), dbDisconnect)
# function to establish MySqlconnection with R server
dbConnectNewIgp<- function(){
  igpnewConnProd <<- dbConnect(MySQL(),user='root',password='password', dbname='igpnew', host='127.0.0.1',port=3306)  
}
dbConnectNewIgp()


###date
todays_date <- Sys.Date()
last_three_months <- todays_date - 90

#####DB Queries 
sql_query <- paste("SELECT count(op.products_id) sale_count, op.products_id, p.products_mrp, count(op.products_id)*p.products_mrp as revenue , p.products_name_for_url FROM orders o join orders_products op  on o.orders_id = op.orders_id join products p on op.products_id = p.products_id and o.fk_associate_id = 5  and o.orders_status not in ('Cancelled') and date(o.date_purchased) >=", last_three_months, " GROUP BY op.products_id ")
sales_history <- dbGetQuery(igpnewConnProd, sql_query)
prod_rank <- dbGetQuery(igpnewConnProd,"select pr.card_id,prod_id,cu.url from prod_rank pr join cards_url cu on pr.card_id = cu.card_id")
home_page_dataframe <- dbGetQuery(igpnewConnProd,"select distinct(prod_id) from prod_rank")
home_page_dataframe$card_id <- 0
home_page_dataframe$url <- 'home'
prod_rank <- rbind(prod_rank,home_page_dataframe)

### correlation between salecount and revenue
# plot(sales_history$sale_count,sales_history$revenue) 
# cor(sales_history$sale_count,sales_history$revenue)   ##.81 linearly higly correlated 

#### normalize
sales_history$normalized_sale_count <- sapply(sales_history$sale_count,function(x) (x-min(sales_history$sale_count))/(max(sales_history$sale_count)-min(sales_history$sale_count)))
sales_history$normalized_revenue <- sapply(sales_history$revenue,function(x) (x-min(sales_history$revenue))/(max(sales_history$revenue)-min(sales_history$revenue)))
sales_history$total_normalized <- 0.5 * sales_history$normalized_sale_count + 0.5 * sales_history$normalized_revenue

### scale
# sales_history$scaled_sale_count <- scale(sales_history$sale_count) 
# sales_history$scaled_revenue <- scale(sales_history$revenue)

### normal distribution
# sales_history$rnorm_sale_count <- rnorm(sales_history$sale_count , mean = 0 ,sd= 1 ) 
# sales_history$rnorm_revenue <- rnorm(sales_history$revenue , mean = 0, sd = 1)
# sales_history$total_rnorm_normalized <- sales_history$rnorm_sale_count + sales_history$rnorm_revenue
# sales_history <- sales_history[order(-sales_history$total_rnorm_normalized),]
# sales_history_rnorm <- sales_history[1:20,]


#######merge
prod_rank_sales_history <- merge(prod_rank,sales_history, by.x='prod_id',by.y='products_id')

### type 0  for top 10 relevant products
prod_rank_sales_history$type <- 0
### arrange in desc total normalized 
prod_rank_sales_history <- prod_rank_sales_history %>% 
  group_by(card_id) %>% 
  arrange(desc(total_normalized)) %>%
  slice(1:10)
prod_rank_sales_history$rank <-  with(prod_rank_sales_history, ave(card_id, card_id, FUN = seq_along))
prod_rank_sales_history <- prod_rank_sales_history[c('url','type','prod_id','rank','products_name_for_url')]
prod_rank_sales_history$id <- seq(1:nrow(prod_rank_sales_history))
prod_rank_sales_history <- prod_rank_sales_history[c('id','url','type','prod_id','rank','products_name_for_url')]


