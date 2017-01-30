### algo code 
## top 10 relevant products based on sales and revenue and het concept.
## top 10 new arrivals based on recently added products to that card and het concept introduced


###  load libraries 
library(logging)

### logging
start_time<-Sys.time()
basicConfig(level = 'FINEST')
loginfo(paste('Editors pick products run started',Sys.time(),sep=" "))


rm(list=ls())
.libPaths("/home/igp/R/x86_64-pc-linux-gnu-library/3.2")

### list of packages required for the session
packages_required= c("RMySQL","reshape2","magrittr","dplyr","stringr","stringi","plyr", "dplyr")

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
last_three_months <- todays_date - 30

#####DB Queries 

sales_history <- dbGetQuery(igpnewConnProd, paste("SELECT count(op.products_id) sale_count, op.products_id, p.products_mrp, count(op.products_id)*p.products_mrp as revenue , p.products_name_for_url FROM orders o join orders_products op  on o.orders_id = op.orders_id join products p on op.products_id = p.products_id and o.fk_associate_id = 5  and o.orders_status not in ('Cancelled') and date(o.date_purchased) >=", last_three_months, " GROUP BY op.products_id "))
prod_rank <- dbGetQuery(igpnewConnProd,"select * from prod_rank ")
card_mapping<-dbGetQuery(igpnewConnProd,'select * from cards_parent_to_child_mapping')
top_card_products <- dbGetQuery(igpnewConnProd, "select * from prod_rank_ds")


#### merging and manipulations 
pr_card_mapping <- merge(x = card_mapping,y=prod_rank,by.x="card_id_n",by.y = "card_id")
pr_card_mapping <- pr_card_mapping[order(pr_card_mapping$card_id_n,pr_card_mapping$rank),]
####ensure product mapped to multiple cards can have diff ranking  so take lower rank and provide uniqueness
pr_card_mapping <- ddply(pr_card_mapping, c("card_url_p","prod_id"), function(pr_card_mapping) return(pr_card_mapping[pr_card_mapping$rank==min(pr_card_mapping$rank),]))
pr_card_mapping <- pr_card_mapping[!duplicated(pr_card_mapping[,c('card_url_p','prod_id')]),]
sales_pr_card_mapping <- merge(pr_card_mapping,sales_history,by.x='prod_id',by.y='products_id')
sales_pr_card_mapping <- sales_pr_card_mapping[c('card_id_p','card_url_p','prod_id','rank','sale_count','products_mrp','revenue','products_name_for_url')]

x <- 5
while (x > 0)
{
  x <- x - 1
  print(x)
}


### correlation between salecount and revenue
# plot(sales_pr_card_mapping$sale_count,sales_pr_card_mapping$revenue) 
# cor(sales_pr_card_mapping$sale_count,sales_pr_card_mapping$revenue)   ##.46 linearly correlated 

#### normalize 
####  impact of product sale count  among all products sale count 
####  impact of product revenue  among all products revenue
sales_pr_card_mapping$normalized_sale_count <- sapply(sales_pr_card_mapping$sale_count,function(x) (x-min(sales_pr_card_mapping$sale_count))/(max(sales_pr_card_mapping$sale_count)-min(sales_pr_card_mapping$sale_count)))
sales_pr_card_mapping$normalized_revenue <- unlist(lapply(sales_pr_card_mapping$revenue,function(x) (x-min(sales_pr_card_mapping$revenue))/(max(sales_pr_card_mapping$revenue)-min(sales_pr_card_mapping$revenue))))
sales_pr_card_mapping$total_normalized <- 0.6 * sales_pr_card_mapping$normalized_sale_count + 0.4 * sales_pr_card_mapping$normalized_revenue
till_now<-sales_pr_card_mapping
#sales_pr_card_mapping <- till_now

### type 0  for top 10 relevant products
sales_pr_card_mapping$type <- 0
### arrange in desc total normalized 
sales_pr_card_mapping <- sales_pr_card_mapping %>% 
  group_by(card_url_p) %>% 
  arrange(desc(total_normalized)) %>%
  slice(1:20)
sales_pr_card_mapping <- sales_pr_card_mapping %>% 
  group_by(card_url_p) %>% 
  arrange(rank) %>%
  slice(1:10)

#sales_pr_card_mapping$seq_rank <-  with(sales_pr_card_mapping, ave(card_id_p, card_id_p, FUN = seq_along))
#sales_pr_card_mapping <- sales_pr_card_mapping[c('url','type','prod_id','seq_rank','products_name_for_url')]
#sales_pr_card_mapping$id <- seq(1:nrow(sales_pr_card_mapping))
#sales_pr_card_mapping <- sales_pr_card_mapping[c('id','url','type','prod_id','seq_rank','products_name_for_url')]


#####################Top New Arrivals #####################################
dbConnectNewIgp()
### twist top/new or new/top
prod_rank <- dbGetQuery(igpnewConnProd,"select pr.card_id,pr.prod_id,pr.rank,p.products_date_added,p.products_name_for_url from prod_rank pr join products p on pr.prod_id = p.products_id and p.products_status=1 ")
card_mapping<-dbGetQuery(igpnewConnProd,'select * from cards_parent_to_child_mapping where card_url_p = "birthday-gifts" ')
#### merging and manipulations 
pr_card_mapping <- merge(x = card_mapping,y=prod_rank,by.x="card_id_n",by.y = "card_id")
pr_card_mapping <- ddply(pr_card_mapping, c("card_url_p","prod_id"), function(pr_card_mapping) return(pr_card_mapping[pr_card_mapping$rank==min(pr_card_mapping$rank),]))
pr_card_mapping <- pr_card_mapping[!duplicated(pr_card_mapping[,c('card_url_p','prod_id')]),]
pr_card_mapping <- pr_card_mapping[c('card_id_p','card_url_p','prod_id','rank','products_date_added','products_name_for_url')]


pr_card_mapping <- pr_card_mapping %>% 
  group_by(card_url_p) %>% 
  arrange(desc(products_date_added)) %>%
  slice(1:50)
pr_card_mapping <- pr_card_mapping %>% 
  group_by(card_url_p) %>% 
  arrange(rank) %>%
  slice(1:10)
