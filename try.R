rm(list=ls())
.libPaths("/home/ruser2/R/x86_64-pc-linux-gnu-library/3.2")
# list of packages required for the session
packages_required= c("RMySQL","reshape2","magrittr","dplyr","stringr","stringi")

# install from CRAN , if not already installed
install_check<- packages_required %in% installed.packages()
if(length(packages_required[!install_check])>0) install.packages(packages_required[!install_check],dependencies = TRUE)

# Load required libraries for the session
lapply(packages_required, FUN = function(X) {
  do.call("require", list(X))
})

start_time<-Sys.time()
#logging
library(logging)
basicConfig(level = 'FINEST')
addHandler(writeToFile, file="~/logs.log", level='DEBUG')
with(getLogger(), names(handlers))
loginfo(paste('Editors pick products run started',Sys.time(),sep=" "))


lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

# function to establish MySqlconnection with R server
dbConnectNewIgp<- function(){
  igpnewConnProd <<- dbConnect(MySQL(),user='root',password='password', dbname='igpnew', host='127.0.0.1',port=3306)  
}
dbConnectNewIgp()

cards_url<-dbGetQuery(igpnewConnProd,'select * from cards_url')
card_mapping<-dbGetQuery(igpnewConnProd,'select * from cards_parent_to_child_mapping')
prod_rank<-dbGetQuery(igpnewConnProd,'select * from prod_rank;')
card_details<-suppressWarnings(dbGetQuery(igpnewConnProd, 'select * from newigp_master_cards_collection;'))
#mpl<-dbGetQuery(igpnewConnProd, 'select ei.products_id, ei.mpl from newigp_product_extra_info ei join products p on p.products_id = ei.products_id where p.products_status = 1;')
mpl<-dbGetQuery(igpnewConnProd, 'select ei.products_id, case when p.products_mrp < 1000 then 1 else case when p.products_mrp < 2500 then 2 else 3 end end as "mpl" from newigp_product_extra_info ei join products p on p.products_id = ei.products_id where p.products_status = 1;')
prod_gid<-dbGetQuery(igpnewConnProd,'SELECT products_id, CASE group_id WHEN 0 THEN products_id ELSE group_id END AS "gid" FROM newigp_product_extra_info;')
newigp_top_product<- data.frame(url=character(0),type=numeric(0),pid=numeric(0),rank=numeric(0))

rel_cards_prods<- merge(x = card_mapping,y=prod_rank,by.x="card_id_n",by.y = "card_id")
rel_cards_prods<- rel_cards_prods[order(rel_cards_prods$card_id_n,rel_cards_prods$rank),]
rel_cards_prods_mpl<- merge(x = rel_cards_prods, y = mpl, by.x="prod_id", by.y="products_id") 
#rel_cards_modest<-rel_cards_prods_mpl[rel_cards_prods_mpl$mpl==1,]
#card_slice_modest<-rel_cards_modest %>%
#  group_by(card_url_p,mpl) %>%
#  arrange(rank) %>%
#  distinct(card_id_p,prod_id,.keep_all=TRUE)%>%
#  slice(1:6)

rel_cards_premium<-rel_cards_prods_mpl[rel_cards_prods_mpl$mpl==2,]
card_slice_premium<-rel_cards_premium %>%
  group_by(card_url_p,mpl) %>%
  arrange(rank) %>%
  distinct(card_id_p,prod_id,.keep_all=TRUE)%>%
  slice(1:12)

rel_cards_luxury<-rel_cards_prods_mpl[rel_cards_prods_mpl$mpl==3,]
card_slice_luxury<-rel_cards_luxury %>%
  group_by(card_url_p,mpl) %>%
  arrange(rank) %>%
  distinct(card_id_p,prod_id,.keep_all=TRUE)%>%
  slice(1:4)
#card_slice_all<-as.data.frame(rbind(card_slice_modest,card_slice_premium,card_slice_luxury))
card_slice_all<-as.data.frame(rbind(card_slice_premium,card_slice_luxury))
card_slice<-card_slice_all %>%
  group_by(card_url_p) %>%
  arrange(desc(mpl),rank) %>%
  distinct(card_id_p,prod_id,.keep_all=TRUE)%>%
  slice(1:36)

card_slice_all<-as.data.frame(card_slice)
card_slice_all$type<-0
newigp_top_product<-card_slice_all[c('card_id_p','card_url_p','type','prod_id','rank')]
colnames(newigp_top_product)<-c("id","url","type","pid","rank")
newigp_top_product$rank<-as.numeric(newigp_top_product$rank)

newigp_top_product<-merge(newigp_top_product,prod_gid, by.x = "pid", by.y = "products_id")
newigp_top_product<-newigp_top_product[!duplicated(newigp_top_product[c("gid","url")]),]
newigp_top_product<-newigp_top_product[c('id','url','type','pid','rank')]

n<-unique(newigp_top_product$url)
for (i in 1:length(n)){
  newigp_top_product[which(newigp_top_product$url==n[i]),]$rank<- rank(-newigp_top_product[which(newigp_top_product$url==n[i]),]$rank,ties.method = 'first')
}
