#### To kill db connection 
rm(list=ls())

## lib path
.libPaths("/home/igp/R/x86_64-pc-linux-gnu-library/3.2")

## to kill db connection 
library(RMySQL)  
killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}
killDbConnections()

#### start time
start_time<-Sys.time()
print(paste("Card to total and luxury product count script started at", start_time, sep = " "))

###install packages
#install.packages(c("data.table","dplyr","stringr"))
library(data.table)
library(dplyr)
library(stringr)

#### db connections
dbConnectNewIgp<- function(){
  igpnewConnProd <<- dbConnect(MySQL(),user='root',password='password', dbname='igpnew', host='127.0.0.1',port=3306)  
}
dbConnectNewIgp()


####home pages
fest_urls<-dbGetQuery(igpnewConnProd,'select cu.*, cc.base_level, cc.title from cards_url cu join newigp_master_cards_collection cc on cu.card_id = cc.card_id where cu.type = "Festival";')
card_prod_count<-dbGetQuery(igpnewConnProd,'SELECT pr.card_id, count(distinct(CASE group_id WHEN 0 THEN products_id ELSE group_id END)) AS "count", (count(distinct(CASE WHEN ei.mpl = 3 THEN CASE group_id WHEN 0 THEN products_id ELSE group_id END ELSE 0 END)) -1)  AS "luxury_count" FROM newigp_product_extra_info ei JOIN prod_rank pr on pr.prod_id = ei.products_id GROUP BY pr.card_id;')
prod_fest<-dbGetQuery(igpnewConnProd, 'select pr.card_id, pr.prod_id, pr.rank, cu.type2 from prod_rank pr join cards_url cu on cu.card_id = pr.card_id join newigp_master_cards_collection cc on cu.card_id = cc.card_id where cu.type = "Festival" and pr.rank < 13 group by pr.prod_id;')
prod_sdd<-dbGetQuery(igpnewConnProd, 'select pr.prod_id, pr.rank, pr.card_id, cc.title, cu.url, case when url = "flowers" then 1 else 2 end as "sort" from prod_rank pr join cards_url cu on cu.card_id = pr.card_id join newigp_master_cards_collection cc on cu.card_id = cc.card_id join products p on p.products_id = pr.prod_id where cu.url in ("flowers","cakes") and p.fk_associate_id = 72 and pr.rank < 20 group by pr.prod_id order by sort, rank;')
prod_luxury<-dbGetQuery(igpnewConnProd, 'select pr.prod_id, pr.rank, pr.card_id as strip_card_id, cu.url, case when cu.sub_category = "cakes" or cu.sub_category = "flowers" then cu.sub_category else cu.category end as "name" from newigp_product_extra_info ei join prod_rank pr on ei.products_id = pr.prod_id join cards_url cu on cu.card_id = pr.card_id where cu.type="Category" and  (cu.type2 = "C" or cu.url in ("cakes","flowers"))  and ei.mpl = 3  group by pr.prod_id order by rank;')
cards_url<-dbGetQuery(igpnewConnProd,'select cu.*, cc.title from cards_url cu join newigp_master_cards_collection cc on cu.card_id = cc.card_id where cu.flag = "home";')
extra<-dbGetQuery(igpnewConnProd, 'select ei.* from newigp_product_extra_info ei join products p on p.products_id = ei.products_id where p.products_status = 1;')
cat_data<-dbGetQuery(igpnewConnProd,'select categories.categories_id,categories.categories_name from categories inner join newigp_category_extra_info on categories.categories_id = newigp_category_extra_info.categories_id')
countries<-dbGetQuery(igpnewConnProd, 'select * from countries;')
cities<-dbGetQuery(igpnewConnProd, 'select * from cities;')
t1<-dbGetQuery(igpnewConnProd, 'select concat(",",group_concat(distinct p.city_id separator ","),",") as "city_id" from pincode p join cards_url cu on cu.country_city = p.city where cu.type2 = "City" and p.type = 1;')
t2<-dbGetQuery(igpnewConnProd, 'select concat(",",group_concat(distinct p.city_id separator ","),",") as "city_id" from pincode p join cards_url cu on cu.country_city = p.city where cu.type2 = "City" and p.type = 2;')
t1<-t1[1,1]
t2<-t2[1,1]
prod_cat<-dbGetQuery(igpnewConnProd, 'select pr.prod_id, pr.rank, case when cu.sub_category = "cakes" or cu.sub_category = "flowers" then cu.sub_category else cu.category end as "name" from prod_rank pr join cards_url cu on cu.card_id = pr.card_id where (cu.type = "Category" and cu.url != "flowers-cakes") and (cu.type2 = "C" or cu.url in ("cakes","flowers")) and pr.rank < 250 group by pr.prod_id;')
prod_cat_fs<-dbGetQuery(igpnewConnProd, 'select pr.prod_id, pr.rank, "Free-Shipping" as name from prod_rank pr join cards_url cu on cu.card_id = pr.card_id where cu.type = "FS" group by pr.prod_id order by pr.rank;')
prod_map_cc<-dbGetQuery(igpnewConnProd, 'select pr.prod_id, case when cu.sub_category = "cakes" or cu.sub_category = "flowers" then cu.sub_category else cu.category end as "name" from prod_rank pr join cards_url cu on cu.card_id = pr.card_id where (cu.type = "Category" and cu.url != "flowers-cakes") and (cu.type2 = "C" or cu.url in ("cakes","flowers"));')
prod_map_cc_fs <- dbGetQuery(igpnewConnProd, 'select pr.prod_id, "Free-Shipping" as "name" from prod_rank pr join products p on pr.prod_id = p.products_id join cards_url cu on cu.card_id = pr.card_id where cu.type = "FS" and p.products_status = 1;') 
prod_map_ei<-dbGetQuery(igpnewConnProd,'SELECT products_id, CASE group_id WHEN 0 THEN products_id ELSE group_id END AS "group_id" FROM newigp_product_extra_info ei;')
urls<-dbGetQuery(igpnewConnProd, 'select case when cu.sub_category = "cakes" or cu.sub_category = "flowers" then cu.sub_category else cu.category end as "name", cu.url from prod_rank pr join cards_url cu on cu.card_id = pr.card_id where (cu.type = "Category" and cu.url != "flowers-cakes") and (cu.type2 = "C" or cu.url in ("cakes","flowers")) group by cu.card_id;')
cjdt <- function(a,b){
  cj = CJ(1:nrow(a),1:nrow(b))
  cbind(a[cj[[1]],],b[cj[[2]],])
}

## Countries and city ##################
##prod_candidates
prod_candidates_nfs<-extra[which(extra$exc_countries!="all" & extra$flag_fs==0),][c('products_id','inc_countries','exc_countries','flag_fs')]
prod_candidates_fs<-extra[which(extra$exc_countries!="all" & extra$flag_fs==1),][c('products_id','inc_countries','exc_countries','flag_fs')]
prod_candidates <- rbind(prod_candidates_nfs,prod_candidates_fs)

##cc_fitlering
cc_co<-cards_url[cards_url$type=="CCS" & cards_url$type2=="Country",]
cc_map_all<-cjdt(cc_co[c('card_id','url','country_city')],prod_candidates[c('products_id')])
colnames(cc_map_all)<-c('card_id','url','country_city','prod_id')
cc_filtering<-merge(cc_map_all,prod_candidates,by.x='prod_id',by.y='products_id')
cc_filtering<-merge(cc_filtering,countries[c('countries_id','countries_name')],by.x='country_city',by.y='countries_name')
cc_filtering_all<-cc_filtering[which(cc_filtering$inc_countries == "all" | cc_filtering$inc_countries == ""),]
cc_filtering_inc<-cc_filtering[which(cc_filtering$inc_countries != "all" & cc_filtering$inc_countries != ""),]
if (nrow(cc_filtering_inc) > 0) {
  cc_filtering_inc$inc_countries<-paste(",",cc_filtering_inc$inc_countries,",",sep="")
  cc_filtering_inc$countries_id<-paste(",",cc_filtering_inc$countries_id,",",sep="")
  for(i in 1:nrow(cc_filtering_inc)){
    sstr<-cc_filtering_inc[i,]$countries_id
    if (!grepl(sstr,cc_filtering_inc[i,]$inc_countries,fixed=TRUE)) {
      cc_filtering_inc[i,]$card_id<-'NA'   
    }
  }
  cc_filtering_inc<-cc_filtering_inc[which(cc_filtering_inc$card_id != 'NA'),]
}
cc_filtering<-rbind(cc_filtering_all,cc_filtering_inc)
cc_filtering_all<-cc_filtering[which(cc_filtering$exc_countries == ""),]
cc_filtering_exc<-cc_filtering[which(cc_filtering$exc_countries != ""),]
if (nrow(cc_filtering_exc) > 0) {
  cc_filtering_exc$exc_countries<-paste(",",cc_filtering_exc$exc_countries,",",sep="")
  cc_filtering_exc$countries_id<-paste(",",cc_filtering_exc$countries_id,",",sep="")
  for(i in 1:nrow(cc_filtering_exc)){
    sstr<-cc_filtering_exc[i,]$countries_id
    if (grepl(sstr,cc_filtering_exc[i,]$exc_countries,fixed=TRUE)) {
      cc_filtering_exc[i,]$card_id<-'NA'   
    }
  }
  cc_filtering_exc<-cc_filtering_exc[which(cc_filtering_exc$card_id != 'NA'),]
}
cc_map_all<-rbind(cc_filtering_all,cc_filtering_exc)
cc_map_all$qp<-"country"
country_map<-cc_map_all[c('card_id','prod_id','qp')]

## Cities ##################
prod_candidates<-extra[extra$exc_cities!="all",][c('products_id','inc_cities','exc_cities')]
cc_co<-cards_url[cards_url$type=="CCS" & cards_url$type2=="City",]
cc_map_all<-cjdt(cc_co[c('card_id','url','country_city')],prod_candidates[c('products_id')])
colnames(cc_map_all)<-c('card_id','url','country_city','prod_id')
cc_filtering<-merge(cc_map_all,prod_candidates,by.x='prod_id',by.y='products_id')
cc_filtering<-merge(cc_filtering,cities,by.x='country_city',by.y='name')
cc_filtering_all<-cc_filtering[which(cc_filtering$inc_cities == "all" | cc_filtering$inc_cities == ""),]
cc_filtering_inc<-cc_filtering[which(cc_filtering$inc_cities != "all" & cc_filtering$inc_cities != ""),]
if (nrow(cc_filtering_inc) > 0) {
  cc_filtering_inc$inc_cities<-paste(",",cc_filtering_inc$inc_cities,",",sep="")
  cc_filtering_inc$id<-paste(",",cc_filtering_inc$id,",",sep="")
  cc_filtering_inc$inc_cities<-str_replace_all(cc_filtering_inc$inc_cities,',1,',t1)
  cc_filtering_inc$inc_cities<-str_replace_all(cc_filtering_inc$inc_cities,',2,',t2)
  for(i in 1:nrow(cc_filtering_inc)){
    sstr<-cc_filtering_inc[i,]$id
    if (!grepl(sstr,cc_filtering_inc[i,]$inc_cities,fixed=TRUE)) {
      cc_filtering_inc[i,]$card_id<-'NA'   
    }
  }
  cc_filtering_inc<-cc_filtering_inc[which(cc_filtering_inc$card_id != 'NA'),]
}
cc_filtering<-rbind(cc_filtering_all,cc_filtering_inc)
cc_filtering_all<-cc_filtering[which(cc_filtering$exc_cities == ""),]
cc_filtering_exc<-cc_filtering[which(cc_filtering$exc_cities != ""),]
if (nrow(cc_filtering_exc) > 0) {
  cc_filtering_inc$exc_cities<-paste(",",cc_filtering_exc$exc_cities,",",sep="")
  cc_filtering_exc$id<-paste(",",cc_filtering_exc$id,",",sep="")
  cc_filtering_exc$exc_cities<-str_replace_all(cc_filtering_exc$exc_cities,',1,',t1)
  cc_filtering_exc$exc_cities<-str_replace_all(cc_filtering_exc$exc_cities,',2,',t2)
  for(i in 1:nrow(cc_filtering_exc)){
    sstr<-cc_filtering_exc[i,]$id
    if (grepl(sstr,cc_filtering_exc[i,]$inc_cities,fixed=TRUE)) {
      cc_filtering_exc[i,]$card_id<-'NA'   
    }
  }
  cc_filtering_exc<-cc_filtering_exc[which(cc_filtering_exc$card_id != 'NA'),]
}
cc_map_all<-rbind(cc_filtering_all,cc_filtering_exc)
cc_map_all$qp<-"city"
city_map<-cc_map_all[c('card_id','prod_id','qp')]

#### binding country_city
final_map<-rbind(city_map,country_map)

#### bind prod_map_cc with fs products 'prod_id','name
prod_map_cc <- rbind(prod_map_cc,prod_map_cc_fs)

#### add count
final_count<-merge(final_map,prod_map_cc, by.x="prod_id", by.y="prod_id")
final_count<-merge(final_count,prod_map_ei, by.x="prod_id", by.y="products_id")
final_count<-final_count[!duplicated(final_count[c("card_id","name","group_id")]),]
final_count<-aggregate(group_id~card_id+name, final_count, FUN = length)
names(final_count)<-c("card_id","name","count")

#### bind prod_cat with rank info of fs products
prod_cat <- rbind(prod_cat,prod_cat_fs)

#### final map and final rank table 
final_map<-merge(final_map, prod_cat, by.x = "prod_id", by.y = "prod_id" )
final_rank<-final_map[!duplicated(final_map[c("card_id","name")]),][c("card_id","qp","name")]
temp_df<-data.frame(rank<-seq(1:16))
names(temp_df)<-c("rank")
final_rank<-merge(final_rank,temp_df)

## strips 
total_strips<-final_map[!duplicated(final_map$name),][c("name")]
strips_fs <- data.frame(name=c(total_strips[total_strips['name'] == 'Free-Shipping',]))
strips <- data.frame(name=c(total_strips[total_strips['name'] != 'Free-Shipping',]))

##main final_rank and final map
main_final_rank <- final_rank
main_final_map <- final_map

## country mapping######################
final_rank <- final_rank[final_rank['qp'] == 'country' ,]
final_map <- final_map[final_map['qp'] == 'country', ]
for (i in 1:nrow(strips)) {
  temp_op<-final_rank[which(final_rank$name == strips[i,1]),]
  temp_df<-final_map[which(final_map$name == strips[i,1]),]
  prods<-temp_df[!duplicated(temp_df$prod_id),][c("prod_id")]
  cards<-temp_df[!duplicated(temp_df$card_id),][c("card_id")]
  if (nrow(prods) == 0 | nrow(cards) == 0) next
  cards$name<-strips[i,1]
  cards<-merge(cards, final_count, by.x=c("card_id","name"), by.y=c("card_id","name"))
  cards$strip_sort_order<-seq(1:nrow(cards))
  prods$dpid<-seq(1:nrow(prods))
  cards$dcid<-seq(1:nrow(cards))
  temp_op<-merge(temp_op,cards,by.x=c("card_id","name"), by.y=c("card_id","name"))
  bin_size <- floor(nrow(prods)/nrow(cards))
  temp_op$dpid<-floor(runif(nrow(cards)*16,(temp_op$rank-1)*bin_size+1,temp_op$rank*bin_size))
  temp_op<-merge(temp_op,prods,by.x="dpid",by.y="dpid")
  temp_op<-merge(temp_op, cards_url[c("card_id","country_city")], by.x="card_id", by.y="card_id")
  temp_op<-merge(temp_op, urls, by.x="name", by.y="name")
  temp_op$url<-paste(temp_op$url,"?",temp_op$qp,"=",temp_op$country_city,sep="")
  temp_op$url<-str_replace_all(temp_op$url," ","%20")
  ifelse(exists("final_op"), final_op<-rbind(final_op,temp_op), final_op<-temp_op)
}

## city mapping ###################
final_rank <- main_final_rank 
final_map <- main_final_map

final_rank <- final_rank[final_rank['qp'] == 'city' ,]
final_map <- final_map[final_map['qp'] == 'city', ]
for (i in 1:nrow(strips)) {
  
  temp_op<-final_rank[which(final_rank$name == strips[i,1]),]
  temp_df<-final_map[which(final_map$name == strips[i,1]),]
  prods<-temp_df[!duplicated(temp_df$prod_id),][c("prod_id")]
  cards<-temp_df[!duplicated(temp_df$card_id),][c("card_id")]
  if (nrow(prods) == 0 | nrow(cards) == 0) next
  cards$name<-strips[i,1]
  cards<-merge(cards, final_count, by.x=c("card_id","name"), by.y=c("card_id","name"))
  cards$strip_sort_order<-seq(1:nrow(cards))
  prods$dpid<-seq(1:nrow(prods))
  cards$dcid<-seq(1:nrow(cards))
  temp_op<-merge(temp_op,cards,by.x=c("card_id","name"), by.y=c("card_id","name"))
  bin_size <- floor(nrow(prods)/nrow(cards))
  temp_op$dpid<-floor(runif(nrow(cards)*16,(temp_op$rank-1)*bin_size+1,temp_op$rank*bin_size))
  temp_op<-merge(temp_op,prods,by.x="dpid",by.y="dpid")
  temp_op<-merge(temp_op, cards_url[c("card_id","country_city")], by.x="card_id", by.y="card_id")
  temp_op<-merge(temp_op, urls, by.x="name", by.y="name")
  temp_op$url<-paste(temp_op$url,"?",temp_op$qp,"=",temp_op$country_city,sep="")
  temp_op$url<-str_replace_all(temp_op$url," ","%20")
  #final_op<-rbind(final_op,temp_op)
  ifelse(exists("final_op"), final_op<-rbind(final_op,temp_op), final_op<-temp_op)
  
}


temp_op<-final_op[!(duplicated(final_op$count)),]
temp_op<-temp_op[order(-temp_op$count),]
temp_op$seq<-seq(1:nrow(temp_op))+10
if(nrow(temp_op[which(temp_op$name=='Cakes'),]) > 0) temp_op[which(temp_op$name=='Cakes'),]$seq<-5
if(nrow(temp_op[which(temp_op$name=='Flowers'),]) > 0) temp_op[which(temp_op$name=='Flowers'),]$seq<-4
temp_op<-merge(final_op,temp_op[c("count","seq")],by.x="count",by.y="count")
temp_op$strip_sort_order<-temp_op$seq
final_op<-temp_op[c("name","card_id","prod_id","rank", "count", "strip_sort_order","country_city","url")]
names(final_op)<-c("strip_display_name", "card_id", "pid", "rank", "count", "strip_sort_order", "ccs_name", "url")

### Country Fs ###################################### 
final_rank <- main_final_rank 
final_map <- main_final_map 
final_rank <- final_rank[final_rank['qp'] == 'country' ,]
final_map <- final_map[final_map['qp'] == 'country', ]
## with card_id qp name
temp_op<-final_rank[which(final_rank$name == strips_fs[1,1]),]
##all the fs products mapped to card_id containing rank [card_id,pid,rank]
temp_df<-final_map[which(final_map$name == strips_fs[1,1]),]
temp_df <- temp_df[order(temp_df$card_id,temp_df$rank),]
temp_df$rank<-as.numeric(with(temp_df, ave(card_id,card_id,FUN=seq_along)))
temp_df <-temp_df[temp_df['rank'] <= 16 ,]
#merge
temp_op <- merge(temp_op,temp_df,by.x=c('card_id','qp','name','rank'),by.y=c('card_id','qp','name','rank'))
#count
cards<-temp_df[!duplicated(temp_df$card_id),][c("card_id")]
cards$name<-strips_fs[1,1]
count<-merge(cards, final_count, by.x=c("card_id","name"), by.y=c("card_id","name"))[c('card_id','count')]
#ccs_name
temp_op <- merge(temp_op,count,by.x=c('card_id'),by.y=c('card_id'))
temp_op<-merge(temp_op, cards_url[c("card_id","country_city")], by.x="card_id", by.y="card_id")
#url
urls <- rbind(data.frame(name=c('Free-Shipping'),url=c('free-shipping')))
temp_op<-merge(temp_op, urls, by.x="name", by.y="name")
temp_op$url<-paste(temp_op$url,"?",temp_op$qp,"=",temp_op$country_city,sep="")
temp_op$url<-str_replace_all(temp_op$url," ","%20")
#strip_sort_order
temp_op$strip_sort_order <- 1
temp_op <- temp_op[c('name','card_id','rank','prod_id','count','country_city','url','strip_sort_order')]
colnames(temp_op) <- c('strip_display_name','card_id','rank','pid','count','ccs_name','url','strip_sort_order')
temp_op <- temp_op[c('strip_display_name','card_id','pid','rank','count','strip_sort_order','ccs_name','url')]
#bind
final_op<- rbind(temp_op,final_op)
final_op$id<-seq(1:nrow(final_op))
final_op<-final_op[c("id","ccs_name","strip_display_name","strip_sort_order","url","pid","rank","count","card_id")]


######SDD Home Page
sdd_home_page<-cards_url[which(cards_url$type == "SDD"),][c("card_id","title")]
prod_sdd<-merge(prod_sdd,card_prod_count,by.x="card_id",by.y="card_id",all.x=TRUE)
if (nrow(prod_sdd[which(prod_sdd$url == "cakes"),]) > 0) prod_sdd[which(prod_sdd$url == "cakes"),]$url = 'cakes?getQ=%7B%22category%22%3A%22%5C%22Flowers%20and%20Cakes%5C%22%22%2C%22subcategory%22%3A%22%5C%22Cakes%5C%22%22%2C%22delivery_types%22%3A%22sdd%22%7D'
if (nrow(prod_sdd[which(prod_sdd$url == "flowers"),]) > 0) prod_sdd[which(prod_sdd$url == "flowers"),]$url = 'flowers?getQ=%7B%22category%22%3A%22%5C%22Flowers%20and%20Cakes%5C%22%22%2C%22subcategory%22%3A%22%5C%22Flowers%5C%22%22%2C%22delivery_types%22%3A%22sdd%22%7D'
if(nrow(prod_sdd[which(is.na(prod_sdd$count)),]) > 0) prod_sdd[which(is.na(prod_sdd$count)),]$count<-1;
temp_op<-merge(sdd_home_page,prod_sdd,by = NULL)
temp_op$id<-seq(nrow(final_op)+1,nrow(final_op)+nrow(temp_op))
sdd_final<-temp_op[c("id","title.x","title.y","sort","url","prod_id","rank","count","card_id.x")]
colnames(sdd_final)<-c("id","ccs_name","strip_display_name","strip_sort_order","url","pid","rank","count","card_id")
final_op<-rbind(final_op,sdd_final)


######Festival Home Pages
fest_strip_1<-data.frame(c('christmas/christmas-best-sellers/christmas-gift-hampers','christmas/christmas-best-sellers/chocolate-hampers','christmas/christmas-best-sellers/plum-cake-hampers','christmas/christmas-best-sellers/cakes-cookies','christmas/christmas-decor/christmas-trees-decor','christmas/christmas-best-sellers/santa-claus-soft-toys'))
colnames(fest_strip_1)<-c("url")
fest_strip_2<-data.frame(c('new-year/gift-baskets/new-year-exclusive-hampers','new-year/flowers-cakes/flowers-with-cakes','new-year/personalized-gifts' , 'new-year/gift-baskets/chocolate-baskets','new-year/flowers-cakes/chocolate-cakes','new-year/international-free-shipping/us-free-shipping','new-year/gift-baskets/gajak-hampers'))
colnames(fest_strip_2)<-c("url")
fest_strip_list<-rbind(fest_strip_1, fest_strip_2)
fest_strip_list$strip_sort_order<-seq(1:nrow(fest_strip_list))*2

fest_urls<-merge(fest_urls,card_prod_count,by.x="card_id",by.y="card_id",all.x=TRUE)
if(nrow(fest_urls[which(is.na(fest_urls$count)),]) > 0) fest_urls[which(is.na(fest_urls$count)),]$count<-1;
fest_home_pages<-fest_urls[which(fest_urls$base_level == 0),][c("card_id","title","type2")]
fest_strips<-merge(fest_strip_list,fest_urls,by.x="url",by.y="url")
fest_strips<-fest_strips[c("card_id","title","type2","url","count","strip_sort_order")]

fest_strips_others<-fest_urls[which(fest_urls$base_level == 1 & fest_urls$type2 != 13 & fest_urls$type2 != 14),][c("card_id","title","type2","url","count")]
fest_strips_others$strip_sort_order<-seq(1:nrow(fest_strips_others))*2
fest_strips<-rbind(fest_strips,fest_strips_others)

card_1 = data.frame(c(rep(21527,12)), c(522559 ,521851 , 522575, 521847, 521848 , 521617 , 521614,522571,521843,521618 , 521662 ,521703),c(seq(1:12)))
colnames(card_1) = c('card_id','prod_id','rank')
card_2 = data.frame(c(rep(21529,11)), c(521693,521850 ,521692 , 216895,521855 ,521705 ,521691 , 521624 , 521623 , 521619 , 521704),c(seq(1:11)))
colnames(card_2) = c('card_id','prod_id','rank')
card_3 = data.frame(c(rep(21528,12)), c(521694,521849,521664,521653,521622,521714,521699 ,521697,521700,521825,521725,521654),c(seq(1:12)))
colnames(card_3) = c('card_id','prod_id','rank')
card_4 = data.frame(c(rep(21530,12)), c(521971 ,521898 , 522097 ,521903 ,522061 , 521951 ,521906 ,522059 ,521952 ,521976 ,521907,521954),c(seq(1:12)))
colnames(card_4) = c('card_id','prod_id','rank')
card_5 = data.frame(c(rep(21534,12)), c(522576,521651,522557,521853,521844,521842,521717,522561,521673,521626,521701,521658),c(seq(1:12)))
colnames(card_5) = c('card_id','prod_id','rank')
card_6 = data.frame(c(rep(21533,12)), c(522076,215908,522082,522081,522083,522087,522080,522078,522079,521706,522075,522086),c(seq(1:12)))
colnames(card_6) = c('card_id','prod_id','rank')
prod_xmas<-rbind(card_1,card_2,card_3,card_4,card_5,card_6)


card_1 = data.frame(c(rep(21557,14)), c(520445,521684,517649,516713 ,520596,521678,517757,520449,521615,521679,521680,520439,521795,521763),c(seq(1:14)))
colnames(card_1) = c('card_id','prod_id','rank')
card_2 = data.frame(c(rep(21551,18)), c(522896,217759,522969,520767,516755,520808,216966,521413,520736,520807,521404,215178,216974,215171,521412,520741,521910,215198),c(seq(1:18)))
colnames(card_2) = c('card_id','prod_id','rank')
card_4 = data.frame(c(rep(21558,13)), c(522058,520452,520001,520459,521797,519998,519980,519987,521826,216895,521765,520006,520009),c(seq(1:13)))
colnames(card_4) = c('card_id','prod_id','rank')
card_3 = data.frame(c(rep(21472,12)), c(521392,522812,522801,522794,522809,522548,522804,522831,522829,522539,522550,522820),c(seq(1:12)))
colnames(card_3) = c('card_id','prod_id','rank')
card_5 = data.frame(c(rep(21555,4)), c( 215180,520789,  518952,215195),c(seq(1:4)))
colnames(card_5) = c('card_id','prod_id','rank')
card_6 = data.frame(c(rep(21579,12)), c(522540,522515,522505,520203,522491,522508,522518,522524,522504,522497,522509,522499),c(seq(1:12)))
colnames(card_6) = c('card_id','prod_id','rank')
card_7 = data.frame(c(rep(21560,4)), c(521685,521846,521641,521678),c(seq(1:4)))
colnames(card_7) = c('card_id','prod_id','rank')

prod_ny<-rbind(card_1,card_2,card_3,card_4,card_5,card_6,card_7)



prod_fest<-prod_fest[which(prod_fest$type2 != 13 & prod_fest$type2 != 14),][c('card_id','prod_id','rank')]
prod_fest<-rbind(prod_fest,prod_xmas,prod_ny)
temp_op<-merge(fest_home_pages,fest_strips,by.x="type2",by.y="type2")
temp_op<-merge(temp_op,prod_fest,by.x="card_id.y",by.y="card_id")

##Banners
#temp_op<-temp_op[c("title.x","title.y","strip_sort_order","url","prod_id","rank","count","card_id.x")]
#xmas_banner_1<-data.frame('title.x'= c('Send Christmas gifts to USA with free shipping',' Send Christmas gifts worldwide with free shipping'),'title.y'=c('christmas-small-usa-cta.jpg','christmas-small-worldwide-cta.jpg'),'strip_sort_order'=c(5,5),'url'=c('christmas/international-free-shipping/us-free-shipping','christmas/international-free-shipping/worldwide-free-shipping'),'prod_id'=c(0,0),'rank'= c(1,2),'count'= c(2,2),'card_id.x'= c(21455,21455))
#ny_banner_1<-data.frame('title.x' = c('Send New Year gifts to USA with free shipping','Send New Year gifts worldwide with free shipping'),'title.y'=c('new-year-small-usa-cta.jpg','new-year-small-worldwide-cta.jpg'),'strip_sort_order'=c(5,5),'url'=c('new-year/international-free-shipping/us-free-shipping','new-year/international-free-shipping/worldwide-free-shipping'),'prod_id'=c(0,0),'rank'=c(1,2),'count'=c(2,2),'card_id.x'=c(21456,21456))
#temp_op<-rbind(temp_op,xmas_banner_1,ny_banner_1)

temp_op$id<-seq(nrow(final_op)+1,nrow(final_op)+nrow(temp_op))
fest_final<-temp_op[c("id","title.x","title.y","strip_sort_order","url","prod_id","rank","count","card_id.x")]
colnames(fest_final)<-c("id","ccs_name","strip_display_name","strip_sort_order","url","pid","rank","count","card_id")
final_op<-rbind(final_op,fest_final)

################## Luxury ###
luxury_home_page<-cards_url[which(cards_url$type=="Luxury"),][c("card_id","title")]
#strips info
prod_luxury_count<-merge(prod_luxury,card_prod_count,by.x='strip_card_id',by.y='card_id')
temp_op<-merge(luxury_home_page,prod_luxury_count)
#strip sort order
strips<-unique(temp_op[c('name','luxury_count')]) 
strips<-strips[with(strips, order(-luxury_count)),]
strips$strip_sort_order<-seq(1:nrow(strips))+10
if(nrow(strips[which(strips$name=='Cakes'),]) > 0) strips[which(strips$name=='Cakes'),]$strip_sort_order<-5
if(nrow(strips[which(strips$name=='Flowers'),]) > 0) strips[which(strips$name=='Flowers'),]$strip_sort_order<-4
temp_op<-merge(temp_op,strips,by.x=c('name','luxury_count'),by.y=c('name','luxury_count'))
temp_op<-unique(temp_op)
temp_op<-temp_op[order(temp_op$rank),]
temp_op$rank<-as.numeric(with(temp_op, ave(name, name, FUN = seq_along)))
temp_op<-temp_op[which(temp_op$rank<=16),]

temp_op$id<-seq(nrow(final_op)+1,nrow(final_op)+nrow(temp_op))
temp_op<-temp_op[c('id','title','name','strip_sort_order','url','prod_id','rank','luxury_count','card_id')]
colnames(temp_op)<-c("id","ccs_name","strip_display_name","strip_sort_order","url","pid","rank","count","card_id")
final_op<-rbind(final_op,temp_op)

