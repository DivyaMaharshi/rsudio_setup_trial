print(paste("started Hover option script at", Sys.time(), sep = " "))
rm(list=ls())
.libPaths("/home/ruser2/R/x86_64-pc-linux-gnu-library/3.2")
# list of packages required for the session
packages_required= c("RMySQL","reshape2","magrittr","plyr","dplyr","stringr","stringi")

# install from CRAN , if not already installed
install_check<- packages_required %in% installed.packages()
if(length(packages_required[!install_check])>0) install.packages(packages_required[!install_check],dependencies = TRUE)

# Load required libraries for the session
lapply(packages_required, FUN = function(X) {
  do.call("require", list(X))
})

start_time<-Sys.time()
# function to establish MySqlconnection with R server
dbConnectNewIgp<- function(){
  igpnewConnProd <<- dbConnect(MySQL(),user='root',password='password', dbname='igpnew', host='127.0.0.1', port=3306)  
}


#run query and fetch results
rro_data<-dbGetQuery(igpnewConnProd, 'select prro.* from product_rro prro join products p on p.products_id = prro.pid where p.products_status = 1;')
is_sc_pt<-dbGetQuery(igpnewConnProd, 'select card_id as "cid", case when product_type = "" then case when sub_category = "" then 0 else 1 end else 2 end as "sc_pt", case when type2 = "Cakes" or type2 = "Flowers" then 1 else 0 end as "fnc" from cards_url;')
extra<-dbGetQuery(igpnewConnProd, 'select pc.pid, case when ei.flag_hamper = 1 then 100 else case when ei.flag_personalize = 1 then 101 else pc.ptid end end as "ptid", case when ei.flag_hamper = 1 then 100 else case when ei.flag_personalize = 1 then 101 else pc.subid end end as "scid", ei.boost from newigp_product_extra_info ei join products p on p.products_id = ei.products_id join product_cat pc on pc.pid = ei.products_id where p.products_status = 1 group by pc.pid;')
codes<-dbGetQuery(igpnewConnProd,'select * from  newigp_product_extra_info ei join products p on ei.products_id = p.products_id where p.products_status = 1')
prod_attr<-dbGetQuery(igpnewConnProd,"select pa.products_id as 'pid', pa.attr_val_id as 'aid', av.attr_set_id from newigp_products_to_attr_val pa join newigp_master_attr_vals av on av.id = pa.attr_val_id;")


get_individual_weight_input_data<-function(){
  prod<-unique(rro_data$pid)
  individual_weight_df<-list()
  for (i in 1:length(prod)){
    # get unique recipients for a product
    rc<-length(unique((subset(rro_data,rro_data$pid==prod[i])$rcid)))
    prob_rc<-1/rc
    #get unique relationships for a product
    rl<-length(unique((subset(rro_data,rro_data$pid==prod[i])$rlid)))
    prob_rl<-1/rl
    # get unique occasions for a product
    oc<-length(unique((subset(rro_data,rro_data$pid==prod[i])$oid)))
    prob_oc<-1/oc
    
    rro_df<-subset(rro_data,rro_data$pid==prod[i])
    
    
    df_tb<-as.data.frame(table(rro_df$rcid))
    df_tb$contri_rc<-((as.data.frame(table(rro_df$rcid)))$Freq)/nrow(rro_df)
    df<-merge(rro_df,df_tb,by.x= "rcid",by.y= "Var1")
    df$contri_rc<-prob_rc*df$contri_rc
    
    
    df_tb<-as.data.frame(table(rro_df$rlid))
    df_tb$contri_rl<-((as.data.frame(table(rro_df$rlid)))$Freq)/nrow(rro_df)
    df<-merge(df,df_tb,by.x= "rlid",by.y= "Var1")
    df$contri_rl<-prob_rl*df$contri_rl
    
    
    df_tb<-as.data.frame(table(rro_df$oid))
    df_tb$contri_oc<-((as.data.frame(table(rro_df$oid)))$Freq)/nrow(rro_df)
    df<-merge(df,df_tb,by.x= "oid",by.y= "Var1")
    df$contri_oc<-prob_oc*df$contri_oc
    
    
    individual_weight_df[[i]]<-df
    
  }
  
  individual_weight_df<-do.call(rbind.data.frame,individual_weight_df)
  individual_weight_df<-individual_weight_df[c(1:4,5,7,9,11)]
  return(individual_weight_df)
}



df<-get_individual_weight_input_data()
df$oid<-as.factor(df$oid)
df$rcid<-as.factor(df$rcid)
df$rlid<-as.factor(df$rlid)
print('random forest Regression')
rf <- randomForest(weight ~ oid+rlid+rcid, data=df,
                   importance=TRUE, na.action=na.omit)
print('done')
df$weight1<-predict(rf,df)

df$contri_rc<-(0.33*(df$weight)/3)+(0.3*df$contri_rc)+(0.33*(df$weight1)/3)
df$contri_rl<-(0.33*(df$weight)/3)+(0.3*df$contri_rl)+(0.33*(df$weight1)/3)
df$contri_oc<-(0.33*(df$weight)/3)+(0.33*df$contri_oc)+(0.33*(df$weight1)/3)


#############################################################
###-----------------get card attributes -----------------####
#############################################################

dbConnectNewIgp()

card_details_rro<-dbGetQuery(igpnewConnProd, 'select mcc.id AS card_id, mav.id as attr_val_id, mav.code as attr_val_code , mas.id as attr_set_id ,mas.code AS attr_set_code from newigp_master_cards_collection as mcc join newigp_master_cards_val as mcv on mcc.id = mcv.card_id join newigp_master_attr_vals as mav on mcv.attribute_val_id = mav.id and mcv.attr_type =2 join newigp_master_attr_sets as mas on mav.attr_set_id = mas.id')
print(paste("no of rows imported =",nrow(card_details_rro),sep = ""))
card_details_rro<-card_details_rro[c(1,5,2)]
card_details_rro<-unique(card_details_rro)
card_details_rro_wide<-dcast(card_details_rro, card_id~attr_set_code, value.var="attr_val_id")



dbConnectNewIgp()
#pull latest card to product mapping from prod_rank
card_prod_mapping<-dbGetQuery(igpnewConnProd, 'select * from prod_rank_ds;')
#card_prod_mapping<-dbGetQuery(igpnewConnProd, 'select ds.* from prod_rank_ds ds join cards_url cu on cu.card_id=ds.card_id and cu.type IN ("Category", "Hampers", "Personalized", "Luxury");')
print(paste("no of rows imported from prod_rank_ds table =",nrow(card_prod_mapping),sep = ""))

#### memory overflow fix - by Priyesh ####
card_prod_mapping_orig<-card_prod_mapping
card_details_rro_wide_orig<-card_details_rro_wide
iter_max<-ceiling(max(card_prod_mapping_orig$card_id)/2500)+1
for (i in 1:iter_max) {
  card_prod_mapping<-card_prod_mapping_orig[which(card_prod_mapping_orig$card_id>(i-1)*2500 & card_prod_mapping_orig$card_id<=i*2500),]
  card_details_rro_wide<-card_details_rro_wide_orig[which(card_details_rro_wide_orig$card_id>(i-1)*2500 & card_details_rro_wide_orig$card_id<=i*2500),]  
  if (nrow(card_prod_mapping) > 0) {
    card_prod_mapping_w_weight = merge(x = card_prod_mapping,y = df,by.x='prod_id',by.y='pid',all.x = T)
    card_prod_mapping_w_weight = merge(x = card_prod_mapping_w_weight, y = card_details_rro_wide, by.x='card_id', by.y = 'card_id', all.x = T)
    
    #(card_details_wide$attr_val_id.recipient=='NA')*df$contri_rc
    card_prod_mapping_w_weight[is.na(card_prod_mapping_w_weight)]<-'NA'
    card_prod_mapping_w_weight$recipient_flag<-!(card_prod_mapping_w_weight$recipient=='NA')
    card_prod_mapping_w_weight$relationship_flag<-!(card_prod_mapping_w_weight$relationship=='NA')
    card_prod_mapping_w_weight$occasion_flag<-!(card_prod_mapping_w_weight$occasion=='NA')
    
    card_prod_mapping_w_weight$contri_rc<- as.numeric(card_prod_mapping_w_weight$contri_rc)
    card_prod_mapping_w_weight$contri_rl<- as.numeric(card_prod_mapping_w_weight$contri_rl)
    card_prod_mapping_w_weight$contri_oc<- as.numeric(card_prod_mapping_w_weight$contri_oc)
    
    card_prod_mapping_w_weight$score<-NA
    
    vec<- card_prod_mapping_w_weight$recipient_flag|card_prod_mapping_w_weight$relationship_flag|card_prod_mapping_w_weight$occasion_flag
    
    card_prod_mapping_w_weight$score[vec]<- ifelse(card_prod_mapping_w_weight$recipient_flag[vec]*card_prod_mapping_w_weight$contri_rc[vec]==0,1,card_prod_mapping_w_weight$contri_rc[vec])*
      ifelse(card_prod_mapping_w_weight$relationship_flag[vec]*card_prod_mapping_w_weight$contri_rl[vec]==0,1,card_prod_mapping_w_weight$contri_rl[vec])*
      ifelse(card_prod_mapping_w_weight$occasion_flag[vec]*card_prod_mapping_w_weight$contri_oc[vec]==0,1,card_prod_mapping_w_weight$contri_oc[vec])
    
    match_vec<- card_prod_mapping_w_weight$rcid==card_prod_mapping_w_weight$recipient|
      card_prod_mapping_w_weight$rlid==card_prod_mapping_w_weight$relationship|
      card_prod_mapping_w_weight$oid==card_prod_mapping_w_weight$occasion
    
    card_prod_mapping_w_weight_match<- card_prod_mapping_w_weight[match_vec,]
    
    if(nrow(card_prod_mapping_w_weight_match[which(!is.na(card_prod_mapping_w_weight_match$card_id)),]) > 0) {
      card_prod_score<-aggregate(score~card_id+prod_id, data = card_prod_mapping_w_weight_match,FUN = max)
      
      card_prod_mapping<-merge(card_prod_mapping,card_prod_score,by.x = c("card_id","prod_id"), 
                               by.y = c("card_id","prod_id"),all.x = TRUE)
      
      if(nrow(card_prod_mapping[is.na(card_prod_mapping$score),]) > 0) card_prod_mapping[is.na(card_prod_mapping$score),]$score<-0
      ifelse(exists("card_prod_score_final"), card_prod_score_final<-rbind(card_prod_score_final,card_prod_mapping), card_prod_score_final<-card_prod_mapping)
    }
    print(paste("iteration",i,"completed"))
  }
}
card_prod_score<-card_prod_score_final
card_prod_mapping<-card_prod_mapping_orig
rm(card_prod_score_final)
rm(card_prod_mapping_orig)
rm(card_details_rro_wide_orig)
### fix ends ###

old_new_product_map<-read.csv("/home/ruser2/Production_scripts_CS/Data-Old-vs-New-Products.csv")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#old_new_product_map$New.Product.Code<-as.character(old_new_product_map$New.Product.Code)
#old_new_product_map$p_code<-substrRight(old_new_product_map$New.Product.Code,4)
#old_new_product_map$New.Product.Code<-as.character(old_new_product_map$New.Product.Code)
df_merge<-merge(old_new_product_map,codes[c('products_id')],by.x = "prod_id",by.y = "products_id")
#df_merge<-df_merge[c('products_id','Old.Product.Sold.Count')]
normalized = function(x){(x-min(x))/(max(x)-min(x))}
#df_merge$old_normalized<-normalized(df_merge$Old.Product.Sold.Count)
df_merge$old_normalized<-log10(log10(df_merge$sales)+1)

card_prod_score<-merge(card_prod_score,card_prod_mapping[c("card_id","prod_id")],by.x=c("card_id","prod_id"), by.y=c("card_id","prod_id"), all.y = TRUE)
card_prod_score<-merge(card_prod_score,df_merge[c('prod_id','old_normalized')],by.x = "prod_id",by.y = "prod_id",all.x = T)
card_prod_score$score<-ifelse(is.na(card_prod_score$score)==TRUE,0,card_prod_score$score)
card_prod_score$old_normalized<-ifelse(is.na(card_prod_score$old_normalized)==TRUE,0,card_prod_score$old_normalized)
card_prod_score$old_normalized<-ifelse(card_prod_score$prod_id > 500000 & card_prod_score$old_normalized == 0,(sample(30, size = nrow(card_prod_score), replace = TRUE)+15)/100,card_prod_score$old_normalized)
card_prod_score$final_score<-card_prod_score$score*.2+card_prod_score$old_normalized*.8

pt_level_het<-read.csv(paste(getwd(),"/ml/Production_scripts_CS/csv/pt_attr_set_for_het.csv",sep=''))
prod_attr<-merge(prod_attr,pt_level_het[c("pt_id","attr_set_id")],by.x = "attr_set_id", by.y = "attr_set_id")
ex_at<-merge(extra[c("pid","boost")],prod_attr[c("pid","aid")],by.x="pid",by.y="pid")
ex_at<-ex_at[c("pid", "aid","boost")]
colnames(ex_at)<-c("pid","gid","boost")
ex_sc<-extra[c("pid", "scid","boost")]
colnames(ex_sc)<-c("pid","gid","boost")
ex_pt<-extra[c("pid", "ptid","boost")]
colnames(ex_pt)<-c("pid","gid","boost")

card_prods<-merge(card_prod_score, is_sc_pt, by.x="card_id", by.y = "cid")
cp1<-card_prods[which(card_prods$sc_pt == 0),]
cp1<-merge(cp1, ex_sc, by.x = "prod_id", by.y = "pid", all.x = TRUE)
cp2<-card_prods[which(card_prods$sc_pt == 1),]
cp2<-merge(cp2, ex_pt, by.x = "prod_id", by.y = "pid", all.x = TRUE)
cp3<-card_prods[which(card_prods$sc_pt == 2),]
cp3<-merge(cp3, ex_at, by.x = "prod_id", by.y = "pid", all.x = TRUE)
card_prods<-rbind(cp1,cp2,cp3)

if(nrow(card_prods[which(is.na(card_prods$gid)),]) > 0) card_prods[which(is.na(card_prods$gid)),]$gid<-0

card_prods<-card_prods[order(card_prods$card_id,-card_prods$final_score),]
card_prods<-card_prods[!duplicated(card_prods[c("card_id","prod_id")]),]

### memory overflow fix - by Priyesh ####
card_prods_orig<-card_prods
iter_max<-ceiling(max(card_prods_orig$card_id)/10000)+1
for (i in 1:iter_max) {
  card_prods<-card_prods_orig[which(card_prods_orig$card_id > (i-1)*10000 & card_prods_orig$card_id <= i*10000),]
  if (nrow(card_prods) > 0) {
    card_prods$het<-with(card_prods, ave(card_id, card_id, gid, FUN = seq_along))
    card_prods[which(card_prods$gid == 100 & card_prods$fnc == 0),]$het<-ceiling(card_prods[which(card_prods$gid == 100 & card_prods$fnc == 0),]$het/3)
    card_prods[which(card_prods$gid == 101 & card_prods$fnc == 0),]$het<-ceiling(card_prods[which(card_prods$gid == 101 & card_prods$fnc == 0),]$het/2) + sample(20, size = nrow(card_prods[which(card_prods$gid == 101 & card_prods$fnc == 0),]), replace = TRUE)/100 
    card_prods[which(card_prods$fnc == 1),]$het<-card_prods[which(card_prods$fnc == 1),]$het * 3
    card_prods$het<-(5000 - card_prods$het + card_prods$final_score)
    ifelse(exists("card_prods_final"), card_prods_final<-rbind(card_prods_final,card_prods), card_prods_final<-card_prods)
    print(paste("iteration",i,"completed"))
  }
}
card_prods<-card_prods_final
rm(card_prods_final)
rm(card_prods_orig)
##### fix ends ######
if (nrow(card_prods[which(floor(card_prods$boost/100) == card_prods$card_id),]) > 0) {
  card_prods[which(floor(card_prods$boost/100) == card_prods$card_id),]$het<-6000 - (card_prods[which(floor(card_prods$boost/100) == card_prods$card_id),]$boost %% 100)
}
card_prods<-card_prods[order(card_prods$card_id,-card_prods$het),]
card_prod_score<-card_prods[c("prod_id","card_id","het")]
colnames(card_prod_score)<-c("prod_id","card_id","final_score")



### memory overflow fix - by Priyesh ####
card_prod_score<-card_prod_score[order(-card_prod_score$final_score),]
card_prod_score_orig<-card_prod_score
iter_max<-ceiling(max(card_prod_score_orig$card_id)/10000)+1
for (i in 1:iter_max) {
  card_prod_score<-card_prod_score_orig[which(card_prod_score_orig$card_id > (i-1)*10000 & card_prod_score_orig$card_id <= i*10000),]
  if (nrow(card_prod_score) > 0) {
    card_prod_score$rank<-with(card_prod_score, ave(card_id, card_id, FUN = seq_along))
    ifelse(exists("card_prod_score_final"), card_prod_score_final<-rbind(card_prod_score_final,card_prod_score), card_prod_score_final<-card_prod_score)
    print(paste("iteration",i,"completed"))
  }
}
card_prod_score<-card_prod_score_final
rm(card_prod_score_final)
rm(card_prod_score_orig)
##### fix ends ######  

dbConnectNewIgp()
prod_rank_ds<- card_prod_score[c('card_id','prod_id','rank')]
prod_rank_ds<-prod_rank_ds[!duplicated(prod_rank_ds[c('card_id','prod_id')]),]
dbSendQuery(igpnewConnProd,paste('TRUNCATE prod_rank;'))
dbWriteTable(conn = igpnewConnProd,name = "prod_rank",value = prod_rank_ds,append=T,row.names=FALSE)dbSendQuery(igpnewConnProd,paste("update newigp_product_extra_info ei join products p on p.products_id = ei.products_id set ei.global_rank = -p.products_mrp where group_type>0;"))
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
loginfo('Product ranking script ran successfully')

end_time<-Sys.time()
print(paste("TOTAL TIME TAKEN by Product Ranking script =", difftime(end_time,start_time,units = "mins"),"mins",sep = " "))