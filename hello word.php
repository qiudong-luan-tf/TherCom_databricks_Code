"Hello World!"
  ## 分组统计
display(sl_email_test_11 %>% filter(RESP_DT_KEY == '20191102') %>% group_by(CELL_NM) %>% summarise(cnt = length(RESP_DT_KEY)) %>% arrange(cnt))


## 表连接
a<-df_from_cdw_Rose_78%>%dplyr::filter(RESP_TYPE_NM=="Mail Open")%>%dplyr::group_by(CELL_NM)%>%dplyr::summarise(email_cnt=length(unique(EMAIL_KEY)))
c<-df_from_cdw_Rose_78%>%dplyr::group_by(CELL_NM)%>%dplyr::summarise(email_cnt=length(unique(EMAIL_KEY)))
c<-df_from_cdw_Rose_78%>%dplyr::group_by(CELL_NM)%>%dplyr::summarise(email_cnt=n_distinct(EMAIL_KEY))
d<-c%>%dplyr::left_join(a,by="CELL_NM")%>%dplyr::left_join(b,by="CELL_NM")
### 增加字段
d<-dplyr::mutate(d,clickpercent=click/open)
###表连接
c<-c%>%dplyr::left_join(a,by="CELL_NM")%>%dplyr::left_join(b,by="CELL_NM")
x %>% left_join(y, by = c("name" = "name"))

coon1 <- merge(x =contact_phone_nbr,y = phone_nbr,by.x='phonenumber',by.y='id',all.x = T) 

### 多条件匹配，交集
merge(a,sl_EFH_table_final_1,by = intersect(names(a), names(sl_EFH_table_final_1)))

### 两个相同的字段匹配，左连接
merge(F_pl_latest,Special_ListPrice,all.x=TRUE)

SL_F_st_sku$HEADER_EFFECIATE_DATE<- lubridate::ymd(SL_F_st_sku$HEADER_EFFECIATE_DATE)


## 修改日期
df_from_cdw_Rose_78$RESP_DT_KEY<-lubridate::ymd(df_from_cdw_Rose_78$RESP_DT_KEY)


## 字符串截取函数
df_from_cdw_Rose_78$Tactics<-substring(df_from_cdw_Rose_78$LIST_NM,1,9)
substr(data,1,4)



## 新增加一个自增字段
sl_web_user_table_id<-sl_web_user_table %>% dplyr::mutate(ID=row_number())


## 字符串匹配函数
unique(sl_email_test_11[grep('CMD',tolower(sl_email_test_11$asset_nm)),]$asset_nm)
txt <- c("Dother")
txt1 <- c("otherD")
b <- SKU_list_2[grep(txt,SKU_list_2$CN),]
b1 <- SKU_list_2[grep(txt1,SKU_list_2$CN),]


## ifelse语句
phone <- phone %>% dplyr::mutate(flag =  case_when(phone$rawphone %in% blacklist_phone$MAIN_PHN_NBR ~  '1',
                                       phone$rawphone %in% blacklist_phone$cleanphone ~  '1',
                                       phone$cleanphone  %in% blacklist_phone$MAIN_PHN_NBR ~ '1',
                                       phone$cleanphone %in% blacklist_phone$cleanphone ~  '1',
                                       TRUE ~ '0'))

phone <- phone %>% dplyr::mutate(flag =  case_when(phone$rawphone %like% '13027760075' ~  '1',
                                       TRUE ~ '0'))


## 删除字段
contact_nbr[,"MAIN_PHN_NBR_1"]<-NULL

## 去重
matchPhone %>% unique()

## 两个字段合并后去重计数
length(unique(paste(SL_quote_st_sku$ST,SL_quote_st_sku$SKU)))

## 筛选出ST_sku计数超过2的字段
filter(SL_quote_st_sku %>% group_by(ST,SKU) %>% summarise(cnt = length(ST)),cnt > 1) %>% arrange(desc(cnt))


## 去空格
SL_quote_st_sku_f$ST <- gsub(" ", "", SL_quote_st_sku_f$ST, fixed = TRUE)
SL_quote_st_sku_f$SKU <- gsub(" ", "", SL_quote_st_sku_f$SKU, fixed = TRUE)
PriceBook_SKU$SKU <- trimws(PriceBook_SKU$SKU,which="both")

## quotetype 拉横数据
library(reshape2)
library(dplyr)
temp2 <- reshape2::dcast(EF_table_1, ST+SKU~QUOTETYPE, value.var = "quotetPrice")

## 拉直数据
library(reshape2)
a1 <- a %>% reshape2::melt(id=c("A_NM","B_NM","C_NM") %>% dplyr::arrange(A_NM))


# if else语句
if(phone$rawphone %in% blacklist_phone$MAIN_PHN_NBR){
  phone$flag='1'
}else if(phone$rawphone %in% blacklist_phone$cleanphone){
  phone$flag='1'
}else if(phone$cleanphone  %in% blacklist_phone$MAIN_PHN_NBR){
  phone$flag='1'
}else if(phone$cleanphone  %in% blacklist_phone$cleanphone){
  phone$flag='1'
}else{
  phone$flag='0'
}
  
# 分条件加列
phone$flag <- ifelse((phone$rawphone %in% blacklist_phone$MAIN_PHN_NBR | phone$rawphone %in% blacklist_phone$cleanphone | phone$cleanphone  %in% blacklist_phone$MAIN_PHN_NBR | phone$cleanphone %in% blacklist_phone$cleanphone),1,0)
  
#  选出最新日期
a1<-a %>% dplyr::group_by(ST,SKU) %>% arrange(ST,SKU,desc(HEADER_EFFECIATE_DATE)) %>%dplyr::mutate(index=row_number()) %>% dplyr::filter(index==1)
  
a1 <- a %>% dplyr::group_by(ST,SKU) %>% dplyr::arrange(QUANTITY,desc(HEADER_EFFECIATE_DATE))%>% mutate(rn = rank(desc(HEADER_EFFECIATE_DATE), ties.method = "first")) %>% dplyr::filter(rn==1)

a1 <- a %>% dplyr::group_by(ST,SKU) %>% dplyr::arrange(QUANTITY,desc(HEADER_EFFECIATE_DATE))%>% mutate(id=seq(1,length(paste(ST,SKU)))) %>% dplyr::filter(id==1)



# 去空格大写
wx_segment_view_raw$post_visid_combined<-toupper(gsub(" ", "", wx_segment_view_raw$post_visid_combined, fixed = TRUE))


## 检查两列是否有重复
sl_EFH_table_final_sample$STSKU <-  paste(sl_EFH_table_final_sample$ST,sl_EFH_table_final_sample$SKU,sep = "")
display(sl_EFH_table_final_gsd_sample %>% dplyr::group_by(STSKU) %>% dplyr::summarise(cnt=length(STSKU)) %>% dplyr::arrange(desc(cnt)))


## 不满足条件，终止下面cmd的运行
stopifnot( nrow(sl_listprice_CNKA %>% dplyr::group_by(SKU) %>% dplyr::summarise(cnt=length(SKU)) %>% dplyr::filter(cnt>1)) != 0)
if (nrow(sl_listprice_CNKA %>% dplyr::group_by(SKU) %>% dplyr::summarise(cnt=length(SKU)) %>% dplyr::filter(cnt>1)) != 0) {
    stop("invalid productid please double check if any space or else in, and resave the file or the script will not run")
    }

if model_day==view_result['end_date'].unique().max():
  del view_result['end_date']
else:
  raise BaseException

## 判断是否相等
identical( 2,2) / 2==2


## 不在
sl_quo_sku_e4_pl %>% dplyr::filter( !(sl_quo_sku_e4_pl$SKU %in% Discontinue_SKU$SKU) ) 

## 当一个CONTACT_ID对应多个CUST_ID时，随机选取一条
display(aa<-c%>%dplyr::group_by(CONTACT_ID)%>%dplyr::mutate(cnt=length(CUST_ID)) %>% mutate(seq=row_number()) %>%dplyr::filter(seq==1) )


##生成123456789
sl_EFH_table$quotetPrice <- rownames(sl_EFH_table)

## 非空去掉
fy_transactions_1 <- fy_transactions %>% dplyr::filter(shipToNumber!='')


## 每个ST，随意选择两条
b1 <- SL_ST_SKU_result_2 %>% dplyr::group_by(ST_number)  %>% dplyr::arrange(ST_number) %>% dplyr::mutate(id=seq(1,length(paste(ST_number,SKUNumber,seq="")))) %>% dplyr::filter(id <=2) 


## 指定字段重命名
colnames(sl_st_contact_login)[6] <- 'Login'


b<-sl_EFH_table_final_sample_2[which((sl_EFH_table_final_sample_2$LISTPRICE)=="999999999"),]

table(a$Tag,a$Tag_1)



## 以特定字符把列进行切分(分成多列)
stsku_division_3$division <- strsplit(stsku_division_3$CONNEX_ID,"_")
stsku_division_4 <- data.frame(stsku_division_3,p=sapply(stsku_division_3$division,function(i){i[[1]]}),q=sapply(stsku_division_3$division,function(i){i[[2]]}))


## 生成系统时间的下月一号
lubridate::ymd(paste0(substr(as.character(Sys.Date() %m+% base::months(1)), 1, 8),"01"))
