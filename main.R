require(tidyverse)
require(lubridate)
require(FSelector)
require(Rfacebook)

source("auth/auth")
source("functions.R")
source("file_storage_config.R")
source("variable_config.R")

# Request a user code:
fb_app <- oauth_app(appname = "facebook",
                    key = app_id,
                    secret = client_token)
#fb_token <- oauth2.0_token(oauth_endpoints("facebook"),
#             fb_app,
#             scope = c('public_profile','ads_read'),
#             type = "application/x-www-form-urlencoded",
#             cache = TRUE)
fb_token <- fbOAuth(app_id, client_token)
# GET request for your user information
fb_args <- "?fields=impressions,spend,ad_id,adset_id&level=ad"
response <- GET("https://graph.facebook.com",
                path = "/v6.0/act_1194617000638527/insights",
                config = config(token = fb_token))

input.temp$廣告名稱<-gsub(" - 複本","",input.temp$廣告名稱) #把" - 複本"拿掉
input.temp$分析報告開始 <- ymd(input.temp$分析報告開始)
input.temp$分析報告結束 <- ymd(input.temp$分析報告結束)

input.raw <- input.temp %>%
  filter(曝光次數>0) %>%
  mutate(CTR=連結點擊次數/曝光次數,
         CVR=網站購買..點擊後.1.天./連結點擊次數,
         #CVR=成果/連結點擊次數,
         month=month(分析報告開始),
         week=week(分析報告開始)) %>%
  left_join(sokyu.temp,by="廣告名稱") 

write_csv(input.raw,"Facebook Processed")
input.creativeSum <- input.raw %>%
  group_by(分析報告開始,廣告名稱,utm_campaign,utm_content) %>%
  summarise(Impression=sum(曝光次數,na.rm=TRUE),
            CPM=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/Impression*1000,
            CTR=sum(連結點擊次數,na.rm=TRUE)/sum(曝光次數,na.rm=TRUE),
            Clicks=sum(連結點擊次數,na.rm=TRUE),
            CVR=sum(網站購買..點擊後.1.天.,na.rm = TRUE)/sum(連結點擊次數,na.rm=TRUE),
            CV=sum(網站購買..點擊後.1.天.,na.rm=TRUE),
            CPA=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/sum(網站購買..點擊後.1.天.,na.rm=TRUE),
            Spend=sum(花費金額..USD.*currency.rate,na.rm=TRUE),
            CTRStd=(sqrt(CTR*(1-CTR)/Impression))*100,
            CVRStd=(sqrt(CVR*(1-CVR)/Clicks))*100,
            CTR=CTR*100,
            CVR=CVR*100) %>%
  left_join(ga.temp,by=c("分析報告開始","utm_campaign","utm_content")) %>%
  mutate(GACPA=Spend/GACV)
write_csv(input.creativeSum,output.file)
input.na <- input.raw[is.na(input.raw$視覺訴求),] #檢查訴求為 na 的值
input.na <- input.raw[is.na(input.raw$GACV),] #檢查訴求為 na 的值

input.raw[is.na(input.raw)] <- 0
input.noKOL <- input.raw %>% filter(!grepl(paste(kol.list, collapse="|"),廣告名稱))

#Summary
summary <- input.raw %>% 
  group_by(行銷活動名稱) %>%
  #group_by(month) %>%
  #filter(分析報告開始>="2018-08-20") %>%
  summarise(Impression=sum(曝光次數,na.rm=TRUE),
            CPM=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/Impression*1000,
            CTR=sum(連結點擊次數,na.rm=TRUE)/sum(曝光次數,na.rm=TRUE),
            Clicks=sum(連結點擊次數,na.rm=TRUE),
            CVR=sum(網站購買..點擊後.1.天.,na.rm = TRUE)/sum(連結點擊次數,na.rm=TRUE),
            #CVR=sum(成果,na.rm = TRUE)/sum(連結點擊次數,na.rm=TRUE),
            CV=sum(網站購買..點擊後.1.天.,na.rm=TRUE),
            #CV=sum(成果,na.rm=TRUE),
            CPA=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/sum(網站購買..點擊後.1.天.,na.rm=TRUE),
            #GACPA=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/sum(GACV,na.rm=TRUE),
            #CPA=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/sum(成果,na.rm=TRUE),
            Spend=sum(花費金額..USD.*currency.rate,na.rm=TRUE),
            CTRStd=(sqrt(CTR*(1-CTR)/Impression))*100,
            CVRStd=(sqrt(CVR*(1-CVR)/Clicks))*100,
            CTR=CTR*100,
            CVR=CVR*100)
summary.noKOL <- input.noKOL %>% 
  group_by(廣告名稱) %>%
  #filter(分析報告開始>="") %>%
  summarise(Impression=sum(曝光次數,na.rm=TRUE),
            CPM=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/Impression*1000,
            CTR=sum(連結點擊次數,na.rm=TRUE)/sum(曝光次數,na.rm=TRUE),
            Clicks=sum(連結點擊次數,na.rm=TRUE),
            CVR=sum(網站購買..點擊後.1.天.,na.rm = TRUE)/sum(連結點擊次數,na.rm=TRUE),
            CV=sum(網站購買..點擊後.1.天.,na.rm=TRUE),
            CPA=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/sum(網站購買..點擊後.1.天.,na.rm=TRUE),
            Spend=sum(花費金額..USD.*currency.rate,na.rm=TRUE),
            CTRStd=(sqrt(CTR*(1-CTR)/Impression))*100,
            CVRStd=(sqrt(CVR*(1-CVR)/Clicks))*100,
            CTR=CTR*100,
            CVR=CVR*100)

#圖表區
ggplot(data=input.raw) + 
  theme(text=element_text(family="Noto Sans CJK TC")) +
  geom_bar(mapping=aes(x=標題訴求,y=網站購買..點擊後.1.天.),stat="identity")  
input.raw %>% 
  filter(grepl("導購|KOL",行銷活動名稱)) %>%
  ggplot(mapping = aes(x=花費金額..USD.*currency.rate,y=網站購買..點擊後.1.天.,fill=視覺訴求)) + 
  theme(text=element_text(family="Noto Sans CJK TC")) +
  geom_point(position="jitter") +
  geom_smooth()


input.raw %>%
  group_by(廣告名稱) %>%
  boxplot(網站購買..點擊後.1.天.)
