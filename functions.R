#creative_group 是用來將素材做篩選分類的函數
creative_group <- function (data,col="",goalFilter="",setFilter="",adFilter="") {
  data %>%
    filter(grepl(goalFilter,成果指標)) %>%
    group_by(.dots=col) %>%
    summarize(Impressions = sum(曝光次數,na.rm=TRUE),
              Clicks = sum(連結點擊次數,na.rm=TRUE),
              CTR=sum(連結點擊次數,na.rm=TRUE)/sum(曝光次數,na.rm=TRUE),
              CVs=sum(網站購買..點擊後.1.天.,na.rm=TRUE),
              CVR=sum(網站購買..點擊後.1.天.,na.rm = TRUE)/sum(連結點擊次數,na.rm=TRUE),
              Spend=sum(花費金額..USD.*currency.rate,na.rm=TRUE),
              CPM=Spend/Impressions*1000,
              CPA=sum(花費金額..USD.*currency.rate,na.rm=TRUE)/sum(網站購買..點擊後.1.天.,na.rm=TRUE)
    ) %>%
    mutate(CTRStd=(sqrt(CTR*(1-CTR)/Impressions))*100,
           CVRStd=(sqrt(CVR*(1-CVR)/Clicks))*100,
           CTR=CTR*100,
           CVR=CVR*100
    )
}