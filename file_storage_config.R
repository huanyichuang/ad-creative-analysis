input.file <- "./rawdata/reports.csv"
input.temp <- read_csv(file(input.file))
appeal.file <- "./rawdata/appeal.csv"
appeal.temp <- read.csv(file(appeal.file),stringsAsFactors = FALSE)
creative.file <- "./rawdata/creatives.csv"
creative.temp <- read.csv(file(creative.file),stringsAsFactors = FALSE)
ga.file <- "ga.csv"
ga.colname <- c("分析報告開始","utm_campaign","utm_content","Session","GACV")
ga.temp <- read_csv(file(ga.file),col_names=ga.colname,skip = 7)
ga.temp$分析報告開始 <- ymd(ga.temp$分析報告開始)
output.file <- paste0("report-",Sys.Date(),".csv")
