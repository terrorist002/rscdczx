#' Data preprocessing for cdc
#'
#' Preprocessing of infectious disease reporting card data
#'
#' @param szk Infectious disease reporting card data
#'
#' @return  List of the reasult
#'
#' @examples
#' scdczx(data)
#'
#' @export
#' @importFrom tidyr
#' @importFrom dplyr



scdczx <- function(data) {
  library(tidyr)
  library(dplyr)
  data <- data %>% filter(审核状态 != "已删除卡")  #去除重卡

  szkdate <- data %>%
    separate(发病日期, into = c("year", "mon" ,"day"), sep = "/") %>% select( year, mon ,day, 性别 , 病人属于 , 重症患者, 人群分类 ,病例分类)   #拆分日期

  #制作数据，制成列表
  nybk <- xtabs(~ mon+ year,data=szkdate)  #不同月份每年的报卡 四格表

  humanclass <- szkdate %>% group_by(人群分类) %>% summarise(n = n())

  belongclass <- szkdate %>% group_by(病人属于) %>% summarise(n = n())
  severeclass <- szkdate %>% group_by(重症患者) %>% summarise(n = n())
  grade <- szkdate %>% group_by(性别) %>% summarise(n = n())

  yearmonbk <- xtabs(~ mon+ year,data=szkdate)
  nv <- as.data.frame(yearmonbk)
  ny <- spread(nv,key=year,value=Freq)
  ny$mon <- as.numeric(as.character(ny$mon))
  ny <-  arrange(ny,by=mon)

  total <- list(humanclass,belongclass,severeclass,grade,ny,nv)

  return(total)


}
