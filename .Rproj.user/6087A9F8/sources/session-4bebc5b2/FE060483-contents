#' Graphing with data
#'
#' Using Data for Mapping
#'
#' @param infect  List of the reasult.
#'
#' @return  plots.
#'
#' @examples
#' scdczx.plot(data)
#'
#' @export
#' @importFrom ggplot2


scdczx.plot <- function(infect) {
  library(ggplot2)
  for (i in 1: (length(infect)-2)) {


    lname <- colnames(infect[[i]][1])

    lnamefactor <- as.symbol(lname)

    zx <- infect[i]
    zx <- as.data.frame(zx)
    zx <- spread(zx,key= lnamefactor, value = n)

    zx <- as.matrix(zx)

    pie(zx,main=lname,labels = colnames(zx),col=rainbow(length(zx)))

  }

  nvplot <- infect[[6]]
  nvplot$mon <- as.numeric(as.character(nvplot$mon))
  nvplot <- arrange(nvplot,by=mon)
  nvplot$mon <-   as.factor(nvplot$mon)
  nvplot %>% ggplot(aes(
    x = mon,
    y = Freq,
    group = year,
    color = year,
  )) +
    geom_line()



}

