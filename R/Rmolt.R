#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#Rmolt for molt graph
#Rmolt(data,nb)
#data is the table, row 1 must contain the name of the feather, cm, cp, Allula,
# carpal, tertiaries, secondaries, primaries and rectrices

#nb is the type of graph: 10 is the complete with 10 primaries;
# 9 is a complete graph with 9 primaries;
# "10_0" is a graph with only 10 primaries
#setwd("E:\\Documents\\Cours\\Master_2\\stage\\Rmolt\\Rmolt\\data//")
# @param data a tble with two columns
# @param primaries a parameter to choose the king of graph

#' Rmolt
#' @title{Rmolt}
#' @author  c(person("Martin","Bozon",email = "bozon.etu@gmail.com", role = c("cre","aut")))
#' @importFrom graphics par
#' @importFrom graphics polygon
#' @importFrom graphics text
#' @name Rmolt
#' @description An easy way to create molt graph of passerines wings.
#' 3 different graph available:
#' a full passerine wing with 9 primaries; argument : primarie=9
#' a full passerine wing with 10 primaries; argument: primarie=10
#' only the 10 primaries; argument: primarie="10_0"
#'
#' The data table must have 2 rows and the order of the feathers must be like this:
#'
#' for 9 primaries:
#' c("CM10","CM9","CM8","CM7","CM6","CM5","CM4","CM3","CM2","CM1",
#' "CP1",	"CP2",	"CP3","CP4","CP5","CP6","CP7","CP8",	"CP9",
#' "CC",
#' "A1",	"A2",	"A3",
#' "T3","T2","T1",
#' "S6", "S5","S4","S3","S2","S1"
#' ,"P1", "P2","P3","P4","P5","P6","P7","P8","P9",
#' "R1","R2","R3","R4","R5","R6")
#'
#' for 10 primaries:
#' c("CM10","CM9","CM8","CM7","CM6","CM5","CM4","CM3","CM2","CM1",
#' "CP1",	"CP2",	"CP3","CP4","CP5","CP6","CP7","CP8",	"CP9",
#' "CC",
#' "A1",	"A2",	"A3",
#' "T3","T2","T1",
#' "S6", "S5","S4","S3","S2","S1"
#' ,"P1", "P2","P3","P4","P5","P6","P7","P8","P9","P10"
#' "R1","R2","R3","R4","R5","R6")
#'
#' for only 10 primaries:
#' c("P1", "P2","P3","P4","P5","P6","P7","P8","P9","P10")
#'
#'
#'
#' dcb, fcf and df are examples data table include in this package
#' @return Don't return value, print molt graph.
#' @param data a data table to create the graph
#' @param primaries an argument to choose the graph
#'


#' @examples
#' data(df)
#' Rmolt(df,"10_0")
#'
#' data(fcf)
#' Rmolt(fcf,10)
#'
#'
#' data(dcb)
#' Rmolt(dcb,9)
#'
#'
#'
#'
#'
#' @export

globalVariables(c("Rmolt_env","feather","primaries"))



Rmolt <- function(data,primaries) {




  primaries <- primaries

  if (primaries == 10){  #9 primaries
    primarie_10(i,data)

    }

  if(primaries == 9){   #10 primaries

    primarie_9(i,data)}


  if(primaries == "10_0"){  #only 10 primaries

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(mar = c(0,0,0,0) + 0.1)
    plot(1,1, col = "white", xlab = "", ylab = ""
         ,xlim=c(0,15), ylim=c(-2,25),axes= FALSE)


    a <-0
    b <- 1
    c <- 0.5
    d <- 15
    e <- 1.2



    polygon (x=c(0+e, 0+e, 1+e, 1+e, 0.5+e),
                  y=c(20, 22, 22, 20, 20),
                  col="white")
    C1 <- "white"
    text(2.3, 23, "0%",cex=1)

    C2 <-polygon (x=c(a+e*2, e*2+a, e*2+b, e*2+b, e*2+c),
                  y=c(20, 22, 22, 20, 20),
                  col="gray90")
    C2 <- "gray90"
    text(2.3+e, 23, "5%",cex=1)

    C3 <-polygon (x=c(a+e*3, e*3+a, e*3+b, e*3+b, e*3+c),
                  y=c(20, 22, 22, 20, 20),
                  col="gray80")
    C3 <- "gray80"
    text(2.3+e*2, 23, "10%",cex=1)

    C4 <-polygon (x=c(a+e*4, e*4+a, e*4+b, e*4+b, e*4+c),
                  y=c(20, 22, 22, 20, 20),
                  col="lightyellow3")
    C4 <- "lightyellow3"
    text(2.3+e*3, 23, "25%",cex=1)

    C5 <-polygon (x=c(a+e*5, e*5+a, e*5+b, e*5+b, e*5+c),
                  y=c(20, 22, 22, 20, 20),
                  col="gray57")
    C5 <- "gray57"
    text(2.3+e*4, 23, "50%",cex=1)

    C6 <-polygon (x=c(a+e*6, e*6+a, e*6+b, e*6+b, e*6+c),
                  y=c(20, 22, 22, 20, 20),
                  col="gray37")
    C6 <- "gray37"
    text(2.3+e*5, 23, "75%",cex=1)

    C7 <-polygon (x=c(a+e*7, e*7+a, e*7+b, e*7+b, e*7+c),
                  y=c(20, 22, 22, 20, 20),
                  col="gray20")
    C7 <- "gray20"
    C8 <-polygon (x=c(a+e*8, e*8+a, e*8+b, e*8+b, e*8+c),
                  y=c(20, 22, 22, 20, 20),
                  col="gray0")
    C8 <- "gray0"
    text(2.3+e*6, 23, "90%",cex=1)
    text(2.3+e*7, 23, "100%",cex=1)


    for(i in 1:9) {


      P3 <-polygon (x=c(a+e*i, e*i+a, e*i+b, e*i+b, e*i+c),
                    y=c(b, d, d, b, a),
                    col=moult_color(i,data))
      text(c+e*i, -1, data[i,1])
    }


    P10 <-polygon (x=c(a+e*(i+1), e*(i+1)+a, e*(i+1)+b, b+e*(i+1), c+e*(i+1)),
                   y=c(4, d, d, 4, 3),
                   col=moult_color(i,data))
    text(c+e*i+1.2, 2, "P10")

  }
}

