
#Rmolt for molt graph
#draw the feathers' graph in function of the percentage values
#' primarie_9
#' @title{primarie_9}
#' @name primarie_9
#' @param i a value to run the loop
#' @param data a data table to choose the color
#' @return No return value but etablish a molt graph for a 9 primaries' bird
#' @examples
#' primarie_9(2,dcb)
#'
#'
#'
#' @export
#'

globalVariables(c("Rmolt_env","feather"))

primarie_9 <-function(i,data){

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(0,0,0,0) + 0.1)
  plot(1,1, col = "white", xlab = "", ylab = ""
       ,xlim=c(0,35), ylim=c(6,50),axes= FALSE)


  a <-0
  b <- 1
  c <- 0.5
  d <- 15
  e <- 1.2

  percentage()

  for(i in 1:10) {  #CM



    CM <-polygon (x=c(a+e*i, a+e*i, b+e*i, b+e*i, c+e*i),
                  y=c(36, 38, 38, 36, 36),
                  col=moult_color(i,data))
    text(c+e*i, 39, data[i,1],cex=0.7)


  }
  for(i in 11:19) {  #CP



    CM <-polygon (x=c(a+e*i, e*i+a, e*i+b, e*i+b, e*i+c),
                  y=c(37, 38, 38, 37, 36),
                  col=moult_color(i,data))
    text(c+e*i, 39, data[i,1],cex=0.7)

  }
  q=1
  for(i in 21:23) {  #Allula




    A1 <-polygon(x=c(22, 22, 22.5+q, 23+q, 22.5+q),
                 y=c(39.6+1.2*q, 40.6+1.2*q, 40.6+1.2*q, 40.1+1.2*q, 39.6+1.2*q),
                 col=moult_color(i,data))
    text(21.5, 40.1+1.2*q, data[i,1],cex=0.7)

    q=q+1

  }#carpal
  moult_color(20,data)

  CARP <-polygon (x=c(15, 15, 16, 16, 16),
                  y=c(41, 43, 43, 41, 41),
                  col=moult_color(i,data))
  text(15.5, 44, data[20,1],cex=0.7)
  q=0

  for(i in 24:26) {  #Tertiaries



    T <-polygon (x=c(2.4+a+e*q, e*q+2.4+a, e*q+2.4+b, e*q+2.4+b, e*q+2.4+c),
                 y=c(22-q*6, 32, 32, 22-q*6, 20-q*6),
                 col=moult_color(i,data))
    text(2.4+c+e*q, 19-q*6, data[i,1],cex=0.7)
    q=q+1

  }

  q=0
  for(i in 27:40) {  #primaries



    T <-polygon (x=c(6+a+e*q, e*q+6+a, e*q+6+b, e*q+6+b, e*q+6+c),
                 y=c(15, 32, 32, 15, 13),
                 col=moult_color(i,data))
    text(6+c+e*q, 12, data[i,1],cex=0.7)
    q=q+1

  }

  moult_color(i+1,data)

  P10 <-polygon (x=c(6+a+e*q, e*q+6+a, e*q+b+6, b+e*q+6, 6+c+e*q),
                 y=c(19, 32,32, 19, 17),
                 col=moult_color(i,data))
  text(6+c+e*q, 16, "P9",cex=0.7)





  for(i in 42:47) {  #rectrices



    T <-polygon (x=c(10.8+a+e*q, e*q+10.8+a, e*q+10.8+b, e*q+10.8+b, e*q+10.8+c),
                 y=c(15, 32, 32, 15, 13),
                 col=moult_color(i,data))
    text(10.8+c+e*q, 12, data[i,1],cex=0.7)
    q=q+1

  }
}

