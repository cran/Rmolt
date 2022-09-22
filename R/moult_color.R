#Rmolt for molt graph
#set the color in function of the percentage value
#' moult_color
#' @title moult_color
#' @name moult_color
#' @param i a value to run the loop
#' @param data a data table to choose the color
#' @return color of the feather depending of the percentage, called for side effect
#' @export

globalVariables(c("Rmolt_env","feather"))
moult_color <- function(i,data){


  if(data[i,2]==0){
    feather = "white"
  }

  if (data[i,2]>=0.00001 & data[i,2]<=0.05){
    feather = "gray90"
    }
  if (data[i,2]>=0.0500001 & data[i,2]<=0.10){
  feather = "gray80"
    }
  if (data[i,2]>=0.100001 & data[i,2]<=0.25){
    feather = "lightyellow3"
  }
  if (data[i,2]>=0.251 & data[i,2]<=0.50){
    feather = "gray57"
    }
  if (data[i,2]>=0.5000001 & data[i,2]<=0.75){
    feather = "gray37"
    }
  if (data[i,2]>=0.7500001 & data[i,2]<=0.90){
    feather = "gray20"
    }
  if (data[i,2]>=0.9000001){
    feather = "gray0"
   }

  feather <- feather

}
