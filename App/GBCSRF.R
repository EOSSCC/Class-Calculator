




# This section covers the functions for the GBCS schema # 




GBCSRF <- function(input){
  highr.s <- (sum(as.numeric(input$carts))+sum(as.numeric(input$cmusgall))+sum(as.numeric(input$cstathom))+sum(as.numeric(input$ctheatre))+sum(as.numeric(input$copera))+sum(as.numeric(input$mclassic))+sum(as.numeric(input$mjazz))+sum(as.numeric(input$ffrench))+sum(as.numeric(input$cdance))-2)
  emergingr.s <- (sum(as.numeric(input$ccompgam))+sum(as.numeric(input$csocnet))+sum(as.numeric(input$csportp))+sum(as.numeric(input$csportw))+sum(as.numeric(input$cfriends))+sum(as.numeric(input$cgym))+sum(as.numeric(input$cgig))+sum(as.numeric(input$mrap))+sum(as.numeric(input$cnet))+sum(as.numeric(input$mrock))-2)
  socialr.n <- length(input$socialr)
  if (input$incomer >=89 & input$savingsr >=100 & input$homevaluer >=325 & mean(as.numeric(input$socialr >=50.1)) & socialr.n>=16 & highr.s>=17 & emergingr.s>=14){
    return(1)
  }
  else if (input$incomer >=47 & input$savingsr >=26 & input$homevaluer >=177 & mean(as.numeric(input$socialr >=45.3)) & socialr.n>=17 & highr.s>=14 & emergingr.s>=17){
    return(2)
  }
  else if (input$incomer >=37 & input$savingsr >=66 & input$homevaluer >=163 & mean(as.numeric(input$socialr >=53.5)) & socialr.n>=4 & highr.s>=9 & emergingr.s>=11){
    return(3)
  }
  else if (input$incomer >=29 & input$savingsr >=5 & input$homevaluer >=129 & mean(as.numeric(input$socialr >=37.8)) & socialr.n>=17 & highr.s>=7 & emergingr.s>=15){
    return(4)
  }
  else if (input$incomer >=13 & input$savingsr >=10 & input$homevaluer >=127 & mean(as.numeric(input$socialr >=41.5)) & socialr.n>=10 & highr.s>=11 & emergingr.s>=7){
    return(5)
  }
  else if (input$incomer >=21 & input$savingsr >=1 & input$homevaluer >=18 & mean(as.numeric(input$socialr >=38.3)) & socialr.n>=15 & highr.s>=10 & emergingr.s>=17.5){
    return(6)
  }
  else if (input$incomer <=8 & input$savingsr <=1 & input$homevaluer <=27 & mean(as.numeric(input$socialr <=30)) & socialr.n<=7 & highr.s<=6 & emergingr.s<=8){
    return(7)
  }
}

