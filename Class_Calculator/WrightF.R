

# This section covers the functions for the Wright Simplified Schema # 
decisions<-function(input){
  if (input$D1s ==2 & input$D2as ==2 & input $D2a1s==5){
    return(1)
  }
  else if (input$D1s ==1 & input$D2as ==1 & input $D2a1s==4){
    return(2)
  }
  else if (input$D1s ==1 & input$D2as ==2 & input $D2a1s==5){
    return(1)
  }
  else if (input$D1s ==1 & input$D2as ==1 & input$D2a1s <4){
    return(3)
  }
}

authoritys<-function(input){
  if (input$C1s ==2){
    return(1)
  }
  else if (input$C1s ==1 & input$C2s ==1 & input$C3s ==1){
    return(1)
  }
  else if (input$C1s ==1 & input$C2s ==2 & input$C3as ==2 & input$C4as ==2){
    return(2)
  }
  else if (input$C1s ==1 & input$C2s ==2 & input$C3as ==1 & input$C4as ==2){
    return(3)
  }
  else if (input$C1s ==1 & input$C2s ==2 & input$C4as ==1){
    return(4)
  }
}

hierarchys<-function(input){
  if (input$H1s ==1){
    return(1)
  }
  else if (input$H1s ==2){
    return(2)
  }
  else if (input$H1s ==3){
    return(3)
  }
}

managerials <- function(input){
  if (decisions(input) ==3 & authoritys(input) ==3 & hierarchys(input) ==1){
    return(1)
  }
  else if (decisions(input) ==3 & authoritys(input) ==3 & hierarchys(input) ==2){
    return(1)
  }
  else if (decisions(input) ==3 & authoritys(input) ==4 & hierarchys(input) ==1){
    return(1)
  }
  else if (decisions(input) ==3 & authoritys(input) ==4 & hierarchys(input) ==2){
    return(1)
  }
  
  else if (decisions(input) ==3 & authoritys(input) ==3 & hierarchys(input) ==3){
    return(2)
  }
  else if (decisions(input) ==3 & authoritys(input) ==4 & hierarchys(input) ==3){
    return(2)
  }
  
  else if (decisions(input) ==3 & authoritys(input) ==1 & hierarchys(input) ==1){
    return(3)
  }
  else if (decisions(input) ==3 & authoritys(input) ==2 & hierarchys(input) ==1){
    return(3)
  }
  else if (decisions(input) ==3 & authoritys(input) ==1 & hierarchys(input) ==2){
    return(3)
  }
  else if (decisions(input) ==3 & authoritys(input) ==2 & hierarchys(input) ==2){
    return(3)
  }
  
  else if (decisions(input) ==3 & authoritys(input) ==1 & hierarchys(input) ==3){
    return(4)
  }
  else if (decisions(input) ==3 & authoritys(input) ==2 & hierarchys(input) ==3){
    return(4)
  }
  
  else if (decisions(input) ==2 & authoritys(input) ==3 & hierarchys(input) ==1){
    return(5)
  }
  else if (decisions(input) ==2 & authoritys(input) ==4 & hierarchys(input) ==1){
    return(5)
  }
  else if (decisions(input) ==2 & authoritys(input) ==3 & hierarchys(input) ==2){
    return(5)
  }
  else if (decisions(input) ==2 & authoritys(input) ==4 & hierarchys(input) ==2){
    return(5)
  }
  
  else if (decisions(input) ==2 & authoritys(input) ==3 & hierarchys(input) ==3){
    return(6)
  }
  else if (decisions(input) ==2 & authoritys(input) ==4 & hierarchys(input) ==3){
    return(6)
  }
  
  else if (decisions(input) ==2 & authoritys(input) ==1 & hierarchys(input) ==1){
    return(7)
  }
  else if (decisions(input) ==2 & authoritys(input) ==2 & hierarchys(input) ==1){
    return(7)
  }
  else if (decisions(input) ==2 & authoritys(input) ==1 & hierarchys(input) ==2){
    return(7)
  }
  else if (decisions(input) ==2 & authoritys(input) ==2 & hierarchys(input) ==2){
    return(7)
  }
  
  else if (decisions(input) ==2 & authoritys(input) ==1 & hierarchys(input) ==3){
    return(8)
  }
  else if (decisions(input) ==2 & authoritys(input) ==2 & hierarchys(input) ==3){
    return(8)
  }
  
  else if (decisions(input) ==1 & authoritys(input) ==4 & hierarchys(input) ==1){
    return(9)
  }
  else if (decisions(input) ==1 & authoritys(input) ==4 & hierarchys(input) ==2){
    return(9)
  }
  
  else if (decisions(input) ==1 & authoritys(input) ==3 & hierarchys(input) ==1){
    return(10)
  }
  else if (decisions(input) ==1 & authoritys(input) ==3 & hierarchys(input) ==2){
    return(10)
  }
  
  else if (decisions(input) ==1 & authoritys(input) ==2 & hierarchys(input) ==1){
    return(11)
  }
  else if (decisions(input) ==1 & authoritys(input) ==2 & hierarchys(input) ==2){
    return(11)
  }
  
  else if (decisions(input) ==1 & authoritys(input) ==4 & hierarchys(input) ==3){
    return(12)
  }
  
  else if (decisions(input) ==1 & authoritys(input) ==3 & hierarchys(input) ==3){
    return(13)
  }
  
  else if (decisions(input) ==1 & authoritys(input) ==1 & hierarchys(input) ==1){
    return(14)
  }
  else if (decisions(input) ==1 & authoritys(input) ==1 & hierarchys(input) ==2){
    return(14)
  }
  
  else if (decisions(input) ==1 & authoritys(input) ==1 & hierarchys(input) ==3){
    return(15)
  }
  else if (decisions(input) ==1 & authoritys(input) ==2 & hierarchys(input) ==3)
  {
    return(15)
  }
}


organisations<-function(input){
  if (managerials(input) ==1){
    return(1)
  }
  else if(managerials(input) ==2){
    return(1)
  }
  else if(managerials(input) ==3){
    return(1)
  }
  else if(managerials(input) ==4){
    return(3)
  }
  else if(managerials(input) ==5){
    return(2)
  }
  else if(managerials(input) ==6){
    return(2)
  }
  else if(managerials(input) ==7){
    return(2)
  }
  else if(managerials(input) ==8){
    return(3)
  }
  else if(managerials(input) ==9){
    return(2)
  }
  else if(managerials(input) ==10){
    return(2)
  }
  else if(managerials(input) ==11){
    return(2)
  }
  else if(managerials(input) ==12){
    return(2)
  }
  else if(managerials(input) ==13){
    return(3)
  }
  else if(managerials(input) ==14){
    return(3)
  }
  else if(managerials(input) ==15){
    return(3)
  }
}

skillss<-function(input){
  if (input$O1s ==1){
    return(1)
  }
  else if (input$O1s ==2){
    return(1)
  }
  else if (input$O1s ==3 & input$E1s ==1){
    return(1)
  }
  
  else if (input$O1s ==4){
    return(2)
  }
  else if (input$O1s ==5){
    return(2)
  }
  else if (input$O1s ==3 & input$E1s ==2){
    return(2)
  }
  else if (input$O1s ==6){
    return(2)
  }
  else if (input$O1s ==7 & input$E1s ==1 & input$B1s ==1){
    return(2)
  }
  else if (input$O1s ==8 & input$E1s ==1 & input$B1s ==1){
    return(2)
  }
  
  else if (input$O1s ==7 & input$E1s ==2){
    return(3)
  }
  else if (input$O1s ==7 & input$B1s ==2){
    return(3)
  }
  else if (input$O1s ==8 & input$E1s ==2){
    return(3)
  }
  else if (input$O1s ==8 & input$B1s ==2){
    return(3)
  }
  else if (input$O1s ==9){
    return(3)
  }
}

capitals<-function(input){
  if (input$SelfEmployments ==2 & input$WNoEs ==3){
    return(1)
  }
  else if (input$SelfEmployments ==2 & input$WNoEs ==2){
    return(2)
  }
  else if (input$SelfEmployments ==2 & input$WNoEs ==1){
    return(3)
  }
  else if (input$SelfEmployments ==1 & input$WNoEs ==4){
    return(4)
  }
}