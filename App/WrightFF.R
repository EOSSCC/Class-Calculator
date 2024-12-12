

decisiont<-function(input){
  if (input$DM ==2){
    return(1)
  }
  else if (input$DM ==1 & input$DMA ==1 & input$DMA1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DMB ==1 & input$DMB1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DMC ==1 & input$DMC1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DMD ==1 & input$DMD1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DME ==1 & input$DME1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DMF ==1 & input$DMF1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DMG ==1 & input$DMG1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DMH ==1 & input$DMH1==4){
    return(2)
  }
  else if (input$DM ==1 & input$DMA ==1 & input$DMA1 <4){
    return(3)
  }
  else if (input$DM ==1 & input$DMB ==1 & input$DMB1 <4){
    return(3)
  }
  else if (input$DM ==1 & input$DMC ==1 & input$DMC1 <4){
    return(3)
  }
  else if (input$DM ==1 & input$DMD ==1 & input$DMD1 <4){
    return(3)
  }
  else if (input$DM ==1 & input$DME ==1 & input$DME1 <4){
    return(3)
  }
  else if (input$DM ==1 & input$DMF ==1 & input$DMF1 <4){
    return(3)
  }
  else if (input$DM ==1 & input$DMG ==1 & input$DMG1 <4){
    return(3)
  }
  else if (input$DM ==1 & input$DMH ==1 & input$DMH1 <4){
    return(3)
  }
  else{
    return(1)
  }
}

authorityt<-function(input){
  if (input$C1S ==2){
    return(1)
  }
  else if (input$C1S ==1 & input$C2S ==1){
    return(1)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C3S ==1 & input$C3SA ==2 & input$C3SB ==2 & input$C3SC ==2 & input$C4A ==2 & input$C4B ==2 & input$C4C ==2 & input$C4D ==2){
    return(2)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C3SA ==2 & input$C3SB ==2 & input$C3SC ==2 & input$C4A ==2 & input$C4B ==2 & input$C4C ==2 & input$C4D ==2){
    return(2)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C3SA ==1 & input$C4A ==2 & input$C4B ==2 & input$C4C ==2 & input$C4D ==2){
    return(3)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C3SB ==1 & input$C4A ==2 & input$C4B ==2 & input$C4C ==2 & input$C4D ==2){
    return(3)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C3SC ==1 & input$C4A ==2 & input$C4B ==2 & input$C4C ==2 & input$C4D ==2){
    return(3)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C4A ==1){
    return(4)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C4B ==1){
    return(4)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C4C ==1){
    return(4)
  }
  else if (input$C1S ==1 & input$C2S ==2 & input$C4D ==1){
    return(4)
  }
  else {
    return(1)
  }
}



hierarchyt<-function(input){
  if (input$H1S ==1){
    return(1)
  }
  else if (input$H1S ==2){
    return(2)
  }
  else if (input$H1S ==3){
    return(3)
  }
  else {
    return(1)
  }
}

managerialt <- function(input){
  if (decisiont(input) ==3 & authorityt(input) ==3 & hierarchyt(input) ==1){
    return(1)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==3 & hierarchyt(input) ==2){
    return(1)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==4 & hierarchyt(input) ==1){
    return(1)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==4 & hierarchyt(input) ==2){
    return(1)
  }
  
  else if (decisiont(input) ==3 & authorityt(input) ==3 & hierarchyt(input) ==3){
    return(2)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==4 & hierarchyt(input) ==3){
    return(2)
  }
  
  else if (decisiont(input) ==3 & authorityt(input) ==1 & hierarchyt(input) ==1){
    return(3)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==2 & hierarchyt(input) ==1){
    return(3)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==1 & hierarchyt(input) ==2){
    return(3)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==2 & hierarchyt(input) ==2){
    return(3)
  }
  
  else if (decisiont(input) ==3 & authorityt(input) ==1 & hierarchyt(input) ==3){
    return(4)
  }
  else if (decisiont(input) ==3 & authorityt(input) ==2 & hierarchyt(input) ==3){
    return(4)
  }
  
  else if (decisiont(input) ==2 & authorityt(input) ==3 & hierarchyt(input) ==1){
    return(5)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==4 & hierarchyt(input) ==1){
    return(5)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==3 & hierarchyt(input) ==2){
    return(5)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==4 & hierarchyt(input) ==2){
    return(5)
  }
  
  else if (decisiont(input) ==2 & authorityt(input) ==3 & hierarchyt(input) ==3){
    return(6)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==4 & hierarchyt(input) ==3){
    return(6)
  }
  
  else if (decisiont(input) ==2 & authorityt(input) ==1 & hierarchyt(input) ==1){
    return(7)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==2 & hierarchyt(input) ==1){
    return(7)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==1 & hierarchyt(input) ==2){
    return(7)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==2 & hierarchyt(input) ==2){
    return(7)
  }
  
  else if (decisiont(input) ==2 & authorityt(input) ==1 & hierarchyt(input) ==3){
    return(8)
  }
  else if (decisiont(input) ==2 & authorityt(input) ==2 & hierarchyt(input) ==3){
    return(8)
  }
  
  else if (decisiont(input) ==1 & authorityt(input) ==4 & hierarchyt(input) ==1){
    return(9)
  }
  else if (decisiont(input) ==1 & authorityt(input) ==4 & hierarchyt(input) ==2){
    return(9)
  }
  
  else if (decisiont(input) ==1 & authorityt(input) ==3 & hierarchyt(input) ==1){
    return(10)
  }
  else if (decisiont(input) ==1 & authorityt(input) ==3 & hierarchyt(input) ==2){
    return(10)
  }
  
  else if (decisiont(input) ==1 & authorityt(input) ==2 & hierarchyt(input) ==1){
    return(11)
  }
  else if (decisiont(input) ==1 & authorityt(input) ==2 & hierarchyt(input) ==2){
    return(11)
  }
  
  else if (decisiont(input) ==1 & authorityt(input) ==4 & hierarchyt(input) ==3){
    return(12)
  }
  
  else if (decisiont(input) ==1 & authorityt(input) ==3 & hierarchyt(input) ==3){
    return(13)
  }
  
  else if (decisiont(input) ==1 & authorityt(input) ==1 & hierarchyt(input) ==1){
    return(14)
  }
  else if (decisiont(input) ==1 & authorityt(input) ==1 & hierarchyt(input) ==2){
    return(14)
  }
  
  else if (decisiont(input) ==1 & authorityt(input) ==1 & hierarchyt(input) ==3){
    return(15)
  }
  else if (decisiont(input) ==1 & authorityt(input) ==2 & hierarchyt(input) ==3)
  {
    return(15)
  }
  else {
    return(15)
  }
}

organisationt<-function(input){
  if (managerialt(input) ==1){
    return(1)
  }
  else if(managerialt(input) ==2){
    return(1)
  }
  else if(managerialt(input) ==3){
    return(1)
  }
  else if(managerialt(input) ==4){
    return(3)
  }
  else if(managerialt(input) ==5){
    return(2)
  }
  else if(managerialt(input) ==6){
    return(2)
  }
  else if(managerialt(input) ==7){
    return(2)
  }
  else if(managerialt(input) ==8){
    return(3)
  }
  else if(managerialt(input) ==9){
    return(2)
  }
  else if(managerialt(input) ==10){
    return(2)
  }
  else if(managerialt(input) ==11){
    return(2)
  }
  else if(managerialt(input) ==12){
    return(2)
  }
  else if(managerialt(input) ==13){
    return(3)
  }
  else if(managerialt(input) ==14){
    return(3)
  }
  else if(managerialt(input) ==15){
    return(3)
  }
  else {
    return(1)
  }
}

skillst<-function(input){
  if (input$O1S ==1){
    return(1)
  }
  else if (input$O1S ==2){
    return(1)
  }
  else if (input$O1S ==3 & input$E1S ==1){
    return(1)
  }
  
  else if (input$O1S ==4){
    return(2)
  }
  else if (input$O1S ==5){
    return(2)
  }
  else if (input$O1S ==3 & input$E1S ==2){
    return(2)
  }
  else if (input$O1S ==6){
    return(2)
  }
  else if (input$O1S ==7 & input$E1S ==1 & input$B1S ==1){
    return(2)
  }
  else if (input$O1S ==8 & input$E1S ==1 & input$B1S ==1){
    return(2)
  }
  
  else if (input$O1S ==7 & input$E1S ==2){
    return(3)
  }
  else if (input$O1S ==7 & input$B1S ==2){
    return(3)
  }
  else if (input$O1S ==8 & input$E1S ==2){
    return(3)
  }
  else if (input$O1S ==8 & input$B1S ==2){
    return(3)
  }
  else if (input$O1S ==9){
    return(3)
  }
  else {
    return(1)
  }
}

capitalt<-function(input){
  if (input$WSE ==2 & input$WNE ==3){
    return(1)
  }
  else if (input$WSE ==2 & input$WNE ==2){
    return(2)
  }
  else if (input$WSE ==2 & input$WNE ==1){
    return(3)
  }
  else if (input$WSE ==1 & input$WNE ==4){
    return(4)
  }
  else {
    return(1)
  }
}