

# This section covers the functions for the GBCS schema # 

GBCSF <- function(input){
social.n <- length(input$social)
high.n <- length(input$high)
emerging.n <- length(input$emerging)
if (input$income ==1) {
  return(1)
}
else if (input$income ==0.6 & social.n>=6 & mean(as.numeric(input$social >=58))) {
  return(1)
}
else if (input$income ==0.375 & input$home >=0 & input$savings ==1) {
  return(1)
}
else if (input$income ==0.6 & social.n<6) {
  return(3)
}
else if (input$income ==0.375 & input$home >0 & input$savings >0.05 & input$savings <1 & social.n <6) {
  return(3)
}
else if (input$income ==0.6 & social.n>=6 & mean(as.numeric(input$social <58))) {
  return(2) 
}
else if (input$income ==0.375 & input$home >0 & input$savings <=0.05 & high.n>=3) {
  return(2) 
}
else if (input$income ==0.375 & input$home >0 & input$savings >=0.05 & input$savings <1 & social.n>=6) {
  return(2) 
}
else if (input$income ==0.375 & input$home ==0) {
  return(6)
}
else if (input$income ==0.175 & input$home ==0 & emerging.n>=4) {
  return(6)
}
else if (input$income ==0.08 & input$home ==0 & emerging.n>4) {
  return(6) 
}
else if (input$income ==0.08 & input$home ==0 & social.n>=6 & emerging.n==4) {
  return(6) 
}
else if (input$income ==0.375 & input$home >0 & input$savings <=0.05 & high.n<3) {
  return(4)
}
else if (input$income ==0.175 & input$home >0 & emerging.n>=3) {
  return(4) 
}
else if (input$income ==0.175 & input$home ==0 & emerging.n<4) {
  return(7)
}
else if (input$income ==0.08 & input$home ==0 & emerging.n<4) {
  return(7)
}
else if (input$income ==0.08 & input$home ==0 & social.n<6 & emerging.n==4) {
  return(7)
}
else if (input$income ==0.175 & input$home >0 & emerging.n<3) {
  return(5)
}
else if (input$income ==0.08 & input$home >0) {
  return(5)
}
else {
  return(8)
}
}

