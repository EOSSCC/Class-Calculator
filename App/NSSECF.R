

# This section covers the functions for the NS-SEC schema # 
status<-function(input){
  if (input$Employed==2 & input$NoEmployees==2){
    return(1)
  }
  if (input$Employed==2 & input$NoEmployees==3){
    return(1)
  }
  else if (input$Employed==2 & input$NoEmployees==1){
    return(2)
  }
  else if (input$Employed==3 & input$NoEmployees==4){
    return(3)
  }
  else if (input$Employed==1 & input$IdSoc>=1115&&input$IdSoc<=1259 & input$NoEmployees==2){
    return(4)
  }
  else if (input$Employed==1 & input$IdSoc>=1115&&input$IdSoc<=1259 & input$NoEmployees==3){
    return(4)
  }
  else if (input$Employed==1 & input$IdSoc>=1115&&input$IdSoc<=1259 & input$NoEmployees==1){
    return(5)
  }
  else if(input$Employed==1 & input$IdSoc>=2111 & input$Supervisory==1){
    return(6)
  }
  else if(input$Employed==1 & input$IdSoc>=2111 & input$Supervisory==2){
    return(7)
  }
}

nssecfull<-function(input){
  #start of SOC2010 1000#
  if (input$IdSoc<=1116 & status(input)>=1){
    return(1.1)
  }
  
  else if (input$IdSoc==1121 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==1121 & status(input)==2){
    return(4)
  }
  else if (input$IdSoc==1121 & status(input)==3){
    return(4)
  }
  else if (input$IdSoc==1121 & status(input)==4){
    return(1.1)
  }
  else if (input$IdSoc==1121 & status(input)>=5){
    return(2)
  }
  
  else if (input$IdSoc==1122 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==1122 & status(input)==2){
    return(4)
  }
  else if (input$IdSoc==1122 & status(input)==3){
    return(4)
  }
  else if (input$IdSoc==1122 & status(input)>=4){
    return(2)
  }
  
  else if (input$IdSoc==1123 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==1123 & status(input)==2){
    return(4)
  }
  else if (input$IdSoc==1123 & status(input)==3){
    return(4)
  }
  else if (input$IdSoc==1123 & status(input)==4){
    return(1.1)
  }
  else if (input$IdSoc==1123 & status(input)>=5){
    return(2)
  }
  
  else if (input$IdSoc==1131 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==1131 & status(input)==2){
    return(4)
  }
  else if (input$IdSoc==1131 & status(input)==3){
    return(4)
  }
  else if (input$IdSoc==1131 & status(input)>=4){
    return(1.1)
  }
  
  else if (input$IdSoc>=1132&&input$IdSoc<=1139 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=1132&&input$IdSoc<=1139 & status(input)==2){
    return(4)
  }
  else if (input$IdSoc>=1132&&input$IdSoc<=1139 & status(input)==3){
    return(4)
  }
  else if (input$IdSoc>=1132&&input$IdSoc<=1139 & status(input)==4){
    return(1.1)
  }
  else if (input$IdSoc>=1132&&input$IdSoc<=1139 & status(input)>=5){
    return(2)
  }
  
  else if (input$IdSoc>=1150&&input$IdSoc<=1162 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=1150&&input$IdSoc<=1162 & status(input)==2){
    return(4)
  }
  else if (input$IdSoc>=1150&&input$IdSoc<=1162 & status(input)==3){
    return(4)
  }
  else if (input$IdSoc>=1150&&input$IdSoc<=1162 & status(input)>=4){
    return(2)
  }
  
  else if (input$IdSoc>=1171&&input$IdSoc<=1173 & status(input)>=1){
    return(1.1)
  }
  
  else if (input$IdSoc>=1181&&input$IdSoc<=1184 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=1181&&input$IdSoc<=1184 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=1181&&input$IdSoc<=1184 & status(input)>=4){
    return(1.1)
  }
  
  else if (input$IdSoc>=1190&&input$IdSoc<=1242 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=1190&&input$IdSoc<=1242 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=1190&&input$IdSoc<=1242 & status(input)>=4){
    return(2)
  }
  
  else if (input$IdSoc==1251 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==1251 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==1251 & status(input)==4){
    return(1.1)
  }
  else if (input$IdSoc==1251 & status(input)>=5){
    return(4)
  }
  
  else if (input$IdSoc>=1252&&input$IdSoc<=1259 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=1252&&input$IdSoc<=1259 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=1252&&input$IdSoc<=1259 & status(input)>=4){
    return(2)
  }
  #start of SOC2010 2000#
  else if (input$IdSoc>=2111&&input$IdSoc<=2126 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==2127 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==2127 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=2129&&input$IdSoc<=2136 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc>=2137&&input$IdSoc<=2141 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=2137&&input$IdSoc<=2141 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=2142&&input$IdSoc<=2213 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==2214 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==2214 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=2215&&input$IdSoc<=2216 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc>=2217&&input$IdSoc<=2222 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=2217&&input$IdSoc<=2222 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==2223 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc>=2229&&input$IdSoc<=2232 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=2229&&input$IdSoc<=2232 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==2311 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc>=2312&&input$IdSoc<=2316 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=2312&&input$IdSoc<=2316 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=2317&&input$IdSoc<=2318 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==2319 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==2319 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==2319 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==2319 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==2319 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=2412&&input$IdSoc<=2426 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==2429 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==2429 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=2431&&input$IdSoc<=2432 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==2433 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==2433 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==2434 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc>=2435&&input$IdSoc<=2436 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=2435&&input$IdSoc<=2436 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==2442 & status(input)>=1){
    return(2)
  }
  
  else if (input$IdSoc>=2443&&input$IdSoc<=2444 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==2449 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==2449 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=2451&&input$IdSoc<=2452 & status(input)>=1){
    return(2)
  }
  
  else if (input$IdSoc>=2461&&input$IdSoc<=2462 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=2449&&input$IdSoc<=2462 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==2463 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc>=2471&&input$IdSoc<=3111 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=2471&&input$IdSoc<=3111 & status(input)>=2){
    return(2)
  }
  #start of SOC2010 3000#
  else if (input$IdSoc==3112 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3112 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==3112 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==3112 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==3112 & status(input)==7){
    return(4)
  }
  
  else if (input$IdSoc>=3113&&input$IdSoc<=3114 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3113&&input$IdSoc<=3114 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3115 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3115 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==3115 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=3116&&input$IdSoc<=3121 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3116&&input$IdSoc<=3121 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3122 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3122 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==3122 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==3122 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==3122 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=3131&&input$IdSoc<=3132 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3131&&input$IdSoc<=3132 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3213 & status(input)>=1&&status(input)<=2){
    return(2)
  }
  else if (input$IdSoc==3213 & status(input)>=3&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==3213 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==3213 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=3216&&input$IdSoc<=3218 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3216&&input$IdSoc<=3218 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=3216&&input$IdSoc<=3218 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc>=3216&&input$IdSoc<=3218 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc>=3216&&input$IdSoc<=3218 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=3219&&input$IdSoc<=3239 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3219&&input$IdSoc<=3239 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=3311&&input$IdSoc<=3314 & status(input)>=1&&status(input)<=2){
    return(2)
  }
  else if (input$IdSoc>=3311&&input$IdSoc<=3314 & status(input)>=3&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc>=3311&&input$IdSoc<=3314 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc>=3311&&input$IdSoc<=3314 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc==3315 & status(input)>=1&&status(input)<=2){
    return(5)
  }
  else if (input$IdSoc==3315 & status(input)>=3&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==3315 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==3315 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=3319&&input$IdSoc<=3416 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3319&&input$IdSoc<=3416 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=3417&&input$IdSoc<=3442 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3417&&input$IdSoc<=3442 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=3417&&input$IdSoc<=3442 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc>=3417&&input$IdSoc<=3442 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc>=3417&&input$IdSoc<=3442 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=3441&&input$IdSoc<=3442 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3319&&input$IdSoc<=3416 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3443 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3443 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==3443 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==3443 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==3443 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==3511 & status(input)>=1){
    return(2)
  }
  
  else if (input$IdSoc==3512 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==3513 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3513 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3520 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3520 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==3520 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==3520 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==3520 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc==3531 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3531 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=3532&&input$IdSoc<=3533 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==3534 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3534 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3535 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc>=3536&&input$IdSoc<=3544 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3536&&input$IdSoc<=3544 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3545 & status(input)>=1){
    return(1.2)
  }
  
  else if (input$IdSoc==3546 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3546 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc==3550 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==3550 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==3550 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==3550 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==3550 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc==3561 & status(input)>=1){
    return(2)
  }
  
  else if (input$IdSoc>=3562&&input$IdSoc<=3567 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=3562&&input$IdSoc<=3567 & status(input)>=2){
    return(2)
  }
  #start of SOC2010 4000#
  else if (input$IdSoc>=4112&&input$IdSoc<=4113 & status(input)>=1&&status(input)<=2){
    return(2)
  }
  else if (input$IdSoc>=4112&&input$IdSoc<=4113 & status(input)>=3&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc>=4112&&input$IdSoc<=4113 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc>=4112&&input$IdSoc<=4113 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc==4114 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4114 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=4121&&input$IdSoc<=4123 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=4121&&input$IdSoc<=4123 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=4121&&input$IdSoc<=4123 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==4124 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4124 & status(input)>=2){
    return(2)
  }
  
  else if (input$IdSoc>=4129&&input$IdSoc<=4132 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=4129&&input$IdSoc<=4132 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=4129&&input$IdSoc<=4132 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==4133 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4133 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==4133 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc==4134 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4134 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==4134 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==4135 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4135 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==4135 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==4135 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==4135 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=4138&&input$IdSoc<=4159 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=4138&&input$IdSoc<=4159 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=4138&&input$IdSoc<=4159 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==4161 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4161 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==4161 & status(input)>=4){
    return(2)
  }
  
  else if (input$IdSoc==4162 & status(input)>=1){
    return(2)
  }
  
  else if (input$IdSoc>=4211&&input$IdSoc<=4215 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=4211&&input$IdSoc<=4215 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=4211&&input$IdSoc<=4215 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc>=4211&&input$IdSoc<=4215 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc>=4211&&input$IdSoc<=4215 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc==4216 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4216 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==4216 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==4216 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==4216 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==4217 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==4217 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==4217 & status(input)>=4){
    return(3)
  }
  #start of SOC2010 5000#
  else if (input$IdSoc==5111 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5111 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5111 & status(input)>=4){
    return(2)
  }
  
  else if (input$IdSoc>=5112&&input$IdSoc<=5113 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5112&&input$IdSoc<=5113 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5112&&input$IdSoc<=5113 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=5112&&input$IdSoc<=5113 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=5112&&input$IdSoc<=5113 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=5114&&input$IdSoc<=5119 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5114&&input$IdSoc<=5119 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5114&&input$IdSoc<=5119 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==5211 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5211 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5211 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc>=5212&&input$IdSoc<=5213 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5212&&input$IdSoc<=5213 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5212&&input$IdSoc<=5213 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc>=5214&&input$IdSoc<=5216 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5214&&input$IdSoc<=5216 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5214&&input$IdSoc<=5216 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc==5221 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5221 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5221 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc>=5222&&input$IdSoc<=5224 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5222&&input$IdSoc<=5224 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5222&&input$IdSoc<=5224 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==5225 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5225 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5225 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc>=5231&&input$IdSoc<=5232 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5231&&input$IdSoc<=5232 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5231&&input$IdSoc<=5232 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==5234 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5234 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5234 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc==5235 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5235 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5235 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==5236 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5236 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5236 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc>=5237&&input$IdSoc<=5241 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5237&&input$IdSoc<=5241 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5237&&input$IdSoc<=5241 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==5242 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5242 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5242 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==5244 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5244 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5244 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=5245&&input$IdSoc<=5249 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5245&&input$IdSoc<=5249 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5245&&input$IdSoc<=5249 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==5250 & status(input)>=1){
    return(2)
  }
  
  else if (input$IdSoc==5311 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5311 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5311 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc>=5312&&input$IdSoc<=5313 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5312&&input$IdSoc<=5313 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5312&&input$IdSoc<=5313 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc==5314 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5314 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5314 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=5315&&input$IdSoc<=5316 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5315&&input$IdSoc<=5316 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5315&&input$IdSoc<=5316 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc==5319 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5319 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5319 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc>=5321&&input$IdSoc<=5323 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5321&&input$IdSoc<=5323 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5321&&input$IdSoc<=5323 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc==5330 & status(input)>=1&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5330 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=5411&&input$IdSoc<=5413 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5411&&input$IdSoc<=5413 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5411&&input$IdSoc<=5413 & status(input)>=4&&input$IdSoc<=5){
    return(7)
  }
  else if (input$IdSoc>=5411&&input$IdSoc<=5413 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=5411&&input$IdSoc<=5413 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==5414 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5414 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5414 & status(input)>=4&&input$IdSoc<=5){
    return(6)
  }
  else if (input$IdSoc==5414 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==5414 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==5419 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5419 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5419 & status(input)>=4&&input$IdSoc<=5){
    return(7)
  }
  else if (input$IdSoc==5419 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==5419 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc>=5421&&input$IdSoc<=5422 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5421&&input$IdSoc<=5422 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5421&&input$IdSoc<=5422 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=5423&&input$IdSoc<=5431 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5423&&input$IdSoc<=5431 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5423&&input$IdSoc<=5431 & status(input)>=4&&input$IdSoc<=5){
    return(7)
  }
  else if (input$IdSoc>=5423&&input$IdSoc<=5431 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=5423&&input$IdSoc<=5431 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==5432 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5432 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5432 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==5433 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5433 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5433 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==5433 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==5433 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==5434 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5434 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5434 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==5435 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5435 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5435 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==5435 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==5435 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==5436 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5436 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5436 & status(input)>=4){
    return(2)
  }
  
  else if (input$IdSoc>=5441&&input$IdSoc<=5443 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=5441&&input$IdSoc<=5443 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=5441&&input$IdSoc<=5443 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc>=5441&&input$IdSoc<=5443 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=5441&&input$IdSoc<=5443 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==5449 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==5449 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==5449 & status(input)>=4){
    return(5)
  }
  #start of SOC2010 6000#
  else if (input$IdSoc==6121 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6121 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6121 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==6121 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==6121 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc==6122 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6122 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6122 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==6122 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6122 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==6123 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6123 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6123 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==6123 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6123 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==6125 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6125 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6125 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==6125 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==6125 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=6126&&input$IdSoc<=6132 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=6126&&input$IdSoc<=6132 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=6126&&input$IdSoc<=6132 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=6126&&input$IdSoc<=6132 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=6126&&input$IdSoc<=6132 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==6139 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6139 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6139 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==6139 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6139 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==6141 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6141 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6141 & status(input)>=4&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==6141 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==6141 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc==6141 & status(input)>=1&&status(input)<=2){
    return(2)
  }
  else if (input$IdSoc==6141 & status(input)>=3&&status(input)<=5){
    return(3)
  }
  else if (input$IdSoc==6141 & status(input)==6){
    return(2)
  }
  else if (input$IdSoc==6141 & status(input)==7){
    return(3)
  }
  
  else if (input$IdSoc>=6143&&input$IdSoc<=6144 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=6143&&input$IdSoc<=6144 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=6143&&input$IdSoc<=6144 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=6143&&input$IdSoc<=6144 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=6143&&input$IdSoc<=6144 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==6145 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6145 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6145 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc==6146 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6146 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6146 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==6146 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6146 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==6147 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6147 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6147 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==6147 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6147 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc>=6148&&input$IdSoc<=6211 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=6148&&input$IdSoc<=6211 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=6148&&input$IdSoc<=6211 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=6148&&input$IdSoc<=6211 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=6148&&input$IdSoc<=6211 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=6212&&input$IdSoc<=6215 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=6212&&input$IdSoc<=6215 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=6212&&input$IdSoc<=6215 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=6212&&input$IdSoc<=6215 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=6212&&input$IdSoc<=6215 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==6219 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6219 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6219 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==6219 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6219 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==6221 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6221 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6221 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==6221 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6221 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==6222 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==6222 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6222 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==6222 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==6222 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=6231&&input$IdSoc<=6232 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=6231&&input$IdSoc<=6232 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=6231&&input$IdSoc<=6232 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc==6240 & status(input)>=1&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==6240 & status(input)>=4&&status(input)<=5){
    return(2)
  }
  else if (input$IdSoc==6240 & status(input)>=6&&status(input)<=7){
    return(5)
  }
  #start of SOC2010 7000#
  else if (input$IdSoc>=7111&&input$IdSoc<=7114 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=7111&&input$IdSoc<=7114 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=7111&&input$IdSoc<=7114 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=7111&&input$IdSoc<=7114 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=7111&&input$IdSoc<=7114 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==7115 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7115 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7115 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==7121 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7121 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7121 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc==7122 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7122 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7122 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc>=7123&&input$IdSoc<=7124 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=7123&&input$IdSoc<=7124 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=7123&&input$IdSoc<=7124 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc>=7125&&input$IdSoc<=7129 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=7125&&input$IdSoc<=7129 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=7125&&input$IdSoc<=7129 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==7130 & status(input)>=1){
    return(2)
  }
  
  else if (input$IdSoc==7211 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7211 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7211 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==7213 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7213 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7213 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc==7214 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7214 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7214 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==7215 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7215 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7215 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==7215 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==7215 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==7219 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7219 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7219 & status(input)>=4){
    return(3)
  }
  
  else if (input$IdSoc==7220 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==7220 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==7220 & status(input)>=4){
    return(2)
  }
  #start of SOC2010 8000#
  else if (input$IdSoc>=8111&&input$IdSoc<=8112 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8111&&input$IdSoc<=8112 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8111&&input$IdSoc<=8112 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=8111&&input$IdSoc<=8112 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8111&&input$IdSoc<=8112 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==8113 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8113 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8113 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==8113 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==8113 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==8114 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8114 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8114 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc>=8116&&input$IdSoc<=8121 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8116&&input$IdSoc<=8121 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8116&&input$IdSoc<=8121 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=8116&&input$IdSoc<=8121 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8116&&input$IdSoc<=8121 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==8122 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8122 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8122 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==8122 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==8122 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==8123 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8123 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8123 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=8124&&input$IdSoc<=8125 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8124&&input$IdSoc<=8125 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8124&&input$IdSoc<=8125 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=8124&&input$IdSoc<=8125 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8124&&input$IdSoc<=8125 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==8126 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8126 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8126 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=8127&&input$IdSoc<=8132 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8127&&input$IdSoc<=8132 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8127&&input$IdSoc<=8132 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=8127&&input$IdSoc<=8132 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8127&&input$IdSoc<=8132 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==8133 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8133 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8133 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc==8134 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8134 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8134 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==8134 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==8134 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==8135 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8135 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8135 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==8135 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==8135 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=8137&&input$IdSoc<=8139 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8137&&input$IdSoc<=8139 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8137&&input$IdSoc<=8139 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc>=8137&&input$IdSoc<=8139 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8137&&input$IdSoc<=8139 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc>=8141&&input$IdSoc<=8142 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8141&&input$IdSoc<=8142 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8141&&input$IdSoc<=8142 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=8141&&input$IdSoc<=8142 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8141&&input$IdSoc<=8142 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==8143 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8143 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8143 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=8149&&input$IdSoc<=8214 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8149&&input$IdSoc<=8214 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8149&&input$IdSoc<=8214 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc>=8149&&input$IdSoc<=8214 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8149&&input$IdSoc<=8214 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc>=8215&&input$IdSoc<=8223 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8215&&input$IdSoc<=8223 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8215&&input$IdSoc<=8223 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=8215&&input$IdSoc<=8223 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=8215&&input$IdSoc<=8223 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc==8229 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8229 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8229 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==8229 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==8229 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==8231 & status(input)>=1){
    return(5)
  }
  
  else if (input$IdSoc==8232 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==8232 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==8232 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==8232 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==8232 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=8233&&input$IdSoc<=8339 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=8233&&input$IdSoc<=8339 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=8233&&input$IdSoc<=8339 & status(input)>=4){
    return(5)
  }
  
  else if (input$IdSoc>=9111&&input$IdSoc<=9112 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9111&&input$IdSoc<=9112 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9111&&input$IdSoc<=9112 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=9111&&input$IdSoc<=9112 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=9111&&input$IdSoc<=9112 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=9119&&input$IdSoc<=9139 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9119&&input$IdSoc<=9139 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9119&&input$IdSoc<=9139 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc>=9119&&input$IdSoc<=9139 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=9119&&input$IdSoc<=9139 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc>=9211&&input$IdSoc<=9219 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9211&&input$IdSoc<=9219 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9211&&input$IdSoc<=9219 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=9211&&input$IdSoc<=9219 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=9211&&input$IdSoc<=9219 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=9231&&input$IdSoc<=9233 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9231&&input$IdSoc<=9233 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9231&&input$IdSoc<=9233 & status(input)>=4&&status(input)<=7){
    return(7)
  }
  
  else if (input$IdSoc>=9234&&input$IdSoc<=9236 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9234&&input$IdSoc<=9236 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9234&&input$IdSoc<=9236 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc>=9234&&input$IdSoc<=9236 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=9234&&input$IdSoc<=9236 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==9239 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==9239 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==9239 & status(input)>=4){
    return(7)
  }
  
  else if (input$IdSoc==9241 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==9241 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==9241 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==9241 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==9241 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=9242&&input$IdSoc<=9244 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9242&&input$IdSoc<=9244 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9242&&input$IdSoc<=9244 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc>=9242&&input$IdSoc<=9244 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=9242&&input$IdSoc<=9244 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc==9249 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==9249 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==9249 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc==9249 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==9249 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=9251&&input$IdSoc<=9259 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9251&&input$IdSoc<=9259 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9251&&input$IdSoc<=9259 & status(input)>=4){
    return(6)
  }
  
  else if (input$IdSoc==9260 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc==9260 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc==9260 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc==9260 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc==9260 & status(input)==7){
    return(7)
  }
  
  else if (input$IdSoc>=9271&&input$IdSoc<=9272 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9271&&input$IdSoc<=9272 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9271&&input$IdSoc<=9272 & status(input)>=4&&status(input)<=5){
    return(6)
  }
  else if (input$IdSoc>=9271&&input$IdSoc<=9272 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=9271&&input$IdSoc<=9272 & status(input)==7){
    return(6)
  }
  
  else if (input$IdSoc>=9273&&input$IdSoc<=9279 & status(input)==1){
    return(1.1)
  }
  else if (input$IdSoc>=9273&&input$IdSoc<=9279 & status(input)>=2&&status(input)<=3){
    return(4)
  }
  else if (input$IdSoc>=9273&&input$IdSoc<=9279 & status(input)>=4&&status(input)<=5){
    return(7)
  }
  else if (input$IdSoc>=9273&&input$IdSoc<=9279 & status(input)==6){
    return(5)
  }
  else if (input$IdSoc>=9273&&input$IdSoc<=9279 & status(input)==7){
    return(7)
  }
}

