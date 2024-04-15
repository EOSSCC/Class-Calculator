

# This section covers the functions for the SEG schema # 

RGSCF<-function(input){
  if(input$SEStatus>=1 & input$SEGSoc==1115){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==1116){
    return(20)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==1121){
    return(20)
  }
  else if(input$SEStatus>=6 & input$SEGSoc==1121){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=1122 & input$SEGSoc<=1162){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==1171){
    return(60)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=1172 & input$SEGSoc<=1259){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2111 & input$SEGSoc<=2129){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2133 & input$SEGSoc<=2139){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2141 & input$SEGSoc<=2142){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2150){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2221 & input$SEGSoc<=2216){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2217 & input$SEGSoc<=2232){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2311){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2312 & input$SEGSoc<=2316){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2317 & input$SEGSoc<=2318){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2319){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2412 & input$SEGSoc<=2423){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2424){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2425 & input$SEGSoc<=2426){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2429){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2431 & input$SEGSoc<=2432){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2433){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2434){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2435 & input$SEGSoc<=2443){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2444){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2449 & input$SEGSoc<=2452){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==2461){
    return(10)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=2462 & input$SEGSoc<=3121){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==3121){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3131 & input$SEGSoc<=3132){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==3213){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3216 & input$SEGSoc<=3239){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==3311){
    return(60)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3312 & input$SEGSoc<=3314){
    return(31)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==3315){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==3315){
    return(31)
  }
  else if(input$SEStatus==7 & input$SEGSoc==3315){
    return(40)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==3319){
    return(40)
  }
  else if(input$SEStatus>=4 & input$SEStatus<=5 & input$SEGSoc==3319){
    return(20)
  }
  else if(input$SEStatus==6 & input$SEGSoc==3319){
    return(31)
  }
  else if(input$SEStatus==7 & input$SEGSoc==3319){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3411 & input$SEGSoc<=3416){
    return(20)
  }
  else if(input$SEStatus==7 & input$SEGSoc==3417){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3421 & input$SEGSoc<=3422){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3441 & input$SEGSoc<=3442){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3443 & input$SEGSoc<=3541){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==3542){
    return(31)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==3543){
    return(20)
  }
  else if(input$SEStatus>=6 & input$SEGSoc==3543){
    return(31)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==3544){
    return(20)
  }
  else if(input$SEStatus>=6 & input$SEGSoc==3544){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3445 & input$SEGSoc<=3546){
    return(20)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==3550){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==3550){
    return(31)
  }
  else if(input$SEStatus==7 & input$SEGSoc==3550){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=3561 & input$SEGSoc<=3567){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=4112 & input$SEGSoc<=4113){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=4114 & input$SEGSoc<=4121){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=4122 & input$SEGSoc<=4135){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==4138){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=4151 & input$SEGSoc<=4159){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==4161){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=4162 & input$SEGSoc<=4213){
    return(31)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==4214){
    return(20)
  }
  else if(input$SEStatus>=4 & input$SEGSoc==4214){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=4215 & input$SEGSoc<=4217){
    return(31)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==5111){
    return(20)
  }
  else if(input$SEStatus>=4 & input$SEGSoc==5111){
    return(32)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==5112){
    return(20)
  }
  else if(input$SEStatus<=5 & input$SEStatus>=4 & input$SEGSoc==5112){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==5112){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==5112){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEStatus<=5 & input$SEGSoc>=5113 & input$SEGSoc<=5119){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=5113 & input$SEGSoc<=5119){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=5113 & input$SEGSoc<=5119){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=5211 & input$SEGSoc<=5312){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==5313){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=5314 & input$SEGSoc<=5315){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==5316){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=5319 & input$SEGSoc<=5435){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==5436){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=5441 & input$SEGSoc<=5442){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==5443){
    return(31)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==5449){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==5449){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==5449){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6121){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc>=6122 & input$SEGSoc<=6126){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=6122 & input$SEGSoc<=6126){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=6122 & input$SEGSoc<=6126){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6131){
    return(20)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==6132){
    return(32)
  }
  else if(input$SEStatus==4 & input$SEGSoc==6132){
    return(40)
  }
  else if(input$SEStatus==5 & input$SEGSoc==6132){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==6132){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==6132){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==6139){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==6139){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==6139){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6141){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6142){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=6143 & input$SEGSoc<=6144){
    return(20)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6145){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==6146){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==6146){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==6146){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6147){
    return(32)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==6148){
    return(32)
  }
  else if(input$SEStatus==4 & input$SEGSoc==6148){
    return(40)
  }
  else if(input$SEStatus==5 & input$SEGSoc==6148){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==6148){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==6148){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==6211){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==6211){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==6211){
    return(40)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==6212){
    return(20)
  }
  else if(input$SEStatus>=4 & input$SEGSoc==6212){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=6214 & input$SEGSoc<=6219){
    return(32)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==6221){
    return(20)
  }
  else if(input$SEStatus>=4 & input$SEGSoc==6221){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==6222){
    return(20)
  }
  else if(input$SEStatus>=6 & input$SEGSoc==6222){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6231){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==6232){
    return(40)
  }
  else if(input$SEStatus<=3 & input$SEGSoc==6240){
    return(32)
  }
  else if(input$SEStatus==4 & input$SEGSoc==6240){
    return(20)
  }
  else if(input$SEStatus==5 & input$SEGSoc==6240){
    return(20)
  }
  else if(input$SEStatus>=6 & input$SEGSoc==6240){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=7111 & input$SEGSoc<=7115){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==7121){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==7122){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==7123){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==7124){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=7125 & input$SEGSoc<=7211){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==7213){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=7214 & input$SEGSoc<=7219){
    return(31)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==7220){
    return(20)
  }
  else if(input$SEStatus==6 & input$SEGSoc==7220){
    return(31)
  }
  else if(input$SEStatus==7 & input$SEGSoc==7220){
    return(31)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8111){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8111){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8111){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==8112){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8113 & input$SEGSoc==8114){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8113 & input$SEGSoc==8114){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8113 & input$SEGSoc==8114){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==8115){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8116){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8116){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8116){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==8117 & input$SEGSoc==8118){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8119){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8119){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8119){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==8121 & input$SEGSoc==8122){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc>=8123 & input$SEGSoc<=8125){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=8123 & input$SEGSoc<=8125){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=8123 & input$SEGSoc<=8125){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8126){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8126){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8126){
    return(50)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==8126){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc>=8129 & input$SEGSoc<=8141){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=8129 & input$SEGSoc<=8141){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=8129 & input$SEGSoc<=8141){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8142){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8142){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8142){
    return(50)
  }
  else if(input$SEStatus<=5 & input$SEGSoc>=8143 & input$SEGSoc<=8149){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=8143 & input$SEGSoc<=8149){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=8143 & input$SEGSoc<=8149){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=8211 & input$SEGSoc<=8214){
    return(32)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==8215){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=8221 & input$SEGSoc<=8222){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8223){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8223){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8223){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=8229 & input$SEGSoc<=8231){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8232){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8232){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8232){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==8233){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc==8233){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==8233){
    return(50)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=8234 & input$SEGSoc<=8239){
    return(32)
  }
  else if(input$SEStatus<=5 & input$SEGSoc>=9111 & input$SEGSoc<=9119){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=9111 & input$SEGSoc<=9119){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=9111 & input$SEGSoc<=9119){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc>=9120 & input$SEGSoc<=9132){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=9120 & input$SEGSoc<=9132){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=9120 & input$SEGSoc<=9132){
    return(50)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9134){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9134){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9134){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9139){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9139){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9139){
    return(50)
  }
  else if(input$SEStatus<=2 & input$SEGSoc==9211){
    return(40)
  }
  else if(input$SEStatus==3 & input$SEGSoc==9211){
    return(50)
  }
  else if(input$SEStatus==4 & input$SEGSoc==9211){
    return(40)
  }
  else if(input$SEStatus==5 & input$SEGSoc==9211){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9211){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9211){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==9219){
    return(31)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=9231 & input$SEGSoc<=9233){
    return(50)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9234){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9234){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9234){
    return(40)
  }
  else if(input$SEStatus<=2 & input$SEGSoc==9235){
    return(40)
  }
  else if(input$SEStatus==3 & input$SEGSoc==9235){
    return(50)
  }
  else if(input$SEStatus==4 & input$SEGSoc==9235){
    return(50)
  }
  else if(input$SEStatus==5 & input$SEGSoc==9235){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9235){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9235){
    return(50)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9236){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9236){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9236){
    return(50)
  }
  else if(input$SEStatus>=1 & input$SEGSoc==9239){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9241){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9241){
    return(31)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9241){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9242){
    return(50)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9242){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9242){
    return(50)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9244){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9244){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9244){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc==9249){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9249){
    return(31)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9249){
    return(40)
  }
  else if(input$SEStatus>=1 & input$SEGSoc>=9251 & input$SEGSoc<=9259){
    return(40)
  }
  else if(input$SEStatus<=2 & input$SEGSoc==9260){
    return(40)
  }
  else if(input$SEStatus==3 & input$SEGSoc==9260){
    return(50)
  }
  else if(input$SEStatus==4 & input$SEGSoc==9260){
    return(40)
  }
  else if(input$SEStatus==5 & input$SEGSoc==9260){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc==9260){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc==9260){
    return(40)
  }
  else if(input$SEStatus<=5 & input$SEGSoc>=9271 & input$SEGSoc<=9279){
    return(40)
  }
  else if(input$SEStatus==6 & input$SEGSoc>=9271 & input$SEGSoc<=9279){
    return(32)
  }
  else if(input$SEStatus==7 & input$SEGSoc>=9271 & input$SEGSoc<=9279){
    return(40)
  }
}
