

# This section covers the functions for the CAMSIS schema # 

CAMSISF<-function(input){
  if(input$CAMSISSoc==1115 & input$EStatus>=1 & input$Sex==1){
    return(73.50)
  }
  else if(input$CAMSISSoc==1115 & input$EStatus>=1 & input$Sex==2){
    return(75.52)
  }
  else if(input$CAMSISSoc==1116 & input$EStatus>=1 & input$Sex==1){
    return(73.50)
  }
  else if(input$CAMSISSoc==1116 & input$EStatus>=1 & input$Sex==2){
    return(75.52)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==1 & input$Sex==1){
    return(59.69)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==1 & input$Sex==2){
    return(58.83)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==2 & input$Sex==1){
    return(69.33)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==2 & input$Sex==2){
    return(58.83)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==3 & input$Sex==1){
    return(56.37)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==3 & input$Sex==2){
    return(58.83)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==4 & input$Sex==1){
    return(56.37)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==4 & input$Sex==2){
    return(58.83)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==5 & input$Sex==1){
    return(56.37)
  }
  else if(input$CAMSISSoc==1121 & input$EStatus==5 & input$Sex==2){
    return(58.83)
  }
  else if(input$CAMSISSoc==1122 & input$EStatus==1 & input$Sex==1){
    return(60.96)
  }
  else if(input$CAMSISSoc==1122 & input$EStatus>=1 & input$Sex==2){
    return(57.83)
  }
  else if(input$CAMSISSoc==1122 & input$EStatus==2 & input$Sex==1){
    return(58.77)
  }
  else if(input$CAMSISSoc==1122 & input$EStatus>=3 & input$Sex==1){
    return(57.74)
  }
  else if(input$CAMSISSoc==1123 & input$EStatus==1 & input$Sex==1){
    return(60.96)
  }
  else if(input$CAMSISSoc==1123 & input$EStatus>=1 & input$Sex==2){
    return(57.83)
  }
  else if(input$CAMSISSoc==1123 & input$EStatus==2 & input$Sex==1){
    return(58.77)
  }
  else if(input$CAMSISSoc==1123 & input$EStatus==3 & input$Sex==1){
    return(48.92)
  }
  else if(input$CAMSISSoc==1123 & input$EStatus==4 & input$Sex==1){
    return(48.92)
  }
  else if(input$CAMSISSoc==1123 & input$EStatus==5 & input$Sex==1){
    return(49.65)
  }
  else if(input$CAMSISSoc==1131 & input$EStatus==1 & input$Sex==1){
    return(68.12)
  }
  else if(input$CAMSISSoc==1131 & input$EStatus>=1 & input$Sex==2){
    return(68.30)
  }
  else if(input$CAMSISSoc==1131 & input$EStatus==2 & input$Sex==1){
    return(62.38)
  }
  else if(input$CAMSISSoc==1131 & input$EStatus>=3 & input$Sex==1){
    return(66.78)
  }
  else if(input$CAMSISSoc==1132 & input$EStatus==1 & input$Sex==1){
    return(58.64)
  }
  else if(input$CAMSISSoc==1132 & input$EStatus>=1 & input$Sex==2){
    return(73.79)
  }
  else if(input$CAMSISSoc==1132 & input$EStatus>=2 & input$Sex==1){
    return(67.25)
  }
  else if(input$CAMSISSoc==1133 & input$EStatus==1 & input$Sex==1){
    return(68.12)
  }
  else if(input$CAMSISSoc==1133 & input$EStatus>=1 & input$Sex==2){
    return(68.16)
  }
  else if(input$CAMSISSoc==1133 & input$EStatus==2 & input$Sex==1){
    return(62.38)
  }
  else if(input$CAMSISSoc==1133 & input$EStatus>=3 & input$Sex==1){
    return(56.33)
  }
  else if(input$CAMSISSoc==1133 & input$EStatus==3 & input$Sex==1){
    return(56.33)
  }
  else if(input$CAMSISSoc==1133 & input$EStatus==4 & input$Sex==1){
    return(56.33)
  }
  else if(input$CAMSISSoc==1133 & input$EStatus==5 & input$Sex==1){
    return(56.33)
  }
  else if(input$CAMSISSoc==1134 & input$EStatus==1 & input$Sex==1){
    return(58.64)
  }
  else if(input$CAMSISSoc==1134 & input$EStatus>=1 & input$Sex==2){
    return(90.28)
  }
  else if(input$CAMSISSoc==1134 & input$EStatus==2 & input$Sex==1){
    return(67.25)
  }
  else if(input$CAMSISSoc==1134 & input$EStatus>=3 & input$Sex==1){
    return(67.84)
  }
  else if(input$CAMSISSoc==1135 & input$EStatus==1 & input$Sex==1){
    return(60.86)
  }
  else if(input$CAMSISSoc==1135 & input$EStatus>=1 & input$Sex==2){
    return(72.27)
  }
  else if(input$CAMSISSoc==1135 & input$EStatus==2 & input$Sex==1){
    return(60.82)
  }
  else if(input$CAMSISSoc==1135 & input$EStatus>=3 & input$Sex==1){
    return(61.68)
  }
  else if(input$CAMSISSoc==1136 & input$EStatus==1 & input$Sex==1){
    return(60.86)
  }
  else if(input$CAMSISSoc==1136 & input$EStatus>=1 & input$Sex==2){
    return(72.34)
  }
  else if(input$CAMSISSoc==1136 & input$EStatus==2 & input$Sex==1){
    return(60.82)
  }
  else if(input$CAMSISSoc==1136 & input$EStatus>=3 & input$Sex==1){
    return(67.09)
  }
  else if(input$CAMSISSoc==1139 & input$EStatus==1 & input$Sex==1){
    return(60.86)
  }
  else if(input$CAMSISSoc==1139 & input$EStatus>=1 & input$Sex==2){
    return(72.34)
  }
  else if(input$CAMSISSoc==1139 & input$EStatus==2 & input$Sex==1){
    return(60.82)
  }
  else if(input$CAMSISSoc==1139 & input$EStatus==3 & input$Sex==1){
    return(69.65)
  }
  else if(input$CAMSISSoc==1139 & input$EStatus==4 & input$Sex==1){
    return(68.23)
  }
  else if(input$CAMSISSoc==1139 & input$EStatus==5 & input$Sex==1){
    return(69.65)
  }
  else if(input$CAMSISSoc==1150 & input$EStatus==1 & input$Sex==1){
    return(68.12)
  }
  else if(input$CAMSISSoc==1150 & input$EStatus>=1 & input$Sex==2){
    return(58.78)
  }
  else if(input$CAMSISSoc==1150 & input$EStatus==2 & input$Sex==1){
    return(62.38)
  }
  else if(input$CAMSISSoc==1150 & input$EStatus>=3 & input$Sex==1){
    return(65.24)
  }
  else if(input$CAMSISSoc==1161 & input$EStatus==1 & input$Sex==1){
    return(54.07)
  }
  else if(input$CAMSISSoc==1161 & input$EStatus>=1 & input$Sex==2){
    return(35.54)
  }
  else if(input$CAMSISSoc==1161 & input$EStatus==2 & input$Sex==1){
    return(54.07)
  }
  else if(input$CAMSISSoc==1161 & input$EStatus>=3 & input$Sex==1){
    return(51.27)
  }
  else if(input$CAMSISSoc==1162 & input$EStatus==1 & input$Sex==1){
    return(54.07)
  }
  else if(input$CAMSISSoc==1162 & input$EStatus>=1 & input$Sex==2){
    return(35.54)
  }
  else if(input$CAMSISSoc==1162 & input$EStatus==2 & input$Sex==1){
    return(54.07)
  }
  else if(input$CAMSISSoc==1162 & input$EStatus>=3 & input$Sex==1){
    return(53.12)
  }
  else if(input$CAMSISSoc==1171 & input$EStatus>=1 & input$Sex==1){
    return(72.70)
  }
  else if(input$CAMSISSoc==1171 & input$EStatus>=1 & input$Sex==2){
    return(57.21)
  }
  else if(input$CAMSISSoc==1172 & input$EStatus>=1 & input$Sex==1){
    return(55.22)
  }
  else if(input$CAMSISSoc==1172 & input$EStatus>=1 & input$Sex==2){
    return(57.21)
  }
  else if(input$CAMSISSoc==1173 & input$EStatus>=1 & input$Sex==1){
    return(60.01)
  }
  else if(input$CAMSISSoc==1173 & input$EStatus>=1 & input$Sex==2){
    return(57.21)
  }
  else if(input$CAMSISSoc==1181 & input$EStatus>=1 & input$Sex==1){
    return(63.89)
  }
  else if(input$CAMSISSoc==1181 & input$EStatus>=1 & input$Sex==2){
    return(63.07)
  }
  else if(input$CAMSISSoc==1184 & input$EStatus>=1 & input$Sex==1){
    return(56.61)
  }
  else if(input$CAMSISSoc==1184 & input$EStatus>=1 & input$Sex==2){
    return(61.45)
  }
  else if(input$CAMSISSoc==1190 & input$EStatus==1 & input$Sex==1){
    return(62.34)
  }
  else if(input$CAMSISSoc==1190 & input$EStatus==1 & input$Sex==2){
    return(52.13)
  }
  else if(input$CAMSISSoc==1190 & input$EStatus==2 & input$Sex==1){
    return(64.47)
  }
  else if(input$CAMSISSoc==1190 & input$EStatus==2 & input$Sex==2){
    return(68.51)
  }
  else if(input$CAMSISSoc==1190 & input$EStatus>=3 & input$Sex==1){
    return(55.51)
  }
  else if(input$CAMSISSoc==1190 & input$EStatus>=3 & input$Sex==2){
    return(45.40)
  }
  else if(input$CAMSISSoc==1211 & input$EStatus>=1 & input$Sex==1){
    return(49.60)
  }
  else if(input$CAMSISSoc==1211 & input$EStatus>=1 & input$Sex==2){
    return(46.63)
  }
  else if(input$CAMSISSoc==1213 & input$EStatus>=1 & input$Sex==1){
    return(49.60)
  }
  else if(input$CAMSISSoc==1213 & input$EStatus>=1 & input$Sex==2){
    return(46.63)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus==1 & input$Sex==1){
    return(43.32)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus==1 & input$Sex==2){
    return(41.85)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus==2 & input$Sex==1){
    return(53.53)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus==2 & input$Sex==2){
    return(57.55)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus==3 & input$Sex==1){
    return(49.37)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus>=3 & input$Sex==2){
    return(41.36)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus==4 & input$Sex==1){
    return(49.59)
  }
  else if(input$CAMSISSoc==1221 & input$EStatus==5 & input$Sex==1){
    return(49.37)
  }
  else if(input$CAMSISSoc==1223 & input$EStatus==1 & input$Sex==1){
    return(43.32)
  }
  else if(input$CAMSISSoc==1223 & input$EStatus==1 & input$Sex==2){
    return(41.85)
  }
  else if(input$CAMSISSoc==1223 & input$EStatus==2 & input$Sex==1){
    return(53.53)
  }
  else if(input$CAMSISSoc==1223 & input$EStatus==2 & input$Sex==2){
    return(57.55)
  }
  else if(input$CAMSISSoc==1223 & input$EStatus>=3 & input$Sex==1){
    return(41.55)
  }
  else if(input$CAMSISSoc==1223 & input$EStatus>=3 & input$Sex==2){
    return(36.09)
  }
  else if(input$CAMSISSoc==1224 & input$EStatus==1 & input$Sex==1){
    return(43.32)
  }
  else if(input$CAMSISSoc==1224 & input$EStatus==1 & input$Sex==2){
    return(41.85)
  }
  else if(input$CAMSISSoc==1224 & input$EStatus==2 & input$Sex==1){
    return(53.53)
  }
  else if(input$CAMSISSoc==1224 & input$EStatus==2 & input$Sex==2){
    return(57.55)
  }
  else if(input$CAMSISSoc==1224 & input$EStatus>=3 & input$Sex==1){
    return(31.49)
  }
  else if(input$CAMSISSoc==1224 & input$EStatus>=3 & input$Sex==2){
    return(30.42)
  }
  else if(input$CAMSISSoc==1225 & input$EStatus==1 & input$Sex==1){
    return(43.32)
  }
  else if(input$CAMSISSoc==1225 & input$EStatus==1 & input$Sex==2){
    return(41.85)
  }
  else if(input$CAMSISSoc==1225 & input$EStatus==2 & input$Sex==1){
    return(53.53)
  }
  else if(input$CAMSISSoc==1225 & input$EStatus==2 & input$Sex==2){
    return(57.55)
  }
  else if(input$CAMSISSoc==1225 & input$EStatus>=3 & input$Sex==1){
    return(50.03)
  }
  else if(input$CAMSISSoc==1225 & input$EStatus>=3 & input$Sex==2){
    return(47.67)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus==1 & input$Sex==1){
    return(43.32)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus==1 & input$Sex==2){
    return(41.85)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus==2 & input$Sex==1){
    return(53.53)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus==2 & input$Sex==2){
    return(57.55)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus==3 & input$Sex==1){
    return(50.03)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus>=3 & input$Sex==2){
    return(47.67)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus==4 & input$Sex==1){
    return(50.03)
  }
  else if(input$CAMSISSoc==1226 & input$EStatus==4 & input$Sex==1){
    return(47.44)
  }
  else if(input$CAMSISSoc==1241 & input$EStatus>=1 & input$Sex==1){
    return(50.50)
  }
  else if(input$CAMSISSoc==1241 & input$EStatus>=1 & input$Sex==2){
    return(47.17)
  }
  else if(input$CAMSISSoc==1242 & input$EStatus>=1 & input$Sex==1){
    return(50.50)
  }
  else if(input$CAMSISSoc==1242 & input$EStatus>=1 & input$Sex==2){
    return(47.17)
  }
  else if(input$CAMSISSoc==1251 & input$EStatus==1 & input$Sex==1){
    return(72.06)
  }
  else if(input$CAMSISSoc==1251 & input$EStatus==1 & input$Sex==2){
    return(68.67)
  }
  else if(input$CAMSISSoc==1251 & input$EStatus==2 & input$Sex==1){
    return(66.51)
  }
  else if(input$CAMSISSoc==1251 & input$EStatus==2 & input$Sex==2){
    return(68.67)
  }
  else if(input$CAMSISSoc==1251 & input$EStatus>=3 & input$Sex==1){
    return(55.13)
  }
  else if(input$CAMSISSoc==1251 & input$EStatus>=3 & input$Sex==2){
    return(62.11)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==1 & input$Sex==1){
    return(62.96)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==1 & input$Sex==2){
    return(68.67)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==2 & input$Sex==1){
    return(62.96)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==2 & input$Sex==2){
    return(68.67)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==3 & input$Sex==1){
    return(56.18)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==3 & input$Sex==2){
    return(62.11)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==4 & input$Sex==1){
    return(56.18)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==4 & input$Sex==2){
    return(68.07)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==5 & input$Sex==1){
    return(59.04)
  }
  else if(input$CAMSISSoc==1252 & input$EStatus==5 & input$Sex==2){
    return(68.07)
  }
  else if(input$CAMSISSoc==1253 & input$EStatus==1 & input$Sex==1){
    return(56.08)
  }
  else if(input$CAMSISSoc==1253 & input$EStatus>=1 & input$Sex==2){
    return(53.10)
  }
  else if(input$CAMSISSoc==1253 & input$EStatus==2 & input$Sex==1){
    return(66.01)
  }
  else if(input$CAMSISSoc==1253 & input$EStatus==3 & input$Sex==1){
    return(53.59)
  }
  else if(input$CAMSISSoc==1253 & input$EStatus==4 & input$Sex==1){
    return(53.59)
  }
  else if(input$CAMSISSoc==1253 & input$EStatus==5 & input$Sex==1){
    return(58.76)
  }
  else if(input$CAMSISSoc==1254 & input$EStatus==1 & input$Sex==1){
    return(43.88)
  }
  else if(input$CAMSISSoc==1254 & input$EStatus==1 & input$Sex==2){
    return(50.62)
  }
  else if(input$CAMSISSoc==1254 & input$EStatus==2 & input$Sex==1){
    return(55.69)
  }
  else if(input$CAMSISSoc==1254 & input$EStatus==2 & input$Sex==2){
    return(56.45)
  }
  else if(input$CAMSISSoc==1254 & input$EStatus>=3 & input$Sex==1){
    return(46.38)
  }
  else if(input$CAMSISSoc==1254 & input$EStatus>=3 & input$Sex==2){
    return(54.57)
  }
  else if(input$CAMSISSoc==1255 & input$EStatus==1 & input$Sex==1){
    return(56.08)
  }
  else if(input$CAMSISSoc==1255 & input$EStatus>=1 & input$Sex==2){
    return(57.21)
  }
  else if(input$CAMSISSoc==1255 & input$EStatus==2 & input$Sex==1){
    return(66.01)
  }
  else if(input$CAMSISSoc==1255 & input$EStatus>=2 & input$Sex==1){
    return(50.42)
  }
  else if(input$CAMSISSoc==1259 & input$EStatus==1 & input$Sex==1){
    return(56.08)
  }
  else if(input$CAMSISSoc==1259 & input$EStatus>=1 & input$Sex==2){
    return(57.21)
  }
  else if(input$CAMSISSoc==1259 & input$EStatus==2 & input$Sex==1){
    return(66.01)
  }
  else if(input$CAMSISSoc==1259 & input$EStatus>=3 & input$Sex==1){
    return(53.59)
  }
  else if(input$CAMSISSoc==2111 & input$EStatus==1 & input$Sex==1){
    return(72.93)
  }
  else if(input$CAMSISSoc==2111 & input$EStatus>=1 & input$Sex==2){
    return(72.24)
  }
  else if(input$CAMSISSoc==2111 & input$EStatus==2 & input$Sex==1){
    return(72.93)
  }
  else if(input$CAMSISSoc==2111 & input$EStatus==3 & input$Sex==1){
    return(67.52)
  }
  else if(input$CAMSISSoc==2111 & input$EStatus==4 & input$Sex==1){
    return(76.84)
  }
  else if(input$CAMSISSoc==2111 & input$EStatus==5 & input$Sex==1){
    return(76.84)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus==1 & input$Sex==1){
    return(67.32)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus==1 & input$Sex==2){
    return(79.47)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus==2 & input$Sex==1){
    return(70.16)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus==2 & input$Sex==2){
    return(74.09)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus==3 & input$Sex==1){
    return(63.81)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus==3 & input$Sex==2){
    return(79.47)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus>=4 & input$Sex==1){
    return(70.16)
  }
  else if(input$CAMSISSoc==2112 & input$EStatus>=4 & input$Sex==2){
    return(74.09)
  }
  else if(input$CAMSISSoc==2113 & input$EStatus==1 & input$Sex==1){
    return(72.68)
  }
  else if(input$CAMSISSoc==2113 & input$EStatus>=1 & input$Sex==2){
    return(80.43)
  }
  else if(input$CAMSISSoc==2113 & input$EStatus==2 & input$Sex==1){
    return(72.66)
  }
  else if(input$CAMSISSoc==2113 & input$EStatus==3 & input$Sex==1){
    return(72.71)
  }
  else if(input$CAMSISSoc==2113 & input$EStatus>=4 & input$Sex==1){
    return(72.66)
  }
  else if(input$CAMSISSoc==2114 & input$EStatus==1 & input$Sex==1){
    return(82.50)
  }
  else if(input$CAMSISSoc==2114 & input$EStatus>=1 & input$Sex==2){
    return(80.43)
  }
  else if(input$CAMSISSoc==2114 & input$EStatus==2 & input$Sex==1){
    return(77.36)
  }
  else if(input$CAMSISSoc==2114 & input$EStatus==3 & input$Sex==1){
    return(92.04)
  }
  else if(input$CAMSISSoc==2114 & input$EStatus==4 & input$Sex==1){
    return(82.50)
  }
  else if(input$CAMSISSoc==2114 & input$EStatus==5 & input$Sex==1){
    return(77.36)
  }
  else if(input$CAMSISSoc==2119 & input$EStatus==1 & input$Sex==1){
    return(92.04)
  }
  else if(input$CAMSISSoc==2119 & input$EStatus>=1 & input$Sex==2){
    return(80.43)
  }
  else if(input$CAMSISSoc==2119 & input$EStatus==2 & input$Sex==1){
    return(77.36)
  }
  else if(input$CAMSISSoc==2119 & input$EStatus==3 & input$Sex==1){
    return(92.04)
  }
  else if(input$CAMSISSoc==2119 & input$EStatus>=4 & input$Sex==1){
    return(77.36)
  }
  else if(input$CAMSISSoc==2121 & input$EStatus==1 & input$Sex==1){
    return(65.79)
  }
  else if(input$CAMSISSoc==2121 & input$EStatus>=1 & input$Sex==2){
    return(78.74)
  }
  else if(input$CAMSISSoc==2121 & input$EStatus==2 & input$Sex==1){
    return(62.76)
  }
  else if(input$CAMSISSoc==2121 & input$EStatus==3 & input$Sex==1){
    return(65.79)
  }
  else if(input$CAMSISSoc==2121 & input$EStatus>=4 & input$Sex==1){
    return(62.76)
  }
  else if(input$CAMSISSoc==2122 & input$EStatus==1 & input$Sex==1){
    return(57.44)
  }
  else if(input$CAMSISSoc==2122 & input$EStatus>=1 & input$Sex==2){
    return(78.74)
  }
  else if(input$CAMSISSoc==2122 & input$EStatus==2 & input$Sex==1){
    return(49.62)
  }
  else if(input$CAMSISSoc==2122 & input$EStatus==3 & input$Sex==1){
    return(57.44)
  }
  else if(input$CAMSISSoc==2122 & input$EStatus>=4 & input$Sex==1){
    return(49.62)
  }
  else if(input$CAMSISSoc==2123 & input$EStatus==1 & input$Sex==1){
    return(61.40)
  }
  else if(input$CAMSISSoc==2123 & input$EStatus>=1 & input$Sex==2){
    return(52.15)
  }
  else if(input$CAMSISSoc==2123 & input$EStatus==2 & input$Sex==1){
    return(51.22)
  }
  else if(input$CAMSISSoc==2123 & input$EStatus==3 & input$Sex==1){
    return(61.40)
  }
  else if(input$CAMSISSoc==2123 & input$EStatus>=4 & input$Sex==1){
    return(51.22)
  }
  else if(input$CAMSISSoc==2124 & input$EStatus==1 & input$Sex==1){
    return(55.69)
  }
  else if(input$CAMSISSoc==2124 & input$EStatus>=1 & input$Sex==2){
    return(52.15)
  }
  else if(input$CAMSISSoc==2124 & input$EStatus==2 & input$Sex==1){
    return(50.61)
  }
  else if(input$CAMSISSoc==2124 & input$EStatus==3 & input$Sex==1){
    return(55.69)
  }
  else if(input$CAMSISSoc==2124 & input$EStatus>=4 & input$Sex==1){
    return(50.61)
  }
  else if(input$CAMSISSoc==2126 & input$EStatus==1 & input$Sex==1){
    return(68.54)
  }
  else if(input$CAMSISSoc==2126 & input$EStatus>=1 & input$Sex==2){
    return(52.15)
  }
  else if(input$CAMSISSoc==2126 & input$EStatus==2 & input$Sex==1){
    return(63.41)
  }
  else if(input$CAMSISSoc==2126 & input$EStatus==3 & input$Sex==1){
    return(68.54)
  }
  else if(input$CAMSISSoc==2126 & input$EStatus>=4 & input$Sex==1){
    return(63.41)
  }
  else if(input$CAMSISSoc==2127 & input$EStatus==1 & input$Sex==1){
    return(58.28)
  }
  else if(input$CAMSISSoc==2127 & input$EStatus>=1 & input$Sex==2){
    return(52.15)
  }
  else if(input$CAMSISSoc==2127 & input$EStatus==2 & input$Sex==1){
    return(52.85)
  }
  else if(input$CAMSISSoc==2127 & input$EStatus==3 & input$Sex==1){
    return(58.28)
  }
  else if(input$CAMSISSoc==2127 & input$EStatus>=4 & input$Sex==1){
    return(52.85)
  }
  else if(input$CAMSISSoc==2129 & input$EStatus==1 & input$Sex==1){
    return(56.02)
  }
  else if(input$CAMSISSoc==2129 & input$EStatus>=1 & input$Sex==2){
    return(52.15)
  }
  else if(input$CAMSISSoc==2129 & input$EStatus==2 & input$Sex==1){
    return(51.29)
  }
  else if(input$CAMSISSoc==2129 & input$EStatus==3 & input$Sex==1){
    return(56.02)
  }
  else if(input$CAMSISSoc==2129 & input$EStatus>=4 & input$Sex==1){
    return(51.29)
  }
  else if(input$CAMSISSoc==2133 & input$EStatus==1 & input$Sex==1){
    return(66.24)
  }
  else if(input$CAMSISSoc==2133 & input$EStatus>=1 & input$Sex==2){
    return(65.90)
  }
  else if(input$CAMSISSoc==2133 & input$EStatus==2 & input$Sex==1){
    return(61.67)
  }
  else if(input$CAMSISSoc==2133 & input$EStatus==3 & input$Sex==1){
    return(66.24)
  }
  else if(input$CAMSISSoc==2133 & input$EStatus>=4 & input$Sex==1){
    return(61.67)
  }
  else if(input$CAMSISSoc==2134 & input$EStatus==1 & input$Sex==1){
    return(67.22)
  }
  else if(input$CAMSISSoc==2134 & input$EStatus>=1 & input$Sex==2){
    return(78.12)
  }
  else if(input$CAMSISSoc==2134 & input$EStatus==2 & input$Sex==1){
    return(67.35)
  }
  else if(input$CAMSISSoc==2134 & input$EStatus==3 & input$Sex==1){
    return(67.22)
  }
  else if(input$CAMSISSoc==2134 & input$EStatus>=4 & input$Sex==1){
    return(67.35)
  }
  else if(input$CAMSISSoc==2135 & input$EStatus==1 & input$Sex==1){
    return(71.15)
  }
  else if(input$CAMSISSoc==2135 & input$EStatus>=1 & input$Sex==2){
    return(53.60)
  }
  else if(input$CAMSISSoc==2135 & input$EStatus==2 & input$Sex==1){
    return(59.45)
  }
  else if(input$CAMSISSoc==2135 & input$EStatus==3 & input$Sex==1){
    return(71.15)
  }
  else if(input$CAMSISSoc==2135 & input$EStatus>=4 & input$Sex==1){
    return(59.45)
  }
  else if(input$CAMSISSoc==2136 & input$EStatus==1 & input$Sex==1){
    return(67.18)
  }
  else if(input$CAMSISSoc==2136 & input$EStatus>=1 & input$Sex==2){
    return(64.01)
  }
  else if(input$CAMSISSoc==2136 & input$EStatus==2 & input$Sex==1){
    return(77.33)
  }
  else if(input$CAMSISSoc==2136 & input$EStatus==3 & input$Sex==1){
    return(67.18)
  }
  else if(input$CAMSISSoc==2136 & input$EStatus>=4 & input$Sex==1){
    return(63.61)
  }
  else if(input$CAMSISSoc==2137 & input$EStatus==1 & input$Sex==1){
    return(71.55)
  }
  else if(input$CAMSISSoc==2137 & input$EStatus>=1 & input$Sex==2){
    return(78.18)
  }
  else if(input$CAMSISSoc==2137 & input$EStatus==2 & input$Sex==1){
    return(83.49)
  }
  else if(input$CAMSISSoc==2137 & input$EStatus==3 & input$Sex==1){
    return(71.55)
  }
  else if(input$CAMSISSoc==2137 & input$EStatus>=4 & input$Sex==1){
    return(42.84)
  }
  else if(input$CAMSISSoc==2139 & input$EStatus==1 & input$Sex==1){
    return(66.49)
  }
  else if(input$CAMSISSoc==2139 & input$EStatus>=1 & input$Sex==2){
    return(71.42)
  }
  else if(input$CAMSISSoc==2139 & input$EStatus==2 & input$Sex==1){
    return(60.58)
  }
  else if(input$CAMSISSoc==2139 & input$EStatus==3 & input$Sex==1){
    return(66.49)
  }
  else if(input$CAMSISSoc==2139 & input$EStatus>=4 & input$Sex==1){
    return(61.91)
  }
  else if(input$CAMSISSoc==2141 & input$EStatus==1 & input$Sex==1){
    return(63.35)
  }
  else if(input$CAMSISSoc==2141 & input$EStatus>=1 & input$Sex==2){
    return(73.03)
  }
  else if(input$CAMSISSoc==2141 & input$EStatus==2 & input$Sex==1){
    return(63.35)
  }
  else if(input$CAMSISSoc==2141 & input$EStatus==3 & input$Sex==1){
    return(64.74)
  }
  else if(input$CAMSISSoc==2141 & input$EStatus>=4 & input$Sex==1){
    return(63.35)
  }
  else if(input$CAMSISSoc==2142 & input$EStatus==1 & input$Sex==1){
    return(63.95)
  }
  else if(input$CAMSISSoc==2142 & input$EStatus>=1 & input$Sex==2){
    return(73.03)
  }
  else if(input$CAMSISSoc==2142 & input$EStatus==2 & input$Sex==1){
    return(63.35)
  }
  else if(input$CAMSISSoc==2142 & input$EStatus==3 & input$Sex==1){
    return(64.74)
  }
  else if(input$CAMSISSoc==2142 & input$EStatus>=4 & input$Sex==1){
    return(63.35)
  }
  else if(input$CAMSISSoc==2150 & input$EStatus>=1 & input$Sex==1){
    return(77.06)
  }
  else if(input$CAMSISSoc==2150 & input$EStatus>=1 & input$Sex==2){
    return(68.37)
  }
  else if(input$CAMSISSoc==2211 & input$EStatus==1 & input$Sex==1){
    return(69.58)
  }
  else if(input$CAMSISSoc==2211 & input$EStatus==1 & input$Sex==2){
    return(92.63)
  }
  else if(input$CAMSISSoc==2211 & input$EStatus==2 & input$Sex==1){
    return(69.58)
  }
  else if(input$CAMSISSoc==2211 & input$EStatus==2 & input$Sex==2){
    return(92.63)
  }
  else if(input$CAMSISSoc==2211 & input$EStatus>=3 & input$Sex==1){
    return(73.08)
  }
  else if(input$CAMSISSoc==2211 & input$EStatus>=3 & input$Sex==2){
    return(75.58)
  }
  else if(input$CAMSISSoc==2212 & input$EStatus>=1 & input$Sex==1){
    return(64.68)
  }
  else if(input$CAMSISSoc==2212 & input$EStatus>=1 & input$Sex==2){
    return(75.02)
  }
  else if(input$CAMSISSoc==2213 & input$EStatus>=1 & input$Sex==1){
    return(58.43)
  }
  else if(input$CAMSISSoc==2213 & input$EStatus>=1 & input$Sex==2){
    return(67.82)
  }
  else if(input$CAMSISSoc==2214 & input$EStatus>=1 & input$Sex==1){
    return(42.92)
  }
  else if(input$CAMSISSoc==2214 & input$EStatus>=1 & input$Sex==2){
    return(65.44)
  }
  else if(input$CAMSISSoc==2215 & input$EStatus==1 & input$Sex==1){
    return(82.48)
  }
  else if(input$CAMSISSoc==2215 & input$EStatus>=1 & input$Sex==2){
    return(71.23)
  }
  else if(input$CAMSISSoc==2215 & input$EStatus>=2 & input$Sex==1){
    return(77.80)
  }
  else if(input$CAMSISSoc==2216 & input$EStatus>=1 & input$Sex==1){
    return(78.57)
  }
  else if(input$CAMSISSoc==2216 & input$EStatus>=1 & input$Sex==2){
    return(61.21)
  }
  else if(input$CAMSISSoc==2217 & input$EStatus>=1 & input$Sex==1){
    return(64.68)
  }
  else if(input$CAMSISSoc==2217 & input$EStatus>=1 & input$Sex==2){
    return(67.46)
  }
  else if(input$CAMSISSoc==2218 & input$EStatus>=1 & input$Sex==1){
    return(64.68)
  }
  else if(input$CAMSISSoc==2218 & input$EStatus>=1 & input$Sex==2){
    return(75.02)
  }
  else if(input$CAMSISSoc==2219 & input$EStatus>=1 & input$Sex==1){
    return(64.68)
  }
  else if(input$CAMSISSoc==2219 & input$EStatus>=1 & input$Sex==2){
    return(75.02)
  }
  else if(input$CAMSISSoc==2221 & input$EStatus>=1 & input$Sex==1){
    return(72.66)
  }
  else if(input$CAMSISSoc==2221 & input$EStatus>=1 & input$Sex==2){
    return(73.29)
  }
  else if(input$CAMSISSoc==2222 & input$EStatus>=1 & input$Sex==1){
    return(72.66)
  }
  else if(input$CAMSISSoc==2222 & input$EStatus>=1 & input$Sex==2){
    return(73.29)
  }
  else if(input$CAMSISSoc==2223 & input$EStatus>=1 & input$Sex==1){
    return(72.66)
  }
  else if(input$CAMSISSoc==2223 & input$EStatus>=1 & input$Sex==2){
    return(72.82)
  }
  else if(input$CAMSISSoc==2229 & input$EStatus>=1 & input$Sex==1){
    return(72.66)
  }
  else if(input$CAMSISSoc==2229 & input$EStatus>=1 & input$Sex==2){
    return(72.82)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==1 & input$Sex==1){
    return(49.67)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==1 & input$Sex==2){
    return(52.74)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==2 & input$Sex==1){
    return(49.67)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==2 & input$Sex==2){
    return(52.62)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==3 & input$Sex==1){
    return(58.61)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==3 & input$Sex==2){
    return(52.74)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus>=4 & input$Sex==1){
    return(44.36)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==4 & input$Sex==2){
    return(48.55)
  }
  else if(input$CAMSISSoc==2231 & input$EStatus==5 & input$Sex==2){
    return(52.62)
  }
  else if(input$CAMSISSoc==2232 & input$EStatus>=1 & input$Sex==1){
    return(58.61)
  }
  else if(input$CAMSISSoc==2232 & input$EStatus>=1 & input$Sex==2){
    return(56.23)
  }
  else if(input$CAMSISSoc==2311 & input$EStatus>=1 & input$Sex==1){
    return(80.02)
  }
  else if(input$CAMSISSoc==2311 & input$EStatus>=1 & input$Sex==2){
    return(82.32)
  }
  else if(input$CAMSISSoc==2312 & input$EStatus>=1 & input$Sex==1){
    return(70.58)
  }
  else if(input$CAMSISSoc==2312 & input$EStatus>=1 & input$Sex==2){
    return(74.85)
  }
  else if(input$CAMSISSoc==2314 & input$EStatus>=1 & input$Sex==1){
    return(73.60)
  }
  else if(input$CAMSISSoc==2314 & input$EStatus>=1 & input$Sex==2){
    return(69.93)
  }
  else if(input$CAMSISSoc==2315 & input$EStatus>=1 & input$Sex==1){
    return(74.73)
  }
  else if(input$CAMSISSoc==2315 & input$EStatus>=1 & input$Sex==2){
    return(63.47)
  }
  else if(input$CAMSISSoc==2316 & input$EStatus>=1 & input$Sex==1){
    return(61.32)
  }
  else if(input$CAMSISSoc==2316 & input$EStatus>=1 & input$Sex==2){
    return(61.08)
  }
  else if(input$CAMSISSoc==2317 & input$EStatus>=1 & input$Sex==1){
    return(74.57)
  }
  else if(input$CAMSISSoc==2317 & input$EStatus>=1 & input$Sex==2){
    return(66.12)
  }
  else if(input$CAMSISSoc==2318 & input$EStatus>=1 & input$Sex==1){
    return(78.92)
  }
  else if(input$CAMSISSoc==2318 & input$EStatus>=1 & input$Sex==2){
    return(73.53)
  }
  else if(input$CAMSISSoc==2319 & input$EStatus==1 & input$Sex==1){
    return(65.76)
  }
  else if(input$CAMSISSoc==2319 & input$EStatus==1 & input$Sex==2){
    return(73.82)
  }
  else if(input$CAMSISSoc==2319 & input$EStatus==2 & input$Sex==1){
    return(65.76)
  }
  else if(input$CAMSISSoc==2319 & input$EStatus==2 & input$Sex==2){
    return(73.82)
  }
  else if(input$CAMSISSoc==2319 & input$EStatus>=3 & input$Sex==1){
    return(70.73)
  }
  else if(input$CAMSISSoc==2319 & input$EStatus>=3 & input$Sex==2){
    return(64.90)
  }
  else if(input$CAMSISSoc==2412 & input$EStatus==1 & input$Sex==1){
    return(76.82)
  }
  else if(input$CAMSISSoc==2412 & input$EStatus==1 & input$Sex==2){
    return(83.30)
  }
  else if(input$CAMSISSoc==2412 & input$EStatus==2 & input$Sex==1){
    return(79.44)
  }
  else if(input$CAMSISSoc==2412 & input$EStatus>=2 & input$Sex==2){
    return(74.47)
  }
  else if(input$CAMSISSoc==2412 & input$EStatus>=3 & input$Sex==1){
    return(74.99)
  }
  else if(input$CAMSISSoc==2413 & input$EStatus==1 & input$Sex==1){
    return(76.82)
  }
  else if(input$CAMSISSoc==2413 & input$EStatus==1 & input$Sex==2){
    return(83.30)
  }
  else if(input$CAMSISSoc==2413 & input$EStatus==2 & input$Sex==1){
    return(79.44)
  }
  else if(input$CAMSISSoc==2413 & input$EStatus>=2 & input$Sex==2){
    return(74.47)
  }
  else if(input$CAMSISSoc==2413 & input$EStatus>=3 & input$Sex==1){
    return(74.99)
  }
  else if(input$CAMSISSoc==2419 & input$EStatus==1 & input$Sex==1){
    return(76.82)
  }
  else if(input$CAMSISSoc==2419 & input$EStatus>=1 & input$Sex==2){
    return(74.47)
  }
  else if(input$CAMSISSoc==2419 & input$EStatus==2 & input$Sex==1){
    return(79.44)
  }
  else if(input$CAMSISSoc==2419 & input$EStatus>=3 & input$Sex==1){
    return(74.99)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus==1 & input$Sex==1){
    return(75.83)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus==1 & input$Sex==2){
    return(83.30)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus==2 & input$Sex==1){
    return(73.82)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus==2 & input$Sex==2){
    return(66.11)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus==3 & input$Sex==1){
    return(64.99)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus==3 & input$Sex==2){
    return(66.95)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus>=4 & input$Sex==1){
    return(66.80)
  }
  else if(input$CAMSISSoc==2421 & input$EStatus>=4 & input$Sex==2){
    return(66.11)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus==1 & input$Sex==1){
    return(75.83)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus==1 & input$Sex==2){
    return(83.30)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus==2 & input$Sex==1){
    return(73.21)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus==2 & input$Sex==2){
    return(68.93)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus==3 & input$Sex==1){
    return(74.47)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus==3 & input$Sex==2){
    return(67.63)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus>=4 & input$Sex==1){
    return(70.38)
  }
  else if(input$CAMSISSoc==2423 & input$EStatus>=4 & input$Sex==2){
    return(68.93)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus==1 & input$Sex==1){
    return(75.83)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus==1 & input$Sex==2){
    return(70.49)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus==2 & input$Sex==1){
    return(73.21)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus==2 & input$Sex==2){
    return(75.22)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus==3 & input$Sex==1){
    return(69.07)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus==3 & input$Sex==2){
    return(67.63)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus>=4 & input$Sex==1){
    return(69.51)
  }
  else if(input$CAMSISSoc==2424 & input$EStatus>=4 & input$Sex==2){
    return(75.22)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus==1 & input$Sex==1){
    return(76.88)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus==1 & input$Sex==2){
    return(72.03)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus==2 & input$Sex==1){
    return(76.88)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus==2 & input$Sex==2){
    return(73.42)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus==3 & input$Sex==1){
    return(71.92)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus==3 & input$Sex==2){
    return(67.63)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus>=4 & input$Sex==1){
    return(81.69)
  }
  else if(input$CAMSISSoc==2425 & input$EStatus>=4 & input$Sex==2){
    return(73.42)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus==1 & input$Sex==1){
    return(69.84)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus==1 & input$Sex==2){
    return(73.36)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus==2 & input$Sex==1){
    return(73.21)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus==2 & input$Sex==2){
    return(74.21)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus==3 & input$Sex==1){
    return(71.92)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus==3 & input$Sex==2){
    return(67.63)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus>=4 & input$Sex==1){
    return(68.52)
  }
  else if(input$CAMSISSoc==2426 & input$EStatus>=4 & input$Sex==2){
    return(74.21)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus==1 & input$Sex==1){
    return(75.83)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus==1 & input$Sex==2){
    return(83.30)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus==2 & input$Sex==1){
    return(73.21)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus==2 & input$Sex==2){
    return(70.78)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus==3 & input$Sex==1){
    return(71.92)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus==3 & input$Sex==2){
    return(67.63)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus>=4 & input$Sex==1){
    return(75.99)
  }
  else if(input$CAMSISSoc==2429 & input$EStatus>=4 & input$Sex==2){
    return(73.42)
  }
  else if(input$CAMSISSoc==2431 & input$EStatus==1 & input$Sex==1){
    return(67.60)
  }
  else if(input$CAMSISSoc==2431 & input$EStatus>=1 & input$Sex==2){
    return(78.07)
  }
  else if(input$CAMSISSoc==2431 & input$EStatus==2 & input$Sex==1){
    return(71.86)
  }
  else if(input$CAMSISSoc==2431 & input$EStatus==3 & input$Sex==1){
    return(80.51)
  }
  else if(input$CAMSISSoc==2431 & input$EStatus>=4 & input$Sex==1){
    return(71.80)
  }
  else if(input$CAMSISSoc==2432 & input$EStatus==1 & input$Sex==1){
    return(67.60)
  }
  else if(input$CAMSISSoc==2432 & input$EStatus>=1 & input$Sex==2){
    return(78.07)
  }
  else if(input$CAMSISSoc==2432 & input$EStatus==2 & input$Sex==1){
    return(62.72)
  }
  else if(input$CAMSISSoc==2432 & input$EStatus==3 & input$Sex==1){
    return(52.95)
  }
  else if(input$CAMSISSoc==2432 & input$EStatus>=4 & input$Sex==1){
    return(64.17)
  }
  else if(input$CAMSISSoc==2433 & input$EStatus==1 & input$Sex==1){
    return(67.60)
  }
  else if(input$CAMSISSoc==2433 & input$EStatus>=1 & input$Sex==2){
    return(78.07)
  }
  else if(input$CAMSISSoc==2433 & input$EStatus==2 & input$Sex==1){
    return(62.72)
  }
  else if(input$CAMSISSoc==2433 & input$EStatus==3 & input$Sex==1){
    return(52.95)
  }
  else if(input$CAMSISSoc==2433 & input$EStatus>=4 & input$Sex==1){
    return(64.17)
  }
  else if(input$CAMSISSoc==2434 & input$EStatus==1 & input$Sex==1){
    return(67.60)
  }
  else if(input$CAMSISSoc==2434 & input$EStatus>=1 & input$Sex==2){
    return(78.07)
  }
  else if(input$CAMSISSoc==2434 & input$EStatus==2 & input$Sex==1){
    return(62.72)
  }
  else if(input$CAMSISSoc==2434 & input$EStatus==3 & input$Sex==1){
    return(74.23)
  }
  else if(input$CAMSISSoc==2434 & input$EStatus>=4 & input$Sex==1){
    return(67.29)
  }
  else if(input$CAMSISSoc==2435 & input$EStatus==1 & input$Sex==1){
    return(73.37)
  }
  else if(input$CAMSISSoc==2435 & input$EStatus>=1 & input$Sex==2){
    return(78.07)
  }
  else if(input$CAMSISSoc==2435 & input$EStatus==2 & input$Sex==1){
    return(71.86)
  }
  else if(input$CAMSISSoc==2435 & input$EStatus==3 & input$Sex==1){
    return(80.51)
  }
  else if(input$CAMSISSoc==2435 & input$EStatus>=4 & input$Sex==1){
    return(71.80)
  }
  else if(input$CAMSISSoc==2436 & input$EStatus==1 & input$Sex==1){
    return(67.60)
  }
  else if(input$CAMSISSoc==2436 & input$EStatus>=1 & input$Sex==2){
    return(78.07)
  }
  else if(input$CAMSISSoc==2436 & input$EStatus>=2 & input$Sex==1){
    return(56.98)
  }
  else if(input$CAMSISSoc==2442 & input$EStatus>=1 & input$Sex==1){
    return(65.01)
  }
  else if(input$CAMSISSoc==2442 & input$EStatus==1 & input$Sex==2){
    return(62.58)
  }
  else if(input$CAMSISSoc==2442 & input$EStatus==2 & input$Sex==2){
    return(57.96)
  }
  else if(input$CAMSISSoc==2442 & input$EStatus==3 & input$Sex==2){
    return(62.58)
  }
  else if(input$CAMSISSoc==2442 & input$EStatus>=4 & input$Sex==2){
    return(57.96)
  }
  else if(input$CAMSISSoc==2443 & input$EStatus>=1 & input$Sex==1){
    return(65.01)
  }
  else if(input$CAMSISSoc==2443 & input$EStatus==1 & input$Sex==2){
    return(58.54)
  }
  else if(input$CAMSISSoc==2443 & input$EStatus==2 & input$Sex==2){
    return(58.54)
  }
  else if(input$CAMSISSoc==2443 & input$EStatus==3 & input$Sex==2){
    return(62.58)
  }
  else if(input$CAMSISSoc==2443 & input$EStatus==4 & input$Sex==2){
    return(58.54)
  }
  else if(input$CAMSISSoc==2443 & input$EStatus==5 & input$Sex==2){
    return(57.96)
  }
  else if(input$CAMSISSoc==2444 & input$EStatus>=1 & input$Sex==1){
    return(71.49)
  }
  else if(input$CAMSISSoc==2444 & input$EStatus>=1 & input$Sex==2){
    return(68.16)
  }
  else if(input$CAMSISSoc==2449 & input$EStatus>=1 & input$Sex==1){
    return(65.01)
  }
  else if(input$CAMSISSoc==2449 & input$EStatus>=1 & input$Sex==2){
    return(68.16)
  }
  else if(input$CAMSISSoc==2451 & input$EStatus>=1 & input$Sex==1){
    return(62.08)
  }
  else if(input$CAMSISSoc==2451 & input$EStatus>=1 & input$Sex==2){
    return(76.87)
  }
  else if(input$CAMSISSoc==2452 & input$EStatus>=1 & input$Sex==1){
    return(62.08)
  }
  else if(input$CAMSISSoc==2452 & input$EStatus>=1 & input$Sex==2){
    return(76.87)
  }
  else if(input$CAMSISSoc==2461 & input$EStatus>=1 & input$Sex==1){
    return(50.57)
  }
  else if(input$CAMSISSoc==2461 & input$EStatus>=1 & input$Sex==2){
    return(53.07)
  }
  else if(input$CAMSISSoc==2462 & input$EStatus>=1 & input$Sex==1){
    return(64.87)
  }
  else if(input$CAMSISSoc==2462 & input$EStatus>=1 & input$Sex==2){
    return(53.07)
  }
  else if(input$CAMSISSoc==2463 & input$EStatus>=1 & input$Sex==1){
    return(64.87)
  }
  else if(input$CAMSISSoc==2463 & input$EStatus>=1 & input$Sex==2){
    return(53.07)
  }
  else if(input$CAMSISSoc==2471 & input$EStatus>=1 & input$Sex==1){
    return(78.46)
  }
  else if(input$CAMSISSoc==2471 & input$EStatus>=1 & input$Sex==2){
    return(76.62)
  }
  else if(input$CAMSISSoc==2472 & input$EStatus>=1 & input$Sex==1){
    return(79.96)
  }
  else if(input$CAMSISSoc==2472 & input$EStatus>=1 & input$Sex==2){
    return(67.38)
  }
  else if(input$CAMSISSoc==2473 & input$EStatus>=1 & input$Sex==1){
    return(73.38)
  }
  else if(input$CAMSISSoc==2473 & input$EStatus>=1 & input$Sex==2){
    return(67.32)
  }
  else if(input$CAMSISSoc==3111 & input$EStatus>=1 & input$Sex==1){
    return(51.91)
  }
  else if(input$CAMSISSoc==3111 & input$EStatus>=1 & input$Sex==2){
    return(59.66)
  }
  else if(input$CAMSISSoc==3112 & input$EStatus>=1 & input$Sex==1){
    return(47.87)
  }
  else if(input$CAMSISSoc==3112 & input$EStatus>=1 & input$Sex==2){
    return(50.51)
  }
  else if(input$CAMSISSoc==3113 & input$EStatus>=1 & input$Sex==1){
    return(47.87)
  }
  else if(input$CAMSISSoc==3113 & input$EStatus>=1 & input$Sex==2){
    return(50.51)
  }
  else if(input$CAMSISSoc==3114 & input$EStatus>=1 & input$Sex==1){
    return(57.46)
  }
  else if(input$CAMSISSoc==3114 & input$EStatus>=1 & input$Sex==2){
    return(50.51)
  }
  else if(input$CAMSISSoc==3115 & input$EStatus>=1 & input$Sex==1){
    return(42.37)
  }
  else if(input$CAMSISSoc==3115 & input$EStatus>=1 & input$Sex==2){
    return(45.88)
  }
  else if(input$CAMSISSoc==3116 & input$EStatus>=1 & input$Sex==1){
    return(30.39)
  }
  else if(input$CAMSISSoc==3116 & input$EStatus>=1 & input$Sex==2){
    return(53.79)
  }
  else if(input$CAMSISSoc==3119 & input$EStatus>=1 & input$Sex==1){
    return(44.35)
  }
  else if(input$CAMSISSoc==3119 & input$EStatus>=1 & input$Sex==2){
    return(53.79)
  }
  else if(input$CAMSISSoc==3121 & input$EStatus>=1 & input$Sex==1){
    return(50.88)
  }
  else if(input$CAMSISSoc==3121 & input$EStatus>=1 & input$Sex==2){
    return(54.02)
  }
  else if(input$CAMSISSoc==3122 & input$EStatus>=1 & input$Sex==1){
    return(52.15)
  }
  else if(input$CAMSISSoc==3122 & input$EStatus>=1 & input$Sex==2){
    return(54.02)
  }
  else if(input$CAMSISSoc==3122 & input$EStatus>=1 & input$Sex==1){
    return(52.15)
  }
  else if(input$CAMSISSoc==3122 & input$EStatus>=1 & input$Sex==2){
    return(54.02)
  }
  else if(input$CAMSISSoc==3131 & input$EStatus>=1 & input$Sex==1){
    return(54.03)
  }
  else if(input$CAMSISSoc==3131 & input$EStatus>=1 & input$Sex==2){
    return(62.06)
  }
  else if(input$CAMSISSoc==3132 & input$EStatus>=1 & input$Sex==1){
    return(49.81)
  }
  else if(input$CAMSISSoc==3132 & input$EStatus>=1 & input$Sex==2){
    return(56.37)
  }
  else if(input$CAMSISSoc==3213 & input$EStatus>=1 & input$Sex==1){
    return(62.13)
  }
  else if(input$CAMSISSoc==3213 & input$EStatus>=1 & input$Sex==2){
    return(61.87)
  }
  else if(input$CAMSISSoc==3216 & input$EStatus>=1 & input$Sex==1){
    return(51.34)
  }
  else if(input$CAMSISSoc==3216 & input$EStatus==1 & input$Sex==2){
    return(45.82)
  }
  else if(input$CAMSISSoc==3216 & input$EStatus==2 & input$Sex==2){
    return(63.69)
  }
  else if(input$CAMSISSoc==3216 & input$EStatus==3 & input$Sex==2){
    return(45.04)
  }
  else if(input$CAMSISSoc==3216 & input$EStatus==4 & input$Sex==2){
    return(45.82)
  }
  else if(input$CAMSISSoc==3216 & input$EStatus==5 & input$Sex==2){
    return(45.04)
  }
  else if(input$CAMSISSoc==3217 & input$EStatus==1 & input$Sex==1){
    return(53.78)
  }
  else if(input$CAMSISSoc==3217 & input$EStatus>=1 & input$Sex==2){
    return(42.63)
  }
  else if(input$CAMSISSoc==3217 & input$EStatus==2 & input$Sex==1){
    return(73.36)
  }
  else if(input$CAMSISSoc==3217 & input$EStatus>=3 & input$Sex==1){
    return(51.34)
  }
  else if(input$CAMSISSoc==3218 & input$EStatus==1 & input$Sex==1){
    return(73.36)
  }
  else if(input$CAMSISSoc==3218 & input$EStatus==1 & input$Sex==2){
    return(48.36)
  }
  else if(input$CAMSISSoc==3218 & input$EStatus==2 & input$Sex==1){
    return(73.36)
  }
  else if(input$CAMSISSoc==3218 & input$EStatus==2 & input$Sex==2){
    return(63.69)
  }
  else if(input$CAMSISSoc==3218 & input$EStatus>=3 & input$Sex==1){
    return(51.34)
  }
  else if(input$CAMSISSoc==3218 & input$EStatus>=3 & input$Sex==2){
    return(45.04)
  }
  else if(input$CAMSISSoc==3219 & input$EStatus==1 & input$Sex==1){
    return(62.35)
  }
  else if(input$CAMSISSoc==3219 & input$EStatus==1 & input$Sex==2){
    return(61.84)
  }
  else if(input$CAMSISSoc==3219 & input$EStatus==2 & input$Sex==1){
    return(73.36)
  }
  else if(input$CAMSISSoc==3219 & input$EStatus==2 & input$Sex==2){
    return(63.69)
  }
  else if(input$CAMSISSoc==3219 & input$EStatus>=3 & input$Sex==1){
    return(51.34)
  }
  else if(input$CAMSISSoc==3219 & input$EStatus>=3 & input$Sex==2){
    return(58.80)
  }
  else if(input$CAMSISSoc==3231 & input$EStatus>=1 & input$Sex==1){
    return(56.62)
  }
  else if(input$CAMSISSoc==3231 & input$EStatus>=1 & input$Sex==2){
    return(52.08)
  }
  else if(input$CAMSISSoc==3233 & input$EStatus>=1 & input$Sex==1){
    return(56.62)
  }
  else if(input$CAMSISSoc==3233 & input$EStatus>=1 & input$Sex==2){
    return(48.47)
  }
  else if(input$CAMSISSoc==3234 & input$EStatus>=1 & input$Sex==1){
    return(46.71)
  }
  else if(input$CAMSISSoc==3234 & input$EStatus>=1 & input$Sex==2){
    return(43.30)
  }
  else if(input$CAMSISSoc==3235 & input$EStatus>=1 & input$Sex==1){
    return(60.36)
  }
  else if(input$CAMSISSoc==3235 & input$EStatus>=1 & input$Sex==2){
    return(54.15)
  }
  else if(input$CAMSISSoc==3239 & input$EStatus>=1 & input$Sex==1){
    return(60.36)
  }
  else if(input$CAMSISSoc==3239 & input$EStatus>=1 & input$Sex==2){
    return(54.15)
  }
  else if(input$CAMSISSoc==3311 & input$EStatus>=1 & input$Sex==1){
    return(47.20)
  }
  else if(input$CAMSISSoc==3311 & input$EStatus>=1 & input$Sex==2){
    return(48.23)
  }
  else if(input$CAMSISSoc==3312 & input$EStatus>=1 & input$Sex==1){
    return(60.64)
  }
  else if(input$CAMSISSoc==3312 & input$EStatus>=1 & input$Sex==2){
    return(48.23)
  }
  else if(input$CAMSISSoc==3313 & input$EStatus>=1 & input$Sex==1){
    return(57.78)
  }
  else if(input$CAMSISSoc==3313 & input$EStatus>=1 & input$Sex==2){
    return(48.23)
  }
  else if(input$CAMSISSoc==3314 & input$EStatus>=1 & input$Sex==1){
    return(45.78)
  }
  else if(input$CAMSISSoc==3314 & input$EStatus>=1 & input$Sex==2){
    return(44.04)
  }
  else if(input$CAMSISSoc==3315 & input$EStatus>=1 & input$Sex==1){
    return(45.78)
  }
  else if(input$CAMSISSoc==3315 & input$EStatus>=1 & input$Sex==2){
    return(44.04)
  }
  else if(input$CAMSISSoc==3319 & input$EStatus>=1 & input$Sex==1){
    return(46.58)
  }
  else if(input$CAMSISSoc==3319 & input$EStatus>=1 & input$Sex==2){
    return(44.04)
  }
  else if(input$CAMSISSoc==3411 & input$EStatus>=1 & input$Sex==1){
    return(77.44)
  }
  else if(input$CAMSISSoc==3411 & input$EStatus>=1 & input$Sex==2){
    return(79.56)
  }
  else if(input$CAMSISSoc==3412 & input$EStatus>=1 & input$Sex==1){
    return(76.19)
  }
  else if(input$CAMSISSoc==3412 & input$EStatus>=1 & input$Sex==2){
    return(80.29)
  }
  else if(input$CAMSISSoc==3413 & input$EStatus>=1 & input$Sex==1){
    return(58.16)
  }
  else if(input$CAMSISSoc==3413 & input$EStatus>=1 & input$Sex==2){
    return(62.41)
  }
  else if(input$CAMSISSoc==3414 & input$EStatus>=1 & input$Sex==1){
    return(58.16)
  }
  else if(input$CAMSISSoc==3414 & input$EStatus>=1 & input$Sex==2){
    return(62.41)
  }
  else if(input$CAMSISSoc==3415 & input$EStatus>=1 & input$Sex==1){
    return(70.73)
  }
  else if(input$CAMSISSoc==3415 & input$EStatus>=1 & input$Sex==2){
    return(88.51)
  }
  else if(input$CAMSISSoc==3416 & input$EStatus==1 & input$Sex==1){
    return(75.65)
  }
  else if(input$CAMSISSoc==3416 & input$EStatus>=1 & input$Sex==2){
    return(79.11)
  }
  else if(input$CAMSISSoc==3416 & input$EStatus==2 & input$Sex==1){
    return(75.65)
  }
  else if(input$CAMSISSoc==3416 & input$EStatus>=3 & input$Sex==1){
    return(86.91)
  }
  else if(input$CAMSISSoc==3417 & input$EStatus==1 & input$Sex==1){
    return(74.22)
  }
  else if(input$CAMSISSoc==3417 & input$EStatus>=1 & input$Sex==2){
    return(68.27)
  }
  else if(input$CAMSISSoc==3417 & input$EStatus==2 & input$Sex==1){
    return(74.22)
  }
  else if(input$CAMSISSoc==3417 & input$EStatus>=3 & input$Sex==1){
    return(57.65)
  }
  else if(input$CAMSISSoc==3421 & input$EStatus==1 & input$Sex==1){
    return(71.12)
  }
  else if(input$CAMSISSoc==3421 & input$EStatus==1 & input$Sex==2){
    return(74.95)
  }
  else if(input$CAMSISSoc==3421 & input$EStatus==2 & input$Sex==1){
    return(71.12)
  }
  else if(input$CAMSISSoc==3421 & input$EStatus==2 & input$Sex==2){
    return(74.95)
  }
  else if(input$CAMSISSoc==3421 & input$EStatus>=3 & input$Sex==1){
    return(63.21)
  }
  else if(input$CAMSISSoc==3421 & input$EStatus>=3 & input$Sex==2){
    return(54.72)
  }
  else if(input$CAMSISSoc==3422 & input$EStatus==1 & input$Sex==1){
    return(71.12)
  }
  else if(input$CAMSISSoc==3422 & input$EStatus==1 & input$Sex==2){
    return(74.95)
  }
  else if(input$CAMSISSoc==3422 & input$EStatus==2 & input$Sex==1){
    return(71.12)
  }
  else if(input$CAMSISSoc==3422 & input$EStatus==2 & input$Sex==2){
    return(74.95)
  }
  else if(input$CAMSISSoc==3422 & input$EStatus>=3 & input$Sex==1){
    return(68.51)
  }
  else if(input$CAMSISSoc==3422 & input$EStatus>=3 & input$Sex==2){
    return(68.98)
  }
  else if(input$CAMSISSoc==3441 & input$EStatus>=1 & input$Sex==1){
    return(54.08)
  }
  else if(input$CAMSISSoc==3441 & input$EStatus>=1 & input$Sex==2){
    return(55.83)
  }
  else if(input$CAMSISSoc==3442 & input$EStatus>=1 & input$Sex==1){
    return(54.08)
  }
  else if(input$CAMSISSoc==3442 & input$EStatus>=1 & input$Sex==2){
    return(55.83)
  }
  else if(input$CAMSISSoc==3443 & input$EStatus>=1 & input$Sex==1){
    return(57.68)
  }
  else if(input$CAMSISSoc==3443 & input$EStatus>=1 & input$Sex==2){
    return(72.60)
  }
  else if(input$CAMSISSoc==3511 & input$EStatus>=1 & input$Sex==1){
    return(62.96)
  }
  else if(input$CAMSISSoc==3511 & input$EStatus>=1 & input$Sex==2){
    return(59.66)
  }
  else if(input$CAMSISSoc==3512 & input$EStatus>=1 & input$Sex==1){
    return(77.37)
  }
  else if(input$CAMSISSoc==3512 & input$EStatus>=1 & input$Sex==2){
    return(59.66)
  }
  else if(input$CAMSISSoc==3513 & input$EStatus>=1 & input$Sex==1){
    return(62.96)
  }
  else if(input$CAMSISSoc==3513 & input$EStatus>=1 & input$Sex==2){
    return(59.66)
  }
  else if(input$CAMSISSoc==3520 & input$EStatus>=1 & input$Sex==1){
    return(65.74)
  }
  else if(input$CAMSISSoc==3520 & input$EStatus>=1 & input$Sex==2){
    return(67.20)
  }
  else if(input$CAMSISSoc==3531 & input$EStatus==1 & input$Sex==1){
    return(66.40)
  }
  else if(input$CAMSISSoc==3531 & input$EStatus==1 & input$Sex==2){
    return(57.39)
  }
  else if(input$CAMSISSoc==3531 & input$EStatus==2 & input$Sex==1){
    return(59.81)
  }
  else if(input$CAMSISSoc==3531 & input$EStatus==2 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3531 & input$EStatus>=3 & input$Sex==1){
    return(56.29)
  }
  else if(input$CAMSISSoc==3531 & input$EStatus>=3 & input$Sex==2){
    return(57.12)
  }
  else if(input$CAMSISSoc==3532 & input$EStatus==1 & input$Sex==1){
    return(66.40)
  }
  else if(input$CAMSISSoc==3532 & input$EStatus==1 & input$Sex==2){
    return(60.42)
  }
  else if(input$CAMSISSoc==3532 & input$EStatus==2 & input$Sex==1){
    return(59.81)
  }
  else if(input$CAMSISSoc==3532 & input$EStatus==2 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3532 & input$EStatus>=3 & input$Sex==1){
    return(57.48)
  }
  else if(input$CAMSISSoc==3532 & input$EStatus>=3 & input$Sex==2){
    return(60.21)
  }
  else if(input$CAMSISSoc==3533 & input$EStatus>=1 & input$Sex==1){
    return(58.68)
  }
  else if(input$CAMSISSoc==3533 & input$EStatus>=1 & input$Sex==2){
    return(56.58)
  }
  else if(input$CAMSISSoc==3534 & input$EStatus==1 & input$Sex==1){
    return(66.40)
  }
  else if(input$CAMSISSoc==3534 & input$EStatus==1 & input$Sex==2){
    return(59.04)
  }
  else if(input$CAMSISSoc==3534 & input$EStatus==2 & input$Sex==1){
    return(59.43)
  }
  else if(input$CAMSISSoc==3534 & input$EStatus==2 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3534 & input$EStatus>=3 & input$Sex==1){
    return(64.45)
  }
  else if(input$CAMSISSoc==3534 & input$EStatus>=3 & input$Sex==2){
    return(58.55)
  }
  else if(input$CAMSISSoc==3535 & input$EStatus==1 & input$Sex==1){
    return(66.40)
  }
  else if(input$CAMSISSoc==3535 & input$EStatus==1 & input$Sex==2){
    return(58.98)
  }
  else if(input$CAMSISSoc==3535 & input$EStatus==2 & input$Sex==1){
    return(59.43)
  }
  else if(input$CAMSISSoc==3535 & input$EStatus==2 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3535 & input$EStatus>=3 & input$Sex==1){
    return(64.45)
  }
  else if(input$CAMSISSoc==3535 & input$EStatus>=3 & input$Sex==2){
    return(58.55)
  }
  else if(input$CAMSISSoc==3536 & input$EStatus==1 & input$Sex==1){
    return(59.45)
  }
  else if(input$CAMSISSoc==3536 & input$EStatus>=1 & input$Sex==2){
    return(58.52)
  }
  else if(input$CAMSISSoc==3536 & input$EStatus==2 & input$Sex==1){
    return(59.43)
  }
  else if(input$CAMSISSoc==3536 & input$EStatus==3 & input$Sex==1){
    return(59.46)
  }
  else if(input$CAMSISSoc==3536 & input$EStatus==4 & input$Sex==1){
    return(59.45)
  }
  else if(input$CAMSISSoc==3536 & input$EStatus==5 & input$Sex==1){
    return(59.46)
  }
  else if(input$CAMSISSoc==3537 & input$EStatus==1 & input$Sex==1){
    return(59.45)
  }
  else if(input$CAMSISSoc==3537 & input$EStatus==1 & input$Sex==2){
    return(58.52)
  }
  else if(input$CAMSISSoc==3537 & input$EStatus==2 & input$Sex==1){
    return(59.43)
  }
  else if(input$CAMSISSoc==3537 & input$EStatus==2 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3537 & input$EStatus>=3 & input$Sex==1){
    return(59.46)
  }
  else if(input$CAMSISSoc==3537 & input$EStatus>=3 & input$Sex==2){
    return(58.52)
  }
  else if(input$CAMSISSoc==3538 & input$EStatus==1 & input$Sex==1){
    return(66.40)
  }
  else if(input$CAMSISSoc==3538 & input$EStatus==1 & input$Sex==2){
    return(60.46)
  }
  else if(input$CAMSISSoc==3538 & input$EStatus==2 & input$Sex==1){
    return(59.43)
  }
  else if(input$CAMSISSoc==3538 & input$EStatus==2 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3538 & input$EStatus>=3 & input$Sex==1){
    return(62.92)
  }
  else if(input$CAMSISSoc==3538 & input$EStatus>=3 & input$Sex==2){
    return(60.39)
  }
  else if(input$CAMSISSoc==3539 & input$EStatus==1 & input$Sex==1){
    return(66.40)
  }
  else if(input$CAMSISSoc==3539 & input$EStatus==1 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3539 & input$EStatus==2 & input$Sex==1){
    return(59.43)
  }
  else if(input$CAMSISSoc==3539 & input$EStatus==2 & input$Sex==2){
    return(63.70)
  }
  else if(input$CAMSISSoc==3539 & input$EStatus>=3 & input$Sex==1){
    return(59.46)
  }
  else if(input$CAMSISSoc==3539 & input$EStatus>=3 & input$Sex==2){
    return(58.52)
  }
  else if(input$CAMSISSoc==3541 & input$EStatus==1 & input$Sex==1){
    return(48.78)
  }
  else if(input$CAMSISSoc==3541 & input$EStatus==1 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3541 & input$EStatus==2 & input$Sex==1){
    return(48.78)
  }
  else if(input$CAMSISSoc==3541 & input$EStatus==2 & input$Sex==2){
    return(57.20)
  }
  else if(input$CAMSISSoc==3541 & input$EStatus>=3 & input$Sex==1){
    return(58.91)
  }
  else if(input$CAMSISSoc==3541 & input$EStatus>=3 & input$Sex==2){
    return(57.07)
  }
  else if(input$CAMSISSoc==3542 & input$EStatus==1 & input$Sex==1){
    return(48.78)
  }
  else if(input$CAMSISSoc==3542 & input$EStatus==1 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3542 & input$EStatus==2 & input$Sex==1){
    return(48.78)
  }
  else if(input$CAMSISSoc==3542 & input$EStatus==2 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3542 & input$EStatus>=3 & input$Sex==1){
    return(51.55)
  }
  else if(input$CAMSISSoc==3542 & input$EStatus>=3 & input$Sex==2){
    return(49.36)
  }
  else if(input$CAMSISSoc==3543 & input$EStatus==1 & input$Sex==1){
    return(67.52)
  }
  else if(input$CAMSISSoc==3543 & input$EStatus==1 & input$Sex==2){
    return(61.04)
  }
  else if(input$CAMSISSoc==3543 & input$EStatus==2 & input$Sex==1){
    return(67.52)
  }
  else if(input$CAMSISSoc==3543 & input$EStatus==2 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3543 & input$EStatus>=3 & input$Sex==1){
    return(61.83)
  }
  else if(input$CAMSISSoc==3543 & input$EStatus>=3 & input$Sex==2){
    return(60.07)
  }
  else if(input$CAMSISSoc==3544 & input$EStatus==1 & input$Sex==1){
    return(65.10)
  }
  else if(input$CAMSISSoc==3544 & input$EStatus==1 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3544 & input$EStatus==2 & input$Sex==1){
    return(65.10)
  }
  else if(input$CAMSISSoc==3544 & input$EStatus==2 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3544 & input$EStatus>=3 & input$Sex==1){
    return(56.05)
  }
  else if(input$CAMSISSoc==3544 & input$EStatus>=3 & input$Sex==2){
    return(54.86)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus==1 & input$Sex==1){
    return(65.10)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus==1 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus==2 & input$Sex==1){
    return(65.10)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus==2 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus==3 & input$Sex==1){
    return(58.65)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus==3 & input$Sex==2){
    return(63.15)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus>=4 & input$Sex==1){
    return(61.33)
  }
  else if(input$CAMSISSoc==3545 & input$EStatus>=4 & input$Sex==2){
    return(63.82)
  }
  else if(input$CAMSISSoc==3546 & input$EStatus==1 & input$Sex==1){
    return(65.10)
  }
  else if(input$CAMSISSoc==3546 & input$EStatus==1 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3546 & input$EStatus==2 & input$Sex==1){
    return(65.10)
  }
  else if(input$CAMSISSoc==3546 & input$EStatus==2 & input$Sex==2){
    return(69.47)
  }
  else if(input$CAMSISSoc==3546 & input$EStatus>=3 & input$Sex==1){
    return(58.79)
  }
  else if(input$CAMSISSoc==3546 & input$EStatus>=3 & input$Sex==2){
    return(51.59)
  }
  else if(input$CAMSISSoc==3550 & input$EStatus>=1 & input$Sex==1){
    return(55.63)
  }
  else if(input$CAMSISSoc==3550 & input$EStatus>=1 & input$Sex==2){
    return(58.83)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus==1 & input$Sex==1){
    return(57.33)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus==1 & input$Sex==2){
    return(52.15)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus==2 & input$Sex==1){
    return(57.33)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus==2 & input$Sex==2){
    return(48.24)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus==3 & input$Sex==1){
    return(63.46)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus==3 & input$Sex==2){
    return(59.38)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus>=4 & input$Sex==1){
    return(57.33)
  }
  else if(input$CAMSISSoc==3561 & input$EStatus>=4 & input$Sex==2){
    return(48.24)
  }
  else if(input$CAMSISSoc==3562 & input$EStatus>=1 & input$Sex==1){
    return(55.59)
  }
  else if(input$CAMSISSoc==3562 & input$EStatus>=1 & input$Sex==2){
    return(60.82)
  }
  else if(input$CAMSISSoc==3563 & input$EStatus==1 & input$Sex==1){
    return(67.22)
  }
  else if(input$CAMSISSoc==3563 & input$EStatus==1 & input$Sex==2){
    return(61.21)
  }
  else if(input$CAMSISSoc==3563 & input$EStatus==2 & input$Sex==1){
    return(67.22)
  }
  else if(input$CAMSISSoc==3563 & input$EStatus==2 & input$Sex==2){
    return(61.21)
  }
  else if(input$CAMSISSoc==3563 & input$EStatus>=3 & input$Sex==1){
    return(54.73)
  }
  else if(input$CAMSISSoc==3563 & input$EStatus>=3 & input$Sex==2){
    return(54.14)
  }
  else if(input$CAMSISSoc==3564 & input$EStatus>=1 & input$Sex==1){
    return(67.14)
  }
  else if(input$CAMSISSoc==3564 & input$EStatus>=1 & input$Sex==2){
    return(51.46)
  }
  else if(input$CAMSISSoc==3565 & input$EStatus>=1 & input$Sex==1){
    return(50.24)
  }
  else if(input$CAMSISSoc==3565 & input$EStatus>=1 & input$Sex==2){
    return(56.54)
  }
  else if(input$CAMSISSoc==3567 & input$EStatus>=1 & input$Sex==1){
    return(55.16)
  }
  else if(input$CAMSISSoc==3567 & input$EStatus>=1 & input$Sex==2){
    return(58.15)
  }
  
  
  
  
}