# Load R packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(ggforce)
library(scales)
library(ggplot2)

  # Define UI
  ui <- fluidPage(theme = shinytheme("cosmo"),
                  textOutput("narrative"),
                  mainPanel(navbarPage('Class Calculator', id = "inTabset", 
                                       tags$figure(
                                         class = "centerFigure",
                                         tags$img(src = "edinburgh.png", style="float:right; padding-right:25px",
                                                  width = 300,
                                                  alt = "Picture of Edinburgh University Logo")),
                                       tabPanel(title="Class Schemas", value = "panel1",
                                                tags$h3("Schema Selector"),
                                                materialSwitch("Classic", "Great British Class Survey",
                                                               right = TRUE),
                                                materialSwitch("Wrights", "Neo-Marxian Simplified",
                                                               right = TRUE),
                                                materialSwitch("NSSEC", "NS-SEC",
                                                               right = TRUE),
                                                tags$h3("What do each of these schemas mean?"),
                                                  tags$h4("What is the Great British Class Survey?"),
                                                  "The Great British Class Schema is in part a response to traiditional conceptualisations of social class that do not effectively capture the role of social and cultural processes in generating class divisions", tags$a(href= "https://doi.org/10.1177/0038038513481128", "(Savage et al 2013)"), "The GBCS derives seven classes that are indicative of combined social, cultural, and economic capital.",
                                                
                                                  tags$h4("What is the Wright Simplified Schema?"),
                                                  "Erik Olin Wright’s model of social stratification comes through an attempt to demonstrate social classes capacity to reveal the underlying dynamics of social processes of exploitation", tags$a(href= "https://www.sscc.wisc.edu/soc/faculty/pages/wright/Published%20writing/Class%20Strucutre%20and%20Income%20Determination-text.pdf", "(Wright 1979.)"), "Unlike Weberian concepts of class, Wright's schema goes beyond the conceptual argument.",
                                                
                                                  tags$h4("What is the NS-SEC Schema?"),
                                                  "The National Statistics Socio-economic classification (NS-SEC) and seeks to measure the employment relations and conditions of occupations", tags$a(href= "https://doi.org/10.1177/0950017016653087", "(Williams 2017)."), "It is is the official socio-economic classification in the United Kingdom.",
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                ),
                                       
                                       
                                       tabPanel(title="GBCS", value = "panel2",
                                                conditionalPanel(
                                                  condition = "input.Classic == '1'",
                                                  tags$h3("What is your annual household income after taxes?"),
                                                radioButtons("income", p("Total income for you/spouse/significant other"),
                                                             choices = list("Under £10k"=0.08, "£10-£25k"=0.175, "£25-50k"=0.375, "£50-£100k"=0.6, "Over £100k"=1), selected = 0.08),
                                                
                                                radioButtons("home", h3("Do you own or rent a property?"),
                                                             choices = list("Own"=1, "Rent"=0), selected = 0),
                                                conditionalPanel(
                                                  condition = "input.home == '1'", 
                                                  radioButtons("homevalue", p("Value of all property owned/mortgaged by you/spouse/significant other"),
                                                               choices = list("Under £125k"=1, "£125-250k"=2, "£250-500k"=3, "Over £500k"=4, "N/A" = 0), selected = 0)),
                                                
                                                tags$h3("Do you have any savings?"),
                                                radioButtons("savings", p("Pensions, shares, ISAs etc"),
                                                             choices= list("None"=0, "£0-10k"=0.05, "£10-25k"=0.10, "£25-50k"=0.20, "£50-100k"=0.40, "Over £100k"=1), selected = 0),
                                                tags$h3("Which of these people of you know socially?"),
                                                checkboxGroupInput("social",
                                                                   p("Select all the people who you know"),
                                                                   choices = list("Secretary"=47.49,
                                                                                  "Nurse"=36.78,
                                                                                  "Teacher"=76.46,
                                                                                  "Cleaner"=10.73,
                                                                                  "University Lecturer"=85.30,
                                                                                  "Artist"=42.95,
                                                                                  "Electrician"=24.44,
                                                                                  "Office Manager"=59.30,
                                                                                  "Solicitor"=75.12,
                                                                                  "Farm Worker"=14.17,
                                                                                  "Chief Executive"=70.82,
                                                                                  "Software Designer"=51.48,
                                                                                  "Call Centre Worker"=27.96,
                                                                                  "Postal Worker"=23.06,
                                                                                  "Scientist"=68.77,
                                                                                  "Lorry Driver"=14.50,
                                                                                  "Accountant"=57.50,
                                                                                  "Shop Assistant"=31.32)),
                                                tags$h3("Which of these cultural activities do you take part in?"),
                                                checkboxGroupInput("high",
                                                                   p("Select all the activities you do sometimes or often"),
                                                                   choices = list("Go to stately homes"=1,
                                                                                  "Go to the opera"=1,
                                                                                  "Listen to jazz"=1,
                                                                                  "Go to the theatre"=1,
                                                                                  "Go to museums/galleries"=1,
                                                                                  "Listen to classical music"=1,
                                                                                  "Do arts and crafts"=1,
                                                                                  "Watch dance or ballet"=1)
                                                ),
                                                checkboxGroupInput("emerging",
                                                                   p(""),
                                                                   choices = list("Listen to rock/indie"=1,
                                                                                  "Go to gigs"=1,
                                                                                  "Play video games"=1,
                                                                                  "Watch sports"=1,
                                                                                  "Excercise/go to gym"=1,
                                                                                  "Use Facebook/Twitter"=1,
                                                                                  "Socialise at home"=1,
                                                                                  "Listen to hip-hop/rap"=1)),
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                       ),
                                       ),
                                       tabPanel(title= "Wright Simplified", value= "panel3",
                                                conditionalPanel(
                                                  condition = "input.Wrights == '1'",
                                                  tags$h3("Employment Status"),
                                                  radioButtons("SelfEmployments", p("Are you employed by someone else or are you self-employed?"),
                                                               choices= list("Employed"=1, "Self-Employed"=2), selected =0),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.SelfEmployments =='2'",
                                                    radioButtons("WNoEs", p("About how many people are employed in this business on a permanent basis?"),
                                                                 choices= list("1"=1,"2-10"=2, "10+"=3, "N/A"=4), selected =4)),
                                                  tags$h3("Decision Making"),
                                                  radioButtons("D1s", p("Do you participate in making, or giving advice around policy related decisions in the workplace?"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  conditionalPanel(
                                                    condition = "input.D1s == '1'",
                                                    tags$h3("For each, say if you are personally involved in this decision, including providing advice on it"),
                                                    radioButtons("D2as", p("decisions to XXXX"),
                                                                 choices= list("Yes"=1, "No"=2), selected =2),
                                                    conditionalPanel(
                                                      condition = "input.D2as == '1'",
                                                      radioButtons("D2a1s", p("In which of the following ways did you usually participate in making the decision?"),
                                                                   choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                  ),
                                                  tags$h3("Supervision"),
                                                  radioButtons("C1s", p("As an official part of your main job do you supervise the work of other employees or tell other employees what work to do?"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  conditionalPanel(
                                                    condition = "input.hiddens == '1'",
                                                    radioButtons("hidden", p("this is for smooth logic tree function, ignore"),
                                                                 choices= list("1"=1), selected =1),
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.C1s == '1'",
                                                    radioButtons("C2s", p("How many people do you directly supervise?"),
                                                                 choices= list("1"=1, "More than 1"=2), selected =0),
                                                    radioButtons("C3s", p("Do any of your subordinates have subordinates under them?"),
                                                                 choices= list("Yes"=1, "No"=2), selected =0),
                                                    tags$h3("Are you directly responsible for the following?"),
                                                    radioButtons("C3as", p("deciding the specific tasks or work assignments performed by your subordinates, deciding what procedures, tools, or materials your subordinates use in doing their work, or deciding how fast your subordinates work, how long they work, or how much work they have to get done"),
                                                                 choices= list("Yes"=1, "No"=2), selected =0),
                                                    tags$h3("Do you have any influence over the following possible sanctions that could be imposed upon subordinates?"),
                                                    radioButtons("C4as", p("granting a pay rise or promotion to a subordinate, preventing a subordinate from getting a pay rise or promotion because of poor work or misbehaviour, Do you or someone higher up have the greatest influence in this decision?, firing or temporarily suspending a subordinate, or issuing a formal warning to a subordinate"),
                                                                 choices= list("Yes"=1, "No"=2), selected =0)),
                                                  conditionalPanel(
                                                    condition= "input.SelfEmployments =='1'",
                                                    tags$h3("Formal Hierarchical Position"),
                                                    radioButtons("H1s", p("Which of the following best describes the position which you hold within your organisation"),
                                                                 choices= list("Managerial Position"=1, "Supervisory Position"=2, "Non-Management Position"=3), selected =0)),
                                                  tags$h3("Occupation"),
                                                  radioButtons("O1s", p("What is your Occupation?"),
                                                               choices= list("Professionals"=1, "Professors"=2, "Managers"=3, "School Teachers"=4, "Craftworkers"=5, "Technicians"=6, "Sales"=7, "Clerical"=8, "Manual non-crafts"=9), selected =0),
                                                  tags$h3("Education"),
                                                  radioButtons("E1s", p("What is your highest level of Education?"),
                                                               choices= list("B.A or more"=1, "Less than a B.A"=2), selected =0),
                                                  conditionalPanel(
                                                    condition= "input.SelfEmployments == '1'",
                                                    tags$h3("Autonomy"),
                                                    radioButtons("B1s", p("Is yours a job in which you are required to design important aspects of your own work and to put your ideas into practice?"),
                                                                 choices= list("Yes"=1, "No"=2), selected=0),
                                                  ),
                                                ),
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                       ),
                                       tabPanel(title= "NS-SEC", value= "panel4",
                                                conditionalPanel(
                                                  condition = "input.NSSEC == '1'",
                                                  tags$h3("Employee or Self-Employed"),
                                                  radioButtons("Employed", p("Do you work as an employee or are you self-employed?"),
                                                               choices= list("Employee"=1, "Self-Employed with employees"=2, "Self-Employed without employees"=3), selected = 0),
                                                  tags$h3("Number of Employees"),
                                                  radioButtons("NoEmployees", p("How many people worked for your employer at the place where you worked? Were there..."),
                                                               choices= list("1-24"=1, "25-499"=2, "500+"=3, "None"=4), selected = 0),
                                                  tags$h3("Supervisory Status"),
                                                  radioButtons("Supervisory", p("In your job, did you have any formal responsibility for supervising the work of other employees?"),
                                                               choices= list("Yes"=1, "No"=2), selected = 0),
                                                  tags$h3("What is your occupation?"),
                                                  p("If you are having trouble finding your exact occupation, type in 'n.e.c' and select the closest general occupational grouping that matches your own"),
                                                  
                                                  pickerInput(
                                                    inputId = "IdSoc",
                                                    label = "SOC2010 Code Search",
                                                    choices = list("Chief executives and senior officials"=1115, "Elected officers and representatives"=1116, "Production managers and directors in manufacturing"=1121, "Production managers and directors in construction"=1122, "Production managers and directors in mining and energy"=1123, "Financial managers and directors"=1131, "Marketing and sales directors"=1132, "Purchasing managers and directors"=1133, "Advertising and public relations directors"=1134, "Human resource managers and directors"=1135, "Information technology and telecommunications directors"=1136, "Functional managers and directors n.e.c."=1139, "Financial institution managers and directors"=1150, "Managers and directors in transport and distribution"=1161, "Managers and directors in storage and warehousing"=1162, "Officers in armed forces"=1171, "Senior police officers"=1172, "Senior officers in fire, ambulance, prison and related services"=1173, "Health services and public health managers and directors"=1181, "Social services managers and directors"=1184, "Managers and directors in retail and wholesale"=1190, "Managers and proprietors in agriculture and horticulture"=1211, "Managers and proprietors in forestry, fishing and related services"=1213, "Hotel and accommodation managers and proprietors"=1221, "Restaurant and catering establishment managers and proprietors"=1223, "Publicans and managers of licensed premises"=1224, "Leisure and sports managers"=1225, "Travel agency managers and proprietors"=1226, "Health care practice managers"=1241, "Residential, day and domiciliary care managers and proprietors"=1242, "Property, housing and estate managers"=1251, "Garage managers and proprietors"=1252, "Hairdressing and beauty salon managers and proprietors"=1253, "Shopkeepers and proprietors – wholesale and retail"=1254, "Waste disposal and environmental services managers"=1255, "Managers and proprietors in other services n.e.c."=1259, "Chemical scientists"=2111, "Biological scientists and biochemists"=2112, "Physical scientists"=2113, "Social and humanities scientists"=2114, "Natural and social science professionals n.e.c."=2119, "Civil engineers"=2121, "Mechanical engineers"=2122, "Electrical engineers"=2123, "Electronics engineers"=2124, "Design and development engineers"=2126, "Production and process engineers"=2127, "Engineering professionals n.e.c."=2129, "IT specialist managers"=2133, "IT project and programme managers"=2134, "IT business analysts, architects and systems designers"=2135, "Programmers and software development professionals"=2136, "Web design and development professionals"=2137, "Information technology and telecommunications professionals n.e.c."=2139, "Conservation professionals"=2141, "Environment professionals"=2142, "Research and development managers"=2150, "Medical practitioners"=2211, "Psychologists"=2212, "Pharmacists"=2213, "Ophthalmic opticians"=2214, "Dental practitioners"=2215, "Veterinarians"=2216, "Medical radiographers"=2217, "Podiatrists"=2218, "Health professionals n.e.c."=2219, "Physiotherapists"=2221, "Occupational therapists"=2222, "Speech and language therapists"=2223, "Therapy professionals n.e.c."=2229, "Nurses"=2231, "Midwives"=2232, "Higher education teaching professionals"=2311, "Further education teaching professionals"=2312, "Secondary education teaching professionals"=2314, "Primary and nursery education teaching professionals"=2315, "Special needs education teaching professionals"=2316, "Senior professionals of educational establishments"=2317, "Education advisers and school inspectors"=2318, "Teaching and other educational professionals n.e.c."=2319, "Barristers and judges"=2413, "Legal professionals n.e.c."=2419, "Chartered and certified accountants"=2421, "Management consultants and business analysts"=2423, "Business and financial project management professionals"=2424, "Actuaries, economists and statisticians"=2425, "Business and related research professionals"=2426, "Business, research and administrative professionals n.e.c."=2429, "Architects"=2431, "Town planning officers"=2432, "Quantity surveyors"=2433, "Chartered surveyors"=2434, "Chartered architectural technologists"=2435, "Construction project managers and related professionals"=2436, "Social workers"=2442, "Probation officers"=2443, "Clergy"=2444, "Welfare professionals n.e.c."=2449, "Librarians"=2451, "Archivists and curators"=2452, "Quality control and planning engineers"=2461, "Quality assurance and regulatory professionals"=2462, "Environmental health professionals"=2463, "Journalists, newspaper and periodical editors"=2471, "Public relations professionals"=2472, "Advertising accounts managers and creative directors"=2473, "Laboratory technicians"=3111, "Electrical and electronics technicians"=3112, "Engineering technicians"=3113, "Building and civil engineering technicians"=3114, "Quality assurance technicians"=3115, "Planning, process and production technicians"=3116, "Science, engineering and production technicians n.e.c."=3119, "Architectural and town planning technicians"=3121, "Draughtspersons"=3122, "IT operations technicians"=3131, "IT user support technicians"=3132, "Paramedics"=3213, "Dispensing opticians"=3216, "Pharmaceutical technicians"=2317, "Medical and dental technicians"=3218, "Health associate professionals n.e.c."=3219, "Youth and community workers"=3231, "Child and early years officers"=3233, "Housing officers"=3234, "Counsellors"=3235, "Welfare and housing associate professionals n.e.c."=3239, "NCOs and other ranks"=3311, "Police officers (sergeant and below)"=3312, "Fire service officers (watch manager and below)"=3313, "Prison service officers (below principal officer)"=3314, "Police community support officers"=3315, "Protective service associate professionals n.e.c."=3319, "Artists"=3411, "Authors, writers and translators"=3412, "Actors, entertainers and presenters"=3413, "Dancers and choreographers"=3414, "Musicians"=3415, "Arts officers, producers and directors"=3416, "Photographers, audio-visual and broadcasting equipment operators"=3417, "Graphic designers"=3421, "Product, clothing and related designers"=3422, "Sports players"=3441, "Sports coaches, instructors and officials"=3442, "Fitness instructors"=3443, "Air traffic controllers"=3511, "Aircraft pilots and flight engineers"=3512, "Ship and hovercraft officers"=3513, "Legal associate professionals"=3520, "Estimators, valuers and assessors"=3531, "Brokers"=3532, "Insurance underwriters"=3533, "Finance and investment analysts and advisers"=3534, "Taxation experts"=3535, "Importers and exporters"=3536, "Financial and accounting technicians"=3537, "Financial accounts managers"=3538, "Business and related associate professionals n.e.c."=3539, "Buyers and procurement officers"=3541, "Business sales executives"=3542, "Marketing associate professionals"=3543, "Estate agents and auctioneers"=3544, "Sales accounts and business development managers"=3545, "Conference and exhibition managers and organisers"=3546, "Conservation and environmental associate professionals"=3550, "Public services associate professionals"=3561, "Human resources and industrial relations officers"=3562, "Vocational and industrial trainers and instructors"=3563, "Careers advisers and vocational guidance specialists"=3564, "Inspectors of standards and regulations"=3565, "Health and safety officers"=3567, "National government administrative occupations"=4112, "Local government administrative occupations"=4113, "Officers of non-governmental organisations"=4114, "Credit controllers"=4121, "Book-keepers, payroll managers and wages clerks"=4122, "Bank and post office clerks"=4123, "Finance officers"=4124, "Financial administrative occupations n.e.c."=4129, "Records clerks and assistants"=4131, "Pensions and insurance clerks and assistants"=4132, "Stock control clerks and assistants"=4133, "Transport and distribution clerks and assistants"=4134, "Library clerks and assistants"=4135, "Human resources administrative occupations"=4138, "Sales administrators"=4151, "Other administrative occupations n.e.c."=4159, "Office managers"=4161, "Office supervisors"=4162, "Medical secretaries"=4211, "Legal secretaries"=4212, "School secretaries"=4213, "Company secretaries"=4214, "Personal assistants and other secretaries"=4215, "Receptionists"=4216, "Typists and related keyboard occupations"=4217, "Farmers"=5111, "Horticultural trades"=5112, "Gardeners and landscape gardeners"=5113, "Groundsmen and greenkeepers"=5114, "Agricultural and fishing trades n.e.c."=5119, "Smiths and forge workers"=5211, "Moulders, core makers and die casters"=5212, "Sheet metal workers"=5213, "Metal plate workers and riveters"=5214, "Welding trades"=5215, "Pipe fitters"=5216, "Metal machining setters and setter-operators"=5221, "Tool makers, tool fitters and markers-out"=5222, "Metal working production and maintenance fitters"=5223, "Precision instrument makers and repairers"=5224, "Air-conditioning and refrigeration engineers"=5225, "Vehicle technicians, mechanics and electricians"=5231, "Vehicle body builders and repairers"=5232, "Vehicle paint technicians"=5234, "Aircraft maintenance and related trades"=5235, "Boat and ship builders and repairers"=5236, "Rail and rolling stock builders and repairers"=5237, "Electricians and electrical fitters"=5241, "Telecommunications engineers"=5242, "TV, video and audio engineers"=5244, "IT engineers"=5245, "Electrical and electronic trades n.e.c."=5249, "Skilled metal, electrical and electronic trades supervisors"=5230, "Steel erectors"=5311, "Bricklayers and masons"=5312, "Roofers, roof tilers and slaters"=5313, "Plumbers and heating and ventilating engineers"=5314, "Carpenters and joiners"=5315, "Glaziers, window fabricators and fitters"=5316, "Construction and building trades n.e.c."=5319, "Plasterers"=5321, "Floorers and wall tilers"=5322, "Painters and decorators"=5323, "Construction and building trades supervisors"=5330, "Weavers and knitters"=5411, "Upholsterers"=5412, "Footwear and leather working trades"=5413, "Tailors and dressmakers"=5414, "Textiles, garments and related trades n.e.c."=5419, "Pre-press technicians"=5421, "Printers"=5422, "Print finishing and binding workers"=5423, "Butchers"=5431, "Bakers and flour confectioners"=5432, "Fishmongers and poultry dressers"=5433, "Chefs"=5434, "Cooks"=5435, "Catering and bar managers"=5436, "Glass and ceramics makers, decorators and finishers"=5441, "Furniture makers and other craft woodworkers"=5442, "Florists"=5443, "Other skilled trades n.e.c."=5449, "Nursery nurses and assistants"=6121, "Childminders and related occupations"=6122, "Playworkers"=6123, "Teaching assistants"=6125, "Educational support assistants"=6126, "Veterinary nurses"=6131, "Pest control officers"=6132, "Animal care services occupations n.e.c."=6139, "Nursing auxiliaries and assistants"=6141, "Ambulance staff (excluding paramedics)"=6142, "Dental nurses"=6143, "Houseparents and residential wardens"=6144, "Care workers and home carers"=6145, "Senior care workers"=6146, "Care escorts"=6147, "Undertakers, mortuary and crematorium assistants"=6148, "Sports and leisure assistants"=6211, "Travel agents"=6212, "Air travel assistants"=6214, "Rail travel assistants"=6215, "Leisure and travel service occupations n.e.c."=6219, "Hairdressers and barbers"=6221, "Beauticians and related occupations"=6222, "Housekeepers and related occupations"=6223, "Caretakers"=6232, "Cleaning and housekeeping managers and supervisors"=6240, "Sales and retail assistants"=7111, "Retail cashiers and check-out operators"=7112, "Telephone salespersons"=7113, "Pharmacy and other dispensing assistants"=7114, "Vehicle and parts salespersons and advisers"=7115, "Collector salespersons and credit agents"=7121, "Debt, rent and other cash collectors"=7122, "Roundspersons and van salespersons"=7123, "Market and street traders and assistants"=7124, "Merchandisers and window dressers"=7125, "Sales related occupations n.e.c."=7129, "Sales supervisors"=7130, "Call and contact centre occupations"=7211, "Telephonists"=7213, "Communication operators"=7214, "Market research interviewers"=7215, "Customer service occupations n.e.c."=7219, "Customer service managers and supervisors"=7220, "Food, drink and tobacco process operatives"=8111, "Glass and ceramics process operatives"=8112, "Textile process operatives"=8113, "Chemical and related process operatives"=8114, "Rubber process operatives"=8115, "Plastics process operatives"=8116, "Metal making and treating process operatives"=8117, "Electroplaters"=8118, "Process operatives n.e.c."=8119, "Paper and wood machine operatives"=8121, "Coal mine operatives"=8122, "Quarry workers and related operatives"=8123, "Energy plant operatives"=8124, "Metal working machine operatives"=8125, "Water and sewerage plant operatives"=8126, "Printing machine assistants"=8127, "Plant and machine operatives n.e.c."=8129, "Assemblers (electrical and electronic products)"=8131, "Assemblers (vehicles and metal goods)"=8132, "Routine inspectors and testers"=8133, "Weighers, graders and sorters"=8134, "Tyre, exhaust and windscreen fitters"=8135, "Sewing machinists"=8137, "Assemblers and routine operatives n.e.c."=8139, "Scaffolders, stagers and riggers"=8141, "Road construction operatives"=8142, "Rail construction and maintenance operatives"=8143, "Construction operatives n.e.c."=8149, "Large goods vehicle drivers"=8211, "Van drivers"=8212, "Bus and coach drivers"=8213, "Taxi and cab drivers and chauffeurs"=8214, "Driving instructors"=8215, "Crane drivers"=8221, "Fork-lift truck drivers"=8222, "Agricultural machinery drivers"=8223, "Mobile machine drivers and operatives n.e.c."=8229, "Train and tram drivers"=8231, "Marine and waterways transport operatives"=8232, "Air transport operatives"=8233, "Rail transport operatives"=8234, "Other drivers and transport operatives n.e.c."=8239, "Farm workers"=9111, "Fishing and other elementary agriculture occupations n.e.c."=9119, "Elementary construction occupations"=9120, "Industrial cleaning process occupations"=9132, "Packers, bottlers, canners and fillers"=9134, "Elementary process plant occupations n.e.c."=9139, "Postal workers, mail sorters, messengers and couriers"=9211, "Elementary administration occupations n.e.c."=9219, "Window cleaners"=9231, "Street cleaners"=9232, "Cleaners and domestics"=9233, "Launderers, dry cleaners and pressers"=9234, "Refuse and salvage occupations"=9235, "Vehicle valeters and cleaners"=9236, "Elementary cleaning occupations n.e.c."=9239, "Security guards and related occupations"=9241, "Parking and civil enforcement occupations"=9242, "School mid-day and crossing patrol occupations"=9244, "Elementary security occupations n.e.c."=9249, "Shelf fillers"=9251, "Elementary sales occupations n.e.c."=9259, "Elementary storage occupations"=9260, "Hospital porters"=9271, "Kitchen and catering assistants"=9272, "Waiters and waitresses"=9273, "Bar staff"=9274, "Leisure and theme park attendants"=9275, "Other elementary services occupations n.e.c."=9279),
                                                    options = list(
                                                      `live-search` = TRUE)
                                                  )
                                                ),
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                ),
                                       tabPanel(title= "Your Result", value= "panel5",
                                                conditionalPanel(
                                                  condition = "input.Classic == '1'",
                                                  tags$h3("Results"),
                                                  p("Your Social Class as Determined by the GBCS is:"),
                                                  verbatimTextOutput("GBCS"),
                                                ),
                                                conditionalPanel(
                                                  condition = "input.Wrights == '1'",
                                                  p("Your Social Class as Determined by Wright Simplified is:"),
                                                  verbatimTextOutput("wrights"),
                                                ),
                                                conditionalPanel(
                                                  condition = "input.NSSEC == '1'",
                                                  p("Your Social Class as Determined by NSSEC is:"),
                                                  verbatimTextOutput("nssecsc"),
                                                ),
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                ),
                                       tabPanel(title= "About", value= "panel6",
                                                tags$h3("About Page"),
                                                p("The Social Class Calculator is a project developed by a team at the University of Edinburgh. The primary goals of this project is to demonstrate open science reserach practices in re-creating the Great British Class Survey. Through this recreation, addtional social class schemas were added. Through this addition, this Social Class Calculator serves as a viable teaching tool for students of Sociology in learning abour social class concepts. With its open source format, this Social Class Calculator also serves as a teaching tool for students and teachers alike wishing to design effective survey tools/constructions using Shiny in the R programming language. For a full breakdown of the project and the code used, see the project GitHub page:"),
                                                tags$a(href="https://github.com/EOSSCC/Class-Calculator", "Click here to access project GitHub page"),
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                
                                       
                  )
                  
                  )
  )
  )                              
                  
  

server <- function(input, output, session) {
  # This section covers the toggling of different social class schemas #
  observeEvent(input$Classic, {
    if(input$Classic ==0){
      hideTab(inputId = "inTabset", target = "panel2")
    }})
  observeEvent(input$Classic, {
    if(input$Classic ==1){
      showTab(inputId = "inTabset", target = "panel2")
    }})
  
  observeEvent(input$Wrights, {
    if(input$Wrights ==0){
      hideTab(inputId = "inTabset", target = "panel3")
    }})
  observeEvent(input$Wrights, {
    if(input$Wrights ==1){
      showTab(inputId = "inTabset", target = "panel3")
    }})
  
  observeEvent(input$NSSEC, {
    if(input$NSSEC ==0){
      hideTab(inputId = "inTabset", target = "panel4")
    }})
  observeEvent(input$NSSEC, {
    if(input$NSSEC ==1){
      showTab(inputId = "inTabset", target = "panel4")
    }})
  
  
  # This section covers the logic tree output text for the GBCS Classic schema #
  output$GBCS <- renderText({
    social.n <- length(input$social)
    high.n <- length(input$high)
    emerging.n <- length(input$emerging)
    if (input$income ==1) {
      result <- "Elite"
    }
    else if (input$income ==0.6 & social.n>=6 & mean(as.numeric(input$social >=58))) {
      result <- "Elite"
    }
    else if (input$income ==0.375 & input$home >=0 & input$savings ==1) {
      result <- "Elite"
    }
    else if (input$income ==0.6 & social.n<6) {
      result <- "Technical Middle"
    }
    else if (input$income ==0.375 & input$home >0 & input$savings >0.05 & input$savings <1 & social.n <6) {
      result <- "Technical Middle"
    }
    else if (input$income ==0.6 & social.n>=6 & mean(as.numeric(input$social <58))) {
      result <- "Established Middle" 
    }
    else if (input$income ==0.375 & input$home >0 & input$savings <=0.05 & high.n>=3) {
      result <- "Established Middle" 
    }
    else if (input$income ==0.375 & input$home >0 & input$savings >=0.05 & input$savings <1 & social.n>=6) {
      result <- "Established Middle" 
    }
    else if (input$income ==0.375 & input$home ==0) {
      result <- "Emergent Worker"
    }
    else if (input$income ==0.175 & input$home ==0 & emerging.n>=4) {
      result <- "Emergent Worker"
    }
    else if (input$income ==0.08 & input$home ==0 & emerging.n>4) {
      result <- "Emergent Worker" 
    }
    else if (input$income ==0.08 & input$home ==0 & social.n>=6 & emerging.n==4) {
      result <- "Emergent Worker" 
    }
    else if (input$income ==0.375 & input$home >0 & input$savings <=0.05 & high.n<3) {
      result <- "Affluent Worker"
    }
    else if (input$income ==0.175 & input$home >0 & emerging.n>=3) {
      result <- "Affluent Worker" 
    }
    else if (input$income ==0.175 & input$home ==0 & emerging.n<4) {
      result <- "Precariat"
    }
    else if (input$income ==0.08 & input$home ==0 & emerging.n<4) {
      result <- "Precariat"
    }
    else if (input$income ==0.08 & input$home ==0 & social.n<6 & emerging.n==4) {
      result <- "Precariat"
    }
    else if (input$income ==0.175 & input$home >0 & emerging.n<3) {
      result <- "Traditional Worker"
    }
    else if (input$income ==0.08 & input$home >0) {
      result <- "Traditional Worker"
    }
    else {
      result <- "Not Enough Information"
    }
    return(result)
  }
  )
  
  
  # This section covers the Wright Simplified Schema # 
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
  
  output$wrights<-renderText({
    if (capitals(input) ==1){
      result<-"Bourgeoisie"
    }
    else if (capitals(input) ==2){
      result<-"Small Employers"
    }
    else if (capitals(input) ==3){
      result<-"Petty Bourgeoisie"
    }
    else if (capitals(input) ==4 & organisations(input) ==1 & skillss(input) ==1){
      result<-"Expert Managers"
    }
    else if (capitals(input) ==4 & organisations(input) ==2 & skillss(input) ==1){
      result<-"Expert Supervisors"
    }
    else if (capitals(input) ==4 & organisations(input) ==3 & skillss(input) ==1){
      result<-"Expert Non-Managers"
    }
    else if (capitals(input) ==4 & organisations(input) ==1 & skillss(input) ==2){
      result<-"Semi-Credentialled Managers"
    }
    else if (capitals(input) ==4 & organisations(input) ==2 & skillss(input) ==2){
      result<-"Semi-Credentialled Supervisors"
    }
    else if (capitals(input) ==4 & organisations(input) == 3 & skillss(input) ==2){
      result<-"Semi-Credentialled Workers"
    }
    else if (capitals(input) ==4 & organisations(input) ==1 & skillss(input) ==3){
      result<-"Uncredentialled Managers"
    }
    else if (capitals(input) ==4 & organisations(input) ==2 & skillss(input) ==3){
      result<-"Uncredentialled Supervisors"
    }
    else if (capitals(input) ==4 & organisations(input) ==3 & skillss(input) ==3){
      result<-"Proletarians"
    }
    else {
      result<-"Not Enough Information"
    }
    return(result)
  })
  
# This section covers the Full NS-SEC schema # 
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
    else if (input$IdSoc>=9231&&input$IdSoc<=9233 & status(input)>=4&&status(input)<=5){
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
  
  
  output$nssecsc<- renderText({
    if (nssecfull(input)==1.1){
      result<-"Large employers and higher managerial professional occupations"
    }
    else if (nssecfull(input)==1.2){
      result<-"Higher professional occupations"
    }
    else if (nssecfull(input)==2){
      result<-"Lower managerial and professional occupations"
    }
    else if (nssecfull(input)==3){
      result<-"Intermediate occupations"
    }
    else if (nssecfull(input)==4){
      result<-"Small employer and own account workers"
    }
    else if (nssecfull(input)==5){
      result<-"Lower supervisory and technical occupations"
    }
    else if (nssecfull(input)==6){
      result<-"Semi-routine occupations"
    }
    else if (nssecfull(input)==7){
      result<-"Routine occupations"
    }
    return(result)
  })
  
}


shinyApp(ui, server)
  
  
  
  
  
  