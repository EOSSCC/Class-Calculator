# Load R packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(ggforce)
library(scales)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyLPA)
library(BiocManager)
library(BiocVersion)
library(survey)
library(mitools)
library(convey)
library(pbapply)
library(haven)
library(mclust)
library(lavaan)

# Load Social Class Schema Source Files
source("GBCSF.R")
source("WrightF.R")
source("NSSECF.R")
source("CAMSISF.R")
source("RGSCF.R")
source("GBCSRF.R")
source("WrightFF.R")


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                
                mainPanel(navbarPage('Class Calculator', id = "inTabset", 
                                     tags$figure(
                                       class = "centerFigure",
                                       tags$img(src = "edinburgh.png", style="float:right; padding-right:25px",
                                                width = 300,
                                                alt = "Picture of Edinburgh University Logo")),
                                     tabPanel(title="Class Schemas", value = "panel1",
                                              tags$h3("Schema Selector"),
                                              materialSwitch("Classic", "BBC Classic",
                                                             right = TRUE),
                                              materialSwitch("GBCSR", "GBCS 2013",
                                                             right = TRUE),
                                              materialSwitch("WrightF", "Neo-Marxian Full",
                                                             right = TRUE),
                                              materialSwitch("Wrights", "Neo-Marxian Simplified",
                                                             right = TRUE),
                                              materialSwitch("NSSEC", "NS-SEC",
                                                             right = TRUE),
                                              materialSwitch("RGSC", "RGSC",
                                                             right = TRUE),
                                              materialSwitch("CAMSIS", "CAMSIS",
                                                             right = TRUE),
                                              actionButton("next_btn", "Next Page"),
                                              tags$h3("How to use the Class Calculator?"),
                                              "Select which class schema you wish to view and undertake using the designated sliders. There is a brief description of each schema below to aid in your decisions. After you have selected the desired schemas simply use the top banner to select the schema you wish to complete first. Go through all questions on each page until you reach the bottom of the page with no questions remaining. After you have done this, simply scroll to the top of the page and move on to the next class schema. If you have no class schemas left to select, simply click on the results page. Some class schemas may take a few seconds to calculate your results. Simply wait until the results are displayed. Each schema will present users with their class result, and a brief description of their given result. For more detailed descriptions of each indiviudal schema follow the respective references that are hosted and linked on this page. For any errors/issues/queries please see the about us page or contact the owners directly at: soatley@ed.ac.uk",
                                              
                                              tags$h3("What do each of these schemas mean?"),
                                              tags$h4("What is the BBC Classic?"),
                                              "The BBC Classic is the online version of the Great British Class Survey. This Schema is in part a response to traiditional conceptualisations of social class that do not effectively capture the role of social and cultural processes in generating class divisions", tags$a(href= "https://doi.org/10.1177/0038038513481128", "(Savage et al 2013)"), "The GBCS derives seven classes that are indicative of combined social, cultural, and economic capital.",
                                              
                                              tags$h4("What is the GBCS 2013 Schema?"),
                                              "This is an attempt to replicate the original GBCS as presented in the 2013 paper. This schema uses user inputs that are identical to that of the original 2013 GBCS. The GfK survey data contains 1026 observations, using the original methodology notebooks provided, a latent trait analysis is run in the background after a user hits 'submit', this includes the user data alongside the original Gfk data and produces a maximum probability statistic for the users class. This is an experimental version and can crash the app in given instances. Once the submit button is clicked, please wait for the latent trait analysis to run and it will then automatically take you to the results page. If you want to re-run this analysis, please refresh the page or it will crash the app.",
                                              
                                              tags$h4("What is the Neo-Marxian Full Schema?"),
                                              "Erik Olin Wright’s model of social stratification comes through an attempt to demonstrate social classes capacity to reveal the underlying dynamics of social processes of exploitation", tags$a(href= "https://www.sscc.wisc.edu/soc/faculty/pages/wright/Published%20writing/Class%20Strucutre%20and%20Income%20Determination-text.pdf", "(Wright 1979)."), "Unlike Weberian concepts of class, Wright's schema goes beyond the conceptual argument.",
                                              
                                              tags$h4("What is the Neo-Marxian Simplified Schema?"),
                                              "This is a simplified version of Wright's Neo-Marxian social class schema. This schema was developed specifically for the Class Calculator but can be used for statistical analysis.",
                                              
                                              tags$h4("What is the NS-SEC Schema?"),
                                              "The National Statistics Socio-economic classification (NS-SEC) and seeks to measure the employment relations and conditions of occupations", tags$a(href= "https://doi.org/10.1177/0950017016653087", "(Williams 2017)."), "It is is the official socio-economic classification in the United Kingdom.",
                                              
                                              tags$h4("What is the RGSC Schema?"),
                                              "An official scheme of class analysis used in British surveys and censuses for much of the 20th century. Was eventually dropped in favour of NS-SEC for official statistics.", tags$a(href= "https://sru.soc.surrey.ac.uk/SRU9.html", "(Rose 1995)."),
                                              
                                              tags$h4("What is CAMSIS?"),
                                              "The CAMSIS project is an internationally comparative assessment of the structures of social interaction and stratification across a number of countries. At its core lies the construction - and dissemination - of occupational scales for each constituent country. The scale values represent an occupational unit's relative position within the national order of social interaction and stratification.", tags$a(href= "https://www.camsis.stir.ac.uk/", "(Lambert 2018)."),
                                            
                                              
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: fixed; bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              )                                     ),
                                     
                                     
                                     tabPanel(title="BBC Classic", value = "panel2",
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
                                                actionButton("submitBBC", "Find Social Class"),
                                                
                                                tags$div(
                                                  p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                  style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                                )                                              ),
                                     ),
                                     tabPanel(title= "GBCS 2013", value= "panel3",
                                              conditionalPanel(
                                                condition = "input.GBCSR == '1'",
                                                sliderInput("incomer", label = h3("What is your annual household income after taxes? (In £s)"), min=0,
                                                            max=100, value=50),
                                                
                                                radioButtons("homer", h3("Do you own or rent a property?"),
                                                             choices = list("Own"=1, "Rent"=0), selected = 0),
                                                conditionalPanel(
                                                  condition = "input.homer == '1'",
                                                  sliderInput("homevaluer", label = h3("Value of all property owned/mortgaged by you/spouse/significant other (In £s)"), min=0,
                                                              max=500, value=250),
                                                ),
                                                tags$h3("Do you have any savings?"),
                                                sliderInput("savingsr", label = p("Pensions, shares, ISAs etc (In £s)"), min=0,
                                                            max=100, value=50),
                                                tags$h3("Which of these people of you know socially?"),
                                                checkboxGroupInput("socialr",
                                                                   p("Select all the people who you know"),
                                                                   choices = list("Secretary"=47.49,
                                                                                  "Travel Agent"=31.96,
                                                                                  "Clerical Officer in National or Local Government"=38.76,
                                                                                  "Finance Manager"=61.03,
                                                                                  "Bank or Building Society Manager"=60.89,
                                                                                  "Publican"=29.25,
                                                                                  "Restaurant Manager"=25.87,
                                                                                  "Factory Worker"=13.99,
                                                                                  "Receptionist"=37.91,
                                                                                  "Civil/Mechanical Engineer"=61.08,
                                                                                  "Medical Practitioner"=82.27,
                                                                                  "Gardener"=20.07,
                                                                                  "Train Driver"=4.48,
                                                                                  "Security Guard"=12.51,
                                                                                  "Machine Operator"=14.8,
                                                                                  "Armed Forces"=57.49,
                                                                                  "Catering Assistant"=17.38,
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
                                                radioButtons("carts", p("How often attended last year: Arts and crafts"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("cmusgall", p("How often attended last year: Museums and galleries"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("cstathom", p("How often attended last year: Visiting stately homes"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("ctheatre", p("How often attended last year: Theatre OR musicals"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("copera", p("How often attended last year: Opera"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("mclassic", p("Music preference: how much do you like classic?"),
                                                             choices = list("Dislike a lot"=1, "Dislike"=2, "Neither"=3, "Like"=4, "Like a lot"=5), selected=1),
                                                radioButtons("mjazz", p("Music preference: how much do you like jazz?"),
                                                             choices = list("Dislike a lot"=1, "Dislike"=2, "Neither"=3, "Like"=4, "Like a lot"=5), selected=1),
                                                radioButtons("ffrench", p("How often attended last year: French Restaurant"),
                                                             choices = list("Never"=0, "Rarely"=2, "Sometimes"=3, "Often"=4), selected=0),
                                                radioButtons("cdance", p("How often attended last year: Watch dance OR ballet"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                
                                                radioButtons("ccompgam", p("How often attended last year: Computer console games"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("csocnet", p("How often attended last year: Online social networks"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("csportp", p("How often attended last year: Playing sports"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("csportw", p("How often attended last year: Watch sports"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("cfriends", p("How often attended last year: Socialising with friends"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("cgym", p("How often attended last year: Sports club OR gym"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("cgig", p("How often attended last year: Modern music gigs"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("mrap", p("Music preference: how much do you like rap?"),
                                                             choices = list("Dislike a lot"=1, "Dislike"=2, "Neither"=3, "Like"=4, "Like a lot"=5), selected=1),
                                                radioButtons("cnet", p("How often attended last year: Surfing the internet"),
                                                             choices = list("Never"=0, "Rarely"=1, "Sometimes"=2, "Often"=3), selected=0),
                                                radioButtons("mrock", p("Music preference: how much do you like rock?"),
                                                             choices = list("Dislike a lot"=1, "Dislike"=2, "Neither"=3, "Like"=4, "Like a lot"=5), selected=1),
                                                actionButton("submit", "Find Social Class"),
                                                tags$div(
                                                  p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                  style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                                )                                              ),
                                     ),
                                     tabPanel(title= "Neo-Marxian Full", value= "panel4",
                                              conditionalPanel(
                                                condition = "input.WrightF == '1'",
                                                tags$h3("Employment Status"),
                                                radioButtons("WSE", p("Are you employed by someone else or are you self-employed?"),
                                                             choices= list("Employed"=1, "Self-Employed"=2), selected =0),
                                                
                                                conditionalPanel(
                                                  condition = "input.WSE =='2'",
                                                  radioButtons("WNE", p("About how many people are employed in this business on a permanent basis?"),
                                                               choices= list("1"=1,"2-10"=2, "10+"=3, "N/A"=4), selected =4)),
                                                tags$h3("Decision Making"),
                                                radioButtons("DM", p("Do you participate in making, or giving advice around policy related decisions in the workplace? That is, making decisions about such things as the products or services delivered, the total number of people employed, budgets, and so forth. Do you participate in making these kinds of decisions, or even provide advice about them?"),
                                                             choices= list("Yes"=1, "No"=2), selected =0),
                                                conditionalPanel(
                                                  condition = "input.DM == '1'",
                                                  tags$h3("Think of the specific place of work. If the organisation for which you work has more than one branch, plant or store, think of the specific location where you work. The following questions will ask you about decisions which might affect your workplace. For each, answer if you are personally involved in this decision, including providing adivce on it."),
                                                  radioButtons("DMA", p("Decisions to increase or decrease total number of people employed in the place where you work"),
                                                               choices= list("Yes"=1, "No"=2), selected =2),
                                                  conditionalPanel(
                                                    condition = "input.DMA == '1'",
                                                    radioButtons("DMA1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                  radioButtons("DMB", p("Policy decisions to significantly change the products, programs or service delivered by the organisation"),
                                                               choices= list("Yes"=1, "No"=2), selected =2),
                                                  conditionalPanel(
                                                    condition = "input.DMB == '1'",
                                                    radioButtons("DMB1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                  radioButtons("DMC", p("Decisions to change the policy concerning the routine pace of work or the amount of work performed in your workplace as a whole or some major part of it"),
                                                               choices= list("Yes"=1, "No"=2), selected =2),
                                                  conditionalPanel(
                                                    condition = "input.DMC == '1'",
                                                    radioButtons("DMC1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                  radioButtons("DMD", p("Policy decisions to significantly change the basic methods or procedures of work in a major part of the workplace"),
                                                               choices= list("Yes"=1, "No"=2), selected =2),
                                                  conditionalPanel(
                                                    condition = "input.DMD == '1'",
                                                    radioButtons("DMD1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                  radioButtons("DME", p("Decisions concerning the budget at the place where you work"),
                                                               choices= list("Yes"=1, "No"=2), selected =2),
                                                  conditionalPanel(
                                                    condition = "input.DME == '1'",
                                                    radioButtons("DME1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                  conditionalPanel(
                                                    condition = "input.DME == '1'",
                                                    radioButtons("DMF", p("Decisions to increase or decrease total number of people employed in the place where you work"),
                                                                 choices= list("Yes"=1, "No"=2), selected =2),
                                                    conditionalPanel(
                                                      condition = "input.DMF == '1'",
                                                      radioButtons("DMF1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                   choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5))),
                                                  radioButtons("DMG", p("General policy decisions about the distribution of funds within the overall budget"),
                                                               choices= list("Yes"=1, "No"=2), selected =2),
                                                  conditionalPanel(
                                                    condition = "input.DMG == '1'",
                                                    radioButtons("DMG1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                  radioButtons("DMH", p("Any other kinds of decisions important for the workplace as a whole"),
                                                               choices= list("Yes"=1, "No"=2), selected =2),
                                                  conditionalPanel(
                                                    condition = "input.DMH == '1'",
                                                    radioButtons("DMH1", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4, "N/A"=5), selected= 5)),
                                                ),
                                                tags$h3("Supervision"),
                                                radioButtons("C1S", p("As an official part of your main job do you supervise the work of other employees or tell other employees what work to do?"),
                                                             choices= list("Yes"=1, "No"=2), selected =0),
                                                conditionalPanel(
                                                  condition = "input.C1S == '1'",
                                                  radioButtons("C2S", p("How many people do you directly supervise?"),
                                                               choices= list("1"=1, "More than 1"=2), selected =0),
                                                  radioButtons("C3S", p("Do any of your subordinates have subordinates under them?"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  tags$h3("Are you directly responsible for the following?"),
                                                  radioButtons("C3SA", p("Deciding the specific tasks or work assignments performed by your subordinates"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  radioButtons("C3SB", p("Deciding what procedures, tools, or materials your subordinates use in doing their work"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  radioButtons("C3SC", p("Deciding how fast your subordinates work, how long they work or how much work they have to get done"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  tags$h3("Do you have any influence over the following possible sanctions that could be imposed upon subordinates?"),
                                                  radioButtons("C4A", p("granting a pay rise or promotion to a subordinate"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  radioButtons("C4B", p("Preventing a subordinate from getting a pay rise or promotion because of poor work or misbehaviour"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  radioButtons("C4C", p("Firing or temporarily suspending a subordinate"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  radioButtons("C4D", p("Issuing a formal warning to a subordinate"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                ),
                                                conditionalPanel(
                                                  condition= "input.WSE =='1'",
                                                  tags$h3("Formal Hierarchical Position"),
                                                  radioButtons("H1S", p("Which of the following best describes the position which you hold within your organisation"),
                                                               choices= list("Managerial Position"=1, "Supervisory Position"=2, "Non-Management Position"=3), selected =0),
                                                  conditionalPanel(
                                                    condition= "input.H1S =='1'",
                                                    radioButtons("H1SA", p("Would that be top, upper, middle, or lower managerial position?"),
                                                                 choices= list("Top"=1, "Upper"=2, "Middle"=3, "Lower"=4), selected =0),
                                                  )
                                                ),
                                                tags$h3("Occupation"),
                                                radioButtons("O1S", p("What is your Occupation?"),
                                                             choices= list("Professionals"=1, "Professors"=2, "Managers"=3, "School Teachers"=4, "Craftworkers"=5, "Technicians"=6, "Sales"=7, "Clerical"=8, "Manual non-crafts"=9), selected =0),
                                                tags$h3("Education"),
                                                radioButtons("E1S", p("What is your highest level of Education?"),
                                                             choices= list("B.A or more"=1, "Less than a B.A"=2), selected =0),
                                                conditionalPanel(
                                                  condition= "input.WSE == '1'",
                                                  tags$h3("Autonomy"),
                                                  radioButtons("B1S", p("Is yours a job in which you are required to design important aspects of your own work and to put your ideas into practice?"),
                                                               choices= list("Yes"=1, "No"=2), selected=0),
                                                  conditionalPanel(
                                                    condition = "input.B1S == '1'",
                                                    radioButtons("B1SA", p("What level of autonomy would you say you have in your job?"),
                                                                 choices= list("High Autonomy"=1, "Probably High Autonomy"=2, "Intermediate Autonomy"=3, "Probably Intermediate Autonomy"=4, "Low Autonomy"=5, "No Autonomy"=6), selected=6),
                                                  ),
                                                ),
                                              ),
                                              actionButton("submitWrightFull", "Find Social Class"),
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              )                                     
                                              ),
                                     tabPanel(title= "Neo-Marxian Simplified", value= "panel5",
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
                                                  tags$h3("For any, are you personally involved in these decisions, including providing advice on it"),
                                                  radioButtons("D2as", p("decisions to increase or decrease total number of people employed, policy decisions to change products or services, change policy around pace of work, changing procedure, decisions concerning budget, or general policy decisions?"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  conditionalPanel(
                                                    condition = "input.D2as == '1'",
                                                    radioButtons("D2a1s", p("In which of the following ways did you usually participate in making the decision?"),
                                                                 choices= list("Make the decision on your own authority"=1, "Participate as a voting member of a group which makes the decision"=2, "Make the decision subject to approval"=3, "Provide advice to the person who actually makes the decision"=4), selected= 0)),
                                                ),
                                                tags$h3("Supervision"),
                                                radioButtons("C1s", p("As an official part of your main job do you supervise the work of other employees or tell other employees what work to do?"),
                                                             choices= list("Yes"=1, "No"=2), selected =0),
                                                conditionalPanel(
                                                  condition = "input.C1s == '1'",
                                                  radioButtons("C2s", p("How many people do you directly supervise?"),
                                                               choices= list("1"=1, "More than 1"=2), selected =0),
                                                  conditionalPanel(
                                                    condition = "input.C2s == '2'",
                                                  radioButtons("C3s", p("Do any of your subordinates have subordinates under them?"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  tags$h3("Are you directly responsible for the following?"),
                                                  radioButtons("C3as", p("deciding the specific tasks or work assignments performed by your subordinates, deciding what procedures, tools, or materials your subordinates use in doing their work, or deciding how fast your subordinates work, how long they work, or how much work they have to get done"),
                                                               choices= list("Yes"=1, "No"=2), selected =0),
                                                  tags$h3("Do you have any influence over the following possible sanctions that could be imposed upon subordinates?"),
                                                  radioButtons("C4as", p("granting a pay rise or promotion to a subordinate, preventing a subordinate from getting a pay rise or promotion because of poor work or misbehaviour, firing or temporarily suspending a subordinate, or issuing a formal warning to a subordinate"),
                                                               choices= list("Yes"=1, "No"=2), selected =0)),
                                                ),
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
                                                actionButton("submitWrightS", "Find Social Class"),
                                                
                                              ),
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              )                                     
                                              ),
                                     tabPanel(title= "NS-SEC", value= "panel6",
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
                                              actionButton("submitNSSEC", "Find Social Class"),
                                              
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              )                                     
                                              ),
                                     tabPanel(title= "RGSC", value= "panel7",
                                              conditionalPanel(
                                                condition = "input.RGSC == '1'",
                                                tags$h3("Employment Status"),
                                                radioButtons("SEStatus", p("What is your Employment Status"),
                                                             choices= list("Self-employed 25 or more employees"=1, "Self-Employed 1-24 employees"=2, "Self-Employed without employees"=3, "Manager with 25 or more employees"=4, "Manager with 1-24 employees"=5, "Supervisor"=6, "Employee"=7), selected = 0),
                                                tags$h3("What is your occupation?"),
                                                p("If you are having trouble finding your exact occupation, type in 'n.e.c' and select the closest general occupational grouping that matches your own"),
                                                
                                                pickerInput(
                                                  inputId = "SEGSoc",
                                                  label = "SOC2010 Code Search",
                                                  choices = list("Chief executives and senior officials"=1115, "Elected officers and representatives"=1116, "Production managers and directors in manufacturing"=1121, "Production managers and directors in construction"=1122, "Production managers and directors in mining and energy"=1123, "Financial managers and directors"=1131, "Marketing and sales directors"=1132, "Purchasing managers and directors"=1133, "Advertising and public relations directors"=1134, "Human resource managers and directors"=1135, "Information technology and telecommunications directors"=1136, "Functional managers and directors n.e.c."=1139, "Financial institution managers and directors"=1150, "Managers and directors in transport and distribution"=1161, "Managers and directors in storage and warehousing"=1162, "Officers in armed forces"=1171, "Senior police officers"=1172, "Senior officers in fire, ambulance, prison and related services"=1173, "Health services and public health managers and directors"=1181, "Social services managers and directors"=1184, "Managers and directors in retail and wholesale"=1190, "Managers and proprietors in agriculture and horticulture"=1211, "Managers and proprietors in forestry, fishing and related services"=1213, "Hotel and accommodation managers and proprietors"=1221, "Restaurant and catering establishment managers and proprietors"=1223, "Publicans and managers of licensed premises"=1224, "Leisure and sports managers"=1225, "Travel agency managers and proprietors"=1226, "Health care practice managers"=1241, "Residential, day and domiciliary care managers and proprietors"=1242, "Property, housing and estate managers"=1251, "Garage managers and proprietors"=1252, "Hairdressing and beauty salon managers and proprietors"=1253, "Shopkeepers and proprietors – wholesale and retail"=1254, "Waste disposal and environmental services managers"=1255, "Managers and proprietors in other services n.e.c."=1259, "Chemical scientists"=2111, "Biological scientists and biochemists"=2112, "Physical scientists"=2113, "Social and humanities scientists"=2114, "Natural and social science professionals n.e.c."=2119, "Civil engineers"=2121, "Mechanical engineers"=2122, "Electrical engineers"=2123, "Electronics engineers"=2124, "Design and development engineers"=2126, "Production and process engineers"=2127, "Engineering professionals n.e.c."=2129, "IT specialist managers"=2133, "IT project and programme managers"=2134, "IT business analysts, architects and systems designers"=2135, "Programmers and software development professionals"=2136, "Web design and development professionals"=2137, "Information technology and telecommunications professionals n.e.c."=2139, "Conservation professionals"=2141, "Environment professionals"=2142, "Research and development managers"=2150, "Medical practitioners"=2211, "Psychologists"=2212, "Pharmacists"=2213, "Ophthalmic opticians"=2214, "Dental practitioners"=2215, "Veterinarians"=2216, "Medical radiographers"=2217, "Podiatrists"=2218, "Health professionals n.e.c."=2219, "Physiotherapists"=2221, "Occupational therapists"=2222, "Speech and language therapists"=2223, "Therapy professionals n.e.c."=2229, "Nurses"=2231, "Midwives"=2232, "Higher education teaching professionals"=2311, "Further education teaching professionals"=2312, "Secondary education teaching professionals"=2314, "Primary and nursery education teaching professionals"=2315, "Special needs education teaching professionals"=2316, "Senior professionals of educational establishments"=2317, "Education advisers and school inspectors"=2318, "Teaching and other educational professionals n.e.c."=2319, "Barristers and judges"=2413, "Legal professionals n.e.c."=2419, "Chartered and certified accountants"=2421, "Management consultants and business analysts"=2423, "Business and financial project management professionals"=2424, "Actuaries, economists and statisticians"=2425, "Business and related research professionals"=2426, "Business, research and administrative professionals n.e.c."=2429, "Architects"=2431, "Town planning officers"=2432, "Quantity surveyors"=2433, "Chartered surveyors"=2434, "Chartered architectural technologists"=2435, "Construction project managers and related professionals"=2436, "Social workers"=2442, "Probation officers"=2443, "Clergy"=2444, "Welfare professionals n.e.c."=2449, "Librarians"=2451, "Archivists and curators"=2452, "Quality control and planning engineers"=2461, "Quality assurance and regulatory professionals"=2462, "Environmental health professionals"=2463, "Journalists, newspaper and periodical editors"=2471, "Public relations professionals"=2472, "Advertising accounts managers and creative directors"=2473, "Laboratory technicians"=3111, "Electrical and electronics technicians"=3112, "Engineering technicians"=3113, "Building and civil engineering technicians"=3114, "Quality assurance technicians"=3115, "Planning, process and production technicians"=3116, "Science, engineering and production technicians n.e.c."=3119, "Architectural and town planning technicians"=3121, "Draughtspersons"=3122, "IT operations technicians"=3131, "IT user support technicians"=3132, "Paramedics"=3213, "Dispensing opticians"=3216, "Pharmaceutical technicians"=2317, "Medical and dental technicians"=3218, "Health associate professionals n.e.c."=3219, "Youth and community workers"=3231, "Child and early years officers"=3233, "Housing officers"=3234, "Counsellors"=3235, "Welfare and housing associate professionals n.e.c."=3239, "NCOs and other ranks"=3311, "Police officers (sergeant and below)"=3312, "Fire service officers (watch manager and below)"=3313, "Prison service officers (below principal officer)"=3314, "Police community support officers"=3315, "Protective service associate professionals n.e.c."=3319, "Artists"=3411, "Authors, writers and translators"=3412, "Actors, entertainers and presenters"=3413, "Dancers and choreographers"=3414, "Musicians"=3415, "Arts officers, producers and directors"=3416, "Photographers, audio-visual and broadcasting equipment operators"=3417, "Graphic designers"=3421, "Product, clothing and related designers"=3422, "Sports players"=3441, "Sports coaches, instructors and officials"=3442, "Fitness instructors"=3443, "Air traffic controllers"=3511, "Aircraft pilots and flight engineers"=3512, "Ship and hovercraft officers"=3513, "Legal associate professionals"=3520, "Estimators, valuers and assessors"=3531, "Brokers"=3532, "Insurance underwriters"=3533, "Finance and investment analysts and advisers"=3534, "Taxation experts"=3535, "Importers and exporters"=3536, "Financial and accounting technicians"=3537, "Financial accounts managers"=3538, "Business and related associate professionals n.e.c."=3539, "Buyers and procurement officers"=3541, "Business sales executives"=3542, "Marketing associate professionals"=3543, "Estate agents and auctioneers"=3544, "Sales accounts and business development managers"=3545, "Conference and exhibition managers and organisers"=3546, "Conservation and environmental associate professionals"=3550, "Public services associate professionals"=3561, "Human resources and industrial relations officers"=3562, "Vocational and industrial trainers and instructors"=3563, "Careers advisers and vocational guidance specialists"=3564, "Inspectors of standards and regulations"=3565, "Health and safety officers"=3567, "National government administrative occupations"=4112, "Local government administrative occupations"=4113, "Officers of non-governmental organisations"=4114, "Credit controllers"=4121, "Book-keepers, payroll managers and wages clerks"=4122, "Bank and post office clerks"=4123, "Finance officers"=4124, "Financial administrative occupations n.e.c."=4129, "Records clerks and assistants"=4131, "Pensions and insurance clerks and assistants"=4132, "Stock control clerks and assistants"=4133, "Transport and distribution clerks and assistants"=4134, "Library clerks and assistants"=4135, "Human resources administrative occupations"=4138, "Sales administrators"=4151, "Other administrative occupations n.e.c."=4159, "Office managers"=4161, "Office supervisors"=4162, "Medical secretaries"=4211, "Legal secretaries"=4212, "School secretaries"=4213, "Company secretaries"=4214, "Personal assistants and other secretaries"=4215, "Receptionists"=4216, "Typists and related keyboard occupations"=4217, "Farmers"=5111, "Horticultural trades"=5112, "Gardeners and landscape gardeners"=5113, "Groundsmen and greenkeepers"=5114, "Agricultural and fishing trades n.e.c."=5119, "Smiths and forge workers"=5211, "Moulders, core makers and die casters"=5212, "Sheet metal workers"=5213, "Metal plate workers and riveters"=5214, "Welding trades"=5215, "Pipe fitters"=5216, "Metal machining setters and setter-operators"=5221, "Tool makers, tool fitters and markers-out"=5222, "Metal working production and maintenance fitters"=5223, "Precision instrument makers and repairers"=5224, "Air-conditioning and refrigeration engineers"=5225, "Vehicle technicians, mechanics and electricians"=5231, "Vehicle body builders and repairers"=5232, "Vehicle paint technicians"=5234, "Aircraft maintenance and related trades"=5235, "Boat and ship builders and repairers"=5236, "Rail and rolling stock builders and repairers"=5237, "Electricians and electrical fitters"=5241, "Telecommunications engineers"=5242, "TV, video and audio engineers"=5244, "IT engineers"=5245, "Electrical and electronic trades n.e.c."=5249, "Skilled metal, electrical and electronic trades supervisors"=5230, "Steel erectors"=5311, "Bricklayers and masons"=5312, "Roofers, roof tilers and slaters"=5313, "Plumbers and heating and ventilating engineers"=5314, "Carpenters and joiners"=5315, "Glaziers, window fabricators and fitters"=5316, "Construction and building trades n.e.c."=5319, "Plasterers"=5321, "Floorers and wall tilers"=5322, "Painters and decorators"=5323, "Construction and building trades supervisors"=5330, "Weavers and knitters"=5411, "Upholsterers"=5412, "Footwear and leather working trades"=5413, "Tailors and dressmakers"=5414, "Textiles, garments and related trades n.e.c."=5419, "Pre-press technicians"=5421, "Printers"=5422, "Print finishing and binding workers"=5423, "Butchers"=5431, "Bakers and flour confectioners"=5432, "Fishmongers and poultry dressers"=5433, "Chefs"=5434, "Cooks"=5435, "Catering and bar managers"=5436, "Glass and ceramics makers, decorators and finishers"=5441, "Furniture makers and other craft woodworkers"=5442, "Florists"=5443, "Other skilled trades n.e.c."=5449, "Nursery nurses and assistants"=6121, "Childminders and related occupations"=6122, "Playworkers"=6123, "Teaching assistants"=6125, "Educational support assistants"=6126, "Veterinary nurses"=6131, "Pest control officers"=6132, "Animal care services occupations n.e.c."=6139, "Nursing auxiliaries and assistants"=6141, "Ambulance staff (excluding paramedics)"=6142, "Dental nurses"=6143, "Houseparents and residential wardens"=6144, "Care workers and home carers"=6145, "Senior care workers"=6146, "Care escorts"=6147, "Undertakers, mortuary and crematorium assistants"=6148, "Sports and leisure assistants"=6211, "Travel agents"=6212, "Air travel assistants"=6214, "Rail travel assistants"=6215, "Leisure and travel service occupations n.e.c."=6219, "Hairdressers and barbers"=6221, "Beauticians and related occupations"=6222, "Housekeepers and related occupations"=6223, "Caretakers"=6232, "Cleaning and housekeeping managers and supervisors"=6240, "Sales and retail assistants"=7111, "Retail cashiers and check-out operators"=7112, "Telephone salespersons"=7113, "Pharmacy and other dispensing assistants"=7114, "Vehicle and parts salespersons and advisers"=7115, "Collector salespersons and credit agents"=7121, "Debt, rent and other cash collectors"=7122, "Roundspersons and van salespersons"=7123, "Market and street traders and assistants"=7124, "Merchandisers and window dressers"=7125, "Sales related occupations n.e.c."=7129, "Sales supervisors"=7130, "Call and contact centre occupations"=7211, "Telephonists"=7213, "Communication operators"=7214, "Market research interviewers"=7215, "Customer service occupations n.e.c."=7219, "Customer service managers and supervisors"=7220, "Food, drink and tobacco process operatives"=8111, "Glass and ceramics process operatives"=8112, "Textile process operatives"=8113, "Chemical and related process operatives"=8114, "Rubber process operatives"=8115, "Plastics process operatives"=8116, "Metal making and treating process operatives"=8117, "Electroplaters"=8118, "Process operatives n.e.c."=8119, "Paper and wood machine operatives"=8121, "Coal mine operatives"=8122, "Quarry workers and related operatives"=8123, "Energy plant operatives"=8124, "Metal working machine operatives"=8125, "Water and sewerage plant operatives"=8126, "Printing machine assistants"=8127, "Plant and machine operatives n.e.c."=8129, "Assemblers (electrical and electronic products)"=8131, "Assemblers (vehicles and metal goods)"=8132, "Routine inspectors and testers"=8133, "Weighers, graders and sorters"=8134, "Tyre, exhaust and windscreen fitters"=8135, "Sewing machinists"=8137, "Assemblers and routine operatives n.e.c."=8139, "Scaffolders, stagers and riggers"=8141, "Road construction operatives"=8142, "Rail construction and maintenance operatives"=8143, "Construction operatives n.e.c."=8149, "Large goods vehicle drivers"=8211, "Van drivers"=8212, "Bus and coach drivers"=8213, "Taxi and cab drivers and chauffeurs"=8214, "Driving instructors"=8215, "Crane drivers"=8221, "Fork-lift truck drivers"=8222, "Agricultural machinery drivers"=8223, "Mobile machine drivers and operatives n.e.c."=8229, "Train and tram drivers"=8231, "Marine and waterways transport operatives"=8232, "Air transport operatives"=8233, "Rail transport operatives"=8234, "Other drivers and transport operatives n.e.c."=8239, "Farm workers"=9111, "Fishing and other elementary agriculture occupations n.e.c."=9119, "Elementary construction occupations"=9120, "Industrial cleaning process occupations"=9132, "Packers, bottlers, canners and fillers"=9134, "Elementary process plant occupations n.e.c."=9139, "Postal workers, mail sorters, messengers and couriers"=9211, "Elementary administration occupations n.e.c."=9219, "Window cleaners"=9231, "Street cleaners"=9232, "Cleaners and domestics"=9233, "Launderers, dry cleaners and pressers"=9234, "Refuse and salvage occupations"=9235, "Vehicle valeters and cleaners"=9236, "Elementary cleaning occupations n.e.c."=9239, "Security guards and related occupations"=9241, "Parking and civil enforcement occupations"=9242, "School mid-day and crossing patrol occupations"=9244, "Elementary security occupations n.e.c."=9249, "Shelf fillers"=9251, "Elementary sales occupations n.e.c."=9259, "Elementary storage occupations"=9260, "Hospital porters"=9271, "Kitchen and catering assistants"=9272, "Waiters and waitresses"=9273, "Bar staff"=9274, "Leisure and theme park attendants"=9275, "Other elementary services occupations n.e.c."=9279),
                                                  options = list(
                                                    `live-search` = TRUE)
                                                ),
                                                actionButton("submitRGSC", "Find Social Class"),
                                              ),
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              )                               
                                              ),
                                     tabPanel(title= "CAMSIS", value= "panel8",
                                              conditionalPanel(
                                                condition = "input.CAMSIS == '1'",
                                                tags$h3("Sex"),
                                                radioButtons("Sex", p("What is your Sex"),
                                                             choices= list("Male"=1, "Female"=2), selected = 0),
                                                tags$h3("Employment Status"),
                                                radioButtons("EStatus", p("What is your Employment Status"),
                                                             choices= list("Self-employed 10+ employees"=1, "Self-Employed 1-9 employees"=2, "Self-Employed without employees"=3, "Supervisor/Manager"=4, "Employee"=5), selected = 0),
                                                tags$h3("What is your occupation?"),
                                                p("If you are having trouble finding your exact occupation, type in 'n.e.c' and select the closest general occupational grouping that matches your own"),
                                                
                                                pickerInput(
                                                  inputId = "CAMSISSoc",
                                                  label = "SOC2010 Code Search",
                                                  choices = list("Chief executives and senior officials"=1115, "Elected officers and representatives"=1116, "Production managers and directors in manufacturing"=1121, "Production managers and directors in construction"=1122, "Production managers and directors in mining and energy"=1123, "Financial managers and directors"=1131, "Marketing and sales directors"=1132, "Purchasing managers and directors"=1133, "Advertising and public relations directors"=1134, "Human resource managers and directors"=1135, "Information technology and telecommunications directors"=1136, "Functional managers and directors n.e.c."=1139, "Financial institution managers and directors"=1150, "Managers and directors in transport and distribution"=1161, "Managers and directors in storage and warehousing"=1162, "Officers in armed forces"=1171, "Senior police officers"=1172, "Senior officers in fire, ambulance, prison and related services"=1173, "Health services and public health managers and directors"=1181, "Social services managers and directors"=1184, "Managers and directors in retail and wholesale"=1190, "Managers and proprietors in agriculture and horticulture"=1211, "Managers and proprietors in forestry, fishing and related services"=1213, "Hotel and accommodation managers and proprietors"=1221, "Restaurant and catering establishment managers and proprietors"=1223, "Publicans and managers of licensed premises"=1224, "Leisure and sports managers"=1225, "Travel agency managers and proprietors"=1226, "Health care practice managers"=1241, "Residential, day and domiciliary care managers and proprietors"=1242, "Property, housing and estate managers"=1251, "Garage managers and proprietors"=1252, "Hairdressing and beauty salon managers and proprietors"=1253, "Shopkeepers and proprietors – wholesale and retail"=1254, "Waste disposal and environmental services managers"=1255, "Managers and proprietors in other services n.e.c."=1259, "Chemical scientists"=2111, "Biological scientists and biochemists"=2112, "Physical scientists"=2113, "Social and humanities scientists"=2114, "Natural and social science professionals n.e.c."=2119, "Civil engineers"=2121, "Mechanical engineers"=2122, "Electrical engineers"=2123, "Electronics engineers"=2124, "Design and development engineers"=2126, "Production and process engineers"=2127, "Engineering professionals n.e.c."=2129, "IT specialist managers"=2133, "IT project and programme managers"=2134, "IT business analysts, architects and systems designers"=2135, "Programmers and software development professionals"=2136, "Web design and development professionals"=2137, "Information technology and telecommunications professionals n.e.c."=2139, "Conservation professionals"=2141, "Environment professionals"=2142, "Research and development managers"=2150, "Medical practitioners"=2211, "Psychologists"=2212, "Pharmacists"=2213, "Ophthalmic opticians"=2214, "Dental practitioners"=2215, "Veterinarians"=2216, "Medical radiographers"=2217, "Podiatrists"=2218, "Health professionals n.e.c."=2219, "Physiotherapists"=2221, "Occupational therapists"=2222, "Speech and language therapists"=2223, "Therapy professionals n.e.c."=2229, "Nurses"=2231, "Midwives"=2232, "Higher education teaching professionals"=2311, "Further education teaching professionals"=2312, "Secondary education teaching professionals"=2314, "Primary and nursery education teaching professionals"=2315, "Special needs education teaching professionals"=2316, "Senior professionals of educational establishments"=2317, "Education advisers and school inspectors"=2318, "Teaching and other educational professionals n.e.c."=2319, "Barristers and judges"=2413, "Legal professionals n.e.c."=2419, "Chartered and certified accountants"=2421, "Management consultants and business analysts"=2423, "Business and financial project management professionals"=2424, "Actuaries, economists and statisticians"=2425, "Business and related research professionals"=2426, "Business, research and administrative professionals n.e.c."=2429, "Architects"=2431, "Town planning officers"=2432, "Quantity surveyors"=2433, "Chartered surveyors"=2434, "Chartered architectural technologists"=2435, "Construction project managers and related professionals"=2436, "Social workers"=2442, "Probation officers"=2443, "Clergy"=2444, "Welfare professionals n.e.c."=2449, "Librarians"=2451, "Archivists and curators"=2452, "Quality control and planning engineers"=2461, "Quality assurance and regulatory professionals"=2462, "Environmental health professionals"=2463, "Journalists, newspaper and periodical editors"=2471, "Public relations professionals"=2472, "Advertising accounts managers and creative directors"=2473, "Laboratory technicians"=3111, "Electrical and electronics technicians"=3112, "Engineering technicians"=3113, "Building and civil engineering technicians"=3114, "Quality assurance technicians"=3115, "Planning, process and production technicians"=3116, "Science, engineering and production technicians n.e.c."=3119, "Architectural and town planning technicians"=3121, "Draughtspersons"=3122, "IT operations technicians"=3131, "IT user support technicians"=3132, "Paramedics"=3213, "Dispensing opticians"=3216, "Pharmaceutical technicians"=2317, "Medical and dental technicians"=3218, "Health associate professionals n.e.c."=3219, "Youth and community workers"=3231, "Child and early years officers"=3233, "Housing officers"=3234, "Counsellors"=3235, "Welfare and housing associate professionals n.e.c."=3239, "NCOs and other ranks"=3311, "Police officers (sergeant and below)"=3312, "Fire service officers (watch manager and below)"=3313, "Prison service officers (below principal officer)"=3314, "Police community support officers"=3315, "Protective service associate professionals n.e.c."=3319, "Artists"=3411, "Authors, writers and translators"=3412, "Actors, entertainers and presenters"=3413, "Dancers and choreographers"=3414, "Musicians"=3415, "Arts officers, producers and directors"=3416, "Photographers, audio-visual and broadcasting equipment operators"=3417, "Graphic designers"=3421, "Product, clothing and related designers"=3422, "Sports players"=3441, "Sports coaches, instructors and officials"=3442, "Fitness instructors"=3443, "Air traffic controllers"=3511, "Aircraft pilots and flight engineers"=3512, "Ship and hovercraft officers"=3513, "Legal associate professionals"=3520, "Estimators, valuers and assessors"=3531, "Brokers"=3532, "Insurance underwriters"=3533, "Finance and investment analysts and advisers"=3534, "Taxation experts"=3535, "Importers and exporters"=3536, "Financial and accounting technicians"=3537, "Financial accounts managers"=3538, "Business and related associate professionals n.e.c."=3539, "Buyers and procurement officers"=3541, "Business sales executives"=3542, "Marketing associate professionals"=3543, "Estate agents and auctioneers"=3544, "Sales accounts and business development managers"=3545, "Conference and exhibition managers and organisers"=3546, "Conservation and environmental associate professionals"=3550, "Public services associate professionals"=3561, "Human resources and industrial relations officers"=3562, "Vocational and industrial trainers and instructors"=3563, "Careers advisers and vocational guidance specialists"=3564, "Inspectors of standards and regulations"=3565, "Health and safety officers"=3567, "National government administrative occupations"=4112, "Local government administrative occupations"=4113, "Officers of non-governmental organisations"=4114, "Credit controllers"=4121, "Book-keepers, payroll managers and wages clerks"=4122, "Bank and post office clerks"=4123, "Finance officers"=4124, "Financial administrative occupations n.e.c."=4129, "Records clerks and assistants"=4131, "Pensions and insurance clerks and assistants"=4132, "Stock control clerks and assistants"=4133, "Transport and distribution clerks and assistants"=4134, "Library clerks and assistants"=4135, "Human resources administrative occupations"=4138, "Sales administrators"=4151, "Other administrative occupations n.e.c."=4159, "Office managers"=4161, "Office supervisors"=4162, "Medical secretaries"=4211, "Legal secretaries"=4212, "School secretaries"=4213, "Company secretaries"=4214, "Personal assistants and other secretaries"=4215, "Receptionists"=4216, "Typists and related keyboard occupations"=4217, "Farmers"=5111, "Horticultural trades"=5112, "Gardeners and landscape gardeners"=5113, "Groundsmen and greenkeepers"=5114, "Agricultural and fishing trades n.e.c."=5119, "Smiths and forge workers"=5211, "Moulders, core makers and die casters"=5212, "Sheet metal workers"=5213, "Metal plate workers and riveters"=5214, "Welding trades"=5215, "Pipe fitters"=5216, "Metal machining setters and setter-operators"=5221, "Tool makers, tool fitters and markers-out"=5222, "Metal working production and maintenance fitters"=5223, "Precision instrument makers and repairers"=5224, "Air-conditioning and refrigeration engineers"=5225, "Vehicle technicians, mechanics and electricians"=5231, "Vehicle body builders and repairers"=5232, "Vehicle paint technicians"=5234, "Aircraft maintenance and related trades"=5235, "Boat and ship builders and repairers"=5236, "Rail and rolling stock builders and repairers"=5237, "Electricians and electrical fitters"=5241, "Telecommunications engineers"=5242, "TV, video and audio engineers"=5244, "IT engineers"=5245, "Electrical and electronic trades n.e.c."=5249, "Skilled metal, electrical and electronic trades supervisors"=5230, "Steel erectors"=5311, "Bricklayers and masons"=5312, "Roofers, roof tilers and slaters"=5313, "Plumbers and heating and ventilating engineers"=5314, "Carpenters and joiners"=5315, "Glaziers, window fabricators and fitters"=5316, "Construction and building trades n.e.c."=5319, "Plasterers"=5321, "Floorers and wall tilers"=5322, "Painters and decorators"=5323, "Construction and building trades supervisors"=5330, "Weavers and knitters"=5411, "Upholsterers"=5412, "Footwear and leather working trades"=5413, "Tailors and dressmakers"=5414, "Textiles, garments and related trades n.e.c."=5419, "Pre-press technicians"=5421, "Printers"=5422, "Print finishing and binding workers"=5423, "Butchers"=5431, "Bakers and flour confectioners"=5432, "Fishmongers and poultry dressers"=5433, "Chefs"=5434, "Cooks"=5435, "Catering and bar managers"=5436, "Glass and ceramics makers, decorators and finishers"=5441, "Furniture makers and other craft woodworkers"=5442, "Florists"=5443, "Other skilled trades n.e.c."=5449, "Nursery nurses and assistants"=6121, "Childminders and related occupations"=6122, "Playworkers"=6123, "Teaching assistants"=6125, "Educational support assistants"=6126, "Veterinary nurses"=6131, "Pest control officers"=6132, "Animal care services occupations n.e.c."=6139, "Nursing auxiliaries and assistants"=6141, "Ambulance staff (excluding paramedics)"=6142, "Dental nurses"=6143, "Houseparents and residential wardens"=6144, "Care workers and home carers"=6145, "Senior care workers"=6146, "Care escorts"=6147, "Undertakers, mortuary and crematorium assistants"=6148, "Sports and leisure assistants"=6211, "Travel agents"=6212, "Air travel assistants"=6214, "Rail travel assistants"=6215, "Leisure and travel service occupations n.e.c."=6219, "Hairdressers and barbers"=6221, "Beauticians and related occupations"=6222, "Housekeepers and related occupations"=6223, "Caretakers"=6232, "Cleaning and housekeeping managers and supervisors"=6240, "Sales and retail assistants"=7111, "Retail cashiers and check-out operators"=7112, "Telephone salespersons"=7113, "Pharmacy and other dispensing assistants"=7114, "Vehicle and parts salespersons and advisers"=7115, "Collector salespersons and credit agents"=7121, "Debt, rent and other cash collectors"=7122, "Roundspersons and van salespersons"=7123, "Market and street traders and assistants"=7124, "Merchandisers and window dressers"=7125, "Sales related occupations n.e.c."=7129, "Sales supervisors"=7130, "Call and contact centre occupations"=7211, "Telephonists"=7213, "Communication operators"=7214, "Market research interviewers"=7215, "Customer service occupations n.e.c."=7219, "Customer service managers and supervisors"=7220, "Food, drink and tobacco process operatives"=8111, "Glass and ceramics process operatives"=8112, "Textile process operatives"=8113, "Chemical and related process operatives"=8114, "Rubber process operatives"=8115, "Plastics process operatives"=8116, "Metal making and treating process operatives"=8117, "Electroplaters"=8118, "Process operatives n.e.c."=8119, "Paper and wood machine operatives"=8121, "Coal mine operatives"=8122, "Quarry workers and related operatives"=8123, "Energy plant operatives"=8124, "Metal working machine operatives"=8125, "Water and sewerage plant operatives"=8126, "Printing machine assistants"=8127, "Plant and machine operatives n.e.c."=8129, "Assemblers (electrical and electronic products)"=8131, "Assemblers (vehicles and metal goods)"=8132, "Routine inspectors and testers"=8133, "Weighers, graders and sorters"=8134, "Tyre, exhaust and windscreen fitters"=8135, "Sewing machinists"=8137, "Assemblers and routine operatives n.e.c."=8139, "Scaffolders, stagers and riggers"=8141, "Road construction operatives"=8142, "Rail construction and maintenance operatives"=8143, "Construction operatives n.e.c."=8149, "Large goods vehicle drivers"=8211, "Van drivers"=8212, "Bus and coach drivers"=8213, "Taxi and cab drivers and chauffeurs"=8214, "Driving instructors"=8215, "Crane drivers"=8221, "Fork-lift truck drivers"=8222, "Agricultural machinery drivers"=8223, "Mobile machine drivers and operatives n.e.c."=8229, "Train and tram drivers"=8231, "Marine and waterways transport operatives"=8232, "Air transport operatives"=8233, "Rail transport operatives"=8234, "Other drivers and transport operatives n.e.c."=8239, "Farm workers"=9111, "Fishing and other elementary agriculture occupations n.e.c."=9119, "Elementary construction occupations"=9120, "Industrial cleaning process occupations"=9132, "Packers, bottlers, canners and fillers"=9134, "Elementary process plant occupations n.e.c."=9139, "Postal workers, mail sorters, messengers and couriers"=9211, "Elementary administration occupations n.e.c."=9219, "Window cleaners"=9231, "Street cleaners"=9232, "Cleaners and domestics"=9233, "Launderers, dry cleaners and pressers"=9234, "Refuse and salvage occupations"=9235, "Vehicle valeters and cleaners"=9236, "Elementary cleaning occupations n.e.c."=9239, "Security guards and related occupations"=9241, "Parking and civil enforcement occupations"=9242, "School mid-day and crossing patrol occupations"=9244, "Elementary security occupations n.e.c."=9249, "Shelf fillers"=9251, "Elementary sales occupations n.e.c."=9259, "Elementary storage occupations"=9260, "Hospital porters"=9271, "Kitchen and catering assistants"=9272, "Waiters and waitresses"=9273, "Bar staff"=9274, "Leisure and theme park attendants"=9275, "Other elementary services occupations n.e.c."=9279),
                                                  options = list(
                                                    `live-search` = TRUE)
                                                ),
                                                actionButton("submitCAMSIS", "Find Social Class"),
                                                
                                              ),
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              )                                     
                                              ),
                                     tabPanel(title= "Your Result", value= "panel9",
                                              conditionalPanel(
                                                condition = "input.Classic == '1'",
                                                tags$h3("Results"),
                                                p(tags$strong("Your Social Class as Determined by the BBC Classic is:")),
                                                uiOutput("GBCS"),
                                                uiOutput("GBCSDefine")
                                              ),
                                              conditionalPanel(
                                                condition = "input.GBCSR == '1'",
                                                p(tags$strong("Your Social Class as Determined by the GBCS Paper is:")),
                                                uiOutput("probabilityresults"),
                                              ),
                                              conditionalPanel(
                                                condition = "input.WrightF == '1'",
                                                p(tags$strong("Your Social Class as Determined by the Full Neo-Marxian Schema is:")),
                                                uiOutput("wrightfull_table"),
                                                uiOutput("WrightDefinefull"),
                                              ),
                                              conditionalPanel(
                                                condition = "input.Wrights == '1'",
                                                p(tags$strong("Your Social Class as Determined by the Simplified Neo-Marxian Schema is:")),
                                                uiOutput("wrights_table"),
                                                uiOutput("WrightDefine"),
                                              ),
                                              conditionalPanel(
                                                condition = "input.NSSEC == '1'",
                                                p(tags$strong("Your Social Class as Determined by NS-SEC is:")),
                                                uiOutput("nssecsc"),
                                                uiOutput("NSSECDefine"),
                                              ),
                                              conditionalPanel(
                                                condition = "input.RGSC == '1'",
                                                p(tags$strong("Your Social Class as Determined by RGSC is:")),
                                                uiOutput("RGSC")
                                              ),
                                              conditionalPanel(
                                                condition = "input.CAMSIS == '1'",
                                                p(tags$strong("Your Social Distance as Determined by CAMSIS is:")),
                                                plotOutput("camsisPlot", height = "400px", width = "600px"),  # Set both height and width to the same value to make it square
                                                textOutput("camsisDesc")
                                              ),
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              )                                    
                                              ),
                                     tabPanel(title= "About", value= "panel9",
                                              tags$h3("About Page"),
                                              p("The Social Class Calculator is a project developed by a team at the University of Edinburgh. The primary goals of this project is to demonstrate open science reserach practices in re-creating the Great British Class Survey. Through this recreation, addtional social class schemas were added. Through this addition, this Social Class Calculator serves as a viable teaching tool for students of Sociology in learning abour social class concepts. With its open source format, this Social Class Calculator also serves as a teaching tool for students and teachers alike wishing to design effective survey tools/constructions using Shiny in the R programming language. For a full breakdown of the project and the code used, see the project GitHub page:"),
                                              tags$a(href="https://github.com/EOSSCC/Class-Calculator", "Click here to access project GitHub page"),
                                              tags$div(
                                                p("© T. V. Gunten and S.Oatley, University of Edinburgh, 2023, CC BY-SA."),
                                                style = "position: fixed; bottom: 0; left: 0; width: 100%; text-align: center; background-color: white; padding: 10px;"
                                              ),                                              
                                              
                                     )
                                     
                )
                )
)                              



server <- function(input, output, session) {
  
  #sources for functions
  source("GBCSF.R")
  source("WrightF.R")
  source("NSSECF.R")
  source("CAMSISF.R")
  source("RGSCF.R")
  source("GBCSRF.R")
  source("WrightFF.R")  
  
  # Button to send to next page
  
  observeEvent(input$next_btn, {
    if (input$Classic) {
      updateTabsetPanel(session, "inTabset", selected = "panel2")
    } else if (input$GBCSR) {
      updateTabsetPanel(session, "inTabset", selected = "panel3")
    } else if (input$WrightF) {
      updateTabsetPanel(session, "inTabset", selected = "panel4")
    } else if (input$Wrights) {
      updateTabsetPanel(session, "inTabset", selected = "panel5")
    } else if (input$NSSEC) {
      updateTabsetPanel(session, "inTabset", selected = "panel6")
    } else if (input$RGSC) {
      updateTabsetPanel(session, "inTabset", selected = "panel7")
    } else if (input$CAMSIS) {
      updateTabsetPanel(session, "inTabset", selected = "panel8")
    }
  })
  
  # Buttons to send to results page
  observeEvent(input$submitBBC, {
    if (input$GBCSR) {
      updateTabsetPanel(session, "inTabset", selected = "panel3")
    } else if (input$WrightF) {
      updateTabsetPanel(session, "inTabset", selected = "panel4")
    } else if (input$Wrights) {
      updateTabsetPanel(session, "inTabset", selected = "panel5")
    } else if (input$NSSEC) {
      updateTabsetPanel(session, "inTabset", selected = "panel6")
    } else if (input$RGSC) {
      updateTabsetPanel(session, "inTabset", selected = "panel7")
    } else if (input$CAMSIS) {
      updateTabsetPanel(session, "inTabset", selected = "panel8")
    } else {
      updateTabsetPanel(session, "inTabset", selected = "panel9")
    }
  })
  
  
  observeEvent(input$submit, {
   if (input$WrightF) {
    updateTabsetPanel(session, "inTabset", selected = "panel4")
  } else if (input$Wrights) {
    updateTabsetPanel(session, "inTabset", selected = "panel5")
  } else if (input$NSSEC) {
    updateTabsetPanel(session, "inTabset", selected = "panel6")
  } else if (input$RGSC) {
    updateTabsetPanel(session, "inTabset", selected = "panel7")
  } else if (input$CAMSIS) {
    updateTabsetPanel(session, "inTabset", selected = "panel8")
  } else {
    updateTabsetPanel(session, "inTabset", selected = "panel9")
  }
    })
  
  observeEvent(input$submitWrightFull, {
   if (input$Wrights) {
    updateTabsetPanel(session, "inTabset", selected = "panel5")
  } else if (input$NSSEC) {
    updateTabsetPanel(session, "inTabset", selected = "panel6")
  } else if (input$RGSC) {
    updateTabsetPanel(session, "inTabset", selected = "panel7")
  } else if (input$CAMSIS) {
    updateTabsetPanel(session, "inTabset", selected = "panel8")
  } else {
    updateTabsetPanel(session, "inTabset", selected = "panel9")
  }  
    })
  
  observeEvent(input$submitWrightS, {
   if (input$NSSEC) {
    updateTabsetPanel(session, "inTabset", selected = "panel6")
  } else if (input$RGSC) {
    updateTabsetPanel(session, "inTabset", selected = "panel7")
  } else if (input$CAMSIS) {
    updateTabsetPanel(session, "inTabset", selected = "panel8")
  } else {
    updateTabsetPanel(session, "inTabset", selected = "panel9")
  }  
  })
  
  
  observeEvent(input$submitNSSEC, {
   if (input$RGSC) {
    updateTabsetPanel(session, "inTabset", selected = "panel7")
  } else if (input$CAMSIS) {
    updateTabsetPanel(session, "inTabset", selected = "panel8")
  } else {
    updateTabsetPanel(session, "inTabset", selected = "panel9")
  }  
  })
  
  observeEvent(input$submitRGSC, {
   if (input$CAMSIS) {
    updateTabsetPanel(session, "inTabset", selected = "panel8")
  } else {
    updateTabsetPanel(session, "inTabset", selected = "panel9")
  }  
  })
  
  observeEvent(input$submitCAMSIS, {
    updateTabsetPanel(session, "inTabset", selected = "panel9")
  })

  
  
  # This section covers the toggling of different social class schemas (Note: If the User doesn't toggle the Schema on then it will not show up in the results page) #
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
      hideTab(inputId = "inTabset", target = "panel5")
    }})
  observeEvent(input$Wrights, {
    if(input$Wrights ==1){
      showTab(inputId = "inTabset", target = "panel5")
    }})
  
  observeEvent(input$NSSEC, {
    if(input$NSSEC ==0){
      hideTab(inputId = "inTabset", target = "panel6")
    }})
  observeEvent(input$NSSEC, {
    if(input$NSSEC ==1){
      showTab(inputId = "inTabset", target = "panel6")
    }})
  
  observeEvent(input$CAMSIS, {
    if(input$CAMSIS ==0){
      hideTab(inputId = "inTabset", target = "panel8")
    }})
  observeEvent(input$CAMSIS, {
    if(input$CAMSIS ==1){
      showTab(inputId = "inTabset", target = "panel8")
    }})
  
  observeEvent(input$RGSC, {
    if(input$RGSC ==0){
      hideTab(inputId = "inTabset", target = "panel7")
    }})
  observeEvent(input$RGSC, {
    if(input$RGSC ==1){
      showTab(inputId = "inTabset", target = "panel7")
    }})
  
  observeEvent(input$GBCSR, {
    if(input$GBCSR ==0){
      hideTab(inputId = "inTabset", target = "panel3")
    }})
  observeEvent(input$GBCSR, {
    if(input$GBCSR ==1){
      showTab(inputId = "inTabset", target = "panel3")
    }})
  observeEvent(input$WrightF, {
    if(input$WrightF ==0){
      hideTab(inputId = "inTabset", target = "panel4")
    }})
  observeEvent(input$WrightF, {
    if(input$WrightF ==1){
      showTab(inputId = "inTabset", target = "panel4")
    }})
  
  #Latent Trait Analysis Section Logic
  #read data for latent trait analysis
  gbcs <- read_dta("gfk_cleaned_eul.dta")  
  
  # Transform user inputs into numerical variables to place as row 1027th observation in dataset
  observeEvent(input$submit, {
    # Create a new row with user inputs
    new_row <- data.frame(
      hhmid = as.numeric(input$incomer),
      Owner = as.numeric(input$homer),
      housemid = as.numeric(input$homevaluer),
      savmid = as.numeric(input$savingsr),
      culture = as.numeric(input$socialr),
      chigh1 = as.numeric(input$carts),
      chigh2 = as.numeric(input$cmusgall),
      chigh3 = as.numeric(input$cstathom),
      chigh4 = as.numeric(input$ctheatre),
      chigh5 = as.numeric(input$copera),
      chigh6 = as.numeric(input$mclassic),
      chigh7 = as.numeric(input$mjazz),
      chigh8 = as.numeric(input$ffrench),
      chigh9 = as.numeric(input$cdance),
      cemer1 = as.numeric(input$ccompgam),
      cemer2 = as.numeric(input$csocnet),
      cemer3 = as.numeric(input$csportp),
      cemer4 = as.numeric(input$csportw),
      cemer5 = as.numeric(input$cfriends),
      cemer6 = as.numeric(input$cgym),
      cemer7 = as.numeric(input$cgig),
      cemer8 = as.numeric(input$mrap),
      cemer9 = as.numeric(input$cnet),
      cemer10 = as.numeric(input$mrock)
    )
    
    # Calculate variables in one go
    new_row <- new_row %>%
      mutate(
        chigh = chigh1 + chigh2 + chigh3 + chigh4 + chigh5 + chigh6 + chigh7 + chigh8 + chigh9,
        cemer = cemer1 + cemer2 + cemer3 + cemer4 + cemer5 + cemer6 + cemer7 + cemer8 + cemer9 + cemer10,
        skmean = mean(culture),
        sknum = sum(culture)
      )
    
# Create rows for data insertion
    master_row <- subset(new_row, select = c(chigh, cemer, sknum, skmean, hhmid, savmid, housemid))
    gbcsf <- subset(gbcs, select = c(chigh, cemer, sknum, skmean, hhmid, savmid, housemid))
    combined_df <- rbind(master_row, gbcsf)
    
# Mutate variables based on methodological notes on UKDS website
    combined_df <- combined_df %>%
      mutate(housemida = housemid-119420/127600) %>%
      mutate(hhmida = hhmid-31855.8/31026.6) %>%
      mutate(savmida = savmid-19985.5/46959.9) %>%
      mutate(assets = savmida + housemida) %>%
      mutate(assetsa = assets+0.69036/1.66483) %>%
      mutate(skmeana = skmean-40.9223/9.74163) %>%
      mutate(sknuma = sknum-13.2515/7.0819) %>%
      mutate(cemera = cemer-13.4669/6.3191) %>%
      mutate(chigha = chigh-10.3109/4.89597)
    
    # Perform Latent Class Analysis
    lpa_model <- combined_df %>%
      dplyr::select(chigha, cemera, skmeana, sknuma, hhmida, assetsa) %>%
      single_imputation() %>%
      estimate_profiles(7)
    
    # Output results
    fit_stats <- get_data(lpa_model)
    probabilities_1027 <- fit_stats[1027, paste0("CPROB", 1:7)]
    
    # Find the index of the maximum probability
    max_prob_index <- which.max(probabilities_1027)
    max_prob <- probabilities_1027[max_prob_index]
    
    # Define the titles for each class
    class_titles <- c(
      "Elite",
      "Established Middle Class",
      "Technical Middle Class",
      "New Affluent Workers",
      "Traditional Working Class",
      "Emergent Service Worker",
      "Precariat"
    )
    
    # Define the descriptions for each class
    class_descriptions <- c(
      "This is the wealthiest and most privileged group in the UK.",
      "This is the most gregarious and the second wealthiest of all the class groups.",
      "This is a small, distinctive and prosperous new class group.",
      "This class group is sociable, has lots of cultural interests and sits in the middle of all the groups in terms of wealth.",
      "This class group scores low for economic, social and cultural factors, but they do have some financial security.",
      "This class group is financially insecure with low scores for savings and house value, but high for social and cultural factors.",
      "This is the poorest and most deprived class group. People in this group score low for economic, social and cultural factors."
    )
    
    # Create the HTML output
    output$probabilityresults <- renderUI({
      # Generate a list of classes with the highest probability highlighted
      class_list <- lapply(1:7, function(i) {
        if (i == max_prob_index) {
          tags$li(style = "font-weight: bold; color: red;",
                  paste0(class_titles[i], ": Probability = ", round(probabilities_1027[i], 3)))
        } else {
          tags$li(paste0(class_titles[i], ": Probability = ", round(probabilities_1027[i], 3)))
        }
      })
      
      # Combine list and description of the selected class
      list(
        tags$ul(class_list),
        p(class_descriptions[max_prob_index])
      )
    })
  })
  
  
  
  
  # This section covers the functions for the GBCS schema output # 
  
  derived_GBCSF <- reactiveVal(NULL)
  
  derive_GBCSF <- function(income, social, home, high, emerging, savings){
    social.n <- length(input$social)
    high.n <- length(input$high)
    emerging.n <- length(input$emerging)
    if (input$income ==1) {
      return("Elite")
    }
    else if (input$income ==0.6 & social.n>=6 & mean(as.numeric(input$social >=58))) {
      return("Elite")
    }
    else if (input$income ==0.375 & input$home >=0 & input$savings ==1) {
      return("Elite")
    }
    else if (input$income ==0.6 & social.n<6) {
      return("Technical Middle Class")
    }
    else if (input$income ==0.375 & input$home >0 & input$savings >0.05 & input$savings <1 & social.n <6) {
      return("Technical Middle Class")
    }
    else if (input$income ==0.6 & social.n>=6 & mean(as.numeric(input$social <58))) {
      return("Established Middle Class") 
    }
    else if (input$income ==0.375 & input$home >0 & input$savings <=0.05 & high.n>=3) {
      return("Established Middle Class") 
    }
    else if (input$income ==0.375 & input$home >0 & input$savings >=0.05 & input$savings <1 & social.n>=6) {
      return("Established Middle Class") 
    }
    else if (input$income ==0.375 & input$home ==0) {
      return("Emergent Service Sector")
    }
    else if (input$income ==0.175 & input$home ==0 & emerging.n>=4) {
      return("Emergent Service Sector")
    }
    else if (input$income ==0.08 & input$home ==0 & emerging.n>4) {
      return("Emergent Service Sector") 
    }
    else if (input$income ==0.08 & input$home ==0 & social.n>=6 & emerging.n==4) {
      return("Emergent Service Sector") 
    }
    else if (input$income ==0.375 & input$home >0 & input$savings <=0.05 & high.n<3) {
      return("New Affluent Workers")
    }
    else if (input$income ==0.175 & input$home >0 & emerging.n>=3) {
      return("New Affluent Workers") 
    }
    else if (input$income ==0.175 & input$home ==0 & emerging.n<4) {
      return("Precariat")
    }
    else if (input$income ==0.08 & input$home ==0 & emerging.n<4) {
      return("Precariat")
    }
    else if (input$income ==0.08 & input$home ==0 & social.n<6 & emerging.n==4) {
      return("Precariat")
    }
    else if (input$income ==0.175 & input$home >0 & emerging.n<3) {
      return("Traditional Working Class")
    }
    else if (input$income ==0.08 & input$home >0) {
      return("Traditional Working Class")
    }
    else {
      return("Not Enough Information")
    }
  }
  
  # Observe the submit button and calculate the derived class
  observeEvent(input$submitBBC, {
    derived_GBCSF(
      derive_GBCSF(
        income = input$income,
        social = input$social,
        home = input$home,
        high = input$high,
        emerging = input$emerging,
        savings = input$savings
      )
    )
  })
  
  # Generate the list of all options with highlighting
  output$GBCS <- renderUI({
    if (is.null(derived_GBCSF())) {
      return(h4("Please submit your details to find your social class."))
    }
    
    all_classes <- c("Elite", "Established Middle Class", "Technical Middle Class", 
                     "New Affluent Workers", "Traditional Working Class", 
                     "Emergent Service Sector", "Precariat")
    highlighted_class <- derived_GBCSF()
    
    # Create the list with highlighting
    tags$ul(
      lapply(all_classes, function(cls) {
        if (cls == highlighted_class) {
          tags$li(
            tags$strong(style = "color: red;", cls)
          )
        } else {
          tags$li(cls)
        }
      })
    )
  })
  
  output$GBCSDefine <- renderUI({
    if (is.null(derived_GBCSF())) {
      return(p("Not Enough Information"))
    }
    description <- switch(
      derived_GBCSF(),
      "Elite" = "This is the wealthiest and most privileged group in the UK.",
      "Established Middle Class" = "This is the most gregarious and the second wealthiest of all the class groups.",
      "Technical Middle Class" = "This is a small, distinctive and prosperous new class group.",
      "New Affluent Workers" = "This class group is sociable, has lots of cultural interests and sits in the middle of all the groups in terms of wealth.",
      "Traditional Working Class" = "This class group scores low for economic, social and cultural factors, but they do have some financial security.",
      "Emergent Service Sector" = "This class group is financially insecure with low scores for savings and house value, but high for social and cultural factors.",
      "Precariat" = "This is the poorest and most deprived class group. People in this group score low for economic, social and cultural factors.",
      "Not Enough Information" = "Not Enough Information"
    )
  })
  
  
  # This section calls the functions from the Wright schema source file and outputs this as text in the results page #
  
  derive_wrights<-function(capitals, organisations, skillss){
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
      result<-"Uncredentialled Workers"
    }
    else {
      result<-"Not Enough Information"
    }
    return(result)
  }
  
  
# This section covers the derivation and output of the Simplified Wright schema 
  derived_WrightS <- reactiveVal(NULL)
  
  # Observe the submit button and calculate the derived class
  observeEvent(input$submitWrightS, {
    # Validate and isolate inputs
    capitals_result <- isolate(input$capitals)
    organisations_result <- isolate(input$organisations)
    skills_result <- isolate(input$skills)
    
    # Derive Wright class using these values
    derived_Wright <- derive_wrights(capitals_result, organisations_result, skills_result)
    
    # Update the reactive value
    derived_WrightS(derived_Wright)
    
    # Wright class schema
    all_classes <- c(
      "Bourgeoisie",
      "Small Employers",
      "Petty Bourgeoisie",
      "Expert Managers",
      "Expert Supervisors",
      "Expert Non-Managers",
      "Semi-Credentialled Managers",
      "Semi-Credentialled Supervisors",
      "Semi-Credentialled Workers",
      "Uncredentialled Managers",
      "Uncredentialled Supervisors",
      "Uncredentialled Workers"
    )
    
    # Display the Wright classes and highlight the derived category
    output$wrights_table <- renderUI({
      derived_class <- derived_WrightS()  # Dynamically highlight the user's derived class
      
      tags$div(
        style = "font-family: Arial, sans-serif; display: flex; justify-content: space-between; margin: 20px auto; max-width: 100%;",
        
        # First Table (Capital Owners)
        tags$div(
          style = "width: 48%; padding: 10px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);",
          tags$h4(style = "text-align: center; font-weight: bold; margin-bottom: 15px;", "Owners of the means of production"),
          
          tags$table(
            style = "width: 100%; border-collapse: collapse; text-align: center;",
            tags$thead(
              tags$tr(
                tags$th(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Levels of Capital Ownership")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(
                  style = ifelse(derived_class == "Bourgeoisie", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"), "Bourgeoisie"
                )
              ),
              tags$tr(
                tags$td(
                  style = ifelse(derived_class == "Small Employers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"), "Small Employers"
                )
              ),
              tags$tr(
                tags$td(
                  style = ifelse(derived_class == "Petty Bourgeoisie", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"), "Petty Bourgeoisie"
                )
              )
            )
          )
        ),
        
        # Second Table (Non-Owners with Skill Assets and Organizational Assets)
        tags$div(
          style = "width: 48%; padding: 10px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);",
          tags$h4(style = "text-align: center; font-weight: bold; margin-bottom: 15px;", "Non-Owner [wage labourers]"),
          
          tags$table(
            style = "width: 100%; border-collapse: collapse; text-align: center;",
            
            # Table Heading
            tags$thead(
              tags$tr(
                tags$th(
                  rowspan = 2, style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Organisational Assets"
                ),
                tags$th(
                  colspan = 3, style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Skills/Credentials Assets"
                )
              ),
              tags$tr(
                tags$th(style = "padding: 10px; background-color: #e8f7fc; border: 1px solid #ccc;", "High"),
                tags$th(style = "padding: 10px; background-color: #e8f7fc; border: 1px solid #ccc;", "Medium"),
                tags$th(style = "padding: 10px; background-color: #e8f7fc; border: 1px solid #ccc;", "Low")
              )
            ),
            
            # Table Body
            tags$tbody(
              tags$tr(
                tags$td(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "High"),
                tags$td(
                  style = ifelse(derived_class == "Expert Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Expert Managers"
                ),
                tags$td(
                  style = ifelse(derived_class == "Semi-Credentialled Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Semi-Credentialled Managers"
                ),
                tags$td(
                  style = ifelse(derived_class == "Uncredentialled Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Uncredentialled Managers"
                )
              ),
              tags$tr(
                tags$td(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Medium"),
                tags$td(
                  style = ifelse(derived_class == "Expert Supervisors", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Expert Supervisors"
                ),
                tags$td(
                  style = ifelse(derived_class == "Semi-Credentialled Supervisors", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Semi-Credentialled Supervisors"
                ),
                tags$td(
                  style = ifelse(derived_class == "Uncredentialled Supervisors", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Uncredentialled Supervisors"
                )
              ),
              tags$tr(
                tags$td(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Low"),
                tags$td(
                  style = ifelse(derived_class == "Expert Non-Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Expert Non-Managers"
                ),
                tags$td(
                  style = ifelse(derived_class == "Semi-Credentialled Workers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Semi-Credentialled Workers"
                ),
                tags$td(
                  style = ifelse(derived_class == "Uncredentialled Workers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                  "Uncredentialled Workers"
                )
              )
            )
          )
        )
      )
    })
    
    
  })
  
  
  output$WrightDefine <- renderUI({
    if (is.null(derived_WrightS())) {
      return(p("Not Enough Information"))
    }
    description <- switch(
      derived_WrightS(),
      "Bourgeoisie" = "Owns sufficient capital to hire workers and not work.",
      "Small Employers" = "Owns sufficient capital to hire workers but must work.",
      "Petty Bourgeoisie" = "Owns sufficient capital to work for self but not to hire workers.",
      "Expert Managers" = "Does not own capital, has high organisational assets and high skill/credential assets.",
      "Expert Supervisors" = "Does not own capital, has average organisational assets and high skill/credential assets.",
      "Expert Non-Managers" = "Does not own capital, has low organisational assets and high skill/credential assets.",
      "Semi-Credentialled Managers" = "Does not own capital, has high organisational assets and average skill/credential assets.",
      "Semi-Credentialled Supervisors" = "Does not own capital, has average organisational assets and average skill/credential assets.",
      "Semi-Credentialled Workers" = "Does not own capital, has low organisational assets and average skill/credential assets.",
      "Uncredentialled Managers" = "Does not own capital, has high organisational assets and low skill/credential assets.",
      "Uncredentialled Supervisors" = "Does not own capital, has average organisational assets and low skill/credential assets.",
      "Uncredentialled Workers" = "Does not own capital, has low organisational assets and low skill/credential assets",
      "Not Enough Information" = "Class could not be derived due to insufficient information."
    )
    
    return(p(description))
  })

  
  # This section covers the functions for the BBC Classic schema # 
  
  derived_NSSEC <- reactiveVal(NULL)
  
  # Observe the submit button and calculate the derived class
  observeEvent(input$submitNSSEC, {
    derived_NSSEC(
      derived_NSSEC <- nssecfull(input)  # Call the nssecfull function and pass the 'input'
    )
  })
  
  # Generate the list of all options with highlighting
  output$nssecsc <- renderUI({
    if (is.null(derived_NSSEC())) {
      return(h4("Please submit your details to find your social class."))
    }
    
    # Define the mapping of class codes to descriptions
    all_classes <- list(
      "1.1" = "Large employers and higher managerial and administrative occupations",
      "1.2" = "Higher professional occupations",
      "2" = "Lower managerial, administrative and professional occupations",
      "3" = "Intermediate occupations",
      "4" = "Small employers and own account workers",
      "5" = "Lower supervisory and technical occupations",
      "6" = "Semi-routine occupations",
      "7" = "Routine occupations"
    )
    
    # Get the derived NSSEC class
    highlighted_class <- derived_NSSEC()
    
    # Create the list with values and descriptions side by side
    tags$ul(
      lapply(names(all_classes), function(code) {
        description <- all_classes[[code]]  # Get the description for the class code
        # Create a formatted item with value and description
        item_text <- paste(code, "-", description)
        
        # If this class code is the selected class, highlight it
        if (code == highlighted_class) {
          tags$li(
            tags$strong(style = "color: red;", item_text)  # Highlight the value and description
          )
        } else {
          tags$li(item_text)  # Normal value and description
        }
      })
    )
  })
  
  output$NSSECDefine <- renderUI({
    if (is.null(derived_NSSEC())) {
      return(p("Not Enough Information"))
    }
    description <- switch(
      derived_NSSEC(),
      "1.1" = "People who employ others of 25+ people, and delegate their managerial functions OR positions in which there is a service relationship with the employer which involve general planning.",
      "1.2" = "Positions, whether occupied by employers, the self-employed or employees, that cover all types of higher professional work.",
      "2" = "Positions that have an attenuated form of service relationship. Employees in these groups generally plan and supervise operations on behalf of the employer under the direction of senior managers.",
      "3" = "Positions in this group are intermediate in terms of employment regulation; they combine elements of both the service relationship and the labour contract.",
      "4" = "People, other than higher or lower professionals, who employ others and so assume some degree of control over them OR Self-employed positions with no employees.",
      "5" = "Positions with a modified form of labour contract and involve formal and immediate supervision of others engaged in such occupations.",
      "6" = "Employees in these positions are regulated by an only slightly modified labour contract typified by a short term and the direct exchange of money for effort.",
      "7" = "These positions have the least need for employee discretion and employees are regulated by a basic labour contract.",
      "Not Enough Information" = "Not Enough Information"
    )
  })


  # This section covers the derivation and output of the RGSC Schema
  
  derived_RGSC <- reactiveVal(NULL)
  
  
  # Observe the submit button and calculate the derived class
  observeEvent(input$submitRGSC, {
    derived_RGSC(
      derived_RGSC <- RGSCF(input)  # Call the nssecfull function and pass the 'input'
    )
  })
  
  # Generate the list of all options with highlighting
  output$RGSC <- renderUI({
    if (is.null(derived_RGSC())) {
      return(h4("Please submit your details to find your social class."))
    }
    
    # Define the mapping of class codes to descriptions
    all_classesrgsc <- list(
      "10" = "Professional Occupations",
      "20" = "Managerial and technical occupations",
      "31" = "Skilled Non-Manual Occupations",
      "32" = "Skilled Manual Occupations",
      "40" = "Partly Skilled Occupations",
      "50" = "Unskilled Occupations",
      "60" = "Armed Forces"
    )
    
    # Get the derived RGSC class
    highlighted_classrgsc <- derived_RGSC()
    
    # Create the list with values and descriptions side by side
    tags$ul(
      lapply(names(all_classesrgsc), function(code) {
        description <- all_classesrgsc[[code]]  # Get the description for the class code
        # Create a formatted item with value and description
        item_text <- paste(code, "-", description)
        
        # If this class code is the selected class, highlight it
        if (code == highlighted_classrgsc) {
          tags$li(
            tags$strong(style = "color: red;", item_text)  # Highlight the value and description
          )
        } else {
          tags$li(item_text)  # Normal value and description
        }
      })
    )
  })
 

  # This section covers the derivation and output of the CAMSIS schema
  
  observeEvent(input$submitCAMSIS, {
    # Calculate the user's CAMSIS score
    user_score <- CAMSISD(input)  # Assuming 'CAMSISD' is the function to calculate the score
    
    # Generate the plot
    output$camsisPlot <- renderPlot({
      # Create a sequence of CAMSIS scores from a normal distribution
      mean_score <- 50
      sd_score <- 15
      camsis_distribution <- rnorm(1000, mean = mean_score, sd = sd_score)
      
      # Plot the distribution
      ggplot(data.frame(x = camsis_distribution), aes(x)) +
        geom_density(fill = "lightblue", alpha = 0.6) +
        geom_vline(xintercept = mean_score, color = "red", linetype = "dashed") +
        geom_vline(xintercept = user_score, color = "blue", linetype = "dotted") +
        annotate("text", x = user_score, y = 0.02, label = paste("Your Score: ", round(user_score, 2)), color = "blue", vjust = -1) +
        labs(title = "CAMSIS Score Distribution", x = "CAMSIS Score", y = "Density") +
        theme_minimal()
    })
    
    # Generate a description based on the user's score
    output$camsisDesc <- renderText({
      mean_score <- 50
      sd_score <- 15
      
      # Compare user's score to mean and generate description
      if (user_score > mean_score + sd_score) {
        desc <- "Your CAMSIS score is above average. You are in the higher percentile of the population."
      } else if (user_score < mean_score - sd_score) {
        desc <- "Your CAMSIS score is below average. You are in the lower percentile of the population."
      } else if (user_score > mean_score - sd_score && user_score < mean_score + sd_score) {
        desc <- "Your CAMSIS score is around the average. You fall within the middle of the population."
      } else {
        desc <- "Your CAMSIS score is exactly the average of the population."
      }
      
      return(desc)
    })
  })
  
  
  # This section covers the derivation and output of the Full Wright schema 
  
# Initialize reactive value
derived_WrightfullS <- reactiveVal(NULL)

# Derive Wright class based on inputs
derive_wrightsfull <- function(capitalt, organisationt, skillst) {
  if (capitalt(input) == 1) {
    result <- "Bourgeoisie"
  } else if (capitalt(input) == 2) {
    result <- "Small Employers"
  } else if (capitalt(input) == 3) {
    result <- "Petty Bourgeoisie"
  } else if (capitalt(input) == 4 & organisationt(input) == 1 & skillst(input) == 1) {
    result <- "Expert Managers"
  } else if (capitalt(input) == 4 & organisationt(input) == 2 & skillst(input) == 1) {
    result <- "Expert Supervisors"
  } else if (capitalt(input) == 4 & organisationt(input) == 3 & skillst(input) == 1) {
    result <- "Expert Non-Managers"
  } else if (capitalt(input) == 4 & organisationt(input) == 1 & skillst(input) == 2) {
    result <- "Semi-Credentialled Managers"
  } else if (capitalt(input) == 4 & organisationt(input) == 2 & skillst(input) == 2) {
    result <- "Semi-Credentialled Supervisors"
  } else if (capitalt(input) == 4 & organisationt(input) == 3 & skillst(input) == 2) {
    result <- "Semi-Credentialled Workers"
  } else if (capitalt(input) == 4 & organisationt(input) == 1 & skillst(input) == 3) {
    result <- "Uncredentialled Managers"
  } else if (capitalt(input) == 4 & organisationt(input) == 2 & skillst(input) == 3) {
    result <- "Uncredentialled Supervisors"
  } else if (capitalt(input) == 4 & organisationt(input) == 3 & skillst(input) == 3) {
    result <- "Uncredentialled Workers"
  } else {
    result <- "Not Enough Information"
  }
  return(result)
}

# Reactive value to store the derived class
derived_WrightSfull <- reactiveVal(NULL)

# Observe the submit button and calculate the derived class
observeEvent(input$submitWrightFull, {
  # Validate and isolate inputs
  capitals_result <- input$capitalt
  organisations_result <- input$organisationt
  skills_result <- input$skillst
  
  # Derive Wright class using these values
  derived_Wrightfull <- derive_wrightsfull(capitals_result, organisations_result, skills_result)
  
  # Update the reactive value
  derived_WrightSfull(derived_Wrightfull)
  
  # Wright class schema and table rendering
  output$wrightfull_table <- renderUI({
    derived_classfull <- derived_WrightSfull()
    
    tags$div(
      style = "font-family: Arial, sans-serif; display: flex; justify-content: space-between; margin: 20px auto; max-width: 100%;",
      
      # First Table (Owners of the Means of Production)
      tags$div(
        style = "width: 48%; padding: 10px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);",
        tags$h4(style = "text-align: center; font-weight: bold; margin-bottom: 15px;", "Owners of the Means of Production"),
        
        tags$table(
          style = "width: 100%; border-collapse: collapse; text-align: center;",
          tags$thead(
            tags$tr(
              tags$th(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Levels of Capital Ownership")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(
                style = ifelse(derived_classfull == "Bourgeoisie", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Bourgeoisie"
              )
            ),
            tags$tr(
              tags$td(
                style = ifelse(derived_classfull == "Small Employers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Small Employers"
              )
            ),
            tags$tr(
              tags$td(
                style = ifelse(derived_classfull == "Petty Bourgeoisie", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Petty Bourgeoisie"
              )
            )
          )
        )
      ),
      
      # Second Table (Non-Owners with Skill Assets and Organizational Assets)
      tags$div(
        style = "width: 48%; padding: 10px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);",
        tags$h4(style = "text-align: center; font-weight: bold; margin-bottom: 15px;", "Non-Owner [wage labourers]"),
        
        tags$table(
          style = "width: 100%; border-collapse: collapse; text-align: center;",
          
          # Table Heading
          tags$thead(
            tags$tr(
              tags$th(
                rowspan = 2, style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Organisational Assets"
              ),
              tags$th(
                colspan = 3, style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Skills/Credentials Assets"
              )
            ),
            tags$tr(
              tags$th(style = "padding: 10px; background-color: #e8f7fc; border: 1px solid #ccc;", "High"),
              tags$th(style = "padding: 10px; background-color: #e8f7fc; border: 1px solid #ccc;", "Medium"),
              tags$th(style = "padding: 10px; background-color: #e8f7fc; border: 1px solid #ccc;", "Low")
            )
          ),
          
          # Table Body
          tags$tbody(
            tags$tr(
              tags$td(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "High"),
              tags$td(
                style = ifelse(derived_classfull == "Expert Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Expert Managers"
              ),
              tags$td(
                style = ifelse(derived_classfull == "Semi-Credentialled Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Semi-Credentialled Managers"
              ),
              tags$td(
                style = ifelse(derived_classfull == "Uncredentialled Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Uncredentialled Managers"
              )
            ),
            tags$tr(
              tags$td(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Medium"),
              tags$td(
                style = ifelse(derived_classfull == "Expert Supervisors", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Expert Supervisors"
              ),
              tags$td(
                style = ifelse(derived_classfull == "Semi-Credentialled Supervisors", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Semi-Credentialled Supervisors"
              ),
              tags$td(
                style = ifelse(derived_classfull == "Uncredentialled Supervisors", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Uncredentialled Supervisors"
              )
            ),
            tags$tr(
              tags$td(style = "padding: 10px; background-color: #f4f4f4; border: 1px solid #ccc;", "Low"),
              tags$td(
                style = ifelse(derived_classfull == "Expert Non-Managers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Expert Non-Managers"
              ),
              tags$td(
                style = ifelse(derived_classfull == "Semi-Credentialled Workers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Semi-Credentialled Workers"
              ),
              tags$td(
                style = ifelse(derived_classfull == "Uncredentialled Workers", "color: #FF0000; font-weight: bold; padding: 10px; border: 1px solid #ccc;", "color: inherit; padding: 10px; border: 1px solid #ccc;"),
                "Uncredentialled Workers"
              )
            )
          )
        )
      )
    )
  })
})

# Description Rendering
output$WrightDefinefull <- renderUI({
  if (is.null(derived_WrightSfull())) {
    return(p("Not Enough Information"))
  }
  
  description <- switch(
    derived_WrightSfull(),
    "Bourgeoisie" = "Owns sufficient capital to hire workers and not work.",
    "Small Employers" = "Owns sufficient capital to hire workers but must work.",
    "Petty Bourgeoisie" = "Owns sufficient capital to work for self but not to hire workers.",
    "Expert Managers" = "Does not own capital, has high organisational assets and high skill/credential assets.",
    "Expert Supervisors" = "Does not own capital, has average organisational assets and high skill/credential assets.",
    "Expert Non-Managers" = "Does not own capital, has low organisational assets and high skill/credential assets.",
    "Semi-Credentialled Managers" = "Does not own capital, has high organisational assets and average skill/credential assets.",
    "Semi-Credentialled Supervisors" = "Does not own capital, has average organisational assets and average skill/credential assets.",
    "Semi-Credentialled Workers" = "Does not own capital, has low organisational assets and average skill/credential assets.",
    "Uncredentialled Managers" = "Does not own capital, has high organisational assets and low skill/credential assets.",
    "Uncredentialled Supervisors" = "Does not own capital, has average organisational assets and low skill/credential assets.",
    "Uncredentialled Workers" = "Does not own capital, has low organisational assets and low skill/credential assets",
    "Not Enough Information" = "Class could not be derived due to insufficient information."
  )
  
  return(p(description))
})

  
  
  
}


shinyApp(ui, server)

# End #



