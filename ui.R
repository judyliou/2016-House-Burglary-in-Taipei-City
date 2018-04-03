library(shiny)

navbarPage("2016 House Burglary in Taipei City",
  
  #Map
  tabPanel("Burglary Map",
    sidebarLayout(      
        sidebarPanel(radioButtons("map", label = h4("Location of Burglaries & Surveillance Cameras"),
                                 choices = list("Burglaries" = 'Burglaries', 
                                                "Surveillance Cameras" = 'Surveillance Cameras', 
                                                "Both" = 'Both'))),
                        
        mainPanel(h4('Taipei Burglaries and Surveillance Cameras Map'),
                  plotOutput("map"))#, width = "120%"))
        )),

  #Cameras in 1km vs Repeat times
  tabPanel("Cameras within 1 km",
    sidebarLayout(
      sidebarPanel(radioButtons("district", label = h4("District"),
                                choices = list("Shilin" = 1,"Datong" = 2,"Da'an" = 3,
                                               "Zhongshan" = 4,"Zhongzhen" = 5,"Neihu" = 6,
                                               "Wenshan" = 7,"Beitou" = 8,"Songshan" = 9,
                                               "Xinyi" = 10,"Nangang" = 11,"Wanhua" = 12))),
      mainPanel(h4('Repeated Burglaries with Number of Nearby Cameras'),
                plotOutput("nearbyCam_all"),
                h4("In Certain District"),
                plotOutput("nearbyCam_certain"))
    )),

  #Postcheck and Prediction
  tabPanel("Burglary Rate",
           sidebarLayout(
             sidebarPanel(
               sliderInput(inputId = "survDen",
                           label = "Surveillance Camera Density\n(per hactare)",
                           min = 0,
                           max = 3,
                           value = 1,
                           step = 0.1),
               sliderInput(inputId = "station",
                           label = "Number of Police Station",
                           min = 5,
                           max = 20,
                           value = 10),
               sliderInput(inputId = "lowIncome",
                           label = "People with Mid and Low Income",
                           min = 0,
                           max = 2500,
                           value = 1000),
               sliderInput(inputId = "avgSurv",
                           label = "Average Number of Cameras within 1 km",
                           min = 1,
                           max = 100,
                           value = 50),
               sliderInput(inputId = "disposable",
                           label = "Disposable Income (million)",
                           min = 0,
                           max = 2,
                           value = 1,
                           step = 0.1)
             ),

             mainPanel( h4("Burglary Rate Prediction"),
                       plotOutput("theftRate"),
                       verbatimTextOutput("coef"),
                       plotOutput("postcheck"))
           ))
 )