

 ## Shiny: shiny app using baby names data
 ## Name:  Fiza Gupta
 ## ID:    fxg180000


 # ------------ Auto install and load required packages ------------

 if (!require('pacman'))install.packages('pacman')
 pacman::p_load(tidyverse,plotly,readxl,ggplot2,shinyjs,shiny,DT,readxl)
 search()


 # --------------READ EXCEL FILE------------------------------------------

 #load data
 df.girl <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet = "Girls' Names", range = "C8:GN107",col_names = FALSE)
 df.boy <-  read_excel("Top100_Popular_Baby_Names.xlsx", sheet = "Boys' Names", range = "C8:GN107",col_names = FALSE)


 # ---------------CLEAN DATA--------------------------------------------

 # Clean data and create a new dataframe for girls and boys

 # new dataframe for girl names 
 df_girl  <- data.frame(matrix(ncol=3,nrow=0))
 x<- c("Name","Year","Count")
 colnames(df_girl) <- x

 #append data to new dataframe 
 for (j in 1:65)  {
   for (i in 1:100) {
     df_girl[100*j-100+i,"Name"] = df.girl[i,3*j-2]
     df_girl[100*j-100+i,"Year"] =1953+j
     df_girl[100*j-100+i,"Count"]=df.girl[+i,3*j-1]
   }
 }

 # new dataframe for boys names   
 df_boy  <- data.frame(matrix(ncol=3,nrow=0))
 x<- c("Name","Year","Count")
 colnames(df_boy) <- x

 #append data to new dataframe 
 for (j in 1:65)  {
   for (i in 1:100) {
     df_boy[100*j-100+i,"Name"] = df.boy[i,3*j-2]
     df_boy[100*j-100+i,"Year"] =1953+j
     df_boy[100*j-100+i,"Count"]=df.boy[+i,3*j-1]
   }
 }

 # --------------- CONVERSION and KNOW ABOUT YOUR DATA------------------

 #convert datatypes and have a overview about the data

 dim(df_girl)
 #write.csv(df_girl, "df_girl.csv")
 df_girl$Count <- as.numeric(df_girl$Count)
 #df_girl$Year <- as.factor(df_girl$Year)
 glimpse(df_girl)

 dim(df_boy)
 write.csv(df_boy, "df_boy.csv")
 df_boy$Count <- as.numeric(df_boy$Count)
 #df_boy$Year <- as.factor(df_boy$Year)
 glimpse(df_boy)


 # ------ CREATE A SHINY APP -------------------------------------

 # Define UI for application that plots features of baby names 
 # Reactivity using baby name files

 ui <- fluidPage(
  
   # formatting of the titlePanel heading
   titlePanel(h1(div(HTML("<em>Popularity of Baby Names in New Zealand (1954 - 2018)</em>")),
                 style='background-color:#AEB6BF;
                      padding-left: 15px; color:#EAEDED ;padding-left:230px;font-size:30px')),
  
  # formatting of the tablabel 
  tags$style(HTML("
   
     .tabbable > .nav > li > a[data-value='Trending Names - Girls'] {background-color: #F5CBA7;   color:black}
     .tabbable > .nav > li > a[data-value='Trending Names - Boys'] {background-color: #AED6F1;   color:black}
     .tabbable > .nav > li > a[data-value='Top Names - Girls'] {background-color: #F5CBA7;   color:black}
     .tabbable > .nav > li > a[data-value='Top Names - Boys'] {background-color: #AED6F1;   color:black}
   
   ")),
  
  #create 4 tabs
  tabsetPanel(
    
    #tab1
    tabPanel( "Trending Names - Girls",
              
              # formatting of tabPanel heading
              h2(div(HTML("<em> Trending of Baby Girl Names </em>")), align = 'center',
                 style= 'background-color:#F5CBA7; color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout( 
                
                sidebarPanel( width = 3,
                  
                  selectInput( 
                    
                    # Inputs variable
                    inputId = "name",
                    label = "Girl Name" ,
                    choices = c(df_girl$Name),
                    selected =  NULL
                  ),
                  
                  h5("Select Name from dropdown: To see its popularity over time")
                ),
                
                # Outputs variables
                mainPanel( 
                  
                  plotlyOutput("line_girl"),
                  
                  htmlOutput(outputId = "val")
                )
                
              )
    ),
    
    #tab2
    tabPanel( "Trending Names - Boys",
              
              # formatting of tabPanel heading
              h2(div(HTML("<em> Trending of Baby Boy Names </em>")), align = 'center',
                 style= 'background-color:#AED6F1; color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout( 
                
                sidebarPanel(  width = 3,
                  
                  # Inputs variables
                  selectInput(
                    inputId = "name_boy",
                    label = "Boy Name" ,
                    choices = c(df_boy$Name)
                  ),
                  
                  h5("Select Name from dropdown: To see its popularity over time")
                ),
                
                # Output variables for line graph
                mainPanel(
                  
                  plotlyOutput("line_boy"),
                  htmlOutput(outputId = "val_boy")
                )
                
              )
    ),
    
    #tab3
    tabPanel( "Top Names - Girls",
              
              # formatting of tabPanel heading
              h2(div(HTML("<em>The Most Popular Girl Names by Year</em>")), align = 'center',
                                     style='background-color:#F5CBA7;
                       color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout(
                
                sidebarPanel(
                  
                  selectInput(
                    
                    # Inputs variables
                    inputId = "year_girl",
                    label = "Year" ,
                    choices = as.character(unique(df_girl$Year)),
                    #selected = "Susane"
                  ),
                  
                  numericInput(
                    
                    # Inputs
                    inputId = "top_girl",
                    label = "Top N" ,
                    value = 10,
                    min = 1,
                    max = 100,
                    step = 1
                  ),
                  
                  h5("Select N: To display top N(default - 10) names used for a year selected")
                  
                ),
                
                # Output
                mainPanel(
                  
                  dataTableOutput("table_girl")
                )
                
              )
    ),
    #tab4
    tabPanel( "Top Names - Boys",
              
              # formatting of tabPanel heading
              h2(div(HTML("<em>The Most Popular Boy Names by Year</em>")), align = 'center',
                 
                 style='background-color:#AED6F1; color:#2C3E50 ;padding-left:70px;font-size: 20px'),
              
              # Sidebar layout with a input and output definitions
              sidebarLayout(
                
                sidebarPanel( 
                  
                  selectInput(
                    
                    # Inputs
                    inputId = "year_boy",
                    label = "Year" ,
                    choices = as.character(unique(df_boy$Year)),
                    #selected = "Susane"
                  ),
                  
                  numericInput(
                    
                    # numeric input for selecting top n names
                    inputId = "top_boy",
                    label = "Top N" ,
                    value = 10,
                    min = 1,
                    max = 100,
                    step = 1
                  ) ,
                  
                  h5("Select N: To display top N(default - 10) names used for a year selected")
                  
                ),
                
                # Output 
                mainPanel(
                  
                  dataTableOutput("table_boy")
                )
                
              )
    )
    
  ) 
  
 )

 # CREATE SERVER FUCNTION
 # Define server function required to create kine graph and table

 server <- function(input, output, session) {
  
  # TAB1 OUTPUT1 PLOTTING
  
  # Create line graph
  output$line_girl <- renderPlotly({
    req(input$name)
    
    data1 = filter(df_girl, Name %in% input$name)
    idx <- c(1, diff(data1$Year))
    i2 <- c(1, which(idx != 1), nrow(data1) + 1)
    
    data1$grp <- rep(1:length(diff(i2)), diff(i2))
    
    # if nothing selected blank screen
    if (identical(input$name, "")) return(NULL)
    
    #plot line graph
    p <- ggplot(data = data1 ) + 
      geom_line(aes(Year, Count, group = grp) , color = "red") +
      geom_bar(aes(x=Year, y=Count),stat="identity", width = .5, fill="#F5CBA7")+
      scale_x_continuous(limits= c(1953, 2019), breaks = seq(1954, 2018, by = 1)) +
      geom_point(aes(x=Year, y= Count), size = 1, color = "red") +
     # geom_text(aes(x=Year, y= Count, label = Count), hjust = -10,  size = 2.2 , color = "black") +
      theme(plot.title = element_text(color="red", size=14, hjust = 5.0, vjust = 5.0, face="bold.italic"),
            axis.text.x = element_text(angle = 90,face="bold", size = 7, vjust = 1.0, hjust = 1.0),
            axis.title.x = element_text(face="bold"),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 18),
            axis.title.y = element_text(face="bold")) +
      scale_y_continuous(breaks = seq(0, 1000, by = 50))+
      xlab("Year") + ylab("Frequency of name used")
    height <- session$clientData$output_p_height
    width <- session$clientData$output_p_width
    
    # for interactive plot
    ggplotly(p, height = height, width = width)
  })
  
  # TAB1 OUTPUT2 TEXT
  
  #Create text output stating yaers in which max and min times name used 
  output$val <- renderUI({
    
    if (identical(input$name, "")) return(NULL)
    data = filter(df_girl, Name %in% input$name)
    data1 = filter(data, Count %in% max(data$Count))
    data2 = filter(data, Count %in% min(data$Count))
    
    #formatting of the text displayed
    div(HTML(
      paste0("<b> Note: </b>",
             " For ", input$name, ", Maximum times name used is ", '"', max(data$Count), '"',  " in ",  '"', data1$Year, '"' ,
             " and Minimum times name used is ", '"', min(data$Count), '"',  " in ", '"', data2$Year, '"', '<br>')),
      style='background-color:#F5CBA7;padding-left: 15px; color:black ;padding-left:20px;')
    
  })
  
  # TAB2 OUTPUT
  output$line_boy <- renderPlotly({
    req(input$name_boy)
    
    data1 = filter(df_boy, Name %in% input$name_boy)
    idx <- c(1, diff(data1$Year))
    i2 <- c(1, which(idx != 1), nrow(data1) + 1)
    
    data1$grp <- rep(1:length(diff(i2)), diff(i2))

    # if nothing selected blank screen
    if (identical(input$name_boy, "")) return(NULL)
    
    #plot line graph
    p <- ggplot(data = data1) + 
      geom_bar(aes(x=Year, y=Count),stat="identity", width = .5 , fill="#AED6F1")+
      scale_x_continuous(limits= c(1953, 2019), breaks = seq(1954, 2018, by = 1)) +
      geom_point(aes(x=Year, y= Count), size = 1, color = "blue") +
      #geom_text(aes(x=Year, y= Count, label = Count), size = 1.2 ,  color = "black") +
      geom_line(aes(Year, Count, group = grp) ,color = "blue") +
      theme(plot.title = element_text(color="blue", size=14, hjust = 5.0, vjust = 5.0, face="bold.italic"),
            axis.text.x = element_text(angle = 90, face = "bold" , size = 7, vjust = 2.0, hjust = 2.0),
            axis.title.x = element_text(face="bold"),
            axis.title.y = element_text(face="bold")) +
      scale_y_continuous(breaks = seq(0, 1600, by = 50))+
      xlab("Year") + ylab("Frequency of name used")
    
    height <- session$clientData$output_p_height
    width <- session$clientData$output_p_width
    
    # for interactive plot
    ggplotly(p, height = height, width = width)
    
  })
  
  #Create text output stating yaers in which max and min times name used 
  output$val_boy <- renderUI({
    
    # if nothing selected blank screen
    if (identical(input$name_boy, "")) return(NULL)
    data = filter(df_boy, Name %in% input$name_boy)
    data1 = filter(data, Count %in% max(data$Count))
    data2 = filter(data, Count %in% min(data$Count))
    
    #formatting of the text displayed
    div(HTML(
      paste0("<b> Note: </b>",
             " For ", input$name_boy, ", Maximum times name used is ", '"', max(data$Count), '"',  " in ",  '"', data1$Year, '"' ,
             " and Minimum times name used is ", '"', min(data$Count), '"',  " in ", '"', data2$Year, '"', '"', '<br>' )),  
      style='background-color:#AED6F1;
                      padding-left: 15px; color:black ;padding-left:20px;')
    
  })
  
  # TAB3 OUTPUT
  
  # Create data table to display top names in year selected
  output$table_girl <- DT::renderDataTable({
    
    req(input$year_girl)
    data_girl = filter(df_girl, Year %in% input$year_girl)
    
    #formatting of the table displayed
    datatable(head(data_girl[order(data_girl$Count, decreasing = TRUE),], input$top_girl), rownames = TRUE, 
              options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
      formatStyle(columns = c("Year","Count","Name"),textAlign = 'center',fontSize = '80%')  %>% 
      formatStyle(columns = c("Year","Count","Name"), target = "cell", backgroundColor = "#FFEEFF") %>% 
      formatStyle(c(1:dim(data_girl)[2]), border = '1px solid #ABB2B9')
  }) 
  
  # TAB4 OUTPUT
  
  # Create data table to display top names in year selected
  output$table_boy <- DT::renderDataTable({
    
    req(input$year_boy)
    data_boy = filter(df_boy, Year %in% input$year_boy)
    
    #formatting of the table displayed
    datatable(head(data_boy[order(data_boy$Count, decreasing = TRUE),], input$top_boy), rownames = TRUE, 
              options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
      formatStyle(columns = c("Year","Count","Name"),textAlign = 'center',  fontSize = '80%')  %>% 
      formatStyle(columns = c("Year","Count","Name"), target = "cell", backgroundColor = "#E9F5FE") %>% 
      formatStyle(c(1:dim(data_boy)[2]), border = '1px solid #ABB2B9')
    
  }) 
  
 }

 # ---------------------RUN THE APP-------------------------------------
 
 # Create a Shiny app object
 shinyApp(ui, server)


