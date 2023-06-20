library(shiny)
library(shinythemes)
library(randomForest)
library(RCurl)
library(caret)
library(shinyWidgets)
library(plotly)
library(DT)
library(rsconnect)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(leaflet)
library(digest)
library(shinyalert)
library(shinyjs)

data = read.csv("C:/Users/HP/Desktop/Shiny/Exercises/Project/Deaths_by_Risk Factors.csv")
data1 = read.csv("C:/Users/HP/Desktop/Shiny/Exercises/Project/Second_Data.csv")
data2 = data1
data3 = read.csv("C:/Users/HP/Desktop/Shiny/Exercises/Project/Deaths_by_Risk Factors1.csv")

w <- map_data('world', region = c("India","Vietnam","Denmark","Ecuador","Sweden","Taiwan","Turkey","UK","USA","Ukraine","Thailand","Switzerland","Russia","Spain","Sri Lanka","South Korea","South Africa","Singapore","Serbia","Scotland","Saudi Arabia","Romania","Portugal","Egypt","Ethiopia","Finland","France","Germany","Greece","Chile","Cuba","Czechia","Ireland","Kazakhstan","Japan","Italy","Iraq","Iran","Indonesia","Kenya","Libya","Nepal","North Korea","Poland","Philippines","North Macedonia","Nigeria","New Zealand","Netherlands","Myanmar","Morocco","Mongolia","Moldova","Mexico","Mali","Malaysia","Lithuania","Australia","Albania","Algeria","Argentina","Brazil","Zimbabwe","Austria","Yemen","Venezuela","China","Bangladesh","Canada","Hungary","Guatemala","England","Pakistan"))

Data9 <- read.csv(file.choose(), header =T)
Country1 <- Data9 %>%
  filter(Entity %in% c("India","Vietnam","Denmark","Ecuador","Portugal","Russia","Taiwan","Turkey","UK","USA","Ukraine","Thailand","Switzerland","Sweden","Egypt","Sri Lanka","Spain","South Korea","South Africa","Singapore","Serbia","Scotland","Saudi Arabia","Romania","Ethiopia","Finland","France","Germany","Greece","Chile","Cuba","Czechia","Iran","Iraq","Ireland","Kazakhstan","Japan","Italy","Australia","Indonesia","Kenya","Libya","North Korea","Philippines","Poland","North Macedonia","Nepal","Nigeria","New Zealand","Netherlands","Myanmar","Morocco","Mongolia","Moldova","Mexico","Mali","Malaysia","Lithuania","Albania","Algeria","Argentina","Brazil","Zimbabwe","Austria","Yemen","Venezuela","China","Bangladesh","Canada","England","Hungary","Guatemala","Pakistan"))
#-------------------------------------------------------------------------------------------
colnames(Country1)[1]  <- "region"
dataset <- inner_join(w,Country1,by="region")
#------------------------------------------------


ui <- navbarPage(
  "Global.org",
  theme = shinytheme("flatly"),
  tabPanel(
    "Home",
    titlePanel(div(
      img(src = "Risk.jpg", width="100%", height="435px", align="centre", class="bg"),
    )),
    
    tags$br(),
    
    tabsetPanel(
      type="tabs",
      tabPanel(
        h3("Summary"),
        sidebarLayout(
        sidebarPanel(
          img(src = "Summary_img.jpg", width="100%", class="bg"),
        ),
        mainPanel(
          tags$b(p("Background", style = "font-family: 'times'; font-size: 20pt; text-align: left")),
          p("It is important to understand what is meant by the cause of death and the risk factor associated with a premature death.
            In the epidemiological framework of the Global Burden of Disease study each death has one specific cause. 
            In their own words: ‘each death is attributed to a single underlying cause — the cause that initiated the 
            series of events leading to death’.", style = "font-family: 'times'; font-size: 16pt; text-align: justify"),
          tags$br(),
          tags$b(p("Why do People die from?", style = "font-family: 'times'; font-size: 20pt; text-align: left")),
          p("This is different from the deaths that happened due to risk factors. 
            These deaths are an estimation of the reduction of the number of deaths 
            that would be achieved if the risk factors to which a population is exposed 
            would be eliminated (in the case of tobacco smoking, for example) or 
            reduced to an optimal, healthy level (in the case of body-mass index).", style = "font-family: 'times'; font-size: 16pt; text-align: justify"),
          p("The Global Burden of Disease is a major global study on the causes of death and 
            disease published in the medical journal The Lancet. These estimates of the annual
            number of deaths by cause are shown here.
            Deaths from causes such as infectious disease, malnutrition, nutritional
            deficiencies, neonatal and maternal deaths are common – and in some cases 
            dominant – across low- and middle-income nations. In Kenya, for example, 
            the leading cause of death remains diarrheal diseases. In South Africa and 
            Botswana, the leading cause of death is HIV/AIDS. In high-income countries 
            however the share of deaths caused by these is very low.",style = "font-family: 'times'; font-size: 16pt; text-align: justify"),
          p("In 2017, there were 56.5 million deaths globally; just over half of these were 
            people who were 70 years or older; 26% were between 50 and 69 years old; 13% were
            between 15 and 49; only 1% were older than 5 and younger than 14; and almost 9% were
            children under the age of 5.", style = "font-family: 'times'; font-size: 16pt; text-align: justify"),
          p("The age at which people die has changed significantly since 1990. Fewer people 
            die at a young age. In 1990 nearly one-quarter of all deaths were in children 
            younger than 5. In 2019, this had declined to just under 9%. In contrast, the share
            of deaths in the over-70s age bracket has increased from a third to half of all deaths over this period.",
            style = "font-family: 'times'; font-size: 16pt; text-align: justify"),
          p(" In countries with good health the share dying at a young age is very low. In Japan more than 85% are 70 years or older.",
            style = "font-family: 'times'; font-size: 16pt; text-align: justify"),
        ),),
        tags$hr()),
      ),
    
    ),
  tabPanel("Risk Analysis",
           sidebarLayout(
             sidebarPanel(
               h3("Data by Year"),
               tags$br(),
               selectInput("year", "Select Year", choices = unique(data$Year)),
               selectInput("country", "Select Country", choices = unique(data$Entity)),
               selectInput("code", "Country Code", choices = unique(data$Code)),
             ),
             
             mainPanel(
               tabsetPanel(
                 type="tabs",
                 tabPanel("High Risks",tableOutput("datahead")),
                 tabPanel("Outdoor_air_pollution",plotOutput(outputId ="piePlot"))
               ),
               tableOutput("data"),
               tags$br(),
               tags$br(),
             )
           )
           ),
  tabPanel("Dashboard",
           dashboardPage(
             dashboardHeader(title = "Interactive Dashboard"),
             dashboardSidebar(
               sidebarMenu(
                 menuItem("Anual Death Rates", tabName = "dashboard", icon = icon("dashboard")),
                 menuItem("Age Analysis", tabName = "analysis", icon = icon("bar-chart")),
                 menuItem("Country Analysis", tabName = "analysis1", icon = icon("bar-chart")),
                 menuItem("Health Analysis", tabName = "analysis2", icon = icon("bar-chart")),
                 menuItem("Total_Deaths",tabName="total_deaths",icon=icon("bar-chart")),
                 menuItem("Addictive to Drugs",tabName="Addictive",icon=icon("bar-chart")),
                 menuItem("Data Insertion", tabName = "insert", icon = icon("bar-chart")),
                 menuItem("Data Updation", tabName = "update", icon = icon("bar-chart")),
                 menuItem("Record Deletion", tabName = "delete", icon = icon("bar-chart")),
                 menuItem("Top 10 Countries", tabName = "Top_Countries", icon = icon("bar-chart")),
                 menuItem("Dataset",tabName = "Data", icon= icon("dashboard"))
                 
               )
             ),
             dashboardBody(
               tabItems(
                 tabItem(tabName = "dashboard",
                         h3("Anual Death rates all over the world"),
                         tags$br(),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("country2", "Select Country", choices = unique(data$Entity)),
                             tableOutput("table1")),
                         
                            mainPanel(
                              tags$br(),
                              tags$br(),
                            plotOutput("chart"))),
                         
                 ),
                 tabItem(tabName = "analysis",
                         h3("Age-wise Death Analysis from 1990 - 2019"),
                         tags$br(),
                         fluidRow(
                           box(plotlyOutput("plot1",height = 325)),
                           box(
                             title="Controls",
                             selectInput("country3", "Select Country", choices = unique(data1$Entity)),
                             tableOutput("table2")                           
                             ),
                           box(height=350,
                               title = "Controls",
                               selectInput("year3","Select Year",choices = unique(data1$Year)),
                               tableOutput("table3")),
                           box(plotOutput("plot2",height=350))
                         )
                    ),
                 tabItem(tabName = "Data",
                              h2("Total Number of Deaths by High Risk Factors all over the world (1990 - 2019)"),
                              tags$br(),
                              DT::dataTableOutput("dataset")
                         ),
                 tabItem(tabName = "analysis1",
                         h3("Geographical Death Analysis"),
                         fluidRow(
                           
                           box(title="controls",
                               selectInput("risk","Select Risk Factor", choices = names(data)[-(1:3)]),
                               selected = names(data)[-(1:3)][1]),
                           plotOutput("map")
                         )
                         ),
                 tabItem(tabName= "analysis2",
                         h3("Health Based Risk Factors in Each Country"),
                         fluidRow(
                           box(title="Controls",
                               selectInput("health","Select Country", choices = unique(data1$Entity)),
                               tableOutput("table4")),
                           box(plotlyOutput("plot3",height=350))
                           
                         )),
                 tabItem(
                   tabName = "total_deaths",
                   fluidRow(
                     h3("Total Deaths"),
                     tags$br(),
                     h2("Total Number of deaths all over the world from 1990 - 2019", style="text-align: center; font-family: 'calibri'"),
                     tags$br(),
                     sidebarPanel(
                       selectInput("country1", "Select Country", choices = unique(data$Entity)),
                       tableOutput("table")),
                     mainPanel(
                       box(width=500, height= 400,plotOutput("barplot")),
                       tags$br(),
                       box(width=500, height= 400,plotOutput("lineplot"))))
                 ),
                 tabItem(tabName="insert",
                         fluidRow(h3("You can enter a record for any country if you know the exact death count for each parameter",style="text-align: center; font-family: 'calibri'"),
                           box(textInput("input1", "Enter Country Name:", value = ""),
                               textInput("input2", "Enter Country Code:", value = ""),
                               numericInput("input3", "Enter Year:", value = "",min = 1900, max = 2024),
                               numericInput("input4", "Deaths by Outdoor air Pollution:", value = ""),
                               numericInput("input5", "Deaths by High systolic blood pressure :", value = ""),
                               numericInput("input6", "Deaths by Diet high in sodium:", value = ""),
                               numericInput("input7", "Deaths by Diet low in whole grains:", value = ""),
                               numericInput("input8", "Deaths by Alcohol_use:", value = ""),
                               numericInput("input9", "Deaths by Diet low in fruits:", value = ""),
                               numericInput("input10", "Deaths by unsafe water source:", value = ""),
                               numericInput("input11", "Deaths by secondhand smoke:", value = ""),
                               numericInput("input12", "Deaths by low birth weight:", value = ""),
                               numericInput("input13", "Deaths by Child wasting:", value = ""),
                               numericInput("input14", "Deaths by unsafe sex:", value = ""),
                               numericInput("input15", "Deaths by diet low in nuts and seeds:", value = ""),
                               actionButton("submit", "Submit")),
                           box(numericInput("input16", "Deaths by Household air pollution:", value = ""),
                               numericInput("input17", "Deaths by Diet low in vegetables:", value = ""),
                               numericInput("input18", "Deaths by low physical activity:", value = ""),
                               numericInput("input19", "Deaths by smoking:", value = ""),
                               numericInput("input20", "Deaths by High fasting plasma glucose:", value = ""),
                               numericInput("input21", "Deaths by air pollution:", value = ""),
                               numericInput("input22", "Deaths by high BMI:", value = ""),
                               numericInput("input23", "Deaths by Unsafe sanitation:", value = ""),
                               numericInput("input24", "Deaths by no access to handwash facility:", value = ""),
                               numericInput("input25", "Deaths by drug use:", value = ""),
                               numericInput("input26", "Deaths by low bone mineral density:", value = ""),
                               numericInput("input27", "Deaths by vitamin A deficiency:", value = ""),
                               numericInput("input28", "Deaths by Child Stunting:", value = ""),
                               numericInput("input29", "Deaths by Discontinued breastfeeding:", value = ""),
                               numericInput("input30", "Deaths by Non exclusive Breastfeeding:", value = ""),
                               numericInput("input31", "Deaths by Iron deficiency:", value = "")),
                           uiOutput("unique_id_message")
                         )),
                 tabItem(
                   tabName = "update",
                   fluidRow(h3("You can Update a record for any country by using Unique ID",style="text-align: center; font-family: 'calibri'"),
                            column(6,
                            box(numericInput("unique", "Enter Unique ID:", value = "")),
                            box(uiOutput("inputs"))),
                            box(checkboxGroupInput("selected_values", "Select Columns to Update", 
                                               choices = names(data3)[-(1:4)]),
                            actionButton("update", "Update",onclick = "shinyalert(message = 'Your record has been updated successfully!', type = 'success')"),
                            ))
                   
                            
                   ),
                 tabItem(
                   tabName = "delete",
                   fluidRow(useShinyjs(),
                     box(numericInput("Unique_del", "UNIQUE_ID", value="")),
                            box(actionButton("delete", "Delete")),
                            verbatimTextOutput("result")
                            )
                 ),
                 tabItem(
                   tabName = "Top_Countries",
                   fluidRow(useShinyjs(),
                            box(selectInput("year_input", "Select Year", choices = unique(data$Year)),
                              tableOutput("Country_table")),
                            box(plotOutput("barplot1")))
                 ),
                 
                 tabItem(tabName = 'Addictive',
                         fluidPage(
                           selectInput("Plot_year","Select Year",choices = unique(data1$Year)),
                           selectInput("Plot_country","Select Country",choices = unique(data$Entity)),
                           fluidRow(
                             shinydashboard::box(title = 'Addictive to Drugs and Alcohol',solidHeader = T, background = 'black',
                                                 tabBox(width = 12,
                                                        title = NULL,
                                                        id = 'tabset3', height = '300px',
                                                        tabPanel(tags$p('Year wise addictive to drugs', style = 'color:black;font-weight:bold;'), span(
                                                          uiOutput('Addiction'), style ='color:red')),
                                                        tabPanel(tags$p('Country Wise addictive to drugs', style = 'color:black;font-weight:bold;'), span(
                                                          uiOutput('Country_Addiction'), style ='color:red'))
                                                 ))
                             )))
                 )
                 
               )
             )
           )
  )

server <- function(input, output, session) {
  data3 = read.csv("C:/Users/HP/Desktop/Shiny/Exercises/Project/Deaths_by_Risk Factors1.csv")
  year <- reactive({
    filter(data, Year == input$year)
  })
  observeEvent(year(), {
    choices <- unique(year()$Entity)
    updateSelectInput(inputId = "country", choices = choices) 
  })
  
  country <- reactive({
    req(input$country)
    filter(year(), Entity == input$country)
  })
  observeEvent(country(), {
    choices <- unique(country()$Code)
    updateSelectInput(inputId = "code", choices = choices)
  })
  
  output$data <- renderTable({
    req(input$code)
    country() %>% 
      filter(Code == input$code) %>% 
      select(Outdoor_air_pollution,High_systolic_blood_pressure,Diet_high_in_sodium,Diet_low_in_whole_grains
)
  })
  output$dataset = DT::renderDataTable({
    data
  })
  output$table <- renderTable({
    setDT(data)
    data_subset <- data[Entity == input$country1, rowSums(.SD, na.rm = TRUE), .SDcols = -c(1,2,3)]
    data_table <- data.table(Year = data[Entity == input$country1,Year], sum = data_subset)
    data_table
  })
  output$barplot <- renderPlot({
    data_subset <- data[Entity == input$country1, rowSums(.SD, na.rm = TRUE), .SDcols = -c(1,2,3)]
    data_table <- data.table(Year=data[Entity == input$country1,Year], Sum=data_subset)
    ggplot(data=data_table, aes(x=Year, y=Sum, color="orange")) + geom_bar(stat="identity")
  })
  output$lineplot <- renderPlot({
    data_subset <- data[Entity == input$country1, rowSums(.SD, na.rm = TRUE), .SDcols = -c(1,2,3)]
    data_table <- data.table(Year=data[Entity == input$country1,Year], Sum=data_subset)
    ggplot(data=data_table, aes(x=Year, y=Sum, color="green")) + geom_line()
  })
  #---------------------------------------------------------------
  filtered_data <- reactive({
    data %>% filter(Entity == input$country2)
  })
  column_sums <- reactive({
    colSums(filtered_data()[,-c(1:3)], na.rm = TRUE)
  })
  sorted_sums <- reactive({
    sort(column_sums(), decreasing = TRUE)
  })
  top_10_attributes <- reactive({
    names(head(sorted_sums(), 10))
  })
  output$chart <- renderPlot({
    ggplot(data.frame(attributes = reorder(top_10_attributes(),sorted_sums()[top_10_attributes()]), values = sorted_sums()[top_10_attributes()]), aes(x = attributes, y = values, fill = attributes)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = color_vector())+
      xlab("Attributes") +
      ylab("Sum") +
      ggtitle(paste0("Top 10 high risk factor in ",input$country2," for the past 29 years")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      coord_flip()
  })
  output$table1 <- renderTable({
    data.frame(Risk_Factors = top_10_attributes(), Total_Deaths = sorted_sums()[top_10_attributes()])
  })
  color_vector <- reactive({
    if(input$country2 == "India"){
      c("red", "green", "blue", "orange", "purple", "pink", "yellow", "black", "gray", "brown")
    }else{
      c("green", "blue", "red", "purple", "pink", "yellow", "black", "gray", "brown", "orange")
    }
  })
  #-----------------------------------------------------------------------------------------------------
  data_filtering2 <- reactive(
    data1 %>% filter(Entity == input$country3)
  )
  column_2 <- reactive({
    colSums(data_filtering2()[,-c(1:3)], na.rm = TRUE)
  })
  sorted_2 <- reactive({
    sort(column_2(), decreasing = TRUE)
  })
  top_13_attributes <- reactive({
    names(head(sorted_2(), 5))
  })
  output$table2 <- renderTable({
    data.frame(Age_Group = top_13_attributes(), Total_Deaths = sorted_2()[top_13_attributes()])
  })
  output$plot1 <- renderPlotly({
    data1 <- data.frame(
      labels = top_13_attributes(),
      values = sorted_2()[top_13_attributes()]
    )
    plot_ly(data1, labels = ~labels, values = ~values, type = "pie")
  })
  #-------------------------------------------------------------------------------------------------------------------------
  output$table3 <- renderTable({
    setDT(data1)
    data_subset1 <- data1[Year == input$year3, rowSums(.SD, na.rm = TRUE), .SDcols = -c(1,2,3)]
    data_table1 <- data.table(Entity = data1[Year == input$year3,Entity], sum = data_subset1)
    data_table1 <- data_table1[order(-sum)][1:5]
    data_table1
  })
  output$plot2 <- renderPlot({
    setDT(data1)
    data_subset1 <- data1[Year == input$year3, rowSums(.SD, na.rm = TRUE), .SDcols = -c(1,2,3)]
    data_table1 <- data.table(Entity = data1[Year == input$year3,Entity], sum = data_subset1)
    data_table1 <- data_table1[order(-sum)][1:5]
    
    ggplot(data_table1, aes(x = Entity, y = sum,color = Entity)) +
    geom_point(size = 4) +
    labs(title = paste("Top 5 region by deaths in ", input$year3), 
         x = "Region", y = "Total Deaths")+
      theme_minimal() +
      scale_color_brewer(type = "qual", palette = "Dark2")+
      coord_flip()
})
  #------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------
  output$map <- renderPlot({
  plain <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5)
  )
  deaths_by_country <- aggregate(dataset[input$risk], by = list(dataset$region), sum)
  colnames(deaths_by_country) <- c("region", "deaths")
  final1 <- inner_join(dataset,deaths_by_country,by="region")
  
  final <- ggplot(data = final1, mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = deaths)) +
    coord_fixed(1.3) +
    scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
    ggtitle("Global Deaths") +
    #geom_text(data = final1, aes(x = long, y = lat, label = region), size = 2.5) +
    plain 
  
  final
  
})
#--------------------------------------------------------------
  data_filter <- reactive(
    data %>% filter(Entity == input$health)
  )
  column_ <- reactive({
    colSums(data_filter()[, c("High_systolic_blood_pressure","Diet_low_in_whole_grains","Diet_low_in_fruits","Diet_high_in_sodium","Low_birth_weight","Diet_low_in_nuts_and_seeds","Diet_low_in_vegetables","High_fasting_plasma_glucose","Low_bone_mineral_density","Vitamin_A_deficiency")])
  })
  sorted_ <- reactive({
    sort(column_(), decreasing = TRUE)
  })
  top_5_attributes <- reactive({
    names(head(sorted_(), 11))
  })

  output$table4 <- renderTable({
    data.frame(Health_risks = top_5_attributes(), Total_Deaths = sorted_()[top_5_attributes()])
  })
  output$plot3 <- renderPlotly({
    data1 <- data.frame(
      labels = top_5_attributes(),
      values = sorted_()[top_5_attributes()]
    )
    plot_ly(data1, labels = ~labels, values = ~values, type = "pie")
  })
#-------------------------------------------------------------------------------------------------
  values <- data.frame()
  observeEvent(input$submit, {
    max_value <- data3 %>% dplyr::select(Unique_ID) %>% dplyr::pull() %>% max()
    Unique_ID <- max_value+1
    column_of_Entity <- tolower(data3$Entity)
    Entity <- as.character(input$input1)
    Entity_lower <- tolower(Entity)
    Code <- as.character(input$input2)
    Year <- input$input3
    Outdoor_air_pollution <- input$input4
    High_systolic_blood_pressure <- input$input5
    Diet_high_in_sodium <- input$input6
    Diet_low_in_whole_grains <- input$input7
    Alcohol_use <- input$input8
    Diet_low_in_fruits <- input$input9
    Unsafe_water_source <- input$input10
    Secondhand_smoke <- input$input11
    Low_birth_weight <- input$input12
    Child_wasting <- input$input13
    Unsafe_sex <- input$input14
    Diet_low_in_nuts_and_seeds <- input$input15
    Household_air_pollution_from_solid_fuels <- input$input16
    Diet_low_in_vegetables <- input$input17
    Low_physical_activity <- input$input18
    Smoking <- input$input19
    High_fasting_plasma_glucose <- input$input20
    Air_pollution <- input$input21
    High_BMI <- input$input22
    Unsafe_Sanitation <- input$input23
    No_access_to_handwashing_facility <- input$input24
    Drug_use <- input$input25
    Low_bone_mineral_density <- input$input26
    Vitamin_A_deficiency <- input$input27
    Child_stunting <- input$input28
    Discontinued_breastfeeding <- input$input29
    Non_exclusive_breastfeeding <- input$input30
    Iron_deficiency <- input$input31
    if((!(Entity_lower %in% column_of_Entity)) || (Year>2024 || Year<1900) || is.na(Code) || is.na(Outdoor_air_pollution) || is.na(High_systolic_blood_pressure) || 
       is.na(Diet_high_in_sodium)||is.na(Diet_low_in_whole_grains)||is.na(Alcohol_use)||is.na(Diet_low_in_fruits)||is.na(Unsafe_water_source) ||is.na(Secondhand_smoke) || 
       is.na(Low_birth_weight)||is.na(Child_wasting)||is.na(Unsafe_sex)||is.na(Diet_low_in_nuts_and_seeds)|| is.na(Household_air_pollution_from_solid_fuels)||
       is.na(Diet_low_in_vegetables)||is.na(Low_physical_activity)||is.na(Smoking)||is.na(High_fasting_plasma_glucose)|| is.na(Air_pollution)||is.na(High_BMI)|| is.na(Unsafe_Sanitation)||
       is.na(No_access_to_handwashing_facility)||is.na(Drug_use)||is.na(Low_bone_mineral_density)|| is.na(Vitamin_A_deficiency)||is.na(Child_stunting)||is.na(Discontinued_breastfeeding)||is.na(Non_exclusive_breastfeeding)|| is.na(Iron_deficiency)){
          shinyjs::alert("Please enter valid input")
    }
    else{
      shinyjs::alert("Successfully inserted!")
      new_row <- data.frame(Unique_ID,Entity,Code,Year,Outdoor_air_pollution,High_systolic_blood_pressure,Diet_high_in_sodium,Diet_low_in_whole_grains,Alcohol_use,Diet_low_in_fruits,Unsafe_water_source,Secondhand_smoke,	Low_birth_weight,Child_wasting,Unsafe_sex,Diet_low_in_nuts_and_seeds, Household_air_pollution_from_solid_fuels,	Diet_low_in_vegetables,Low_physical_activity,	Smoking,High_fasting_plasma_glucose, Air_pollution,High_BMI, Unsafe_Sanitation,	No_access_to_handwashing_facility, 	Drug_use,Low_bone_mineral_density, Vitamin_A_deficiency,Child_stunting,Discontinued_breastfeeding, Non_exclusive_breastfeeding, Iron_deficiency)
      dataset2 <- rbind(data3, new_row)
      data3 <- write.csv(dataset2, "C:/Users/HP/Desktop/Shiny/Exercises/Project/Deaths_by_Risk Factors1.csv",row.names = FALSE)
      output$unique_id_message <- renderUI({
        HTML(
          paste("<div style='color: green; text-align: center; font-size: 20px;'>Your Unique ID is:", Unique_ID, "</div>")
        )
      })
    }
    })
  output$inputs <- renderUI({
    lapply(input$selected_values, function(col) {
      textInput(col, label = col, value = data3[data3$Unique_ID == input$unique,col])
    })
  })
  observeEvent(input$update, {
    if ((input$unique=="") || !is.numeric(input$unique)){
      shinyjs::alert("Please enter some valid unique Id")
    }
    else{
    selected_inputs <- lapply(input$selected_values, function(col) {
      input[[col]]
    })
    
    # Update the values for the selected columns
    for (i in seq_along(input$selected_values)) {
      data3[data3$Unique_ID == input$unique,input$selected_values[i]] <- selected_inputs[[i]]
    }
    
    write.csv(data3, "C:/Users/HP/Desktop/Shiny/Exercises/Project/Deaths_by_Risk Factors1.csv",row.names = FALSE)}
  })
  
  observeEvent(input$delete, {
    if ((input$Unique_del=="") || !is.numeric(input$Unique_del)) {
      shinyjs::alert("Please enter some valid unique Id")
    } 
    else{
      shinyjs::alert("Successfully deleted!")
      output$result <- renderText(paste("You entered:", input$Unique_del))
      data3 <- subset(data3, data3$Unique_ID != input$Unique_del)
      write.csv(data3, "C:/Users/HP/Desktop/Shiny/Exercises/Project/Deaths_by_Risk Factors1.csv",row.names = FALSE)
    }
    
  })
  output$Country_table <- renderTable({
    setDT(data1)
    data_subset1 <- data1[Year == input$year_input, rowSums(.SD, na.rm = TRUE), .SDcols = -c(1,2,3)]
    data_table1 <- data.table(Entity = data1[Year == input$year_input,Entity], sum = data_subset1)
    data_table1 <- data_table1[order(-sum)][1:20]
    data_table1
  })
  output$barplot1 <- renderPlot({
    setDT(data1)
    data_subset1 <- data1[Year == input$year_input, rowSums(.SD, na.rm = TRUE), .SDcols = -c(1,2,3)]
    data_table1 <- data.table(Entity = data1[Year == input$year_input,Entity], sum = data_subset1)
    data_table1 <- data_table1[order(-sum)][1:20]
    
    ggplot(data_table1, aes(x = Entity, y = sum,color = Entity)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 20 region by deaths in ", input$year_input), 
           x = "Region", y = "Total Deaths")+
      theme_minimal() +
      scale_color_brewer(type = "qual", palette = "Dark2")+
      coord_flip()
  })
  
  output$Addiction <- renderUI({
    
    if(nrow(filter(data,Year == input$Plot_year)) == 0) 
      return('No data to show')
    
    span(style = 'color:black;font-weight:bold;font-size: 80%;', DT::DTOutput('Additive'))
  })
    data_filtering1 <- reactive(
      data %>% filter(Year == input$Plot_year)
    )
    column_1 <- reactive({
      colSums(data_filtering1()[, c("Drug_use","Alcohol_use","Secondhand_smoke","Smoking")])
    })
    sorted_1 <- reactive({
      sort(column_1(), decreasing = TRUE)
    })
    top_12_attributes <- reactive({
      names(head(sorted_1(), 11))
    })
    
    output$Additive <- DT::renderDataTable(
    data.frame(Alcohol_risks = top_12_attributes(), Total_Deaths = sorted_1()[top_12_attributes()]),options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), rownames = NULL
    )
    
    output$Country_Addiction <-renderUI({
      if(nrow(filter(data,Entity == input$Plot_country)) == 0) 
        return('No data to show')
      
      span(style = 'color:black;font-weight:bold;font-size: 80%;', DT::DTOutput('Additive1'))
    })
  
    data_filtering3 <- reactive(
      data %>% filter(Entity == input$Plot_country)
    )
    column_3 <- reactive({
      colSums(data_filtering3()[, c("Drug_use","Alcohol_use","Secondhand_smoke","Smoking")])
    })
    sorted_3 <- reactive({
      sort(column_3(), decreasing = TRUE)
    })
    top_14_attributes <- reactive({
      names(head(sorted_3(), 5))
    })
    output$Additive1 <- DT::renderDataTable(
      data.frame(Alcohol_risks = top_14_attributes(), Total_Deaths = sorted_3()[top_14_attributes()]),options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), rownames = NULL)

}

shinyApp(ui, server)

