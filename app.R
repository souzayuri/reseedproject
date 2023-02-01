# library ------------------------------------------------------------------


if(!require("devtools")) install.packages("devtools", dependencies = TRUE)
if(!require("shiny")) install.packages("shiny", dependencies = TRUE)
if(!require("janitor")) install.packages("janitor", dependencies = TRUE)
if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("purrr")) install.packages("purrr", dependencies = TRUE)
if(!require("rlang")) install.packages("rlang", dependencies = TRUE)
if(!require("stringr")) install.packages("stringr", dependencies = TRUE)
if(!require("noteMD")) devtools::install_github("jienagu/noteMD")
if(!require("DT")) install.packages("DT", dependencies = TRUE)
if(!require("r2d3")) install.packages("r2d3", dependencies = TRUE)
if(!require("webshot")) install.packages("webshot", dependencies = TRUE)
if(!require("htmlwidgets")) install.packages("htmlwidgets", dependencies = TRUE)
if(!require("memor")) install.packages("memor", dependencies = TRUE)
if(!require("shinyjs")) install.packages("shinyjs", dependencies = TRUE)
if(!require("nivopie")) devtools::install_github("jienagu/nivopie")
if(!require("shinythemes")) install.packages("shinythemes", dependencies = TRUE)
#webshot::install_phantomjs()
#tinytex::install_tinytex()
if(!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if(!require("performance")) install.packages("performance", dependencies = TRUE)
if(!require("shinyWidgets")) install.packages("shinyWidgets", dependencies = TRUE)
if(!require("rmarkdown")) install.packages("rmarkdown", dependencies = TRUE)
if(!require("networkD3")) devtools::install_github("christophergandrud/networkD3")
if(!require("stats")) install.packages("stats", dependencies = TRUE)
if(!require("stargazer")) install.packages("stargazer", dependencies = TRUE)
if(!require("caret")) install.packages("caret", dependencies = TRUE)
if(!require("sjPlot")) install.packages("sjPlot", dependencies = TRUE)
if(!require("sjlabelled")) install.packages("sjlabelled", dependencies = TRUE)
if(!require("sjmisc")) install.packages("sjmisc", dependencies = TRUE)
if(!require("RColorBrewer")) install.packages("RColorBrewer", dependencies = TRUE)
if(!require("collapsibleTree")) install.packages("collapsibleTree", dependencies = TRUE)
if(!require("fontawesome")) install.packages("fontawesome", dependencies = TRUE)
if(!require("shinycssloaders")) devtools::install_github("daattali/shinycssloaders")
if(!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)


# data tables --------------------------------------------------------------



# plant traits pie graph  ------------------------------------------


#plant_traits <- read_rds("reseed_treeco.rds") %>% 
#  dplyr::select(!c(1:3,6,14:15,47,51:53,55:67)) %>% 
#  rename(Genus = Genus.corrected,
#         Species = Species.corrected) %>%  
#  rename_with(str_to_lower) %>% 
#  janitor::clean_names() %>% 
#  unite('species', genus:species) %>% 
#  mutate(species = str_replace(species, "_", " "),
#         plant_traits = str_replace(plant_traits, "_", " "),
#         plant_traits = str_replace(plant_traits, "_", " "),
#         plant_traits = str_to_title(plant_traits)) %>% 
#  mutate_all(list(~na_if(.,""))) %>% 
#  arrange(species) %>% 
#  drop_na(plant_traits)

#names(plant_traits)
#write_rds(plant_traits, "plant_traits.rds")

plant_traits <- read_rds("plant_traits.rds")
names(plant_traits)


# main plant traits table -------------------------------------------------

plant_traits_table <- read_rds("reseed_treeco.rds") %>% 
  filter(!Source == "TreeCo")
names(plant_traits_table)


# plant traits table exploration --------------------------------------------


#dttable <- read_rds("reseed_treeco.rds") %>% 
#  mutate(Genus_1 = Genus.corrected) %>% 
#  unite('Species.corrected', Genus.corrected:Species.corrected) %>% 
#  rename(Genus = Genus_1,
#         Species = Species.corrected) %>% 
#  select(c(1:3,67, everything())) %>% 
#  select(1,4,5,7:12,14) %>% 
#  filter(Plant.trait.value <= 800)
#names(dttable)

#plot(dttable$Plant.trait.value)
#write_rds(dttable, "dttable.rds")

dttable <- read_rds("dttable.rds") %>% 
  dplyr::ungroup()

names(dttable)


dttable.y <- dttable %>% 
  select(c(5,7:10))

dttable.x <- dttable %>% 
  select(c(5,7:10))


dttable.c <- dttable %>% 
  select(c(1:4,6))




# statisticals ------------------------------------------------------------

planttraits.sttc <- read_rds("dttable.rds") %>% 
  dplyr::mutate(across(where(is.character), ~na_if(., ""))) %>% 
  dplyr::mutate(Family = as.factor(Family),
         Genus = as.factor(Genus),
         Species = as.factor(Species),
         Plant.traits = as.factor(Plant.traits)) %>% 
  tibble::as.tibble()
names(planttraits.sttc)

planttraits.sttc.yaxes <- planttraits.sttc %>% 
  dplyr::select(c(5,7:10))

names(planttraits.sttc.yaxes)

# dispersers animals --------------------------------------------------------

# for Sankey Network

#dispersores_df <- read_rds("reseed_treeco.rds") %>% 
#  tidyr::unite('Species', Genus.corrected:Species.corrected) %>% 
#  dplyr::select(Species, Dispersal.group) %>% 
#  tidyr::drop_na() %>% 
#  tidyr::separate(Dispersal.group, c("x","y","z"), ",") %>% 
#  tidyr::pivot_longer(cols = 2:4, names_to = "value", values_to = "dispersers") %>% 
#  tidyr::drop_na() %>% 
#  dplyr::mutate(value = 1,
#                dispersers = stringr::str_replace(dispersers, "^\\S* ", "")) %>%
#  dplyr::mutate(dispersers = Hmisc::capitalize(dispersers),
#                Plant_specie = sub("_", " ", Species)) %>% 
#  dplyr::select(4,3,2) %>% 
#  dplyr::group_by(Plant_specie, dispersers) %>% 
#  dplyr::summarise(sum = sum(value)) %>% 
#  dplyr::ungroup() 


#write_rds(dispersores_df, "dispersores_df.rds")


dispersores_df <- read_rds("dispersores_df.rds")
colnames(dispersores_df) <- c("source", "target", "value")

nodes <- data.frame(name=c(as.character(dispersores_df$source), as.character(dispersores_df$target)) %>% unique())

dispersores_df$IDsource <- match(dispersores_df$source, nodes$name)-1 
dispersores_df$IDtarget <- match(dispersores_df$target, nodes$name)-1




# dispersal syndrome ------------------------------------------------------


Syndrome <- read_rds("reseed_treeco.rds") %>% 
  unite('Species', Genus.corrected:Species.corrected) %>% 
  select(c("Species","Dispersal.syndrome", "Dispersal.group")) %>% 
  tidyr::drop_na(Dispersal.syndrome) %>% 
  rename(Syndrome = Dispersal.syndrome) %>% 
  group_by(Species, Syndrome) %>% 
  unique() %>% 
  ungroup() %>% 
  tidyr::separate(Dispersal.group, c("x","y","z"), ",") %>% 
  tidyr::separate(Syndrome, c("a","b","c"), ",") %>% 
  pivot_longer(cols = 5:7, values_to = "Dispersers") %>% 
  select(!name) %>% 
  pivot_longer(cols = 2:4, values_to = "Syndrome") %>%
  select(!name) %>% 
  mutate(Syndrome = stringr::str_replace(Syndrome, "^\\S* ", ""),
         Dispersers = stringr::str_replace(Dispersers, "^\\S* ", ""),
         Species = stringr::str_replace(Species, "^\\S* ", "")) %>% 
  mutate(Species = sub("_", " ", Species)) %>% 
  unique() %>% 
  dplyr::filter(if_any(c(Syndrome, Dispersers), ~ !is.na(.))) %>% 
  select(c(3,2,1)) %>% 
  drop_na(Syndrome) 

  #mutate(z = if_else(y == is.character(y), z, "false"))  alternativa para remover usando uma condição as linhas repetidas

#write_csv2(Syndrome, "Syndrome.csv")
#Syndrome <- read_csv2("Syndrome.csv")

#write_rds(Syndrome, "Syndrome.rds")
Syndrome <- read_rds("Syndrome.rds")


# corumbatai plots coordinates table --------------------------------------
# plots map data

plots_coordinates <- read_rds("plots_coordinates.rds")

shapeData <- sf::st_read(dsn = "bacia_do_corumbatai.shp")
shapeData <- shapeData$geometry

# corumbatai plant traits table ------------------------------------------------------------

spp_plots_coor_traits <- read_rds("spp_plots_coor_traits.rds") %>% 
  select(c(1,6,4,5,2,3,7,9,8))
spp_plots_coor_traits

# corumbatai plant traits graph  ------------------------------------------------------------
spp_plots_coor_traits2 <- spp_plots_coor_traits %>% 
  unite('age', plot:age, sep = " ") %>% 
  unite('plant.traits', c(plant.traits, trait.unit), sep = "_") %>% 
  group_by(age, species, plant.traits) %>% 
  summarise(traits.mean.value = mean(traits.mean.value)) %>% 
  ungroup()

spp_plots_coor_traits2

# ui ----------------------------------------------------------------------

col.list <- c("white")
#Change the color
colors <- paste0("background:",col.list,";")
#Change the color
#colors <- paste0(colors,"color:black;")
#Change the font+
#colors <- paste0(colors,"font-family: Arial;")
#Change to bold
#colors <- paste0(colors,"font-weight: bold;")

ui <- bootstrapPage(
  
  #changing the color of the buttons in tables
  tags$head(
    tags$style(
      HTML(
        ".dt-button.buttons-colvis {
              background: #7e665b !important;
              color: white !important;
              font-weight: bold;
              opacity: 1;
        }
           
        .dt-button.buttons-columnVisibility.active {
              background: #8eb9b2 !important;
              color: #4b402b !important;
              opacity: 1;
           }"
      )
    )
  ),
  
  
  div(style="display:inline-block", img(src="gif_trees_birds_grid_reseed.gif", 
                                         style="position: header; width: 100%; margin-left:0%; margin-top: 0%")),
 # shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("sandstone"),
    title = "Atlantic forest plant traits",
    setBackgroundColor(color = c("#FFF5EE")),

    
    
    #header = tagList(
    #  useShinydashboard()
    #),
    
                        

# Pie graph ---------------------------------------------------------------
    
    tabPanel(title = "Traits Summary",
             icon = icon("pie-chart", "fa-2x", lib = "font-awesome", verify_fa = FALSE),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "species",
                   label = "Species:",
                   selected = "Araucaria angustifolia",
                   choices = c(unique(plant_traits$species)),
                   size = 25, selectize = FALSE
                 ),
                 shinydashboard::box(textOutput("counter"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   id = "tabs",
                   tabPanel(
                     title = "Analytics Dashboard",
                     value = "page1",
                     useShinyjs(),
                     checkboxInput("OneMore", label = h5("Show and Report donut Chart?"), T),
                     fluidRow(
                       column(
                         width = 6,
                         r2d3::d3Output("traitbar")
                       ),
                       div(id='Hide',
                           column(
                             width = 6,
                             nivopie::nivopieOutput("traitpie")
                           )
                       )
                     )
                   )
                 )
               )
             )
    ),

# datatable ---------------------------------------------------------------
          
    tabPanel(title = "Datatable",
             icon = icon("table", "fa-2x", lib = "font-awesome", verify_fa = FALSE),
             sidebarLayout(
               checkboxInput(inputId = "show_data",
                             label = "Show data table",
                             value = TRUE),
               column(12, shinycssloaders::withSpinner(DTOutput('traitstable'),
                                                       color = "#0dc5c1", 
                                                       image = "loading.gif",
                                                       image.width = 700,
                                                       image.height = 350)))
             ),

# Data exploration --------------------------------------------------------
    
    tabPanel(title = "Data Exploration",
             icon = icon("bar-chart","fa-2x", lib = "font-awesome", verify_fa = FALSE),
             sidebarLayout(
               sidebarPanel(
                 uiOutput("x_axis_select"),
                 uiOutput("y_axis_select"),
                 uiOutput("choose_filters"),
                 uiOutput("filter_value_select"),
                 uiOutput("choose_wraps"),
                 actionButton("graph_update", "Update Graphs")
               ),
               
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel(title = "Scatterplot",
                                      shinycssloaders::withSpinner(plotOutput("scatterplot"),
                                                                   color = "#0dc5c1", 
                                                                   image = "loading.gif",
                                                                   image.width = 700,
                                                                   image.height = 350)),
                             tabPanel(title = "Boxplot",
                                      shinycssloaders::withSpinner(plotOutput("boxplot"), 
                                                                   color = "#0dc5c1", 
                                                                   image = "loading.gif",
                                                                   image.width = 700,
                                                                   image.height = 350), 
                                      tableOutput("fivenumsum")),
                             tabPanel(title = "Density Plot",
                                      shinycssloaders::withSpinner(plotOutput("densityplot"), 
                                                                   color = "#0dc5c1", 
                                                                   image = "loading.gif",
                                                                   image.width = 700,
                                                                   image.height = 350),
                                      tableOutput("means"))
                 )
               )
             )
          ),


# statisticals ------------------------------------------------------------

    tabPanel(title = "Statisticals",
             icon = icon("area-chart","fa-2x", lib = "font-awesome", verify_fa = FALSE),
             sidebarLayout(
               sidebarPanel(
                 uiOutput("xvariable"),
                 uiOutput("yvariable")
           ),
           
           mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel(title = "Datatable",
                                  shinycssloaders::withSpinner(DTOutput("datasttc"),
                                                               color = "#0dc5c1", 
                                                               image = "loading.gif",
                                                               image.width = 700,
                                                               image.height = 350)),
                         tabPanel(title = "Data Summary",
                                  shinydashboard::box(verbatimTextOutput("Summ"), width = 10),
                                  shinydashboard::box(verbatimTextOutput("Summ_old"), width = 10)
                         ),
                         tabPanel(title = "Model",
                                  shinydashboard::box(verbatimTextOutput("GLMModel"), width = 6, 
                                                      title = "Model Summary"),
                                  shinydashboard::box(verbatimTextOutput("ImpVar"), width = 6, 
                                                      title = "Variable Importance")
                         ),
                         tabPanel(title = "Estimates",
                                  shinycssloaders::withSpinner(plotOutput("Estimates"),
                                                               color = "#0dc5c1", 
                                                               image = "loading.gif",
                                                               image.width = 700,
                                                               image.height = 350)),
                         tabPanel(title = "Residuals",
                                  shinycssloaders::withSpinner(plotOutput("residualPlots"),
                                                               color = "#0dc5c1", 
                                                               image = "loading.gif",
                                                               image.width = 700,
                                                               image.height = 350))   
                         
             )
           )
         )
    ),



# dispersers --------------------------------------------------------------
    
        tabPanel(title = "Animal Dispersers",
                 icon = icon("sitemap", "fa-2x", lib = "font-awesome", verify_fa = FALSE),
                   mainPanel(
                     tabsetPanel(type = "hidden",
                                 tabPanel(title = "Animal dispersers",
                                          networkD3::sankeyNetworkOutput("diagram", 
                                                                         height = "7000px", 
                                                                         width = "220%")))
                     
           )),

# Syndrome ----------------------------------------------------------------


        tabPanel(title = "Disperse Syndrome",
                 icon = icon("share-alt", "fa-2x", lib = "font-awesome", verify_fa = FALSE),
                 mainPanel(
                   tabsetPanel(type = "hidden",
                               tabPanel(title = "Disperse syndrome",
                                        collapsibleTree::collapsibleTreeOutput("syndrome",
                                                                               height = "1700px",
                                                                               width = "150%"
                                                                               )
                                        ))
           
         )),

# Corumbatai plots ---------------------------------------------------------------


      tabPanel(title = "Corumbatai plots",
               icon = icon("map-pin", "fa-2x", lib = "font-awesome", verify_fa = FALSE),
               sidebarLayout(
                 sidebarPanel(      
                   selectInput(
                   inputId = "species_corum",
                   label = "Species:",
                   choices = c("ALL", unique(spp_plots_coor_traits2$species)),
                   selected = "Trichilia pallida")
                   ),
               
               mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel(title = "Map",
                                       leafletOutput(outputId = "mymap", width="100%", height="620px")),
                              tabPanel(title = "Datatable",
                                       shinycssloaders::withSpinner(DTOutput("corumbtable"), 
                                                                    color = "#0dc5c1", 
                                                                    image = "loading.gif",
                                                                    image.width = 700,
                                                                    image.height = 350)),
                              tabPanel(title = "Exploratory Graphs",
                                       plotOutput("barplot"),
                                       width = "100%",
                                       height = "400px")
                       
           )
         )
      )
    ),


# Contacts ----------------------------------------------------------------
        tabPanel(title = "Contacts & Collaborators",
                 icon = icon("address-book", "fa-2x", lib = "font-awesome", verify_fa = FALSE),
                 includeMarkdown("contacts/contact.Rmd"),
                 img(src = "marina.jpg"),
                 includeMarkdown("contacts/marina.Rmd"),
                 img(src = "robert.jpg"),
                 includeMarkdown("contacts/robert.Rmd"),
                 img(src = "willian.jpg"),
                 includeMarkdown("contacts/willian.Rmd"),
                 img(src = "yuri.jpg"),
                 includeMarkdown("contacts/yuri.Rmd"),
                 includeMarkdown("contacts/collaborators.Rmd"),
                 img(src = "merel.jpg"),
                 includeMarkdown("contacts/merel.Rmd"),
                 img(src = "pedro.jpg"),
                 includeMarkdown("contacts/pedro.Rmd"),
                 img(src = "pizo.jpg"),
                 includeMarkdown("contacts/pizo.Rmd"),
                 img(src = "mathias.jpg"),
                 includeMarkdown("contacts/mathias.Rmd"),
                 img(src = "agencies.jpg"),
                 includeMarkdown("contacts/credits.Rmd"))

  )
)
# server ------------------------------------------------------------------



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
# Trait summary -----------------------------------------------------------
  
  
  shinyjs::useShinyjs()
  observe({
    shinyjs::toggle(id = "Hide", condition = input$OneMore, anim = TRUE, animType = "fade")
  })
  plant_traits_filtered <- reactive({
    if (input$species != "ALL") plant_traits <- dplyr::filter(plant_traits, species == input$species)
    plant_traits
  })
  
  bar_graphD3 <- reactive({
    grouped <- ifelse(input$species != "ALL", expr(plant_traits), expr(species))
   
    spptraitdata <- plant_traits_filtered() %>%
      dplyr::group_by(!!grouped) %>%
      dplyr::tally() %>%
      dplyr::collect() %>%
      dplyr::mutate(
        y = n,
        x = !!grouped) %>%
      dplyr::select(x, y)

    spptraitdata <- spptraitdata %>%
      dplyr::mutate(label = x)
    
    r2d3::r2d3(spptraitdata, "bar_plot.js", d3_version = 3)
  })
  
  pie_graph <- reactive({
    grouped2 <- ifelse(input$species != "ALL", expr(plant_traits), expr(species))
    
    spptraitdata2 <- plant_traits_filtered() %>%
      dplyr::group_by(!!grouped2) %>%
      dplyr::tally() %>%
      dplyr::collect() %>%
      dplyr::mutate(
        value = n,
        id = !!grouped2) %>%
      dplyr::select(id, value)
    
    spptraitdata3 <- data.frame(spptraitdata2)
    spptraitdata3$id <- as.factor(spptraitdata3$id)
    nivopie::nivopie(spptraitdata3, innerRadius=0.5, cornerRadius=5, fit=T, sortByValue=T,
            colors='paired', enableRadialLabels=F, radialLabelsLinkDiagonalLength=1,
            radialLabelsLinkHorizontalLength=8,
            enableSlicesLabels=T, sliceLabel='id',isInteractive=T)
    
  })

  output$traitbar = r2d3::renderD3({
    bar_graphD3()
  })

  output$traitpie=nivopie::renderNivopie({
    pie_graph()
  })
  
# plant/trait bar click (server) ---------------------------------
  observeEvent(input$bar_clicked != "", {
    if (input$species == "ALL") {
      updateSelectInput(session, "species", selected = input$bar_clicked)
    }
  }, ignoreInit = TRUE)
  
  

# datatable -------------------------------------------------------------------
  
  
  output$traitstable <- DT::renderDataTable( server = FALSE, 
    if(input$show_data) {
      datatable(
        plant_traits_table,
        filter = "top",
        selection = "none",
        #class = "compact stripe row-border nowrap",
        class = 'cell-border stripe',
        callback=JS('$("button.buttons-excel").css({"background": "#7e665b", "color": "white", "font-weight": "bold"}); 
                    return table;'),
        #escape = -1,
        # Escape the HTML in all except 1st column (which has the buttons)
        extensions = c("Buttons", "KeyTable", 'Scroller'),
        options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
            "}"),
          order = list(63, 'desc'), #reorder by modifying data
          keys = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            #list(targets = -1, orderable = TRUE),
            list(className = 'dt-center', targets = "_all"),
            list(targets = c(1:67), width = '200px')),
          scrollX = TRUE,
          dom = 'Blfrtip',
          deferRender = TRUE,
          scrollY = 450,
          scroller = TRUE,
          buttons = list(
            I('colvis'),
            list(
              extend = "excel",
              charset = 'UTF-8',
              bom = TRUE,
              text = "Download",
              title = paste0("plant_traits-", Sys.Date())
            )
          )
        )
      )
    }
  )
  
  
# exploratory graphs ------------------------------------------------------
  
  #read data
  data <- reactive(dttable)
  
  data.x <- reactive(dttable.x)
  data.y <- reactive(dttable.y)
  data.c <- reactive(dttable.c)

# Input: get which columns to use as filters ----
  output$choose_filters <- renderUI({
    colheads <- reactive(names(data.c()))
    checkboxGroupInput("filter_choice", "Choose columns to use as filters:", choices=colheads())
    
  })
  
# Input: get which columns to use as wraps ----
  output$choose_wraps <- renderUI({
    colheads <- reactive(names(data.c()))
    selectizeInput("wrap_choice", "Choose an option to display multiple graphs by all categories of a variable
                   (Exemple: Choose Plant.traits to see the X effect on Y for all traits):", choices=c("no wrap", colheads()))
    
  })
  
  #create filters 
  filters_list <- eventReactive(input$filter_choice,{
    values <- vector()
    nameslist <- vector()
    i=length(input$filter_choice)
    while(i>0) {
      column=input$filter_choice[i]
      filters=unique(data()[[column]])
      j=length(filters)
      while (j>0){
        values <- c(values, paste(input$filter_choice[i], "== '", filters[j], "'", sep = ''))
        nameslist <- c(nameslist, paste(as.character(input$filter_choice[i]), ":", as.character(filters[j])))
        j=j-1
      }
      i=i-1
    }
    names(values)<-nameslist
    values
  })
  
  
# output plots ------------
  #axis selection
  output$x_axis_select <- renderUI({
    varSelectInput("x_axis_selection", "Choose X axis", selected = "Sample.size.value", data.x())
  })
  output$y_axis_select <- renderUI({
    varSelectInput("y_axis_selection", "Choose Y axis", selected = "Plant.trait.value", data.y())
  })
  #filter selection
  output$filter_value_select <- renderUI({
    selectInput("filter_value_selection", "Select filters to use", choices=filters_list())
  }) 
  
  
  
  observe({ toggle(id="graph_update", condition=!is.null(data))})
  
  #filter data
  
  filtereddata = eventReactive(input$graph_update, {
    filtereddatatemp = data()
    x<-input$filter_value_selection
    if (length(input$filter_choice)==0){
      x <- c()}
    i=length(x)
    while (i>0) {
      filtereddatatemp=dplyr::filter(data(), eval(parse(text=x[i])))
      i=i-1
    }
    filtereddatatemp
  })
  
  #building new data frame
  
  newdf <- eventReactive(input$graph_update, {
    x=filtereddata()[[input$x_axis_selection]]
    y=filtereddata()[[input$y_axis_selection]]
    if (length(input$wrap_choice)<=0|input$wrap_choice=="no wrap") {
      wrapdata = vector(mode='numeric', length=length(y))
    } else {
      wrapdata = filtereddata()[[input$wrap_choice]]
    }
    newdf <- data.frame(x, y, wrapdata)
    newdf
  })
  
  #scatterplot
  
  output$scatterplot <-renderPlot({
    ggplot(newdf())+
      geom_smooth(mapping=aes(x=x, y=y), method='lm', size=3, color = "#FF7F50") + 
      geom_point(mapping=aes(x=x, y=y), size = 8, shape = 21,  color = "black", fill = "gray60",) +
      labs(x=input$x_axis_selection, y=input$y_axis_selection)+
      theme_bw() + 
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            plot.background = element_rect(fill = "#FFF5EE")) +
      facet_wrap(vars(wrapdata))
  })
  
  #boxplot
  
  output$boxplot <- renderPlot({
    ggplot(newdf())+
      geom_boxplot(mapping=aes(x="", y=newdf()$x))+
      labs(x="", y=input$x_axis_selection)+
      theme_bw() +
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            plot.background = element_rect(fill = "#FFF5EE")) +
      facet_wrap(vars(wrapdata))
  })
  
  fivenumsumtable = eventReactive(newdf(), {
    if (newdf()$wrapdata[1]) {
      values=as.vector(unique(newdf()$wrapdata))
      name=vector(mode="character", length=length(values))
      min=vector(mode="numeric", length=length(values))
      q1=vector(mode="numeric", length=length(values))
      med=vector(mode="numeric", length=length(values))
      q3=vector(mode="numeric", length=length(values))
      max=vector(mode="numeric", length=length(values))
      for (i in c(1:length(values))){
        data=newdf()%>%
          dplyr::filter(wrapdata==values[i])
        name[i]=as.character(values[i])
        min[i]=min(data$x)
        q1[i]=quantile(data$x, probs =.25)
        med[i]=median(data$x)
        q3[i]=quantile(data$x, probs =.75)
        max[i]=max(data$x)
      }
      fivenumsumtable=tibble(" "=name, Minimum=min, Q1=q1, Median=med, Q3=q3, Maximum=max)
    } else {
      fivenumsumtable=tibble(Minimum=min(newdf()$x, na.rm = TRUE), 
                             Q1=quantile(newdf()$x, .25, na.rm = TRUE), 
                             Median=median(newdf()$x, na.rm = TRUE), 
                             Q3=quantile(newdf()$x, .75, na.rm = TRUE), 
                             Maximum=max(newdf()$x, na.rm = TRUE))
      
    }
    fivenumsumtable
  })
  
  
  output$fivenumsum <- renderTable(fivenumsumtable())
  
  #density plot
  
  output$densityplot <- renderPlot({
    ggplot(newdf())+
      geom_density(mapping=aes(x=newdf()$x))+
      labs(x=input$x_axis_selection)+
      theme_bw()+
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            plot.background = element_rect(fill = "#FFF5EE")) +
      facet_wrap(vars(wrapdata))
  })
  
  meanstable = eventReactive(newdf(), {
    if (newdf()$wrapdata[1]) {
      values=as.vector(unique(newdf()$wrapdata))
      name=vector(mode="character", length=length(values))
      mean=vector(mode="numeric", length=length(values))
      for (i in c(1:length(values))){
        data=newdf()%>%
          dplyr::filter(wrapdata==values[i])
        name[i]=as.character(values[i])
        mean[i]=mean(data$x, na.rm = TRUE)
      }
      meanssumtable=tibble(" "=name, Mean=mean)
    } else {
      meanssumtable=tibble(Mean=mean(newdf()$x, na.rm = TRUE))
    }
    meanssumtable
  })
  
  output$means <- renderTable(meanstable())  

  
    
  

# Statisticals ------------------------------------------------------------

  

# datatable ---------------------------------------------------------------


  output$datasttc <-  DT::renderDataTable(server = FALSE, {
    datatable(planttraits.sttc,
              selection = "none",
              class = 'cell-border stripe',
              extensions = c("Buttons", "KeyTable", 'Scroller'),
              options = list(keys = TRUE,
                             autoWidth = TRUE,
                             scrollX = TRUE,
                             dom = 'Blfrtip',
                             deferRender = TRUE,
                             scrollY = 450,
                             scroller = TRUE,
                             buttons = list(
                               I('colvis')#,
                               #list(
                                # extend = "excel",
                                # charset = 'UTF-8',
                                # bom = TRUE,
                                # text = "Download",
                                # title = paste0("plant_traits_statistics-", Sys.Date())
                               #)
                             )
                             ))}
    )
  

# datatable summary -------------------------------------------------------

  output$Summ <- renderPrint(
    stargazer(
      planttraits.sttc,
      type = "text",
      title = "Descriptive statistics"#,
      #     digits = 1,
      #      out = "table1.txt"
    )
  )
  output$Summ_old <- renderPrint(summary(planttraits.sttc))
  output$structure <- renderPrint(str(planttraits.sttc))
  
  

# GLM model results -------------------------------------------------------
  
  
  output$xvariable <- renderUI({
    req(planttraits.sttc)
    xaxis<-colnames(planttraits.sttc)
    pickerInput(inputId = 'xvarib',
                label = 'Select x-axis variable',
                choices = c(xaxis[1:length(xaxis)]), selected=xaxis[4],
#                options = list(`style` = "live-search"),
                choicesOpt = list(
                  style = colors),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    req(planttraits.sttc)
    yaxis<-colnames(planttraits.sttc) 
    pickerInput(inputId = 'yvarib',
                label = 'Select y-axis variable',
                choices = c(names(planttraits.sttc.yaxes)), selected=yaxis[1],
#                options = list(`style` = "live-search"),
                choicesOpt = list(
                  style = colors),
                multiple = FALSE)
    
  })
  
  GLM_Model <- reactive({
    req(planttraits.sttc, input$xvarib, input$yvarib)
    xvrbl <- as.numeric(data()[[as.name(input$xvarib)]])
    yvrbl <- as.numeric(data()[[as.name(input$yvarib)]])
    current_formula <- paste0(input$yvarib, " ~ ", paste0(input$xvarib, collapse = " + "))
    current_formula <- as.formula(current_formula)
    glm_model <- stats::glm(current_formula, 
                            data = planttraits.sttc, 
                            family = gaussian)
    return(glm_model)
  })
  
  
  # GLM model summary
  output$GLMModel <- renderPrint(summary(GLM_Model()))
  
  
  # GLM model importance
  
  Importance <- reactive({
    caret::varImp(GLM_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(caret::varImp(GLM_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  

# GLM estimates graph -----------------------------------------------------


  
  output$Estimates <- renderPlot({
    plot_model(GLM_Model(), show.values = TRUE, value.offset = .3, sort.est = TRUE)
    
  })

# GLM residuals plot graphs -----------------------------------------------
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
    plot(GLM_Model())
    par(mfrow = c(2, 2)) # Change back to 1 x 1
  })
  
  #  output$residualPlots <- renderPlot({
  #    performance::check_model(GLM_Model())
  #  })
  
  
  
# Dispersers diagram plot ------------------------------------------------------

  
  output$diagram <- networkD3::renderSankeyNetwork({
    networkD3::sankeyNetwork(Links = dispersores_df, 
                             Nodes = nodes,
                             Source = "IDsource", 
                             Target = "IDtarget",
                             Value = "value", 
                             fontSize = 20,
                             NodeID = "name", 
                             sinksRight=FALSE) %>% 
      htmlwidgets::onRender('function(el) { el.querySelector("svg").removeAttribute("viewBox") }')
    
  })
  


# Syndrome ----------------------------------------------------------------


  
  output$syndrome <- collapsibleTree::renderCollapsibleTree({
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(758)
    mycolors
    collapsibleTree::collapsibleTree(Syndrome, 
                                     hierarchy = c("Syndrome","Dispersers","Species"),
                                     collapsed = TRUE,
                                     fill = mycolors,
                                     fontSize = 14)
  })

# Plots map ---------------------------------------------------------------

  plot_n_spp <- 
  output$mymap <- renderLeaflet({
    leaflet(plots_coordinates) %>% 
      setView(lat = -22.4512168, lng = -47.8272986, zoom = 9)  %>% 
      addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "National Geographic World Map") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "World Physical") %>%
      addProviderTiles(providers$Stamen, group = "Stamen") %>%
      addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery", 
                                      "National Geographic World Map", "Open Topo Map", 
                                      "World Physical", "Stamen"), 
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      addTiles(group = "Open Street Map") %>% 
      addMiniMap(zoomLevelOffset = -4) %>%
      #addScaleBar() %>% 
      addPolygons(data=shapeData, weight=2, col = '#7B68EE') %>% 
      addCircleMarkers(data = plots_coordinates, lat = ~ latitude, lng = ~ longitude,  popupOptions = TRUE,
                       group = "age" , weight = 20, color = c("#00FF00", "#00FF00", "#00FF00", "#00FF00", "#00FF00",
                                                              "blue", "blue", "blue", "red", "blue", "blue",
                                                              "red", "red", "blue"), label = c("C3-old - 35 species", 
                                                                                               "C2-old - 15 species", 
                                                                                               "C6-old - 24 species",
                                                                                               "C5-old - 19 species", 
                                                                                               "C1-old - 29 species", 
                                                                                               "12-semi-old - 21 species",
                                                                                               "20-semi-old - 20 species", 
                                                                                               "31-semi-old - 19 species", 
                                                                                               "53-new - 16 species", 
                                                                                               "56-semi-old - 25 species", 
                                                                                               "64-semi-old - 28 species", 
                                                                                               "90-new - 19 species",
                                                                                               "206-new - 24 species", 
                                                                                               "207-semi-old - 26 species")) %>% 
      addLegend(position = "bottomleft", colors = c("#00FF00", "blue", "red", "#7B68EE"), 
                labels= c("Old > 100 years", "Semi-old 30-100 years","New < 30 years", "Corumbataí watershed"), title = "Plots description")
    
  })  
  


# corumbatai table --------------------------------------------------------
  
  output$corumbtable <-  DT::renderDataTable(server = FALSE, {datatable(
    spp_plots_coor_traits,
    filter = "top",
    selection = "none",
    class = 'cell-border stripe',
    extensions = c("Buttons", "KeyTable", 'Scroller'),
    options = list(keys = TRUE,
                   autoWidth = TRUE,
                   scrollX = TRUE,
                   dom = 'Blfrtip',
                   deferRender = TRUE,
                   scrollY = 450,
                   scroller = TRUE,
                   buttons = list(
                     I('colvis')#,
                     #list(
                    #   extend = "excel",
                    #   charset = 'UTF-8',
                    #   bom = TRUE,
                    #   text = "Download",
                    #   title = paste0("plant_traits_corumbatai-", Sys.Date())
                     #  )
                     )
                   ))}
    )

# corumbatai graph --------------------------------------------------------

  #filtered_data_corum <- reactive({
  #  dplyr::filter(spp_plots_coor_traits2, species == input$species_corum)
  #})
  
  
  filtered_data_corum <- reactive({
    if (input$species_corum != "ALL") spp_plots_coor_traits2 <- filter(spp_plots_coor_traits2, 
                                                                            species == input$species_corum)
    spp_plots_coor_traits2
  })
  
  output$barplot <- renderPlot({ 
    ggplot(filtered_data_corum(), aes(y=traits.mean.value, x=plant.traits)) + 
      geom_bar(stat = "identity", fill = "#663399") + theme_bw() + 
      xlab("Plant traits") + ylab("Mean trait values") +
      theme(axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),
            strip.text = element_text(size=16),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            plot.background = element_rect(fill = "#FFF5EE")) +
      facet_wrap(~age)
  })
  
  
  

# counter -----------------------------------------------------------------

  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) 
        counter <- 0
      else
        load(file="counter.Rdata")
      counter  <- counter + 1
      save(counter, file="counter.Rdata")     
      paste("Hits: ", counter)
    })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)



# -------------------------------------------------------------------------



