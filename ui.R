#ui.R
options(encoding="UTF-8")

ccaa=c("-","Andalucía","Aragón","Asturias","Baleares","Canarias","Cantabria","Castilla y León",
       "Castilla La Mancha","CataluÃ±a","Comunitat Valenciana","Extremadura","Galicia","Madrid",
       "Murcia","Navarra","País Vasco","La Rioja")
prov=c("-","Albacete","Alicante","Almería","Álava","Asturias","Ávila",
      "Badajoz","Baleares, Illes","Barcelona","Bizkaia","Burgos","Cáceres",
      "Cádiz","Cantabria","Castellón","Ciudad Real","Córdoba","Coruña, A",
      "Cuenca","Gipuzkoa","Girona","Granada","Guadalajara","Huelva","Huesca",
      "Jaén","León","Lleida","Lugo","Madrid","Málaga","Murcia","Navarra","Ourense",
      "Palencia","Palmas, Las","Pontevedra","Rioja, La","Salamanca","Santa Cruz de Tenerife",
      "Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid",
      "Zamora","Zaragoza")

codccaa=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
names(codccaa)=ccaa

codprov=c(0,2,3,4,1,33,5,6,7,8,48,9,10,11,39,12,13,14,15,16,20,17,18,19,21,
          22,23,24,25,27,28,29,30,31,32,34,35,36,26,37,38,40,41,42,43,44,
          45,46,47,49,50)
names(codprov)=prov

####Layout

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
    titlePanel(div(h2("Mapas de municipios de España",style="color:#006EC1"),
                 h6("Creado por Ignacio Archondo Contreras",style="color:#004481")
    #img(src="logo3.png")
  )),
  sidebarLayout(
    sidebarPanel(
      helpText("Esta aplicación dibuja el mapa de España dependiendo de los datos introducidos"),
      selectInput("varsel",label=h3("Selecciona la variable"),choices=c("-","Pob","Poblog"),selected="-"),
      h3("Región"),
      radioButtons("setter",h4("Elige la región"),choices=c("Toda España"=0,"CCAA"=1,"Provincia"=2),selected=0),
      selectInput("selCCAA",h4("Elige CCAA"),choices=codccaa,selected="-"),
      selectInput("selprov",h4("Elige Provincia"),choices=codprov,selected="-"),
      br(),
      actionButton("Dibujar","Dibujar",icon("pencil-square-o"),width = "355px",
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      
    ),
    mainPanel(
      tabsetPanel(id="tabs1",
                  tabPanel("Mapa",
                           fluidRow(column(12,br())),
                           fluidRow(column(4,strong("Variable",style="color:#006EC1")),
                                    #column(1,br()),
                                    column(4,strong("Región",style="color:#006EC1"))),
                           fluidRow(column(4,h4(textOutput("textvar"),style="color:#009EE5"),
                                    h5(textOutput("vartip"),style="color:gray")),
                                    column(4,h4(textOutput("textreg"),style="color:#009EE5")),
                                    column(4,shinyjs::disabled(actionButton("export","Exportar",width="200px",icon("print"),style="color: #fff; background-color: #337ab7; 
                                        border-color: #2e6da4")))),
                  plotOutput("plot",height="400px"),
                            fluidRow(column(12,br())),
                            fluidRow(column(6,h4("Colores",style="color:#006EC1")),
                                     column(6,h4(textOutput("medpoint"),style="color:#006EC1"))),
                            fluidRow(column(3, a("Catálogo",target="_blank",
                                                 href="http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"),
                                            radioButtons("sel_col","",choices=c("BBVA"=0,"Personalizado"=1),selected=0)),
                                     column(2,br(),textInput("entmin","Min",value=""),textInput("entmax","Max",value="")),
                                     column(1,br()),
                                     column(6,checkboxInput("automed",label="Incluir",value=FALSE),
                                            sliderInput("slidermed",NULL,min=0,max=1,value=0.5),
                                     checkboxInput("pickmed",label="Personalizar color medio",value=FALSE),
                                     textInput("colmed","Med",value="",width = "120px")))),
                  tabPanel("Tabla de datos",
                           fluidRow(column(12,h3("Tabla de datos",style="color:#006EC1"))),
                           fluidRow(column(12,helpText("Tabla actualizada con los datos cargados en el programa"))),
                           fluidRow(column(12,br(),div(style='height:600px; overflow-y: scroll', tableOutput("datatot"))))),
                  tabPanel("Agregar datos",
                           fluidRow(column(12,h3("Cargar más datos",style="color:#006EC1"))),
                           fluidRow(column(12,helpText("El archivo importado tiene que contener una columna 
                                                       llamada CodMun con los códigos de municipio."))),
                           fluidRow(column(12,helpText("Si se usa un csv, los valores tienen que estar separados por punto y coma ( ; )."))),
                           fluidRow(column(12,radioButtons("selFile",h4("Selecciona el tipo de archivo"),choices=c(".csv"=0,".xls"=1,".xlsx"=2),
                                                                    selected = 0,inline = T))),
                           fluidRow(column(4,fileInput("ruta",label = NULL)),
                                    column(4,
                                           actionButton("merge","Importar",icon("magic"),width="150px",
                                                        style="color: #fff; background-color: #337ab7; 
                                                        border-color: #2e6da4"))
                                    ),
                           div(style='height:450px; overflow-y: scroll', tableOutput("csvdata"))),
                  tabPanel("Cortar variables",
                           fluidRow(column(12,h3("Cortar variables",style="color:#006EC1"))),
                           fluidRow(column(12,helpText("Introducir el nombre de la nueva variable y los 
                                                       puntos de corte separados por comas (ej: 3,5,8)"))),
                           fluidRow(column(4,
                                           selectInput("varselcut",label="Selecciona la variable a cortar",
                                                       choices=c("-","Pob","Poblog"),selected="-"))),
                           fluidRow(column(4,textInput("nomcor","Nombre de la nueva variable",value=""),
                                           textInput("pun_cortes","Puntos de corte",value="")),
                                    column(6,br(),br(),br(),br(),br(),
                                           actionButton("cortbutt","   Cortar",width='150px',icon("scissors"),
                                                        style="color: #fff; background-color: #337ab7; 
                                                        border-color: #2e6da4"))),
                           fluidRow(column(12,br())),
                           tableOutput("tabvar"))
                  
      )
    )
    
    
  )
))
