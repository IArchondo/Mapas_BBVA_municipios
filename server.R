#server.R

source("helper.R")

#if (!require("pacman")) install.packages("pacman")
#pacman::p_load("maptools","dplyr","data.table","reshape2","ggplot2","plyr","rgdal","rgeos",
#               "shinyjs","scales","DT","readxl")

#list_of_packages= c("maptools","dplyr","data.table","reshape2","ggplot2",
#                    "plyr","rgdal","rgeos","shinyjs","scales","DT","readxl")

#new.packages=list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
list_of_packages <- c("maptools","dplyr","data.table","reshape2","ggplot2","plyr","rgdal","rgeos","shinyjs","scales","DT","readxl")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

#library(maptools)
#library(dplyr)
#library(data.table)
#library(reshape2)
#library(ggplot2)
#library(plyr)
#library(rgdal)
#library(rgeos)
#library(shinyjs)
#library(scales)
#library(DT)
#library(readxl)

dataini<-read.csv2("Pob3.csv",sep=";")
graf.fin<-NULL

#----------------------------------------------------------------------
#----------------------------------------------------------------------
##            OPERATIVO             ##


shinyServer(function(input, output,session) {
  observe({
    if(input$varsel=="-"){loc=1} else {
    loc=which(colnames(dataini)==input$varsel)}
    if (input$setter==0){updateSelectInput(session,"selCCAA",selected="-")}
    if (input$setter==0){updateSelectInput(session,"selprov",selected="-")}
    if (input$setter==1){updateSelectInput(session,"selprov",selected="-")}
    if (input$setter==2){updateSelectInput(session,"selCCAA",selected="-")}
    
    if (input$varsel=="-"){} else{
    if(is.factor(dataini[[loc]])==TRUE){} else{
    if(sum(dataini[loc]<0)>0){
      updateSliderInput(session,"slidermed",min=-1,max=1,step=0.1,value=0)
    } else{
      updateSliderInput(session,"slidermed",min=0,max=1,value=0.5)
    }}
    }
    
    shinyjs::toggleState("selCCAA",input$setter==1)
    shinyjs::toggleState("selprov",input$setter==2)
    shinyjs::toggleState("Dibujar",input$varsel!="-"& (input$setter==0 | input$selCCAA!=""|
                                                        input$selprov!=""))
    shinyjs::toggleState("entmax",input$sel_col==1)
    shinyjs::toggleState("entmin",input$sel_col==1)
    shinyjs::toggleState("automed",is.factor(dataini[[loc]])==F)
    shinyjs::toggleState("pickmed",is.factor(dataini[[loc]])==F & input$automed==T)
    shinyjs::toggleState("slidermed",input$automed==T & is.factor(dataini[[loc]])==F)
    shinyjs::toggleState("colmed",input$pickmed==T & is.factor(dataini[[loc]])==F)
    shinyjs::toggleState("nomcor",input$varselcut!="-")
    shinyjs::toggleState("pun_cortes",input$nomcor!="")
    shinyjs::toggleState("cortbutt",input$pun_cortes!=""& input$varselcut!="-" & input$nomcor!="")
    shinyjs::toggleState("merge",!is.null(input$ruta))
  })
  
  ###TABLAS###-----------------------------
  
  #ponemos la data en el output  
  output$datatot=renderTable({
    dataini
  })
  
  #ponemos las estadisticas de las variables en el output
  output$tabvar=renderTable({
    data.frame(unclass(summary(dataini[3:ncol(dataini)])),check.names = F,stringsAsFactors = F)
  })
  
  #cargamos los datos
  datacsv=reactive({
    file1<<-input$ruta
    if (is.null(file1)){return()}
    else {
      if (input$selFile==0){
        datax=read.csv(file=file1$datapath,sep=";")
        datax}
          else {
            if(input$selFile==1){
              file.rename(file1$datapath,paste(file1$datapath,".xls",sep=""))
              datax=read_excel(path=paste(file1$datapath,".xls",sep=""),1)
              datax$CodMun=as.integer(datax$CodMun)
              datax
            }else{
            if(input$selFile==2){
                file.rename(file1$datapath,paste(file1$datapath,".xlsx",sep=""))
                datax=read_excel(path=paste(file1$datapath,".xlsx",sep=""),1)
                datax$CodMun=as.integer(datax$CodMun)
                datax
            }
              }
      }
    }
  })
  
  #ponemos los datos en el output
  output$csvdata=renderTable({
    # Sys.setlocale('LC_ALL','C') 
    if(is.null(datacsv())){return()}
    datacsv()
  })
  
  #----------------------------------------------------------------------
  ##            LABELS             ##
  
  #Texto de error
  output$textvar=renderText({
    if (input$varsel=="-"){"Selecciona una variable"}
    else {input$varsel}
  })
  
  #label de variable
  output$vartip=renderText({
    if(input$varsel=="-"){"[ ]"} else {
      loc=which(colnames(dataini)==input$varsel)
      if(is.factor(dataini[[loc]])==TRUE){"[variable discreta]"}
      else {"[variable continua]"}}    
  })
  
  
  #label de region
  output$textreg=renderText({
    ccaa=c("-","Andalucía","Aragón","Asturias","Baleares","Canarias","Cantabria","Castilla y LeÃ³n",
           "Castilla La Mancha","Cataluña","Comunitat Valenciana","Extremadura","Galicia","Madrid",
           "Murcia","Navarra","País Vasco","La Rioja")
    provnom=c("-","Albacete","Alicante","Almería","Álava","Asturias","Ávila",
           "Badajoz","Baleares, Illes","Barcelona","Bizkaia","Burgos","Cáceres",
           "Cádiz","Cantabria","Castellón","Ciudad Real","Córdoba","Coruña, A",
           "Cuenca","Gipuzkoa","Girona","Granada","Guadalajara","Huelva","Huesca",
           "Jaén","León","Lleida","Lugo","Madrid","Málaga","Murcia","Navarra","Ourense",
           "Palencia","Palmas, Las","Pontevedra","Rioja, La","Salamanca","Santa Cruz de Tenerife",
           "Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid",
           "Zamora","Zaragoza")
    codprov=c(0,2,3,4,1,33,5,6,7,8,48,9,10,11,39,12,13,14,15,16,20,17,18,19,21,
              22,23,24,25,27,28,29,30,31,32,34,35,36,26,37,38,40,41,42,43,44,
              45,46,47,49,50)
    names(codprov)=provnom
    codprovalf=sort(codprov)
    codprovalfnames=names(codprovalf)
    if (input$setter==0){"España"} else
      if(input$setter==1){
        if(input$selCCAA==""){"Selecciona una CCAA"}
        else {ccaa[as.integer(input$selCCAA)+1]}
      } else {
        if(input$selprov==""){"Selecciona una Provincia"}
        else {
          codprovalfnames[as.integer(input$selprov)+1]}
      }
      
  })
  
  #label punto medio
  output$medpoint=renderText({
    if(input$varsel=="-"){loc=1} else {
      loc=which(colnames(dataini)==input$varsel)}
    
    if(is.factor(dataini[[loc]])==FALSE){"Punto medio"}
    else {"Punto medio no disponible para variables discretas"}
    
  })
  
  #----------------------------------------------------------------------
  ##            BOTONES             ##
  
  #BOTON combinamos los datos importados con los de la tabla principal
  observeEvent(input$merge,{
    if(is.null(input$ruta)){}
    else{
      withProgress(message = 'Importando datos', value = 0,{
        incProgress(0.0,detail="Combinando")
      datainiprev<<-merge(dataini,datacsv(),by="CodMun",all.x=TRUE)
      datainiprev=datainiprev[, -grep(".y", colnames(datainiprev))]
      nom=colnames(datainiprev)
      nom=gsub(".x","",nom)
      colnames(datainiprev)=nom
        incProgress(0.5,detail="Reemplazando")      
      dataini<<-datainiprev
      output$datatot=renderTable({
        dataini
      })
      incProgress(0.5,detail="Actualizando menú")
      updateSelectInput(session,"varsel",choices=c("-",colnames(dataini)[3:length(colnames(dataini))]))
      updateSelectInput(session,"varselcut",choices=c("-",colnames(dataini)[3:length(colnames(dataini))]))
      output$tabvar=renderTable({
        data.frame(unclass(summary(dataini[3:ncol(dataini)])),check.names = F,stringsAsFactors = F)
      })
      updateTabsetPanel(session, "tabs1 ",selected = "Tabla de datos")
      }
      )}
  })
  
  #BOTON cortar
  observeEvent(input$cortbutt,{
    if (input$varselcut=="-"){}
    else{
      datcor=cortador(dataini,input$pun_cortes,input$varselcut)
      datacut=dataini
      datacut[,input$nomcor]=datcor
      dataini<<-datacut
      
      output$datatot=renderTable({
        dataini
      })
      updateSelectInput(session,"varsel",choices=c("-",colnames(dataini)[3:length(colnames(dataini))]))
      updateSelectInput(session,"varselcut",choices=c("-",colnames(dataini)[3:length(colnames(dataini))]))
      output$tabvar=renderTable({
        data.frame(unclass(summary(dataini[3:ncol(dataini)])),check.names = F,stringsAsFactors = F)
      })
      updateTabsetPanel(session, "tabs1 ",selected = "Tabla total")
    }
  })
  
  # BOTON exportador
  observeEvent(input$export,{
    print(graf.fin)
  }
  )
  
  #---------------------------------------------------------------------
  ##            ESTETiCA             ##
  
  #temaBBVA
  temaBBVA=theme(panel.background=element_blank(),
                legend.position='bottom',axis.title.x=element_blank(),
                 axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),
                 panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
                legend.key.width = unit(1.3,"cm"))
  
  
  #---------------------------------------------------------------------
  #---------------------------------------------------------------------
  ##            DIBUJA EL GRAFICO             ##
  
  mapabut= eventReactive(input$Dibujar, {
    
    shinyjs::enable("export")
    mapa=graphing(dataini,input$setter,input$selCCAA,input$selprov,input$varsel,input$slidermed)
    varused=input$varsel
    mapa[3]=varused
    return(mapa)
  })
  
  output$plot=renderPlot({
      maparet=mapabut()
      
      dicc=which(colnames(maparet[[2]])==maparet[3])
      withProgress(message = 'Coloreando', value = 0,{
        incProgress(0.0,detail="Procesando")
      if (is.null(maparet[[1]])){}
      else{
        incProgress(0.5,detail="Insertando colores")
        if (input$sel_col==0){
          plot=modif(ggpl=maparet[[1]],mpdf=maparet[[2]],medcheck=input$automed,slidermed=input$slidermed,
                     acc=dicc,pickmed=input$pickmed,colmed=input$colmed)
          graf.fin<<-plot+
            temaBBVA
          incProgress(0.25,detail="Finalizando")
          graf.fin
        }
        else{
          if (input$entmin==""  | input$entmax == ""){}
          else {
          plot=modif(ggpl=maparet[[1]],mpdf=maparet[[2]],medcheck=input$automed,slidermed=input$slidermed,
                     acc=dicc,colmin=input$entmin,colmax=input$entmax,pickmed=input$pickmed,colmed=input$colmed)
          graf.fin<<-plot+
            temaBBVA
          incProgress(0.25,detail="Finalizando")
          graf.fin
          }
        }
      
      }
      
    })
  })
  
  
})
