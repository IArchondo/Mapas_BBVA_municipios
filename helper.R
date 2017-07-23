library(maptools)
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(plyr)
library(rgdal)
library(rgeos)


cortador=function(dat,str,var){
  dicc_cut=which(colnames(dat)==var)
  cutvar=as.numeric(unlist(strsplit(str,split=",")))
  cuteado=cut(dat[[dicc_cut]],breaks=cutvar,include.lowest = T,dig.lab=10)
  return(cuteado)
}

graphing=function(datax,setter,selCCAA,selProv,varsel,slidermed){
  withProgress(message = 'Creando mapa', value = 0,{
    incProgress(0.0,detail="Leyendo mapa")
    map=readShapeSpatial('./Shapefile/mun_map.shp')
    map@data$Cod_CCAAint=as.integer(map@data$Cod_CCAA)
    map@data$Cod_Provint=as.integer(map@data$Cod_Prov)
    incProgress(0.25,detail="Combinando con datos")
    map@data=merge(map@data,datax,by.x='Codigoint',by.y='CodMun',sort=F)
    map@data$id=rownames(map@data)
    incProgress(0.25,detail="Plotting")
    map.points=fortify(map,region='id')
    map.df=join(map.points,map@data,by='id')
    map.df$Cod_CCAAint=as.integer(map.df$Cod_CCAA)
    
    
      if(varsel=="-"){return (NULL)}
      else {
        dicc=grep(varsel,colnames(map.df))
        incProgress(0.25,detail="Dibujando")
        if (setter==0){
          mapa=ggplot(map.df)+
            aes_string("long","lat",group="group",fill=varsel)+
            geom_polygon()+
            geom_path(alpha=0)+
            coord_equal()
          return(list(mapa,map.df))
          incProgress(0.12,detail="Finalizando")
        }
        else {
          if (setter==1){
          map.dfx<-subset(map.df,map.df$Cod_CCAAint==selCCAA)
          mapa=ggplot(map.dfx)+
            aes_string("long","lat",group="group",fill=varsel)+
            geom_polygon()+
            geom_path(alpha=0)+
            coord_equal()
          return(list(mapa,map.dfx))
          incProgress(0.12,detail="Finalizando")
          }
          else {
            map.dfp<-subset(map.df,map.df$Cod_Provint==selProv)
            mapa=ggplot(map.dfp)+
              aes_string("long","lat",group="group",fill=varsel)+
              geom_polygon()+
              geom_path(alpha=0)+
              coord_equal()
            return(list(mapa,map.dfp))
            incProgress(0.12,detail="Finalizando")
          }
        }
      }
  })
}



#modificador

modif=function(ggpl,mpdf,colmin='#89D1F3',colmax='#006EC1',medcheck,slidermed,acc,pickmed,colmed){
  colormed="white"
  if(pickmed==T){colormed=colmed}
  if(pickmed==T & colmed==""& is.factor(mpdf[[acc]])==F){}
  else{
  if (is.factor(mpdf[[acc]])==TRUE){
    gs.pal <- colorRampPalette(c(colmin,colmax),bias=.1,space="rgb")
    plot=ggpl+
      scale_fill_manual(values=gs.pal(nlevels(mpdf[[acc]])),label=comma)
    return(plot)
  }
  else{
  if (medcheck==FALSE){
    plot=ggpl+
      scale_fill_gradient(low=colmin,high=colmax,label=comma)
    return(plot)
  }
  else{
    if (slidermed>=0){
    plot=ggpl+
      scale_fill_gradient2(low=colmin,mid=colormed,high=colmax,midpoint=slidermed*max(mpdf[acc]),label=comma)
    return(plot)
    } else {
      plot=ggpl+
        scale_fill_gradient2(low=colmin,mid=colormed,high=colmax,midpoint=-(slidermed*min(mpdf[acc])),label=comma)
      return(plot)
    }
  }
  }}
  
}


