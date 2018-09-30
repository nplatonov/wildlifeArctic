'main' <- function(fname) {
   require(leaflet)
   isRmd <- !inherits(try(rmarkdown::all_output_formats(knitr::current_input()))
                     ,"try-error")
   if (is.null(md_lang <- getOption("md_lang")))
      md_lang <- "en"
  # fname <- c("hotel.geojson","cafe.geojson","fast_food.geojson","restaurant.geojson")
   fname <- dir(path=".",pattern="\\.geojson$",full.names=TRUE)
   bname <- basename(fname)
   da <- lapply(fname,function(f){
      da <- sf::read_sf(f) %>% sf::st_transform(crs=4326)
      ind <- which(vapply(da,function(col) !inherits(col,"sfc"),TRUE))
      attr(da,"label") <- da$name # paste("FID:",seq(nrow(da)))
      query2 <- unname(apply(da,1,function(x){
         x <- x[ind]
         x <- x[!sapply(x,is.na)]
         paste(paste0("<TR><TD><B>",names(x),"</B></TD>"
                     ,"<TD>",as.character(x),"</TD></TR>")
              ,collapse="")
      }))
      attr(da,"popup") <- paste0("<TABLE>",query2,"</TABLE>")
      mycol <- ursa::colorize(seq(nrow(da)),stretch="category",ncolor=nrow(da)
                             ,pal.dark=0.3,pal.light=0.7)
      if (nrow(da)<5)
         mypal <- colorFactor(as.character(mycol$colortable),domain=mycol$index)
      else
         mypal <- colorNumeric(as.character(mycol$colortable),domain=mycol$index)
      attr(da,"pal") <- mypal#(seq(nrow(da)))
      attr(da,"geoType") <- unique(as.character(sf::st_geometry_type(da)))
     # str(unclass(da))
     # print(object.size(da))
      da
   })
   iconHotel <- makeAwesomeIcon(icon='bed',library='fa'
                               ,markerColor='blue',iconColor='black')
   iconRestaurant <- makeAwesomeIcon(icon='cutlery',library='fa'
                              ,markerColor='pink',iconColor='black')
   iconCafe <- makeAwesomeIcon(icon='glass',library='fa'
                              ,markerColor='lightblue',iconColor='black')
   iconFastFood <- makeAwesomeIcon(icon='coffee',library='fa'
                              ,markerColor='lightgreen',iconColor='black')
   iconVenue <- makeAwesomeIcon(icon='university',library='fa'
                               ,markerColor='orange',iconColor='black')
   iconPlane <- makeAwesomeIcon(icon='plane',library='fa'
                               ,markerColor='green',iconColor='black')
   iconTrain <- makeAwesomeIcon(icon='train',library='fa'
                               ,markerColor='green',iconColor='black')
   iconOther <- makeAwesomeIcon(icon='flag',library='fa'
                              ,markerColor='gray',iconColor='black')
   provList <- c("OpenStreetMap.Mapnik"
                ,switch(md_lang,ru="CartoDB.Positron","Esri.WorldStreetMap")
                )
   m <- leaflet()
   m <- setView(m,lng=40.551,lat=64.5429,zoom=13)
   for (p in c(provList)) {
      m <- addProviderTiles(m,providers[[p]],group=p)
   }
   if (FALSE) {
      m <- addTiles(m,urlTemplate=""
                  # ,options=tileOptions(minZoom=minZ,maxZoom=maxZ)
                  ,group="blank")
      provList <- c(provList,"blank")
   }
   m <- addAwesomeMarkers(m,lng=40.5486,lat=64.52872#,radius=3
                         ,popup=paste("САФУ 1 корпус"
                                     ,"набережная Северной Двины, 17"
                                     ,sep="<BR>")
                         ,label=switch(md_lang,ru="Место проведения заседаний"
                                              ,"Conference venue")
                         ,icon=iconVenue
                       ,group=switch(md_lang,ru="Конференция","Conference"))
   m <- addAwesomeMarkers(m,lng=40.5742,lat=64.55076
                         ,popup="ст. Архангельск-Город"
                       ,label=switch(md_lang,ru="Железнодорожный вокзал"
                                    ,"Railway Station")
                       ,icon=iconTrain
                     ,group=switch(md_lang,ru="Транспорт","Terminals"))
   m <- addAwesomeMarkers(m,lng=40.71328,lat=64.59610
                         ,popup=switch(md_lang,ru="Аэропорт Талаги","ARH/ULAA")
                         ,label=switch(md_lang,ru="Аэропорт г. Архангельск"
                                      ,"Airport")
                         ,icon=iconPlane
                     ,group=switch(md_lang,ru="Транспорт","Terminals"))
   ovName <- if (md_lang=="ru") c("Конференция","Транспорт")
             else c("Conference","Terminals")
   for (i in seq_along(da)) {
      gType <- attr(da[[i]],"geoType")
     # print(gType)
      for (mType in c("POINT")) {
         if (mType %in% gType) {
            if (length(grep("hotel",bname[i],ignore.case=TRUE))) {
               grName <- switch(md_lang,ru="Гостиницы","Hotel")
               icon <- iconHotel
               zoom <- 6:12
            }
            else if (length(grep("cafe",bname[i],ignore.case=TRUE))) {
               grName <- switch(md_lang,ru="Кафе","Cafe")
               icon <- iconCafe
               zoom <- 12:17
            }
            else if (length(grep("restaurant",bname[i],ignore.case=TRUE))) {
               grName <- switch(md_lang,ru="Рестораны","Restaurant")
               icon <- iconRestaurant
               zoom <- 12:17
            }
            else if (length(grep("fast_food",bname[i],ignore.case=TRUE))) {
               grName <- switch(md_lang,ru="Фаст-фуд","Fast Food")
               icon <- iconFastFood
               zoom <- 12:17
            }
            else {
               grName <- bname[i]
               icon <- iconOther
               zoom <- 1:16
            }
            ovName <- c(ovName,grName)
            print(zoom)
            m <- addAwesomeMarkers(m,data=da[[i]]
                            ,label=attr(da[[i]],"label")
                            ,popup=attr(da[[i]],"popup")
                            ,options=markerOptions(opacity=0.7)
                           # ,clusterOptions=markerClusterOptions()
                            ,icon=icon
                            ,group=grName)
         }
      }
   }
   m <- addLayersControl(m,position="topleft"
                        ,baseGroups=c(provList),overlayGroups=ovName
                        ,options=layersControlOptions(collapsed=TRUE)
                        ) %>%
      hideGroup(c("Кафе","Рестораны","Фаст-фуд","Cafe","Restaurant","Fast Food"
                 )) %>%
     # addMeasure(primaryLengthUnit="meters",primaryAreaUnit="sqmeters") %>%
     # addMiniMap(zoomLevelOffset=-6
     #           ,tiles=providers$OpenStreetMap.Mapnik
     #           ,toggleDisplay=TRUE
     #           ,position = "bottomright") %>%
      addScaleBar("topleft"
                 ,options = scaleBarOptions(imperial=FALSE,maxWidth=100)
                 )
   if (!isRmd) {
      fname <- "leaflet.html"
      htmlwidgets::saveWidget(m,file=fname)
      system2("open",list(fname))
   }
   m
}
m <- main()
