invisible({
   isRmd <- !inherits(output <- try(rmarkdown::all_output_formats(
                                          knitr::current_input())),"try-error")
   if (isRmd) {
      isPDF <- length(grep("pdf",output[[1]]))>0
      isHTML <- length(grep("html|github",output[[1]]))>0
      isBOOK <- length(grep("book",output[[1]]))>0
   }
   else
      isHTML <- TRUE
   if (is.null(md_lang <- getOption("md_lang")))
      md_lang <- "ru"
   rasp0 <- readRDS("rasp0.rds")[,c("from","to","activity","section1","section2")]
   rasp0 <- rasp0[!is.na(rasp0$section1) | !is.na(rasp0$section2),]
   rasp0$section1[is.na(rasp0$section1)] <- ""
   rasp0$section2[is.na(rasp0$section2)] <- ""
  # print(rasp0$section2)
   rasp0$section <- paste(rasp0$section1,rasp0$section2)
  # rasp0$section1 <- rasp0$section2 <- NULL
   subject <- readRDS("locale.rds")
   section <- subject$section
   subjO <- paste0("oral",seq(10)-1)
   subjP <- paste0("poster.*",seq(10)-1,".*")
   subjR <- paste0("round.*",seq(5))
   subjAll <- c(subjO,subjP,subjR)
  # sec0 <- section # grep(("oral|round"),section,ignore.case=TRUE,value=TRUE)
   schedule <- rep("",length(subjAll))
   names(schedule) <- subjAll
   julian <- rep("",length(subjAll))
   rasp0$julian <- format(rasp0$from,"%j")
   for (i in sample(seq_along(subjAll))) {
      s <- subjAll[i]
      if ((length(grep("oral",s,ignore.case=TRUE)))&&
         (!length(grep(s,section,ignore.case=TRUE))))
         next
     # j <- grep(paste0("oral",subjO[i]),subject,ignore.case=TRUE)
     # if (!length(j))
     #    next
      j1 <- grep(s,rasp0$section1,ignore.case=TRUE)
      j2 <- grep(s,rasp0$section2,ignore.case=TRUE)
      j <- c(j1,j2)
      if (!length(j))
         next
      raspA <- rasp0[j,]
      a0 <- raspA$activity
      j0 <- raspA$julian
      t1 <- aggregate(raspA[,"from"],by=list(activity=a0,julian=j0)
                     ,min)[,c("activity","x")]
      t2 <- aggregate(raspA[,"to"],by=list(activity=a0,julian=j0)
                     ,max)[,c("x"),drop=FALSE]
      res <- cbind(t1,t2)
      colnames(res)[tail(seq(colnames(res)),2)] <- c("from","to")
     # ind <- seq_along(t1$x)
      colnames(res) <- c("activity","from","to")
      res <- res[order(res$from),]
      anchor <- paste0(format(res$from,"d%j_"),res$activity,"_",md_lang)
      sameday <- length(unique(format(res$from,"%j")))==1
      julian[i] <- format(res$from[1],"%j")
      sameday <- FALSE
      if ((!sameday)||(nrow(res)==1)) {
         ##~ res <- paste(paste0(format(res$from,"%d %b %H:%M-")
                            ##~ ,format(res$to,"%H:%M")),collapse="<BR>")
         res <- paste0(format(res$from,"%d %b %H:%M&ndash;")
                            ,format(res$to,"%H:%M"))
         
      }
      else {
         res <- paste0(format(res$from[1],"%d %b<BR>")
                      ,paste(paste0(format(res$from,"%H:%M&ndash;")
                                  ,format(res$to,"%H:%M")),collapse="<BR>"))
      }
      res <- gsub("\\s+","&nbsp;",res)
      res <- paste0("[",res,"](#",anchor,")")
      res <- paste(res,collapse="; ")
      schedule[i] <- res
   }
   schedule <- schedule[nchar(schedule)>0]
   sname <- names(schedule)
   themeO <- sort(unique(gsub("\\D","",grep("(oral|poster)",sname
                 ,ignore.case=TRUE,value=TRUE))))
   themeR <- sort(unique(gsub("\\D","",grep("(round)",sname
                 ,ignore.case=TRUE,value=TRUE))))
   tname <- c(paste0("oral",themeO),paste0("round.*",themeR))
   da <- data.frame(section=tname,name="",title="",date="")
   for (i in sample(nrow(da))) {
      ind2 <- grep(da$section[i],subject$section,ignore.case=TRUE)
      da$name[i] <- subject[ind2,paste0("name_",md_lang)]
      da$title[i] <- subject[ind2,paste0("title_",md_lang)]
      d3 <- schedule[da$section[i]]
      if (is.na(d3))
         d3 <- character()
      if (length(grep("oral",sname[i],ignore.case=TRUE))) {
         ind3 <- grep(paste0("poster.*",gsub("\\D","",da$section[i])),sname
                     ,ignore.case=TRUE)
         d3p <- paste0(schedule[ind3],"*")
         if (length(ind3)) {
            d3 <- if (length(d3)) paste(d3,d3p,sep="<BR>") else d3p
         }
      }
     # names(d3) <- sname[i]
     # print(nchar(d3))
      da$date[i] <- d3
   }
   if (!isRmd) {
      da$title <- substr(da$title,1,23)
      da$date <- substr(da$date,1,23)
      print(da)
   }
   hasName <- !is.na(da$name)
   da$name[!hasName] <- ""
  # print(da$name)
  # print(da$title)
   da$name[hasName] <- paste(paste0("<b>",da$name[hasName],"</b>")
                            ,da$title[hasName],sep="<BR>")
  # if (length(noName))
      da$name[!hasName] <- da$title[!hasName]
   da$title <- ""
   indO <- grep("oral",da$section,ignore.case=TRUE)
   indR <- grep("round",da$section,ignore.case=TRUE)
   if (!isRmd)
      da$title <- substr(da$title,1,32)
   oral <- da[indO,-1]
   oral$title <- NULL
   if (!isHTML)
      colnames(oral) <- rep("",ncol(oral))
   else {
      if (md_lang=="ru")
         colnames(oral) <- c("Название","Время")
      else
         colnames(oral) <- c("Title","Time")
   }
   aO <- knitr::kable(oral,format="pandoc",row.names=FALSE)
   if (isRmd) {
      asterix <- switch(md_lang,ru="Постерные сессии отмечены звездочкой (*)"
                       ,"Poster Sessions are marked by asterix (*)")
      cat(paste0("\n\n### ",switch(md_lang,ru="Темы заседаний","Meeting topics")
                ,"{- #subjoral_",md_lang,"}\n\n"))
      print(aO)
      cat(paste0("\n\n<span style=\"font-size:85%;\">",asterix,"</span>\n\n"))
   }
   round <- da[indR,-1]
   round$title <- NULL
   if (!isHTML)
      colnames(round) <- rep("",ncol(round))
   else {
      if (md_lang=="ru")
         colnames(round) <- c("Название","Время")
      else
         colnames(round) <- c("Title","Time")
   }
   name_singular <- switch(md_lang,ru="Круглый стол","Round Table")
   name_plural <- switch(md_lang,ru="Круглые столы","Round Tables")
   aR <- knitr::kable(round,format="pandoc",row.names=FALSE,padding=2)
   if (isRmd) {
      cat(paste0("\n\n### ",ifelse(nrow(round)>1,name_plurar,name_singular)
                ,"{- #subjround_",md_lang,"}\n\n"))
      print(aR)
   }
   if (!isRmd)
      message("Ok")
})
