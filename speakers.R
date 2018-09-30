invisible({
   isRmd <- !inherits(try(rmarkdown::all_output_formats(knitr::current_input()))
                     ,"try-error")
   pdfpath <- c("./pdf","http://sevin.000webhostapp.com/wildlifeArctic/pdf")[2]
   if (is.null(md_lang <- getOption("md_lang")))
      md_lang <- "en"
   width <- switch(md_lang,ru=50,62)
   speech <- readRDS("abstract.Rds")
   speaker <- sapply(speech,function(x){
      y <- x[[md_lang]]$speaker
      if (is.null(y))
         y <- ""
      y
   })
   empty <- which(nchar(speaker)<3)
   id <- sapply(speech,function(x) x$id)
   spId <- sapply(speech,function(x) x$spId)
   kind <- sapply(speech,function(x) x$kind)
   key <- sapply(speech,function(x) x$key)
   hasPDF <- sapply(speech,function(x) x$hasPDF)
   indPDF <- which(hasPDF)
   if (FALSE) { ## moved to '_02-read_md.R'
      for (i in sample(seq_along(key))) {
        # str(speech[[i]]);q()
         src <- file.path("../pdf.key",paste0(key[i],".pdf"))
         if (!file.exists(src))
            next
         dst <- file.path("./wildlifeArctic/pdf",paste0(id[i],".pdf"))
         file.copy(src,dst,overwrite=FALSE,copy.date=TRUE)
      }
   }
   subj <- sapply(speech,function(x) x$subject)
   when <- sapply(speech,function(x) {
      y <- x$when
      if (is.null(y))
         y <- ""
      y
   })
  # print(data.frame(id=id,spId=spId,speaker=speaker,when=when,subj=subj))
   rasp0 <- readRDS("rasp0.rds")
   rasp1 <- readRDS("rasp1.rds")
   locale <- readRDS("locale.rds")
   ind1 <- match(when,rasp0$id)
   ind2 <- which(!is.na(ind1))
   ind3 <- c(na.omit(ind1))
   t3 <- rep("",length(when))
   anchor <- rep("",length(when))
   anchor[ind2] <- paste0(format(rasp0[ind1[ind2],"from"],"d%j_")
                         ,rasp0[ind1[ind2],"activity"])
   t3[ind2] <- paste0(format(rasp0[ind1[ind2],"from"],"%d %b, %H:%M-")
                ,format(rasp0[ind1[ind2],"to"],"%H:%M"))
   ind <- grep("poster",kind,ignore.case=TRUE)
   ind1 <- which(sapply(rasp1,function(x){
      length(grep("poster",x$activity,ignore.case=TRUE)>0)
   })>0)
   n <- 1 ## ncolumns
   for (i in sample(seq_along(ind1))) {
      ind1a <- ind1[i]
      ind2 <- grep("poster",rasp1[[ind1a]]$activity,ignore.case=TRUE)
      d3 <- as.Date(names(ind1a))
      rasp0a <- rasp0[as.Date(rasp0$from)==d3,]
      rasp0a <- rasp0a[grep("poster",rasp0a$section2,ignore.case=TRUE),]
      sec <- unlist(strsplit(gsub("\\D","",unique(rasp0a$section2)),split=""))
      ind0 <- ind[which(!is.na(match(subj[ind],sec)))]
      t2 <- paste0(format(d3,"%d %b "),rasp1[[ind1a]]$time[ind2],"*")
      t3[ind0] <- t2
      anchor[ind0] <- paste0(format(d3,"d%j_"),rasp1[[ind1a]][ind2,"activity"])
   }
   indC <- grep("(cancel|absent)",kind,ignore.case=TRUE) ## only "cancel"
   if (length(indC)) {
      t3[indC] <- switch(md_lang,ru="Отменено","Canceled")
      anchor[indC] <- "canceled"
   }
   title <- sapply(speech,function(x) {
      res <- paste0(substr(x[[md_lang]]$title,1,width),"&#8230; ")
   })
  # indAbsent <- kind %in% c("Absent")
  # print(table(indAbsent))
  # print(t3)
  # q()
   t3 <- gsub("\\s+","&nbsp;",t3)
   if (n<=1)
      link <- title
   else
      link <- rep("",length(hasPDF))
   link[indPDF] <- paste0(link[indPDF]
                         ,"[",t3[indPDF],"](#",anchor[indPDF],"_",md_lang,")"
                         ,"&nbsp;([",switch(md_lang,ru="Аннотация","Abstract"),"]"
                         ,"(#",id[indPDF],"_",md_lang,")"
                         ,",&nbsp;[",switch(md_lang,ru="Презентация","Presentation"),"]"
                         ,"(",pdfpath,"/",id[indPDF],".pdf","))"
                         )
   link[-indPDF] <- paste0(link[-indPDF]
                          ,"[",t3[-indPDF],"](#",anchor[-indPDF],"_",md_lang,")"
                          ,"&nbsp;([",switch(md_lang,ru="Аннотация","Abstract"),"]"
                          ,"(#",id[-indPDF],"_",md_lang,"))"
                          )
  # print(head(link))
  # print(head(linkA))
  # print(head(id))
  # link[ind] <- paste0(link[ind]," *")
   da <- data.frame(sp=speaker,link=link)#,linkA=linkA)
   if (length(empty))
      da <- da[-empty,]
   sep <- "<br>"
   da2 <- aggregate(da,list(speaker=da$sp),function(x) {
      xu <- unique(x)
      if (length(xu)>1) {
        # d <- gsub(".*\\D(d\\d{3})\\D.*","\\1",xu)
         d <- gsub("&nbsp;"," ",xu)
         d <- gsub("^.*(\\d{2}\\s\\S{3})(\\,)*\\s(\\d{2}:\\d{2}).+$","\\1 \\3",d)
         d <- as.POSIXct(paste("2004",d),format="%Y %d %b %H:%M")
         xu <- xu[order(d)]
      }
      paste(xu,collapse=sep)
   })#[,-1]
   sortInd <- stringi::stri_order(da2$speaker)
   da2 <- da2[sortInd,]
   if (n<=1)
      da2$speaker <- paste0("**",da2$speaker,"**")
  # str(da2)
  # q()
  # da2$speaker <- paste0("<a name=",dQuote(paste0(da2$spId,"_",md_lang))
  #                      ,"></a>",da2$speaker)
  # listA <- paste(da2$link,da2$linkA,sep=" ")
   listA <- paste(da2$speaker,da2$link,sep=sep)
   m <- n-length(listA)%%n
   if (m!=n)
      listA <- c(listA,rep("",m))
   listA <- as.data.frame(matrix(listA,ncol=n))
   colnames(listA) <- rep("",ncol(listA))
   asterix <- switch(md_lang,ru="Звездочкой (*) помечена постерная сессия"
                            ,"Poster presentation is marked by asterix (*)")
   if (isRmd)
      cat(paste0("\n\n",asterix,"\n\n"))
   a2 <- knitr::kable(listA,format="markdown",row.names=FALSE
                     ,padding=0,longtable=TRUE
                     )
   if (isRmd)
      print(a2)
   else
      print(listA)
   if (!isRmd)
      message("Ok")
  # if (isRmd)
  #    cat("\n\n[^*]: Постерная сессия\n\n")
})
