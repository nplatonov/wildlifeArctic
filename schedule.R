'main' <- function() {
   isRmd <- !inherits(output <- try(rmarkdown::all_output_formats(
                                          knitr::current_input())),"try-error")
   pdfpath <- c("./pdf","http://sevin.000webhostapp.com/wildlifeArctic/pdf")[2]
   if (isRmd) {
     # output <- rmarkdown::all_output_formats(knitr::current_input())[1]
      isPDF <- length(grep("pdf",output[[1]]))>0
      isHTML <- length(grep("html|github",output[[1]]))>0
      isBOOK <- length(grep("book",output[[1]]))>0
      isDOC <- length(grep("word",output[[1]]))>0
   }
   else {
      isHTML <- TRUE
      isDOC <- !FALSE
   }
   if (is.null(md_lang <- getOption("md_lang")))
      md_lang <- "ru"
   Sys.setlocale("LC_CTYPE",switch(md_lang,ru="Russian","Russian"))
   Sys.setlocale("LC_TIME",switch(md_lang,ru="Russian","English"))
   
   rasp0 <- readRDS("rasp0.rds")
   rasp1 <- readRDS("rasp1.rds")
   rasp2 <- readRDS("rasp2.rds")
   locale <- readRDS("locale.rds")
   speech <- readRDS("abstract.rds")
   d3 <- as.Date(names(rasp1))
   speaker <- sapply(speech,function(x) x$speaker)
   presentation <- sapply(speech,function(x) x$id)
   spId <- sapply(speech,function(x) x$spId)
   hasPDF <- sapply(speech,function(x) x$hasPDF)
   rasp5a <- rasp1
   poster <- locale[grep("^poster",locale$section,ignore.case=TRUE)
                   ,paste0("name_",md_lang)]
   for (i in sample(seq_along(d3))) {
      rasp4 <- rasp1[[i]]
      ind <- match(rasp4$activity,locale$section)
      ind2 <- grep("session|poster|detail",rasp4$activity,ignore.case=TRUE)
      ind3 <- grep("poster",rasp4$activity,ignore.case=TRUE)
      anchor <- paste0(format(d3[i],"#d%j_"),rasp4$activity,"_",md_lang)
      rasp4$activity <- locale[ind,paste0("name_",md_lang)]
      if (length(ind3)) {
         ac <- rasp4$activity[ind3]
      }
      if (!isDOC)
         rasp4$activity[ind2] <- paste0("[",rasp4$activity[ind2],"](",anchor[ind2],")")
      if ((length(ind3))&&(!isDOC)) {
         posterS <- gsub("\\s","&nbsp;",poster)
         ac <- gsub(sprintf("(.*)(%s)(.*)",poster)
                   ,sprintf("\\1[%s](%s)\\3",posterS,anchor[ind3]),ac)
         rasp4$activity[ind3] <- ac
      }
      rasp5a[[i]] <- rasp4
   }
   rasp5a <- lapply(rasp5a,function(x) {
      if (!isDOC)
         x$time <- paste0("~",x$time,"~")
      else
         x$time <- paste0("&#945;~",x$time,"~&#945;")
      y <- apply(x,1,function(y){
         paste(rev(y),collapse="<BR>") # &nbsp;
      })
      y <- data.frame(a=y)
      y
   })
   forcedDiv <- !FALSE ## FAILED for mobile browsers
   if (all(sapply(rasp5a,is.data.frame)))
      for (i in seq_along(d3)) {
         label <- format(d3[i],"[%a, %d %b](#d%j")
         colnames(rasp5a[[i]]) <- paste0(label,"_",md_lang,")")
      }
   if ((isDOC)||(!forcedDiv)) {
      rasp5a <- lapply(rasp5a,function(x) {
         unname(sapply(x,function(y) paste(y,collapse="<BR>")))
      })
      if (isDOC) {
         rname <- format(as.Date(names(rasp5a)),"%a, %d %B")
         rname <- gsub("Январь","января",rname)
         rname <- gsub("Февраль","февараля",rname)
         rname <- gsub("Март","марта",rname)
         rname <- gsub("Апрель","апреля",rname)
         rname <- gsub("Май","мая",rname)
         rname <- gsub("Июнь","июня",rname)
         rname <- gsub("Июль","июля",rname)
         rname <- gsub("Август","августа",rname)
         rname <- gsub("Сентябрь","сентября",rname)
         rname <- gsub("Октябрь","октября",rname)
         rname <- gsub("Ноябрь","ноября",rname)
         rname <- gsub("Декабрь","декабря",rname)
         rname <- paste0("**",rname,"**")
      }
      rasp5a <- as.data.frame(rasp5a)
      colnames(rasp5a) <- rname
   }
   if (!isRmd)
      str(rasp5a)
   if (isRmd) {
      if (!is.data.frame(rasp5a)) {
         n <- length(rasp5a)
         if (forcedDiv) {
            cat(paste0("\n\n<div class=\"col",n,"\">\n\n"))
            for (i in seq(n)) {
               a3 <- knitr::kable(rasp5a[[i]],format="pandoc",row.names=FALSE)
               print(a3)
            }
            cat("\n</div>\n\n")
         }
         else {
            a3 <- knitr::kable(rasp5a,format="pandoc",row.names=FALSE)
            print(a3)
         }
      }
      else {
         a3 <- knitr::kable(rasp5a,format='pandoc',row.names=FALSE)
         print(a3)
      }
   }
   for (i in seq_along(d3)) {
      toBreak <- FALSE
      if (!isRmd)
         print(d3[i])
      if (isRmd)
         cat(paste0(format(d3[i],"\n\n## %A, %d %b {- #d%j_"),md_lang,"}\n\n"))
      rasp2a <- rasp2[[which(as.Date(names(rasp2))==d3[i])]]
      rasp5b <- rasp1[[i]]
      rasp5b$detail <- ""
      d <- sapply(rasp2a,function(x) {
         y <- grep("time",names(x),ignore.case=TRUE,value=TRUE,invert=TRUE)
         if (length(grep("poster",y,ignore.case=TRUE))) {
            y <- paste0("Oral",unlist(strsplit(gsub("\\D","",y),split="")))
         }
         indR <- grep("round",y,ignore.case=TRUE)
         y <- locale[match(y,locale$section),paste0("name_",md_lang)]
         if (length(indR))
            y[indR] <- paste(switch(md_lang,ru="Круглый стол",en="Round Table")
                            ,y[indR])
         y <- paste(na.omit(y),collapse=", ")
         y
      })
      rasp5b[match(names(d),rasp5b$activity),"detail"] <- d
      if (!isRmd)
         rasp5b$detail <- substr(rasp5b$detail,1,48)
     # indA <- grep("session|poster|detail",rasp5b$activity,ignore.case=TRUE)
      indA <- seq_along(rasp5b$activity)
      nameA <- rasp5b$activity[indA]
     # print(nameA)
     # print(names(rasp2a))
     # next
      anchor <- paste0(format(d3[i],"d%j_"),rasp5b$activity,"_",md_lang)
      ind <- match(rasp5b$activity,locale$section)
      indB <- grep("poster",rasp5b$activity,ignore.case=TRUE)
      rasp5b$activity <- locale[ind,paste0("name_",md_lang)]
      if (length(indB))
         ac <- rasp5b$activity[indB]
      rasp5b$activity[indA] <- paste0("[",rasp5b$activity[indA],"](#",anchor[indA],")")
      if ((length(indB))&&(!isDOC)) {
         rasp5b$activity[indB] <- gsub(sprintf("(.*)(%s)(.*)",poster)
                                      ,sprintf("\\1[\\2](#%s)\\3",anchor[indB]),ac)
        # rasp5b$detail[indB] <- ""
      }
     # print(rasp5b$activity)
     # q()
      a <- paste0(rasp5b$activity
                 ,ifelse(!nchar(rasp5b$detail),"",paste0(" (",rasp5b$detail,")")))
      da <- data.frame(Activity=a,Time=rasp5b$time)
      if (!isHTML)
         colnames(da) <- rep("",ncol(da))
      else if (md_lang=="ru")
         colnames(da) <- c("Мероприятие","Время")
      a3 <- knitr::kable(da,format="markdown"
                        ,row.names=FALSE 
                        ,longtable=TRUE,escape=FALSE
                  )
      if ((isRmd)&&(!isDOC))
         print(a3)
      session <- names(rasp2a)
      sname <- locale[match(session,locale$section),paste0("name_",md_lang)]
      sname <- paste(sname,format(d3[i],"%d %b"))
     # stime <- sapply(rasp2a,function(x) x$time)
     # print(stime)
      for (k in seq_along(indA)) {
         isAttention <- !(nameA[k] %in% names(rasp2a))
         if ((isAttention)&&(isRmd)) {
            t3 <- rasp1[[i]]$time[match(nameA[k],rasp1[[i]]$activity)]
           # ac <- rasp1[[i]]$activity
            ind <- match(nameA[k],locale$section)
            name <- locale[ind,paste0("name_",md_lang)]
            title <- locale[ind,paste0("title_",md_lang)]
            name <- paste(name,format(d3[i],"%d %b"))
            cat("\n\n")
            cat(paste0(ifelse(isDOC,"###","####")," ",name," "
                      ,t3," {- #",format(d3[i],"d%j_")
                      ,nameA[k],"_",md_lang,"}"))
            cat("\n\n")
            if (is.na(title)) {
               if (isDOC)
                  cat("\n\n")
               next
            }
            ind2 <- grep("\\.md$",title)
            if (length(ind2)) { ## details are in *.md
               for (j in ind2) {
                  Fin <- file(title[j],encoding="utf8")
                  cat(paste(readLines(Fin),collapse="\n"))
                  close(Fin)
               }
            }
            else {
               cat(title)
            }
            cat("\n\n")
            next
         }
         j <- match(nameA[k],names(rasp2a))
         rasp2b <- rasp2a[[j]]
         if (length(grep(poster,sname[j]))) {
            sname[j] <- paste(poster,format(d3[i],"%d %b"))
         }
         if (isRmd)
            cat(paste0("### ",sname[j]," ",rasp2b$time
                      ,"{- #",format(d3[i],"d%j_"),session[j],"_",md_lang,"}\n\n"))
         pname <- names(rasp2b)
         indO <- grep("oral",pname,ignore.case=TRUE)
         if (length(indO)) {
            oname <- locale[match(pname[indO],locale$section),paste0("name_",md_lang)]
            tname <- locale[match(pname[indO],locale$section),paste0("title_",md_lang)]
            if (!isDOC)
               sec <- paste0("#### ","[",oname,"](#subjoral_",md_lang,")"
                            ,": <i>",tname,"</i>\n\n")
            else
               sec <- paste0("#### ",oname,": <i>",tname,"</i>\n\n")
            for (k in seq_along(indO)) {
              # toBreak <- TRUE
               if (isRmd) {
                  if (!is.na(tname[k]))
                     cat(sec[k])
               }
               rasp2c <- rasp2b[[indO[k]]]
               ind <- match(rasp2c$presentation,presentation)
               sp <- sapply(speech[ind],function(x) {
                  if ((isDOC)&&(md_lang=="ru")) {
                     y <- x[[md_lang]]$FIO
                  }
                  else
                     y <- x[[md_lang]]$speaker
                  y[is.null(y)] <- ""
                  y
               })
               pr <- sapply(speech[ind],function(x) {
                  y <- x[[md_lang]]$title
                  y[is.null(y)] <- ""
                  y
               })
               isA <- which(sapply(speech[ind],function(x) {
                  length(x[[md_lang]]$abstract)>0
               }))
               if (!isDOC) {
                  pr[isA] <- paste0("[",pr[isA],"]","(#",rasp2c$presentation[isA]
                                   ,"_",md_lang,")")
               }
               pr1 <- paste0(pr," ([",switch(md_lang,ru="Презентация","Presentation")
                            ,"](",pdfpath,"/",presentation[ind],".pdf","))")
               pr[hasPDF[ind]] <- pr1[hasPDF[ind]]
               rasp2d <- data.frame(sp=sp,pr=pr,time=rasp2c$time)
               indC <- which(sapply(speech[ind],function(x) x$kind %in% "Absent"))
               if (length(indC)) {
                  indD <- grep("absent",locale$section,ignore.case=TRUE)
                  rasp2d$time[indC] <- locale[indD,paste0("name_",md_lang)]
               }
               col3 <- switch(md_lang,ru="Время","Time")
               col1 <- switch(md_lang,ru="Докладчик","Speaker")
               col2 <- switch(md_lang,ru="Название","Title")
               if (TRUE) {
                  col1 <- paste0("[",col1,"](#speakers_",md_lang,")")
               }
               colnames(rasp2d) <- c(col1,col2,col3)
               if (isDOC) {
                  colnames(rasp2d) <- c("sp","title","time")
                  rasp2d$sp <- paste0("~",rasp2d$time,"~ ***"
                                     ,rasp2d$sp,"*** ",rasp2d$title)
                  rasp2d$title <- NULL
                  rasp2d$time <- NULL
                  colnames(rasp2d) <- ""
               }
               a2 <- knitr::kable(rasp2d,format="markdown",row.names=FALSE
                          # ,caption=paste0("<b>",sec[j],"</b>")
                          # ,booktabs=!TRUE,longtable=!TRUE,table.attr="id=\"mytable\""
                           )
               if (isRmd)
                  print(a2)
              # else
              #    str(rasp2d)
               if (toBreak)
                  break
            }
         }
         indP <- grep("poster",pname,ignore.case=TRUE)
         if (length(indP)) {
            rasp2c <- rasp2b[[indP]]
           # if (!isRmd)
           #    print(rasp2c)
            ind <- match(rasp2c$presentation,presentation)
            sp <- sapply(speech[ind],function(x) {
               y <- x[[md_lang]]$speaker
               y[is.null(y)] <- ""
               y
            })
            pr <- sapply(speech[ind],function(x) {
               y <- x[[md_lang]]$title
               y[is.null(y)] <- ""
               y
            })
            subj <- sapply(speech[ind],function(x) {
               indS <- grep(paste0("oral",x$subject),locale$section,ignore.case=TRUE)
               s <- locale[indS,paste0("name_",md_lang)]
               s <- gsub("\\s+","&nbsp;",s)
               if (!isDOC)
                  s <- paste0("[",s,"](#subjoral_",md_lang,")")
               s
            })
            if (!isDOC)
               pr <- paste0("[",pr,"]","(#",rasp2c$presentation,"_",md_lang,")")
            pr1 <- paste0(pr," ([",switch(md_lang,ru="Постер","Poster")
                         ,"](",pdfpath,"/",presentation[ind],".pdf","))")
            pr[hasPDF[ind]] <- pr1[hasPDF[ind]]
            rasp2d <- data.frame(sp=sp,pr=pr,subj=subj)
           # col3 <- switch(md_lang,ru="Время","Time")
            col1 <- switch(md_lang,ru="Докладчик","Speaker")
            col2 <- switch(md_lang,ru="Название","Title")
            col3 <- switch(md_lang,ru="Направление","Subject")
            if (TRUE) {
               col1 <- paste0("[",col1,"](#speakers_",md_lang,")")
            }
            colnames(rasp2d) <- c(col1,col2,col3)
            if (isDOC) {
               colnames(rasp2d) <- c("sp","pr","subj")
               rasp2d$sp <- paste0("***",rasp2d$sp,"*** ",rasp2d$pr
                                  ,"(*",rasp2d$subj,"*)")
               rasp2d$pr <- NULL
               rasp2d$subj <- NULL
               colnames(rasp2d) <- ""
            }
            a2 <- knitr::kable(rasp2d,format="markdown",row.names=FALSE
                       # ,caption=paste0("<b>",sec[j],"</b>")
                       # ,booktabs=!TRUE,longtable=!TRUE,table.attr="id=\"mytable\""
                        )
            if (isRmd)
               print(a2)
         }
         indR <- grep("round",pname,ignore.case=TRUE)
         if (TRUE) { ## round tables inline of the sections 
            if (length(indR)) {
               rasp2c <- rasp2b[indR]
               if (length(indR)>1)
                  round1 <- switch(md_lang,ru="Круглые столы","Round Tables")
               else
                  round1 <- switch(md_lang,ru="Круглый стол","Round Table")
               if (!isDOC)
                  round1 <- paste0("[",round1,"](#subjround_",md_lang,")")
               if (isRmd)
                  cat(paste0("\n\n#### ",round1,"\n\n"))
               ind <- match(names(rasp2c),locale$section)
               t3 <- sapply(rasp2c,function(x) x$time)
               da <- data.frame(name=locale[ind,paste0("name_",md_lang)]
                               ,title=locale[ind,paste0("title_",md_lang)]
                               ,time=t3)
               da$name[is.na(da$name)] <- ""
              # da$name <- paste0("[",da$name,"](#subjround_",md_lang,")")
               if (!isRmd) {
                  da$title <- substr(da$title,1,32)
                 # print(da)
               }
               else {
                  da$name <- gsub("\\s+","&nbsp;",da$name)
                  if (isDOC) {
                     da$name <- paste0("~",da$time,"~ **",da$name,"** *",da$title,"*")
                     da$title <- NULL
                     da$time <- NULL
                  }
                  colnames(da) <- rep("",ncol(da))
                  a2 <- knitr::kable(da,format="markdown",row.names=FALSE)
                  print(a2)
               }
            }
         }
         if (toBreak)
            break
      }
      if (FALSE) { ## round tables on the bottom
         rtable <- lapply(rasp2a,function(x) {
            ind <- grep("round",names(x),ignore.case=TRUE)
            if (length(ind))
               return(data.frame(name=names(x)[ind],time=x[[ind]]$time))
            NULL
         })
         rtable <- rtable[which(!sapply(rtable,is.null))]
         if (length(rtable)) {
            rtable <- do.call("rbind",lapply(rtable,data.frame))
            if (nrow(rtable)>1)
               round1 <- switch(md_lang,ru="Круглые столы","Round Tables")
            else
               round1 <- switch(md_lang,ru="Круглый стол","Round Table")
            if (isRmd)
               cat(paste0("\n\n### ",round1," {- #table",format(d3[i],"%j")
                         ,"_",md_lang,"}","\n\n"))
            ind <- match(rtable$name,locale$section)
            da <- data.frame(name=locale[ind,paste0("name_",md_lang)]
                            ,title=locale[ind,paste0("title_",md_lang)]
                            ,time=rtable$time)
            da$name <- paste0("[",da$name,"](#subjround_",md_lang,")")
            if (!isRmd) {
               da$title <- substr(da$title,1,32)
              # print(da)
            }
            else {
               da$name <- gsub("\\s+","&nbsp;",da$name)
               colnames(da) <- rep("",ncol(da))
               a2 <- knitr::kable(da,format="markdown",row.names=FALSE)
               print(a2)
            }
         }
      }
      if (toBreak)
         break
   }
  # ind <- which(sapply(speech,function(x) x$id=="h104"))
  # str(speech[[ind]])
   ind <- which(sapply(speech,function(x) x$kind %in% "Cancel"))
   res <- data.frame(sp=rep("",length(ind)),title="",subj=0,absId="",spId="")
   if (length(ind)) {
      ind3 <- match("Cancel",locale$section)
      if (length(ind3))
         title <- locale[ind3,paste0("name_",md_lang)]
      else
         title <- switch(md_lang,ru="без выступления","Without presentations")
      if ((isRmd)&&(!isDOC))
         cat(paste0("\n\n## ",title,"{- #canceled_",md_lang,"}\n\n"))
      for (i in sample(seq_along(ind))) {
         j <- ind[i]
         res$sp[i]=speech[[j]][[md_lang]]$speaker
        # if (j==103)
        #    str(speech[[j]])
         res$title[i]=speech[[j]][[md_lang]]$title
         res$subj[i]=speech[[j]]$subject
         res$absId[i]=speech[[j]]$id
         res$spId[i]=speech[[j]]$spId
         ind2 <- match(paste0("Oral",res$subj[i]),locale$section)
         if (length(ind2))
            res$subj[i] <- locale[ind2,paste0("name_",md_lang)]
         res$title[i] <- paste0("[",res$title[i],"](#",res$absId[i],"_",md_lang,")")
         res$subj[i] <- gsub("\\s+","&nbsp;",res$subj[i])
         res$subj[i] <- paste0("[",res$subj[i],"](#subjoral_",md_lang,")")
         if (j==103)
            str(res[i,])
      }
      if (!isRmd)
         res$title <- substr(res$title,1,38)
     # if (!isRmd)
     #    print(res)
      resTab <- subset(res,select=c(sp,title,subj))
      resTab <- resTab[stringi::stri_order(resTab$sp),]
      col1 <- switch(md_lang,ru="Докладчик","Speaker")
      col2 <- switch(md_lang,ru="Название","Title")
      col3 <- switch(md_lang,ru="Направление","Subject")
      if (TRUE) {
         col1 <- paste0("[",col1,"](#speakers_",md_lang,")")
      }
      colnames(resTab) <- c(col1,col2,col3)
      if (isDOC)
         colnames(resTab) <- rep("",ncol(resTab))
      a8 <- knitr::kable(resTab,format="pandoc",row.names=FALSE)
      if ((isRmd)&&(!isDOC))
         print(a8)
     # else
     #    str(resTab)
   }
   if (!isRmd)
      message("Ok")
   0L
}
invisible(main())
