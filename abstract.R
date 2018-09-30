invisible({
   isRmd <- !inherits(output <- try(rmarkdown::all_output_formats(
                                          knitr::current_input())),"try-error")
   pdfpath <- c("./pdf","http://sevin.000webhostapp.com/wildlifeArctic/pdf")[2]
   if (isRmd) {
     # output <- rmarkdown::all_output_formats(knitr::current_input())[1]
      isPDF <- length(grep("pdf",output[[1]]))>0
      isHTML <- length(grep("html|github",output[[1]]))>0
      isBOOK <- length(grep("book",output[[1]]))>0
   }
   else {
      isHTML <- TRUE
      isBOOK <- FALSE
      isPDF <- FALSE
   }
   if (is.null(md_lang <- getOption("md_lang")))
      md_lang <- "ru"
   if (isHTML) {
      s <- "\n\n
      <style>
      .plhidden h3
      {
         color: red;
         font-size: 0%
      }
      </style>\n\n"
     # if (isRmd)
     #    cat(s)
   }
   if (isBOOK) {
      pdfResize <- "\n\n<script type=\"text/javascript\">
      function autoResize(iframe) {
          $(iframe).height($(iframe).contents().find('html').height());
      }
      </script>\n\n"
   }
   if (isRmd)
      cat(paste0("\n\n[",switch(md_lang,ru="Скачать сборник тезисов в PDF"
                                          ,"Download Book of Abstracts (PDF)")
                ,"](",pdfpath,"/proceedings.pdf)\n\n"))
   fullAnno <- FALSE || isHTML
   result <- readRDS("abstract.rds")
   locale <- readRDS("locale.rds")
   id <- paste0("@@@",sapply(result,function(x) x$id))
   speaker <- sapply(result,function(x) x[[md_lang]]$speaker)
   spInd <- stringi::stri_order(speaker)
   when <- readRDS("rasp0.rds")
   when$section1[is.na(when$section1)] <- ""
   when$section2[is.na(when$section2)] <- ""
   po <- grep("poster",paste(when$section1,when$section2),ignore.case=TRUE)
   poname <- c(when$section1[po],when$section2[po])
   poname <- unique(poname[nchar(poname)>0])
   polab <- rep("",length(poname))
   potime <- rep("",length(poname))
   for (i in seq_along(poname)) {
      ind0 <- which(when$section1[po]==poname[i] | when$section2[po]==poname[i])
      polab[i] <- paste0(format(when[head(po[ind0],1),"from"],"#d%j_")
                        ,unique(when[po[ind0],"activity"]),"_",md_lang)
      potime[i] <- paste(format(when[head(po[ind0],1),"from"],"%d %b, %H:%M")
                        ,format(when[tail(po[ind0],1),"to"],"%H:%M"),sep="-")
   }
   potitle <- unique(gsub("\\d","",poname))
   potitle <- locale[match(potitle,locale$section),paste0("name_",md_lang)]
   po <- paste0(potime," (",potitle,")")
   ind <- grep("poster",locale$section,ignore.case=TRUE)
   pos <- locale[ind,paste0("name_",md_lang)]
   txtBody1 <- character()
   txtBody2 <- character()
   posterName <- locale[grep("^poster",locale$section,ignore.case=TRUE)
                   ,paste0("name_",md_lang)]
   for (i in spInd) {
      resw <- result[[i]]
     # if (resw$key!="Соловьев_Б")
     #    next
     # str(resw)
     # next
      res <- resw[[md_lang]]
      if (!length(res$abstract))
         next
      authors <- res$authors
      ind <- match(res$speaker,authors)
      authors[ind] <- paste0("**",authors[ind],"**")
      if (length(res$organization)>1) {
         authors <- paste(paste0(authors,paste0("^",res$affiliation,"^")),collapse=", ")
         org <- paste(paste0(seq_along(res$organization),". "),res$organization,"\n")
      }
      else {
         authors <- paste(authors,collapse=", ")
         org <- paste0("+ ",res$organization,collapse=" ")
      }
      p <- when[when$id==resw$when,,drop=FALSE]
      pa <- p[,c("section1","section2")]
      pa[is.na(pa)] <- "___"
      oral <- grep("oral",pa,ignore.case=TRUE)
      poster <- grep("poster",pa,ignore.case=TRUE)
      if (length(grep("(oral)",resw$kind,ignore.case=TRUE))) {
         section <- locale[match(pa[,oral],locale$section),paste0("name_",md_lang)]
         title <- locale[match(pa[,oral],locale$section),paste0("title_",md_lang)]
         section <- paste0("[",section,"](#subjoral_",md_lang,"): <i>",title,"</i>")
        # anchor <- paste0("#oral",gsub("\\D","",pa[,oral]),"_",md_lang)
         anchor <- paste0(format(p[,"from"],"#d%j_")
                         ,p[,"activity"],"_",md_lang)
         p <- paste0(format(p[,"from"],"%d %b, %H:%M"),"-",format(p[,"to"],"%H:%M"))
      }
      else if (length(grep("poster",resw$kind,ignore.case=TRUE))) {
         subj <- paste0("Oral",resw$subject)
         ind <- grep(resw$subject,gsub("\\D","",poname))
         section <- locale[match(subj,locale$section),paste0("name_",md_lang)]
         title <- locale[match(subj,locale$section),paste0("title_",md_lang)]
         section <- paste0("[",section,"](#subjoral_",md_lang,"): <i>",title,"</i>")
         anchor <- polab[ind]
         p <- po[ind]
      }
      else if (length(grep("(cancel|absent)",resw$kind,ignore.case=TRUE))) {
        # str(resw)
         subj <- paste0("Oral",resw$subject)
         ind <- grep(resw$subject,gsub("\\D","",poname))
         section <- locale[match(subj,locale$section),paste0("name_",md_lang)]
         title <- locale[match(subj,locale$section),paste0("title_",md_lang)]
         section <- paste0("[",section,"](#subjoral_",md_lang,"): <i>",title,"</i>")
         anchor <- paste0("#canceled_",md_lang)
         ind <- grep("cancel",locale$section,ignore.case=TRUE)
         p <- switch(md_lang,locale[ind,paste0("name_",md_lang)])
      }
      txt <- c(""
              ,rep("<BR>",5)
              ,""
             # ,ifelse(isBOOK,pdfResize,"")
             # ,""
              ,paste0("$$$ "
                    # ,"<span style=",dQuote("color: transparent;"),">"
                     ,"<i>",res$speaker,"</i>",": ",res$title
                    # ,"</span>"
                     ," {- #",id[i],"_",md_lang," .plhidden}"
                     )
              ,""
             # ,paste0("<a name=",dQuote(gsub("h","h",id[i])),">","</a>")
             # ,""
             # ,paste0("[",section,"](",anchor,"), ",p)
              ,paste0("[",p,"]","(",anchor,") "
                     ,ifelse(resw$hasPDF,paste0("(","["
                                 ,switch(md_lang,ru="Презентация","Presentation")
                                 ,"](",pdfpath,"/",resw$id,".pdf)",")"),"")
                     ,"<BR>",section)
              ,""
              ,"*** ***"
              ,""
              ,paste0("<span style=\"font-size:140%; font-variant:small-caps;\">"
                     ,res$title,"</span>")
              ,""
              ,paste("<span style=\"font-size:120%;\">",authors,"</span>")
              ,""
              ,org
              ,""
              ,paste(res$abstract,collapse="\n\n")
              ,""
##~ <frameset rows="100%" cols="*">
   ##~ <frame src="Верстка сайту/Шаблон.html" name="topFrame" scrolling="no" noresize>
##~ </frameset>
              ,ifelse(resw$hasPDF,paste0("<iframe"
                                        ," width=",dQuote("768")
                                        ," height=",dQuote("576")
                                       # ," width=",dQuote("100%")
                                       # ," height=",dQuote("100%")
                                        ," src=",dQuote(paste0(pdfpath,"/",resw$id,".pdf"))
                                        ," frameborder=",dQuote(0)
                                       # ," marginheight=",dQuote(0)
                                       # ," onload=",dQuote("autoResize(this);")
                                        ,"></iframe>")
                     ,"")
              ##~ ,ifelse(resw$hasPDF,paste0("<frameset"
                                        ##~ ," rows=",dQuote("100%")
                                        ##~ ," cols=",dQuote("*")
                                        ##~ ," src=",dQuote(paste0(pdfpath,"/",resw$id,".pdf"))
                                        ##~ ," scrolling=",("no")
                                        ##~ ," noresize"
                                        ##~ ,"></frameset>")
                     ##~ ,"")
              ,""
              )
      txt1 <- gsub("\\@\\@\\@\\w","h",gsub("\\$\\$\\$","###",txt))
      txt2 <- gsub("\\@\\@\\@","",gsub("\\$\\$\\$","##",txt)) 
      txtBody1 <- c(txtBody1,txt1)
      txtBody2 <- c(txtBody2,txt2)
   }
   txtBody <- "\n"
   if (fullAnno) {
     # cat("\n# Аннотации {- #abstract}\n\n")
      txtBody <- c(txtBody,paste(txtBody1,collapse="\n"),"\n")
   }
   if (isBOOK)
      txtBody <- c(txtBody,paste(txtBody2,collapse="\n"),"\n")
   if (isRmd) {
      cat(txtBody)
      cat("\n\n")
   }
   if (!isRmd)
      message("Ok")
})
