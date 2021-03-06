## ---- echo=FALSE--------------------------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

## ----fig.width=7,fig.height=3  ,cache=FALSE, message = FALSE, warning = FALSE----

## Load libraries
suppressPackageStartupMessages({
library(tidyverse)
library(ggplot2) 
library(rlang)
library(utils)
library(data.table)
library(dumbbell)

     
})



## Generate Some Random Data

z<-data.frame(Group = c(rep("A",10),rep("B",10)),
              Subject = c(paste("sub_",1:10,sep=""),paste("sub_",1:10,sep="")),
              result = c(sample(1:100000, 20, replace=T)),
              analysis = c(rep("a",5),rep("b",5) ,rep("a",5),rep("b",5) )
              
)

## Create 2 groups "A", "B"

b<-z %>% filter(Group == "A")
c<-z %>% filter(Group == "B")

b$Subject<-as.factor(b$Subject)
c$Subject<-as.factor(c$Subject)

d<-merge(b,c, by.x=c("Subject", "analysis"), by.y = c("Subject","analysis"))


## Order by delta
e<-d %>% mutate("diff"=pmin(result.x-result.y)) %>% arrange(diff)
d$Subject<-factor(d$Subject,  e$Subject)


##Create a basic dumbell Plot
dumbbell(xdf=d,id = "Subject",key="analysis",column1 = "result.x",column2 = "result.y",
         lab1 = "labelA",lab2 = "labelB") 



## ----fig.width=7,fig.height=4  ,cache=FALSE,message = FALSE, warning = FALSE----

##Adding a delta column 
dumbbell(xdf=d,id = "Subject",key="analysis",column1 = "result.x",column2 = "result.y",
         lab1 = "labelA",lab2 = "labelB",delt=1,expandx = 0.1) 

## ----fig.width=7,fig.height=4  ,cache=FALSE,message = FALSE, warning = FALSE----

##Adding values as labels
dumbbell(xdf=d,id = "Subject",key="analysis",column1 = "result.x",column2 = "result.y",
         lab1 = "labelA",lab2 = "labelB", pt_val = 1, expandx = 0.05, col_lab1 = "blue", col_lab2 = "red") 

## ----fig.width=7,fig.height=4  ,cache=FALSE,message = FALSE, warning = FALSE----


##Adding arrows
dumbbell(xdf=d,id = "Subject",key="analysis",column1 = "result.x",column2 = "result.y",
         lab1 = "labelA",lab2 = "labelB", expandx = 0.01, arrow = 1, pt_alpha = 0.6, arrow_size = 0.2, 
          segsize = 0.7, pointsize = 1.5, col_seg1 = "#A9A9A9", col_seg2 = "#A9A9A9")

## ----fig.width=7,fig.height=4  ,cache=FALSE,message = FALSE, warning = FALSE----


##Adding facets
dumbbell(xdf=d,id = "Subject",key="analysis",column1 = "result.x",column2 = "result.y",lab1 = "labelA",lab2 = "labelB")  + 
   facet_wrap(analysis ~., ncol=1,scales = "free") 

## ----fig.width=7,fig.height=4  ,cache=FALSE,message = FALSE, warning = FALSE----


##Adding facets
dumbbell(xdf=d,id = "Subject",key="analysis",column1 = "result.x",column2 = "result.y",lab1 = "labelA",lab2 = "labelB", pval=1)  + 
   facet_wrap(analysis ~., ncol=1,scales = "free") 

## ----fig.width=7,fig.height=4  ,cache=FALSE,message = FALSE, warning = FALSE----


##Adding facets, highlight one direction, add arrows and the delta value
dumbbell(xdf=d,id = "Subject",key="analysis",column1 = "result.x",column2 = "result.y",lab1 = "labelA",lab2 = "labelB",
   delt = 1,col_seg2 = "red", col_seg1 = "blue",arrow = 1 ,pt_alpha = 0.6, pointsize = 2 ,expandx = 0.2  ,segsize = 0.5,textsize = 2,pval=1
 ) + 
   facet_wrap(analysis ~., ncol=1,scales = "free") 

