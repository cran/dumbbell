#' @include global.R
NULL
#' Dumbbell Plot
#' 
#' 
#' @author Foo Cheung, \email{foocheung@@yahoo.com}
#' @keywords dumbbell
#' @import dplyr 
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#' @import rlang
#' @import utils
#' @import rstatix
#' 
#' @description Draws a Dumbbell Plot, essentially a dot plot with two series of data.
#' @param xdf data a data frame, \code{xdf= data frame}
#'   A data frame containing at least four columns
#'   corresponding, respectively, to (1) the first variable containing the "id",
#'   (2) the second variable containing the "key" , (3) the third variable containing the start of the point "column1", the first data series,
#'   (4) the fourth variable containing the end of the point "column2", the second data series
#' 
#' @param id is the name of the column containing the id variable which will label the y axis eg(subject1,subject2 etc) eg \code{id = "id"}
#' @param key is the name of the column containing the key variable telling us which measure we use in each row eg \code{key = "key"}
#' @param column1,column2 first and second series of data eg \code{column1 = "Control"} \code{column2 = "Test"}
#' @param lab1,lab2 labels for data series eg \code{lab1 = "Test"} \code{lab2 = "Control"}
#' @param pt_val Add option to show the point values eg \code{pt_val = 1}
#' @param textsize numeric value specifying the text size eg \code{textsize = 3}
#' @param segsize numeric value specifying the segment width eg \code{segsize = 1}
#' @param delt Add a delta column to the plot eg \code{delt = 1}
#' @param p_col1,p_col2 colors for start and end points eg \code{pcol1 = "red"}
#' @param col_lab1,col_lab2 color text below each dumbell eg \code{col_lab1 = "red"}
#' @param expandx Add space to the both ends of the x axis eg \code{expandx = 0.6}
#' @param expandy Add space to the both ends of the y axis eg \code{expandy = 1}
#' @param leg Add legend title \code{legend = "legend title"}
#' @param pt_alpha Add transparentcy to points \code{pt_alpha = 0.6}
#' @param arrow_size Add size to arrows \code{arrow_size = 0.2}
#' @param arrow Adds an arrow to one end of the dumbbell eg \code{arrow = 1}
#' @param col_seg1,col_seg2 Adds a color to each arrow in each direction eg \code{col_seg1 = "red"}
#' @param pval Adds pvalue to the facet label, from using a wilcox paired test eg \code{pval = 1} or a paired t_test eg \code{pval = 2} (requires to use facet_wrap).
#' @param pointsize Adds pointsize to the points eg \code{pointsize = 3}
#' @param title Adds title to the plot eg \code{title = "This is a plot title"}
#' @return Dumbbell plot
#' @export
#' @examples
#' library(tidyverse)
#' library(ggplot2) 
#' library(rlang)
#' library(utils)
#' library(data.table)
#' library(dumbbell)
#' ## create data
#' z<-data.frame(Group = c(rep("A",20),rep("B",20)),
#'               #  Subject = c(paste("sub_",1:20,sep=""),paste("sub_",1:20,sep="")),
#'               Subject = c(paste(1:20,sep=""),paste(1:20,sep="")),
#'               result = c(sample(1:100000, 40, replace=TRUE)),
#'               analysis = c(rep("a",10),rep("b",10) ,rep("b",10),rep("a",10) )
#'               
#' )
#' 
#' b<-z %>% filter(Group == 'A')
#' c<-z %>% filter(Group == 'B')
#' 
#' d<-merge(b,c, by.x="Subject", by.y = "Subject")
#' 
#' 
#' e<-d %>% mutate("diff"=result.x-result.y) %>% arrange(diff)
#' 
#' 
#' 
#' 
#' 
#' d$Subject<-factor(d$Subject, levels = e$Subject)
#' 
#'
#' ## Basic plot
#' dumbbell(xdf=d,id= "Subject",key="analysis.x",column1 = "result.x",column2 = "result.y") 









dumbbell<-function(xdf,id,key, column1,column2,lab1,lab2,title,pointsize, textsize,segsize,expandx,expandy, p_col1,p_col2,leg,col_seg1,
                   col_seg2,col_lab1,col_lab2, pt_alpha,arrow_size, arrow, pt_val,delt,pval) {
  
  
  if(missing(pval)) {
    pval=0
  } 
  
  if(missing(delt)) {
    delt=0
  } 
  
  
  if(missing(pt_val)) {
    pt_val=0
  } 
  
  if(missing(arrow)) {
    arrow=0
  } 
  
  if(missing(arrow_size)) {
    arrow_size=0.2
  } 
  
  if(missing(pointsize)) {
    pointsize=1
  } 
  if(missing(textsize)) {
    textsize=2
  } 
  if(missing(segsize)) {
    segsize=1
  } 
  if(missing(expandx)) {
    expandx=0.05
  } 
  if(missing(expandy)) {
    expandy=1
  }
  
  if(missing(p_col1)) {
    p_col1="red"
  } 
  if(missing(p_col2)) {
    p_col2="blue"
  }
  if(missing(leg)) {
    leg=""
  }
  if(missing(col_seg1)) {
    col_seg1="gray80"
  }
  if(missing(col_seg2)) {
    col_seg2="gray80"
  }
  if(missing(col_lab1)) {
    col_lab1="black"
  }
  
  if(missing(col_lab2)) {
    col_lab2="black"
  }
  
  if(missing(lab1)) {
    lab1="Addlab1"
  }
  if(missing(lab2)) {
    lab2="Addlab2"
  }
  
  if(missing(title)) {
    title=""
  }
  
  if(missing(pt_alpha)) {
    pt_alpha=1
  }
  
  
  xdf<-data.table::data.table(xdf)
  
  
  
  xdf<-xdf %>% dplyr::select(all_of(id), all_of(key),all_of(column1),all_of(column2))  %>%
    dplyr::rename(!!lab1 := all_of(column1)) %>% 
    dplyr::rename(!!lab2 := all_of(column2)) %>% 
    dplyr::mutate("ori" = ifelse(!!sym(lab1) > !!sym(lab2), "+","-")) %>%
    dplyr::mutate_at(vars(all_of(id)), factor)
  
  datsts<-xdf %>%  dplyr::select(!!key,!!id, all_of(lab1),all_of(lab2))
  
  
  
  
  
  if( pval ==1){
    
    stat.test<-datsts %>% gather(key="key10",value="value", -c(1,2)) %>% group_by(!!sym(key)) %>% rstatix::wilcox_test(value  ~ key10, paired = TRUE)
    
    xstat<-merge(xdf,stat.test,by.x=all_of(key),by.y=all_of(key)) 
    
    xdf <-  xstat %>% unite(!!key,all_of(key),p, sep="\npval:") %>% select(c(1:5))
  }
  else if( pval ==2){
      
      stat.test<-datsts %>% gather(key="key10",value="value", -c(1,2)) %>% group_by(!!sym(key)) %>% rstatix::t_test(value  ~ key10, paired = TRUE)
      
      xstat<-merge(xdf,stat.test,by.x=all_of(key),by.y=all_of(key)) 
      
      xdf <-  xstat %>% unite(!!key,all_of(key),p, sep="\npval:") %>% select(c(1:5))
    }
    {
    
    
  }
  
  
  
  xdf2<- xdf %>% mutate("diff"=round(!!sym(lab1) - !!sym(lab2),2)) %>% arrange(diff)
  
  
  aa<- ggplot() +
    geom_segment(
      data = gather(xdf, measure, val, -c(!!sym(key),!!sym(id),ori)) %>%
        group_by(!!sym(id)) %>%
        top_n(-1) %>%
        slice(1) %>%
        ungroup(),
      aes(x = 0, xend = as.numeric(val), y =!!sym(id) , yend =!!sym(id)),
      linetype = "dotted", size = 0.5, color = "gray80"
      
      
    ) 
  
  
  
  
  if(arrow == 0){
    bb <- aa + geom_segment(
      data = xdf %>% filter(ori == '-') %>% gather( measure, val, -c(!!sym(key),!!sym(id),ori)) %>%
        dplyr::group_by(!!sym(id),!!sym(key)) %>%
        dplyr::summarise(start = range(val)[1], end = range(val)[2] ,.groups = 'drop') %>%
        ungroup(),
      aes(x= as.numeric(start), xend = as.numeric(end) , y =!!sym(id), yend = !!sym(id)),
      color = col_seg2, size = segsize) +
      geom_segment(
        data = xdf %>% filter(ori == '+') %>% gather( measure, val, -c(!!sym(key),!!sym(id),ori)) %>%
          dplyr::group_by(!!sym(id),!!sym(key)) %>%
          dplyr::summarise(start = range(val)[1], end = range(val)[2] ,.groups = 'drop') %>%
          ungroup(),
        aes(x= as.numeric(end), xend = as.numeric(start) , y =!!sym(id), yend = !!sym(id)),
        color = col_seg1, size = segsize )
    
    
    
    
  }
  else{
    bb<-  aa + geom_segment(
      data = xdf %>% filter(ori == '-') %>% gather( measure, val, -c(!!sym(key),!!sym(id),ori)) %>%
        dplyr::group_by(!!sym(id),!!sym(key)) %>%
        dplyr::summarise(start = range(val)[1], end = range(val)[2] ,.groups = 'drop') %>%
        ungroup(),
      aes(x= as.numeric(start), xend = as.numeric(end) , y =!!sym(id), yend = !!sym(id)),
      color = col_seg2, size = segsize    , arrow = arrow(length = unit(arrow_size, "cm"),type = "closed")
      
    ) + 
      geom_segment(
        data = xdf %>% filter(ori == '+') %>% gather( measure, val, -c(!!sym(key),!!sym(id),ori)) %>%
          dplyr::group_by(!!sym(id),!!sym(key)) %>%
          dplyr::summarise(start = range(val)[1], end = range(val)[2] ,.groups = 'drop') %>%
          ungroup(),
        aes(x= as.numeric(end), xend = as.numeric(start) , y =!!sym(id), yend = !!sym(id)),
        color = col_seg1, size = segsize    , arrow = arrow(length = unit(arrow_size, "cm"),type = "closed")
      )  
    
  }
  
  
  
  
  
  if (pt_val ==1 ){
    cc<- bb+  geom_text(data=gather(xdf %>%  dplyr::select(-!!lab1), measure, val, -c(!!sym(key),!!sym(id),ori )), aes(x=val, y=!!sym(id), label=val),
                        size=textsize, vjust=2.5  , color=col_lab1) +
      
      geom_text(data=gather(xdf %>%  dplyr::select(-!!lab2), measure, val, -c(!!sym(key),!!sym(id),ori )), aes(x=val, y=!!sym(id), label=val),
                size=textsize, vjust=2.5 , color=col_lab2) 
    
    
    
    
  }
  else{
    
    cc<-bb
  }
  
  
  
  if (delt == 1){
    
    
    
    
    dd<- cc + geom_text(data=xdf %>%  dplyr::select(!!key,!!id, all_of(lab1),all_of(lab2)) %>% mutate("diff"=round(!!sym(lab1) - !!sym(lab2),2)),
                        aes(label=diff, y=!!sym(id), x=Inf ), fontface="bold", size=textsize, hjust= 1.1)   + 
      annotate("text",x=Inf, y = Inf, label="DIFF",color="red", size=textsize, fontface="bold", vjust=1.1, hjust= 1.1) 
    
  }
  else{
    dd<- cc 
  }
  
  dd + 
    geom_point(
      data = gather(xdf, measure, value, -c(!!sym(key),!!sym(id),ori)),
      aes(value, !!sym(id), color = measure),
      size = pointsize,alpha = pt_alpha
    )+
    
    
    scale_colour_manual(values = c(p_col1, p_col2)) +
    guides(col=guide_legend(leg)) +
    
    labs(
      x = "", y = "",
      title = title
    ) +
    
    
    theme_light()   + 
    
    theme(legend.position = "top")  +
    scale_x_continuous(expand = expansion(mult = expandx))+
    scale_y_discrete(expand = expansion(add= 1)) 
}