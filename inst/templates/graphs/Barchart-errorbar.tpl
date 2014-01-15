<!--head
meta:
  title: Barchart-errorbar
  description: A barchart with errorbars, and possibility to use two grouping variables.
  author: Balázs Sághy (@sabalazs)
  packages:
  - doBy
  - ggplot2
  - stringr
inputs:
- required: yes
  class: numeric
  name: var1
  label: Bars represent
  standalone: no
  value: ~
  length:
    min: 1.0
    max: 1.0
  description: The bars will represent the values of this variable
- required: yes
  class: factor
  name: var2
  label: First grouping variable
  standalone: no
  value: ~
  length:
    min: 1.0
    max: 1.0
  description: This is the first factor variable to group by.
- required: no
  class: factor
  name: var3
  label: Second grouping variable
  standalone: no
  value: ~
  length:
    min: 1.0
    max: 1.0
  description: This is the second factor variable to group by. (Optional)
- name: rotate.labels
  label: Rotate axes' labels
  description: Specifying if the axes' labels should be rotated, to avoid overlapping.
  class: logical
  value: FALSE
  required: no
  standalone: yes
head-->
<%=

## Most of the code was copy-pasted from a related article on cookbook

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
##   summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {


if (!is.null(var3)) { data=data.frame(var1=var1, var2=var2, var3=var3)
} else { data=data.frame(var1=var1, var2=var2) }
measurevar="var1"
if (!is.null(var3)) { groupvars=c("var2","var3")
} else { groupvars="var2" }
na.rm=FALSE
conf.interval=.95
    # New version of length which can handle NA's: if na.rm==T, don't count them
  data <- na.omit(data)
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) { sum(!is.na(x)) 
        } else { length(x) }
    }

    # Collapse the data
    if (!is.null(var3)) { formula <- as.formula(paste("var1 ~ var2 + var3"))
	} else { formula <- as.formula(paste("var1 ~ var2")) }
    datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)

    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

dfc <- datac

## Error bars represent standard error of the mean
  
if (rotate.labels != FALSE){
  panderOptions('graph.axis.angle', 2)
}

if ((evalsOptions('width')!=480 | evalsOptions('height'))!=480) {
  x <- (480 - min(evalsOptions('width'), evalsOptions('height')))/480
  panderOptions('graph.fontsize',panderOptions('graph.fontsize')-x*panderOptions('graph.fontsize'))
}

 if (!is.null(var3)) { 
plottolj <- ggplot(dfc, aes(x=var2, y=var1, fill=var3)) + xlab(str_wrap(rp.label(var2), width=60)) + ylab(str_wrap(rp.label(var1), width=60)) + labs(fill=str_wrap(rp.label(var3), width=15)) 
plottolj <- plottolj + geom_bar(position=position_dodge(), stat='identity')
plottolj <- plottolj + geom_errorbar(aes(ymin=var1-se, ymax=var1+se), width=.2, position=position_dodge(.9))
} else {
plottolj <- ggplot(dfc, aes(x=var2, y=var1)) + xlab(str_wrap(rp.label(var2), width=60)) + ylab(str_wrap(rp.label(var1), width=60))
plottolj <- plottolj + geom_bar(position=position_dodge(), stat='identity')
plottolj <- plottolj + geom_errorbar(aes(ymin=var1-se, ymax=var1+se), width=.2, position=position_dodge(.9))
}

plottolj

%>