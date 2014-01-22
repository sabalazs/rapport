<!--head
meta:
  title: Graphing (Mosaic Chart)
  author: Gergely Daróczi, Dániel Nagy
  description: In this template we will present you a Mosaic Chart.
  email: ~
  packages:
  - ggplot2
  example:
  - rapport('MosaicChart.tpl', data=ius2008, x = 'game', y = "gender")
  - rapport('MosaicChart.tpl', data=ius2008, x = 'game', y = "net.required")
inputs:
- name: x
  label: Factor Variable on the X axis
  description: This is the variable that you will be able to see presented on the X axis
  class: factor
  length:
    min: 1.0
    max: 1.0
  required: yes
  standalone: no
- name: y
  label: Factor Variable on the Y axis
  description: This is the variable that you will be able to see presented on the Y axis
  class: factor
  length:
    min: 1.0
    max: 1.0
  required: no
  standalone: no
head-->

<%=
panderOptions('graph.legend.position', 'top')
%>

## Adjusting font size, if the user has different default plotsize/fontsize settings
  
if (exists('fontsize') && !is.null(fontsize)) {panderOptions('graph.fontsize',fontsize)
  } else {
    fsmultip <- 0
    fsmultip <- (480 - min(evalsOptions('width'), evalsOptions('height')))/480
    panderOptions('graph.fontsize',panderOptions('graph.fontsize')-fsmultip*panderOptions('graph.fontsize'))
  }

<%=
table <- table(rp.data[,x.name], rp.data[,y.name], deparse.level = 0, useNA = 'no')

set.caption('Mosaic chart')
t        <- melt(table)
t$x      <- rowSums(table) / sum(table) * 100
t$xmax   <- cumsum(rowSums(table)) / sum(table) * 100
t$xmin   <- t$xmax - t$x
t$y      <- t$value / rep(rowSums(table), ncol(table)) * 100
t        <- t[with(t, order(Var.1)), ]
t$ymax   <- cumsum(t$y) - as.vector(sapply(1:nrow(table) - 1, rep, ncol(table))) * 100
t$ymin   <- t$ymax - t$y
t$xxtext <- with(t, xmin + (xmax - xmin) / 2)
t$xytext <- as.vector(sapply(rep(c(103, -3), length.out = nrow(table)), rep, ncol(table)))
ggplot(t, aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = Var.2)) + geom_rect() + geom_rect(colour = 'white', show_guide = FALSE) + geom_text(aes(x = xxtext, y = xytext, label = Var.1), size = 4) + xlab('') + ylab('') + theme(legend.position = 'top') + labs(fill = '')
%>
