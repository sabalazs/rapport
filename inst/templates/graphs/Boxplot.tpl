<!--head
meta:
  title: Boxplot
  description: ''
  author: Daniel Nagy, Balázs Sághy (@sabalazs)
  packages: RColorBrewer
inputs:
- required: yes
  class: numeric
  name: var1
  label: Used Variable 1
  standalone: no
  length:
    min: 1.0
    max: 1.0
  description: This is the first variable that you will use here
- required: no
  class: factor
  name: var2
  label: Used Variable 2
  standalone: no
  length:
    min: 1.0
    max: 1.0
  description: This is the factor variable that you will use here
- required: no
  class: character
  name: plot.title
  label: Title of the plot
  standalone: yes
  value: default
  length:
    min: 1.0
    max: 1.0
  description: This is good to set the title of the plot.
- name: plot.title.pos
  label: Position of the title of the plot
  description: Specifying the position of the title of the plot
  class: character
  options:
  - on the plot
  - outside the plot
  - nowhere
  value: on the plot
  matchable: yes
  allow_multiple: no
  required: no
  standalone: yes
- required: no
  class: character
  name: fontcolor
  label: Color of the font
  standalone: yes
  value: black
  length:
    min: 1.0
    max: 1.0
  description: Specifying the default font color
- required: no
  class: integer
  name: fontsize
  label: Size of the font
  standalone: yes
  value: 12.0
  length:
    min: 1.0
    max: 1.0
  description: Specifying the base font size in pixels
  limit:
    min: 1.0
    max: 50.0
- required: no
  class: character
  name: background
  label: Background's color
  standalone: yes
  value: white
  length:
    min: 1.0
    max: 1.0
  description: Specifying the plots main background's color
- required: no
  class: character
  name: colp
  label: Color palette
  standalone: yes
  value: Set1
  length:
    min: 1.0
    max: 1.0
  description: Color paletter from colorbrewer.com
  matchable: yes
  options:
  - BrBG
  - PiYG
  - PRGn
  - PuOr
  - RdBu
  - RdGy
  - RdYlBu
  - RdYlGn
  - Spectral
  - Accent
  - Dark2
  - Paired
  - Pastel1
  - Pastel2
  - Set1
  - Set2
  - Set3
  - Blues
  - BuGn
  - BuPu
  - GnBu
  - Greens
  - Greys
  - Oranges
  - OrRd
  - PuBu
  - PuBuGn
  - PuRd
  - Purples
  - RdPu
  - Reds
  - YlGn
  - YlGnBu
  - YlOrBr
  - YlOrRd
  allow_multiple: no
- required: no
  class: logical
  name: color.rnd
  label: Reordered colors
  standalone: yes
  value: no
  length:
    min: 1.0
    max: 1.0
  description: Specifying if the palette should be reordered randomly before rendering
    each plot to get colorful images
- required: no
  class: integer
  name: axis.angle
  label: Angle of the axes
  standalone: yes
  value: 1.0
  length:
    min: 1.0
    max: 1.0
  description: Specifying the angle of axes' labels
  limit:
    min: 1.0
    max: 4.0
head-->

<%=

if (length(var2) == 0) {
var1 <- na.omit(var1)
} else {
var1 <- na.omit(var1)
var2 <- na.omit(var2)
}

if (fontcolor != "black") panderOptions('graph.fontcolor', fontcolor)
if (fontsize != 12) panderOptions('graph.fontsize', fontsize)
if (background != "white") panderOptions('graph.background', background)
if (color.rnd != FALSE) panderOptions('graph.color.rnd', color.rnd)
if (axis.angle != 1) panderOptions('graph.axis.angle', axis.angle)
cs <- brewer.pal(brewer.pal.info[which(rownames(brewer.pal.info) == colp),1], colp)
if (colp != "Set1") panderOptions('graph.colors', cs)

## Adjusting font size, if the user has different default plotsize/fontsize settings
  
if (exists('fontsize') && !is.null(fontsize)) {panderOptions('graph.fontsize',fontsize)
  } else {
    fsmultip <- 0
    fsmultip <- (480 - min(evalsOptions('width'), evalsOptions('height')))/480
    panderOptions('graph.fontsize',panderOptions('graph.fontsize')-fsmultip*panderOptions('graph.fontsize'))
  }

## Calculating ideal line length for the labels

line_length <- 60
fgsize_ratio <- panderOptions('graph.fontsize')/min(evalsOptions('width'), evalsOptions('height'))
if (fgsize_ratio > 12/480) {
  line_length <- floor (60*((12/480)/fgsize_ratio))
} 

if (length(var2) == 0) {
if (plot.title == "default")  {
main_lab <- sprintf('Boxplot of %s',var1.name)
} else {
main_lab <- plot.title
}
} else {
if (plot.title == "default") { 
main_lab <- sprintf('Boxplot of %s and %s',var1.name, var2.name)
} else {
main_lab <- plot.title
}}

if (length(var2) == 0) {
set.caption(main_lab)
suppressWarnings(bwplot(var1, main = main_lab, xlab = str_wrap(var1.label, width=line_length)))
} else {
set.caption(main_lab)
suppressWarnings(bwplot(var1 ~ var2, main = main_lab, xlab = str_wrap(var2.label, width=line_length), ylab = str_wrap(var1.label, width=line_length)))
}
%>