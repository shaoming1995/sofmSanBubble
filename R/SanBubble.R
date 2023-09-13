#' @title 桑基气泡图绘图工具
#' @param Bubble 气泡图数据框
#' @param Sankey 桑基图数据库
#' @param watermark YES是有水印,NO是无水印
#' @export
SanBubble<-function(Bubble,Sankey,watermark){
  Bubble$pathname <- factor(Bubble$pathname,levels = rev(Bubble$pathname))
  #基础富集气泡图：
  if(!require(ggsankey))devtools::install_github("davidsjoberg/ggsankey")
  if(!require(cols4all))install.packages('cols4all')
  if(!require(ggplot2))install.packages('ggplot2')
  if(!require(tidyverse))install.packages('tidyverse')
  if(!require(cowplot))install.packages('cowplot')
  if(!require(shinyjs))install.packages("shinyjs")
  if(!require(kableExtra))install.packages("kableExtra")
  if(!require(colorblindcheck))install.packages("colorblindcheck")
  library(tidyverse)
  library(ggsankey)
  library(ggplot2)
  library(cols4all)
  library(cowplot)
  Bubble <- Bubble %>%
    mutate(ymax = cumsum(count)) %>% #ymax为Width列的累加和
    mutate(ymin = ymax -count) %>%
    mutate(label = (ymin + ymax)/2) #取xmin和xmax的中心位置作为标签位置
  head(Bubble)
  #自定义主题与配色修改：
  mytheme <- theme(axis.title = element_text(size = 13),
                   axis.text = element_text(size = 11),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   legend.title = element_text(size = 13),
                   legend.text = element_text(size = 11))
  p1 <- ggplot() +
    geom_point(data = Bubble,
               aes(x = -log10(Pvalue),
                   y = label,
                   size = count,
                   color = HR)) +
    scale_colour_distiller(palette = "Reds", direction = 1) + #更改配色
    labs(x = "-log10(Pvalue)",
         y = "") +
    theme_bw() +
    mytheme
  #将数据转换为绘图所需格式：
  df <- Sankey %>%
    make_long( name, pathname)
  head(df)

  #指定绘图顺序（转换为因子）：
  df$node <- factor(df$node,levels = c(Sankey$pathname %>% unique()%>% rev(),
                                       Sankey$name %>% unique() %>% rev()))
  #自定义配色：
  c4a_gui()
  number<-Sankey[!duplicated(Sankey$name),]
  A<-dim(number)[[1]]
  B<-dim(Bubble)[[1]]
  C<-A+B
  mycol <- c4a('rainbow_wh_rd',C)
  #绘图：
  p2 <- ggplot(df, aes(x = x,
                       next_x = next_x,
                       node = node,
                       next_node = next_node,
                       fill = node,
                       label = node)) +
    geom_sankey(flow.alpha = 0.5,
                flow.fill = 'grey',
                flow.color = 'grey80', #条带描边色
                node.fill = mycol, #节点填充色
                smooth = 8,
                width = 0.08) +
    geom_sankey_text(size = 3.2,
                     color = "black")+
    theme_void() +
    theme(legend.position = 'none')
  p3 <- p2+ theme(plot.margin = unit(c(0,4,0,0),units="cm"))
  #拼图(在p5的空白位置中插入p3)：
  if(watermark=="YES"){
    ggdraw() + draw_plot(p3) + draw_plot(p1, scale = 0.5, x = 0.55, y=-0.21, width=0.48, height=1.37)+
      draw_label("桑基气泡图-医小研(抖音)", colour = "#80404080", size = 30, angle = 45)

    }else{
        ggdraw() + draw_plot(p3) + draw_plot(p1, scale = 0.5, x = 0.55, y=-0.21, width=0.48, height=1.37)

        }

}


