### Claus Wilke twitter 3dPCA
# https://twitter.com/ClausWilke/status/979082026002407424
library(plot3D)
library(cowplot)
p <- ggplot(cyls, aes(cyl, cyl, color=cyl))+geom_point()+
scale_color_manual(values = colors, name="cylinders")+
theme_hc(base_size = 12)+
theme(legend.position = "top", legend.justification = "center")
legend <- get_legend(p)
pfun <- function(theta=30, phi=20){
  function(){
    par(bg = "transparent",
    mai = c(0, 0.1, 0, 0))
    scatter3D(mtcars$disp, mtcars$hp, mtcars$mpg, colvar = mtcars$cyl,
              col=colors,
              pch=19, bty = "b2", theta = theta, phi = phi, colkey = FALSE,
              xlab="displacement (cu.in)", ylab="power (hp)", zlab="efficiency (mpg)",
              cex.lab=.8)
  }
}
plot_grid(pfun(30,20), pfun(-30,20),
          legend, legend,
          pfun(30,40), pfun(-30,40),
          rel_heights = c(1,0.1,1), ncol = 2,
          labels = c("a", "b", "", "", "c", "d"),
          label_fontface = "plain"          )
