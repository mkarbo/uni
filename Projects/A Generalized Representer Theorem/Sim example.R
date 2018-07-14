library(ggplot2)
library(e1071)
library(reshape)
library(grid)
library(ggthemes)
library(gridExtra)
#----
theme_Publication <- function(base_size=14, base_family="helvetica") {

  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size= unit(1, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
} 
#----

#--- data simulated
f <- function(x){
  (2*(x^4)-6*x^3-6*x^2+25*x)*0.1
}
x = rnorm(250,0,1)
noise = rnorm(250,0,1)
simdata <- data.frame(x=x,y=f(x)+noise)


#---- plotted noised non-linear data
g1 <- ggplot(data = simdata, aes(x,y))
g1+geom_point()



#---- fit model and tune it
#pol
svr.pol.mod <- svm(x~y, simdata, kernel ="polynomial")
svr.pol.tune <- tune(svm, y ~ x,  data = simdata,
                   kernel = "polynomial",
                   ranges = list(epsilon = seq(0,1,0.1), 
                                 cost = 2^(2:5))
                  )
svr.pol.tune.mod <- svr.pol.tune$best.model
svr.pol.tune.pred<- predict(svr.pol.tune.mod,
                            data = simdata) 
svr.pol.pred <- predict(svr.pol.mod, data = simdata)


#RBF
svr.rbf.mod <- svm(x~y, simdata, kernel = "radial")
svr.rbf.tune<- tune(svm, y ~ x,  data = simdata,
                kernel = "radial",
                ranges = list(epsilon = seq(0,1,0.1), 
                              cost = 2^(2:5))
                )
svr.rbf.tune.mod <- svr.rbf.tune$best.model
svr.rbf.tune.pred<- predict(svr.rbf.tune.mod,
                           data = simdata) 
svr.rbf.pred <- predict(svr.rbf.mod, data = simdata)

#Linear
svr.linear.tune <- tune(svm, y~x, data = simdata,
                        kernel = "linear",
                        ranges = list(epsilon = seq(0,0.3,0.1), 
                                      cost = 2^(2:5))
                        )
svr.linear.tune.mod <- svr.linear.tune$best.model
svr.linear.tune.pred <- predict(svr.linear.tune.mod, data = simdata)

g1+geom_point(size=1)+
  geom_line(aes(y=f(x), colour = "true fit"),linetype = "dashed",size=1)+
  geom_line(aes(y=svr.linear.tune.pred, colour = "linear svr curve"),size=1)+
  geom_line(aes(y= svr.pol.tune.pred,colour="polynomial svr curve"),size=1)+
  geom_line(aes(y= svr.rbf.tune.pred,colour=" rbf svr curve"),size=1)+
  scale_colour_Publication()+
  theme_Publication()

#----
datmelt = melt(data.frame(x=x, 
                          original = f(x)+noise,
                          svr = svrpred,
                          lin = linpred),
               id.vars = "x")
#----

ggplot(datmelt, aes(x, value, colour = variable)) +
  geom_point(size=0.6) +
  geom_smooth(aes(colour = variable))+
  scale_colour_Publication()+
  theme_Publication()


