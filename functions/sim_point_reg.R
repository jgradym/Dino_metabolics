#Simulate points given slope, x value range, Intercept/or intercept in given range print plot (for easy conceptual plot)

sim.point.reg <- function(slope, xlwr, xupr, ylwr, yupr, intercept, add_line){
  x <- runif(100, min = xlwr, max = xupr)

  y <-slope*x + intercept + rnorm(100, mean = 0, sd =0)
if(missing(add_line)){
  
  x_y <- cbind(x,y)
  ggplot(data.frame(x_y), aes(x,y))+
    geom_smooth(method="lm", se=F)+
    theme_classic(base_size = 14)+
    theme(axis.ticks = element_blank())+ 
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.1,
                                     decimal.mark = '.'))
 
}else{

  y2 <-slope*x+intercept + rnorm(100, mean = 0, sd =0)
  y2 <- y2+add_line
   x_y <- cbind(x,y, y2)
  ggplot(data.frame(x_y), aes(x,y))+
    geom_smooth(method="lm", se=F)+
    geom_smooth(aes(x=x, y=y2),method="lm", se=F, color="red")+
    theme_classic(base_size = 14)+
    theme(
          axis.ticks = element_blank())+
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.1,
                                     decimal.mark = '.'), limits = c(ylwr, yupr))+
    scale_x_continuous(
       limits = c(xlwr, xupr))
}
}

#test it
sim.point.reg(slope = -0.25, xlwr=min(full.analyses.df$Log.10..Body.Mass..g.), xupr =max(full.analyses.df$Log.10..Body.Mass..g.), ylwr=0, yupr = 1, add_line = 0.1)



