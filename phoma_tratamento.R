#R script with steps taken to calculate the generalized beta function for the phoma isolates

#The current analysis was performed in R 3.6.0 (2019-04-26)
#Additional packages: tidyverse(1.2.1), xlsx(0.6.1), ggthemes(4.2.0)


library(xlsx) #Loading library to read excel .xlsx files
library(tidyverse) #tidyverse colletion of packages to handle data and plotting
library(ggthemes) #further plotting package with extra themes
#Reading the database for analysis
Base_phoma <- read.xlsx("data/Phoma_tarda_single_table_final.xlsx", sheetIndex = 1, encoding = "UTF-8")
 

#Ensuring we are only reading the important columns 
#and ignoring possible modifications in the excel file
Base_phoma <- Base_phoma[,seq(1,4)]

Base_phoma%>% #A error check
  filter(is.na(Mycelial_Growth_mm_day))


#Errorbarplot for each isolate:

isolados <- Base_phoma%>%
  select(Isolate)%>%
  pull(.)%>%
  factor(.)%>%
  levels(.)

for (i in 1:length(isolados)){
png(paste0("figs/","errorbar/",isolados[i],"_errorbarplot.png"))
print(Base_phoma%>%
  filter(Isolate == isolados[i])%>%
  group_by(Temperature_C)%>%
  summarize(cresc_medio = mean(Mycelial_Growth_mm_day, na.rm = T), cresc_min = min(Mycelial_Growth_mm_day, na.rm = T), cresc_max = max(Mycelial_Growth_mm_day, na.rm = T))%>%
  ggplot(aes(x = Temperature_C, y = cresc_medio, ymin = cresc_min, ymax = cresc_max, group = 1))+
  theme_few()+
  stat_summary(geom = "line", fun.y = mean, size = 1)+
  geom_errorbar(width = 0.2, size = 0.7)+
  geom_point()+
  ggtitle(isolados[i])+
  theme(legend.position = "none")+xlab("Temperature ºC")+ ylab("Mycelial Growth (mm/day)")+
  ggtitle("Mycelial Growth", subtitle = "Errorbar"))
dev.off()
}




#Next step: Model the data through the generalized beta function:

gener_beta <- function(b1,b2,b3,b4,b5, x){#Calling the function
  return(b1*(x-b2)^b4*(b3-x)^b5)
} #Declaração da Função

loss_gener_beta <- function(b){#Loss Function for the data and optimization algorithm
  #Inside this function I am already calling the dataframe with the data,
  #To apply it todifferent datasets one needs to slightly modify this.
  b1 <- b[1]
  b2<-b[2]
  b3<-b[3]
  b4<-b[4]
  b5<-b[5]
  (1/2)*(sum((base_teste$Mycelial_Growth_mm_day-gener_beta(b1,b2,b3,b4,b5,base_teste$Temperature_C))^2))*(1/dim(base_teste)[1])
}

gradient_loss_beta <- function(b){
  #Here I'm calculating the gradients as an input for the optimization algorithmn
  #Again, I'm already calling the dataframe I'm using for optimizing
  t = base_teste$Temperature_C
  y = base_teste$Mycelial_Growth_mm_day
  del_1 = sum((-1)*(y-gener_beta(b[1],b[2],b[3],b[4],b[5], t))*(t-b[2])^b[4]*(b[3]-t)^b[5])
  del_2 = sum((-1)*(y-gener_beta(b[1],b[2],b[3],b[4],b[5], t))*b[1]*b[4]*(-1)*(t-b[2])^(b[4]-1)*(b[3]-t)^b[5])
  del_3 = sum((-1)*(y-gener_beta(b[1],b[2],b[3],b[4],b[5], t))*b[1]*b[5]*(t-b[2])^b[4]*(b[3]-t)^(b[5]-1))
  del_4 = sum((-1)*(y-gener_beta(b[1],b[2],b[3],b[4],b[5], t))*b[1]*(b[3]-t)^b[5]*(t-b[2])^b[4]*log(t-b[2]))
  del_5 = sum((-1)*(y-gener_beta(b[1],b[2],b[3],b[4],b[5], t))*b[1]*(t-b[2])^b[4]*(b[3]-t)^b[5]*log(b[3]-t))
  return((c(del_1,del_2,del_3,del_4,del_5)/dim(base_teste)[1]))
}

#Optimizing for each isolate:


isolados <- Base_phoma%>% #Pulling all isolates from the dataset:
  select(Isolate)%>%
  pull(.)%>%
  factor(.)%>%
  levels(.)

params <- data.frame(Isolate = c(), par = c(), mse = c(), convergence = c(), b1 = c(), b2 = c(), b3 = c(), b4 = c(), b5 = c(), R_2 = c(), method = c(), F_test = c())
j = 1#Declaring an empty dataframe to input the results
for (i in isolados){
  Base_phoma%>%
    filter(!is.na(Mycelial_Growth_mm_day))%>%
    filter(Isolate == i)-> base_teste #Pulling the dataframe for each isolate

  p <- constrOptim(c(0.3,5,30,0.7,0.7), f = loss_gener_beta, grad = gradient_loss_beta, ui = rbind(c(1,0,0,0,0),c(0,-1,0,0,0),c(0,0,1,0,0),c(0,0,0,1,0),c(0,0,0,0,1)),
                   ci = c(0,-7,27,0,0)) #Performing the Constrained Opimizer for each isolate. Detais in the Annex.
  base_teste%>%
    mutate(beta = gener_beta(p$par[1],p$par[2],p$par[3],p$par[4],p$par[5], Temperature_C))%>%
    select(beta)%>%
    pull(.)-> x
  
  method = "BFGS"
  #Adding the results to each row
  params[j,"Isolate"] = i
  params[j,"mse"] = p$value*2 #Since we are optimizing on half of the MSE, here I'm multiplying by 2
  params[j,"convergence"] = p$convergence #Checking if the algorithmn converged. 0 means ok.
  params[j,"b1"] = p$par[1]
  params[j,"b2"] = p$par[2]
  params[j,"b3"] = p$par[3]
  params[j,"b4"] = p$par[4]
  params[j,"b5"] = p$par[5]
  params[j,"R_2"] = cor(x, base_teste$Mycelial_Growth_mm_day)^2
  params[j, "F_test"] = (nrow(base_teste)-2)*params[j, "R_2"]/(1-params[j, "R_2"])
  params[j,"method"] = method
  j = j+1
  print(p)
  }
params <- params %>%
  mutate(maxim = (b2*b5+b3*b4)/(b4+b5)) #Calculating the max temperature by maximizing the function (setting the derivative = 0)

#Plotting the results
setwd("~\\phoma\\figs\\beta_adjusted")
#Saving an image for each Isolate
for (i in 1:nrow(params)){
  plot_dados <- data.frame(Temperature_C = seq(7,27,by = 0.01), fit = gener_beta(params[i,"b1"],params[i,"b2"],params[i,"b3"],params[i,"b4"],params[i,"b5"], seq(7,27,0.01)))
  Base_phoma%>%
    filter(Isolate == params[i, "Isolate"])%>%
    group_by(Temperature_C)%>%
    summarize(cresc_medio = mean(Mycelial_Growth_mm_day, na.rm = T), 
              cresc_min = min(Mycelial_Growth_mm_day, na.rm = T), cresc_max = max(Mycelial_Growth_mm_day, na.rm = T))->base_plot
  eq <- substitute(~~italic(r)^2~"="~r2,list(r2 = format(params[i, "R_2"], digits = 2)))
  png(paste0(params[i,"Isolate"],"_beta",".png"), res = 96)
    print(ggplot(data = base_plot, aes(x = Temperature_C, y = cresc_medio))+geom_point()+geom_errorbar(aes(ymin = cresc_min,ymax = cresc_max), width = 0.2)+
    geom_line(data = plot_dados, aes(x = Temperature_C, y = fit))+ggtitle(params[i, "Isolate"])+theme_few()+xlab("Temperature ºC")+
      ylab("Mycelial Growth (mm/day)")+geom_text(label = as.expression(eq), aes(x = 7, y = max(cresc_max)),vjust = "inward", hjust = "inward"))
  dev.off()
}

#Saving the results in an excel sheet (.xlsx file)
setwd("~//phoma//output_data")
write.xlsx2(params, file = "Generalized Beta parameters.xlsx")
#Histogram of max growth temperatures:
params%>%
  select(maxim)%>%
  pull(.)%>%
  hist(.)
hist(params$maxim)
