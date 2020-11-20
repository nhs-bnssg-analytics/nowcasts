## name:          nowcasts (developed for projecting near-term bed occupancy)
## author:        richard wood (richard.wood16@nhs.net)
## organisation:  bnssg ccg (https://bnssgccg.nhs.uk/)
## further info:  https://otexts.com/fpp2/arima-ets.html

library(tidyverse)
library(forecast)

#######################################################################################################################################################

input_file<-"wd/nowcasts_input.csv"

output_location<-"wd/"

today_date<-"2020-11-09"

fitting_window<-30

proj_window<-7

#######################################################################################################################################################

fets<-function(x,h) forecast(ets(x),h=h)
farima<-function(x,h) forecast(auto.arima(x),h=h)

options(warn=-1)

today_date<-as.Date(today_date,"%Y-%m-%d")

dat<-read.csv(input_file,sep=",") %>%
  mutate(dates=as.Date(dates,"%d/%m/%Y"))

dat1<-dat %>%
  pivot_longer(cols=-dates,names_to="measure",values_to="num")

dat2<-lapply(unique(dat1$measure),function(z) {
  tmp0<-dat1[which(dat1$measure==z),]
  tmp1<-tmp0 %>%
    filter(dates<=today_date) %>%
    do(tail(.,n=fitting_window))
  tmp2<-ts(tmp1$num,start=c(as.numeric(format(tmp1$dates[1],"%Y")),as.numeric(format(tmp1$dates[1],"%j"))))
  err_ets<-tsCV(tmp2,fets,h=1)
  ts_ets<-mean(err_ets^2,na.rm=TRUE)
  err_arima<-tsCV(tmp2,farima,h=1)
  ts_arima<-mean(err_arima^2,na.rm=TRUE)
  if (ts_ets<ts_arima) {
    tmp3<-tmp2 %>% 
      ets()
    tmp4<-tmp3 %>%
      forecast(h=proj_window)
    print(paste("fitted ets to",tmp1$measure[1]),quote=FALSE)
  } else {
    tmp3<-tmp2 %>% 
      auto.arima()
    tmp4<-tmp3 %>%
      forecast(h=proj_window)
    print(paste("fitted arima to",tmp1$measure[1]," (arima order:",paste(arimaorder(tmp3),collapse=" "),")"),quote=FALSE)
  }
  ycentral<-c(tmp4$x,tmp4$mean)
  ylower1<-c(tmp4$x,tmp4$lower[,2])
  ylower2<-c(tmp4$x,tmp4$lower[,1])
  yupper1<-c(tmp4$x,tmp4$upper[,2])
  yupper2<-c(tmp4$x,tmp4$upper[,1])
  x<-seq.Date(min(tmp1$dates),by=1,length.out=length(ycentral))
  tmp5<-data.frame(measure=tmp1$measure[1],dates=x,ycentral=ycentral,ylower1=ylower1,ylower2=ylower2,yupper1=yupper1,yupper2=yupper2)
  tmp6<-left_join(tmp5,tmp0[,-3],by="dates")
  return(tmp6)
})

# plots
plots<-lapply(dat2,function(x) {
  ttitle<-x$measure[1]
  x %>%
    mutate(ylower1a=ifelse(dates>today_date,round(ylower1,0),NA)) %>%
    mutate(ylower1a=ifelse(as.numeric(dates) %% 2 == as.numeric(today_date) %% 2,NA,ylower1a)) %>%
    mutate(yupper1a=ifelse(dates>today_date,round(yupper1,0),NA)) %>%
    mutate(yupper1a=ifelse(as.numeric(dates) %% 2 == as.numeric(today_date) %% 2,NA,yupper1a)) %>%
    mutate(ycentrala=ifelse(dates>today_date,round(ycentral,0),NA)) %>%
    mutate(ycentrala=ifelse(as.numeric(dates) %% 2 == as.numeric(today_date) %% 2,NA,ycentrala)) %>%
    ggplot(aes(x=dates)) +
    geom_ribbon(aes(ymin=ylower1,ymax=yupper1),fill="steelblue1",alpha=0.4) +
    geom_ribbon(aes(ymin=ylower2,ymax=yupper2),fill="steelblue3",alpha=0.4) +
    geom_line(aes(y=ycentral),colour="dodgerblue4",size=1) +
    geom_text(aes(x=dates,y=ylower1a,label=ylower1a),position=position_dodge(width=0.9),vjust=+1.5,size=2) +
    geom_text(aes(x=dates,y=yupper1a,label=yupper1a),position=position_dodge(width=0.9),vjust=-1,size=2) +
    geom_text(aes(x=dates,y=ycentrala,label=ycentrala),position=position_dodge(width=0.9),vjust=-0.8,size=2) +
    labs(title=ttitle) +
    scale_x_date(breaks=scales::pretty_breaks(n=6)) +
    scale_y_continuous(breaks=scales::pretty_breaks(n=6),expand=expansion(mult=c(0,0.15))) +
    coord_cartesian(ylim=c(0,NA)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_text(angle=45,hjust=1),
          axis.title.y=element_blank())
})

# output plots
pdf(paste0(output_location,"/nowcasts_plots.pdf"),height=3,width=7)
print(plots)
dev.off()

# output data
dat_out<-suppressWarnings(do.call("bind_rows",dat2))
write.csv(dat_out,paste0(output_location,"/nowcasts_data.csv"),row.names=FALSE)








