#Housekeeping
packages<-c("tidyverse","rio","rlist","data.table","nardl","dynamac","urca","tseries",
            "readxl","dint","lubridate","summarytools","rtf","patchwork","haven")
sapply(packages,library,character.only=T)

#Read data
master_data<-read_excel("master data.xlsx")

#processing
master_data<-master_data[-100,-c(3,5)]
names(master_data)<-c("Date","food","er_rate","oil_dol")
master_data<-master_data %>% mutate(oil_rs=oil_dol*er_rate)
master_data$Date<-seq(as.Date("2012-04-01"),
                      as.Date("2020-06-01"),by="month")
master_data<-master_data %>% filter(between(Date,as.Date("2012-04-01"),as.Date("2017-03-01"))) 

#plot data
#data_p<-master_data %>% pivot_longer(names_to = "Variables",values_to = "Values",-Date)
#p<-data_p %>% ggplot(aes(x=Date,y=Values))+
 #       geom_line()+
  #      facet_wrap(~Variables,scales = "free")


a<-master_data %>% ggplot(aes(x=Date,y=food))+
        geom_line(col="orangered",lwd=1)+
        theme_bw()+
        scale_x_date(date_labels = "%b-%Y")+
        labs(y="Food Price Index")

b<-master_data %>% ggplot(aes(x=Date,y=oil_dol))+
        geom_line(col="midnightblue",lwd=1)+
        theme_bw()+
        scale_x_date(date_labels = "%b-%Y")+
        labs(y="Oil Price ($/Barrel)")+
        scale_y_continuous(breaks =seq(0,110,by=20))

c<-master_data %>% ggplot(aes(x=Date,y=oil_rs))+
        geom_line(col=c("#228B22"),lwd=1)+
        theme_bw()+
        scale_x_date(date_labels = "%b-%Y")+
        labs(y="Oil Price (₹/Barrel)")

d<-(a+b)/c
ggsave("new_trend.jpg",plot = last_plot(),width = 9,height = 6)

#summary statistics
stat<-summary(master_data)
stat1<-as.data.frame(t(descr(master_data)))
stat2<-stat1 %>% select(c(1,5,2,3,7,11,13))
rownames(stat2)<-c("n","Food Price Index","Oil Price ($)","Oil Price (₹)")
stat2<-stat2[-1,]
stat2<-round(stat2,3)

rtf<-RTF("summary_stat.doc")
addParagraph(rtf,"Summary Statistics")
addTable(rtf,stat2,row.names = T)
addParagraph(rtf,"\n")
addParagraph(rtf,"Correlation Table")
addTable(rtf,t,row.names = T)
done(rtf)

#correlation
t<-corstarsl(master_data[,-c(1,3)])
t[1,1]<-1
t[2,2]<-1
t[3,3]<-1
dimnames(t)<-list(c("Food Price Index","Oil Price ($)","Oil Price (₹)"),
                  c("Food Price Index","Oil Price ($)","Oil Price (₹)"))

master_DT<-data.table(master_data)

#log of all the required variables
log_fn<-function(DT,cols){
        for(col in cols){
                new_name<-paste0("ln_",col)
                DT[,(new_name):= log(get(col))]
        }
}

vars<-master_DT[,.(food,oil_dol,oil_rs)] %>% names()

log_fn(master_DT,cols = vars)

master_DT<-master_DT[,er_rate:=NULL]

#Unit root tests
#levels

unit_root<-apply(master_DT[,-1],2,function(x){
        return(
                list(
                        ur.ers(x, type = "DF-GLS",model = "constant",lag.max = 4),
                        ur.ers(x, type = "DF-GLS",model = "trend",lag.max = 4),
                        summary(ur.pp(x,type = "Z-tau",model = "constant")),
                        summary(ur.pp(x,type = "Z-tau",model = "trend"))
                )
        )
}) %>% unlist()

kpss_test<-map(master_DT[,-1],~ur.kpss(.x,type = "tau",lags = "short")) %>% unlist()

test_stat_levels_unitroot<- unit_root%>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),model=.x@model,
                                                              test=.x@test.name),.id="Variable")

kpss_stat<-kpss_test %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),
                                              test=.x@test.name),.id="Variable")

tot_stat<-full_join(test_stat_levels_unitroot,kpss_stat)

writexl::write_xlsx(tot_stat,"unit root.xlsx")

unitroot_crit<-map(unit_root[1:4],~data.frame(crit_val=round(.x@cval,3)))
kpss_crit<-map(kpss_test[1],~data.frame(crit_val=round(.x@cval,3))) %>% unlist() %>% 
        as.data.frame() %>% t() %>% as.data.frame()
names<-list("ERS-Intercept","ERS-Trend","PP-Intercept","PP-Trend")


walk2(unitroot_crit,names,~write_xlsx(.x,paste0(.y,".xlsx")))
write_xlsx(kpss_crit,"kpss.xlsx") 

#first difference
diff_fn<-function(DT,cols,order){
        for(col in cols){
                new_name<-paste0("diff_",col)
                DT[,(new_name):= get(col)-shift(get(col),type = "lag",n=order)]
        }
}
vars_diff<-master_DT[,-1] %>% names()

diff_fn(master_DT,vars_diff,1)


unit_root_diff<-apply(master_DT[-1,-1],2,function(x){
        return(
                list(
                        ur.ers(x, type = "DF-GLS",model = "constant",lag.max = 4),
                        ur.ers(x, type = "DF-GLS",model = "trend",lag.max = 4),
                        summary(ur.pp(x,type = "Z-tau",model = "constant")),
                        summary(ur.pp(x,type = "Z-tau",model = "trend"))
                )
        )
}) %>% unlist()

kpss_test_fd<-map(master_DT[-1,-1],~ur.kpss(.x,type = "tau",lags = "short")) %>% unlist()

test_stat_diff_unitroot<- unit_root_diff%>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),model=.x@model,
                                                                 test=.x@test.name),.id="Variable")

kpss_stat_diff<-kpss_test_fd %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),
                                                      test=.x@test.name),.id="Variable")

tot_stat_diff<-full_join(test_stat_diff_unitroot,kpss_stat_diff)

unitroot_crit_diff<-map(unit_root_diff[1:4],~data.frame(crit_val=round(.x@cval,3)))
kpss_crit_diff<-map(kpss_test_fd[1],~data.frame(crit_val=round(.x@cval,3))) %>% unlist() %>% 
        as.data.frame() %>% t() %>% as.data.frame()
names_diff<-map(names,~paste0("DIFF_",.x))


walk2(unitroot_crit_diff,names_diff,~write_xlsx(.x,paste0(.y,".xlsx")))
write_xlsx(kpss_crit_diff,"kpss_diff.xlsx")        
writexl::write_xlsx(tot_stat_diff,"unit root_fd.xlsx")

#NARDL
mod1<-nardl(ln_food~ln_oil_dol,master_DT,ic="aic",graph = T,case=3)
summary(mod1)
mod2<-nardl(food~oil_dol,master_DT,ic="aic",maxlag = T,graph = T,case=3)
summary(mod2)
mod3<-nardl(ln_food~ln_oil_rs,master_DT,ic="aic",graph = T,case=3)
summary(mod3)
mod4<-nardl(food~oil_rs,master_DT,ic="aic",graph = T,case=3)
summary(mod4)
  
plotmplier(mod1,mod1$nl,1,10)
matser_dt2$Date<-as.Date.character(matser_dt2$Date)
write.xlsx(matser_dt2,"Data_est.xlsx")
write_dta(matser_dt2,"data_est.dta")
class(master_DT$Date)
matser_dt2<-master_DT
matser_dt2$Date<-as_yearmon(matser_dt2$Date)
months.POSIXt(as.Date.POSIXct(matser_dt2$Date))
as.Date(matser_dt2$Date,origin="1900-01-01",%Y-%m)
as.yearmon(matse)