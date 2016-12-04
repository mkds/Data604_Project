#Read Daily summary provided by Iowa Environmental Mesonet (IEM)
daily_summary=read.csv("nyc_daily_summary.csv",na.strings = "None",stringsAsFactors = F)
str(daily_summary)

#Convert day into date format
daily_summary$day=as.Date(daily_summary$day,"%Y-%m-%d")
str(daily_summary)

#Try linear models
precip_lm1=lm(precip_in~.,daily_summary[,3:15])
summary(precip_lm1)
daily_summary$month=as.factor(format(daily_summary$day,"%m"))
precip_lm2=lm(precip_in~.,daily_summary[,3:16])
summary(precip_lm2)

##########Try gradient boosting#########
## Try a regerssion model to predict inches then try
###   Logistic model to predict if it will rain or not ##

library(xgboost)

comp=complete.cases(daily_summary)
precip_gbt1 <- xgboost(data = data.matrix(daily_summary[comp,c(3:6,8:16)]),
                      label = data.matrix(daily_summary$precip_in[comp]), 
                      max.depth = 5, eta = 1, nround = 50,
          nthread = 2, objective = "reg:linear")


xgb.importance(model=precip_gbt,feature_names = colnames(daily_summary[comp,c(3:6,8:16)]))

daily_summary$year=as.numeric(format(daily_summary$day,"%Y"))
precip_gbt2 <- xgboost(data = data.matrix(daily_summary[comp,c(3:6,8:17)]),
                      label = data.matrix(daily_summary$precip_in[comp]), 
                      max.depth = 5, eta = 1, nround = 50,
                      nthread = 2, objective = "reg:linear")


xgb.importance(model=precip_gbt2,feature_names = colnames(daily_summary[comp,c(3:6,8:17)]))


### Let's use temp., dewpoint and relative humidity for the simulation
daily=daily_summary[,c(3:7,10:17)]
str(daily)
comp_cases=complete.cases(daily)
daily=daily[comp_cases,]

precip_gbt3 <- xgboost(data = data.matrix(daily[,-5]),
                      label = data.matrix(daily$precip_in), 
                      max.depth = 5, eta = 1, nround = 50,
                      nthread = 5, objective = "reg:linear")

xgb.importance(model=precip_gbt3,feature_names = colnames(daily[,-5]))

cor(daily$max_temp_f[4:3503],daily$max_temp_f[1:3500],use = "pair")

##Function to build transition matrix on numeric values
get_trans_mat=function(x,n){
  x=x[!is.na(x)]
  l=length(x)
  value_range = max(x) - min(x)
  div_factor= value_range/n
  x_bin=round(round(x/div_factor)*div_factor,3)
  mat=table(x_bin[-1],x_bin[-l])
  n=nrow(mat)
  for (i in 1:n){
    mat[i,]=round(mat[i,]/sum(mat[i,]),4)
  }
  return(mat)
}

### Function to give the band in which the value 
##              falls on transition matrix
get_band=function(cur_val,mat){
  vals=as.numeric(rownames(mat))
  cur_row=which.min(abs(vals-cur_val))
  return(vals[cur_row])
  
}


### Get next simulated value from transition matrix 
get_nxt_chain=function(cur_val,mat){
  vals=as.numeric(rownames(mat))
  cur_row=which.min(abs(vals-cur_val))
  nxt_indx=which.max(mat[cur_row,])
  return(vals[nxt_indx])
  #return(sum(vals*mat[cur_row,]))
  
}
  
######Check forecast only using precipetation#####
precip_nxt=get_trans_mat(daily$precip_in,40)

precip_forecast=NULL
for(i in 1:nrow(daily)){
  precip_forecast[i]=get_nxt_chain(daily$precip_in[i-1],precip_nxt)
  
}

table(precip_forecast[-nrow(daily)]>0.05,daily$precip_in[-1]>0.05)

### Using transition matrix for precipitation results in very poor simulation


###Let's  try to simulate parameters and check
max_temp_nxt=get_trans_mat(daily$max_temp_f,40)
min_temp_nxt=get_trans_mat(daily$min_temp_f,40)
max_dew_nxt=get_trans_mat(daily$max_dewpoint_f,40)
min_dew_nxt=get_trans_mat(daily$min_dewpoint_f,40)
avg_rh_nxt=get_trans_mat(daily$avg_rh,40)
min_rh_nxt=get_trans_mat(daily$min_rh,40)
max_rh_nxt=get_trans_mat(daily$max_rh,40)

daily1=daily
#### Using transition matrix simulate parameter value for next day
for (i in 1:nrow(daily1)){
  daily1$max_temp_f[i]=get_band(daily1$max_temp_f[i],
                                         max_temp_nxt)
  
  daily1$min_temp_f[i]=get_band(daily1$min_temp_f[i],
                                         min_temp_nxt)
  daily1$max_dewpoint_f[i]=get_band(daily1$max_dewpoint_f[i],
                                             max_dew_nxt)
  daily1$min_dewpoint_f[i]=get_band(daily1$min_dewpoint_f[i],
                                             min_dew_nxt)
  
  daily1$min_rh[i]=get_band(daily1$min_rh[i],
                                     min_rh_nxt)
  
  daily1$max_rh[i]=get_band(daily1$max_rh[i],
                                     max_rh_nxt)
  daily1$avg_rh[i]=get_band(daily1$avg_rh[i],
                                     avg_rh_nxt)
  
  
}

b=xgboost(data = data.matrix(daily1[,-5]),
          label = data.matrix(daily1$precip_in>0.05),
          max.depth = 1, eta = 1, nround = 50,
          nthread = 5, objective = "binary:logistic")



precip_gbt4 <- xgboost(data = data.matrix(daily1[,-5]),
                       label = data.matrix(daily1$precip_in), 
                       max.depth = 1, eta = 1, nround = 50,
                       nthread = 5, objective = "reg:linear")

pred_daily=daily1
for (i in nrow(pred_daily):2){
  pred_daily$max_temp_f[i]=get_nxt_chain(pred_daily$max_temp_f[i-1],
                                         max_temp_nxt)
  
  pred_daily$min_temp_f[i]=get_nxt_chain(pred_daily$min_temp_f[i-1],
                                         min_temp_nxt)
  pred_daily$max_dewpoint_f[i]=get_nxt_chain(pred_daily$max_dewpoint_f[i-1],
                                         max_dew_nxt)
  pred_daily$min_dewpoint_f[i]=get_nxt_chain(pred_daily$min_dewpoint_f[i-1],
                                         min_dew_nxt)
  
  pred_daily$min_rh[i]=get_nxt_chain(pred_daily$min_rh[i-1],
                                             min_rh_nxt)
  
  pred_daily$max_rh[i]=get_nxt_chain(pred_daily$max_rh[i-1],
                                     max_rh_nxt)
  pred_daily$avg_rh[i]=get_nxt_chain(pred_daily$avg_rh[i-1],
                                     avg_rh_nxt)
  
    
}
in1=predict(precip_gbt4,data.matrix(pred_daily[,-5]))
bin=predict(b,data.matrix(pred_daily[,-5]))
sqrt(mean((in1-daily$precip_in)^2))

table(bin>.5,daily$precip_in>0.05)




##### Check if using transitions by month helps #####
min_temp_nxt_m=NULL
max_temp_nxt_m=NULL
max_dew_nxt_m=NULL
min_dew_nxt_m=NULL
avg_rh_nxt_m=NULL
min_rh_nxt_m=NULL
max_rh_nxt_m=NULL
month=as.numeric(as.character(daily$month))
for(i in 1:12){
  max_temp_nxt_m[[i]]=get_trans_mat(daily$max_temp_f[month==i],40)
  min_temp_nxt_m[[i]]=get_trans_mat(daily$min_temp_f[month==i],40)
  max_dew_nxt_m[[i]]=get_trans_mat(daily$max_dewpoint_f[month==i],40)
  min_dew_nxt_m[[i]]=get_trans_mat(daily$min_dewpoint_f[month==i],40)
  avg_rh_nxt_m[[i]]=get_trans_mat(daily$avg_rh[month==i],40)
  min_rh_nxt_m[[i]]=get_trans_mat(daily$min_rh[month==i],40)
  max_rh_nxt_m[[i]]=get_trans_mat(daily$max_rh[month==i],40)


}



daily2=daily
for (i in 1:nrow(daily1)){
  daily1$max_temp_f[i]=get_band(daily1$max_temp_f[i],
                                max_temp_nxt_m[[month[i]]])
  
  daily1$min_temp_f[i]=get_band(daily1$min_temp_f[i],
                                min_temp_nxt_m[[month[i]]])
  daily1$max_dewpoint_f[i]=get_band(daily1$max_dewpoint_f[i],
                                    max_dew_nxt_m[[month[i]]])
  daily1$min_dewpoint_f[i]=get_band(daily1$min_dewpoint_f[i],
                                    min_dew_nxt_m[[month[i]]])
  
  daily1$min_rh[i]=get_band(daily1$min_rh[i],
                            min_rh_nxt_m[[month[i]]])
  
  daily1$max_rh[i]=get_band(daily1$max_rh[i],
                            max_rh_nxt_m[[month[i]]])
  daily1$avg_rh[i]=get_band(daily1$avg_rh[i],
                            avg_rh_nxt_m[[month[i]]])
  
  
}

b2=xgboost(data = data.matrix(daily2[,-5]),
          label = data.matrix(daily2$precip_in>0.05),
          max.depth = 1, eta = 1, nround = 50,
          nthread = 5, objective = "binary:logistic")



precip2_gbt4 <- xgboost(data = data.matrix(daily2[,-5]),
                       label = data.matrix(daily2$precip_in), 
                       max.depth = 1, eta = 1, nround = 50,
                       nthread = 5, objective = "reg:linear")

pred_daily2=daily2
for (i in nrow(pred_daily2):2){
  pred_daily2$max_temp_f[i]=get_nxt_chain(pred_daily2$max_temp_f[i-1],
                                         max_temp_nxt_m[[month[i]]])
  
  pred_daily2$min_temp_f[i]=get_nxt_chain(pred_daily2$min_temp_f[i-1],
                                         min_temp_nxt_m[[month[i]]])
  pred_daily2$max_dewpoint_f[i]=get_nxt_chain(pred_daily2$max_dewpoint_f[i-1],
                                             max_dew_nxt_m[[month[i]]])
  pred_daily2$min_dewpoint_f[i]=get_nxt_chain(pred_daily2$min_dewpoint_f[i-1],
                                             min_dew_nxt_m[[month[i]]])
  
  pred_daily2$min_rh[i]=get_nxt_chain(pred_daily2$min_rh[i-1],
                                     min_rh_nxt_m[[month[i]]])
  
  pred_daily2$max_rh[i]=get_nxt_chain(pred_daily2$max_rh[i-1],
                                     max_rh_nxt_m[[month[i]]])
  pred_daily2$avg_rh[i]=get_nxt_chain(pred_daily2$avg_rh[i-1],
                                     avg_rh_nxt_m[[month[i]]])
  
  
}
in2=predict(precip2_gbt4,data.matrix(pred_daily2[,-5]))
bin2=predict(b2,data.matrix(pred_daily2[,-5]))
sqrt(mean((in2-daily$precip_in)^2))

table(bin2>.5,daily$precip_in>0.05)
#### Looks like transition matrix  by month is not good

#### To do - Three day forecast
#### To do - try transition matrix by season









