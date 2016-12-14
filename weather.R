#Read Daily summary provided by Iowa Environmental Mesonet (IEM)
daily_summary=read.csv("nyc_daily_summary.csv",na.strings = "None",stringsAsFactors = F)
str(daily_summary)

#Convert day into date format
daily_summary$day=as.Date(daily_summary$day,"%Y-%m-%d")
str(daily_summary)
daily_summary$month=as.factor(format(daily_summary$day,"%m"))
daily_summary$year=as.numeric(format(daily_summary$day,"%Y"))
daily_summary$daynum=as.numeric(format(daily_summary$day,"%d"))
comp=complete.cases(daily_summary)
daily_all=daily_summary[comp,]
summary(daily_all)
daily_tp=daily_all[,c("precip_in","max_temp_f","min_temp_f","climo_high_f",
                          "climo_low_f","climo_precip_in","month")]

daily_tph=daily_all[,c("precip_in","max_temp_f","min_temp_f","climo_high_f",
                      "climo_low_f","climo_precip_in","month","min_rh",
                      "max_rh","avg_rh")]

daily_tphd=daily_all[,c("precip_in","max_temp_f","min_temp_f","climo_high_f",
                       "climo_low_f","climo_precip_in","month","min_rh",
                       "max_rh","avg_rh","max_dewpoint_f","min_dewpoint_f")]

############### Models ############

library(xgboost)
x1tp=xgboost(data = data.matrix(daily_tp[,-1]),
             label = data.matrix(daily_tp$precip_in), 
             max.depth = 2, eta = 1, nround = 50,
             nthread = 2, objective = "reg:linear")

x1tph=xgboost(data = data.matrix(daily_tph[,-1]),
             label = data.matrix(daily_tph$precip_in), 
             max.depth = 2, eta = 1, nround = 50,
             nthread = 2, objective = "reg:linear")

xbtph1=xgboost(data = data.matrix(daily_tph[,-1]),
              label = data.matrix(daily_tph$precip_in>0.01), 
              max.depth = 2, eta = 1, nround = 50,
              nthread = 2, objective = "binary:logistic")

xbtph5=xgboost(data = data.matrix(daily_tph[,-1]),
               label = data.matrix(daily_tph$precip_in>0.05), 
               max.depth = 2, eta = 1, nround = 50,
               nthread = 2, objective = "binary:logistic")

xbtpah5=xgboost(data = data.matrix(daily_tph[,c(-1,-8,-9)]),
        label = data.matrix(daily_tph$precip_in>0.05), 
        max.depth = 2, eta = 1, nround = 50,
        nthread = 2, objective = "binary:logistic")
xbtphd5=xgboost(data = data.matrix(daily_tphd[,-1]),
                label = data.matrix(daily_tph$precip_in>0.05), 
                max.depth = 2, eta = 1, nround = 50,
                nthread = 2, objective = "binary:logistic")

#============
max_temp_nxt=get_trans_mat(daily_all$max_temp_f,40)
min_temp_nxt=get_trans_mat(daily_all$min_temp_f,40)
max_dew_nxt=get_trans_mat(daily_all$max_dewpoint_f,40)
min_dew_nxt=get_trans_mat(daily_all$min_dewpoint_f,40)
avg_rh_nxt=get_trans_mat(daily_all$avg_rh,40)
min_rh_nxt=get_trans_mat(daily_all$min_rh,40)
max_rh_nxt=get_trans_mat(daily_all$max_rh,40)

#===============

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
  #return(vals[nxt_indx])
  return(sum(vals*mat[cur_row,]))
  
}


mc_sim=function(cur_val,mat,n){
  vals=as.numeric(rownames(mat))
  cur_state=which.min(abs(vals-cur_val))
  state=matrix(0,ncol=ncol(mat))
  state[cur_state]=1
  #nxt_indx=which.max(mat[cur_state,])
  #return(vals[nxt_indx])
  value=NULL
  for (i in 1:n){
    state=state%*%mat
    value[i]=sum(vals*state)
  }
  #return(value)
  return(sum(vals*state))
  
}

#=====


forecast_ser=function(num_days=1,bins=30){
  max_temp_nxt=get_trans_mat(daily_tph$max_temp_f,n=bins)
  min_temp_nxt=get_trans_mat(daily_tph$min_temp_f,n=bins)
  #max_dew_nxt=get_trans_mat(daily_tph$max_dewpoint_f,n=bins)
  #min_dew_nxt=get_trans_mat(daily_tph$min_dewpoint_f,n=bins)
  avg_rh_nxt=get_trans_mat(daily_tph$avg_rh,n=bins)
  min_rh_nxt=get_trans_mat(daily_tph$min_rh,n=bins)
  max_rh_nxt=get_trans_mat(daily_tph$max_rh,n=bins)
  
  sim_predictors=daily_tph
  
  for (i in nrow(sim_predictors):(num_days+1)){
    max_temp=mc_sim(daily_tph$max_temp_f[i-num_days],max_temp_nxt,num_days)
    min_temp=mc_sim(daily_tph$min_temp_f[i-num_days],min_temp_nxt,num_days)
    #max_dewpoint=mc(daily2$max_dewpoint_f[i-num_days]
    #min_dewpoint=mc(daily2$min_dewpoint_f[i- num_days]
    max_rh=mc_sim(daily_tph$max_rh[i-num_days],max_rh_nxt,num_days)
    min_rh=mc_sim(daily_tph$min_rh[i-num_days],min_rh_nxt,num_days)
    avg_rh=mc_sim(daily_tph$avg_rh[i-num_days],avg_rh_nxt,num_days)
    
    
    sim_predictors$max_temp_f[i]=max_temp
    sim_predictors$min_temp_f[i]=min_temp
    #sim_predictors$max_dewpoint_f[i]=max_dewpoint
    #sim_predictors$min_dewpoint_f[i]=min_dewpoint
    sim_predictors$min_rh[i]=min_rh
    sim_predictors$max_rh[i]=max_rh
    sim_predictors$avg_rh[i]=avg_rh
    
  }
  
  
  in1=predict(x1tph,data.matrix(sim_predictors[,-1]))
  #bi1_1=predict(xbtph1,data.matrix(sim_predictors[,-1]))
  #bi1_5=predict(xbtph5,data.matrix(sim_predictors[,-1]))
  print(sqrt(mean((in1-daily_tph$precip_in)^2)))
  #month_nxt_day=month_rmse(daily_all$precip_in,in1,month=daily_all$month,year=daily_all$year)
  print(table(in1>0.01,daily_tph$precip_in>0.01))
  #print(table(bi1_1>.5,daily_all$precip_in>0.01))
  #print(table(bi1_5>.5,daily_all$precip_in>0.05))
  Overall=sum((in1>.05) == (daily_tph$precip_in>0.05))/nrow(daily_tph)
  RainyDays=sum((in1>.05) & (daily_tph$precip_in>0.05))/sum(daily_tph$precip_in>0.05)
  cat("OVerall",Overall)
  cat("\nRain",RainyDays)
  sim_predictors$pred_percip=in1
  return(sim_predictors)
  
}
acc_1d=forecast_ser(1,10)
acc_3d=forecast_ser(3,10)
acc_5d=forecast_ser(5,10)
acc_20d=forecast_ser(20,10)
