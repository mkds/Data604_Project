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


#Train with upto 2015 and do prediction for 2016 see how the model performs
year=daily_all$year
x=xgboost(data = data.matrix(daily_tph[year!=2016,-1]),
          label = data.matrix(daily_tph$precip_in[year!=2016]), 
          max.depth = 2, eta = 1, nround = 50,
          nthread = 2, objective = "reg:linear")
pr=predict(x,data.matrix(daily_tph[year==2016,-1]))
table(pr>0.05,daily_tph$precip_in[year==2016]>0.05)

#Now train with all data so that we could use this for simulation

x1tph=xgboost(data = data.matrix(daily_tph[,-1]),
             label = data.matrix(daily_tph$precip_in), 
             max.depth = 2, eta = 1, nround = 50,
             nthread = 2, objective = "reg:linear")



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
  c=ncol(mat)
  if (n > c) mat=mat[1:(n-1),]
  if (c > n) mat=mat[,1:(c-1)]
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

#Tried various values for number of states and state 10 looked better
sim_1d=forecast_ser(1,10)
sim_3d=forecast_ser(3,10)
sim_5d=forecast_ser(5,10)
sim_20d=forecast_ser(20,10)

#Detect temprature season using Gibbs sampling
daily_all$week=as.numeric(format(daily_all$day,"%W"))


gibbs_season=function(n){
  #Inital Values for breakpoints
  t1 = sample(1:180,1)
  t2 = sample((t1+5):250,1)
  t3 = sample((t2+5):275,1)
  t4 = sample((t3+5):295,1)
  
  season_change=data.frame(t1,t2,t3,t4)
  
  #Gibbs Sampling
  for (j in 1:n){
    #Values by break up
    y1 = gety(t1:(t2-1))
    y2 = gety(t2:(t3-1))
    y3 = gety(t3:(t4-1))
    y4 = gety(c(t4:295,1:(t1-1)))
    
    #Mean value for each season
    m1 = mean(y1)
    m2 = mean(y2)
    m3 = mean(y3)
    m4 = mean(y4)
    
    s1 = sd(y1)
    s2 = sd(y2)
    s3 = sd(y3)
    s4 = sd(y4)
    
    if (is.na(s1)) s1 = 0.01
    if (is.na(s2)) s2 = 0.01
    if (is.na(s3)) s3 = 0.01
    if (is.na(s4)) s4 = 0.01
    
    mu1 = rnorm(1,m1,s1/length(y1))
    mu2 = rnorm(1,m2,s2/length(y2))
    mu3 = rnorm(1,m3,s3/length(y3))
    mu4 = rnorm(1,m4,s4/length(y4))
    
    if (length(y1) > 1){
      si1 = rchisq(1,length(y1)-1) * s1 / (length(y1)-1) 
    } else {
      si1 = 0.01 
    }
    
    if (length(y2) > 1){
      si2 = rchisq(1,length(y2)-1) * s2 / (length(y2)-1) 
    } else {
      si2 = 0.01 
    }
    
    if (length(y3) > 1){
      si3 = rchisq(1,length(y3)-1) * s3 / (length(y3)-1) 
    } else {
      si3 = 0.01 
    }
    
    if (length(y4) > 1){
      si4 = rchisq(1,length(y4)-1) * s4 / (length(y4)-1) 
    } else {
      si4 = 0.01 
    }
    
    
    pt1 = numeric(180)
    pt2 = numeric(250)
    pt3 = numeric(275)
    pt4 = numeric(295)
    
    
    for (i in 1:min(t2-1,180)){
      group1 = gety(i:(t2-1))
      group2 = gety(c(t4:295,1:(i-1)))
      
      pt1[i] = conditional(group1,mu1,si1) * conditional(group2,mu4,si4)
      
    }
    
    
    for (i in (t1+1):min(t3-1,250)){
      group1 = gety((t1+1):i)
      group2 = gety((i+1):(t3-1))
      
      pt2[i] = conditional(group1,mu1,si1) * conditional(group2,mu2,si2)
      
    }
    
    for (i in (t2+1):min(t4-1,275)){
      group1 = gety((t2+1):i)
      group2 = gety((i+1):(t4-1))
      
      pt3[i] = conditional(group1,mu2,si2) * conditional(group2,mu3,si3)
      
    }
    
    for (i in (t3+1):295){
      group1 = gety((t3+1):i)
      group2 = gety(c(i:295,1:(t1-1)))
      
      pt4[i] = conditional(group1,mu3,si3) * conditional(group2,mu4,si4)
      
    }
    
    if (sum(pt1) > 0) {
      t1 = sample(1:180,1,prob=pt1) 
    } else {
      t1 = sample(1:180,1)
    }
    
    if (sum(pt2[(t1+1):250]) > 0) {
      t2 = sample((t1+1):250,1,prob=pt2[(t1+1):250]) 
    } else {
      t2 = sample((t1+1):250,1)
    }
    
    if (sum(pt3[(t2+1):275]) > 0) {
      t3 = sample((t2+1):275,1,prob=pt3[(t2+1):275]) 
    } else { 
      t3 = sample((t2+1):275,1)
    }
    if (sum(pt4[(t3+1):295]) > 0) {
      t4 = sample((t3+1):295,1,prob=pt4[(t3+1):295]) 
    } else {
      t4 = sample((t3+1):295,1)
    }
    
    season_change=rbind(season_change,c(t1,t2,t3,t4))
    
    if ( (j %% 50 ) == 0) print(j)
    
  }
  return(season_change)  
  
}


#Conditional probability
conditional=function(y,m,s){
  my = mean(y)
  ly = length(y)
  if (ly > 1) sy = sd(y) else sy=0.01
  p = prod(dnorm(y,m,s))*                        #Likelyhood of data given mean, sd
    dnorm(m,my,sy/sqrt(ly)) *             #Probability of mean
    dchisq((s/sy)^2*ly,ly)               #Probability of sd
  
  return(p)
}

gety=function(i) return(y[i])


weekly = daily_all[daily_all$year>2010,] %>% group_by(week,year) %>% 
  summarise(wtemp=mean(max_temp_f))
weekly=as.data.frame(weekly)
y=weekly$wtemp

bp_temp=gibbs_season(5000)
par(mfrow=c(2,2))
hist(bp_temp$t1,main="Change Point 1")
hist(bp_temp$t2,main="Change Point 2")
hist(bp_temp$t3,main="Change Point 3")
hist(bp_temp$t4,main="Change Point 4")

#Expected value for change point on the time series
mean(bp_temp$t1)
mean(bp_temp$t2)
mean(bp_temp$t3)
mean(bp_temp$t4)

#Week on which the changepoint occurs
weekly$week[79]
weekly$week[120]
weekly$week[219]
weekly$week[256]

daily_all$season = 1
week=daily_all$week
daily_all$season[week >= 13 & week < 20] = 2
daily_all$season[week >= 20 & week < 37] = 3
daily_all$season[week >= 37 & week < 44] = 4
daily_tph$season = daily_all$season

#Try to run simulation by season
all_tph = daily_tph
daily_tph = all_tph[daily_all$season==1,]

sim_s1_1d=forecast_ser(1,10)


#Validate simulation
#The p-value should be high as we expect simulated value to have same distribution 
#      as actual value
t.test(daily_tph$max_temp_f,sim_s1_1d$max_temp_f)
t.test(daily_tph$max_rh,sim_s1_1d$max_rh)
t.test(daily_tph$avg_rh,sim_s1_1d$avg_rh)
t.test(daily_tph$min_temp_f,sim_s1_1d$min_temp_f)
t.test(daily_tph$min_rh,sim_s1_1d$min_rh)

#The high p-value 

sim_s1_3d=forecast_ser(3,10)
sim_s1_5d=forecast_ser(5,10)
sim_s1_20d=forecast_ser(20,10)


daily_tph = all_tph[daily_all$season==2,]

sim_s2_1d=forecast_ser(1,10)
sim_s2_3d=forecast_ser(3,10)
sim_s2_5d=forecast_ser(5,10)
sim_s2_20d=forecast_ser(20,10)

daily_tph = all_tph[daily_all$season==3,]

sim_s3_1d=forecast_ser(1,10)
sim_s3_3d=forecast_ser(3,10)
sim_s3_5d=forecast_ser(5,10)
sim_s3_20d=forecast_ser(20,10)

daily_tph = all_tph[daily_all$season==4,]

sim_s4_1d=forecast_ser(1,10)
sim_s4_3d=forecast_ser(3,10)
sim_s4_5d=forecast_ser(5,10)
sim_s4_20d=forecast_ser(20,10)

#=============== Simulation using fullset rather than by season seems to be better
daily_tph = all_tph
sim_1d=forecast_ser(1,10)
sim_3d=forecast_ser(3,10)
sim_5d=forecast_ser(5,10)
sim_20d=forecast_ser(20,10)

