# THIS CODE IS ADAPTED FROM PW PROJECT FROM 2017-2018 WITH HARRING AND STAPLETON


# preliminary steps -------------------------------------------------------

rm(list = ls(all = TRUE))
#install.packages("MplusAutomation") #one-time installation
library(MplusAutomation)

# define seed
seed <- 123456

# define shell files locations
in.biago <- "C:\\Users\\yangsup\\Desktop\\dissertation\\mplus_shells\\"
in.cal <- "C:\\Users\\dlee.intern\\Google Drive\\Documents\\academic\\dissertation\\1_simulation\\mplus_shells\\"
in.desk <- 
in.lap <- 
  
# define location
loc <- in.biago


# preliminary functions ---------------------------------------------------

# missing mechanism functions

#data.complete <- read.table(paste0(loc,'1_datagen\\generated_data.txt' ))
#mechanism.test <- "MCAR"
#miss.percentage <- 5

miss.fun <- function(comp.dat, miss.mech, miss.perc){
#comp.dat <- data.complete
#miss.mech <- mechanism.test  
#miss.perc <- miss.percentage
    
  # read in generated data
  colnames(comp.dat) <- c("y1","y2","y3","y4","grp")
  np <- nrow(comp.dat) #N-size
  ni <- 4 #number of timepoints
  bin <- matrix(rep(NA, np * ni), np, ni)
  
  q <- miss.mech
  a <- miss.perc
  
  w <- as.numeric(a)
  w <- (100 - w)/100 # controls percentage of missing (set percentage of non-missing desired)
  
  if( q == "MCAR" ){
    
    for (u in 1:np){
      for (v in 1:ni){
        rini <- runif(1)
        if (rini > w){ #compare the random value with the probability 
          bin[u, v] <- 1 #assign as "missing" if this random value is greater than the probability.
        }else{
          bin[u, v] <- 0 #assign as "observed" otherwise
        }  
      }
    }
  }
  
  if (q == "MAR"){
    # add MAR mechanism here
  }
  
  miss.dat <- comp.dat[,1:4]
  miss.dat[which(bin[,1] == 1), 1] <- -99
  miss.dat[which(bin[,2] == 1), 2] <- -99
  miss.dat[which(bin[,3] == 1), 3] <- -99
  miss.dat[which(bin[,4] == 1), 4] <- -99
  
  final.dat <- cbind(miss.dat, comp.dat)  
  
  return(final.dat)
  
}


# generate data using mplus -----------------------------------------------

# 2 factors at this step: 
## 1) sample size: 150, 300, 800, 1600, 5000 
## 2) separation: MD = 0.5 (very poor), MD = 1.0 (poor), MD = 1.5 (moderate), MD = 2.0 (high)

# define sample sizes
ss <- c("150", "800") # sample size condition loop: ss = 150, 300, 800, 1600, 5000
# define separation
sep <- c("mod", "vpoor", "poor", "mod", "high") # separation condition loop: very poor, poor, moderate, high
# define missing mechanism
miss.mech <- c("MCAR") # missing conditions: MAR, MCAR
# define missing percentage
miss.perc <- c("5", "10" , "15", "20", "25")
# define missing data handling methods
methds <- c("fd", "ld", "fi") # missing handling method: fd (full data), ld (listwise delete), fi (full-info ML), mi (multiple imp.), 2m (2step MI), fb (fully Bayesian)
# define repetitions
reps <- 5

# define collection structure
# level 0 collecion (across classes)
df0 <- data.frame("c1" = rep(NA, reps),
                  "c2" = rep(NA, reps),
                  "c3" = rep(NA, reps))

# level 1 collection (across output variables)
df1 <- list(    int = df0,
              intse = df0,
               slop = df0,
             slopse = df0,
               vint = df0,
             vintse = df0,
              vslop = df0,
            vslopse = df0)

df2 <- data.frame(wrnings = rep(NA,reps))

# level 2 collection (across missing handling methods)
resultlist <- list("fd" = df1,
                   "ld" = df1,
                   "fi" = df1)

# level 3 collection (across missing percentage)
resultlist2 <- list("miss5perc" = resultlist,
                   "miss10perc" = resultlist,
                   "miss15perc" = resultlist,
                   "miss20perc" = resultlist,
                   "miss25perc" = resultlist)

# level 4 collection (across missing mechanism)
resultlist3 <- list("MCAR" = resultlist2)#,
                  #missMAR = resultlist2)

#level 5 collection (across separation)
resultlist4 <- list("vpoor" = resultlist3,
                     "poor" = resultlist3,
                      "mod" = resultlist3,
                     "high" = resultlist3)

#level 6 collection (across sample size)
resultlist5 <- list("150" = resultlist4,
                    "800" = resultlist4)

s = "150"
p = "mod"
q = "MCAR"
w = "20"
m = "fd"

resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]]

# LOOP START --------------------------------------------------------------
#r = 1
for (r in 1:reps) {
  
  # read in mplus data generation syntax
  gensyntax <- readLines(paste0(loc,'1_datagen\\datagen.inp'))
  gensyntax[5] <- paste0("SEED = ", r + seed, ";") # seed number
  
#  s <- "800" 
  
  # sample size condition loop: ss = 150, 300, 800, 1600, 5000
  for (s in ss){
    
    gensyntax[3] = paste0('NOBSERVATIONS = ', s, ';')
    
    # separation condition loop: very poor, poor, moderate, high
    for (p in sep){
#      p = "mod"
      if (p == "vpoor"){
        
        
        gensyntax[33] <- "[i*42.530](i3);" 
        gensyntax[57] <- "[i*42.530](i3);"
        
      }
      if (p == "poor"){
        
        gensyntax[33] <- "[i*39.790](i3);" 
        gensyntax[57] <- "[i*39.790](i3);" 
      }
      if (p == "mod"){
        
        gensyntax[33] <- "[i*38.558](i3);"
        gensyntax[57] <- "[i*38.558](i3);"
        
      }
      if (p == "high"){
        
        gensyntax[33] <- "[i*37.6](i3);" 
        gensyntax[57] <- "[i*37.6](i3);" 
        
      }
      
      gensyntax[6] = "SAVE = generated_data.txt;"
      filename = paste0(loc,'1_datagen\\datagen.inp' )
      write(gensyntax, file = filename)
      runModels(paste0(loc,'1_datagen\\datagen.inp'), recursive=FALSE, logFile = NULL)
      
      # create missing data -----------------------------------------------------
      
      # 2 factors at this step:
      ## 1) missing mechanism
      ## 2) percentage of missing
      
      # missing conditions: MAR, MCAR
      for (q in miss.mech) {
#        q = "MCAR"
        
        # missing percentages: 5, 10 ,1, 20, 25
        for ( w in miss.perc) {
#          w = "5"
          
          data.complete <- read.table(paste0(loc,'1_datagen\\generated_data.txt' ))
          mechanism.test <- q
          miss.percentage <- w
        
          temp.dat <- miss.fun(data.complete, q, miss.percentage)
          
          write.table(temp.dat, file = paste0(loc, "2_analysis\\generated_data_miss.txt"), col.names = FALSE, row.names = FALSE)

          
          # analyze -----------------------------------------------------------------
          
          # 1 factor at this step: 
          ## 1) missing handling method: LD (listwise delete), FI (full-info ML), MI (multiple imp.), 2M (2step MI), FB (fully Bayesian)
          
          for( m in methds) {
#          m = "fd"
          
          if (m == "fd") {
            # read in mplus data analysis syntax and modify for full-data analysis
            mplus.filename <- "fd_ld_fi"
            analsyntax <- readLines(paste0(loc,'2_analysis\\', mplus.filename, '.inp'))
            analsyntax[4] <- "!LISTWISE = ON;"
            analsyntax[6] <- "VARIABLE: NAMES ARE y1m-y4m y1-y4 c;"
            
          }
          if (m == "ld") {
            mplus.filename <- "fd_ld_fi"
            analsyntax <- readLines(paste0(loc,'2_analysis\\', mplus.filename, '.inp'))
            analsyntax[4] <- "LISTWISE = ON;"
            analsyntax[6] <- "VARIABLE: NAMES ARE y1-y4 y1m-y4m c;"
            
          }
          if (m == "fi") {
            mplus.filename <- "fd_ld_fi"
            analsyntax <- readLines(paste0(loc,'2_analysis\\', mplus.filename, '.inp'))
            analsyntax[4] <- "!LISTWISE = ON;"
            analsyntax[6] <- "VARIABLE: NAMES ARE y1m-y4m y1-y4 c;"
            
          }
          if (m == "mi") {
            
          }
          if (m == "2m") {
            
            
          }
          if (m == "fb") {
            
            
          }
            
            filename = paste0(loc,'2_analysis\\', mplus.filename, '.inp' )
            write(analsyntax, file = filename)
            runModels(filename, recursive = FALSE, logFile = NULL)
            extract.pars <- readModels(paste0(loc,'2_analysis\\', mplus.filename, '.out'))
            extract.pars <- extract.pars[["parameters"]]
            #############################
            # ALL INTERCEPT INFORMATION #
            #############################
            # s = sample size, p = separation, q = missing mechanism, w = missing percentage, m = method
            # extract intercept means
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["int"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Means"
                                           &extract.pars$unstandardized[ , "param"] == "I" , 3]
            # extract intercept SE
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["intse"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Means"
                                           &extract.pars$unstandardized[ , "param"] == "I" , 4]
            # extract intercept variance
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["vint"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Variances"
                                           &extract.pars$unstandardized[ , "param"] == "I" , 3]
            # extract intercept variance SE
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["vintse"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Variances"
                                           &extract.pars$unstandardized[ , "param"] == "I" , 4]
            #############################
            # ALL SLOPE INFORMATION #
            #############################
            # extract slope means
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["slop"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Means"
                                           &extract.pars$unstandardized[ , "param"] == "S" , 3]
            # extract slope SE
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["slopse"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Means"
                                           &extract.pars$unstandardized[ , "param"] == "S" , 4]
            # extract slope variance
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["vslop"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Variances"
                                           &extract.pars$unstandardized[ , "param"] == "S" , 3]
            # extract slope variance SE
            resultlist5[[s]][[p]][[q]][[paste0("miss", w, "perc")]][[m]][["vslopse"]][r, ] <-
              extract.pars$unstandardized[ extract.pars$unstandardized[ , "paramHeader"] == "Variances"
                                           &extract.pars$unstandardized[ , "param"] == "S" , 4]
            
            
            
          }
          
        }
        
        
      }
      
    }
    
  }
  
}























# collect parameters ------------------------------------------------------

# collect point estimates and standard errors for:
# 1) class 1 intercept mean
# 2) class 1 slope mean
# 3) class 2 intercept mean
# 4) class 2 slope mean
# 5) class 3 intercept mean
# 6) class 3 slope mean
# 7) all classes intercept variance
# 8) all classes slope variance
# 9) all classes I and S variance
# 10) class 1 proportion
# 11) class 2 proportion
# 12) class 3 proportion

# 13) standard error bias









#==================
#
# SAMPLE FUNCTION
#
#==================

SAMPFUN = function(data_school, data_stud){
  #=================
  #
  #  SAMPLE  
  #
  #=================
  #SORT by REGION and PUBLIC
  data_school = data_school[order(data_school$reg,data_school$public),]
  data_stud = data_stud[order(data_stud$reg,data_stud$public),]
  
  #SAMPLE SCHOOLS
  school_samp = strata(data_school, c("reg", "public"), size = rep(10,12), method = "srswor")
  school_samp = school_samp[order(school_samp$reg,school_samp$public,school_samp$ID_unit),]
  
  #GET THE REST OF DATA FOR SAMPLED SCHOOLS
  school_samp <- getdata(data_school, school_samp)
  
  #MERGE SAMPLED SCHOOLS WITH STUDENTS IN THOSE SCHOOLS
  school_samp_studs = merge(school_samp, data_stud, by=c("u_school_id", "reg", "public"))
  school_samp_studs = school_samp_studs[order(school_samp_studs$ellcat,school_samp_studs$u_school_id),]
  
  #SAMPLE STUDENTS BY ELL#
  stud_samp = strata(school_samp_studs, c("ellcat"), size = c(800,800), method = "srswor")
  
  #GET THE REST OF THE DATA FOR SAMPLED STUDENTS
  stud_samp <- getdata(school_samp_studs, stud_samp)
  
  #FINAL SAMPLE  
  final_samp = merge(stud_samp, school_samp_studs, by=c("u_school_id", "reg", "public","school","kid","y1","y2","y3","y4","y5","y6","ell","sc","ses","u_kid_id","scz","ellz","sesz","sccat","sescat","classA2","classA3","classA4","ellcat"))
  
  #CREATE FINAL SAMPLING WEIGHT (BASE WEIGHT)
  final_samp$bweight <- (1/final_samp$Prob.x)*(1/final_samp$Prob.y) 
  
  #SORT ONE LAST TIME BY REGION, SCHOOL, STUDENT
  final_samp = final_samp[order(final_samp$reg,final_samp$u_school_id, final_samp$u_kid_id),]
  
  #DROP UNWANTED VARIABLES
  dr0p = c("ID_unit.x","ID_unit.y", "Prob.x", "Prob.y", "Stratum.x", "school", "kid")
  final_samp = final_samp[ ,!(names(final_samp) %in% dr0p)] 
  
  #SAVE DATA
  #path = "C:\\Users\\yangsup\\Desktop\\temp_survselect_040817\\R_fulldat_weights.dat"
  #write.table(final_samp2, path, col.names = FALSE, row.names = FALSE, na = "-99" )
  
  return(final_samp)
  
}

MISSINGFUN = function(w2c, w3c, w4c, w5c, w6c, final_samp, MC){
  
  #wave 2 missing
  final_samp[,"MC"] <- rnorm(nrow(sampdat), 0, 1)
  final_samp[,"miss_cont2"] <- w2c[1]*final_samp$public + w2c[2]*final_samp$ellz + w2c[3]*final_samp$scz + w2c[4]*final_samp$sesz + final_samp$MC*MC
  
  final_samp[,"miss_cont2"] <- scale(final_samp[,"miss_cont2"])
  final_samp[,"pat6"] <- ifelse(final_samp$"miss_cont2" <= -1.645, 1, 0 )
  final_samp[, c("y1c","y2c","y3c","y4c","y5c","y6c") ] = final_samp[, c("y1","y2","y3","y4","y5","y6") ] #retain the complete datasets
  final_samp[final_samp$"pat6" == 1, c("y2","y3","y4","y5","y6") ] = NA
  
  #wave 3 missing
  final_samp[,"MC"] <- rnorm(nrow(sampdat), 0, 1)
  final_samp[,"miss_cont3"] <- ifelse(final_samp$"pat6" == 0, w3c[1]*final_samp$public + w3c[2]*final_samp$ellz + 
                                        w3c[3]*final_samp$scz + w3c[4]*final_samp$sesz + final_samp$MC*MC, NA )
  final_samp[,"miss_cont3"] <- scale(final_samp[,"miss_cont3"])  
  final_samp[,"pat5"] <- ifelse(final_samp$"miss_cont3" <= -1.003, 1, 0 )
  final_samp[,c("y3","y4","y5","y6")] <- ifelse(rep(final_samp$"pat5" == 1,4), rep(NA,4), 
                                                c(final_samp$"y3",final_samp$"y4",final_samp$"y5",final_samp$"y6") )
  
  #wave 4 missing
  final_samp[,"MC"] <- rnorm(nrow(sampdat), 0, 1)
  final_samp[,"miss_cont4"] <- ifelse( (final_samp$"pat6" == 0 & final_samp$"pat5" == 0), w4c[1]*final_samp$public + w4c[2]*final_samp$ellz + w4c[3]*final_samp$scz + w4c[4]*final_samp$sesz + final_samp$MC*MC, NA )
  
  final_samp[,"miss_cont4"] <- scale(final_samp[,"miss_cont4"])  
  final_samp[,"pat4"] <- ifelse(final_samp$"miss_cont4" <= -.887, 1, 0 )
  final_samp[,c("y4","y5","y6")] <- ifelse(rep(final_samp$"pat4" == 1,3), rep(NA,3), 
                                           c(final_samp$"y4",final_samp$"y5",final_samp$"y6") )
  
  
  #wave 5 missing
  final_samp[,"MC"] <- rnorm(nrow(sampdat), 0, 1)
  final_samp[,"miss_cont5"] <- ifelse( (final_samp$"pat6" == 0 & final_samp$"pat5" == 0 & final_samp$"pat4" == 0), w5c[1]*final_samp$public + w5c[2]*final_samp$ellz + w5c[3]*final_samp$scz + w5c[4]*final_samp$sesz + final_samp$MC*MC, NA )
  
  final_samp[,"miss_cont5"] <- scale(final_samp[,"miss_cont5"])  
  final_samp[,"pat3"] <- ifelse(final_samp$"miss_cont5" <= -.736, 1, 0 )
  final_samp[,c("y5","y6")] <- ifelse(rep(final_samp$"pat3" == 1,2), rep(NA,2), 
                                      c(final_samp$"y5",final_samp$"y6") )
  
  #wave 6 missing 
  final_samp[,"MC"] <- rnorm(nrow(sampdat), 0, 1)
  final_samp[,"miss_cont6"] <- ifelse( (final_samp$"pat6" == 0 & final_samp$"pat5" == 0 & final_samp$"pat4" == 0 & final_samp$"pat3" == 0), w6c[1]*final_samp$public + w6c[2]*final_samp$ellz + 
                                         w6c[3]*final_samp$scz + w6c[4]*final_samp$sesz + final_samp$MC*MC, NA )
  final_samp[,"miss_cont6"] <- scale(final_samp[,"miss_cont6"])  
  final_samp[,"pat2"] <- ifelse(final_samp$"miss_cont6" <= -.525, 1, 0 )
  final_samp[,c("y6")] <- ifelse(rep(final_samp$"pat2" == 1,1), rep(NA,1), 
                                 c(final_samp$"y6") )
  
  final_samp[,"pattern"] <- ifelse(final_samp$pat6 == 1, 6, 
                                   ifelse(final_samp$pat5 == 1, 5, 
                                          ifelse(final_samp$pat4 == 1, 4,
                                                 ifelse(final_samp$pat3 == 1, 3,
                                                        ifelse(final_samp$pat2 == 1, 2, 1)
                                                 )
                                          )
                                   )
                                   
  )
  
  #final data frame
  final_samp2 <- data.frame(
    u_school_id = final_samp$u_school_id, reg = final_samp$reg, public = final_samp$public, 
    y1 = final_samp$y1, y2 = final_samp$y2, y3 =  final_samp$y3, y4 = final_samp$y4, y5 = final_samp$y5, y6 = final_samp$y6, 
    y1c = final_samp$y1c, y2c = final_samp$y2c, y3c = final_samp$y3c, y4c = final_samp$y4c, y5c = final_samp$y5c, y6c = final_samp$y6c, 
    pattern = final_samp$pattern, ell = final_samp$ell, sc = final_samp$sc, 
    ses = final_samp$ses, u_kid_id = final_samp$u_kid_id, ellz = final_samp$ellz, scz = final_samp$scz, sesz = final_samp$sesz, ellcat = final_samp$ellcat, sccat = final_samp$sccat, sescat = final_samp$sescat, 
    classA2 = final_samp$classA2,classA3 = final_samp$classA3, classA4 = final_samp$classA4, Stratum = final_samp$Stratum.y , bweight = final_samp$bweight                
  )
  return(final_samp2)
  
}  

#======================
#
# WEIGHT ADJUSTMENTS
#
#======================

#==================================
#
#  WEIGHTING CLASS APPROACH WEIGHTS
#
#==================================

WCFUN = function(missdat){
  
  a2 = table(missdat$classA2)
  a3 = table(missdat$classA3)
  a4 = table(missdat$classA4)
  patt = table(missdat$pattern)
  
  a2pat = table(missdat$classA2,missdat$pattern)
  a3pat = table(missdat$classA3,missdat$pattern)
  a4pat = table(missdat$classA4,missdat$pattern)
  
  missdat$N2 = NA
  missdat$N3 = NA
  missdat$N4 = NA
  missdat$n2 = NA
  missdat$n3 = NA
  missdat$n4 = NA
  
  for(i in 1:nrow(missdat)){
    for(j in 1:6){
      if(missdat$classA2[i]==j){missdat$N2[i]=a2[j]}		
    }
  }
  
  for(i in 1:nrow(missdat)){
    for(j in 1:18){
      if(missdat$classA3[i]==j){missdat$N3[i]=a3[j]}		
    }
  }
  
  for(i in 1:nrow(missdat)){
    for(j in 1:36){
      if(missdat$classA4[i]==j){missdat$N4[i]=a4[j]}		
    }
  }
  
  
  
  
  for(i in 1:nrow(missdat)){
    for(j in 1:6){
      for(k in 1:6){
        if(missdat$classA2[i]==j & missdat$pattern[i]==k){missdat$n2[i]=a2pat[j,k]}	
      }		
    }
  }
  
  
  for(i in 1:nrow(missdat)){
    for(j in 1:18){
      for(k in 1:6){
        if(missdat$classA3[i]==j & missdat$pattern[i]==k){missdat$n3[i]=a3pat[j,k]}	
      }		
    }
  }
  
  for(i in 1:nrow(missdat)){
    for(j in 1:36){
      for(k in 1:6){
        if(missdat$classA4[i]==j & missdat$pattern[i]==k){missdat$n4[i]=a4pat[j,k]}	
      }		
    }
  }
  
  missdat$adj_w_2 = ifelse(missdat$pattern == 1, round((missdat$N2/missdat$n2)*missdat$bweight,4),0)
  missdat$adj_w_3 = ifelse(missdat$pattern == 1, round((missdat$N3/missdat$n3)*missdat$bweight,4),0)
  missdat$adj_w_4 = ifelse(missdat$pattern == 1, round((missdat$N4/missdat$n4)*missdat$bweight,4),0)
  
  return(missdat)
  
}

#======================================
#
#  RESPONSE PROPENSITY APPROACH WEIGHTS
#
#======================================

RPFUN = function(missdat){
  
  #create a variable of response indicator
  
  for(i in 1:nrow(missdat)){
    
    if( any( is.na( missdat[i,c("y1","y2","y3","y4","y5","y6")] )  )  ) { missdat[i,"resp prop"] = 0 } 
    
    else { missdat[i,"resp prop"] = 1 }  
    
  }
  
  missdat$public = factor(missdat$public)  
  #missdat$ellcat = factor(missdat$ellcat)
  
  #RESPONSE PROPENSITY USING ONLY some AUX VARS. z$sc
  
  lmodel = glm( missdat[ ,"resp prop"] ~ missdat[ ,"scz"], family = binomial(logit)  )
  
  newdata = with( missdat, data.frame( sc = missdat[ ,"scz"]  ) )
  
  missdat[,"prop prediction"] = 1/predict(lmodel, newdata , type = "response" ) 
  
  for(i in 1:nrow(missdat)){
    
    if( missdat[i,"resp prop"] == 1 ) { missdat[i,"prop_weight_3"] = round(missdat[i,"bweight"]*missdat[i,"prop prediction"],3)  }
    
    else
      
    { missdat[i,"prop_weight_3"] = 0 } 
    
  }
  
  #RESPONSE PROPENSITY USING ALL 4 AUX VARS. 
  
  lmodel = glm( missdat[ ,"resp prop"] ~ missdat[ ,"sesz"]+missdat[ ,"scz"]+missdat[ ,"ellz"]+missdat[,"public"], family = binomial(logit)  )
  
  newdata = with( missdat, data.frame( ses = missdat[ ,"sesz"], sc = missdat[ ,"scz"], ellcat = missdat[ ,"ellz"], public = missdat[,"public"]   ) )
  
  missdat[,"prop prediction"] = 1/predict(lmodel, newdata , type = "response" )
  
  
  for(i in 1:nrow(missdat)){
    
    if( missdat[i,"resp prop"] == 1 ) { missdat[i,"prop_weight_4"] = round(missdat[i,"bweight"]*missdat[i,"prop prediction"],3)  }
    
    else
      
    { missdat[i,"prop_weight_4"] = 0 } 
    
  }
  
  return(missdat)
  
}

#=================
#
#  READ IN DATA
#
#=================

#PROPENSITY MISSING COEFFICIENTS
coefflist <- list()
coefflist[["00"]] = matrix(c(0, 0, 0, 0,
                             0, 0, 0, 0,
                             0, 0, 0, 0,
                             0, 0, 0, 0,
                             0, 0, 0, 0), 4, 5, byrow = FALSE)

coefflist[["01"]] = matrix(c(0.25, 0.10, 0.10, 0.10,
                             0.251, 0.101, 0.101, 0.101, 
                             0.253, 0.101, 0.101, 0.101,
                             0.254, 0.102, 0.102, 0.102,
                             0.255, 0.102, 0.102, 0.102), 4, 5, byrow = FALSE)

coefflist[["03"]] = matrix(c(0.750, 0.30, 0.30, 0.30,
                             0.786, 0.314, 0.314, 0.314,
                             0.824, 0.330, 0.330, 0.330,
                             0.864, 0.346, 0.346, 0.346,
                             0.906, 0.362, 0.362, 0.362), 4, 5, byrow = FALSE)

MultConst <- list()
MultConst[["00"]] = 1
MultConst[["01"]] = 0.98
MultConst[["03"]] = 0.8

outaux <- c( "00", "01" ,"03") #list of conditions for correlation between OUTCOME and AUX. variables
#outaux <- c( "03") #list of conditions for correlation between OUTCOME and AUX. variables

missaux <- c("00", "01","03") #list of conditions for correlation between PROP MISSING and AUX. variables
#missaux <- c("01","03") #list of conditions for correlation between PROP MISSING and AUX. variables

models <- c("FIML04","FIML14","FIML34", "FIML44", "RP_34", "RP_44", "WC_24" , "WC_34", "WC_44", "MI3", "MI4", "FULLSAMP" ) #added MI part

reps = 200

df1 = data.frame(int = rep(NA,reps), 
                 intse = rep(NA,reps),
                 slop = rep(NA,reps),
                 slopse = rep(NA,reps),
                 vint = rep(NA,reps),
                 vintse = rep(NA,reps),
                 vslop = rep(NA,reps),
                 vslopse = rep(NA,reps))

df2 = data.frame(wrnings = rep(NA,reps))

resultlist = list(FIML04 = df1, FIML14 = df1, FIML34 = df1, FIML44 = df1, RP_34 = df1, RP_44 = df1, WC_24 = df1,  WC_34 = df1, WC_44 = df1, MI3 = df1, MI4 = df1, FULLSAMP = df1) #added MI part

resultlist2 = list(missaux00 = resultlist, missaux01 = resultlist, missaux03 = resultlist)

resultlist3 = list(outaux00 = resultlist2, outaux01 = resultlist2, outaux03 = resultlist2)

warningList = list(FIML04 = df1, FIML14 = df1, FIML34 = df2, FIML44 = df2, RP_34 = df2, RP_44 = df2, WC_24 = df2, WC_34 = df2, WC_44 = df2, MI3 = df2, MI4 = df2, FULLSAMP = df2) #added MI part

warningList2 = list(missaux00 = warningList, missaux01 = warningList, missaux03 = warningList)

warningList3 = list(outaux00 = warningList2, outaux01 = warningList2, outaux03 = warningList2)


#resultlist3 = list(outaux01 = resultlist2, outaux03 = resultlist2)

#MAIN LOOP STARTS HERE
#MAIN LOOP STARTS HERE
#MAIN LOOP STARTS HERE

for(oo in outaux){
  
  print(paste0(oo,"_","reading pop. data"))
  
  data_school = read.table(paste0("\\\\tsclient\\UMD\\PW\\SIM\\1.PW_datagen\\041417\\superpop",oo,"_school.dat"), header = TRUE)
  data_stud = read.table(paste0("\\\\tsclient\\UMD\\PW\\SIM\\1.PW_datagen\\041417\\superpop",oo,"_stud.dat"), header = TRUE)
  
  for(r in 1:reps){
    
    print(paste0(r,"_","sampling"))  
    
    set.seed(530 + r)
    
    sampdat = SAMPFUN(data_school, data_stud)
    
    for(mi in missaux){
      
      print(paste0(mi,"_","create missing"))
      
      coeffs = coefflist[[mi]]
      Mconst = MultConst[[mi]] 
      
      MIsample = MISSINGFUN(coeffs[,1], coeffs[,2], coeffs[,3], coeffs[,4], coeffs[,5], sampdat, Mconst)
      MIsample = WCFUN(MIsample)
      MIsample = RPFUN(MIsample)
      
      #center for Mplus
      #MIsample$sccat = scale(MIsample$sccat,center=TRUE,scale=FALSE)
      #MIsample$sescat = scale(MIsample$sescat,center=TRUE,scale=FALSE)
      
      missdat = with( MIsample, data.frame( y1 = MIsample[ ,"y1"], y1c = MIsample[ ,"y1c"], 
                                            y2 = MIsample[ ,"y2"], y2c = MIsample[ ,"y2c"],   
                                            y3 = MIsample[ ,"y3"], y3c = MIsample[ ,"y3c"],    
                                            y4 = MIsample[ ,"y4"], y4c = MIsample[ ,"y4c"],    
                                            y5 = MIsample[ ,"y5"], y5c = MIsample[ ,"y5c"],    
                                            y6 = MIsample[ ,"y6"], y6c = MIsample[ ,"y6c"],
                                            reg = MIsample[,"reg"],
                                            ses = MIsample[ ,"sesz"],
                                            sescat = MIsample[,"sescat"],
                                            sc = MIsample[,"scz"],
                                            sccat = MIsample[,"sccat"],
                                            pub = MIsample[ ,"public"],
                                            ell = MIsample[,"ellz"],
                                            ellcat = MIsample[ ,"ellcat"], 
                                            unique_kid_id = MIsample[ ,"u_kid_id"],
                                            unique_school_id = MIsample[,"u_school_id"],
                                            stratum = MIsample[,"Stratum"],
                                            overallw = MIsample[ ,"bweight"], 
                                            adj_w_2 = MIsample[ ,"adj_w_2"],
                                            adj_w_3 = MIsample[ ,"adj_w_3"], 
                                            adj_w_4 = MIsample[ ,"adj_w_4"],
                                            prop_weight_3 = MIsample[ ,"prop_weight_3"],
                                            prop_weight_4 = MIsample[ ,"prop_weight_4"]
                                            
      ) )
      
      altpath = paste0("\\\\tsclient\\UMD\\PW\\SIM\\3.PW_creating missing\\misssamp.dat")
      
      write.table(missdat, file = altpath, col.names = FALSE, row.names = FALSE, sep="\t", na = "-99")
      
      for(mo in models){
        
        print(paste0(mo,"_","modeling"))
        
        fitsyntax = readLines("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp")
        
        if(mo == "FIML04"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = overallw;"
          fitsyntax[16] = " "
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "FIML14"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = overallw;"
          fitsyntax[16] = "auxiliary = sc(m);"
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "FIML34"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = overallw;"
          fitsyntax[16] = "auxiliary = ellcat(m) pub(m) sc(m);"
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "FIML44"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = overallw;"
          fitsyntax[16] = "auxiliary = ellcat(m) pub(m) sccat(m) sescat(m);"
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "RP_34"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = pw_3;"
          fitsyntax[16] = ""
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "RP_44"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = pw_4;"
          fitsyntax[16] = ""
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "WC_24"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = adj_w_2;"
          fitsyntax[16] = ""
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "WC_34"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = adj_w_3;"
          fitsyntax[16] = ""
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        }
        
        if(mo == "WC_44"){
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6;"
          fitsyntax[15] = "weight = adj_w_4;"
          fitsyntax[16] = ""
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\mplus_shell.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing"  )
        } 
        
        if(mo == "MI3"){ #added MI3 part
          
          #READ IN MPLUS IMPUTATION .INP SHELL FILE
          fitsyntax = readLines("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_imputation.inp")
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6"
          fitsyntax[15] = "ellcat pub sccat;"
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_imputation.inp' )
          write(fitsyntax, file = filename)
          #RUN THE IMPUTATIONS
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_imputation.inp", recursive=FALSE, logFile = NULL)
          
          #CREATE A TEMPORARY MATRIX TO COLLECT THE IMPUTATION PARAMETERS
          tempImpCollect = matrix(rep(NA, 8*10), 10, 8)
          
          #RUN THE ANALYTIC MODEL 10 TIMES (USING WEIGHTS THIS TIME)
          for(g in 1:10){ #for the g number of imputations, run the model using weights
            fitsyntax = readLines("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell.inp")
            fitsyntax[7] = paste0("missing\\imputation\\missimp",g,".dat;")
            fitsyntax[11] = "ellcat pub sccat"
            fitsyntax[15] = "weight = overallw;"
            filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_',g,'.inp' )
            write(fitsyntax, file = filename)
            runModels(paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_',g,'.inp'), recursive=FALSE, logFile = NULL)  
            
            extract.parsTEMP <- extractModelParameters( paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_',g,'.out') )
            extract.parsTEMP <- extract.parsTEMP[["unstandardized"]]
            
            tempImpCollect[g,] = c(extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "INT", 3], 
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "INT", 4],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "SLP", 3],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "SLP", 4],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "INT", 3],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "INT", 4],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "SLP", 3],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "SLP", 4]) 
          }
          
          #COMBINE THE RESULTS AS PER RUBIN
          RubMeans = apply(tempImpCollect[,c(1,3,5,7)], 2, mean)
          RubSEs = apply(tempImpCollect[,c(2,4,6,8)], 2, mean ) + (1 + 1/10)*apply(tempImpCollect[,c(1,3,5,7)], 2, var )
          
          allImpComb = c(RubMeans[1], RubSEs[1], RubMeans[2], RubSEs[2], RubMeans[3], RubSEs[3], RubMeans[4], RubSEs[4] )
          
        }
        
        if(mo == "MI4"){ #added MI4 part
          #READ IN MPLUS IMPUTATION .INP SHELL FILE
          fitsyntax = readLines("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_imputation.inp")
          fitsyntax[14] = "usevar = y1 y2 y3 y4 y5 y6"
          fitsyntax[15] = "ellcat pub sccat sescat;"
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_imputation.inp' )
          write(fitsyntax, file = filename)
          #RUN THE IMPUTATIONS
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_imputation.inp", recursive=FALSE, logFile = NULL)
          
          #CREATE A TEMPORARY MATRIX TO COLLECT THE IMPUTATION PARAMETERS
          tempImpCollect = matrix(rep(NA, 8*10), 10, 8)
          
          #RUN THE ANALYTIC MODEL 10 TIMES (USING WEIGHTS THIS TIME)
          for(g in 1:10){ #for the g number of imputations, run the model using weights
            fitsyntax = readLines("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell.inp")
            fitsyntax[7] = paste0("missing\\imputation\\missimp",g,".dat;")
            fitsyntax[11] = "ellcat pub sccat sescat"
            fitsyntax[15] = "weight = overallw;"
            filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_',g,'.inp' )
            write(fitsyntax, file = filename)
            runModels(paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_',g,'.inp'), recursive=FALSE, logFile = NULL)  
            
            extract.parsTEMP <- extractModelParameters( paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing\\imputation\\mplus_shell_',g,'.out') )
            extract.parsTEMP <- extract.parsTEMP[["unstandardized"]]
            
            tempImpCollect[g,] = c(extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "INT", 3], 
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "INT", 4],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "SLP", 3],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Means" & extract.parsTEMP$param == "SLP", 4],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "INT", 3],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "INT", 4],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "SLP", 3],
                                   extract.parsTEMP[extract.parsTEMP$paramHeader == "Variances" & extract.parsTEMP$param == "SLP", 4]) 
          }
          
          #COMBINE THE RESULTS AS PER RUBIN
          RubMeans = apply(tempImpCollect[,c(1,3,5,7)], 2, mean)
          RubSEs = apply(tempImpCollect[,c(2,4,6,8)], 2, mean ) + (1 + 1/10)*apply(tempImpCollect[,c(1,3,5,7)], 2, var )
          
          allImpComb = c(RubMeans[1], RubSEs[1], RubMeans[2], RubSEs[2], RubMeans[3], RubSEs[3], RubMeans[4], RubSEs[4] )
          
        } 
        
        if(mo == "FULLSAMP"){
          fitsyntax = readLines("C:\\Users\\yangsup\\Desktop\\mplusfiles\\complete\\mplus_shell_comp.inp")
          fitsyntax[14] = "usevar = y1c y2c y3c y4c y5c y6c;"
          fitsyntax[15] = "weight = overallw;"
          fitsyntax[16] = ""
          filename = paste0('C:\\Users\\yangsup\\Desktop\\mplusfiles\\complete\\mplus_shell_comp.inp' )
          write(fitsyntax, file = filename)
          runModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\complete", recursive=FALSE, logFile = NULL)
          extract.pars <- extractModelParameters( "C:\\Users\\yangsup\\Desktop\\mplusfiles\\complete")
        }
        
        oa = paste0("outaux",oo)
        ma = paste0("missaux",mi)
        
        #RESULTS FROM THE IMPUTATION HAVE ALREADY BEEN COMBINED
        if(mo == "MI3" | mo == "MI4"){
          
          resultlist3[[oa]][[ma]][[mo]][r,] = allImpComb
          
          #NOTE THAT THIS IS NOT FROM THE CORRECT MPLUS OUTPUT
          WarningsCheck = readModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing")  
          
          if(length(WarningsCheck$warnings) == 0){ warningList3[[oa]][[ma]][[mo]][r,] = "ok" }else{ warningList3[[oa]][[ma]][[mo]][r,] = "WARNINGS" }
          
        }else{
          
          #check if any warnings popped up
          WarningsCheck = readModels("C:\\Users\\yangsup\\Desktop\\mplusfiles\\missing")  
          
          if(length(WarningsCheck$warnings) == 0){ warningList3[[oa]][[ma]][[mo]][r,] = "ok" }else{ warningList3[[oa]][[ma]][[mo]][r,] = "WARNINGS" }
          
          #collect parameters from Mplus output
          extract.pars <- extract.pars[["unstandardized"]]
          
          resultlist3[[oa]][[ma]][[mo]][r,] = 
            c(extract.pars[extract.pars$paramHeader == "Means" & extract.pars$param == "INT", 3], 
              extract.pars[extract.pars$paramHeader == "Means" & extract.pars$param == "INT", 4],
              extract.pars[extract.pars$paramHeader == "Means" & extract.pars$param == "SLP", 3],
              extract.pars[extract.pars$paramHeader == "Means" & extract.pars$param == "SLP", 4],
              extract.pars[extract.pars$paramHeader == "Variances" & extract.pars$param == "INT", 3],
              extract.pars[extract.pars$paramHeader == "Variances" & extract.pars$param == "INT", 4],
              extract.pars[extract.pars$paramHeader == "Variances" & extract.pars$param == "SLP", 3],
              extract.pars[extract.pars$paramHeader == "Variances" & extract.pars$param == "SLP", 4])
        }
      }
      
    }
    
  }
  
}

write.csv(warningList3, file = "\\\\tsclient\\UMD\\PW\\SIM\\6.PW_results\\WarningResults011118.csv", row.names = TRUE, na = "-99")

write.csv(resultlist3, file = "\\\\tsclient\\UMD\\PW\\SIM\\6.PW_results\\AllResults011118.csv", row.names = TRUE, na = "-99")

model.results <- do.call(rbind, resultlist3)

meanResults = list(
  out00miss00 = do.call(rbind, lapply(model.results[[1,1]], function(i) apply(i, 2, mean)) ),
  out00miss01 = do.call(rbind, lapply(model.results[[1,2]], function(i) apply(i, 2, mean)) ),
  out00miss03 = do.call(rbind, lapply(model.results[[1,3]], function(i) apply(i, 2, mean)) ),
  out01miss00 = do.call(rbind, lapply(model.results[[2,1]], function(i) apply(i, 2, mean)) ),
  out01miss01 = do.call(rbind, lapply(model.results[[2,2]], function(i) apply(i, 2, mean)) ),
  out01miss03 = do.call(rbind, lapply(model.results[[2,3]], function(i) apply(i, 2, mean)) ),
  out03miss00 = do.call(rbind, lapply(model.results[[3,1]], function(i) apply(i, 2, mean)) ),
  out03miss01 = do.call(rbind, lapply(model.results[[3,2]], function(i) apply(i, 2, mean)) ),
  out03miss03 = do.call(rbind, lapply(model.results[[3,3]], function(i) apply(i, 2, mean)) )
)

#code to obtain correct standard errors

SEResults =  list(
  out00miss00 = do.call(rbind, lapply(model.results[[1,1]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out00miss01 = do.call(rbind, lapply(model.results[[1,2]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out00miss03 = do.call(rbind, lapply(model.results[[1,3]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out01miss00 = do.call(rbind, lapply(model.results[[2,1]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out01miss01 = do.call(rbind, lapply(model.results[[2,2]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out01miss03 = do.call(rbind, lapply(model.results[[2,3]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out03miss00 = do.call(rbind, lapply(model.results[[3,1]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out03miss01 = do.call(rbind, lapply(model.results[[3,2]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) ),
  out03miss03 = do.call(rbind, lapply(model.results[[3,3]], function(i) apply(i, 2, function(x) sqrt( sum(x^2)/200 ) )) )
) 

SDResults =  list(
  out00miss00 = do.call(rbind, lapply(model.results[[1,1]], function(i) apply(i, 2, sd)) ),
  out00miss01 = do.call(rbind, lapply(model.results[[1,2]], function(i) apply(i, 2, sd)) ),
  out00miss03 = do.call(rbind, lapply(model.results[[1,3]], function(i) apply(i, 2, sd)) ),
  out01miss00 = do.call(rbind, lapply(model.results[[2,1]], function(i) apply(i, 2, sd)) ),
  out01miss01 = do.call(rbind, lapply(model.results[[2,2]], function(i) apply(i, 2, sd)) ),
  out01miss03 = do.call(rbind, lapply(model.results[[2,3]], function(i) apply(i, 2, sd)) ),
  out03miss00 = do.call(rbind, lapply(model.results[[3,1]], function(i) apply(i, 2, sd)) ),
  out03miss01 = do.call(rbind, lapply(model.results[[3,2]], function(i) apply(i, 2, sd)) ),
  out03miss03 = do.call(rbind, lapply(model.results[[3,3]], function(i) apply(i, 2, sd)) )
) 


write.csv(meanResults, "\\\\tsclient\\UMD\\PW\\SIM\\6.PW_results\\meanResults011118.csv"  )
write.csv(SEResults, "\\\\tsclient\\UMD\\PW\\SIM\\6.PW_results\\SEResults011118.csv")
write.csv(SDResults, "\\\\tsclient\\UMD\\PW\\SIM\\6.PW_results\\SDResults011118.csv")
