# Use a different workspace for each script used in this simulation study.


library(foreach)
library(lavaan)
library(doParallel)
registerDoParallel(cores=7)
library(psych)

#Four factor Model 1:
fourFat5.cor.0 <- "f1 =~ u1 + u2 + u3 + u4 + u5 
f2 =~ u6 + u7 + u8 + u9 + u10 
f3 =~ u11 + u12 + u13 + u14 + u15
f4 =~ u16 + u17 + u18 + u19 + u20
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1"

#Four factor Model 2
fourFat5.cor.2 <- "f1 =~ u1 + u2 + u3 + u4 + u5 
f2 =~ u6 + u7 + u8 + u9 + u10 
f3 =~ u11 + u12 + u13 + u14 + u15
f4 =~ u16 + u17 + u18 + u19 + u20
f1 ~~ 0.2*f2
f1 ~~ 0.2*f3
f1 ~~ 0.2*f4
f2 ~~ 0.2*f3
f3 ~~ 0.2*f4
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1"

#Four factor Model 3
fourFat5.cor.5 <- "f1 =~ u1 + u2 + u3 + u4 + u5 
f2 =~ u6 + u7 + u8 + u9 + u10 
f3 =~ u11 + u12 + u13 + u14 + u15
f4 =~ u16 + u17 + u18 + u19 + u20
f1 ~~ 0.5*f2
f1 ~~ 0.5*f3
f1 ~~ 0.5*f4
f2 ~~ 0.5*f3
f3 ~~ 0.5*f4
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1"

#Four factor Model 4
fourFat5.cor.7 <- "f1 =~ u1 + u2 + u3 + u4 + u5 
f2 =~ u6 + u7 + u8 + u9 + u10 
f3 =~ u11 + u12 + u13 + u14 + u15
f4 =~ u16 + u17 + u18 + u19 + u20
f1 ~~ 0.7*f2
f1 ~~ 0.7*f3
f1 ~~ 0.7*f4
f2 ~~ 0.7*f3
f3 ~~ 0.7*f4
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1"



#Four factor Model 5:
fourFat10.cor.0 <- "f1 =~ u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10
f2 =~ u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + u20 
f3 =~ u21 + u22 + u23 + u24 + u25 + u26 + u27 + u28 + u29 + u30
f4 =~ u31 + u32 + u33 + u34 + u35 + u36 + u37 + u38 + u39 + u40
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1
u21	| 	0*t1
u22	| 	0*t1
u23	| 	0*t1
u24	| 	0*t1
u25	| 	0*t1
u26	| 	0*t1
u27	| 	0*t1
u28	| 	0*t1
u29	| 	0*t1
u30	| 	0*t1
u31	| 	0*t1
u32	| 	0*t1
u33	| 	0*t1
u34	| 	0*t1
u35	| 	0*t1
u36	| 	0*t1
u37	| 	0*t1
u38	| 	0*t1
u39	| 	0*t1
u40	| 	0*t1"

#four factor Model 6
fourFat10.cor.2 <- "f1 =~ u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10
f2 =~ u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + u20 
f3 =~ u21 + u22 + u23 + u24 + u25 + u26 + u27 + u28 + u29 + u30
f4 =~ u31 + u32 + u33 + u34 + u35 + u36 + u37 + u38 + u39 + u40
f1 ~~ 0.2*f2
f1 ~~ 0.2*f3
f1 ~~ 0.2*f4
f2 ~~ 0.2*f3
f2 ~~ 0.2*f4
f3 ~~ 0.2*f4
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1
u21	| 	0*t1
u22	| 	0*t1
u23	| 	0*t1
u24	| 	0*t1
u25	| 	0*t1
u26	| 	0*t1
u27	| 	0*t1
u28	| 	0*t1
u29	| 	0*t1
u30	| 	0*t1
u31	| 	0*t1
u32	| 	0*t1
u33	| 	0*t1
u34	| 	0*t1
u35	| 	0*t1
u36	| 	0*t1
u37	| 	0*t1
u38	| 	0*t1
u39	| 	0*t1
u40	| 	0*t1"


#four factor Model 7
fourFat10.cor.5 <- "f1 =~ u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10
f2 =~ u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + u20 
f3 =~ u21 + u22 + u23 + u24 + u25 + u26 + u27 + u28 + u29 + u30
f4 =~ u31 + u32 + u33 + u34 + u35 + u36 + u37 + u38 + u39 + u40
f1 ~~ 0.5*f2
f1 ~~ 0.5*f3
f1 ~~ 0.5*f4
f2 ~~ 0.5*f3
f2 ~~ 0.5*f4
f3 ~~ 0.5*f4
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1
u21	| 	0*t1
u22	| 	0*t1
u23	| 	0*t1
u24	| 	0*t1
u25	| 	0*t1
u26	| 	0*t1
u27	| 	0*t1
u28	| 	0*t1
u29	| 	0*t1
u30	| 	0*t1
u31	| 	0*t1
u32	| 	0*t1
u33	| 	0*t1
u34	| 	0*t1
u35	| 	0*t1
u36	| 	0*t1
u37	| 	0*t1
u38	| 	0*t1
u39	| 	0*t1
u40	| 	0*t1"

#four factor Model 8
fourFat10.cor.7 <- "f1 =~ u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10
f2 =~ u11 + u12 + u13 + u14 + u15 + u16 + u17 + u18 + u19 + u20 
f3 =~ u21 + u22 + u23 + u24 + u25 + u26 + u27 + u28 + u29 + u30
f4 =~ u31 + u32 + u33 + u34 + u35 + u36 + u37 + u38 + u39 + u40
f1 ~~ 0.7*f2
f1 ~~ 0.7*f3
f1 ~~ 0.7*f4
f2 ~~ 0.7*f3
f2 ~~ 0.7*f4
f3 ~~ 0.7*f4
u1 	| 	0*t1
u2	| 	0*t1
u3	| 	0*t1
u4	| 	0*t1
u5	| 	0*t1
u6	| 	0*t1
u7	| 	0*t1
u8	| 	0*t1
u9	| 	0*t1
u10	| 	0*t1
u11	| 	0*t1
u12	| 	0*t1
u13	| 	0*t1
u14	| 	0*t1
u15	| 	0*t1
u16	| 	0*t1
u17	| 	0*t1
u18	| 	0*t1
u19	| 	0*t1
u20	| 	0*t1
u21	| 	0*t1
u22	| 	0*t1
u23	| 	0*t1
u24	| 	0*t1
u25	| 	0*t1
u26	| 	0*t1
u27	| 	0*t1
u28	| 	0*t1
u29	| 	0*t1
u30	| 	0*t1
u31	| 	0*t1
u32	| 	0*t1
u33	| 	0*t1
u34	| 	0*t1
u35	| 	0*t1
u36	| 	0*t1
u37	| 	0*t1
u38	| 	0*t1
u39	| 	0*t1
u40	| 	0*t1"


mod4 <- c(fourFat5.cor.0, fourFat5.cor.2, fourFat5.cor.5,fourFat5.cor.7,
          fourFat10.cor.0, fourFat10.cor.2, fourFat10.cor.5,fourFat10.cor.7)

#creating the datasets

#4 factors:

## Sample Size = 100
seeds4.100<-list(NULL)
data4.100 <- list(NULL)
iter<-400
for(i in 1:iter) {
  print(paste0("iter:", i))
  seeds4.100[[i]] <-.Random.seed
  data4.100[[i]] <- foreach(b = 1:8) %dopar% (lavaan::simulateData(model = mod4[b], sample.nobs= 100,  model.type = "cfa",  return.type = "data.frame", return.fit = FALSE, standardized = TRUE))
}



## Sample Size = 500
seeds4.500<-list(NULL)
data4.500 <- list(NULL)
for(i in 1:iter) {
  seeds4.500[[i]]<-.Random.seed
  data4.500[[i]] <-  foreach(b = 1:8) %dopar% (lavaan::simulateData(model = mod4[b], sample.nobs= 500, model.type = "cfa",  return.type = "data.frame", return.fit = FALSE, standardized = TRUE))
}



## Sample Size = 1000
seeds4.1000<-list(NULL)
data4.1000 <- list(NULL)
for(i in 1:iter) {
  seeds4.1000[[i]]<-.Random.seed
  data4.1000[[i]] <- foreach(b = 1:8) %dopar% (lavaan::simulateData(model = mod4[b], sample.nobs= 1000, model.type = "cfa",  return.type = "data.frame", return.fit = FALSE, standardized = TRUE))
}


## Sample Size = 5000
seeds4.5000.1<-list(NULL)
data4.5000.1 <- list(NULL)
for(i in 1:100) {
  seeds4.5000.1[[i]]<-.Random.seed
  data4.5000.1[[i]] <- foreach(b = 1:8) %dopar% (lavaan::simulateData(model = mod4[b], sample.nobs= 5000, model.type = "cfa",  return.type = "data.frame", return.fit = FALSE, standardized = TRUE))
}



seeds4.5000.2<-list(NULL)
data4.5000.2 <- list(NULL)

for(i in 101:200) {
  seeds4.5000.2[[i]]<-.Random.seed
  data4.5000.2[[i]] <- foreach(b = 1:8) %dopar% (lavaan::simulateData(model = mod4[b], sample.nobs= 5000, model.type = "cfa",  return.type = "data.frame", return.fit = FALSE, standardized = TRUE))
}




seeds4.5000.3<-list(NULL)
data4.5000.3 <- list(NULL)

for(i in 201:300) {
  seeds4.5000.3[[i]]<-.Random.seed
  data4.5000.3[[i]] <- foreach(b = 1:8) %dopar% (lavaan::simulateData(model = mod4[b], sample.nobs= 5000, model.type = "cfa",  return.type = "data.frame", return.fit = FALSE, standardized = TRUE))
}


seeds4.5000.4<-list(NULL)
data4.5000.4 <- list(NULL)

for(i in 301:400) {
  seeds4.5000.4[[i]]<-.Random.seed
  data4.5000.4[[i]] <- foreach(b = 1:8) %dopar% (lavaan::simulateData(model = mod4[b], sample.nobs= 5000, model.type = "cfa",  return.type = "data.frame", return.fit = FALSE, standardized = TRUE))
}

