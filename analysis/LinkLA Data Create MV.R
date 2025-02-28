library(dplyr)
# 
setwd("~/Documents/Research/LOXZIP")

dat <- read.csv(file="LINK LA Cost Analysis.csv", header=T, na.strings = "N")

#dat<-dat[!is.na(dat$prcarevisit),]

#y<- dat$prcarevisit

linkla <- dat |> mutate(visit = as.numeric(time4)+1) |>
  mutate(day_prev_visit = case_when(
    visit == 1 ~ -365,
    visit == 2 ~ 0,
    visit == 3 ~ days_bs_fu1,
    visit == 4 ~days_bs_fu2
  )) |>
  mutate(day = case_when(
    visit == 1 ~ 0,
    visit == 2 ~ days_bs_fu1,
    visit == 3 ~ days_bs_fu2,
    visit == 4 ~days_bs_fu3
  )) |>
  mutate(deltat = day - day_prev_visit) |>
  select(id, studyarm, visit, day, day_prev_visit, prcarevisit, spcarevisit, asuma1a) |> 
  filter(prcarevisit/((day - day_prev_visit)/30) < 2) |> # Remove observations with more than 2 visits/month 
  na.omit()

# Analysis

library("splines")
library("rstan")
library("splines2")

knots = c(90, 180, 365)

# Spline design matrix
bsMat <- bSpline(linkla$day, knots = knots, degree = 3, intercept = FALSE)

#Spline integrals
ibsMat <- ibs(linkla$day, knots = knots, degree = 3, intercept = FALSE)

ibs_Mat_prev_visit <-  ibs(linkla$day_prev_visit, knots = knots, degree = 3, intercept = FALSE)

plot(bsMat, mark_knots = "internal")
plot(ibsMat, mark_knots = "internal")
plot(ibs_Mat_prev_visit, mark_knots = "internal")

num_data <- length(X); num_basis <- nrow(B)



