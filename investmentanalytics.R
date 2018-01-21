#Long-term (2yr) regression data
slopevec1 <- c(0.0333695, -0.00406546, 0.0158256, 0.0023128,1.43616)
interceptvec1 <- c(30.3066,110.9242,81.94224,14.16875,1927.8732)
currentxvec1 <- c(505,505,505,505,505)
currentyvec1 <- c(50.44,108.18,87.54,16.7,2810.3)
rsquaredvec1 <- c(0.9224,0.1765,0.6645,0.1641,0.9599)

#Short-term (1mo) regression data
slopevec2 <- c(0.1745, -0.0263, 0.01705, 0.05293, 5.6619)
interceptvec2 <- c(-38.0758, 121.9158, 79.0279, -9.7213, -69.1478)
currentxvec2 <- c(505, 505, 505, 505, 505)
currentyvec2 <- c(50.44, 108.18, 87.54, 16.7, 2810.3)
rsquaredvec2 <- c(0.901, 0.5428, 0.3115, 0.9037, 0.9065)

#Regression between different asset classes
slopevec3 <- c(0.442635736689587, 1.41770369115159, -0.360812406910161, -0.0896410695525273, 0.0106646909574298, 60.7945835420819,1.47951429021599,0.12796223135922,120.2019567353,0.00182371749757,-60.4279509628,-0.002633581860029,40.7798206853,0.02291224765017,2.4262395042977,0.0655174995129,-0.85374965047400,-0.0662242593610,-0.0896838323429,-0.25761852225269)
interceptvec3 <- c(68.793843510401,-83.102469070993,125.5906397035,117.60123124739,61.5106894018,-2934.073433777,64.11131818272,3.755895423073,517.19589698105,10.5755078192,8931.3932507326,115.9298685202,711.00194567705,-13.748152285657,2.938764695606,12.21508730115,132.55751636805,112.46266589265,24.608781687963,113.69821749935)
currentyvec3 <- c(87.54,50.44,87.54,108.18,87.54,2810.3,87.54,16.7,2810.3,16.7,2810.3,108.18,2810.3,50.44,50.44,16.7,50.44,108.18,16.7,108.18)
currentxvec3 <- c(50.44,87.54,108.18,87.54,2810.3,87.54,16.7,87.54,16.7,2810.3,108.18,2810.3,50.44,2810.3,16.7,50.44,108.18,50.44,108.18,16.7)
rsquaredvec3 <- c(0.6268, 0.6268,0.03042,0.03042,0.6477,0.6477,0.1877,0.1877,0.2177,0.2177,0.1575,0.1575,0.9342,0.9342,0.1573,0.1573,0.05467,0.05467,0.02117,0.02117)

#Function to normalize a vector such that the sum of elements is equal to 1
normalize = function(vec){
  absvec = c()
  for (i in 1:length(vec)){
      absvec[i] = abs(vec[i])
  }
  sum <- sum(absvec)
  new_vec = c()
  for (i in 1:length(vec)){
    new_vec[i] = vec[i]/sum
  }
  return (new_vec) 
}

#Function to calculate the predicted price by plugging in to a linear regression
#formula
current_prediction = function(slope, intercept, currentx){
  prediction <- intercept + slope*currentx
  return(prediction)
}

#Function implementing current_prediction for vectors rather than single values
current_prediction_vec = function(slopevec, interceptvec, currentxvec){
  prediction_vec = c()
  for (i in 1:length(slopevec)){
    prediction_vec[i] = current_prediction(slopevec[i], interceptvec[i], currentxvec[i])
  }
  return(prediction_vec)
}

#Function to calculate the slope of a regression line relative to the values 
#themselves, a ratio-based indicator of change
normalized_slope = function(slope, currenty){
  return(slope/currenty)
}

#Function implementing normalized_slope for vectors rather than single values
normalized_slope_vec = function(slopevec, currentyvec){
  norm_slope_vec = c()
  for (i in 1:length(slopevec)){
    norm_slope_vec[i] = normalized_slope(slopevec[i], currentyvec[i])
  }
  return(norm_slope_vec)
}

#Function to calculate the difference between a regression prediction and
#an actual value
prediction_error = function(slope, intercept, currentx, currenty){
  prediction <- current_prediction(slope, intercept, currentx)
  return ((currenty - prediction)/currenty)
}

#Function implementing prediction_error for vectors rather than single values
prediction_error_vec = function(slopevec, interceptvec, currentxvec, currentyvec){
  pred_error_vec = c()
  for (i in 1:length(slopevec)){
    pred_error_vec[i] = prediction_error(slopevec[i], interceptvec[i], currentxvec[i], currentyvec[i])
  }
  return(pred_error_vec)
}

#Function assessing the strength of a correlation incorporating both the correlation
#coefficient and the ratio of the slope to the actual values
regression_fit = function(rsquared, slope, currenty){
  norm_slope = normalized_slope(slope, currenty)
  return(rsquared * norm_slope)
}

#Function implementing regression_fit for vectors rather than single values
regression_vecfit = function(rsquaredvec, slopevec, currentyvec){
  norm_result = c()
  for (i in 1:length(slopevec)){
    norm_result[i] = regression_fit(rsquaredvec[i], slopevec[i], currentyvec[i])
  }
  return(norm_result)
}

#Function calculating the confidence of expected change based on current deviation
#from the regression line and the strength of the regression. This is predicated
#on a mean-inversion philosophy - that prices will return to the regression line
#in the long term.
regression_scr = function(slopevec, interceptvec, currentxvec, currentyvec, rsquaredvec){
  scr = c()
  for (i in 1:length(slopevec)){
    scr[i] = regression_vecfit(rsquaredvec, slopevec, currentyvec)[i]*prediction_error_vec(slopevec, interceptvec, currentxvec, currentyvec)[i]
  }
  return(normalize(scr))
}

#Function calculating the different amounts by which short-term and long-term regression
#lines should be weighted when predicting prices any number of months into the future.
#The weights are calculated exponentially, which is the most intuitive.
calculate_weights = function(months){
  weights = c()
  weights[1] = 0.8 * 0.75 ^ (months-1)
  weights[2] = 1 - weights[1]
  return (weights)
}

#Function predicting the change in prices over a specified number of months using
#the slope of the regression line
pred_change = function(months, slopevec, currentyvec){
  days_per_month <- 21
  pred_change = c()
  for (i in 1:length(slopevec)){
    pred_change[i] = slopevec[i] * days_per_month * months
  }
  return(pred_change)
}
#print(pred_change(1, slopevec1, currentyvec1))

#Function predicting the change in prices across all asset classes
full_pred_change = function(slopevec, currentyvec){
  return(c(pred_change(1,slopevec,currentyvec), pred_change(2,slopevec,currentyvec), pred_change(6,slopevec,currentyvec), pred_change(12,slopevec,currentyvec), pred_change(24,slopevec,currentyvec)))
}
#print(full_pred_change(slopevec1, currentyvec1))
#print(full_pred_change(slopevec2, currentyvec2))

#Function used in weighted_pred_return to use the appropriate index
find_index = function(index){
  mod_index = index %% 5
  if (mod_index == 0){
    mod_index = 5
  }
  return(mod_index)
}
#print(find_index(10))

#Predicted price change based on regression analysis, factoring in: a) the differing degrees to which 
#short and long term predictions should be used, b) the statistical confidence in each regression and
#c) how much the asset is overvalued or undervalued, based on mean inversion.
#Change is given as a percentage.
weighted_pred_return = function(slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2){
  underestimation_weight = 0.6
  pred_change_1 = full_pred_change(slopevec1, currentyvec1)
  pred_change_2 = full_pred_change(slopevec2, currentyvec2)
  months = c(1,2,6,12,24)
  weighted_pred_change = c()
  for (i in 1:length(pred_change_1)){
      index = (i-1)%/%5 + 1
      weights = calculate_weights(months[index])
      current = currentyvec1[find_index(i)]
      weighted_pred_change = 0.4 * (weights[2] * pred_change_1[i] + weights[1] * pred_change_2[i])
      underestimation = underestimation_weight * weighted_regression_score(months[index], slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2)[find_index(i)]
      weighted_pred_change[i] = (weighted_pred_change + underestimation)/current * 100
      cat("Element", i, ":", weighted_pred_change[i], "\n")
  }
}
#print(weighted_pred_return(slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2))

#Function to calculate the confidence and magnitude of upward/downward prediction of the 
#hybrid short and long term regression using weights
weighted_regression_score = function(month, slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2){
  reg_1 = -1 * regression_scr(slopevec1, interceptvec1, currentxvec1, currentyvec1, rsquaredvec1)
  reg_2 = -1 * regression_scr(slopevec2, interceptvec2, currentxvec2, currentyvec2, rsquaredvec2)
  weights = calculate_weights(month)
  weighted_scrs = c()
  for (i in 1:length(reg_1)){
    weighted_scrs[i] = weights[1]*reg_1[i] + weights[2]*reg_2[i]
  }
  return(normalize(weighted_scrs))
}
#print(weighted_regression_score(12, slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2))

#Function to calculate the average values in an array located at specified indices
index_average = function(indices, array){
  sum = 0
  for (i in 1:length(indices)){
    sum = sum + array[indices[i]]
  }
  return (sum/length(indices))
}
#print(index_average(c(1,3,5),c(2,5,7,7,12)))

#Function to calculate the extent to which an asset is overvalued or undervalued, based on 
#external indications (regression against other asset prices). If an asset tends to fall below
#the regression line then it is determined to be undervalued.
overall_under_over_ext = function(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3){
  EEM = c(2,14,15,17)
  AGG = c(4,12,18,20)
  HYG = c(1,3,5,7)
  GSCI = c(8,10,16,19)
  SP = c(6,9,11,13)
  all_reg_scrs = regression_scr(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3)
  EEM_scr = index_average(EEM, all_reg_scrs)
  AGG_scr = index_average(AGG, all_reg_scrs)
  HYG_scr = index_average(HYG, all_reg_scrs)
  GSCI_scr = index_average(GSCI, all_reg_scrs)
  SP_scr = index_average(SP, all_reg_scrs)
  all_under_over = c(EEM_scr, AGG_scr, HYG_scr, GSCI_scr, SP_scr)
  norm_under_over = normalize(all_under_over)
  return(norm_under_over)
}
#print(overall_under_over_ext(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3))

#Function to calculate the extent to which an asset is overvalued or undervalued, based on 
#internal indications, namely a combination of short-term and long-term regression with weights
overall_under_over_int = function(slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2){
  pred_change_1 = full_pred_change(slopevec1, currentyvec1)
  pred_change_2 = full_pred_change(slopevec2, currentyvec2)
  months = c(1,2,6,12,24)
  all_under_over = c()
  for (i in 1:length(pred_change_1)){
    index = (i-1)%/%5 + 1
    weights = calculate_weights(months[index])
    current = currentyvec1[find_index(i)]
    weighted_pred_change = 0.4 * (weights[1] * pred_change_1[i] + weights[2] * pred_change_2[i])
    underestimation = weighted_regression_score(months[index], slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2)[find_index(i)]
    all_under_over[i] = underestimation/current * 100
  }
  normalize(all_under_over)
  return(0.59737*all_under_over[1:5])
}
#print(overall_under_over_int(slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2))

#Overall confidence in price predictions based on an analysis of all r squared values
r_squared_conf = function(rsquaredvec1, rsquaredvec2, rsquaredvec3){
  EEM = c(2,14,15,17)
  AGG = c(4,12,18,20)
  HYG = c(1,3,5,7)
  GSCI = c(8,10,16,19)
  SP = c(6,9,11,13)
  first_second_avg = c()
  for (i in 1:length(rsquaredvec1)){
    first_second_avg[i] = (rsquaredvec1[i] + rsquaredvec2[i])/2
  }
  third_avg = c(index_average(EEM,rsquaredvec3), index_average(AGG, rsquaredvec3), index_average(HYG, rsquaredvec3), index_average(GSCI, rsquaredvec3), index_average(SP, rsquaredvec3))
  weighted_avg = c()
  for (i in 1:length(third_avg)){
    weighted_avg[i] = 0.6*first_second_avg[i] + 0.4*third_avg[i]
    if (weighted_avg[i] < 0.5){
      weighted_avg[i] = weighted_avg[i] * 1.5
    }
  }
  return(weighted_avg)
}
#print(r_squared_conf(rsquaredvec1, rsquaredvec2, rsquaredvec3))

#Overall confidence in price predictions based on analysis of all deviations of the 
#present prices from the regression line
pred_error_conf = function(slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3){
  EEM = c(2,14,15,17)
  AGG = c(4,12,18,20)
  HYG = c(1,3,5,7)
  GSCI = c(8,10,16,19)
  SP = c(6,9,11,13)
  pred_error_1 = prediction_error_vec(slopevec1, interceptvec1, currentxvec1, currentyvec1)
  pred_error_2 = prediction_error_vec(slopevec2, interceptvec2, currentxvec2, currentyvec2)
  pred_firsttwo_avg = c()
  for (i in 1:length(pred_error_1)){
    pred_firsttwo_avg[i] = (pred_error_1[i] + pred_error_2[i])/2
  }
  predict_errors_third = prediction_error_vec(slopevec3, interceptvec3, currentxvec3, currentyvec3)
  pred_third_avg = c(abs(index_average(EEM,predict_errors_third)), abs(index_average(AGG,predict_errors_third)), abs(index_average(HYG, predict_errors_third)), abs(index_average(GSCI, predict_errors_third)), abs(index_average(SP, predict_errors_third)))
  weighted_avg = c()
  for (i in 1:length(pred_third_avg)){
    weighted_avg[i] = 1 - (0.6*pred_firsttwo_avg[i] + 0.4*pred_third_avg[i])
  }
  return(weighted_avg)
}
#print(pred_error_conf(slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3))

#Overall confidence in price predictions based on the difference between the overvalued/undervalued
#indices given by external and internal indications
over_under_conf = function(slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3){
  ext = overall_under_over_ext(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3)
  int = overall_under_over_int(slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2)
  avg = c()
  for (i in 1:length(ext)){
    avg[i] = 1 - 0.25 * abs(ext[i] - int[i])
  }
  return(avg)
}
#print(over_under_conf(slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3))

#A function aggregating the previous functions into a single measure of confidence in price predictions
#for each asset class
total_conf = function(rsquaredvec1, rsquaredvec2, rsquaredvec3, slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3){
  r_squared_conf = r_squared_conf(rsquaredvec1, rsquaredvec2, rsquaredvec3)
  pred_error_conf = pred_error_conf(slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3)
  over_under_conf = over_under_conf(slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3)
  final_conf = c()
  for (i in 1:length(r_squared_conf)){
    final_conf[i] = 100 * (r_squared_conf[i]*0.2 + pred_error_conf[i]*0.4 + over_under_conf*0.4)
  }
  return(final_conf)
}
#print(total_conf(rsquaredvec1, rsquaredvec2, rsquaredvec3, slopevec1, interceptvec1, currentxvec1, currentyvec1,slopevec2, interceptvec2, currentxvec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3))

#A function calculating upward or downward "momentum" based on long-term regression slope
#and strength of fit
momentum_first = function(rsquaredvec1, slopevec1, currentyvec1){
  first_fit = regression_vecfit(rsquaredvec1, slopevec1, currentyvec1)
  norm_first_fit = normalize(first_fit)
  return(norm_first_fit)
}
#print(momentum_first(rsquaredvec1, slopevec1, currentyvec1))

#A function calculating upward or downward "momentum" based on short-term regression slope
#and strength of fit
momentum_second = function(rsquaredvec2, slopevec2, currentyvec2){
  second_fit = regression_vecfit(rsquaredvec2, slopevec2, currentyvec2)
  norm_second_fit = normalize(second_fit)
  return(norm_second_fit)
}
#print(momentum_second(rsquaredvec2, slopevec2, currentyvec2))

#A function calculating upward or downward "momentum" based on regressions against other
#asset classes. Additional considerations may be incorporated here.
momentum_third = function(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3){
  EEM = c(2,14,15,17)
  AGG = c(4,12,18,20)
  HYG = c(1,3,5,7)
  GSCI = c(8,10,16,19)
  SP = c(6,9,11,13)
  all_fits = regression_vecfit(rsquaredvec3, slopevec3, currentyvec3)
  avg_fits = c(index_average(EEM, all_fits), index_average(AGG, all_fits), index_average(HYG, all_fits), index_average(GSCI, all_fits), index_average(SP, all_fits))
  norm_avg_fits = normalize(avg_fits)
  return(norm_avg_fits)
}
#print(momentum_third(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3))

#A function calculating long-term "momentum" based on a weighted combination of
#long-term regression and external asset class regression
long_term_momentum = function(rsquaredvec1, slopevec1, currentyvec1,slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3){
  first_weight <- 0.7
  third_weight <- 0.3
  first = momentum_first(rsquaredvec1, slopevec1, currentyvec1)
  third = momentum_third(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3)
  short_term_momentum = c()
  for (i in 1:length(third)){
    short_term_momentum[i] = first[i]*first_weight + third[i]*third_weight
  }
  norm_short_term_momentum = normalize(short_term_momentum)
  return(norm_short_term_momentum)
}
#print(long_term_momentum(rsquaredvec1, slopevec1, currentyvec1,slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3))

#A function calculating short-term "momentum" based on a weighted combination of short-term regression
#and external asset class regression
short_term_momentum = function(rsquaredvec2, slopevec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3){
  second_weight <- 0.55
  third_weight <- 0.45
  second = momentum_second(rsquaredvec2, slopevec2, currentyvec2)
  third = momentum_third(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3)
  long_term_momentum = c()
  for (i in 1:length(third)){
    long_term_momentum[i] = second[i]*second_weight + third[i]*third_weight
  }
  norm_long_term_momentum = normalize(long_term_momentum)
  return(norm_long_term_momentum)
}
#print(short_term_momentum(rsquaredvec2, slopevec2, currentyvec2,slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3))

#A function calculating overall "momentum" based on a weighted combination of short-term, long-term, and
#external asset class regression
overall_momentum = function(rsquaredvec1, slopevec1, currentyvec1, rsquaredvec2, slopevec2, currentyvec2, slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3){
  first_two_weight <- 0.6
  third_weight <- 0.4
  first_two = momentum_firsttwo(rsquaredvec1, slopevec1, currentyvec1, rsquaredvec2, slopevec2, currentyvec2)
  third = momentum_third(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3)
  overall_momentum = c()
  for (i in 1:length(third)){
    overall_momentum[i] = first_two[i] * first_two_weight + third[i] * third_weight
  }
  return(overall_momentum)
}
#print(overall_momentum(rsquaredvec1, slopevec1, currentyvec1, rsquaredvec2, slopevec2, currentyvec2, slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3))

#The below will print detailed statistics for each set of regression data. 1 corresponds to 
#long-term (2yr), 2 to short-term (1mo), and 3 to external asset class regression.
# print("Regression 1 Key Statistics:")
# print("Prediction errors: ")
#print(prediction_error_vec(slopevec1, interceptvec1, currentxvec1, currentyvec1))
# print("Regression fits: ")
# print(regression_vecfit(rsquaredvec1, slopevec1, currentyvec1))
# print("Overall regression scr: ")
# print(regression_scr(slopevec1, interceptvec1, currentxvec1, currentyvec1, rsquaredvec1))

# print("Regression 2 Key Statistics:")
# print("Prediction errors: ")
#print(prediction_error_vec(slopevec2, interceptvec2, currentxvec2, currentyvec2))
# print("Regression fits: ")
# print(regression_vecfit(rsquaredvec2, slopevec2, currentyvec2))
# print("Overall regression scr: ")
# print(regression_scr(slopevec2, interceptvec2, currentxvec2, currentyvec2, rsquaredvec2))

# print("Predicted Returns:")
# weighted_pred_return(slopevec1, interceptvec1, currentxvec1, currentyvec1, slopevec2, interceptvec2, currentxvec2, currentyvec2)
# 
# print("Regression 3 Key Statistics:")
# print("Prediction errors: ")
#print(prediction_error_vec(slopevec3, interceptvec3, currentxvec3, currentyvec3))
# print("Regression fits: ")
# print(regression_vecfit(rsquaredvec3, slopevec3, currentyvec3))
# print("Overall regression scr: ")
# print(regression_scr(slopevec3, interceptvec3, currentxvec3, currentyvec3, rsquaredvec3))