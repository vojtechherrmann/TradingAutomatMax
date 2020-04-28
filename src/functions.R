convertSignalToInt <- function(data) {
  return(
    data %>%
      mutate(
        signal = case_when(
          signal == "Buy" ~ 1,
          signal == "Sell" ~ -1
        )
      )
  )
}

convertStatusToStr <- function(data) {
  return(
    data %>%
      mutate(
        status = case_when(
          status == 1 ~ "Buy",
          status == -1 ~ "Sell",
          status == 2 ~ "SL",
          status == -2 ~ "TP",
          TRUE ~ ""
        )
      )
  )
}

replaceNaNWithNA <- function(e) {
  if (is.nan(e)) {
    return(NA)
  } else {
    return(e)
  }
}

cppMakeScenariosStr <- "
    List MakeScenarios(
      NumericVector id,
      NumericVector close_price_1,
      NumericVector close_price_2,
      NumericVector signal,
      double stop_loss,
      double take_profit,
      double pip_value_stock_1,
      double pip_value_stock_2
    ) {
    
      int n = id.size();
    
      // output vectors
      NumericVector buy_price(n, R_NaN);
      NumericVector sell_price(n, R_NaN);
      NumericVector mtm(n, R_NaN);
      NumericVector status(n, R_NaN);
      
      // input for step calculations
      double prev_buy_price = R_NaN;
      double prev_sell_price = R_NaN;
      double prev_status = R_NaN;
      double current_close_price_1 = R_NaN;
      double current_close_price_2 = R_NaN;
      double current_signal = R_NaN;
      
      // calculations
      double calc_mtm = R_NaN;
      double calc_buy_price = R_NaN;
      double calc_sell_price = R_NaN;
      double calc_status = R_NaN;
      
      // for cycle over rows in df
      for (int i = 0; i < n; ++i) {
      
        // previous values needed for calculations
        if (i > 0) {
          prev_buy_price = buy_price[i-1];
          prev_sell_price = sell_price[i-1];
          prev_status = status[i-1];
        }
        // current values needed for calculations
        current_close_price_1 = close_price_1[i];
        current_close_price_2 = close_price_2[i];
        current_signal = signal[i];
        
        // all calculations done only if both current prices are not Null
        if (
            !(
              NumericVector::is_na(current_close_price_1) 
              || NumericVector::is_na(current_close_price_2)
            )
        ) {
        
          // mtm
          if (prev_status == 1) {
            calc_mtm = (
              ((prev_sell_price-current_close_price_2)*pip_value_stock_2)
              +((current_close_price_1-prev_buy_price)*pip_value_stock_1)
            );
          } else if (prev_status == -1) {
            calc_mtm = (
              ((prev_sell_price-current_close_price_1)*pip_value_stock_1)
              +((current_close_price_2-prev_buy_price)*pip_value_stock_2)
            );
          } else {
            calc_mtm = R_NaN;
          }
          
          // status
          if (
              (NumericVector::is_na(prev_status))
              || (prev_status == -2) 
              || (prev_status == 2)
          ) {
            calc_status = current_signal;
          } else if (!(NumericVector::is_na(calc_mtm))) {
            if (calc_mtm < stop_loss) {
              calc_status = 2;
            } else if (calc_mtm > take_profit) {
              calc_status = -2;
            } else {  
              calc_status = prev_status;
            }
          } else {
            calc_status = R_NaN;
          }
                    
          // buy price, sell price
          if (
              (
                (NumericVector::is_na(prev_status))
                && (NumericVector::is_na(calc_status))
              ) 
              || (
                prev_status == calc_status
              )
          ) {
            calc_buy_price = prev_buy_price;
            calc_sell_price = prev_sell_price;
          } else if ((calc_status == 2) || (calc_status == -2)) {
            calc_buy_price = R_NaN;
            calc_sell_price = R_NaN;
          } else if (current_signal == 1) {
            calc_buy_price = current_close_price_1;
            calc_sell_price = current_close_price_2;
          } else if (current_signal == -1) {
            calc_buy_price = current_close_price_2;
            calc_sell_price = current_close_price_1;
          } else {
            calc_buy_price = R_NaN;
            calc_sell_price = R_NaN;
          }
          
          // assigning to output vectors
          buy_price[i] = calc_buy_price;
          sell_price[i] = calc_sell_price;
          mtm[i] = calc_mtm;
          status[i] = calc_status;
          
        }
         
      }
      
      List ret;
      ret[\"buy_price\"] = buy_price;
      ret[\"sell_price\"] = sell_price;
      ret[\"mtm\"] = mtm;
      ret[\"status\"] = status;
    
      return ret;
  
    }
  "
cppMakeScenarios = cppFunction(cppMakeScenariosStr)

make_scenarios <- function(
  data,
  stop_loss,
  take_profit,
  pip_value_stock_1,
  pip_value_stock_2
) {

  # converting strings to int
  cppInputData = data %>%
    convertSignalToInt()  
  # cpp function application
  cppMakeScenariosResults <- 
    cppMakeScenarios(
      cppInputData$id,
      cppInputData$close_price_1,
      cppInputData$close_price_2,
      cppInputData$signal,
      stop_loss,
      take_profit,
      pip_value_stock_1,
      pip_value_stock_2
    )
  
  # assigning result to data
  data_result = data
  for (output_vector_name in names(cppMakeScenariosResults)) {
    output_vector = cppMakeScenariosResults[[output_vector_name]]
    if (length(output_vector) != nrow(data_result)) {
      stop("Length mismatch.")
    }
    data_result[[output_vector_name]] = 
      sapply(FUN = replaceNaNWithNA, output_vector)
  }
  
  return (
    # converting int to strings
    data_result %>%
      convertStatusToStr()
  )

}
