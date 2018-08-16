#' Compute the undiscounted price of an option with Black76 model.
#'
#' /code{compOptionPrice} computes the price of an option on futures. The price is not discounted.
#'
#' @param option_type type of the option, either a call or a put as 'C' or 'P'.
#' @param future price of the future contract.
#' @param strike strike of the option.
#' @param vol implied volatility.
#' @param time_to_expiry remaining life of the option, in fraction of a year.
#' @return The undiscounted price of a \code{option_type} of strike \code{strike} and option life of \code{time_to_expiry}.
#' @examples
#' df <- data.frame(strike = c(50, 20), # the option strike - in $
#'                  type = c("C", "P"), # either “c” for call option or “p” for a put option
#'                  futurePrice = c(48.03, 48.03), # the price of the underlying future - in $
#'                  vol = c(0.08, 0.30) # the implied volatility of the underlying future
#'                  time_to_expiry = c(0.1423, 0.1423))
#'
#'compOptionPrice(df)
#'@export compOptionPrice
#'
compOptionPrice <- function(option_type, future, strike, vol,time_to_exp){
  # check inputs
  possible_types <- c('C','P')
  if (!(toupper(option_type) %in%  possible_types)) {
    stop('Option types must be C or P. Got ', option_type)
  }

  # compute option price
  mult <- ifelse(toupper(option_type)=='C', 1, -1)
  d1 <- (log(future/strike) + (vol/2)*time_to_exp) / (vol * sqrt(time_to_exp))
  d2 <- d1 - vol * sqrt(time_to_exp)
  return (mult * (future * pnorm(mult * d1) - strike * pnorm(mult * d2)))
}


#' Compute the undiscounted delta of an option.
#'
#' @param option_type type of the option, either a call or a put. Use 'c' for a call and 'p' for a  put.
#' @param future price of the future contract.
#' @param strike strike of the option.
#' @param vol implied volatility.
#' @param time_to_expiry remaining life of the option, in fraction of a year.
#' @return The undiscounted delta of a \code{option_type} of strike \code{strike} and option life of \code{time_to_expiry}.
#' @examples
#' df <- data.frame(strike = c(50, 20), # the option strike - in $
#'                  type = c("C", "P"), # either “c” for call option or “p” for a put option
#'                  futurePrice = c(48.03, 48.03), # the price of the underlying future - in $
#'                  vol = c(0.08, 0.30) # the implied volatility of the underlying future
#'                  time_to_expiry = c(0.1423, 0.1423))
#'
#'compDelta(df)
#'@export compDelta
#'
compDelta <- function(option_type, future, strike, vol,time_to_exp){
  # check inputs
  # compute option delta
  mult <- ifelse(toupper(option_type)=='C', 1, -1)
  d1 <- (log(future/strike) + (vol/2)*time_to_exp) / (vol * sqrt(time_to_exp))
  return (mult * pnorm(mult * d1))
}


#' Computes and plots the implied volatility of a set of options.
#'
#' The implied vol is derived from Black76 model. As implied volatility is derived from observed market prices,
#' it is assumed that there is always an implied volatility figure that corresponds to the quoted prices,
#' provided all inputs are filled in correctly.
#' The user can choose to plot the implied volatility curve or just to return the computed ivol results.
#' The user can elect to see ivol curves by strike, moneyness or option delta. Finally, the user can also
#' elect to see both calls and puts, or to see only calls or only puts.
#'
#' @param df data.frame containing all necessary inputs to derive implied volatility.
#' Columns of the data.frame should  be:
#' strike: Strike price
#' type: Option type, either call or put, entered as character 'C' or 'P'.
#' option price: The market price of the option.
#' future price: The price of the underlying future contract.
#' time to expiry: The remaining life time of the option, expressed as a fraction of a year.
#' @param plotting (optional, default=TRUE) Allows the user to switch off plotting and only get implied volatilities back.
#' @param type (optional, default='both') Separates calls and puts. Takes one of values 'both', 'call', 'put'.
#' @param style (optional, default='strike') Allows the user to choose between strike, moneyness and delta as plot's x-axis.
#' @return The computed implied volatilities of a set of options. The ivol curves are also plotted by expiry.
#' if an error is encountered during computation, the implied volatility is set to zero and the point is excluded from the plot.
#' @examples
#'df <- data.frame(strike = c(50, 20), # the option strike - in $
#'                 type = c("C", "P"), # either “c” for call option or “p” for a put option
#'                 optionPrice = c(1.62,0.01), # the option price - in $
#'                 futurePrice = c(48.03, 48.03), # the price of the underlying future - in $
#'                 time_to_expiry = c(0.1423, 0.1423))
#'Default case:
#'plotImpliedVol(df)
#'
#'Get only implied volatility back:
#'plotImpliedVol(df, plotting=FALSE)
#'
#'Plot calls only, by delta:
#'plotImpliedVol(df, type='call', style='delta')
#'
#'Plot puts on by strike:
#'plotImpliedVol(df, type='put')
#'
#' @import ggplot2
#' @export plotImpliedVol
#'
plotImpliedVol <- function(df, plotting, type, style) {
  # check inputs
  if (length(dim(df)) != 2) {
    stop('The input should be a matrix or data.frame. Got ', typeof(a))
  }

  if (dim(df)[2] <= 4) {
    stop('There are missing inputs in the data.frame. \n',
         'Got ', paste(names(df), sep=" "), ' Expect at least strike,
         type, option price, future price, time to expiry')
  }
  df = as.data.frame(df) # make sure df in a data.frame
  # rename data.frame labels
  colnames(df) <- c(
    'strike',
    'type',
    'optionPrice',
    'futurePrice',
    'time_to_expiry'
  )

  # optional arguments handling
  # plotting, invalid argument default to TRUE
  if(missing(plotting) || plotting!=FALSE) {
    plotting <- TRUE
  }

  # plot style,
  if(missing(style)) {
    style <- 'strike'
  } else {
    if (!(tolower(style) %in% c('moneyness', 'delta', 'strike'))){
      stop('Style can be strike, moneyness or delta. Got ', style)
    }
    style <- tolower(style)
  }

  # option type to plot
  if(missing(type) || !(tolower(type) %in% c('both', 'call', 'put'))){
    type <- 'both'
  } else {
    if (!(tolower(type) %in% c('both', 'call', 'put'))){
      stop('Type can be both, call or put. Got ', type)
    }
    type <- tolower(type)
  }

  # input error checking
  # replace any non numerical values in numerical columns by NAs
  df[c(1,3:5)] = sapply(df[c(1,3:5)],function(x) as.numeric(as.character(x))) # force non numeric data
                                                                              #in numeric columns to NAs
  # replace type by upper case
  df[2] = lapply(df[2], function(x) toupper(as.character(x)))

  # check missing values within the data.frame
  if (sum(is.na.data.frame(df))!=0){
    # report missing values
    warning('There are missing values  in the input. The rows below have been omitted: \n',
            paste0(names(df),' ', df[!complete.cases(df),],  collapse='\n'), '\n')
    df = df[complete.cases(df),]
  }

  # compute implied vol
  res <- rep(0, nrow(df))
  excl <- vector(mode='numeric', length=0) # exclude potential errors
  for (i in seq(length=nrow(df))) {
    res[i] <- tryCatch(
      # try
      {
        uniroot(function(x) {df$optionPrice[i] - compOptionPrice(df$type[i],
                                                                 df$futurePrice[i],
                                                                 df$strike[i],
                                                                 x,
                                                                 df$time_to_expiry[i])
        }, lower=1e-6, upper=10.0)$root
      },
      #catch
      error=function(x){
        message('No value found for row ', i, '. Value has been set to zero.')
        return(0)
      }
    )
    # add error to exclude list
    if (res[i]==0){
      excl <- c(excl, i)
    }
  }
  df$ivol <- res

  if (plotting){
    # series to plot
    if (type=='call'){
      df <- df[df$type=='C',]
    } else if (type=='put') {
      df <- df[df$type=='P',]
    }

    # handle plot style
    if (style=='moneyness'){
      df[,style] <- df$futurePrice / df$strike
    } else if (style=='delta'){
      df[,style] <- compDelta(df$type,
                              df$futurePrice,
                              df$strike,
                              df$ivol,
                              df$time_to_expiry)
      # case need calls and puts with delta display, reference delta to call, i.e. positive values.
      # in practice, there should be only calls with delta below 50% and only puts with delta above 50%.
      if (tolower(type)=='both'){df[df$type=='P',style] <- 1.0 + df[df$type=='P',style]}
    }

    if (length(excl)==0) excl <- nrow(df) + 1
    # plot implied volatilty curve by expiries.
    ivol_plot <- ggplot(df[-excl,], aes(x=get(style),
                                        y=ivol,
                                        shape=as.factor(type),
                                        color=as.factor(round(time_to_expiry, digits=2)),
                                        group=as.factor(round(time_to_expiry, digits=2)))) +
      geom_line() +
      geom_point(size=3) +
      ggtitle(paste0('Implied volatility by ',style)) +
      labs(x=style, col='Time to expiry', shape='Call/Put') +
      scale_y_continuous(labels=scales::percent) +
      theme(plot.title=element_text(color="#666666", face="bold", size=16, hjust=0.5)) +
      theme(axis.title=element_text(color="#666666", face="bold", size=12)) +
      theme(axis.text.y=element_text(color="#666666", face="bold", size=8)) +
      if (style %in% c('moneyness', 'delta')) {scale_x_continuous(labels=scales::percent)}
    else {scale_x_continuous(labels=scales::comma)}
    # display plot
    plot(ivol_plot)
  }
  # return ivols
  return(df$ivol)
}


