

sign_mix <- function(p){
    v1 <- sign_absFB_20D(p,trailing = T, dir = 1)
    v2 <- sign_MA(p)
    tmp <- rep(0, length(p))
    tmp <- ifelse(v1>0&v2>0,1,ifelse(v1<0&v2<0,-1,0))
    return(tmp)
}


position <- function(v, sign_fun){
    tmp<-Lag(sign_fun(v),1)
    tmp[1]<- 0  # otherwise, it will be NA
    return(tmp)
}


# opt_return <- function(price, pos,cost= 0.005, exp = 7){
#     # position contains 1, -1 ,or 2
#     option_cost <- cost
#     # cost occurs when postition is taken, 0.5%
#     # of crit_date : 1 for take position date, -1 for offset date, 2 for both
#     crit_date <- rep(0 , length(pos)) 
#     pos[1] <- 0
#     for(i in 2:length(pos)){
#         
#         if(pos[i]==pos[i-1]){
#             next
#         }else{
#             if(pos[i-1]==0){
#                 crit_date[i] <- 1
#             }else{
#                 ## take position
#                 crit_date[i] <- ifelse(pos[i]==0, 0, 1)
#                 crit_date[i-1] <- ifelse(crit_date[i-1]==0, -1, 2)
#             }
#         }
#     }
#     
#     cost <- rep(0 , length(pos))
#     the_return <- rep(0 , length(pos))
#     my_return <- xts(rep(0 , length(pos)), order.by=index(price)) 
#     pin_1 <- 0
#     pin_date <- NULL
#     for(i in 2:length(pos)){
#         if(crit_date[i]==2){
#             price_roc <- (as.numeric(price[i])-as.numeric(price[i-1]))/as.numeric(price[i-1])
#             price_roc <- price_roc * pos[i]
#             cost[i] <- (-1)*option_cost
#             if(price_roc < 0){
#                 pin_1 <- 0
#                 next
#             }
#             
#             my_return[i] <- price_roc - option_cost
#         }else if(crit_date[i]==1){
#             my_return[i] <- (-1)*option_cost
#             cost[i] <- (-1)*option_cost
#             pin_1 <- (i-1)
#             pin_date <- index(price[i])
#         }else if(crit_date[i]==-1){
#             # if out of money , disengage
#             price_roc <- (as.numeric(price[i])-as.numeric(price[pin_1]))/as.numeric(price[pin_1])
#             price_roc <- price_roc * pos[i]
#             
#             if(price_roc < 0){
#                 pin_1 <- 0
#                 pin_date <- NULL
#                 next
#             }
#             the_return[i] <- price_roc
#             my_return[i] <- price_roc
#             pin_1 <- 0
#             pin_date <- NULL
#             next
#         }
#         if(!is.null(pin_date)){
#             if(index(price[i])-pin_date >= exp){
#                 price_roc <- (as.numeric(price[i-1])-as.numeric(price[pin_1]))/as.numeric(price[pin_1])
#                 price_roc <- price_roc * pos[i]
#                 pin_date <- index(price[i])
#                 cost[i] <- (-1)*option_cost
#                 the_return[i] <-  ifelse(price_roc >0, price_roc, 0)
#                 my_return[i] <- ifelse(price_roc >0, price_roc - option_cost, (-1)*option_cost)
#                 pin_1 <- i-1
#             }
#         }
#         
#     }
#     
#     # assign("out",cbind(pos, crit_date, price,cost,the_return, my_return), env = .GlobalEnv)
#     # colnames(out) <- c("position", "critical_date", "close_price","option","return", "net_return")
# #     assign("out",out, env = .GlobalEnv)
# #     write.csv(out,"raw_data.csv",row.names = index(price))
# #     
#     return(my_return)
# }
opt_return <- function(price, pos,cost= 0.005, exp = 7){
    # position contains 1, -1 ,or 2
    option_cost <- cost
    # cost occurs when postition is taken, 0.5%
    # of crit_date : 1 for take position date, -1 for offset date, 2 for both
    crit_date <- rep(0 , length(pos)) 
    pos[1]<- 0
    for(i in 2:length(pos)){
        
        if(pos[i]==pos[i-1]){
            next
        }else{
            if(pos[i-1]==0){
                crit_date[i] <- 1
            }else{
                ## take position
                crit_date[i] <- ifelse(pos[i]==0, 0, 1)
                crit_date[i-1] <- ifelse(crit_date[i-1]==0, -1, 2)
            }
        }
    }
    
    cost <- rep(0 , length(pos))
    the_return <- rep(0 , length(pos))
    my_return <- xts(rep(0 , length(pos)), order.by=index(price)) 
    pin_1 <- 0
    pin_date <- NULL
    for(i in 2:length(pos)){
        if(crit_date[i]==2){
            price_roc <- (as.numeric(price[i])-as.numeric(price[i-1]))/as.numeric(price[i-1])
            price_roc <- price_roc * pos[i]
            cost[i] <- option_cost*(-1) * abs(pos[i])
            if(price_roc < 0){
                pin_1 <- 0
                next
            }
            
            my_return[i] <- price_roc - option_cost* abs(pos[i])
        }else if(crit_date[i]==1){
            my_return[i] <- option_cost*(-1)* abs(pos[i])
            cost[i] <- option_cost*(-1)* abs(pos[i])
            pin_1 <- (i-1)
            pin_date <- index(price[i])
        }else if(crit_date[i]==-1){
            # if out of money , disengage
            price_roc <- (as.numeric(price[i])-as.numeric(price[pin_1]))/as.numeric(price[pin_1])
            price_roc <- price_roc * pos[i]
            if(price_roc < 0){
                pin_1 <- 0
                pin_date <- NULL
                next
            }
            the_return[i] <- price_roc
            my_return[i] <- price_roc
            pin_1 <- 0
            pin_date <- NULL
            next
        }
        if(!is.null(pin_date)){
            if(index(price[i])-pin_date >= exp){
                price_roc <- (as.numeric(price[i-1])-as.numeric(price[pin_1]))/as.numeric(price[pin_1])
                price_roc <- price_roc * pos[i]
                pin_date <- index(price[i])
                cost[i] <- option_cost*(-1) * abs(pos[i])
                the_return[i] <-  ifelse(price_roc >0, price_roc, 0)
                my_return[i] <- ifelse(price_roc >0, price_roc - option_cost* abs(pos[i]), option_cost*(-1)*abs(pos[i]))
                pin_1 <- i-1
            }
        }
        
    }
#     
#     assign("out",cbind(pos, crit_date, price,cost,the_return, my_return), env = .GlobalEnv)
#     colnames(out) <- c("position", "critical_date", "close_price","option","return", "net_return")
#     assign("out",out, env = .GlobalEnv)
#     write.csv(out,"raw_data.csv",row.names = index(price))
    
    return(my_return)
}

Performance <- function(x) {
    
    cumRetx = Return.cumulative(x) 
    # prod(1 + R)-1
    
    annRetx = Return.annualized(x, scale=252) 
    # prod(1 + Ra)^(scale/n) - 1  
    # 252 refers to year, 12 to month, 4 to quarter
    
    sharpex = SharpeRatio.annualized(x, scale=252)
    # the excess return per unit of risk (represented by variance)
    # Rf = 0, by default
    
    winpctx = length(x[x > 0])/length(x[x != 0])
    winpctx = winpctx*100
    # winning rate
    
    annSDx = sd.annualized(x, scale=252)
    # std = sqrt(var(R))
    
    DDs <- findDrawdowns(x)
    # !!!!!!! findDrawdowns can't handle data over 10222 observations
    
    ##¡@drawdowns are defined by "total value" of portfolio
    
    maxDDx = min(DDs$return)
    
    maxLx = max(DDs$length)
    
    Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
    Perf = round(Perf, 6)
    names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                    "Win (in %)", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
    return(Perf)
}

# chart_analysis <- function(p,sign_fun){
#     bmk_return <- dailyReturn(p)
#     pos <- position(p, sign_fun)
#     my_return <- opt_return(p, pos)
#     
#     names(bmk_return) <- "benchmark"
#     names(my_return) <- "my_strategy"
#     result <- cbind("benchmark"=Performance(bmk_return),"my_strategy"=Performance(my_return))
#     charts.PerformanceSummary(cbind(my_return,bmk_return), colorset = c("darkblue","black"))
# }

chart_plot <- function(p, pos){
    bmk_return <- dailyReturn(p)
    my_return <- opt_return(p, pos)
    names(bmk_return) <- "benchmark"
    names(my_return) <- "my_strategy"
    result <- cbind("benchmark"=Performance(bmk_return),"my_strategy"=Performance(my_return))
    charts.PerformanceSummary(cbind(my_return,bmk_return), colorset = c("darkblue","black"))
    
}