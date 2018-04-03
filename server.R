#server
library(shiny)
library(rethinking)
library(geosphere)
library(dplyr)
library(ggmap)

d <- read.csv("D:\\祐瑄\\台大\\Course\\大四\\QBS\\individual project\\edited data\\data_new.csv")
#d$theftRate_percent <- d$theftRate_percent*10
#d$househundred <- as.integer(round(d$househundred/10)) #thousand
theft_105 <- read.csv("D:\\祐瑄\\台大\\Course\\大四\\QBS\\individual project\\edited data\\theft105_new.csv")
surv_loc <- read.csv("D:\\祐瑄\\台大\\Course\\大四\\QBS\\individual project\\surveillance.csv")
dist_name <- d$dist_eng
distList <- as.data.frame(table(theft_105$dist))
colnames(distList) <- c("dist","freq")

postcheck <- function( fit , x , prob=0.89 , window=20 , n=1000 , col=rangi2 , ...  ) {
  
  undot <- function( astring ) {
    astring <- gsub( "." , "_" , astring , fixed=TRUE )
    astring
  }
  
  pred <- link(fit,n=n)
  sims <- sim(fit,n=n)
  
  if ( class(pred)=="list" )
    if ( length(pred)>1 ) pred <- pred[[1]]
  
  # get outcome variable
  lik <- flist_untag(fit@formula)[[1]]
  dname <- as.character(lik[[3]][[1]])
  outcome <- as.character(lik[[2]])
  if ( class(fit)=="map2stan" ) outcome <- undot(outcome)
  y <- fit@data[[ outcome ]]
  
  # check for x-axis variable definition
  if ( !missing(x) ) {
    if ( x %in% names(fit@data) ) {
      x_var <- fit@data[[x]]
      r <- range(x_var)*c(0.9,1.1)
      x_seq <- seq(from=r[1],to=r[2],length.out=30)
    }
  }
  
  # compute posterior predictions for each case
  
  mu <- apply( pred , 2 , mean )
  mu.PI <- apply( pred , 2 , PI , prob=prob )
  y.PI <- apply( sims , 2 , PI , prob=prob )
  
  # figure out paging
  if ( length(window)==1 ) {
    window <- min(window,length(mu))
    num_pages <- ceiling( length(mu)/window )
    cases_per_page <- window
  }
  
  # display
  ny <- length(fit@data[[ outcome ]])
  ymin <- min(c(as.numeric(y.PI),mu,y))
  ymax <- max(c(as.numeric(y.PI),mu,y))
  
  # check for aggregated binomial context
  mumax <- max(c(as.numeric(mu.PI)))
  if ( ymax > 1 & mumax <= 1 & dname %in% c("dbinom","dbetabinom") ) {
    # probably aggregated binomial
    size_var <- as.character(lik[[3]][[2]])
    size_var <- fit@data[[ size_var ]]
    for ( i in 1:ny ) {
      y.PI[,i] <- y.PI[,i]/size_var[i]
      y[i] <- y[i]/size_var[i]
    }
    ymin <- 0
    ymax <- 0.2
  }
  if ( dname=="dordlogit" ) {
    # put mu and mu.PI on outcome scale
    ymin <- 1
    nlevels <- dim(pred)[3]
    mu <- sapply( 1:dim(pred)[2] , 
                  function(i) { 
                    temp <- t(pred[,i,])*1:7
                    mean(apply(temp,2,sum))
                  } )
    mu.PI <- sapply( 1:dim(pred)[2] , 
                     function(i) { 
                       temp <- t(pred[,i,])*1:7
                       PI(apply(temp,2,sum),prob)
                     } )
  }
  
  start <- 1
  end <- cases_per_page
  
  for ( i in 1:num_pages ) {
    
    end <- min( ny , end )
    window <- start:end
    
    set_nice_margins()
    plot( y[window] , xlab="District" , ylab="Burglary Rate(%)" , col=col , pch=16 , ylim=c( ymin , ymax ) , xaxt="n" , ... )
    axis( 1 , at=1:length(window) , labels=dist_name, cex.axis = 0.7 )
    
    points( 1:length(window) , mu[window] )
    for ( x in 1:length(window) ) {
      lines( c(x,x) , mu.PI[,window[x]] )
      points( c(x,x) , y.PI[,window[x]] , cex=0.7 , pch=3 )
    }
    mtext( paste("Posterior validation check") )
    
    if ( num_pages > 1 ) {
      ask_old <- devAskNewPage(ask = TRUE)
      on.exit(devAskNewPage(ask = ask_old), add = TRUE)
    }
    
    start <- end + 1
    end <- end + cases_per_page
    
  }
  
  # invisible result
  result <- list(
    mean=mu,
    PI=mu.PI,
    outPI=y.PI
  )
  invisible( result )
}

#District Model
m7.2 <- map2stan(
  alist(
    theft105 ~ dbinom(househundred, p) ,
    logit(p) <- a + bC*camDen + bP*policeStation + bM*log(mid.low.income105) 
    + bS*surv.avg + bD*DisposableInc,
    a ~ dnorm(0,1),
    c(bC, bP, bM, bS, bD) ~ dnorm(0,0.5)
  ),
  data=d,iter = 5000, warmup = 2500, chains = 2
  )

#Nearby Survalliance Camera Model
m.surv.dist5 <- map2stan(
  alist(
    totalSurv ~ dpois(lambda) ,
    log(lambda) <- a[dist] + bT[dist]*times,
    a[dist] ~ dnorm(0,100),
    bT[dist] ~ dnorm(0,10)
  ),
  data=theft_105, iter = 1200, warmup = 600, chains = 2
  )


#----------------------

function(input, output){
  
  #Map of theft and cameras location
  output$map <- renderPlot({
    
    map <- get_map(location = c(lon = 121.54, lat = 25.05),zoom = 13,maptype = "roadmap")
    
    if(input$map=='Burglaries'){
      ggmap(map, darken = c(0.3,"white"))+ xlab('Longitude')+ ylab('Latitude')+
        geom_point(aes(x = lng,y = lat), data = theft_105, col = 'red', size = 2, alpha = 0.3)
    } else if(input$map=='Surveillance Cameras'){
      ggmap(map, darken = c(0.3,"white"))+ xlab('Longitude')+ ylab('Latitude')+
        geom_point(aes(x = lng,y = lat), data = surv_loc, size = 2, alpha = 0.2)
    } else if (input$map=='Both'){
      ggmap(map, darken = c(0.3,"white"))+ xlab('Longitude')+ ylab('Latitude')+
        geom_point(aes(x = lng,y = lat), data = surv_loc, size = 2, alpha = 0.2)+
        geom_point(aes(x = lng,y = lat), data = theft_105, col = 'red', size = 2, alpha = 0.3)
    }
    
    },height = 600, width = 600)
  
  
  #Number of Cameras in 1 km VS Repeart Times
  output$nearbyCam_all <- renderPlot({
    
    theft_105 %>% ggplot(aes(times,totalSurv))+ theme_bw()+
      ylab('Surveillance Cameras in 1 km')+xlab('Repeat Times')+
      #ggtitle('Repeated Burglary with Number of Nearby Cameras')+
      geom_point(aes(col = dist))+ labs(colour='District')
    
  })
  
  
  #Number of Cameras in 1 km VS Repeart Times(Select certain district)
  output$nearbyCam_certain <- renderPlot({
    
    times.seq <- seq( from=0 , to=60 , length.out=1000 )
    d.pred <- data.frame(dist = as.integer(input$district), times= times.seq)
    
    lambda.pred.h <- ensemble( m.surv.dist5 , data=d.pred )
    lambda.med <- apply( lambda.pred.h$link , 2 , median )
    lambda.PI <- apply( lambda.pred.h$link , 2 , PI )

    plot( theft_105$times, theft_105$totalSurv,
          lwd=2, cex=1.4, pch=16, #main = 'In Certain District',
          col=alpha(ifelse(theft_105$dist==distList$dist[as.integer(input$district)], "darkorange1", "#999999"),0.5),
          ylab="Nearby Surveillance Cameras", xlab="Repeat Times")
    
    lines( times.seq , lambda.med , col='dodgerblue2',lwd=2 )
    shade( lambda.PI , times.seq , col=col.alpha(rangi2,0.2) )
    
  })
  
  
  #Postcheck
  output$postcheck <- renderPlot({postcheck(m7.2)},height = 250)
  
  
  #Theft Rate Prediction 
  output$theftRate <- renderPlot({
    
    survDen.seq <- seq( from=0 , to=3 , length.out=100 )
    pred1 <- data.frame(camDen = survDen.seq,
                        policeStation = input$station,
                        mid_low_income105 = input$lowIncome,
                        surv_avg = input$avgSurv,
                        DisposableInc = input$disposable)
    
    point.pred1 <- data.frame(camDen = input$survDen,
                              policeStation = input$station,
                              mid_low_income105 = input$lowIncome,
                              surv_avg = input$avgSurv,
                              DisposableInc = input$disposable)
    
    mu <- link(m7.2, data = pred1)
    mu.mean <- apply(mu,2,mean)
    mu.PI <- apply(mu,2,PI)
    
    plot( d$camDen, d$theftRate_percent , 
          col='dodgerblue4', type = 'n',
          lwd=2,cex =1.2, xlab="Surveillance Camera Density" , ylab="Burglary Rate(%)")
    lines( survDen.seq, mu.mean , col='dodgerblue3',lwd=2 )
    shade( mu.PI,survDen.seq , col=col.alpha(rangi2,0.3) )
    
    point.mu <- link(m7.2, data = point.pred1 )
    point.mu.mean <- apply( point.mu, 2, mean)
    points(input$survDen,point.mu.mean,pch=16,
           col=col.alpha("indianred2",0.8), cex = 2)
    
  })
  
  #Theft Rate Model Coefficients
  output$coef <- renderPrint({precis(m7.2)})
  
}