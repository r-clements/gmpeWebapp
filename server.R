library(shiny)
library(ggplot2)
library(verification)
library(lme4)
library(grid)
library(plyr)
library(mgcv)
library(RColorBrewer)
library(ggmap)

#choose model colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

#make a qq-plot for ggplot
ggQQ <- function(LM) # argument: a linear model
{
  y <- quantile(LM$resids, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample = resids)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="blue")
  
  return(p)
}

#score function
make.scores <- function(score.data) {
  mae.fun <- function(d) {
    mean(abs(d$resids))
  }
  rmse.fun <- function(d) {
    sqrt(mean(d$resids^2))
  }
  prob.score.fun <- function(d) {
    pr.scores <- crps(d$obs, d[,c("pred", "sigma")])
    c(pr.scores$IGN, pr.scores$CRPS)
  }
  score.data <- score.data[,c("model", "resids", "sigma", "pred", "obs")]
  score.data$pred <- log10(score.data$pred)
  score.data$obs <- log10(score.data$obs)
  mae <- ddply(score.data, "model", .fun = mae.fun)
  rmse <- ddply(score.data, "model", .fun = rmse.fun)
  prob.scores <- ddply(score.data, "model", .fun = prob.score.fun)
  scores <- data.frame(mae, rmse[,2], prob.scores[,c(2,3)])
  names(scores) <- c("Model", "MAE", "RMSE", "IGN", "CRPS")
  scores
}

make.running.scores <- function(score.data) {
  mae.fun <- function(d) {
    mean(abs(d$resids))
  }
  rmse.fun <- function(d) {
    sqrt(mean(d$resids^2))
  }
  prob.score.fun <- function(d) {
    pr.scores <- crps(d$obs, d[,c("pred", "sigma")])
    c(pr.scores$IGN, pr.scores$CRPS)
  }
  score.data <- score.data[,c("model", "resids", "sigma", "pred", "obs", "event.num")]
  score.data$pred <- log10(score.data$pred)
  score.data$obs <- log10(score.data$obs)
  mae <- ddply(score.data, c("model", "event.num"), .fun = mae.fun)
  rmse <- ddply(score.data, c("model", "event.num"), .fun = rmse.fun)
  prob.scores <- ddply(score.data, c("model", "event.num"), .fun = prob.score.fun)
  scores <- data.frame(mae, rmse[,3], prob.scores[,c(3,4)])
  names(scores) <- c("Model", "Event.num", "MAE", "RMSE", "IGN", "CRPS")
  scores
}

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  data <- reactive({
    if(input$local == FALSE) {
      path <- input$url
    } else {
      df <- input$file
      path <- df$datapath
    }
    
    data <- read.csv(path, h = TRUE)
    data$resids <- (data$resids - mean(data$resids))/sd(data$resids)
    data
  })
  
  output$Models <- renderUI({
    model.data <- data()
    model.names <- unique(model.data$model)
    
    selectInput(inputId = "model_name",
                label = "GMPE:",
                choices = model.names,
                selected = model.names[1])
  })
  
  output$Models2 <- renderUI({
    model.data <- data()
    model.names <- unique(model.data$model)
    
    selectInput(inputId = "model_name2",
                label = "GMPE:",
                choices = model.names,
                selected = model.names,
                multiple = TRUE)
  })
  
  subdata <- reactive({
    gm.data <- data()
    if (is.null(input$event_num) || is.null(input$model_name) || 
          is.null(input$Rhyp) || is.null(input$Mag) || is.null(input$VS30) ||
          is.null(input$input_eqtype) || is.null(input$input_siteclass)) {
      return(NULL)
    }
    gm.data <- gm.data[gm.data$event.num %in% input$event_num, ]
    gm.data <- gm.data[gm.data$model %in% input$model_name, ]
    gm.data <- gm.data[(gm.data$Rhyp <= max(input$Rhyp))&(gm.data$Rhyp >= min(input$Rhyp)), ]
    gm.data <- gm.data[(gm.data$mag <= max(input$Mag))&(gm.data$mag >= min(input$Mag)),]
    gm.data <- gm.data[(gm.data$AVS30 <= max(input$VS30))&(gm.data$AVS30 >= min(input$VS30)),]
    gm.data <- gm.data[gm.data$eq.type %in% input$input_eqtype, ]
    gm.data <- gm.data[gm.data$siteClass %in% input$input_siteclass, ]
    gm.data
  })
  
  subdata2 <- reactive({
    gm.data <- data()
    if (is.null(input$event_num) || is.null(input$model_name) || 
          is.null(input$Rhyp) || is.null(input$Mag) || is.null(input$VS30) ||
          is.null(input$input_eqtype) || is.null(input$input_siteclass)) {
      return(NULL)
    }
    gm.data <- gm.data[gm.data$event.num %in% input$event_num, ]
    gm.data <- gm.data[gm.data$model %in% input$model_name2, ]
    gm.data <- gm.data[(gm.data$Rhyp <= max(input$Rhyp))&(gm.data$Rhyp >= min(input$Rhyp)), ]
    gm.data <- gm.data[(gm.data$mag <= max(input$Mag))&(gm.data$mag >= min(input$Mag)),]
    gm.data <- gm.data[(gm.data$AVS30 <= max(input$VS30))&(gm.data$AVS30 >= min(input$VS30)),]
    gm.data <- gm.data[gm.data$eq.type %in% input$input_eqtype, ]
    gm.data <- gm.data[gm.data$siteClass %in% input$input_siteclass, ]
    gm.data
  })
  
  output$Events <- renderUI({
    event.data <- data()
    event.nums <- unique(event.data$event.num)
    
    selectInput(inputId = "event_num",
                label = "Event number:",
                choices = event.nums,
                selected = event.nums,
                multiple = TRUE)
  })
  
  output$Rhyp <- renderUI({
    gm.data <- data()
    if(is.null(input$event_num)){
      return(NULL)
    }
    gm.data <- gm.data[gm.data$event.num %in% input$event_num, ]
    min.dist <- floor(min(gm.data$Rhyp))
    max.dist <- ceiling(max(gm.data$Rhyp))
    
    sliderInput("Rhyp", 
                "Epicentral Distance (km) range:", 
                min = min.dist, 
                max = max.dist, 
                step = 1,
                value = c(min.dist, max.dist))
  })
  
  output$Mag <- renderUI({
    gm.data <- data()
    if(is.null(input$event_num)){
      return(NULL)
    }
    gm.data <- gm.data[gm.data$event.num %in% input$event_num, ]
    min.mag <- floor(min(gm.data$mag))
    max.mag <- ceiling(max(gm.data$mag))
    
    sliderInput("Mag", 
                "Magnitude range:", 
                min = min.mag,
                max = max.mag,
                step = .1,
                value = c(min.mag, max.mag))
  })
  
  output$VS30 <- renderUI({
    gm.data <- data()
    if(is.null(input$event_num)){
      return(NULL)
    }
    gm.data <- gm.data[gm.data$event.num %in% input$event_num, ]
    min.vs30 <- floor(min(gm.data$AVS30))
    max.vs30 <- ceiling(max(gm.data$AVS30))
    
    sliderInput("VS30", 
                "VS30:", 
                min = min.vs30,
                max = max.vs30,
                step = .1,
                value = c(min.vs30, max.vs30))
  })
  
  output$eqtype <- renderUI({
    gm.data <- data()
    if(is.null(input$event_num)){
      return(NULL)
    }
    gm.data <- gm.data[gm.data$event.num %in% input$event_num, ]
    eq.types <- unique(gm.data$eq.type)
    selectInput(inputId = "input_eqtype",
                label = "",
                choices = eq.types,
                selected = eq.types,
                multiple = TRUE)
  })
  
  output$siteClass <- renderUI({
    gm.data <- data()
    if(is.null(input$event_num)){
      return(NULL)
    }
    gm.data <- gm.data[gm.data$event.num %in% input$event_num, ]
    sites <- unique(gm.data$siteClass)
    selectInput(inputId = "input_siteclass",
                label = "",
                choices = sites,
                selected = sites,
                multiple = TRUE)
  })
  
  output$plot_eqs <- renderPlot({
    gm.data <- subdata()
    if(is.null(gm.data)){
      return()
    }
    eq <- data.frame(lon = gm.data$eqlon, lat = gm.data$eqlat, magnitude = gm.data$mag)
    eq <- unique(eq)
    
    load("jp")
    p <- ggmap(jp)
    p <- p +
      geom_point(data = eq, mapping = aes(x = lon, y = lat, size=magnitude)) +
      xlab("Longitude") + ylab("Latitude") + 
      labs(title = "Earthquake locations") +
      theme(axis.text = element_text(size=14),
            axis.title.y = element_text(size=16, vjust=.5), 
            axis.title.x = element_text(size=16, vjust=.25))
    print(p)
  }, height=1000)
  
  output$plot_eqs2 <- renderPlot({
    gm.data <- subdata()
    if(is.null(gm.data)){
      return()
    }
    eq <- data.frame(lon = gm.data$eqlon, lat = gm.data$eqlat, magnitude = gm.data$mag)
    eq <- unique(eq)
    
    load("jp")
    p <- ggmap(jp)
    p <- p +
      geom_point(data = eq, mapping = aes(x = lon, y = lat, size=magnitude)) +
      xlab("Longitude") + ylab("Latitude") + 
      labs(title = "Earthquake locations") +
      theme(axis.text = element_text(size=14),
            axis.title.y = element_text(size=16, vjust=.5), 
            axis.title.x = element_text(size=16, vjust=.25))
    print(p)
  }, height=1000)
  
  output$plot_resid <- renderPlot({
    gm.data <- subdata()
    if(is.null(gm.data)){
      return()
    }
    p0 <- ggQQ(gm.data)
    p0.1 <- ggplot() + geom_point(data = gm.data, aes(x = pred, y = resids)) + 
      xlab("Predicted value") + ylab("Standardized residual")
    p1 <- ggplot() + geom_point(data = gm.data, aes(x = Rhyp, y = resids)) +
      xlab("Hypocentral distance") + ylab("Standardized residual")
    p2 <- ggplot() + geom_point(data = gm.data, aes(x = AVS30, y = resids)) +
      xlab("VS30") + ylab("Standardized residual")
    p3 <- ggplot() + geom_boxplot(data = gm.data, aes(x = factor(mag), y = resids)) +
      xlab("Magnitude") + ylab("Standardized residual")
    p4 <- ggplot() + geom_boxplot(data = gm.data, aes(x = siteClass, y = resids)) +
      xlab("Site class") + ylab("Standardized residual")
    p5 <- ggplot() + geom_boxplot(data = gm.data, aes(x = eq.type, y = resids)) +
      xlab("EQ type") + ylab("Standardized residual")
    p6 <- ggplot() + geom_boxplot(data = gm.data, aes(x = factor(event.num), y = resids)) +
      theme(axis.text.x=element_text(angle = 90)) +
      xlab("Event number") + ylab("Standardized residual")
    plots <- list(p0, p0.1, p1, p2, p3, p4, p5, p6)
    layout <- matrix(c(1,2,3,4,5,6,7,8), nrow = 4, byrow = TRUE)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:8) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }, height = 1000)
  
  output$plot_unex_resid <- renderPlot({
    gm.data <- subdata()
    if(is.null(gm.data)){
      return()
    }
    tmp <- gm.data[,c("resids", "Rhyp", "mag", "AVS30", "eq.type", "siteClass", "event.num")]
    if(length(unique(tmp$event.num)) > 1) {
      if(length(unique(tmp$eq.type)) == 1) {
        tmp <- subset(tmp, select=-c(eq.type))
      }
      if(length(unique(tmp$siteClass)) == 1) {
        tmp <- subset(tmp, select=-c(siteClass))
      }
      if(length(unique(tmp$mag)) == 1) {
        tmp <- subset(tmp, select=-c(mag))
      }
      model.fit <- lmer(resids ~ . - event.num + (1|event.num), data = tmp)
      summary(model.fit)
    } else {
      stop("No random effect model")
    }
    coeff <- fixef(model.fit)
    coeff <- data.frame(t(coeff))
    gm.data$unex.resids <- residuals(model.fit)
    gm.data$pred.fit <- fitted(model.fit)
    tmp.data <- data.frame(resids = gm.data$unex.resids)
    tmp.data$resids <- gm.data$unex.resids <- (gm.data$unex.resids - mean(gm.data$unex.resids))/sd(gm.data$unex.resids)
    gm.data$unex.resids.Rhyp <- coeff$Rhyp*gm.data$Rhyp + gm.data$unex.resids
    gm.data$unex.resids.AVS30 <- coeff$AVS30*gm.data$AVS30 + gm.data$unex.resids
    if(length(unique(gm.data$mag)) == 1) {
      coeff$mag <- 0
    }
    gm.data$unex.resids.mag <- coeff$mag*gm.data$mag + gm.data$unex.resids
    
    siteClassHolder <- eqTypeHolder <- rep(0, nrow(gm.data))
    
    if(length(unique(gm.data$eq.type)) > 1) {
      coef.eq.type <- subset(coeff, select=grep("eq.type", names(coeff)))
      name.coef.eq.type <- names(coef.eq.type)
      name.coef.eq.type <- gsub("eq.type", "", name.coef.eq.type)
      for(i in name.coef.eq.type) {
        eqTypeHolder[as.character(gm.data$eq.type) %in% i] <- coef.eq.type[,paste0("eq.type",i)]
      }
    }
    
    if(length(unique(gm.data$siteClass)) > 1) {
      coef.siteClass <- subset(coeff, select=grep("siteClass", names(coeff)))
      name.coef.siteClass <- names(coef.siteClass)
      name.coef.siteClass <- gsub("siteClass", "", name.coef.siteClass)
      for(i in name.coef.siteClass) {
        siteClassHolder[as.character(gm.data$siteClass) %in% i] <- coef.siteClass[,paste0("siteClass",i)]
      }
    }
    
    gm.data$unex.resids.siteClass <- siteClassHolder + gm.data$unex.resids
    gm.data$unex.resids.eq.type <- eqTypeHolder + gm.data$unex.resids
      
    p0 <- ggQQ(tmp.data)
    p0.1 <- ggplot() + geom_point(data = gm.data, aes(x = pred.fit, y = unex.resids)) +
    xlab("Predicted value") + ylab("Standardized residual")
    p1 <- ggplot() + geom_point(data = gm.data, aes(x = Rhyp, y = unex.resids.Rhyp)) +
      xlab("Hypocentral distance") + ylab("Standardized residual")
    p2 <- ggplot() + geom_point(data = gm.data, aes(x = AVS30, y = unex.resids.AVS30)) +
      xlab("VS30") + ylab("Standardized residual")
    p3 <- ggplot() + geom_boxplot(data = gm.data, aes(x = factor(mag), y = unex.resids.mag)) +
      xlab("Magnitude") + ylab("Standardized residual")
    p4 <- ggplot() + geom_boxplot(data = gm.data, aes(x = siteClass, y = unex.resids.siteClass)) +
      xlab("Site class") + ylab("Standardized residual")
    p5 <- ggplot() + geom_boxplot(data = gm.data, aes(x = eq.type, y = unex.resids.eq.type)) +
      xlab("EQ type") + ylab("Standardized residual")
    p6 <- ggplot() + geom_boxplot(data = gm.data, aes(x = factor(event.num), y = unex.resids)) +
      theme(axis.text.x=element_text(angle = 90)) +
      xlab("Event number") + ylab("Standardized residual")
    plots <- list(p0, p0.1, p1, p2, p3, p4, p5, p6)
    layout <- matrix(c(1,2,3,4,5,6,7,8), nrow = 4, byrow = TRUE)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:8) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }, height = 1000)

  
  output$summary <- renderPrint({
    gm.data <- subdata()
    if(is.null(gm.data)){
      return()
    }
    gm.data <- gm.data[,c("resids", "Rhyp", "mag", "AVS30", "eq.type", "siteClass")]
    if(length(unique(gm.data$eq.type)) == 1) {
      gm.data <- subset(gm.data, select=-c(eq.type))
    }
    if(length(unique(gm.data$siteClass)) == 1) {
      gm.data <- subset(gm.data, select=-c(siteClass))
    }
    if(length(unique(gm.data$mag)) == 1) {
      gm.data <- subset(gm.data, select=-c(mag))
    }
    model.fit <- lm(resids ~ ., data = gm.data)
    summary(model.fit)
  }) 
  
  output$summary.re <- renderPrint({
    gm.data <- subdata()
    if(is.null(gm.data)){
      return()
    }
    gm.data <- gm.data[,c("resids", "Rhyp", "mag", "AVS30", "eq.type", "siteClass", "event.num")]
    if(length(unique(gm.data$event.num)) > 1) {
      if(length(unique(gm.data$eq.type)) == 1) {
        gm.data <- subset(gm.data, select=-c(eq.type))
      }
      if(length(unique(gm.data$siteClass)) == 1) {
        gm.data <- subset(gm.data, select=-c(siteClass))
      }
      if(length(unique(gm.data$mag)) == 1) {
        gm.data <- subset(gm.data, select=-c(mag))
      }
      model.fit <- lmer(resids ~ . - event.num + (1|event.num), data = gm.data)
      summary(model.fit)
    } else {
      print("No random effect model")
    }
  }) 
  
  output$spatplot <- renderPlot({
      gm.data <- subdata()
      if(is.null(gm.data)){
        return()
      }
      spatial.data <- data.frame(lon = gm.data$stlon, lat = gm.data$stlat, 
                                 residual = gm.data$resids)
      if(input$view_type == "ave") {
        pred.average <- ddply(spatial.data, c("lat", "lon"), function(x) {mean(x[,"residual"])})
        pred.ave.data <- join(pred.average, spatial.data)
        sp <- unique(pred.ave.data[,c("lon", "lat", "V1")])
        names(sp) <- c("lon", "lat", "pred")
      }
      if(input$view_type == "smooth") {
        spatial.model <- gam(residual ~ s(lon, lat, k = 50), data = spatial.data)
        spat.data.unique <- unique(spatial.data[,c(1,2)])
        pred <- predict(spatial.model, newdata = spat.data.unique)
        sp <- cbind(spat.data.unique, pred)
      }
      
      brewer.div <- colorRampPalette(brewer.pal(9, "RdBu"), interpolate = "spline")
      cols <- (brewer.div(256))
      lower <- -max(abs(sp$pred))
      upper <- max(abs(sp$pred))
      cutoffs <- seq(lower,upper, length.out=257)
      brks <- seq(lower, upper, length.out=10)[-c(5,6)]; labs <- round(brks, 2) 
      labs[c(1,8)] <- c(paste0("<= ", round(lower,2)), paste0(">= ", round(upper,2)))
      
      load("jp")
      p <- ggmap(jp)
      p <- p +
        geom_point(data = sp, mapping = aes(x = lon, y = lat, colour = pred), size=5) +
        scale_colour_gradientn(colours=cols, 
                               name="Residual", values = cutoffs, limits = c(lower,upper),
                               labels=labs, breaks=brks, rescaler =
                                 function(x,...) x, oob=identity, guide="colourbar") +
        xlab("Longitude") + ylab("Latitude") + 
        labs(title = "Smoothed residuals") +
        theme(axis.text = element_text(size=14),
              axis.title.y = element_text(size=16, vjust=.5), 
              axis.title.x = element_text(size=16, vjust=.25))
      print(p)
  }, height = 1000)
  
  output$spatplot2 <- renderPlot({
    gm.data <- subdata()
    if(is.null(gm.data)){
      return()
    }
    tmp <- gm.data[,c("resids", "Rhyp", "mag", "AVS30", "eq.type", "siteClass", "event.num")]
    if(length(unique(tmp$event.num)) > 1) {
      if(length(unique(tmp$eq.type)) == 1) {
        tmp <- subset(tmp, select=-c(eq.type))
      }
      if(length(unique(tmp$siteClass)) == 1) {
        tmp <- subset(tmp, select=-c(siteClass))
      }
      if(length(unique(tmp$mag)) == 1) {
        tmp <- subset(tmp, select=-c(mag))
      }
      #re.formula <- resids ~ Rhyp + mag + AVS30 + eq.type + siteClass + (1|event.num)
      model.fit <- lmer(resids ~ . - event.num + (1|event.num), data = tmp)
    } else {
      stop("No random effect model")
    }
    gm.data$unex.resids <- residuals(model.fit)
    spatial.data <- data.frame(lon = gm.data$stlon, lat = gm.data$stlat, 
                               residual = gm.data$unex.resids)
    if(input$view_type == "ave") {
      pred.average <- ddply(spatial.data, c("lat", "lon"), function(x) {mean(x[,"residual"])})
      pred.ave.data <- join(pred.average, spatial.data)
      sp <- unique(pred.ave.data[,c("lon", "lat", "V1")])
      names(sp) <- c("lon", "lat", "pred")
    }
    if(input$view_type == "smooth") {
      spatial.model <- gam(residual ~ s(lon, lat, k = 50), data = spatial.data)
      spat.data.unique <- unique(spatial.data[,c(1,2)])
      pred <- predict(spatial.model, newdata = spat.data.unique)
      sp <- cbind(spat.data.unique, pred)
    }
    
    brewer.div <- colorRampPalette(brewer.pal(9, "RdBu"), interpolate = "spline")
    cols <- (brewer.div(256))
    lower <- -max(abs(sp$pred))
    upper <- max(abs(sp$pred))
    cutoffs <- seq(lower,upper, length.out=257)
    brks <- seq(lower, upper, length.out=10)[-c(5,6)]; labs <- round(brks, 2) 
    labs[c(1,8)] <- c(paste0("<= ", round(lower,2)), paste0(">= ", round(upper,2)))
    
    load("jp")
    p <- ggmap(jp)
    p <- p +
      geom_point(data = sp, mapping = aes(x = lon, y = lat, colour = pred), size=5) +
      scale_colour_gradientn(colours=cols, 
                             name="Residual", values = cutoffs, limits = c(lower,upper),
                             labels=labs, breaks=brks, rescaler =
                               function(x,...) x, oob=identity, guide="colourbar") +
      xlab("Longitude") + ylab("Latitude") + 
      labs(title = "Smoothed residuals") +
      theme(axis.text = element_text(size=14),
            axis.title.y = element_text(size=16, vjust=.5), 
            axis.title.x = element_text(size=16, vjust=.25))
    print(p)
  }, height = 1000)
  
  output$scores <- renderTable({
    gm.data <- subdata2()
    if(is.null(gm.data)){
      return()
    }
    temp <- make.scores(gm.data)
    temp
  })
  
  output$runningPlot <- renderPlot({
    gm.data <- data()
    if(is.null(gm.data)){
      return()
    }
    n <- length(unique(gm.data$model))
    model.cols <- data.frame(unique(gm.data$model), gg_color_hue(n))
    model.cols <- data.frame(lapply(model.cols, as.character), stringsAsFactors=FALSE)
    
    gm.data <- subdata2()
    if(is.null(gm.data)){
      return()
    }
    temp <- make.running.scores(gm.data)
    temp$Event.num <- as.character(temp$Event.num)
    
    col.Model <- rep(0, nrow(temp))
    for(i in 1:length(col.Model))
      col.Model[i] <- model.cols[which(model.cols[,1]==temp[i,"Model"]),2]
                
    p1 <- ggplot(temp, aes(x=Event.num, y=MAE)) +
      geom_line(aes(colour = Model, group = Model)) + 
      geom_point(aes(colour = Model), size=3)    +
      labs(x = "Event Num", y = "MAE") + 
      theme(axis.text.x=element_text(angle = 90))
    
    p2 <- ggplot(temp, aes(x=Event.num, y=RMSE)) +
      geom_line(aes(colour = Model, group = Model)) + 
      geom_point(aes(colour = Model), size=3)    +
      labs(x = "Event Num", y = "RMSE") + 
      theme(axis.text.x=element_text(angle = 90))
    
    p3 <- ggplot(temp, aes(x=Event.num, y=IGN)) +
      geom_line(aes(colour = Model, group = Model)) + 
      geom_point(aes(colour = Model), size=3)    +
      labs(x = "Event Num", y = "IGN") + 
      theme(axis.text.x=element_text(angle = 90))
    
    p4 <- ggplot(temp, aes(x=Event.num, y=CRPS)) +
      geom_line(aes(colour = Model, group = Model)) + 
      geom_point(aes(colour = Model), size=3)    +
      labs(x = "Event Num", y = "CRPS") + 
      theme(axis.text.x=element_text(angle = 90))
    plots <- list(p1, p2, p3, p4)
    
    layout <- matrix(c(1,2,3,4), nrow = 4, byrow = TRUE)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:4) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }, height = 1000)
})
