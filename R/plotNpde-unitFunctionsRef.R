#####################  Plots with reference profile  #############################

#' @importFrom stats approx

aux.npdeplot.meanprof <- function(mbin, msim) {
  # Compute a reference profile based on simulations from the model
  # mbin : matrix with columns xat (centre of the bins) and xgrp (bin number/group)
  # msim : matrix with simulations, 2 columns used (grp=which bin, ysim=simulated value)
  ymed <- tapply(msim$ysim, msim$grp, mean)
  sdmed <- tapply(msim$ysim, msim$grp, sd)
  ymed <- ymed[match(mbin$xlab, names(ymed), nomatch = 0)]
  sdmed <- sdmed[match(mbin$xlab, names(sdmed), nomatch = 0)]

  if (length(ymed) < dim(mbin)[1]) {
    cat(
      "Not all time points/bins are represented in the subset used for the reference profile: spline interpolation will be used to predict the entire profile, but this may distort the aspect of the plot significantly; we advise using another reference profile.\n"
    )

    xcent <- mbin$xat[mbin$xlab %in% names(ymed)]

    if ( length(xcent)!=0){
      ypmed <- spline(xcent, ymed, xout = mbin$xat)$y
      spmed <- spline(xcent, sdmed, xout = mbin$xat)$y

      iint <- 1 - as.integer(mbin$xlab %in% names(ymed))
      mpref <-
        data.frame(
          xat = mbin$xat,
          grp = mbin$grp,
          mean = ypmed,
          sd = spmed,
          int = iint,
          xlab = mbin$xlab)}else{stop("nan values for splines")}
  } else
    mpref <-
    data.frame(
      xat = mbin$xat,
      grp = mbin$grp,
      mean = ymed,
      sd = sdmed,
      int = rep(0, length(sdmed)),
      xlab = mbin$xlab
    )
  return(mpref)
}

aux.npdeplot.computepi <-
  function(
    plmat,
    plot.opt,
    xlab,
    xat,
    mpref = NULL,
    dotline = NULL,
    sim.ypl = NULL,
    distrib = "norm",
    onlog = FALSE) {

    # Compute prediction interval for the observed data, the size of which depends on the number of observations in each bin
    # Input
    # plmat: matrix of values to plot
    # plot.opt:
    # xat: center of bins
    # xlab: group tag
    # mpref: reference profile
    # dotline:
    # sim.ypl:
    # distrib: reference distribution
    # onlog: whether E and SD are computed on the observed value or after log transformation
    # Output
    # bnds: boundaries of the prediction intervals for each bin present in plmat [may be <nbin]

    xinf <- sqrt(12)
    alpha <- (1 - plot.opt$vpc.interval) / 2
    nseuil <- 200
    alp.pi <- plot.opt$pi.size
    if (alp.pi < 0.5)
      alp.pi <- (1 - alp.pi)
    quant <- c(alpha, 0.5, 1 - alpha)

    if (!plot.opt$approx.pi & !is.null(sim.ypl)) {
      nrep <- length(sim.ypl) / dim(plmat)[1]

      yprov <- list(
        grp = plmat$grp,
        cens = plmat$cens,
        ypl = matrix(sim.ypl, ncol = nrep)
      )

      bnds <- compute.bands.true(yprov, quant, alpha = alp.pi)
      bnds$xcent <- xat

    } else {
      nobs <- tapply(plmat$grp, plmat$grp, length)

      bnds <-
        compute.bands(nobs, nseuil, quant, distrib, alpha = alp.pi)
      bnds$xcent <- xat[match(names(nobs), xlab)]

    }

    # Transforming the boundaries of reference profile

    if (!is.null(mpref)) {

      if (onlog) {
        if (distrib == "unif") {
          for (i in 1:3)
            bnds$binf[, i] <- exp((bnds$binf[, i] - 0.5) * mpref$sd * xinf + mpref$mean)
          for (i in 1:3)
            bnds$bmed[, i] <- exp((bnds$bmed[, i] - 0.5) * mpref$sd * xinf + mpref$mean)
          for (i in 1:3)
            bnds$bsup[, i] <- exp((bnds$bsup[, i] - 0.5) * mpref$sd * xinf + mpref$mean)
        } else {
          for (i in 1:3)
            bnds$binf[, i] <- exp(bnds$binf[, i] * mpref$sd + mpref$mean)
          for (i in 1:3)
            bnds$bmed[, i] <- exp(bnds$bmed[, i] * mpref$sd + mpref$mean)
          for (i in 1:3)
            bnds$bsup[, i] <- exp(bnds$bsup[, i] * mpref$sd + mpref$mean)
        }
      } else {
        if (distrib == "unif") {
          for (i in 1:3)
            bnds$binf[, i] <- (bnds$binf[, i] - 0.5) * mpref$sd * xinf + mpref$mean
          for (i in 1:3)
            bnds$bmed[, i] <- (bnds$bmed[, i] - 0.5) * mpref$sd * xinf + mpref$mean
          for (i in 1:3)
            bnds$bsup[, i] <- (bnds$bsup[, i] - 0.5) * mpref$sd * xinf + mpref$mean
        } else {
          for (i in 1:3)
            bnds$binf[, i] <- bnds$binf[, i] * mpref$sd + mpref$mean
          for (i in 1:3)
            bnds$bmed[, i] <- bnds$bmed[, i] * mpref$sd + mpref$mean
          for (i in 1:3)
            bnds$bsup[, i] <- bnds$bsup[, i] * mpref$sd + mpref$mean
        }
      }
      if (is.null(dotline))
        dline <- NULL
      else {
        dline <- data.frame(xat = mpref$xat)
        for (i in 1:length(dotline)) {
          if (onlog) {
            if (distrib == "unif")
              x1 <-
                exp((dotline[i] - 0.5) * mpref$sd * xinf + mpref$mean)
            else
              x1 <- exp(dotline[i] * mpref$sd + mpref$mean)
          } else {
            if (distrib == "unif")
              x1 <-
                (dotline[i] - 0.5) * mpref$sd * xinf + mpref$mean
            else
              x1 <- dotline[i] * mpref$sd + mpref$mean
          }
          dline <- cbind(dline, x1)
        }
      }
    }	else {
      if (is.numeric(plmat$x) & !is.null(dotline)) {
        dline <-
          data.frame(xat = seq(
            min(plmat$x, na.rm = TRUE),
            max(plmat$x, na.rm = TRUE),
            length.out = 100
          ))
        for (i in 1:length(dotline))
          dline <- cbind(dline, rep(dotline[i], 100))
      } else
        dline <- NULL
    }

    return(list(bnds = bnds, dline = dline))


  }

aux.npdeplot.transform <-
  function(plmat,
           plot.opt,
           xat,
           mpref = NULL,
           distrib = "norm",
           onlog = FALSE) {
    # Input
    # plmat: values to plot
    # plot.opt: preferences for plots
    # mpref: reference profile
    # distrib: reference distribution
    # onlog: whether E and SD are computed on the observed value or after log transformation
    # Output
    # plmat: updated with a ty column containing the data to plot
    # percobs: percentile of the observed distribution, used in the plots
    xinf <- sqrt(12) # used only if distrib is "unif"
    alpha <- (1 - plot.opt$vpc.interval) / 2
    quant <- c(alpha, 0.5, 1 - alpha)
    if (is.null(mpref)) {
      ty <- plmat$y
    } else {
      mpr <-
        mpref[mpref$int == 0, ] # use only points not interpolated initially, to avoid double interpolation
      yfmed <- spline(mpr$xat, mpr$mean, xout = plmat$x)$y
      sfmed <- spline(mpr$xat, mpr$sd, xout = plmat$x)$y
      if (onlog) {
        if (distrib == "unif")
          ty <-
            exp((plmat$y - 0.5) * sfmed * xinf + yfmed)
        else
          ty <- exp(plmat$y * sfmed + yfmed)
      } else {
        if (distrib == "unif")
          ty <- (plmat$y - 0.5) * sfmed * xinf + yfmed
        else
          ty <- plmat$y * sfmed + yfmed
      }
    }
    percobs <-
      matrix(unlist(tapply(ty, plmat$grp, quantile, quant, na.rm = T)), ncol =
               3, byrow = T)
    row.names(percobs) <- xat
    plmat <- cbind(plmat, ty = ty)
    return(list(plmat = plmat, percobs = percobs))
  }



##########################################################################################################################################################################

# aux.npdeplot.plot

# function : plot vpc and xscatter

##########################################################################################################################################################################

aux.npdeplot.plot <- function(obs.mat, pi.mat, plot.opt) {

  # data no censored
  plotdatapoint <- data.frame(obs.mat$x[obs.mat$cens == 0], obs.mat$y[obs.mat$cens == 0])
  colnames(plotdatapoint) = c("x1", "y1")

  # data censored
  plotdatapoint2 <-  data.frame(obs.mat$x[obs.mat$cens == 1], obs.mat$y[obs.mat$cens == 1])
  colnames(plotdatapoint2) = c("x2", "y2")

  # data under the loq
  plotDataUnderLOQ = subset( obs.mat, obs.mat$y < unique( obs.mat$loq ) )

  # data ipred under the loq
  if ( length (obs.mat$ipred) !=0 )
  {
    plotImputedDatatUnderLOQ = subset( obs.mat, obs.mat$ipred < unique( obs.mat$loq ) )
  }

  # error message if no ipred and impute.loq TRUE
  if ( length (obs.mat$ipred) ==0 && plot.opt$impute.loq == TRUE){
    stop( " no ipred to plot")
}

  # take the plot options
  plot.opt <- sapply(plot.opt, "[[", 1)

  # list to stack the ggplot
  list_plot = list()

  nameCovariate = unique(pi.mat$covariate )    # nom de la covariable
  namesCategories = unique(pi.mat$category )   # nombre de catégories pour lacovariable
  numberCategories =  length(namesCategories)  # nombre de catégories pour la covariable

  # nrow.grid, ncol.grid : number of row and cols of the grid.arrange
  nrow.grid = 1
  ncol.grid = numberCategories

  # set xlim & ylim
  if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
    x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])}
  else {    xlim = c(0.1,max(obs.mat$x,na.rm=TRUE))
  }

  if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
    y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
  } else {
    ylim = c(min(pi.mat$binf0025),max(pi.mat$bsup0975))
  }

  # intersection area between ICs and curves to fill intersection area
  n_interp = 500 # nb of point for interpolation

  # loop over the covariate
  for (iter in 1:numberCategories){

    plotdata = pi.mat[which(pi.mat$category==namesCategories[iter]),]

    # data loq
    loq = unique(obs.mat$loq)

    # interpolation des prediction pour filling area outliers
    interp1 <- approx(plotdata$xcent, plotdata$per.min,  n = n_interp)
    interp2 <- approx(plotdata$xcent, plotdata$bsup0025, n = n_interp)
    interp3 <- approx(plotdata$xcent, plotdata$binf0025, n = n_interp)

    plotdatainterpol1 <- data.frame(interp1$x, interp1$y,
                                    interp2$x, interp2$y,
                                    interp3$x, interp3$y)

    interp1 <-  approx(plotdata$xcent, plotdata$per.med, n = n_interp)
    interp2 <-  approx(plotdata$xcent, plotdata$bsup05,  n = n_interp)
    interp3 <-  approx(plotdata$xcent, plotdata$binf05,  n = n_interp)

    plotdatainterpol2 <- data.frame(interp1$x,interp1$y,
                                    interp2$x,interp2$y,
                                    interp3$x,interp3$y)

    interp1 <- approx(plotdata$xcent, plotdata$per.max,  n = n_interp)
    interp2 <- approx(plotdata$xcent, plotdata$bsup0975, n = n_interp)
    interp3 <- approx(plotdata$xcent, plotdata$binf0975, n = n_interp)

    plotdatainterpol3 <- data.frame(interp1$x, interp1$y,
                                    interp2$x, interp2$y,
                                    interp3$x, interp3$y)

    colnames(plotdatainterpol1) = c("x_area_0.25","y_area_0.25","X0.025.1","Y0.025.1","X0.025","Y0.025")
    colnames(plotdatainterpol2) = c("x_area_0.5","y_area_0.5","X0.5.1","Y0.5.1", "X0.5","Y0.5")
    colnames(plotdatainterpol3) = c("x_area_0.975","y_area_0.975","X0.975.1","Y0.975.1","X0.975", "Y0.975")

    if (plot.opt$plot.box==TRUE) {

      p <- ggplot(obs.mat, aes(x = cov, y = y)) +

        theme_bw() +

        theme(plot.title = element_text(hjust = 0.5),

              panel.background=element_rect("white"),

              aspect.ratio = 1,

              axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
              axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

              axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
              axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

              panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid)) +

        # box pot
        stat_boxplot(geom ='errorbar', width = 0.5) +
        expand_limits(y = 0) +
        geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
        geom_jitter(width = 0.2,colour= "blue") +
        stat_summary(fun.y=mean, colour="red", geom="point", shape=18, size=3,show_guide = FALSE)+
        stat_summary(fun.y=mean, colour="red", geom="text", show_guide = FALSE, vjust=-0.7, aes( label=round(..y.., digits=1)))+

        xlab(unique(pi.mat$covariate)) +

        scale_y_continuous(plot.opt$which,
                           limits = c(0,1.01*max(obs.mat$y)),
                           expand = expand_scale(mult = c(0, 0)),
                           scales::pretty_breaks(n = plot.opt$breaks.y))


      list_plot[[1]] <- p
      ncol.grid = 1

      # title for graph
      plot.opt$main = ""

    }else{


      # title for graph
      plot.opt$main = unique(pi.mat$covariate)

      # ggplot template
      p <- ggplot(plotdata, aes(x = xcent)) +

        # theme of the ggplot template
        theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),

              axis.title.x = element_text(size = plot.opt$size.xlab),
              axis.title.y = element_blank(),

              axis.text.x = element_text(size=plot.opt$size.text.x),
              axis.text.y = element_text(size=plot.opt$size.text.y),

              axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
              axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

              panel.background=element_rect("white"),
              panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))  +

        # sub title for each subplot : covariate + category
        { if (numberCategories>1) ggtitle(paste0(namesCategories[iter]))} +

        # coordinates x-y
        coord_cartesian(xlim=xlim, ylim=ylim) +
        expand_limits(x = 0, y = 0) +

        # bands
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(aes(ymin = binf0025, ymax = bsup0025), fill = plot.opt$fill.bands, alpha = plot.opt$alpha.bands) } +
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(aes(ymin = binf05,   ymax = bsup05),   fill = plot.opt$fill.med, alpha = plot.opt$alpha.med) } +
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(aes(ymin = binf0975, ymax = bsup0975), fill = plot.opt$fill.bands, alpha = plot.opt$alpha.bands) } +

        # fill intersection area as outliers
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(plotdatainterpol1,
                      mapping = aes(x = x_area_0.25, ymin = y_area_0.25, ymax = pmin(Y0.025.1, y_area_0.25)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        { if ( plot.opt$bands == TRUE )
          geom_ribbon(plotdatainterpol1,
                      mapping = aes(x = x_area_0.25, ymin = y_area_0.25, ymax = pmax(Y0.025, y_area_0.25)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        { if ( plot.opt$bands == TRUE )
          geom_ribbon(plotdatainterpol2,
                      mapping = aes(x = x_area_0.5, ymin = y_area_0.5,ymax = pmin(Y0.5.1, y_area_0.5)),
                      fill = plot.opt$fill.outliers.med, alpha = plot.opt$alpha.outliers.med) } +

        { if ( plot.opt$bands == TRUE )
          geom_ribbon(plotdatainterpol2,
                      mapping = aes(x = x_area_0.5,ymin = y_area_0.5,ymax = pmax(Y0.5, y_area_0.5)),
                      fill = plot.opt$fill.outliers.med, alpha = plot.opt$alpha.outliers.med) } +

        { if ( plot.opt$bands == TRUE )
          geom_ribbon(plotdatainterpol3,
                      mapping = aes(x = x_area_0.975, ymin = y_area_0.975, ymax = pmin(Y0.975.1, y_area_0.975)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        { if ( plot.opt$bands == TRUE )
          geom_ribbon(plotdatainterpol3,
                      mapping = aes(x = x_area_0.975,ymin = y_area_0.975,ymax = pmax(Y0.975, y_area_0.975)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        # plot predictions
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = per.min), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.band)+#} +
        #  {if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = per.med), linetype = plot.opt$lty.med,colour = plot.opt$col.med,size = plot.opt$lwd.med)+#} +
        # {if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = per.max), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.band)+#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = binf0025), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.band)+#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = bsup0025), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.band)+#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = binf05), linetype = plot.opt$lty.med,colour = plot.opt$col.med,size = plot.opt$lwd.med)+#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = bsup05), linetype = plot.opt$lty.med,colour = plot.opt$col.med,size = plot.opt$lwd.med)+#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = binf0975), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.band)+#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = bsup0975), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.band)+#} +

        # plot lines bnds
        geom_line(pi.mat, mapping = aes(y = dotline.min), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.bands)+
        geom_line(pi.mat, mapping = aes(y = dotline.med), linetype = plot.opt$lty.med,colour   = plot.opt$col.med,size   = plot.opt$lwd.med)+
        geom_line(pi.mat, mapping = aes(y = dotline.max), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,size = plot.opt$lwd.bands) +

        # plot loq
        { if (plot.opt$line.loq == TRUE & !is.na(loq) )
          geom_line(aes(y = loq),
                    colour = plot.opt$col.line.loq,
                    size = plot.opt$lwd.line.loq,
                    linetype = plot.opt$lty.line.loq)} +

        # plot non censored data
        { if ( plot.opt$plot.obs == TRUE )
          geom_point( plotdatapoint, mapping = aes( x = x1, y = y1 ),
                      color = plot.opt$col.pobs,
                      shape = plot.opt$pch.pobs,
                      size = plot.opt$size.pobs ) } +

        # plot censored data
        { if ( plot.opt$plot.obs == TRUE )
          geom_point( plotdatapoint2, mapping = aes( x = x2, y = y2 ),
                      color = plot.opt$col.pcens,
                      shape =  plot.opt$pch.pcens,
                      size = plot.opt$size.pcens ) } +

        # plot for ipred
        { if ( plot.opt$impute.loq == TRUE )
          geom_point( plotImputedDatatUnderLOQ, mapping = aes( x = x, y = ipred ),
                      color =  plot.opt$col.pcens,
                      shape =  plot.opt$pch.pcens,
                      size = plot.opt$size.pcens )} +

        # plor for data under loq
        { if ( plot.opt$plot.loq == TRUE && length( plotDataUnderLOQ$loq ) !=0 )
          geom_point( plotDataUnderLOQ, mapping = aes( x = x, y = y ),
                      color =  plot.opt$col.pcens,
                      shape =  plot.opt$pch.pcens,
                      size = plot.opt$size.pcens )} +

        # x-y scales

        {
          if (plot.opt$xlog == FALSE)

            scale_x_continuous(plot.opt$xlab,
                               scales::pretty_breaks(n = plot.opt$breaks.x))
        } +

        {
          if (plot.opt$ylog == FALSE)
            scale_y_continuous(plot.opt$ylab,
                               scales::pretty_breaks(n = plot.opt$breaks.y))
        } +

        {
          if (plot.opt$xlog == TRUE)
            scale_x_log10(plot.opt$xlab,
                          breaks = scales::trans_breaks("log10", function(x) 10 ^ x),
                          labels = scales::trans_format("log10", scales::math_format(10 ^ .x)))
        } +

        {
          if (plot.opt$ylog == TRUE)
            scale_y_log10(plot.opt$ylab,
                          breaks = scales::trans_breaks("log10", function(x) 10 ^ x),
                          labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
            )
        } +

        # if log scales plot logticks
        { if (plot.opt$xlog == TRUE) annotation_logticks(sides = "b")} +
        { if (plot.opt$ylog == TRUE) annotation_logticks(sides = "l")}

      # list of ggplot
      list_plot[[iter]] <- p

    }}

  # to plot in thr waffle plot
  if (plot.opt$plot.default==TRUE){

    return(p)

  }else{

    # finally plot
    grid.arrange(grobs = list_plot,
                 nrow = nrow.grid,
                 ncol = ncol.grid,
                 top = textGrob(paste0(plot.opt$main,'\n'),
                                vjust = 1,
                                gp = gpar(fontsize=plot.opt$size.main)),
                 left = textGrob(plot.opt$ylab,rot=90,vjust=1))
  }

  ##########################################################################################################################################################################
} # END FUNCTION
##########################################################################################################################################################################



# # -----------------------------------------------------------------------
#
# # aux.npdeplot.main
#
# # -----------------------------------------------------------------------
#
#aux.npdeplot.main <- function(npdeObject, plmat, plot.opt,  namcat = NULL, msim = NULL, ref.prof = NULL, sim.ypl = NULL, dotline, distrib = "norm", onlog = FALSE) {

aux.npdeplot.main <- function(npdeObject, plmat, plot.opt, dotline, distrib = "norm", onlog = FALSE) {

  # prediction interval matrix
  pi.mat = list()

  namesBoundsData = apply(expand.grid(c("0.025","0.5","0.975"), c("binf","bmed","bsup")), 1, paste, collapse=".")

  # name for pi.mat
  namesCov = as.factor(unique( plmat$cov))


  ### begin loop icat
  list_plmat1 = list()
  list_percobs = list()
  list_bnds = list()
  list_dotline = list()

  if (is.numeric(plmat$x)) {

    # ECO TODO: should binning be done on full data or after censoring ?

    xbin <- npde.binning(plmat$x,
                         plot.opt,
                         verbose = plot.opt$interactive)

    plmat <- data.frame(grp = xbin$xgrp,
                        plmat)

    xcent <- xbin$xat

  } else {

    plmat <- data.frame(plmat$x,
                        grp = plmat$x,
                        plmat)

    xcent <- unique(plmat$x)

  }

  xcent = plmat$xcent

  xlab <- as.character(1:length(xcent))
  zecat <- unique(plmat$cov)
  ncat <- length(zecat)

  #  if (!is.null(msim)) {
  #    nrep <- dim(msim)[1] / dim(plmat)[1]
  #    msim <- cbind(msim, grp = rep(xbin$xgrp, nrep))
  #    mbin <- data.frame(xat = xbin$xat,
  #                       grp = names(xbin$xat),
  #                       xlab = xlab)
  #  } else {

  #if (!is.null(sim.ypl)) {
  #   nrep <- length(sim.ypl) / dim(plmat)[1]
  #   }


  covsplit = plot.opt$covsplit
  #  if (ncat == 1)
  #    covsplit <- FALSE
  #  else
  #    covsplit <- TRUE

  #  if (!is.null(msim))
  #    mpref <-
  #    aux.npdeplot.meanprof(mbin, msim[msim$use == 1, ])
  #  else
  #   mpref <- NULL

  mpref = NULL

  #ix <-   c(grep("col", names(plot.opt)), grep("lty", names(plot.opt)), grep("lwd", names(plot.opt)))
  #for (icol in ix)
  #plot.opt[[icol]] <- rep(plot.opt[[icol]], ncat)

  for (icat in 1:ncat) {

    # tit <- plot.opt$main
    # if (tit == "")
    #   tit <- namcat[icat]
    # if (covsplit & is.null(ref.prof)) {
    is.cat <- (plmat$cov == zecat[icat])
    #   if (!is.null(msim))
    #     mpref <- aux.npdeplot.meanprof(mbin, msim[rep(is.cat, nrep), ])
    # } else {
    #   is.cat <- rep(TRUE, dim(plmat)[1])
    #   if (plot.opt$main == "" & !is.null(ref.prof)) {
    #     if (icat == 1) {
    #       cat("The plot uses a reference profile\n")
    #       if (covsplit)
    #         tit <- paste("Reference profile:", tit, sep = "")
    #     }
    #   }
    # }

    #plot.opt2 <- plot.opt
    #for (icol in ix)
    #plot.opt2[[icol]] <- plot.opt[[icat]]

    plmat1 <- plmat[is.cat, ]

    xcal <-
      aux.npdeplot.transform(
        plmat1,
        plot.opt,
        xat = xcent[xlab %in% plmat1$grp],
        mpref = mpref,
        distrib = distrib,
        onlog = onlog
      )

    plmat1 <- xcal$plmat
    percobs <- xcal$percobs

    #if (!is.null(mpref))
    #  xat <-
    #  mpref$xat[xlab %in% plmat1$grp]
    #else
    xat <- xcent[xlab %in% plmat1$grp]


    #  if (!is.null(sim.ypl))
    #    sim.ypl1 <- sim.ypl[rep(is.cat, nrep)]
    #  else
    #    sim.ypl1 <- NULL
    sim.ypl1 <- NULL

    xcal <-
      aux.npdeplot.computepi(
        plmat1,
        plot.opt,
        xlab = xlab,
        xat = xcent,
        mpref = mpref,
        dotline = dotline,
        sim.ypl = sim.ypl1,
        distrib = distrib,
        onlog = onlog
      )

    ### loop to ggplot grid

    l = list(xcal$bnds$binf,
             xcal$bnds$bmed,
             xcal$bnds$bsup)

    pi.mat[[icat]] = as.data.frame(Reduce(cbind,l))

    pi.mat[[icat]] = cbind(rep(namesCov[[icat]],
                               length(unique(plmat$grp))),
                           rep(plot.opt$which.cov,
                               length(unique(plmat$grp))),
                           pi.mat[[icat]])

    pi.mat[[icat]] = cbind(1:length(unique(plmat$grp)),
                           pi.mat[[icat]],
                           percobs)


  }	### end loop icat

#  pi.mat = rbindlist(pi.mat)
  pi.mat <- do.call("rbind", pi.mat) # or integrate that during the previous loop, ie create a data frame and rbind it to one of the previous iteration

  colnames(pi.mat) = c("group",
                       "covariate",
                       "category",
                       namesBoundsData,
                       c("per.min","per.med","per.max"))

  return(pi.mat)

  #######################################################################
} # end function
#######################################################################

