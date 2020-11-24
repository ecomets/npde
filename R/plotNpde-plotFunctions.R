##################################################################################
#' Select plot for a NpdeObject object
#'
#' Select plot for a NpdeObject object
#'
#' @usage npde.plot.select(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,
#' x.scatter=FALSE,pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.x.scatter=FALSE,
#' cov.pred.scatter=FALSE,cov.x.box=FALSE,cov.pred.box=FALSE, cov.ecdf=FALSE, vpc=FALSE,...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param data boolean, whether to produce a plot of the data
#' @param ecdf boolean, whether to produce a distribution plot of the empirical distribution function
#' @param qqplot boolean, whether to produce a QQ-plot of the empirical distribution function
#' @param histogram boolean, whether to produce a histogram of the metric
#' @param x.scatter boolean, whether to produce a scatterplot of the metric as a function of X
#' @param pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions
#' @param x.box boolean, whether to produce whisker plots of the metric as a function of X
#' @param pred.box boolean, whether to produce whisker plots of the metric as a function of predictions
#' @param cov.x.scatter boolean, whether to produce a scatterplot of the metric as a function of X, split by covariate(s)
#' @param cov.pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions, split by covariate(s)
#' @param cov.x.box boolean, whether to produce whisker plots of the metric as a function of X, split by covariate(s)
#' @param cov.pred.box boolean, whether to produce whisker plots of the metric as a function of predictions, split by covariate(s)
#' @param cov.ecdf boolean, whether to produce a distribution plot of the empirical distribution function, split by covariate(s)
#' @param vpc boolean, whether to produce a VPC
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords plot
#' @export

npde.plot.select<-function(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,x.scatter=FALSE,pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.x.scatter=FALSE,cov.pred.scatter=FALSE,cov.x.box=FALSE,cov.pred.box=FALSE, cov.ecdf=FALSE, vpc=FALSE,...) {
  # Function selecting which plots are to be drawn
  namObj<-deparse(substitute(npdeObject))
  interactive<-npdeObject["prefs"]$interactive
  # ECO TODO: replace with partial matching
  if(data) plot(npdeObject,plot.type="data",...)
  if(ecdf) plot(npdeObject,plot.type="ecdf",...)
  if(qqplot) plot(npdeObject,plot.type="qqplot",...)
  if(histogram) plot(npdeObject,plot.type="histogram",...)
  if(x.scatter) plot(npdeObject,plot.type="x.scatter",...)
  if(pred.box) plot(npdeObject,plot.type="pred.scatter",box=TRUE,...)
  if(x.box) plot(npdeObject,plot.type="x.scatter",box=TRUE,...)
  if(pred.scatter) plot(npdeObject,plot.type="pred.scatter",...)
  if(cov.x.scatter) plot(npdeObject,plot.type="cov.x.scatter",...)
  if(cov.pred.scatter) plot(npdeObject,plot.type="cov.pred.scatter",...)
  if(cov.ecdf) plot(npdeObject,plot.type="cov.ecdf",...)
  if(cov.x.box) plot(npdeObject,plot.type="cov.x.scatter",box=TRUE,...)
  if(cov.pred.box) plot(npdeObject,plot.type="cov.pred.scatter",box=TRUE,...)
  if(vpc) plot(npdeObject,plot.type="vpc",...)
}

#' Default plots for a NpdeObject object
#'
#' Default plots for a NpdeObject object
#'
#' @usage default.npde.plots(npdeObject, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot internal

#### Meta-niveau
default.npde.plots<-function(npdeObject,...) {
  # When plot(npdeObject) is called without plot.type
  par(mfrow=c(2,2),ask=npdeObject["prefs"]$ask)
  npde.plot.select(npdeObject,qqplot=TRUE,histogram=TRUE, x.scatter=TRUE,pred.scatter=TRUE,new=FALSE,...)
}

#' Covariate plots for a NpdeObject object
#'
#' Covariate plots for a NpdeObject object
#'
#' @usage npde.plot.covariates(npdeObject, which="x", ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which one of "x" (scatterplots of the metric versus X), "pred" (scatterplots of the metric versus predictions) or "ecdf" (empirical distribution function)
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot
#' @export

npde.plot.covariates<-function(npdeObject,which="x",...) {
  # Parameters or random effects versus covariates
  if(which=="x") {
    plot(npdeObject,plot.type="cov.x.scatter",...)
  }
  if(which=="pred") {
    plot(npdeObject,plot.type="cov.pred.scatter",...)
  }
  if(which=="ecdf") {
    plot(npdeObject,plot.type="cov.ecdf",...)
  }
}

#' Plots for pd and npde
#'
#' Plots for pd and npde
#'
#' @aliases npde.plot.pd npde.plot.npde
#' @usage npde.plot.pd(npdeObject, ...)
#' @usage npde.plot.npde(npdeObject, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot
#' @export

npde.plot.npde<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for npde\n")
  default.npde.plots(npdeObject,...)
}

#' @export

npde.plot.pd<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for pd\n")
  default.npde.plots(npdeObject,which="pd",...)
}

################################    Data    #####################################

#' Plot a NpdeData object
#'
#' Produces a spaghetti plot of the data
#'
#' @aliases npde.plot.data
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot
#' @export

npde.plot.data<-function(npdeObject,...) {

## to do
## npdeData = npdeObject["data"]
## plot npdeData avec prefs npdeObject
## ensuite changer options

# method plot.npde.data(x,y,...)
# list(..) = list de preferences
# ensuite remplacer les prefs par list(...)

# but plot plot(yvir50@data) = plot(yvir50,plot.type=“data”)


  # data censored / no censored
  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
    icens<-npdeObject["data"]["icens"]
    is.cens<-npdeObject["data"]["data"]$cens==1
  } else { has.cens<-FALSE}

  # data plot x,y and id
  x<-npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]]
  y<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  plot.opt<-npdeObject["prefs"]
  id<-(npdeObject["data"]["data"]$index %in% plot.opt$ilist)

  if(plot.opt$impute.loq & length(npdeObject["results"]["res"]$ycomp)>0 & npdeObject["options"]["cens.method"]!="omit"){
    y<-npdeObject["results"]["res"]$ycomp
  }

  # plot options
  dots.plot.opt = list(...)
  plot.opt<-set.plotoptions.default(npdeObject)
  plot.opt <- modifyList(plot.opt, dots.plot.opt[intersect(names(dots.plot.opt), names(plot.opt))])


  if (length(intersect(dots.plot.opt,"col.lobs"))==0){
    plot.opt$col.lobs = plot.opt$col
  }

  if (length(intersect(dots.plot.opt,"lwd.lobs"))==0){
    plot.opt$lwd.lobs = plot.opt$lwd
  }

  if (length(intersect(dots.plot.opt,"lty.lobs"))==0){
    plot.opt$lty.lobs = plot.opt$lty
  }

  if (length(intersect(dots.plot.opt,"pch.pobs"))==0){
    plot.opt$pch.pobs <- plot.opt$pch
  }

  if (length(intersect(dots.plot.opt,"pch.pcens"))==0){
    plot.opt$pch.pcens = plot.opt$pch
  }

  # -----------------------------------------------------------------------------------------------------------------

  if(has.cens) {

    xplot = x[id & !is.cens]
    yplot = y[id & !is.cens]
    grouplot = npdeObject@data@data$index[id & !is.cens]
    dataplot = data.frame(grouplot,xplot,yplot)
    colnames(dataplot) = c("group","x","y")

    dataplot_bis = dataplot[dataplot$y<npdeObject@data@loq,]
    colnames(dataplot_bis) = c("group","x","y")

    group_loq_plot = npdeObject@data@data$index[id & is.cens]
    dataloq_plot = data.frame(group_loq_plot, x[id & is.cens],y[id & is.cens])
    colnames(dataloq_plot) = c("group","x","y")

    dataplot = rbind(dataplot,dataplot_bis,dataloq_plot)

    #  xlim and ylim
    if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
      x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
    } else {
      x.limits = 1.01*c(0,max(dataplot$x,na.rm = TRUE))}

    if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
      y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
    } else {
      y.limits = c(min(dataloq_plot$y, dataplot$y,na.rm = TRUE),max(dataloq_plot$y, dataplot$y,na.rm = TRUE))}

    # ggplot template
      p = ggplot(dataplot, aes(x=x, y=y)) +

      theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
            axis.title.x = element_text(size = plot.opt$size.xlab),
            axis.title.y = element_text(size = plot.opt$size.ylab),

            #axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
            #axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

            panel.background=element_rect("white"),
            panel.grid.major = element_line(ifelse(plot.opt$grid==TRUE,"grey","white")),
            panel.grid.minor = element_line(ifelse(plot.opt$grid==TRUE,"grey","white"))) +

      ggtitle(plot.opt$main) +

      coord_cartesian(xlim=x.limits, ylim=y.limits) +

      geom_point(dataplot,
                 mapping=aes(x=x,y=y),
                 color = plot.opt$col.pobs,
                 alpha = plot.opt$alpha.pobs,
                 size = plot.opt$size.pobs,
                 shape = plot.opt$pch.pobs)  +

      geom_line(aes(group=group),
                linetype = plot.opt$lty,
                color = plot.opt$col,
                alpha = plot.opt$alpha,
                size = plot.opt$lwd )+

      {if(plot.opt$plot.loq==TRUE)

        geom_point(dataplot_bis,
                   mapping=aes(x=x,y=y),
                   color = plot.opt$col.pcens,
                   shape = plot.opt$pch.pcens,
                   size = plot.opt$size.pcens,
                   alpha = plot.opt$alpha.pcens )}+

          {if(plot.opt$plot.loq==TRUE)

        geom_point(dataloq_plot,
                   mapping = aes(x=x,y=y),
                   color = plot.opt$col.pcens,
                   shape = plot.opt$pch.pcens,
                   size = plot.opt$size.pcens,
                   alpha = plot.opt$alpha.pcens)} +

      scale_x_continuous(npdeObject@data@name.predictor, scales::pretty_breaks(n = plot.opt$breaks.x)) +
      scale_y_continuous(npdeObject@data@name.response, scales::pretty_breaks(n = plot.opt$breaks.y))
    print(p)

  }# end if cens

  if(!has.cens) {

    xplot = x[id]
    yplot = y[id]
    grouplot = npdeObject@data@data$index[id]
    dataplot = data.frame(grouplot,xplot,yplot)
    colnames(dataplot) = c("group","x","y")

    # xlim and ylim
    if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
      x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
    } else {
      x.limits = 1.01*c(0,max(dataplot$x,na.rm = TRUE))}

    if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
      y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
    } else {
      y.limits = c(min(dataplot$y,na.rm = TRUE),max(dataplot$y,na.rm = TRUE))}

    ## ggplot template

    p = ggplot(dataplot, aes(x=x, y=y)) +

      theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
            axis.title.x = element_text(size = plot.opt$size.xlab),
            axis.title.y = element_text(size = plot.opt$size.ylab),

            axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
            axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),
            panel.background=element_rect("white"),
            panel.grid.major = element_line(ifelse(plot.opt$grid==TRUE,"grey","white")),
            panel.grid.minor = element_line(ifelse(plot.opt$grid==TRUE,"grey","white"))) +

      ggtitle(plot.opt$main) +

      coord_cartesian(xlim=x.limits, ylim=y.limits) +

      geom_point(color = plot.opt$col.pobs,
                 alpha = plot.opt$alpha.pobs,
                 size = plot.opt$size.pobs,
                 shape = plot.opt$pch.pobs)  +

      geom_line(aes(group=group),
                linetype = plot.opt$lty,
                color = plot.opt$col,
                alpha = plot.opt$alpha,
                size = plot.opt$lwd )+

      scale_x_continuous(plot.opt$xlab, scales::pretty_breaks(n = plot.opt$breaks.x)) +
      scale_y_continuous(plot.opt$ylab, scales::pretty_breaks(n = plot.opt$breaks.y)) +

      xlab(npdeObject@data@name.predictor) +
      ylab(npdeObject@data@name.response)

    print(p)

  }

} # end function

################################    Default gof plots  #################################

#' Diagnostic plots
#'
#' The default diagnostic plots produced after a call to \code{\link{npde}} or \code{\link{autonpde}} include a histogram of the distribution, a QQ-plot compared to the theoretical distribution, and scatterplots versus the independent variable and versus the population predictions from the model
#'
#' @usage npde.plot.default(npdeObject, ...)
#'
#' @aliases plot aux.scatter.box aux.scatter compute.bands.true compute.bands aux.npdeplot.meanprof aux.npdeplot.computepi aux.npdeplot.transform aux.npdeplot.plot aux.npdeplot.main
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @export
#' @keywords plot
## #' @keywords plot by default : qqplot, hist, x.scatter, pred.scatter

npde.plot.default<-function(npdeObject,  ...) {

  # remove the covariables for the waffle plot by defaults
  if (length(npdeObject@data@name.covariates)!=0){

    new_npdeObject = npdeObject
    drops = new_npdeObject@data@name.covariates
    new_npdeObject@data@data <- new_npdeObject@data@data[ , !(names(new_npdeObject@data@data) %in% drops)]

  }else{

    new_npdeObject = npdeObject

  }

  # modify the plot options with the user ones
  dots.plot.opt = list(...)
  plot.opt <- set.plotoptions.default(new_npdeObject)
  plot.opt <- modifyList(plot.opt, dots.plot.opt[intersect(names(dots.plot.opt), names(plot.opt))])

  if (length(intersect(dots.plot.opt,"col.lobs"))==0){
    plot.opt$col.lobs = plot.opt$col
  }

  if (length(intersect(dots.plot.opt,"lwd.lobs"))==0){
    plot.opt$lwd.lobs = plot.opt$lwd
  }

  if (length(intersect(dots.plot.opt,"lty.lobs"))==0){
    plot.opt$lty.lobs = plot.opt$lty
  }

  if (length(intersect(dots.plot.opt,"pch.pobs"))==0){
    plot.opt$pch.pobs <- plot.opt$pch
  }

  if (length(intersect(dots.plot.opt,"pch.pcens"))==0){
    plot.opt$pch.pcens = plot.opt$pch
  }

  hist <- npde.plot.dist(new_npdeObject,plot.opt$which,dist.type="hist",plot.default=TRUE,...)
  qqplot <- npde.plot.dist(new_npdeObject,plot.opt$which,dist.type="qqplot",plot.default=TRUE,...)
  x.scatter <- npde.plot.meanprofile(new_npdeObject,plot.opt$which, xaxis="x", plot.default=TRUE,...)
  pred.scatter <- npde.plot.meanprofile(new_npdeObject,plot.opt$which, xaxis="pred", plot.default=TRUE, ...)

  grid.arrange(hist, qqplot, x.scatter, pred.scatter, nrow=2, ncol=2,top="")

} # end function npde.plot.default

#################    	     Distributions - npde/pd          ###################

#' Plots of pd/npde versus their theoretical distribution
#'
#' Produces a plot of the corresponding metric versus their theoretical distribution as a histogram,a QQ-plot, or the empirical cdf
#'
#' @usage npde.plot.dist(npdeObject,which="npde",dist.type="qqplot",
#' covsplit=FALSE,...)
#'
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which a string determining which metric to plot, one of "npde", "pd" or "npd" (defaults to "npde")
#' @param dist.type string, one of "ecdf" (empirical cumulative density function), "hist" (histogram) or "qqplot" (QQ-plot of the empirical distribution versus the theoretical quantiles) to determine which type of plot (default is "qqplot")
#' @param covsplit boolean. If TRUE and covariates are present in the dataset, the plots will be stratified for each covariate
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}}).
#'
#' @details
#' The size.pobs and size.pcens control the size of the plotting symbols for respectively uncensored and censored observations.
#' For distribution plots they are in fact multiplied by a 2/3 factor so that they appear to have a similar size to the observations in the scatterplots.
#'
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords plot
#' @export

npde.plot.dist<-function(npdeObject,which="npde",dist.type="qqplot",covsplit=FALSE,...) {

  # list pour les options
  dots.plot.opt = list(...)
  plot.opt<-npdeObject["prefs"]

  # list pour récupérer data sur covariates
  plmat_cov = list()

  #test for covariate or not

  plot.opt.defaults<-function(plot.opt,which="npde",dist.type="qqplot") {

    if(dist.type=="ecdf") {
      plot.opt$ylab<-"Empirical cdf"
      plot.opt$ylim<-c(0,1)
      plot.opt$xlab<-switch(which,pd="Sample quantiles (pd)",npd="Sample quantiles (npd)",npde="Sample quantiles (npde)")
    }

    if(dist.type=="qqplot") {
      plot.opt$xlab<-"Theoretical Quantiles"
      plot.opt$ylab<-switch(which,pd="Sample quantiles (pd)",npd="Sample quantiles (npd)",npde="Sample quantiles (npde)")
      if(which %in% c("npd","npde")) {
        plot.opt$main<-paste("Q-Q plot versus N(0,1) for ",which)
      } else {
        plot.opt$main<-"Q-Q plot versus U(0,1) for pd"
      }
    }

    if(dist.type=="hist") {
      plot.opt$ylab<-"Frequency"
      plot.opt$xlab<-switch(which,pd="Sample quantiles (pd)",npd="Sample quantiles (npd)",npde="Sample quantiles (npde)")
    }
    return(plot.opt)
  }

  args1<-match.call(expand.dots=TRUE)
  i1<-match("main",names(args1))


  if(!is.na(i1)) {
    change.main<-TRUE
  } else change.main<-FALSE

  i1<-match("ncat",names(args1))

  if(!is.na(i1)) {
    change.ncat<-TRUE
  } else change.ncat<-FALSE

  if(match(which,c("npde","pd","npd"),nomatch=0)==0) {
    cat("Option which=",which,"not recognised\n")
    return()
  }

  if(match(dist.type,c("ecdf","qqplot","hist"),nomatch=0)==0) {
    cat("Option dist.type=",dist.type,"not recognised\n")
    return()
  }

  if(which=="npde" & length(npdeObject["results"]["res"]$npde)==0)  {
    cat("    Missing npde object to plot.\n")
    return()
  }

  if(which %in% c("pd","npd") & length(npdeObject["results"]["res"]$pd)==0) {
    cat("    Missing pd object to plot.\n")
    return()
  }

  if(which=="pd") distrib<-"unif" else distrib<-"norm"

  if(length(npdeObject["data"]["icens"])>0) has.cens<-TRUE else has.cens<-FALSE

  # for plot by default when covariate
  # ie for boxplot with covariates

  plot.opt<-set.plotoptions.default(npdeObject)
  plot.opt <- modifyList(plot.opt, dots.plot.opt[intersect(names(dots.plot.opt), names(plot.opt))])

 if(covsplit==FALSE & plot.opt$plot.default==TRUE) {
    covsplit<-FALSE
 }

  else if(covsplit==TRUE & length(npdeObject["data"]["name.covariates"])==0) {
    print("No covariates in the dataset\n")
    covsplit<-FALSE
  }
  else if(covsplit==FALSE &  (length(dots.plot.opt)!=2) &length(npdeObject["data"]["name.covariates"])!=0) {
    covsplit<-TRUE
  }
  # case for plot without optio,s, only type.plot
  else if( (length(dots.plot.opt)==2) & (covsplit==FALSE) & (length(npdeObject["data"]["name.covariates"])!=0)) {

    covsplit==FALSE
  }

  if(covsplit) {

    if(plot.opt$which.cov=="") plot.opt$which.cov<-"all"

    if(is.numeric(plot.opt$which.cov)) plot.opt$which.cov<-npdeObject["data"]["name.covariates"][plot.opt$which.cov]

    if(plot.opt$which.cov=="all") lcov<-npdeObject["data"]["name.covariates"] else {
      icov<-match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
      lcov<-npdeObject["data"]["name.covariates"][icov]
    }

    ncov<-length(lcov)

    if(ncov==0) {
      if(npdeObject@options$verbose) cat("Cannot find covariate(s)", plot.opt$which.cov,"in the dataset\n")
      covsplit<-FALSE
    }
  } else ncov<-0

  sim.ypl<-NULL

  if(!plot.opt$approx.pi) {
    if(which %in% c("pd","npd")) {
      if(length(npdeObject["results"]["pd.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated pd are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["pd.sim"]
    }
    if(which=="npde") {
      if(length(npdeObject["results"]["npde.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated npde are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["npde.sim"]
    }
  }

  if(which=="npde") ypl<-npdeObject["results"]["res"]$npde

  if(which %in% c("pd","npd")) ypl<-npdeObject["results"]["res"]$pd

  if(which=="npd") ypl<-qnorm(ypl)

  if(has.cens) plmat<-data.frame(xpd=ypl, cens=npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]) else plmat<-data.frame(xpd=ypl,cens=rep(0,length(ypl)))

  keep<-npdeObject["data"]["not.miss"]

  if(npdeObject["options"]["cens.method"]=="omit" & has.cens){
    keep<-(npdeObject["data"]["not.miss"] & npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]==0)
  }

  idobs<-npdeObject["data"]["data"][,npdeObject["data"]["name.group"]]
  idobs<-idobs[keep]
  plmat<-plmat[keep,]

  plot.opt<-set.plotoptions.default(npdeObject)
  plot.opt <- modifyList(plot.opt, dots.plot.opt[intersect(names(dots.plot.opt), names(plot.opt))])


  if (length(intersect(dots.plot.opt,"col.lobs"))==0){
    plot.opt$col.lobs = plot.opt$col
  }

  if (length(intersect(dots.plot.opt,"lwd.lobs"))==0){
    plot.opt$lwd.lobs = plot.opt$lwd
  }

  if (length(intersect(dots.plot.opt,"lty.lobs"))==0){
    plot.opt$lty.lobs = plot.opt$lty
  }

  if (length(intersect(dots.plot.opt,"pch.pobs"))==0){
    plot.opt$pch.pobs <- plot.opt$pch
  }

  if (length(intersect(dots.plot.opt,"pch.pcens"))==0){
    plot.opt$pch.pcens = plot.opt$pch
  }
  # Ajustement de la taille des points pour qu'ils correspondent dans les différents graphes
  plot.opt$pch.pobs<-plot.opt$pch.pobs*2/3
  plot.opt$pch.pcens<-plot.opt$pch.pcens*2/3

  # -----------------------------------------------------------------------------------------------------------------

  if(covsplit==TRUE) {

    for(icov in 1:ncov) {

      namcov<-lcov[icov]
      zecov<-npdeObject["data"]["data"][keep,namcov]
      idobs<-npdeObject["data"]["data"][keep,npdeObject["data"]["name.group"]]
      ucov<-zecov[match(unique(idobs),idobs)]

      if(!is.numeric(ucov) & length(unique(ucov))<=4) {

        covcont<-FALSE
        ncat<-length(unique(ucov))
        namcat<-paste(namcov,sort(unique(zecov)),sep="=")

      } else {

        covcont<-TRUE

        if(change.ncat | plot.opt$ncat!=3) {
          ncat<-plot.opt$ncat
          seqcat<-seq(0,1,length.out=(ncat+1))
          zecov<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)
          nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
          namcat<-paste(namcov,nam1,sep=": ")

        } else {

          zecov<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
          ncat<-3
          namcat<-paste(namcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
        }
      }

      zecat<-sort(unique(zecov))

      plmat1<-data.frame(plmat,cov=zecov)

      for(ic in 1:length(zecat)) {

        plmat_cov[[ic]] <- plmat1[plmat1$cov==zecat[ic],]
      }

      if(dist.type=="hist" & plot.opt$which.cov==namcov){


        output <- aux.plot.hist(plmat_cov,namcat, plot.opt,distrib=distrib, nclass=plot.opt$bin.number, sim.ypl=sim.ypl[plmat1$cov==zecat[ic],])
        return(output)

      }else{

        if ((dist.type=="qqplot") || (dist.type=="ecdf")){

          aux.plot.dist(plmat_cov,namcat, plot.opt, dist.type=dist.type,distrib=distrib,nrep=npdeObject["sim.data"]["nrep"], ties=npdeObject["options"]$ties, sim.ypl=sim.ypl[plmat1$cov==zecat[ic],])

        }}

    }# end loop icov

  } # end test covsplit

  else if (covsplit==FALSE) {


    if (dist.type=="hist")  {

      aux.plot.hist(plmat, namcat, plot.opt,distrib=distrib, nclass=plot.opt$bin.number,sim.ypl=sim.ypl) }

    else{

      aux.plot.dist(plmat,namcat, plot.opt, dist.type=dist.type,distrib=distrib,nrep=npdeObject["sim.data"]["nrep"], ties=npdeObject["options"]$ties, sim.ypl=sim.ypl)

    }
  }
} # end functions

#--------------------------------------------------------------------------------------------------
# Prediction intervals, vpc
#--------------------------------------------------------------------------------------------------

compute.vpc.pi<-function(ysim,xgrp,idrep,nbin,alpha) { # ysim, xgrp => obsmat qui inclut xgrp

  nsim<-length(unique(idrep))

  sim.pi.low<-sim.pi.med<-sim.pi.up<-matrix(0,nrow=nbin,ncol=nsim)

  i0<-1
  for(irep in unique(idrep)) {

    ysim1<-ysim[idrep==irep]

    l1<-unlist(tapply(ysim1,xgrp,function(vec) quantile(vec,c(alpha,0.5,1-alpha),na.rm=TRUE)))

    l1<-matrix(l1,ncol=3,byrow=TRUE)

    sim.pi.low[,i0]<-l1[,1]
    sim.pi.med[,i0]<-l1[,2]
    sim.pi.up[,i0]<-l1[,3]

    i0<-i0+1
  }

  return(list(sim.pi.low=sim.pi.low,sim.pi.med=sim.pi.med,sim.pi.up=sim.pi.up))
}

###############################	   VPC	 ########################################

#' Visual Predictive Check (VPC)
#'
#' Produces a VPC plot for the data using the simulated data provided. Note that non-stratified VPC are not suited to unbalanced designs
#' when features such as dose or covariates enter the model. We suggest using reference profiles instead to retain a VPC-like profile
#' while ensuring meaningful prediction intervals (Comets et al. 2013).
#'
#' @usage npde.plot.vpc(npdeObject, npc=FALSE, ...)
#'
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param npc a boolean, indicating whether to compute Numerical Predictive Checks (not yet implemented)
## #' @param covsplit boolean. If TRUE and covariates are present in the dataset, the plots will be stratified for each covariate
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#'
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.  Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},  23:2036--49, 2006.
#' @references E. Comets, THT Nguyen, F. Mentré. Additional features and graphs in the new npde library for R.
#' \emph{22nd PAGE meeting, Glasgow, UK}, 2013.
#' @keywords plot
#' @export


npde.plot.vpc<-function(npdeObject,npc=FALSE,...) {

  has.cens<-FALSE

  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
    is.cens<-(npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]==1)
  } else is.cens<-rep(FALSE,dim(npdeObject["data"]["data"])[1])

  plot.opt<-npdeObject["prefs"]

  if(npdeObject["options"]["cens.method"]=="omit") plot.opt$impute.loq<-FALSE
  plot.opt$main<-"Visual Predictive Check"
  plot.opt<-replace.plotoptions(plot.opt,...)

  #if(plot.opt$new) {
  #  mfrow<-plot.opt$mfrow
  #  if(length(mfrow)==0) mfrow<-c(1,1)
  #  par(mfrow=mfrow,ask=plot.opt$ask)
  #}

  #logtyp<-""
  #if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
  #if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")

  # Binning

  not.miss<-npdeObject["data"]["not.miss"]

  if(!plot.opt$impute.loq & has.cens) not.miss[npdeObject["data"]["data"][, npdeObject["data"]["name.cens"]]==1]<-FALSE

  xvec<-npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]]

  if(has.cens & npdeObject["options"]["cens.method"]!="omit" & plot.opt$impute.loq) ydat<-npdeObject["results"]["res"]$ycomp else ydat<-npdeObject["data"]["data"][, npdeObject["data"]["name.response"]]

  xvec<-xvec[not.miss]
  ydat<-ydat[not.miss]
  is.cens<-is.cens[not.miss]

  ysim<-npdeObject["sim.data"]["datsim"]$ysim[not.miss]

  if(!plot.opt$impute.loq) {
    ysim[ysim<npdeObject["data"]["loq"]]<-NA
  }

  alpha<-(1-plot.opt$vpc.interval)/2

  xbin<-npde.binning(xvec,plot.opt,verbose=plot.opt$interactive)
  xgrp<-xbin$xgrp
  xpl<-xbin$xat
  nbin<-length(unique(xgrp))

  # ECO TODO: implement the optimal binning algorithm of Marc (see library mclust)
  # Observed data

  ypl<-tapply(ydat,xgrp,mean)

  obs.bnd<-cbind(tapply(ydat,xgrp,quantile,alpha),tapply(ydat,xgrp,mean), tapply(ydat,xgrp,quantile,1-alpha))



  if(plot.opt$bands) {

    idsim<-npdeObject["sim.data"]["datsim"]$idsim[not.miss]
    idrep<-npdeObject["sim.data"]["datsim"]$irsim[not.miss]

    #nsim<-npdeObject["prefs"]$bands.nrep
    nsim = 200

    if(nsim>npdeObject["sim.data"]["nrep"]) nsim<-npdeObject["sim.data"]["nrep"]

    isamp<-sample(1:npdeObject["sim.data"]["nrep"],nsim)
    idx<-match(idrep,isamp,nomatch=0)>0

    sbnd<-compute.vpc.pi(ysim[idx],xgrp,idrep[idx],nbin,0.95)

    pi.low<-apply(sbnd$sim.pi.low,1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
    pi.med<-apply(sbnd$sim.pi.med,1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
    pi.up<-apply(sbnd$sim.pi.up,1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)

    vec1<-c(pi.low,obs.bnd[,1])
    vec2<-c(obs.bnd[,3],pi.up)

    #if(length(grep("y",logtyp))>0) {
    #  vec1<-vec1[vec1>0]
    #  vec2<-vec2[vec2>0]
    #}

    limy<-c(min(vec1),max(vec2))
    xvec1<-xvec

    #if(length(grep("x",logtyp))>0) xvec1<-xvec1[xvec1>0]

    limx<-c(min(xvec1),max(xvec1))
    plot(xpl,ypl,type="n")#,xlim=limx,ylim=limy,xlab=plot.opt$xlab, ylab=plot.opt$ylab, main=plot.opt$main,sub=plot.opt$sub,cex.lab=plot.opt$cex.lab,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
    idx<-1:length(xpl)

    #if(length(grep("y",logtyp))>0) idx<-which(pi.low[1,]>0 & pi.low[3,]>0)

    polygon(c(xpl[idx],rev(xpl[idx])),c(pi.low[1,idx],rev(pi.low[3,idx])), col=plot.opt$col.fillpi,lty=plot.opt$lty.lpi, lwd=plot.opt$lwd.lpi, border=plot.opt$col.lpi)

    #if(length(grep("y",logtyp))>0) idx<-which(pi.up[1,]>0 & pi.up[3,]>0)

    polygon(c(xpl[idx],rev(xpl[idx])),c(pi.up[1,idx],rev(pi.up[3,idx])), col=plot.opt$col.fillpi,lty=plot.opt$lty.lpi, lwd=plot.opt$lwd.lpi, border=plot.opt$col.lpi)

    #if(length(grep("y",logtyp))>0) idx<-which(pi.med[1,]>0 & pi.med[3,]>0)

    polygon(c(xpl[idx],rev(xpl[idx])),c(pi.med[1,idx],rev(pi.med[3,idx])), col=plot.opt$col.fillmed,lty=plot.opt$lty.lmed, lwd=plot.opt$lwd.lmed, border=plot.opt$col.lmed)
    lines(xpl,pi.low[2,],lty=plot.opt$lty.lpi, col=plot.opt$col.lpi,lwd=plot.opt$lwd.lpi)
    lines(xpl,pi.med[2,],lty=plot.opt$lty.lmed, col=plot.opt$col.lmed,lwd=plot.opt$lwd.lmed)
    lines(xpl,pi.up[2,],lty=plot.opt$lty.lpi, col=plot.opt$col.lpi,lwd=plot.opt$lwd.lpi)
    lines(xpl,obs.bnd[,2],lty=plot.opt$lty.lobs, col=plot.opt$col.lobs,lwd=plot.opt$lwd.lobs)

    for (icol in c(1,3)) lines(xpl,obs.bnd[,icol],lty=plot.opt$lty.lobs, col=plot.opt$col.lobs,lwd=plot.opt$lwd.lobs)
    if(plot.opt$plot.obs) {
      if(has.cens) {
        points(xvec[!is.cens],ydat[!is.cens],pch=plot.opt$pch.pobs,col=plot.opt$col.pobs, cex=plot.opt$cex)
        if(plot.opt$plot.loq) points(xvec[is.cens],ydat[is.cens], col=plot.opt$col.pcens, pch=plot.opt$pch.pcens, cex=plot.opt$cex)
        if(plot.opt$line.loq & length(npdeObject["data"]["loq"])>0) abline(h=npdeObject["data"]["loq"], lty=plot.opt$lty.abline, col=plot.opt$col.abline,lwd=plot.opt$lwd.abline)
      } else points(xvec,ydat,pch=plot.opt$pch.pobs,col=plot.opt$col.pobs, cex=plot.opt$cex)
    }

  } else {


    # Simulated data
    nsim<-npdeObject["sim.data"]["nrep"]
    id.grp<-rep(xgrp,nsim)
    sim.bnd<-cbind(tapply(ysim,id.grp,quantile,alpha),tapply(ysim,id.grp,mean), tapply(ysim,id.grp,quantile,1-alpha))


    vec1<-c(obs.bnd[,1],sim.bnd[,1])
    vec2<-c(obs.bnd[,3],sim.bnd[,3])

    #if(length(grep("y",logtyp))>0) {
    #  vec1<-vec1[vec1>0]
    #  vec2<-vec2[vec2>0]
    #}

    limy<-c(min(vec1),max(vec2))
    xvec1<-xvec

    #if(length(grep("x",logtyp))>0) xvec1<-xvec1[xvec1>0]

    limx<-c(min(xvec1),max(xvec1))
    plot(xpl,ypl,type="n",xlim=limx,ylim=limy,xlab=plot.opt$xlab, ylab=plot.opt$ylab, cex.lab=plot.opt$cex.lab,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,main=plot.opt$main, sub=plot.opt$sub,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)

    idx<-1:length(xpl)

    #if(length(grep("y",logtyp))>0) idx<-which(sim.bnd[idx,3]>0 & sim.bnd[idx,1]>0)


    polygon(c(xpl[idx],rev(xpl[idx])),c(sim.bnd[idx,3],rev(sim.bnd[idx,1])), col=plot.opt$col.fillpi,lty=plot.opt$lty.lpi, lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi)
    lines(xpl,sim.bnd[,2],lty=plot.opt$lty.lmed, col=plot.opt$col.lmed,lwd=plot.opt$lwd.lmed)
    lines(xpl,obs.bnd[,2],lty=plot.opt$lty.lobs, col=plot.opt$col.lobs,lwd=plot.opt$lwd.lobs)
    for (icol in c(1,3)) lines(xpl,obs.bnd[,icol],lty=plot.opt$lty.lobs,col=plot.opt$lcol.lobs,lwd=plot.opt$lwd.lobs)
    if(plot.opt$plot.obs) {
      if(has.cens) {
        points(xvec[!is.cens],ydat[!is.cens],pch=plot.opt$pch.pobs,col=plot.opt$col.pobs, cex=plot.opt$cex)
        if(plot.opt$plot.loq) points(xvec[is.cens], ydat[is.cens],col=plot.opt$col.pcens, pch=plot.opt$pch.pcens, cex=plot.opt$cex)
        if(plot.opt$line.loq & length(npdeObject["data"]["loq"])>0) abline(h=npdeObject["data"]["loq"],lty=plot.opt$lty.abline, col=plot.opt$col.abline,lwd=plot.opt$lwd.abline)
      } else points(xvec,ydat,pch=plot.opt$pch.pobs,col=plot.opt$col.pobs, cex=plot.opt$cex)
    }
  }


  npc.stat<-c()
  if(npc==TRUE) {
    # ECO TODO: compute NPC - interpolation ?
  }
  invisible(list(npc=npc.stat))
}











###############################	   P(Y<LOQ)	 ########################################

#' Plot of the probability that the observations are below the LOQ
#'
#' Plots the probability that the observations are below the LOQ along with the model predicted interval
#'
#' @usage npde.plot.loq(npdeObject,xaxis="x",nsim=200,...)
#'
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param xaxis a string character, one of "x" (to plot P(Y<LOQ) versus the value of the independent predictor) or "ypred" (versus the value of the population predictions). Defaults to "x"
#' @param nsim number of simulations to be used for the computation of the prediction interval
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#'
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords plot
#' @export


npde.plot.loq<-function(npdeObject,xaxis="x",nsim=200,...) {
  # Plot of the probability of an observation being LOQ versus X or predictions, overlaying
  ### the predicted probability (according to the model)
  ### the observed probability
  ### a prediction band obtained using the simulated data

  args1<-match.call(expand.dots=TRUE)
  i1<-match("loq",names(args1))
  if(!is.na(i1)) {
    loq<-as.numeric(args1[[i1]])
  } else {
    if(length(npdeObject["data"]["loq"])==0) {
      ploq<-c()
      yobs<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
      if(length(npdeObject["data"]["loq"])>0) {
        loq<-npdeObject["data"]["loq"]
        if(npdeObject@options$verbose) cat("Computing p(y<LOQ) using LOQ=",loq,"\n")
      } else {
        yloq<-yobs[npdeObject["data"]["icens"]]
        if(length(unique(yloq))==1) {
          if(npdeObject@options$verbose) cat("Same LOQ for all missing data, loq=",loq,"\n")
          loq<-unique(yloq)
        } else {
          loq<-min(unique(yloq))
          if(npdeObject@options$verbose) cat("Computing p(y<LOQ) for the lowest LOQ, loq=",loq,"\n")
        }
        npdeObject["data"]["loq"]<-loq
      }
      if(is.infinite(npdeObject["data"]["loq"])) {
        if(npdeObject@options$verbose) cat("No loq defined in the data, and no censored data to define it, please call npde.plot.loq with the option loq=XXX where XXX is the value of the LOQ.\n")
        return()
      }
    } else loq<-npdeObject["data"]["loq"]
  }

  has.cens<-FALSE

  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
  }

  plot.opt<-npdeObject["prefs"]
  # plot.opt$main<-"Probability of being under the LOQ"
  #  plot.opt$ylab<-"Pr(Y<LOQ)"
  #  plot.opt<-replace.plotoptions(plot.opt,...)
  #  if(plot.opt$new) {
  #    mfrow<-plot.opt$mfrow
  #    if(length(mfrow)==0) mfrow<-c(1,1)
  #    par(mfrow=mfrow,ask=plot.opt$ask)
  #  }

  # logtyp<-""
  #  if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
  #  if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")
  nsim<-min(nsim,npdeObject["sim.data"]["nrep"])

  # Binning
  xvec<-switch(xaxis, x=npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]], pred=npdeObject["results"]["res"]$ypred, cov="Not implemented yet")


  if(!is.numeric(xvec)) {
    if(npdeObject@options$verbose) cat(xvec,"\n")
    return()
  }

  if(has.cens) {

    ydat<-npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]

  }else {

    ydat<-rep(0,length(xvec))

  }


  xbin<-npde.binning(xvec,plot.opt,verbose=plot.opt$interactive)

  xgrp<-xbin$xgrp
  xpl<-xbin$xat
  nbin<-length(unique(xgrp))
  isamp<-sample(1:npdeObject["sim.data"]["nrep"],nsim)
  ysim<-npdeObject["sim.data"]["datsim"]$ysim
  xtab<-matrix(nrow=nsim,ncol=nbin)

  for(i in 1:nsim)
    xtab[i,]<-tapply(ysim[npdeObject["sim.data"]["datsim"]$irsim==isamp[i]] < loq,xgrp,mean)

  alpha<-(1-plot.opt$vpc.interval)/2
  quant<-c(alpha,0.5,1-alpha)

  ypl<-apply(xtab,2,quantile,quant)

  xobs<-tapply(ydat,xgrp,mean)

  if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(0,max(c(xtab,xobs),na.rm=T))

  ypl=t(ypl)

  plotdata = data.frame(xpl,ypl,xobs)

  # --------------------------------------------------------------------------
  # plot : loq
  # --------------------------------------------------------------------------
  # plot options
  dots.plot.opt = list(...)
  plot.opt<-set.plotoptions.default(npdeObject)
  plot.opt <- modifyList(plot.opt, dots.plot.opt[intersect(names(dots.plot.opt), names(plot.opt))])


  if (length(intersect(dots.plot.opt,"col.lobs"))==0){
    plot.opt$col.lobs = plot.opt$col
  }

  if (length(intersect(dots.plot.opt,"lwd.lobs"))==0){
    plot.opt$lwd.lobs = plot.opt$lwd
  }

  if (length(intersect(dots.plot.opt,"lty.lobs"))==0){
    plot.opt$lty.lobs = plot.opt$lty
  }

  if (length(intersect(dots.plot.opt,"pch.pobs"))==0){
    plot.opt$pch.pobs <- plot.opt$pch
  }
  if (length(intersect(dots.plot.opt,"pch.pcens"))==0){
    plot.opt$pch.pcens <- plot.opt$pch
  }
  if (length(intersect(dots.plot.opt,"size.pobs"))==0){
    plot.opt$size.pobs <- plot.opt$size
  }
  if (length(intersect(dots.plot.opt,"size.pcens"))==0){
    plot.opt$size.pcens = plot.opt$size
  }

  # -----------------------------------------------------------------------------------------------------------------

  #  xlim and ylim
  if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
    x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
  } else {
    x.limits = 1.01*c(0,max(plotdata$xpl,na.rm = TRUE))}

  y.limits = c(0,1)

  plot.opt$xlab= npdeObject@data@name.predictor
  plot.opt$ylab= "Pr[Y<LOQ]"

  p <- ggplot(plotdata,aes(x = xpl)) +

    # title and axis labs
    theme(plot.title = element_text(hjust = 0.5,size = plot.opt$size.main),
          plot.subtitle = element_text(hjust = 0.5,size = plot.opt$size.sub),

          axis.title.x = element_text(size = plot.opt$size.xlab),
          axis.title.y = element_text(size = plot.opt$size.ylab),

          axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

          panel.background=element_rect("white"),
          panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")),
          panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")),
          panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")),
          panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")))  +

    labs(title = plot.opt$main, subtitle = plot.opt$sub) +

    coord_cartesian(xlim=x.limits, ylim=y.limits) +

    geom_ribbon(aes(ymin = X2.5., ymax =  X97.5.) ,
                fill=plot.opt$fill.bands,
                alpha = plot.opt$alpha.bands,
                linetype=2) +

    geom_line(aes(y = X2.5.),
              color = plot.opt$col.bands,
              alpha = plot.opt$alpha.bands,
              size = plot.opt$lwd.bands,
              linetype = plot.opt$lty.bands) +

    geom_line(aes(y = X97.5.),
              color = plot.opt$col.bands,
              alpha = plot.opt$alpha.bands,
              size = plot.opt$lwd.bands,
              linetype = plot.opt$lty.bands) +

    geom_line(aes(y = X50.),
              color = plot.opt$col.bands,
              alpha = plot.opt$alpha.bands,
              size = plot.opt$lwd.bands,
              linetype = plot.opt$lty.bands) +

    geom_line(aes(y = xobs),
              color = plot.opt$col,
              alpha = plot.opt$alpha,
              size = plot.opt$lwd,
              linetype = plot.opt$lty) +

    scale_x_continuous(plot.opt$xlab, scales::pretty_breaks(n = plot.opt$breaks.x))+
    scale_y_continuous(plot.opt$ylab, scales::pretty_breaks(n = plot.opt$breaks.y))

  print(p)

}





