
############################################################################################################################

# Histogram

############################################################################################################################

aux.plot.hist<-function(plmat2,namcat,plot.opt,distrib="norm", nclass=10,sim.ypl=NULL) {

  list_plot = list()

  if (plot.opt$which.cov !="all"  ){

   title_graph = namcat #c("<Q1","Q1-Q3",">Q3")

    for ( iter_plot in 1:length(namcat)){

      plmat <- plmat2[[iter_plot]]

      ndat<-dim(plmat)[1]

      xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
      sim.ypl<-matrix(xsamp,nrow=ndat)

      x<-hist(plmat$xpd,breaks=nclass,plot=FALSE)

      alpha <- plot.opt$pi.size

      if(alpha>0.5) alpha<-1-alpha

      tmat<-matrix(nrow=length(x$breaks)-1,ncol=dim(sim.ypl)[2])

      nB<-length(x$breaks)

      for(j in 1:dim(sim.ypl)[2]) {

        xvec<-cut(sim.ypl[,j],breaks=x$breaks,include.lowest=TRUE, ordered_result=TRUE)

        tmat[,j]<-table(xvec)
      }

      row.names(tmat)<-names(table(xvec))

      bnds<-apply(tmat,1,quantile,c(alpha/2,0.5,1-alpha/2))

      ## data

        data <- data.frame(name = x$mids,
                           value = x$counts,
                           bnds_min = bnds[1,],
                           bnds_mean = bnds[2,],
                           bnds_max = bnds[3,])

        # xlim and ylim
        if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
          x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
        } else {
          if (plot.opt$xlog == TRUE){
            x.limits = c(0.1,max(data$name,na.rm = TRUE))
          }else {
            x.limits = 1.01*c(min(data$name,na.rm = TRUE),max(data$name,na.rm = TRUE))
          }}

        if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
          y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
        } else {

          if (plot.opt$ylog == TRUE){
            y.limits = c(0.1,max(data$value,na.rm = TRUE))
          } else {
            y.limits = c(0,1.01*max(data$value,bnds[3,],na.rm = TRUE))
          }}

        p <- ggplot(data) + #theme_bw() +

        { if (iter_plot == 1)

          theme(plot.title = element_text(hjust = 0.5,size = plot.opt$size.main),
                panel.background=element_rect("white"),
                panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),

                axis.title.x = element_text(size = plot.opt$size.xlab),
                axis.title.y = element_text(size = plot.opt$size.ylab),

                axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")))

        } +

                { if (iter_plot > 1)

                  theme(plot.title = element_text(hjust = 0.5,size = plot.opt$size.main),

                        panel.background=element_rect("white"),
                        panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                        panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),

                        axis.title.x = element_text(size = plot.opt$size.xlab),
                        axis.title.y = element_blank(),
                        axis.text.x  = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                        axis.text.y  =  element_blank())
                } +

          # main title
          ggtitle(plot.opt$main) +

          # coordinates x-y
          coord_cartesian(xlim=x.limits, ylim=y.limits) +

                     {  if(plot.opt$bands==TRUE)
                       geom_crossbar( aes(x=name, y=bnds_mean, ymin=bnds_min, ymax=bnds_max),
                                      width=diff(x$mids)[1],
                                      colour = plot.opt$col.bands,
                                      fill = plot.opt$fill.bands,
                                      alpha = plot.opt$alpha.bands,
                                      linetype =  plot.opt$lty.bands,
                                      size = plot.opt$lwd.bands )} +
          geom_bar( aes(x=name, y=value),
                    stat="identity",
                    width = diff(x$mids)[1],
                    colour = plot.opt$col,
                    fill = plot.opt$fill,
                    alpha = plot.opt$alpha,
                    linetype =  plot.opt$lty,
                    size = plot.opt$lwd ) +

## remove log scales pour les histogrammes
           # x-y logscales
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
                                       breaks = scales::trans_breaks("log10", function(x) 10 ^ x),labels = scales::trans_format("log10", scales::math_format(10 ^ .x)))
                     } +

                     {
                       if (plot.opt$ylog == TRUE)
                         scale_y_log10(plot.opt$ylab,
                                       breaks = scales::trans_breaks("log10", function(x) 10 ^ x),labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
                         )
                     } +
            #if log scales plot logticks
                     { if (plot.opt$xlog == TRUE) annotation_logticks(sides = "b")} +
                     { if (plot.opt$ylog == TRUE) annotation_logticks(sides = "l")} +
        # graph title
        ggtitle(title_graph[iter_plot])
        list_plot[[iter_plot]] <- p
     }

  if (plot.opt$plot.default==TRUE){


   return(list_plot)

  }else{

      grid.arrange(grobs = list_plot, nrow=1,
                top = textGrob(paste0(plot.opt$main,'\n'),
               vjust = 1,
              gp = gpar(fontsize=plot.opt$size.main)))
  }

}

else if (plot.opt$which.cov =="all"){


    plmat <- plmat2
    ndat<-dim(plmat)[1]
    #ndat = length(plmat)

    xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
    sim.ypl<-matrix(xsamp,nrow=ndat)

    x<-hist(plmat$xpd,breaks=nclass,plot=FALSE)

    alpha <- plot.opt$pi.size

    if(alpha>0.5) alpha<-1-alpha

    tmat<-matrix(nrow=length(x$breaks)-1,ncol=dim(sim.ypl)[2])

    nB<-length(x$breaks)

    for(j in 1:dim(sim.ypl)[2]) {

      xvec<-cut(sim.ypl[,j],breaks=x$breaks,include.lowest=TRUE, ordered_result=TRUE)

      tmat[,j]<-table(xvec)
    }

    row.names(tmat)<-names(table(xvec))

    bnds<-apply(tmat,1,quantile,c(alpha/2,0.5,1-alpha/2))

    data <- data.frame(name = x$mids, value = x$counts)


    # xlim and ylim
    if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
      x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
    } else {
      if (plot.opt$xlog == TRUE){
        x.limits = c(0.1,max(data$name,na.rm = TRUE))
      }else {
        x.limits = 1.01*c(min(data$name,na.rm = TRUE),max(data$name,na.rm = TRUE))
      }}

    if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
      y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
    } else {

      if (plot.opt$ylog == TRUE){
        y.limits = c(0.1,max(data$value,na.rm = TRUE))
      } else {
        y.limits = c(0,1.01*max(data$value,bnds[3,],na.rm = TRUE))
      }}

    if(plot.opt$xlab=="") plot.opt$xlab<-"Sample quantiles (npde)" # ROMAIN TODO: ici il faudrait tester si on est en pd, npde ou npd et ajuster le titre en conséquence avec un paste("Sample quantiles (",XXX,")",sep="))
    if(plot.opt$ylab=="") plot.opt$ylab<-"Counts"





    # ggplot template

    p <- ggplot(data,aes(x=name, y=value)) +

      # title and axis labs
      theme(plot.title = element_text(hjust = 0.5,size = plot.opt$size.main),
            plot.subtitle = element_text(hjust = 0.5,size = plot.opt$size.sub),

            panel.background=element_rect("white"),
            panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
            panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),

            axis.title.x = element_text(size = plot.opt$size.xlab),
            axis.title.y = element_text(size = plot.opt$size.ylab),

            axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
            axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")))+

      # main title
      ggtitle(plot.opt$main) +

      labs(title = plot.opt$main, subtitle = plot.opt$sub) +

      # coordinates x-y
      coord_cartesian(xlim=x.limits, ylim=y.limits) +

      geom_bar( aes(x=name, y=value),
                colour = plot.opt$col,
                stat="identity",
                width = diff(x$mids)[1],
                fill = plot.opt$fill,
                alpha = plot.opt$alpha,
                linetype =  plot.opt$lty,
                size = plot.opt$lwd ) +

      {  if(plot.opt$bands==TRUE)

           geom_crossbar( aes(x=name, y=bnds[2,], ymin=bnds[1,], ymax=bnds[3,]),
                          colour = plot.opt$col.bands,
                          width=diff(x$mids)[1],

                          fill = plot.opt$fill.bands,
                          alpha = plot.opt$alpha.bands,
                          linetype =  plot.opt$lty.bands,
                          size = plot.opt$lwd.bands )
       }+

       # remove all legend
       theme(legend.position="none")+

         # x-y logscales
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


    # for plot by default
    if (plot.opt$plot.default==TRUE){

    return(p)}

    else{
      print(p)
    }
    }

} # end function

############################################################################################################################

# Empirical cdf and  qqplot

############################################################################################################################

#' @importFrom stats ecdf

aux.plot.dist <- function(plmat2,  namcat, plot.opt,dist.type,distrib,nrep=0, nclass=0, covsplit = FALSE, ties=TRUE,sim.ypl=NULL,verbose=FALSE) {

  if (plot.opt$which.cov =="all"){

    plmat <- plmat2


    if(nclass==0){
      binning<-FALSE
    }else {
      binning<-TRUE
      nbin<-plot.opt$bin.number}

    # -------------------------------------------------------------------------------------------------
    # band or not
    #  approx.pi		: Whether approximate prediction bands should be obtained for the distribution plots
    # for qq plot : distrib = "norm"
    # -------------------------------------------------------------------------------------------------

    ndat<-dim(plmat)[1]

    approx.pi=TRUE

    xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
    sim.ypl<-matrix(xsamp,nrow=ndat)

    # -------------------------------------------------------------------------------------------------
    #	prediction discrepancy pd : handle extreme values of the observations
    # -------------------------------------------------------------------------------------------------

    if(ties & nrep>0){
      yq<-seq(1/(2*nrep),1-1/(2*nrep),length.out=ndat)}
    else{
      yq<-seq(0,1,length.out=ndat)
    }

    # -------------------------------------------------------------------------------------------------
    # Quantile pour loi Gaussienne npde ~ N(0,1)
    # -------------------------------------------------------------------------------------------------

    if(distrib=="norm"){
      yq<-qnorm(yq)
    }

    # -------------------------------------------------------------------------------------------------
    # calcul de la ecdf
    # -------------------------------------------------------------------------------------------------

    if(dist.type=="ecdf") {
      yq<-seq(1/ndat,1,length.out=ndat)
    }

    ymat<-plmat[order(plmat$xpd),]
    ymat<-cbind(ymat,ecdf=yq)


    if(dist.type=="ecdf") {

      ysh<-data.frame(x=plmat$xpd,y=yq)
      yobs<-data.frame(x=ymat$xpd,y=ymat$ecdf,cens=ymat$cens)

    } else{

      ysh<-data.frame(x=yq,y=plmat$xpd)
      yobs<-data.frame(x=ymat$ecdf,y=ymat$xpd,cens=ymat$cens)
    }

    if(!binning) {

      # si ecdf : xvec = npde et yvec = Uniform[0,1]
      # sinon : xvec = N[0,1] et yvec = npde

      xvec<-ysh$x
      yvec<-ysh$y

      alpha<-plot.opt$pi.size

      if(alpha>0.5) alpha<-1-alpha
      sim.sort<-colsort(sim.ypl)

      # quantiles min,max,median
      bnds<-apply(sim.sort,1,quantile,c(alpha/2,0.5,1-alpha/2))

      # --------------------------------------------------------------------------------------------------------------
      # plot data : no censored + covsplit
      # --------------------------------------------------------------------------------------------------------------

      # -----------------------------------------------
      # plot ecdf
      # -----------------------------------------------

      if(dist.type=="ecdf") {

        plotdata = as.data.frame(cbind((yobs$x[yobs$cens==0]),yobs$y[yobs$cens==0],bnds[1,yobs$cens==0],bnds[2,yobs$cens==0],bnds[3,yobs$cens==0]))
        plotdata_censored = as.data.frame(cbind((yobs$x[yobs$cens==1]),yobs$y[yobs$cens==1]))
        plotdata_obs = as.data.frame(cbind((yobs$x[yobs$cens==0]),yobs$y[yobs$cens==0]))

        plot(plotdata[,1],plotdata[,2])
        lines(ecdf(plotdata[,3]),col="red")
        lines(ecdf(plotdata[,4]),col="red")
        lines(ecdf(plotdata[,5]),col="red")


        colnames(plotdata)=c("x","y","bnd_min","bnd","bnd_max")
        colnames(plotdata_censored)=c("x_censored","y_censored")
        colnames(plotdata_obs)=c("x_obs","y_obs")

        # xlim and ylim
        if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
          x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
        } else {
          x.limits = c(min(plotdata$x,na.rm = TRUE),max(plotdata$x,na.rm = TRUE))
          }

        if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
          y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
        } else {

          if (plot.opt$ylog == TRUE){
            y.limits = c(0.1,0)
          } else {
            y.limits = c(0,1)
          }}

        ## ggplot template
        p <- ggplot(plotdata) +

          theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
                axis.title.x = element_text(size = plot.opt$size.xlab),
                axis.title.y = element_text(size = plot.opt$size.ylab),

                #axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                #axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

                axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
                axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

                panel.background=element_rect("white"),
                panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))+

          # main title
          ggtitle(plot.opt$main) +

          # coordinates x-y
          coord_cartesian(xlim=x.limits, ylim=y.limits) +

          {if(plot.opt$bands==TRUE)
            geom_ribbon(aes(y = y,  xmin = bnd_min,xmax = bnd_max),
                        fill=plot.opt$fill.bands,
                        alpha = plot.opt$alpha.bands)} +

          {if (plot.opt$type=="l" || plot.opt$type=="b")

            geom_line(aes(y=y, x=x),
                      colour = plot.opt$col,
                      linetype = plot.opt$lty,
                      size = plot.opt$lwd)} +

          {if (plot.opt$type=="p" || plot.opt$type=="b")
            geom_point(plotdata_obs,mapping = aes(y=y_obs, x=x_obs), colour = plot.opt$col.pobs,
                       size = plot.opt$size.pobs)} +

          {if (plot.opt$type=="p" || plot.opt$type=="b")
            geom_point(plotdata_censored, mapping = aes(y=y_censored, x=x_censored),
                       colour = plot.opt$col.pcens,
                       size = plot.opt$size.pcens)} +

          geom_line(aes(x=bnd_min, y=y),
                    linetype = plot.opt$lty.bands,
                    colour = plot.opt$col.bands,
                    size = plot.opt$lwd.bands)+

          geom_line(aes(x=bnd, y=y),
                    linetype = plot.opt$lty.bands,
                    colour = plot.opt$col.bands,
                    size = plot.opt$lwd.bands)+

          geom_line(aes(x=bnd_max, y=y),
                    linetype = plot.opt$lty.bands,
                    colour = plot.opt$col.bands,
                    size = plot.opt$lwd.bands)+

          scale_x_continuous(plot.opt$xlab,
                             scales::pretty_breaks(n = plot.opt$breaks.x)) +

          scale_y_continuous(plot.opt$ylab,
                             scales::pretty_breaks(n = plot.opt$breaks.y))


        print(p)

      } # end ecdf

      # -----------------------------------------------
      # plot qqplot
      # -----------------------------------------------

      if(dist.type=="qqplot") {


        plotdata = as.data.frame(cbind(yobs$x,yobs$y,bnds[1,],bnds[2,],bnds[3,]))

        plotdata_censored = as.data.frame(cbind((yobs$x[yobs$cens==1]),yobs$y[yobs$cens==1]))
        plotdata_obs = as.data.frame(cbind((yobs$x[yobs$cens==0]),yobs$y[yobs$cens==0]))

        colnames(plotdata)=c("x","y","bnd_min","bnd","bnd_max")
        colnames(plotdata_censored)=c("x_censored","y_censored")
        colnames(plotdata_obs)=c("x_obs","y_obs")

        # xlim and ylim

        if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
          x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
        } else {
          if (plot.opt$xlog == TRUE){
            x.limits = c(0.1,max(plotdata$x,na.rm = TRUE))
          }else {
            x.limits = c(min(plotdata$x,na.rm = TRUE),max(plotdata$x,na.rm = TRUE))
          }}

        if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
          y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
        } else {

          if (plot.opt$ylog == TRUE){
            y.limits = c(0.1,max(plotdata$bnd_max,na.rm = TRUE))
          } else {
            y.limits = c(min(plotdata$bnd_min,plotdata$y, na.rm = TRUE),max(plotdata$y,plotdata$bnd_max,na.rm = TRUE))
          }}

        ## ggplot template

        if(plot.opt$xlab=="") plot.opt$xlab<-"Sample quantiles (npde)"
        if(plot.opt$ylab=="") plot.opt$ylab<-"Theorical quantiles"

        p <- ggplot(plotdata, aes(x = xcent)) +

          # title and axis labs

          theme(plot.title = element_text(hjust = 0.5,size = plot.opt$size.main),
                plot.subtitle = element_text(hjust = 0.5,size = plot.opt$size.sub),

                axis.title.x = element_text(size = plot.opt$size.xlab),
                axis.title.y = element_text(size = plot.opt$size.ylab),

                #axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                #axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

                axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
                axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

                panel.background=element_rect("white"),
                panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))  +

          # main title
          ggtitle(plot.opt$main) +

          labs(title = plot.opt$main, subtitle = plot.opt$sub) +

          # coordinates x-y

          coord_cartesian(xlim=x.limits, ylim=y.limits) +

          {if(plot.opt$bands==TRUE)
         geom_ribbon(aes(ymin=bnd_min, ymax=bnd_max, x=x),
                        fill=plot.opt$fill.bands,
                      alpha = plot.opt$alpha.bands)

          } +

          geom_line(aes(y=bnd_min, x=x),
                    linetype = plot.opt$lty.bands,
                    colour = plot.opt$col.bands,
                    size = plot.opt$lwd.bands)+

          geom_line(aes(y=bnd, x=x),
                    linetype = plot.opt$lty.bands,
                    colour = plot.opt$col.bands,
                    size = plot.opt$lwd.bands)+

          geom_line(aes(y=bnd_max, x=x),
                    linetype = plot.opt$lty.bands,
                    colour = plot.opt$col.bands,
                    size = plot.opt$lwd.bands)+

          {if (plot.opt$type=="l" || plot.opt$type=="b")

          geom_line(aes(y=y, x=x),
                    colour = plot.opt$col,
                    linetype = plot.opt$lty,
                    size = plot.opt$lwd)} +

          {if (plot.opt$type=="p" || plot.opt$type=="b")
          geom_point(plotdata_obs,mapping = aes(y=y_obs, x=x_obs), colour = plot.opt$col.pobs,
                     size = plot.opt$size.pobs)} +

          {if (plot.opt$type=="p" || plot.opt$type=="b")
          geom_point(plotdata_censored, mapping = aes(y=y_censored, x=x_censored),
                     colour = plot.opt$col.pcens,
                     size = plot.opt$size.pcens,
                     shape = plot.opt$pch.pcens)} +


          # x-y logscales
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

        # boolean for plot by default

        if (plot.opt$plot.default==TRUE){

          return(p)}

        else{
          print(p)
        }

      } # end qqplot

    } # end binning

  } # end if not cov

  # -------------------------------------------------------------------------------------------------
  #  Plot for covariates
  # -------------------------------------------------------------------------------------------------

  if (plot.opt$which.cov !="all"){

    list_plot = list()
    plot_ylim=list()

    for ( iter_plot in 1:length(namcat)){

      plmat <- plmat2[[iter_plot]]

      ind_censored = which(plmat$cens ==1)

      plmat = plmat[plmat$cens==0,]

      # -------------------------------------------------------------------------------------------------
      # plmat = [npde,boolean donnée censuré 1/0]
      # pd, pde ~ U[0,1]
      # npde ~ N[0,1]
      # version 3.0 : pas de binning, ie !binning
      # binning or not
      # 4 options
      #(1) equal bin sizes on the X-axis;
      #(2) equal bin widths on the X-axis;
      #(3) optimal binning using a clustering algorithm to select the optimal breaks;
      #(4) user-selected breaks
      # -------------------------------------------------------------------------------------------------

      if(nclass==0){
        binning<-FALSE
      }else {
        binning<-TRUE
        nbin<-plot.opt$bin.number}

      # -------------------------------------------------------------------------------------------------
      # band or not
      # approx.pi		: Whether approximate prediction bands should be obtained for the distribution plots
      # for qq plot : distrib = "norm"
      # -------------------------------------------------------------------------------------------------

      ndat<-dim(plmat)[1]

      # sim.ypl = N(0,1) ou U[0,1]
      # sim.ypl = matrix nda*ndat

      #approx.pi=TRUE
      xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
      sim.ypl<-matrix(xsamp,nrow=ndat)
      #plot.bands<-TRUE

      # -------------------------------------------------------------------------------------------------
      #	prediction discrepancy pd : handle extreme values of the observations
      # -------------------------------------------------------------------------------------------------

      if(ties & nrep>0){
        yq<-seq(1/(2*nrep),1-1/(2*nrep),length.out=ndat)}
      else{
        yq<-seq(0,1,length.out=ndat)
      }

      # -------------------------------------------------------------------------------------------------
      # Quantile pour loi Gaussienne npde ~ N(0,1)
      # -------------------------------------------------------------------------------------------------
      # yq ~ U[0,1]
      if(distrib=="norm"){
        yq<-qnorm(yq)
      }

      # -------------------------------------------------------------------------------------------------
      # calcul de la ecdf
      # -------------------------------------------------------------------------------------------------

      if(dist.type=="ecdf") {

        yq<-seq(1/ndat,1,length.out=ndat)
      }

      ymat<-plmat[order(plmat$xpd),]
      ymat<-cbind(ymat,ecdf=yq)

      if(dist.type=="ecdf") {
        ysh<-data.frame(x=plmat$xpd,y=yq)
        yobs<-data.frame(x=ymat$xpd,y=ymat$ecdf,cens=ymat$cens)
      } else {

        ysh<-data.frame(x=yq,y=plmat$xpd)
        yobs<-data.frame(x=ymat$ecdf,y=ymat$xpd,cens=ymat$cens)

      }

      # -------------------------------------------------------------------------------------------------
      # plot with no binning
      # -------------------------------------------------------------------------------------------------

      if(!binning) {

        # si ecdf : xvec = npde et yvec = Uniform[0,1]
        # sinon : xvec = N[0,1] et yvec = npde

        xvec<-ysh$x
        yvec<-ysh$y

        # pi.size = Width of the prediction interval on the quantiles
        # pi.size = 0.95
        alpha<-plot.opt$pi.size

        # alpha = 0.95 ---> quantile = c(0.475, 0.500, 0.525)
        if(alpha>0.5) alpha<-1-alpha
        sim.sort<-colsort(sim.ypl)

        # quantiles min,max,median
        bnds<-apply(sim.sort,1,quantile,c(alpha/2,0.5,1-alpha/2))

        if(dist.type=="ecdf") {
          xvec<-c(xvec,c(bnds))
          yvec<-c(yvec,yq)
        }

        if(dist.type=="qqplot") {
          xvec<-c(xvec,yq)
          yvec<-c(yvec,c(bnds))

        }
      }

        if(dist.type=="qqplot") {

          plotdata = as.data.frame(cbind(yobs$x[yobs$cens==0],yobs$y[yobs$cens==0],bnds[1,],bnds[2,],bnds[3,]))
          colnames(plotdata)=c("x","y","bnd_min","bnd","bnd_max")

          plotdata_censored = as.data.frame(cbind(yobs$x[ind_censored],yobs$y[ind_censored]))
          colnames(plotdata_censored)=c("x_censored","y_censored")


          if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
            x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
          } else {
            if (plot.opt$xlog == TRUE){
              x.limits = c(0.1,max(plotdata$x,na.rm = TRUE))
            }else {
              x.limits = c(min(plotdata$x,na.rm = TRUE),max(plotdata$x,na.rm = TRUE))
            }}

          if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
            y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
          } else {

            if (plot.opt$ylog == TRUE){
              y.limits = c(0.1,max(plotdata$bnd_max,na.rm = TRUE))
            } else {
              y.limits = c(min(plotdata$bnd_min,plotdata$y, na.rm = TRUE),
                           max(plotdata$y,plotdata$bnd_max,na.rm = TRUE))
              plot_ylim[[iter_plot]] = y.limits

            }}

          plot.opt$main =  "QQ-plot versus N(0,1) for npde"
          plot.opt$sub = namcat[iter_plot]

          p <- ggplot(plotdata, aes(x = xcent)) +

            { if (iter_plot == 1)

              theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
                    axis.title.x = element_text(size = plot.opt$size.xlab),
                    axis.title.y = element_blank(), #element_text(size = plot.opt$size.ylab),

                    #axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                    #axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

                    axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
                    axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

                    panel.background=element_rect("white"),
                    panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid)) } +

            { if (iter_plot > 1)
              theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
                    axis.title.x = element_text(size = plot.opt$size.xlab),
                    axis.title.y = element_blank(),

                    #axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                    #axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

                    axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
                    axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

                    panel.background=element_rect("white"),
                    panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))} +

            # main title
            ggtitle(plot.opt$sub) +

            {if(plot.opt$bands==TRUE)
              geom_ribbon(aes(ymin=bnd_min, ymax=bnd_max, x=x),
                          fill=plot.opt$fill.bands,
                          alpha = plot.opt$alpha.bands)} +

            {if (plot.opt$type=="l" || plot.opt$type=="b")

              geom_line(aes(y=y, x=x),
                        colour = plot.opt$col,
                        linetype = plot.opt$lty,
                        size = plot.opt$lwd)} +

            {if (plot.opt$type=="p" || plot.opt$type=="b")
              geom_point(plotdata,mapping = aes(y=y, x=x),
                         colour = plot.opt$col.pobs,
                         size = plot.opt$size.pobs)} +

            {if (plot.opt$type=="p" || plot.opt$type=="b")
              geom_point(plotdata_censored, mapping = aes(y=y_censored, x=x_censored),
                         colour = plot.opt$col.pcens,
                         size = plot.opt$size.pcens)} +

            geom_line(aes(y=bnd_min, x=x),
                      linetype = plot.opt$lty.bands,
                      colour = plot.opt$col.bands,
                      size = plot.opt$lwd.bands)+

            geom_line(aes(y=bnd, x=x),
                      linetype = plot.opt$lty.bands,
                      colour = plot.opt$col.bands,
                      size = plot.opt$lwd.bands)+

            geom_line(aes(y=bnd_max, x=x),
                      linetype = plot.opt$lty.bands,
                      colour = plot.opt$col.bands,
                      size = plot.opt$lwd.bands)+

            # x-y logscales
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

        }# end test if qqplot

        # -------------------------------------------------------------------------------------------------------------------------
        # Template : ecdf, bands, covariates
        # -------------------------------------------------------------------------------------------------------------------------

        if(dist.type=="ecdf") {


          plotdata = as.data.frame(cbind((yobs$x[yobs$cens==0]),yobs$y[yobs$cens==0],bnds[1,yobs$cens==0],bnds[2,yobs$cens==0],bnds[3,yobs$cens==0]))

          plotdata_censored = as.data.frame(cbind((yobs$x[yobs$cens==1]),yobs$y[yobs$cens==1]))
          plotdata_obs = as.data.frame(cbind((yobs$x[yobs$cens==0]),yobs$y[yobs$cens==0]))

          colnames(plotdata)=c("x","y","bnd_min","bnd","bnd_max")
          colnames(plotdata_censored)=c("x_censored","y_censored")
          colnames(plotdata_obs)=c("x_obs","y_obs")

          # xlim and ylim
          if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
            x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
          } else {
            x.limits = c(min(plotdata$x,na.rm = TRUE),max(plotdata$x,na.rm = TRUE))
          }

          if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
            y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
          } else {

            if (plot.opt$ylog == TRUE){
              y.limits = c(0.1,0)
            } else {
              y.limits = c(0,1)
            }
            plot_ylim[[iter_plot]] = y.limits

            }

          if(plot.opt$xlab=="") plot.opt$xlab<-"Sample quantiles (npde)"
          if(plot.opt$ylab=="") plot.opt$ylab<-"Empirical cdf"

          p <- ggplot(plotdata) +

            { if (iter_plot == 1)

              theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
                    axis.title.x = element_text(size = plot.opt$size.xlab),
                    axis.title.y = element_blank(), #element_text(size = plot.opt$size.ylab),

                    axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                    axis.text.y =  element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),
                    panel.background=element_rect("white"),
                    panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))} +

            { if (iter_plot > 1)
              theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
                    axis.title.x = element_text(size = plot.opt$size.xlab),
                    axis.title.y = element_blank(),

                    #axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
                    #axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

                    axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
                    axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

                    panel.background=element_rect("white"),
                    panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
                    panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))} +

            # main title
            ggtitle(plot.opt$sub) +

            {if(plot.opt$bands==TRUE)
              geom_ribbon(aes(y = y,  xmin = bnd_min,xmax = bnd_max),
                          fill=plot.opt$fill.bands,
                          alpha = plot.opt$alpha.bands)} +

            {if (plot.opt$type=="l" || plot.opt$type=="b")

              geom_line(aes(y=y, x=x),
                        colour = plot.opt$col,
                        linetype = plot.opt$lty,
                        size = plot.opt$lwd)} +

            {if (plot.opt$type=="p" || plot.opt$type=="b")
              geom_point(plotdata_obs,mapping = aes(y=y_obs, x=x_obs), colour = plot.opt$col.pobs,
                         size = plot.opt$size.pobs)} +

            {if (plot.opt$type=="p" || plot.opt$type=="b")
              geom_point(plotdata_censored, mapping = aes(y=y_censored, x=x_censored),
                         colour = plot.opt$col.pcens,
                         size = plot.opt$size.pcens)} +


            geom_line(aes(x=bnd_min, y=y),
                      linetype = plot.opt$lty.bands,
                      colour = plot.opt$col.bands,
                      size = plot.opt$lwd.bands)+

            geom_line(aes(x=bnd, y=y),
                      linetype = plot.opt$lty.bands,
                      colour = plot.opt$col.bands,
                      size = plot.opt$lwd.bands)+

            geom_line(aes(x=bnd_max, y=y),
                      linetype = plot.opt$lty.bands,
                      colour = plot.opt$col.bands,
                      size = plot.opt$lwd.bands)+



            scale_x_continuous(plot.opt$xlab,
                               scales::pretty_breaks(n = plot.opt$breaks.x)) +

            scale_y_continuous(plot.opt$ylab,
                               scales::pretty_breaks(n = plot.opt$breaks.y))


        } # end test if ecdf

        list_plot[[iter_plot]] <- p

    }

    # adjust y-axis
    min_yaxis = min( unlist(plot_ylim))
    max_yaxis = max( unlist(plot_ylim))

  for (i in 1:length(list_plot)){
   list_plot[[i]] <- list_plot[[i]] + coord_cartesian(xlim=x.limits, ylim=c(min_yaxis,max_yaxis))
  }

        grid.arrange(grobs = list_plot, nrow=1,
                 left = textGrob("Theorical quantiles", rot = 90, vjust = 1),
                 top = textGrob(paste0(plot.opt$main,'\n'),vjust = 1, gp = gpar(fontsize=plot.opt$size.main)))

  }
} # end function aux.plot.dist

