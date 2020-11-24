aux.npdeplot.meanprof<-function(mbin,msim) {
  # Compute a reference profile based on simulations from the model
  # mbin : matrix with columns xat (centre of the bins) and xgrp (bin number/group)
  # msim : matrix with simulations, 2 columns used (grp=which bin, ysim=simulated value)
  ymed<-tapply(msim[,"ysim"],msim[,"grp"],mean)
  sdmed<-tapply(msim[,"ysim"],msim[,"grp"],sd)
  ymed<-ymed[match(mbin$xlab,names(ymed),nomatch=0)]
  sdmed<-sdmed[match(mbin$xlab,names(sdmed),nomatch=0)]
  #	ymed<-ymed[order(names(ymed))]
  #	sdmed<-sdmed[order(names(sdmed))]
  if(length(ymed)<dim(mbin)[1]) {
    # Linear interpolation
    #				ypmed<-approx(xcent,ymed,xout=mbin$xat,rule=2)$y
    # Spline interpolation
    cat("Not all time points/bins are represented in the subset used for the reference profile: spline interpolation will be used to predict the entire profile, but this may distort the aspect of the plot significantly; we advise using another reference profile.\n")
    xcent<-mbin$xat[mbin$xlab %in% names(ymed)]
    ypmed<-spline(xcent,ymed,xout=mbin$xat)$y
    spmed<-spline(xcent,sdmed,xout=mbin$xat)$y
    iint<-1-as.integer(mbin$xlab %in% names(ymed))
    mpref<-data.frame(xat=mbin$xat,grp=mbin$grp,mean=ypmed,sd=spmed,int=iint,xlab=mbin$xlab)
  } else mpref<-data.frame(xat=mbin$xat,grp=mbin$grp,mean=ymed,sd=sdmed,int=rep(0,length(sdmed)),xlab=mbin$xlab)
  return(mpref)
}

aux.npdeplot.computepi<-function(plmat,plot.opt,xlab,xat,mpref=NULL,dotline=NULL,sim.ypl=NULL,distrib="norm",onlog=FALSE) {


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
  xinf<-sqrt(12)
  alpha<-(1-plot.opt$vpc.interval)/2
  nseuil<-200
  alp.pi<-plot.opt$pi.size
  if(alp.pi<0.5) alp.pi<-(1-alp.pi)
  quant<-c(alpha,0.5,1-alpha)
  if(!plot.opt$approx.pi & !is.null(sim.ypl)) {
    nrep<-length(sim.ypl)/dim(plmat)[1]
    yprov<-list(grp=plmat$grp,cens=plmat$cens,ypl=matrix(sim.ypl,ncol=nrep))
    bnds<-compute.bands.true(yprov,quant,xlab=xlab,alpha=alp.pi)
    bnds$xcent<-xat
  } else {
    nobs<-tapply(plmat$grp,plmat$grp,length)
    bnds<-compute.bands(nobs,nseuil,quant,distrib,alp.pi)

    bnds$xcent<-xat[match(names(nobs),xlab)]
  }
  # Transforming the boundaries if reference profile
  if(!is.null(mpref)) {

    if(onlog) {
      if(distrib=="unif") {
        for(i in 1:3) bnds$binf[,i]<-exp((bnds$binf[,i]-0.5)*mpref$sd*xinf+mpref$mean)
        for(i in 1:3) bnds$bmed[,i]<-exp((bnds$bmed[,i]-0.5)*mpref$sd*xinf+mpref$mean)
        for(i in 1:3) bnds$bsup[,i]<-exp((bnds$bsup[,i]-0.5)*mpref$sd*xinf+mpref$mean)
      } else {
        for(i in 1:3) bnds$binf[,i]<-exp(bnds$binf[,i]*mpref$sd+mpref$mean)
        for(i in 1:3) bnds$bmed[,i]<-exp(bnds$bmed[,i]*mpref$sd+mpref$mean)
        for(i in 1:3) bnds$bsup[,i]<-exp(bnds$bsup[,i]*mpref$sd+mpref$mean)
      }
    } else {
      if(distrib=="unif") {
        for(i in 1:3) bnds$binf[,i]<-(bnds$binf[,i]-0.5)*mpref$sd*xinf+mpref$mean
        for(i in 1:3) bnds$bmed[,i]<-(bnds$bmed[,i]-0.5)*mpref$sd*xinf+mpref$mean
        for(i in 1:3) bnds$bsup[,i]<-(bnds$bsup[,i]-0.5)*mpref$sd*xinf+mpref$mean
      } else {
        for(i in 1:3) bnds$binf[,i]<-bnds$binf[,i]*mpref$sd+mpref$mean
        for(i in 1:3) bnds$bmed[,i]<-bnds$bmed[,i]*mpref$sd+mpref$mean
        for(i in 1:3) bnds$bsup[,i]<-bnds$bsup[,i]*mpref$sd+mpref$mean
      }
    }
    if(is.null(dotline)) dline<-NULL else {
      dline<-data.frame(xat=mpref$xat)
      for(i in 1:length(dotline)) {
        if(onlog) {
          if(distrib=="unif") x1<-exp((dotline[i]-0.5)*mpref$sd*xinf+mpref$mean) else x1<-exp(dotline[i]*mpref$sd+mpref$mean)
        } else {
          if(distrib=="unif") x1<-(dotline[i]-0.5)*mpref$sd*xinf+mpref$mean else x1<-dotline[i]*mpref$sd+mpref$mean
        }
        dline<-cbind(dline,x1)
      }
    }
  }	else {

    if(is.numeric(plmat$x) & !is.null(dotline)) {
      dline<-data.frame(xat=seq(min(plmat$x,na.rm=TRUE),max(plmat$x,na.rm=TRUE),length.out=100))
      for(i in 1:length(dotline))
        dline<-cbind(dline,rep(dotline[i],100))
    } else dline<-NULL
  }
  return(list(bnds=bnds,dline=dline))
}

aux.npdeplot.transform<-function(plmat,plot.opt,xat,mpref=NULL,distrib="norm",onlog=FALSE) {
  # Input
  # plmat: values to plot
  # plot.opt: preferences for plots
  # mpref: reference profile
  # distrib: reference distribution
  # onlog: whether E and SD are computed on the observed value or after log transformation
  # Output
  # plmat: updated with a ty column containing the data to plot
  # percobs: percentile of the observed distribution, used in the plots
  # dotprof: dotline profile
  xinf<-sqrt(12) # used only if distrib is "unif"
  alpha<-(1-plot.opt$vpc.interval)/2
  quant<-c(alpha,0.5,1-alpha)
  if(is.null(mpref)) {
    ty<-plmat$y
  } else {
    mpr<-mpref[mpref$int==0,] # use only points not interpolated initially, to avoid double interpolation
    yfmed<-spline(mpr$xat,mpr$mean,xout=plmat$x)$y
    sfmed<-spline(mpr$xat,mpr$sd,xout=plmat$x)$y
    if(onlog) {
      if(distrib=="unif") ty<-exp((plmat$y-0.5)*sfmed*xinf+yfmed) else ty<-exp(plmat$y*sfmed+yfmed)
    } else {
      if(distrib=="unif") ty<-(plmat$y-0.5)*sfmed*xinf+yfmed else ty<-plmat$y*sfmed+yfmed
    }
  }
  percobs<-matrix(unlist(tapply(ty,plmat$grp,quantile,quant,na.rm=T)),ncol=3,byrow=T)

  row.names(percobs)<-xat
  plmat<-cbind(plmat,ty=ty)
  return(list(plmat=plmat,percobs=percobs))
}

# ---------------------------------------------------------------------------------------

# npde.plot.meanprofile

# ---------------------------------------------------------------------------------------

npde.plot.meanprofile<-function(npdeObject,which="npde",xaxis="x",plot.opt,...){ #} xscale=FALSE, onlog=FALSE, ref.prof=NULL, ...) {

  xscale=FALSE
  onlog=FALSE
  ref.prof=NULL

  # -----------------------------------------------------------------------------------------------------------------

  # obs.mat : matrix for observations = [grp, xcent, x, cens, y, cov, namcat]

  # -----------------------------------------------------------------------------------------------------------------

  # ---------------------------------------------------------------------------------------
  # refs and plot options
  # ---------------------------------------------------------------------------------------

  #plot.opt = npdeObject["prefs"]
  userPlotOptions  = list(...)
  plot.opt <- set.plotoptions.default( npdeObject )
  plot.opt <- modifyList( plot.opt, userPlotOptions[ intersect( names( userPlotOptions ), names( plot.opt ) ) ] )

  covsplit = plot.opt$covsplit

  #xlab = paste0( npdeObject@data@name.predictor," ", "(", npdeObject@data@units$x,")" )
  #ylab = paste0( npdeObject@data@name.response," ", "(", npdeObject@data@units$y,")" )

  # ---------------------------------------------------------------------------------------
  # not.miss : not missing data in the data
  # ---------------------------------------------------------------------------------------

  # not.miss TRUE / FALSE

  not.miss = npdeObject["data"]["not.miss"]

  # ---------------------------------------------------------------------------------------
  # covariates in the npdeObject
  # ---------------------------------------------------------------------------------------

  # hasCovariates TRUE / FALSE

  if (length(npdeObject["data"]["name.covariates"])>0){
    hasCovariates = TRUE
  }else{
    hasCovariates = FALSE
  }

  # ---------------------------------------------------------------------------------------
  # test if censored data
  # ---------------------------------------------------------------------------------------

  if (length(npdeObject["data"]["icens"])>0){
    has.cens = TRUE
  }else{
    has.cens = FALSE
  }

  # ---------------------------------------------------------------------------------------
  # data X and Y
  # xpl : x in plot
  # ypl : y in plot
  # sim.ypl : simulations des observations
  # ---------------------------------------------------------------------------------------

  xpl <- npdeObject["data"]["data"][, npdeObject["data"]["name.predictor"]]

  if(xaxis=="x") xpl<-npdeObject["data"]["data"][, npdeObject["data"]["name.predictor"]]

  if(xaxis=="cov") xpl<-rep(1,length(ypl))

  if(xaxis=="pred") xpl<-npdeObject["results"]["res"]$ypred

  # ------------------------------------------------------------------------------------------
  # sim.ypl : initialisation to NULL
  # ------------------------------------------------------------------------------------------

  sim.ypl<-NULL

  #sim.ypl<-npdeObject["sim.data"]["datsim"]$ysim

  # if(!is.null(sim.ypl)) {
  #  sim.ypl<-sim.ypl[rep(npdeObject["data"]["not.miss"],npdeObject["sim.data"]["nrep"])]

  #}}

  if(plot.opt$approx.pi==FALSE) {
    if(which %in% c("pd","npd")) {
      sim.ypl<-npdeObject["results"]["pd.sim"]
    }
    if (which %in% c("npde","tnpde")){
      sim.ypl<-npdeObject["results"]["npde.sim"]
    }
  }

  # ------------------------------------------------------------------------------------------
  # ypl
  # ------------------------------------------------------------------------------------------

  if(which=="yobs" & plot.opt$bands) {
    sim.ypl<-npdeObject["sim.data"]["datsim"]$ysim
    plot.opt$approx.pi<-FALSE
  }

  if(which=="npde") {
    ypl<-npdeObject["results"]["res"]$npde
    npdeObject@prefs$plot.tnpde = FALSE
  }

  if(which=="tnpde") {
    ypl<-npdeObject["results"]["res"]$tnpde
    npdeObject@prefs$plot.tnpde = TRUE
  }


  if(which %in% c("pd","npd")){
    ypl<-npdeObject["results"]["res"]$pd
  }


  if(which=="yobs") {
    ypl<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]


  }

  if(which=="npd") {
    ypl<-qnorm(ypl)
    if(!plot.opt$approx.pi) sim.ypl<-qnorm(sim.ypl)
  }

  # if(which=="yobs") {
  #
  #   ypl<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  # }
  #
  # if(which=="npde") {
  #   ypl<-npdeObject["results"]["res"]$npde
  # }
  #
  # if(which=="tnpde") {
  #   ypl<-npdeObject["results"]["res"]$tnpde
  # }
  #
  # if(which %in% c("pd","npd")){
  #   ypl<-npdeObject["results"]["res"]$pd
  # }
  #
  #
  # if(which=="npd") {
  #   ypl<-qnorm(ypl)
  #   if(plot.opt$approx.pi==FALSE) {
  #     sim.ypl<-qnorm(sim.ypl)}
  # }
  #
  #

  # ------------------------------------------------------------------------------------------

  # ---------------------------------------------------------------------------------------
  # plmat : matrix with quantiles for plot
  # plmat = [x, cens, y, cov]
  # cov = interval with quantile
  # ---------------------------------------------------------------------------------------

  # déclaration de la matrice plmat : data cendored or not

  if(has.cens) {

    plmat<-data.frame(x=xpl,cens=npdeObject["data"]["data"][, npdeObject["data"]["name.cens"]],y=ypl)

  }else{

    plmat<-data.frame(x=xpl,cens=rep(0,length(xpl)),y=ypl)

  }

  plmat <- plmat[npdeObject["data"]["not.miss"],]

  # ---------------------------------------------------------------------------------------
  # idobs = ID : group of the patient in the observed data (i.e. index du group du patient)
  # ---------------------------------------------------------------------------------------

  idobs <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]

  #-------------------------------------------------------------------------------------
  # check data : faire une function à part pour checker tout est ok
  #-------------------------------------------------------------------------------------

  if (covsplit == TRUE & length(npdeObject@data@name.covariates)==0){
    cat("Error : covsplit == TRUE & length(npdeObject@data@name.covariates)==0")
    return(0)}

  # check the switch
  if(match(which,c("npde","tnpde","pd","npd","yobs"),nomatch=0)==0) {
    cat("Option which=",which,"not recognised\n")
    return()
  }
  if(match(xaxis,c("x","pred","cov"),nomatch=0)==0) {
    cat("Option xaxis=",xaxis,"not recognised\n")
    return()
  }
  if(xaxis=="cov" & length(npdeObject["data"]["name.covariates"])==0) {
    cat("Option xaxis set to 'cov' but no covariate in dataset\n")
    return()
  }
  # checks on presence of the value to plot (no need to check when which is yobs, the data is always there)
  if(which %in% c("npde","tnpde") & (length(npdeObject["results"]["res"]$npde)==0 || length(npdeObject["results"]["res"]$tnpde)==0)) {
    cat("    Missing npde object to plot.\n")
    return()
  }
  if(which %in% c("pd","npd") & length(npdeObject["results"]["res"]$pd)==0)  {
    cat("    Missing pd object to plot.\n")
    return()
  }

  # ---------------------------------------------------------------------------------------
  # xscale and mpref
  # ---------------------------------------------------------------------------------------

  if(xscale & !is.null(ref.prof)) {
    if(!is.list(ref.prof)) {
      cat("The reference profile must be entered as a named list, eg list(ID=c(1,5)) to select subjects with ID=1 and 5 as reference; names should refer to columns in the data file.\n")
      #ref.prof<-NULL
    }
  }

  if(xaxis=="cov") {
    if(covsplit & npdeObject@options$verbose) cat("    graph versus covariates requested, setting covsplit to FALSE\n")
  }

  if(which=="pd") distrib<-"unif" else distrib<-"norm"

  # variabilité pour le profil de référence
  # modifie taille de prédiction en fonction de la variabilité inter individuelle

  if(xscale) msim<-npdeObject["sim.data"]["datsim"]$ysim else msim<-NULL

  if(xscale) msim<-msim[rep(npdeObject["data"]["not.miss"],npdeObject["sim.data"]["nrep"])]

  # Reference profile: extracting matrix msim corresponding to the reference profile
  if(!is.null(msim)) {
    if(!is.null(ref.prof) & xscale) {
      iuse<-rep(0,dim(plmat)[1])
      dat1<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],]
      for(iref in 1:length(ref.prof)) {
        i<-names(ref.prof)[iref]
        i1<-which(dat1[,i] %in% ref.prof[[iref]])
        if(iref==1) idx1<-i1 else idx1<-intersect(idx1,i1)
      }
      iuse[idx1]<-1
      iuse<-rep(iuse,npdeObject["sim.data"]["nrep"])
      #			}
    } else iuse<-rep(1,length(msim))
    msim<-data.frame(ysim=msim,use=iuse)
  }

  #-------------------------------------------------------------------------------------
  # lines for quantile
  #-------------------------------------------------------------------------------------

  if(plot.opt$which %in% c("npd","npde","tnpde")) {
    x1<-abs(qnorm((1-plot.opt$pi.size)/2))
    dotline<-c(-x1,0,x1)
  }

  if(plot.opt$which=="pd") {
    dotline<-c((1-plot.opt$pi.size)/2,0.5,1-(1-plot.opt$pi.size)/2)
  }

  # ---------------------------------------------------------------------------------------
  # covsplit and covariates
  # ---------------------------------------------------------------------------------------

  # covsplit TRUE / FALSE
  # lcov = name.covariates
  # lunit = units of the covariates
  # icov =  index of the choosen covariates in the list of all the covariates of the data
  # lunit = units of the covariates
  # ncov : nombre de covariates
  # zecov : data des covariates lcov
  # ucov : categories des données des covariates
  # zecat : quantiles "<Q1","Q1-Q3",">Q3" for the covariables

  # postit
  # ! pas de covariable dans le temps
  # ! ie traitement par periode
  # ! effet periode pour la covariable ie periode observation
  # ! parametres pk peuvent changer
  # ! variabilité inter occasion + évaluation de cette variabilité
  # ! correlation + fmethode machine learning prediction FDR

    # loop over covariates
  icov = match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
  ncat = max(icov)

  for (iter_covariate in 1:length(icov)){

    # test for cvosplit
    if (covsplit == TRUE){

      # icov = index of the covariate names
      # index, unit, not.missing for the covariate
      lcov =  npdeObject["data"]["name.covariates"][icov[iter_covariate]]

      lunit = npdeObject["data"]["units"]$covariates[icov[iter_covariate]]
      zecov = npdeObject["data"]["data"][npdeObject["data"]["not.miss"],lcov]
      ucov = zecov[match(unique(idobs),idobs)]

      # -------------------------------------------------------------------
      # calcul des quantiles pour catégories numériques
      # -------------------------------------------------------------------

      if(is.numeric(ucov)){

        # case for number of split !=3
        if(plot.opt$ncat!=3) {
          ncat<-plot.opt$ncat
          seqcat<-seq(0,1,length.out=(ncat+1))
          zecov<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)
          nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
          namcat<-paste(namcov,nam1,sep=": ")

        } else {

          # case for number of split = 3
          zecov<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)

          namcat<-paste(lcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")

        }}

      # -------------------------------------------------------------------
      # catégories non numériques
      # -------------------------------------------------------------------

      if(!is.numeric(ucov)){
        namcat<-paste(lcov,unique(ucov), sep=": ")

      }

      plmat$cov <- zecov

    } # end  covsplit

    else {

      plmat<-cbind(plmat,cov=rep(1,dim(plmat)[1]))
      namcat = plot.opt$which.cov

    }

    # remove rows with x,y NAN values
    plmat = plmat[complete.cases(plmat), ]

    # -----------------------------------------------------------------------------------------------------------------
    # obs.mat and pi.mat for vpc and scatter plot
    # -----------------------------------------------------------------------------------------------------------------

    ### obs.mat & pi.mat : vpc

    if(which == "yobs") {

      pi.mat = list()

      xvec= plmat$x
      ydat = plmat$y

      xbin<-npde.binning(xvec,plot.opt,verbose=plot.opt$interactive)

      xgrp<-xbin$xgrp
      xpl<-xbin$xat
      nbin<-length(unique(xgrp))

      ysim<-npdeObject["sim.data"]["datsim"]$ysim[not.miss]

      alpha<-(1-plot.opt$vpc.interval)/2

      # ECO TODO: implement the optimal binning algorithm of Marc (see library mclust)
      # Observed data

      obs.bnd<-cbind(tapply(ydat,xgrp,quantile,alpha),tapply(ydat,xgrp,mean), tapply(ydat,xgrp,quantile,1-alpha))

      #take the replication indices for not missing data
      idrep = npdeObject["sim.data"]["datsim"]$irsim[not.miss]

      npdeObject["prefs"]$bands.nrep = 1000 # mis ici à 1000 par défaut

      nsim = npdeObject["prefs"]$bands.nrep

      if (nsim > npdeObject["sim.data"]["nrep"]){
        nsim = npdeObject["sim.data"]["nrep"]
      }

      #nsim<-npdeObject["sim.data"]["nrep"]
      id.grp<-rep(xgrp,nsim)

      ##print(nsim)
      ##print(length(xgrp))
      ##print(length(ysim))

      sim.bnd<-cbind(tapply(ysim,id.grp,quantile,alpha),tapply(ysim,id.grp,mean), tapply(ysim,id.grp,quantile,1-alpha))

      # used to get index for covarites
      sim.cov = rep(plmat$cov,nsim)

      # name for pi.mat
      namesCov = as.factor(unique(sim.cov))

      # case for no covariate
      if (covsplit==FALSE){
        name = npdeObject@data@name.response
      }

      # names for the row of the dataframe boundsData
      namesBoundsData = apply(expand.grid(c("binf","bmed","bsup"),c("0025","05","0975")), 1, paste, collapse="")

      # list for prediction interval
      pi.mat = list()

      for (iter in 1:length(namesCov)){

        # name for the covariable
        name = namesCov[iter]
        # index for simulatedcovariates
        index.sim.cov = which(sim.cov==name)
        # index for corresponding groups
        index.grp = which(plmat$cov==name)
        xgrp.sim.cov = xgrp[index.grp]

        # compute the vpc pi : pi low, upper and median
        sbnd = compute.vpc.pi(ysim[index.sim.cov],xgrp.sim.cov,idrep[index.sim.cov],nbin,alpha)

        pi.low = apply(sbnd$sim.pi.low,1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
        pi.med = apply(sbnd$sim.pi.med,1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)
        pi.up  = apply(sbnd$sim.pi.up, 1,quantile,c(0.025,0.5,0.975),na.rm=TRUE)

        # stack in a list convert in pi.mat
        l = list(pi.low, pi.med,pi.up)

        pi.mat[[iter]] = t(as.data.frame(Reduce(rbind,l)))

        pi.mat[[iter]] = cbind.data.frame(
          1:nbin,
          namcat[iter],
          plot.opt$which.cov[[iter_covariate]] ,
          xpl,
          pi.mat[[iter]],
          obs.bnd,
          sim.bnd)

      } # end loop

    } else {

      # ----------------------------------------------------------------
      # obs.mat & pi.mat : x.scatter
      # ----------------------------------------------------------------

      # prediction interval matrix
      pi.mat = list()

      namesBoundsData = c("binf0025","binf05","binf0975",
                          "bmed0025","bmed05","bmed0975",
                          "bsup0025","bsup05","bsup0975")

      # name for pi.mat
      namesCov = as.factor(unique( plmat$cov))

      if (is.numeric(plmat$x)) {

        # ECO TODO: should binning be done on full data or after censoring ?
        xbin <- npde.binning(plmat$x, plot.opt, verbose = plot.opt$interactive)

        plmat <- data.frame(grp = xbin$xgrp, plmat)

        xcent <- xbin$xat

      } else {

        plmat <- data.frame(plmat$x, grp = plmat$x, plmat)

        xcent <- unique(plmat$x)

      }

      xlab <- as.character(1:length(xcent))

      if (!is.null(msim)) {

        nrep <- dim(msim)[1] / dim(plmat)[1]

        msim <- cbind(msim, grp = rep(xbin$xgrp, nrep))

        mbin <- data.frame(xat = xbin$xat,
                           grp = names(xbin$xat),
                           xlab = xlab)
      } else {

        if (!is.null(sim.ypl))
          nrep <- length(sim.ypl) / dim(plmat)[1]
      }

      zecat <- unique(plmat$cov)
      ncat <- length(zecat)

      if (!is.null(msim))
        mpref <- aux.npdeplot.meanprof(mbin, msim[msim$use == 1, ])
      else
        mpref <- NULL

      for (icat in 1:ncat) {

        if (covsplit & is.null(ref.prof)) {
          is.cat <- (plmat$cov == zecat[icat])
          if (!is.null(msim))
            mpref <- aux.npdeplot.meanprof(mbin, msim[rep(is.cat, nrep), ])
        } else {
          is.cat <- rep(TRUE, dim(plmat)[1])
        }

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

        if (!is.null(mpref))
          xat <-
          mpref$xat[xlab %in% plmat1$grp]
        else
          xat <- xcent[xlab %in% plmat1$grp]

        if (!is.null(sim.ypl))
          sim.ypl1 <- sim.ypl[rep(is.cat, nrep)]
        else
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
        l = list(xcal$bnds$xcent,
                 xcal$bnds$binf,
                 xcal$bnds$bmed,
                 xcal$bnds$bsup)

        pi.mat[[icat]] = as.data.frame(Reduce(cbind,l),check.names = TRUE)

        nrow.pi.mat = dim(pi.mat[[icat]])[1]

        pi.mat[[icat]] = cbind(
          1:nrow.pi.mat,
          namcat[[icat]],
          plot.opt$which.cov,
          pi.mat[[icat]],
          percobs,
          matrix(rep(dotline,each=nrow.pi.mat),nrow=nrow.pi.mat)
        )

      }	### end loop icat

    }

    # -----------------------------------------------------------------------------------------------------------------
    # obs.mat & pi.mat
    # -----------------------------------------------------------------------------------------------------------------

#    pi.mat = rbindlist(pi.mat)
    pi.mat <- do.call("rbind", pi.mat) # or integrate that during the previous loop, ie create a data frame and rbind it to one of the previous iteration

    # names for the data
    colnames(pi.mat) = c("groups",
                         "category",
                         "covariate",
                         "xcent",
                         namesBoundsData,
                         c("per.min","per.med","per.max"),
                         c("dotline.min","dotline.med","dotline.max"))
    obs.mat = plmat

    # add column loq to obs.mat
    loq = npdeObject@data@loq

    if (length(loq)==0){
      obs.mat[["loq"]] = NaN
    }else{
      obs.mat[["loq"]] = npdeObject@data@loq
    }

    # add column ipred to obs.mat for individual prediction for vpc plot
    if(which == "yobs")
    {

      if ( !identical( npdeObject@data@name.ipred, character(0) ) )
      {
        obs.mat[["ipred"]] = npdeObject@data@data$ipred
      }
    }

    # -----------------------------------------------------------------------------------------------------------------
    # plot vpc and scatter plot
    # -----------------------------------------------------------------------------------------------------------------

    plotScatterAndVPC <- aux.npdeplot.plot(obs.mat, pi.mat, plot.opt)

  } # end loop covariate

  return(plotScatterAndVPC)


  ##########################################################################################################################################################################
} # END FUNCTION
##########################################################################################################################################################################

# --------------------------------------------------------------
# les options bizarres : onlong and co (see NpdeControl)
# --------------------------------------------------------------

# which.resplot=c("res.vs.pred") : désactivée
# smooth=TRUE, # désactivée # pour  rajouter points milieu bin # pas dans scatter
# line.smooth="s", # désactivée # pour  rajouter points milieu bin # pas dans scatter
# box=TRUE, # désactivée # à remettre
# ncat=1 # fonctionne pas
# interactive=TRUE,  #  désactivée
# boxwex  #  désactivée
# varwidth  #  désactivée # taille boite à moustaches
# range=3 # désactivée, ne sers pas ailleurs que dans npdeControl # taille de l'interquartile dans boite à moustaches
# onlog=FALSE : définie par défaut FALSE dans node.mean.profile, pas redéfinie ailleurs
# log(y) for vpc + meanprofile
# log(x)


