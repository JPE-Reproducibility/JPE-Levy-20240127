# Runtime checks for baseline bootstrap inputs and complete draw collections.
bootstrap_check_fit <- function(m, context) {
  required <- c("X", "Z", "Y", "W", "X.os", "Z.os", "mod", "lmbd")
  if (!all(required %in% names(m))) stop(context, ": incomplete model object")
  stopifnot(length(m$lmbd)==1L, is.finite(m$lmbd), m$lmbd>=0,
            identical(dim(m$mod$phi), c(ncol(m$X), ncol(m$Z))),
            length(m$mod$intercept)==nrow(m$Z),
            all(is.finite(m$mod$phi)), all(is.finite(m$mod$intercept)),
            length(m$mod$obj)==1L, is.finite(m$mod$obj),
            nrow(m$Y)==nrow(m$X), ncol(m$Y)==nrow(m$Z),
            length(m$W)==nrow(m$X), all(is.finite(m$W)), all(m$W>=0),
            length(m$X.os)==nrow(m$X), length(m$Z.os)==nrow(m$Z))
  if (!is.null(m$mod$eta)) stopifnot(all(is.finite(m$mod$eta)))
  if (!(m$mod$conv %in% c(1,3))) stop(context, ": unusable stopping code ", m$mod$conv)
  m$mod$conv
}

bootstrap_check_baseline <- function(folder, prepared=NULL) {
  for (g in 4:6) {
    path <- file.path(folder, paste0("model_wave_",g,".Rds"))
    if (!file.exists(path)) stop("Missing baseline bootstrap starting model: ",path)
    m <- readRDS(path)
    bootstrap_check_fit(m,path)
    if (!is.null(prepared)) {
      ix <- prepared$X.all$BB_Wave_num==g
      iz <- prepared$Z.all$BB_Wave_num==g
      expected <- list(X=prepared$X[ix,,drop=FALSE], Z=prepared$Z[iz,,drop=FALSE],
                       W=prepared$X.all$BB_Weight_CW[ix],
                       X.os=paste0(prepared$X.all$BB_Country[ix],g),
                       Z.os=paste0(prepared$Z.all$BB_Country[iz],g))
      y <- paste0(prepared$X.all$Y_manifesto_key[ix],g)
      expected$Y <- matrix(0,nrow(expected$X),nrow(expected$Z),dimnames=list(NULL,rownames(expected$Z)))
      expected$Y[cbind(seq_along(y),match(y,colnames(expected$Y)))] <- 1
      for (n in names(expected)) {
        if (!isTRUE(all.equal(m[[n]],expected[[n]],tolerance=1e-8,scale=1)))
          stop(path,": baseline input differs from current data/cleandata: ",n)
      }
    }
  }
  invisible(TRUE)
}

bootstrap_check_collection <- function(folder, seeds, template=NULL, include_fits=FALSE) {
  dirs <- list.dirs(folder,recursive=FALSE,full.names=FALSE)
  if (!setequal(dirs,as.character(seeds))) stop("Incomplete or unexpected bootstrap seed directories: ",folder)
  statuses <- integer()
  for (seed in seeds) {
    d <- file.path(folder,as.character(seed))
    file <- file.path(d,"dec_4_6.Rds")
    if (!file.exists(file)) stop("Missing bootstrap decomposition: ",file)
    a <- as.matrix(readRDS(file))
    if (is.null(template)) template <- a
    b <- as.matrix(template)
    if (!identical(dimnames(a),dimnames(b)) || !identical(dim(a),dim(b)) || !identical(is.na(a),is.na(b)))
      stop("Unexpected bootstrap decomposition schema/missingness: ",file)
    stopifnot(is.numeric(a),!any(is.infinite(a)),all(is.finite(a[,seq(3,12,3),drop=FALSE])))
    if (include_fits) {
      required <- c(paste0("model_wave_",4:6,".Rds"),"dec_4_6.Rds")
      if (!setequal(list.files(d),required)) stop("Unexpected/incomplete bootstrap files: ",d)
      for (g in 4:6) {
        file <- file.path(d,paste0("model_wave_",g,".Rds"))
        statuses <- c(statuses,bootstrap_check_fit(readRDS(file),file))
      }
    }
  }
  if (length(statuses) && any(statuses!=1)) warning(sum(statuses!=1)," bootstrap fits stopped without further objective improvement (code 3); see diagnostics.")
  cat("Validated bootstrap collection:",folder,";",length(seeds),"decompositions;",length(statuses),"wave fits\n")
  invisible(statuses)
}
