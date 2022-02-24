#' @title QC-PM: Quantile Composite-based Path Modeling
#'
#' @description
#' \code{qcpm}  estimates path model paramters by quantile composite-based  
#' path modelling approach. 
#'
#' @details
#' 
#' Users can choose to estimate the model parameters for a specific quantile (tau) or to 
#' use the default quantile values: tau = (0.25, 0,50, 0.75). It is also possible to fix 
#' the quantile to 0.5 in the outer step of the iterative algorithm by using the parameter 
#' \code{fix.quantile = TRUE} for handling the measurement invariance issue (Dolce et al. 2021; 
#' Henseler et al. 2016). 
#' 
#'
#' @param model A description of the user-specified model. The model is described using 
#' the the lavaan sintax or cSEM sintax. Structural and measurement model are defined 
#' enclosed between double quotes. The causal link between constructs is defined by 
#' using the tilde ("~") operator. On the left-hand side of the operator there is the 
#' dependent constructand on the right-hand sideconstruct there are the explanatory 
#' constructs, separated by the ("+") operator. Regarding the measurement model, 
#' each construct must be defined, listing their indicators and indicating the way 
#' outer weights are computed in the iterative algorithm: mode Amode B. Mode A is 
#' defined by using the special operator ("=~"); Mode B is defined by using the special 
#' operator ("<~"). On the left-hand side of the operator, we have the cosntruct and on 
#' the right-hand side we have the MVs, separated by the ("+") operator. Variables labels 
#' cannot contain (".").
#' 
#' 
#' @param data  is a data frame or a data matrix (statistical units x manifest variables).
#' @param scheme is a string indicating the type of inner weighting scheme. It is equal to 
#' \code{factorial} by default. Possible values are \code{centroid} or \code{factorial}.
#' @param tau indicates the specific quantile that must be considered for the estimation. 
#' It is equal to \code{NULL} by default. When defined, this value is used instead of the default 
#' values (0.25, 0.5, 0.75).
#' @param fix.quantile is a boolean. When equal to \code{TRUE}, the quantile used in the outer step of the 
#' iterative algorithm is fixed to 0.5. It is used when measurement invariance is an issue. 
#' It is equal to \code{FALSE} by default.
#' @param qcorr is a boolean. If it si equal to \code{TRUE}, loadings are estimated by using quantile 
#' correlations. By default it is equal to \code{FALSE}.
#' @param tol is a decimal value indicating the tolerance criterion for the iterations (tol=0.00001
#' by default). 
#' @param maxiter integer indicating the maximum number of iterations (maxiter=100 by default). 
#' 
#' @return An object of class \code{qcpm}. 
#' @return \item{outer.weights}{the outer weights estimated for each considered quantile.}
#' @return \item{outer.loadings}{the outer loadings estimated for each considered quantile.}
#' @return \item{path.coefficients}{the path coefficients estimated for each considered quantile.}
#' @return \item{latent.scores}{list of the component scores for each considered quantile.}
#' @return \item{data}{original dataset used for the analysis.}
#' @return \item{model}{internal parameters related to the model estimation.}
#' 
#' @author Cristina Davino, Pasquale Dolce, Giuseppe Lamberti, Domenico Vistocco
#'
#' 
#' 
#' @references Davino, C., Dolce, P., Taralli, S., Vistocco, D. (2020)  Composite-Based Path 
#' Modeling for Conditional Quantiles Prediction. An Application to Assess 
#' Health Differences at Local Level in a Well-Being Perspective. 
#' \emph{Social Indicator Research}, pp. 1-30, doi:10.1007/s11205-020-02425-5.
#' 
#' @references Davino, C., Vinzi, V.E. (2016) Quantile composite-based path
#' modeling. \emph{Advansed Data Analysis and Classification}, \bold{10}, pp. 
#' 491-520, doi:10.1007/s11634-015-0231-9.
#' 
#' @references Dolce, P., Davino, C., Vistocco, D. (2021) Quantile composite-based path modeling: 
#' algorithms, properties and applications.\emph{Advansed Data Analysis and Classification},
#' doi:10.1007/s11634-021-00469-0.
#'
#' @references Henseler J., Ringle C.M., Sarstedt M. (2016) Testingmeasurement invariance of 
#' composites using partial least squares. \emph{Internationa Marketing Review}, \bold{33(3)}, pp. 
#' 405-431, doi:10.1108/IMR-09-2014-0304
#' 
#' @references Li G, Li Y, Tsai C (2014) Quantile correlations and quantile autoregressive modeling. 
#' \emph{Journal of the American Statistical Association}, \bold{110(509)} pp.233-245, 
#' doi: 10.1080/01621459.2014.892007
#' 
#' @seealso \code{\link{summary}}, \code{\link{assessment}}, \code{\link{boot}}, and 
#' \code{\link{reliability}}
#' 
#' @import quantreg
#' @importFrom cSEM parseModel 
#' @importFrom broom tidy
#' 
#' @export
#' @examples
#' 
#' # Example of QC-PM in Well-Being analysis
#' # model with three LVs and reflective indicators
#' 
#' # load library and dataset province
#' library(qcpm)
#' data(province)
#' 
#' # Define the model using laavan sintax. Use a set of regression formulas defining 
#' # firstly the structural model and then the measurement model
#' model <- "
#' # Structural model
#' EcoW ~ Edu
#' Health ~ Edu + EcoW
#'
#' # Reflective measurement model
#' Edu =~ O22 + O23 + O24 + O25aa + O26 + O_27_28 + O_27_28_AA
#' EcoW =~ O41 + O44aa + O45 + O46aa + O42 + O43
#' Health =~  O11F + O11M + O12MEAN_aa
#' "
#' 
#' # Apply qcpm
#' well.qcpm = qcpm(model,province)
#' well.qcpm
#' 
qcpm = function(model, data, scheme="factorial", tau = NULL,fix.quantile=FALSE,qcorr=FALSE,
                tol=0.00001, maxiter=100){
  
  if (length(grep("\\.", colnames(data))) > 0) {
    stop("At least one variable name in your data set contain a `.` (dot).", 
         " Dots are a reserved special character in qcpm Please rename these variables in your data and the model description.")
  }
  mod=cSEM::parseModel(model)
  
  inner = as.matrix(mod$structural)
  measurement = mod$measurement
  
  # Apply trasformation from cSEM to classic PLSPM modes
  # From common factor to A and from composite to B
  
  construct_type2 = NULL
  for (i in 1: length(mod$construct_type)){
    
    if(mod$construct_type[i]=="Common factor") 
    {construct_type2[i] = "A"} else {construct_type2[i] = "B"}
  }
  
  modes = construct_type2
  
  outer = list()
  
  for (i in 1: dim(inner)[1]){
    
    outer[[length(outer)+1]] = 
      match(names(which(mod$measurement[i,]==1)),colnames(data))    
  }
  
  checks = get_checks(data,inner,outer,scheme,tau)
  
  scheme = checks$scheme
  data = checks$data
  tau = checks$tau
  outer = checks$outer
  
  mvs.mean=colMeans(data)
  mvs.sd = apply(data,2,stats::sd)
  
  data.mod =  data[,mod$indicators]
  
  if(is.null(tau) == TRUE ) {tau_Alg = c(0.25,0.50,0.75)}
  else {tau_Alg = c(tau,0.5)}
  get_info(data, inner, outer, modes,scheme, tau,tau_Alg,fix.quantile)
  IDM = inner
  sets = outer
  
  n = nrow(data.mod)
  Nom.MVs = colnames(data.mod)
  
  lvs = ncol(IDM)
  lvs.names = colnames(IDM)
  mvs = length(unlist(sets))
  
  blocks = unlist(lapply(sets, length))
  
  ODM = matrix(0, mvs, lvs)
  colnames(ODM) = lvs.names
  
  ITER = matrix(NA, length(tau_Alg), 1)
  WEIGHTS = matrix(NA,mvs,length(tau_Alg))
  LOADINGS = matrix(NA,mvs,length(tau_Alg))
  PATHS  = matrix(NA,sum(IDM),length(tau_Alg))
  SCORES = list()
  Tau=NULL
  
  for (h in 1:length(tau_Alg)){ 
    
    get_internal = get_internal_parameter_estimation(data.mod, 
                                                     modes=modes, 
                                                     tau_Alg[h], 
                                                     lvs, 
                                                     lvs.names, 
                                                     IDM, 
                                                     sets,
                                                     scheme,
                                                     method="br",
                                                     fix.quantile,
                                                     tol,
                                                     maxiter) 
    
    
    if (any(is.na(c(get_internal$W.DIF)))==TRUE) next
    
    if (utils::tail(get_internal$W.DIF,1)>0.001) {
      warning(paste(" when tau =",tau_Alg[h])," the algorithm reached the maximum number of iterations" )}
    
    
    LVs = data.mod%*%get_internal$Wbeta1
    LVs = sweep(LVs, 2, get_internal$Intercept, "+")
    LVs = scale(LVs)*sqrt((n-1)/n)
    colnames(LVs) = lvs.names
    
    load = get_loadings(data.mod, sets, mvs, lvs, IDM, LV=LVs, tau=tau_Alg[h],qcorr)
    
    LOADINGS[,h] = round(as.matrix(rowSums(load)),4)
    ITER[h,] = rev(get_internal$ITER)[1]
    
    PATHS[,h] =  round(as.matrix(get_paths(IDM,Y_lvs=LVs,tau = tau_Alg[h])),4)
    
    WEIGHTS[,h] = round(as.matrix(get_internal$outer.weights),4)
    
    Tau=c(Tau,tau_Alg[h])
    
    LVs = sweep(LVs, 2, get_internal$Intercept, "+")
    
    SCORES[[length(SCORES)+1]] = as.data.frame(LVs)
    
  }
  
  if(is.null(Tau)==TRUE) {
    cat("\n")
    print("no admisible solution found")
    cat("\n")
  }
  
  rownames(ITER)    = tau_Alg
  colnames(ITER)    = "iterations"
  rownames(WEIGHTS) = rownames(LOADINGS) = Nom.MVs
  colnames(WEIGHTS) = colnames(LOADINGS) =colnames(PATHS)= tau_Alg
  rownames(PATHS)   = get_element(IDM)
  names(SCORES)     = Tau
  
  model = list(qcpm.iteration = ITER,mvs.mean=mvs.mean,mvs.sd = mvs.sd,
               qcpm.outer.matrix = measurement, qcpm.outer.list=sets, tau=tau, 
               tau_Alg=tau_Alg, qcpm.modes=modes, 
               qcpm.inner = inner,fix.quantile=fix.quantile,
               outer.weights2 = as.matrix(WEIGHTS))
  
  
  if(fix.quantile == TRUE && is.null(tau)==TRUE){
    
    outer.weights = as.matrix(WEIGHTS[,2])
    outer.loadings = as.matrix(LOADINGS[,2])
    path.coefficients =PATHS
    
    colnames(outer.weights)=colnames(outer.loadings)=0.5
  }
  
  else if(fix.quantile == TRUE && is.null(tau)==FALSE){
    
    outer.weights = as.matrix(WEIGHTS[,2])
    outer.loadings = as.matrix(LOADINGS[,2])
    path.coefficients = as.matrix(PATHS[,1])
    
    colnames(outer.weights)=colnames(outer.loadings)=0.5
    colnames(path.coefficients)= tau
    
  }
  else if(fix.quantile == FALSE && is.null(tau)==FALSE){
    
    outer.weights = as.matrix(WEIGHTS[,1])
    outer.loadings = as.matrix(LOADINGS[,1])
    path.coefficients = as.matrix(PATHS[,1])
    
    colnames(outer.weights)=colnames(outer.loadings)=
      colnames(path.coefficients)= tau
    
  }
  
  else if(fix.quantile == FALSE && is.null(tau)==TRUE){
    
    outer.weights = WEIGHTS
    outer.loadings = LOADINGS
    path.coefficients = PATHS
    latent.scores = SCORES
  }
  

  res = list( outer.weights = outer.weights, 
              outer.loadings = outer.loadings, 
              path.coefficients = path.coefficients, 
              latent.scores = SCORES, 
              data=data.mod,
              model = model)
  
  class(res) = "qcpm"
  
  return(res)
  
}