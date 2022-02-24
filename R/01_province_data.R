#' Province dataset example
#' 
#' @docType data
#' @name province
#' @usage province
#' @format This data set allows to estimate  the relationships among Health  (\code{Health}), 
#' Education and training (\code{Edu}) and Economic well-being (\code{EcoW}) 
#' in the Italian provinces using a subset of the indicators collected by the Italian Statistical 
#' Institute (ISTAT) to measure equitable and sustainable well-being (BES, from the Italian Benessere 
#' Equo e Sostenibile) in territories. Data refers to the 2019 edition of the BES report (ISTAT, 2018, 
#' 2019a, 2019b). A subset of 16 indicators (manifest variables) are observed on the 110 Italian provinces 
#' and metropolitan cities (i.e. at NUTS3 level) to measure the latent variables \code{Health}, \code{Edu} 
#' and \code{EcoW}. The interest in such an application concerns both advances in knowledge 
#' about the dynamics producing the well-being outcomes at local level (multiplier effects or trade-offs) 
#' and a more complete evaluation of regional inequalities of well-being.
#' 
#' Data Strucuture
#' 
#' A data frame with 110 Italian provinces and metropolitan cities and 16 variables (i.e., indicators) related to 
#' three latent variables: Health (3 indicators), Education and training (7 indicators), and Economic well-being 
#' (6 indicators).
#' 
#'
#' Manifest variables description for each latent variable:
#'
#'\itemize{
#'\item{LV1} {Health} (\code{Health})
#'\itemize{
#'\item{MV1 \code{O11M}}: {life expectancy at birth of males}
#'\item{MV2 \code{O11F}}: {life expectancy at birth of female}
#'\item{MV3 \code{O12MEAN_aa}}: {infant mortality rate}
#'}
#'\item{LV2} {Education and training} (\code{Edu})
#'\itemize{
#'\item{MV4 \code{O22}}: {people with at least upper secondary education level (25-64 years old)}
#'\item{MV5 \code{O23}}: {people having completed tertiary education (30-34 years old)}
#'\item{MV6 \code{O24}}: {first-time entry rate to university by cohort of upper secondary graduates}
#'\item{MV7 \code{O25aa}}: {people not in education, employment or training (Neet)}
#'\item{MV8 \code{O26}}: {ratio of people aged 25-64 years partici- pating in formal 
#'or non-formal education to the total people aged 25-64 years}
#'\item{MV9 \code{O_27_28}}: {cores obtained in the tests of functional skills of the 
#'students in the II classes of upper secondary education}
#'\item{MV10 \code{O_27_28_A}}: {Differences between males and females students in the level of 
#'numeracy and literacy}
#'}
#'\item{LV3} {Economic wellbeing} (\code{EcoW})
#'\itemize{
#'\item{MV11 \code{O41}}: {per capita disposable income}
#'\item{MV12 \code{O44aa}}: {pensioners with low pension amount}
#'\item{MV13 \code{O45}}: {per capita net wealth}
#'\item{MV14 \code{O46aa}}: {rate of bad debts of the bank loans to families}
#'\item{MV15 \code{O42}}: {average annual salary of employees}
#'\item{MV16 \code{O43}}: {average annual amount of pension income per capita}
#'}
#'}
#'
#' For a full description of the variables, see table 3 of Davino et al (2020).
#'
#'
#'@references Davino, C., Dolce, P., Taralli, S., Vistocco, D. (2020).  Composite-Based Path 
#' Modeling for Conditional Quantiles Prediction. An Application to Assess 
#' Health Differences at Local Level in a Well-Being Perspective. 
#' \emph{Social Indicator Research} doi:10.1007/s11205-020-02425-5.
#'
#'@references Davino, C., Dolce, P., Taralli, S., Esposito Vinzi, V. (2018). A quantile 
#'composite-indicator approach for the measurement of equitable and sustainable well-being: 
#'A case study of the italian provinces. \emph{Social Indicators Research}, \bold{136}, pp. 999–1029, 
#'Dordrecht, Kluwer Academic Publishers.
#'
#'@references Davino, C., Dolce, P., Taralli, S. (2017). Quantile composite-based model: 
#'A recent advance in pls-pm. a preliminary approach to handle heterogeneity in the measurement 
#'of equitable and sustainable well-being. In H. Latan & R. Noonan (eds), \emph{Partial least 
#'squares path modeling: basic concepts, methodological issues and applications} (pp. 81–108). 
#'Cham: Springer.  
#'
#'@references ISTAT. (2019a). Misure del Benessere dei territori. Tavole di dati. Rome, 
#'Istat. \url{https://www.istat.it/it/archivio/230627}.
#'
#'@references ISTAT. (2019b). Le differenze territoriali di benessere - Una lettura a livello 
#'provinciale. Rome, Istat.\url{https://www.istat.it/it/archivio/233243}.
#'
#'@references ISTAT. (2018). Bes report 2018: Equitable and sustainable well-being in Italy. 
#'Rome, Istat. \url{https://www.istat.it/en/archivio/225140}.
#'
#' @keywords datasets
NULL
