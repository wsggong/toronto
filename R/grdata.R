################################################################################
#' Growth rate data (1960-1985)
#'
#' A dataset containing the Annualized GDP growth rate in the period 1960-1985 of different countries
#' and initial log GDP in 1960 with other attributes related to education, market efficiency, political stability, market openness and demographic characteristics.
#' In addition cross product term between log GDP in 1960 and education variables.
#' Because of missing obeservation, we have 70 observation of \code{lr} data.
#'
#' @format A data frame with 80 obs. of 49 variables
#' \describe{
#'  \item{shcode}{country code}
#'  \item{gr}{Annualized GDP growth rate in the period 1960-1985}
#'  \item{gdp60}{Real GDP per capita in 1960(1985 price)}
#'  \item{lr}{Adult literacy rate in 1960}
#'  \item{lgdp60}{log-GDP per capita in 1960 (1985 price)}
#'  \item{ls_k}{log(investment/output) annualized over 1960-1985; a proxy for log(physical savings rate)}
#'  \item{lgr_pop}{log(population growth rate) annualized over 1960-1985}
#'  \item{pyrm60}{log(average years of primary schooling) in the male population in 1960}
#'  \item{pyrf60}{log(average years of primary schooling) in the female population in 1960}
#'  \item{syrm60}{log(average years of secondary schooling) in the male population in 1960}
#'  \item{syrf60}{log(average years of secondary schooling) in the female population in 1960}
#'  \item{hyrm60}{log(average years of higher schooling) in the male population in 1960}
#'  \item{hyrf60}{log(average years of higher schooling) in the female population in 1960}
#'  \item{nom60}{percentage of no schooling in the male population in 1960}
#'  \item{nof60}{percentage of no schooling in the female population in 1960}
#'  \item{prim60}{percentage of primary schooling attained in the male population in 1960}
#'  \item{prif60}{percentage of primary schooling attained in the female population in 1960}
#'  \item{pricm60}{percentage of primary schooling complete in the male population in 1960}
#'  \item{pricf60}{percentage of primary schooling complete in the female population in 1960}
#'  \item{secm60}{percentage of secondary schooling attained in the male population in 1960}
#'  \item{secf60}{percentage of secondary schooling attained in the female population in 1960}
#'  \item{seccm60}{percentage of secondary schooling complete in the male population in 1960}
#'  \item{seccf60}{percentage of secondary schooling complete in the female population in 1960}
#'  \item{llife}{log(life expectancy at age 0) averaged over 1960-1985}
#'  \item{lfert}{log(fertility rate) averaged over 1960-1985}
#'  \item{edu_gdp}{Government expenditure on eduction per GDP averaged over 1960-1985}
#'  \item{gcon_gdp}{Government consumption expenditure net of defence and education per GDP averaged over 1960-1985}
#'  \item{revol}{Number of revolutions per year over 1960-1984}
#'  \item{revcoup}{Number of revolutions and coups per year over 1960-1984}
#'  \item{wardum}{Dummy for countries that participated in at least one external war over 1960-1984}
#'  \item{wartime}{Fraction of time over 1960-1985 involved in external war}
#'  \item{lbmp}{log(1 + black market premium averaged over 1960–1985)}
#'  \item{tot}{Term-of-trade shock}
#'  \item{pyrm60.1}{cross-product term between lgdp60 and pyrm60}
#'  \item{pyrf60.1}{cross-product term between lgdp60 and pyrf60}
#'  \item{syrm60.1}{cross-product term between lgdp60 and syrm60}
#'  \item{syrf60.1}{cross-product term between lgdp60 and syrf60}
#'  \item{hyrm60.1}{cross-product term between lgdp60 and hyrm60}
#'  \item{hyrf60.1}{cross-product term between lgdp60 and hyrf60}
#'  \item{nom60.1}{cross-product term between lgdp60 and nom60}
#'  \item{nof60.1}{cross-product term between lgdp60 and nof60}
#'  \item{prim60.1}{cross-product term between lgdp60 and prim60}
#'  \item{prif60.1}{cross-product term between lgdp60 and prif60}
#'  \item{pricm60.1}{cross-product term between lgdp60 and pricm60}
#'  \item{pricf60.1}{cross-product term between lgdp60 and pricf60}
#'  \item{secm60.1}{cross-product term between lgdp60 and secm60}
#'  \item{secf60.1}{cross-product term between lgdp60 and secf60}
#'  \item{seccm60.1}{cross-product term between lgdp60 and seccm60}
#'  \item{seccf60.1}{cross-product term between lgdp60 and seccf60}
#' }
#'
#' @docType data
#' @name grdata
#' @seealso \code{plot.l4acp} and \code{l4acp} to see the example of application of this data.
#' @references Sokbae Lee, Myung Hwan Seo, and Youngki Shin, (2016) \emph{The Lasso for High-Dimensional Regression with a Possible Change Point}, Journal of the Royal Statistical Society Series B, Vol 78(1), 193-210
"grdata"

