# file to source all R-functions network analysis
if(!("ggm" %in% .packages(all.available=TRUE))) install.packages("ggm")
if(!("gtools" %in% .packages(all.available=TRUE))) install.packages("gtools")
if(!("SIN" %in% .packages(all.available=TRUE))) install.packages("SIN")
if(!("MASS" %in% .packages(all.available=TRUE))) install.packages("MASS")
if(!("corpcor" %in% .packages(all.available=TRUE))) install.packages("corpcor")
if(!("pcalg" %in% .packages(all.available=TRUE))) install.packages("pcalg")
if(!("qgraph" %in% .packages(all.available=TRUE))) install.packages("qgraph")
if(!("InvariantCausalPrediction" %in% .packages(all.available=TRUE))) install.packages("InvariantCausalPrediction")
if(!("RBGL" %in% .packages(all.available=TRUE))){ 
	source("http://bioconductor.org/biocLite.R")
	biocLite("RBGL")
}
if(!("graph" %in% .packages(all.available=TRUE))){ 
	source("http://bioconductor.org/biocLite.R")
	biocLite("graph")
}


require(ggm)
require(gtools)
require(SIN)
require(MASS)
require(corpcor)
require(pcalg)
require(qgraph)
require(RBGL)
require(InvariantCausalPrediction)

sin.ag <-
function (data, plot = TRUE, holm = TRUE, alpha = 0.1, beta = 0.5) 
{
    cov <- cov(data)
    n <- dim(data)[1]
    p <- dim(data)[2]
    sin.amat <- sinUG(cov, n, holm = holm)
    sin <- sin.amat
    sin.lt <- sin[lower.tri(sin, diag = FALSE)]
    if (plot) {
    	connect <- combinations(p, 2)
    	lc <- dim(connect)[1]
    	make.name <- function(a) paste(a[1],paste("-",a[2],sep=""),sep="")
    	leg <- apply(connect,1,make.name)
        plot(sin.lt, pch = 16, bty = "n", axes = FALSE, xlab = "edge", 
            ylab = "P-value")
        axis(1, at = 1:lc, labels = leg, las = 2)
        axis(2, at = c(0.2, 0.4, 0.6, 0.8, 1), labels = TRUE)
        lines(c(1, lc), c(alpha, alpha), col = "gray")
        lines(c(1, lc), c(beta, beta), col = "gray")
        name <- deparse(substitute(data))
        title(main = paste(attr(data, "cond")), font.main = 1)
        text(1,0.15,"0.1",col="gray")
        text(1,0.55,"0.5",col="gray")        
    }
    sin.amat[sin.amat < alpha] <- 2
    sin.amat[sin.amat != 2] <- 0
    sin.amat[sin.amat == 2] <- 1
    diag(sin.amat) <- 0
    names(sin.lt) <- leg
    invisible(list(pval=sin.lt,amat=sin.amat))
}

aic.ag <-
function (fit, S, n) 
{
    p <- length(fit$Shat[1, ])
    q <- p * (p - 1)/2 - fit$df
    npars <- p + q
    fitS <- -n * log(2 * pi) - n * log(det(S)) - (n * p)
    aic <- fit$dev + fitS + 2 * npars
    return(list(aic = aic, npars = npars))
}


set.seed(34)

