source("packages.R")
source("database_connection.R")
source("logging.R")


sql_query = "select orders_id,date_purchased from orders where date(date_purchased) >= '2016-01-01' order by date_purchased desc"
data <- dbGetQuery(igpnewConnProd,sql_query)
names(data)<-c("product","date")
data$date<-as.Date(as.character(data$date),format= "%Y-%m-%d %H:%M:%S")
data$short.date = strftime(data$date, "%Y/%m")
data$flag<-1
aggr.stat = aggregate(flag~ product+short.date, data= data, FUN = sum)
pl<-cast(aggr.stat, product~short.date, sum)
l<-t(pl)
# Methods 
#"ACF" Autocorrelation-based method.  diss.ACF.
#  "AR.LPC.CEPS" Linear Predictive Coding ARIMA method. This method has two value-per-
#   series arguments, the ARIMA order, and the seasonality. diss.AR.LPC.CEPS.
#  "AR.MAH" Model-based ARMA method.  diss.AR.MAH.
#  "AR.PIC" Model-based ARMA method. This method has a value-per-series argument, the ARIMA order.  diss.AR.PIC.
# "CDM" Compression-based dissimilarity method.  diss.CDM.
# "CID" Complexity-Invariant distance.  diss.CID.
# "COR" Correlation-based method.  diss.COR.
# "CORT" Temporal Correlation and Raw values method.  diss.CORT. "DTWARP" Dynamic Time Warping method.  diss.DTWARP. "DWT" Discrete wavelet transform method.  diss.DWT.
# "EUCL" Euclidean distance.  diss.EUCL. For many more convetional distances,  link[stats]{dist}, though you may need to transpose the dataset.
# "FRECHET" Frechet distance.  diss.FRECHET.
# "INT.PER" Integrate Periodogram-based method.  diss.INT.PER. "NCD" Normalized Compression Distance.  diss.NCD.
# "PACF" Partial Autocorrelation-based method.  diss.PACF.
# "PDC" Permutation distribution divergence. Uses the pdc package.  pdcDist for ad- ditional arguments and details. Note that series given by numeric matrices are interpreted row-wise and not column-wise, opposite as in pdcDist.
# "PER" Periodogram-based method.  diss.PER.
# "PRED" Prediction Density-based method. This method has two value-per-series agument,
# the logarithm and difference transform.  diss.PRED.
# "MINDIST.SAX" Distance that lower bounds the Euclidean, based on the Symbolic Aggre-
#   gate approXimation measure.  diss.MINDIST.SAX.
# "SPEC.LLR" Spectral Density by Local-Linear Estimation method.  diss.SPEC.LLR. 
# "SPEC.GLK" Log-Spectra Generalized Likelihood Ratio test method.  diss.SPEC.GLK. 
# "SPEC.ISD"IntregatedSquaredDifferencesbetweenLog-Spectrasmethod.diss.SPEC.ISD.
tsdist <- diss( t(l) , "ACF", p=0.05)
names(tsdist) <- colnames(l)
#perform hierachical clustering to the dist object
hc <- hclust(tsdist)
cluster<-cutree(hc)
#show the results
plot(hc)
pvalues.clust(tsdist, 0.05)