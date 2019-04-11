library("MASS")

#### definitions ###

font = "Helvetica"
plotColors = c("steelblue3", "firebrick3", "grey40", "grey40")
width=17.4/2.54
height=17.4/2.54
pdfFile="figures/figure1.pdf"

#### functions ####

expmean <- function(data){
  col=as.numeric(colnames(data))
  for (i in 1:length(colnames(data))){
    data[, i] = data[, i]*col[[i]]
  }
  rowSums(data)
}

plotit <- function(x, data, pval, vars, panel, main="", legpos="top", ylim=NA, plot="EV", 
                   inset=c(0,0), colors=plotColors, 
                   ylab="Expected mean ± expected standard error of the mean",
                   xlab= "Sleepiness (KSS: 1-9)") {
  
  col=rainbow(length(data))
  col=plotColors
  lty=c(1,1,2,3)
  mono=c("grey80", "grey60")
  
  if (plot %in% c("EV", "binary")) {
    plot(data.frame(x, data[1]), type="n", ylab=ylab, xlab=xlab, main=main, ylim=ylim)
    
    labels = c()
    for (i in 1:length(data)) {
      v = vars[i, "var"]
      lines(data.frame(x, data[v]), type="l", lty=lty[i], lwd=1.5, col=col[i])
      if (pval[[v]] <.001) {
        labels = c(labels, paste0(vars[vars$var==v, "label"], " (p<.001)"))
      }
      else{
        labels = c(labels, paste0(vars[vars$var==v, "label"], " (p=", format(pval[[v]], digits=1, nsmall=3), ")"))
      }
    }
    
    if (!is.na(legpos)) {
      legend(legpos,
             labels,
             col=col,
             lwd=1.5,
             lty=lty,
             bty="n",
             y.intersp=1.1,
             inset=inset,
             cex=.75
      )
    }
  }
  else {
    hist(as.numeric(data$MOS_24$kss), col=mono[2] ,  xlab=xlab, main="Sleepiness distribution",
           breaks=seq(1.5, 9.5, 1))
    hist(as.numeric(data$MOS_24$kss[data$MOS_24$sd == 0]), add=TRUE, col=mono[1], xlab="",
     breaks=seq(1.5, 9.5, 1))
    axis(1, at=seq(2,9, 1), labels=seq(2, 9, 1))
    
    if (!is.na(legpos)) {
      legend(legpos,
             c("Normal sleep", "Sleep deprived", "(p<.001)"),
             col=mono,
             lwd=c(3, 3, NA),
             lty=c(1,1,NA),
             bty="n",
             y.intersp=1.1,
             inset=inset,
             cex=.8
      )
    }
    
  }
  mtext(panel, side=3, outer=FALSE, line=.8, adj=-.2, font=2, cex=1.5)
}

#### predict data ####

load("data/sleepiness_as_motivation_models.RDta")
panels = read.csv("data/motivation_tables - Panels.csv", stringsAsFactors = FALSE)
variables = read.csv("data/motivation_tables - Variables.csv", stringsAsFactors = FALSE)
pv=merge(panels, variables)

data=list(newdata=data.frame(kss=seq(2,9,.1)))
pval=c()
d=data
for (v in unique(pv$var)) {
  if (pv[pv$var == v, "plot" ] == "EV") {
    t = predict(models[[v]]$logistic$kss, newdata = d$newdata, type="p")
    y=expmean(t)
  }
  if (pv[pv$var == v, "plot" ] == "binary") {
    t = predict(models[[v]]$logistic$kss, newdata = d$newdata, type="p")
    y=rowSums(t[ ,2:length(colnames(t))])
  }
  if (pv[pv$var == v, "plot" ] == "hist") {
    y=data.frame(sd=models$data$sd, kss=models$data$kss)
  }
  data[[v]] = y
  if (!is.null(models[[v]]$logistic$p$kss)) {
    pval[[v]] = models[[v]]$logistic$p$kss
    }
}

#### plot data ####

pdf(pdfFile, width=width, height=height, fonts=font)

op <- par(no.readonly = TRUE)
par(mfrow=c(3,3), mar=c(3, 3, 3, .5), mgp=c(1.7, .5, 0), cex=.6, family=font)

for (P in LETTERS[1:9]) {
  l=panels[panels$panel==P, ]
  v=variables[variables$panel==P, ]
  plotit(d$newdata, data=data[v$var], pval=pval[v$var], panel=P, ylim=c(l$ymin, l$ymax), 
         main=l$plab, ylab=l$ylab, legpos=l$legpos, vars=v, plot=l$plot)
}  

par(op)
dev.off()