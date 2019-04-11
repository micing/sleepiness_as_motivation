library("MASS")

#### definitions ###

font = "Helvetica"
plotColors = c("steelblue3", "firebrick3", "grey40")
width=17.4/2.54
height=17.4/2.54
pdfFile="figures/figureS1.pdf"

#### functions ####

plotit <- function(m, main="", legpos="top", ylab="Probability", panel="A", 
                        xlab= "Sleepiness"){
  d=data.frame(kss=seq(2,9,.1))
  y=predict(m, newdata = d, type="p")
  col=c(plotColors, plotColors, plotColors)
  lty=c(rep(1, length(plotColors)-1), rep(2, length(plotColors)-1),rep(3, length(plotColors)))
  
  plot(d$kss, y[, 1], type="n", ylab=ylab, xlab=xlab, main=main, ylim=c(0,1))
  
  for (i in 1:length(colnames(y))) {
    lines(d$kss, y[, i], type="l", lwd=1, lty=lty[i], col=col[i])
  }
  
  if (!is.na(legpos)) {
    legend(legpos,
           colnames(y),
           col=col,
           lwd=1,
           lty=lty,
           bty="n",
           y.intersp=1,
           title="Response",
           cex=.8
    )
  }
  
  mtext(panel, side=3, outer=FALSE, line=.8, adj=-.2, font=2, cex=.8)
}

#### plot data ####

load("data/sleepiness_as_motivation_models.RDta")
panels = read.csv("data/motivation_tables - Panels.csv", stringsAsFactors = FALSE)
variables = read.csv("data/motivation_tables - Variables.csv", stringsAsFactors = FALSE)

pdf(pdfFile, width=width, height=height, fonts=font)

op <- par(no.readonly = TRUE)
par(mfrow=c(4,6), mar=c(3, 3, 3, .5), mgp=c(1.7, .5, 0), cex=.4, family=font)

var=data.frame()
for (P in LETTERS[1:9]) {var=rbind(var, variables[variables$panel==P, ])}  

for (i in 1:(length(variables$var)-1)) {
  v=var[i, "var" ]
  l=var[i, "label" ]
  plotit(models[[v]]$logistic$kss, main=l, panel=LETTERS[i])

}  

par(op)
dev.off()