stop()
###########################################################################
#              plot with arrows for all the SRM components                #
###########################################################################

### 0. General preparation ###
# example dataset
  data(two.groups)
  clinical <- subset(two.groups, group==1)

# subtract the correct table from the output
  x <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data = clinical, means=T)
  
  eff <- parameterEstimates(x$fit)
  MS <- eff[grepl(".means.", eff$label, fixed=TRUE), c(1, 5:10)]
    # these 2 are the exact same lines as on Github
  est_act <- MS[2:5,1:2]
  est_par <- MS[6:9,1:2]
  est_rel <- MS[10:21,1:2]
  

# Create axis & labels
  l_actor <- rep(c('C1','C2', 'F', 'M'), each=3)
      # If only want one line per actor effect (i.e. version 2):
      # l_actor <- c('C1','C2', 'F', 'M')
  l_partner <- c('C2','F','M','C1', 'F','M','C1','C2', 'M','C1','C2','F')
  l_dyad <- c('sib', 'c1/f', 'c1/m', 'sib', 'c2/f', 'c2/m', 'c1/f','c2/f','mar','c1/f','c2/f', 'mar')
  
  plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim=c(-1,1),xlim=c(0,10.5))
  axis(side=2,las=2, seq(-1,1, by =0.2))
  segments(x0=-0.5, x1=10.5, y0=c(0, -1), y1=c(0,-1), lty=c(2,1))
      # in the previous 3 lines 0 represents the family mean, in order to show the real value, the previous line should be (lets call it version 3)
      # note that MS[1,2] is the familymean substracted from the output
      # plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim=c(MS[1,2]-0.8,MS[1,2]+0.8),xlim=c(0,10.5))
      # axis(side=2,las=2, seq(round(MS[1,2]-0.8,2),round(MS[1,2]+0.8,2), by =0.2))
      # segments(x0=-0.5, x1=10.5, y0=c(MS[1,2], MS[1,2]-0.8), y1=c(MS[1,2],MS[1,2]-0.8), col=c("green3","black"))
  mtext(c('actor', l_actor), at=c(-1, 0.25, 1, 1.75, 3, 3.75, 4.5, 5.75, 6.5, 7.25, 8.5, 9.25, 10),side=1,line=0)
      # version 2:
      # do not use the previous line, but use instead:
      # mtext(c('actor', l_actor), at=c(-1, 1, 3.75, 6.5, 9.25),side=1,line=0, col = "blue")
      # axis(1,at=c(0,2),col="blue",line=1,labels=rep("",2),lwd.ticks=0)
      # axis(1,at=c(2.75,4.75),col="blue",line=1,labels=rep("",2),lwd.ticks=0)
      # axis(1,at=c(5.5,7.5),col="blue",line=1,labels=rep("",2),lwd.ticks=0)
      # axis(1,at=c(8.25,10.25),col="blue",line=1,labels=rep("",2),lwd.ticks=0)
  mtext(c('partner', l_partner), at=c(-1, 0.25, 1, 1.75, 3, 3.75, 4.5, 5.75, 6.5, 7.25, 8.5, 9.25, 10),side=1,line=1)
  mtext(c('dyad', l_dyad), at=c(-1, 0.25, 1, 1.75, 3, 3.75, 4.5, 5.75, 6.5, 7.25, 8.5, 9.25, 10),side=1,line=2, cex=c(1,rep(2/3,each=12)))


# Arrows
  # define the x-position of the arrows
  xpos_actor <- c(0.15, 0.9, 1.65,2.9, 3.65,4.4,5.65, 6.4, 7.15, 8.4,9.15, 9.9)
      # version 2:
      # xpos_actor <- c(0.15, 2.9, 5.65, 8.4)
  xpos_partner <- c(0.30, 1.05, 1.8, 3.05, 3.8, 4.55, 5.8, 6.55, 7.3, 8.55, 9.3, 10.05)
  xpos_rel <- c(0.45, 1.2, 1.95, 3.2, 3.95, 4.70, 5.95, 6.7, 7.45, 8.7, 9.45, 10.2)
      # version 3: replace zero by family mean
      # ypos_actor<- rep(MS[1,2]+est_act[,2], each=3)
      # ypos_part <- c(MS[1,2]+est_par[c(2:4,1,3:4,1:2,4,1:3),2])
      # ypos_rel <- MS[1,2]+est_rel[,2]

    # y-position of the arrows
  ypos_actor<- rep(est_act[,2], each=3)
  ypos_part <- c(est_par[c(2:4,1,3:4,1:2,4,1:3),2])
  ypos_rel <- est_rel[,2]

    # draw the arrows
  arrow_act <- arrows(x0=xpos_actor, x1=xpos_actor, y0=0, y1=ypos_actor, length=0.1)
      # version 2: add in the previous line: col="blue"
      # verion 3: replace zero by family mean
      # arrow_act <- arrows(x0=xpos_actor, x1=xpos_actor, y0=MS[1,2], y1=ypos_actor, length=0.1)
  arrow_par <- arrows(x0=xpos_partner, x1=xpos_partner, y0=ypos_actor, y1=(ypos_actor+ypos_part), length=0.1, lty=6)
      # version 3: arrow_par <- arrows(x0=xpos_partner, x1=xpos_partner, y0=ypos_actor, y1=(ypos_actor+ypos_part-MS[1,2]), length=0.1, lty=6)
  arrow_rel <- arrows(x0=xpos_rel, x1=xpos_rel, y0=(ypos_actor+ypos_part), y1=(ypos_actor+ypos_part+ypos_rel-(2*MS[1,2])), length=0.1, lty=2)
  legend("topright" , c('actor', 'partner', 'relationship'), lty=c(1,6,2), horiz=F)
      # version 2: add in the previous line: col=c("blue", "black", "black")
      # version 3: replace zero + green line in legend
      # arrow_rel <- arrows(x0=xpos_rel, x1=xpos_rel, y0=(ypos_actor+ypos_part-MS[1,2]), y1=(ypos_actor+ypos_part+ypos_rel-(2*MS[1,2])), length=0.1, lty=2)
      # legend("topright" , c('family', 'actor', 'partner', 'relationship'), lty=c(1,6,2), horiz=F, col=c("green3","black","black","black"))


    # with the shape-package the arrowheads look nicer
      library(shape)
      arrow_act <- Arrows(x0=xpos_actor, x1=xpos_actor, y0=0, y1=ypos_actor, arr.length=0.2)
      arrow_par <- Arrows(x0=xpos_partner, x1=xpos_partner, y0=ypos_actor, y1=(ypos_actor+ypos_part), arr.length=0.2, lty=6)
      arrow_rel <- Arrows(x0=xpos_rel, x1=xpos_rel, y0=(ypos_actor+ypos_part), y1=(ypos_actor+ypos_part+ypos_rel-(2*MS[1,2])), arr.length=0.2, lty=2)
      legend("topright" , c('actor', 'partner', 'relationship'), lty=c(1,6,2), horiz=F)





###########################################################################
#         plot comparing actor and partner effect with family mean        #
###########################################################################


### 0. Preparation (same as in the previous code) ###
  # subtract the correct table from the output
  x <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data = clinical, means=T)
  parameterEstimates(x$fit)
  eff <- parameterEstimates(x$fit)
  MS <- eff[grepl(".means.", eff$label, fixed=TRUE), c(1, 5:10)]
         #these 2 are the exact same lines as on Github
  est_FE <- MS[1,2]
  est_act <- MS[2:5,2]
  est_par <- MS[6:9,2]

#create axis & three lines of labels for the x-axis
  roles <- c('C1','C2', 'F', 'M')
  plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim=c(-0.5,0.5),xlim=c(0,6))
  axis(side=2,las=2, seq(-0.5,0.5, by =0.1))
  segments(x0=-0.3, x1=6, y0=c(0, -0.5), y1=c(0,-0.5), lty=c(2,1))
     # Version 2: family mean as reference (instead of zero) + colors for the different effects
    # plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim=c(MS[1,2]-0.5,MS[1,2]+0.5),xlim=c(0,6))
    # axis(side=2,las=2, seq(round(MS[1,2]-0.5,2),round(MS[1,2]+0.5,2), by =0.1))
    # segments(x0=-0.3, x1=6, y0=c(MS[1,2], MS[1,2]-0.5), y1=c(MS[1,2],MS[1,2]-0.5), lty=c(3,1), col=c("green3", "black"))
  mtext(roles, at=c(0.5,2,3.5,5),side=1,line=0)


  #arrows
  #define the x-position of the arrows
  xpos_act <-c(0.35,1.85,3.35, 5.15)
  xpos_par <- c(0.65,2.15,3.65,5.35)

  # draw the arrows
  arrow_act <- arrows(x0=xpos_act, x1=xpos_act, y0=0, y1=est_act, length=0.1)
  arrow_par <- arrows(x0=xpos_par, x1=xpos_par, y0=0, y1=est_par, length=0.1, lty = 2)
  legend(x=4,y=0.45,c('actor', 'partner'), lty=c(1,2), horiz=F)
    # version 2:
    # arrow_act <- arrows(x0=xpos_act, x1=xpos_act, y0=MS[1,2], y1=MS[1,2]+est_act, length=0.1, col="blue")
    # arrow_par <- arrows(x0=xpos_par, x1=xpos_par, y0=MS[1,2], y1=MS[1,2]+est_par, length=0.1, lty = 2, col="brown2")
    # legend(x=4,y=MS[1,2]+0.45,c('family', 'actor', 'partner'), col=c("green3", "blue","brown2"), lty=c(3,1,2), horiz=F)











