# different plot types
#
par(ask=FALSE)
plot(c(10,20),c(1,13),col="white",xlab="",ylab="",main="type=",xaxt="n",yaxt="n",cex.main=2.5)
ltypes = c("l","p","b","c","o","s","S","h")
lpos = c(seq(0,10,2),13,15)
for (i in seq_along(ltypes)) {
  lines(lpos[i] + 1:20,1:20, type=ltypes[i],lwd=3)
}
text(9.5+1:8,12-1:8,c("l","p","b","c","o","s","S","h"),cex=2,col="red")


#
# different line types
#
plot(c(10,20),c(0,10),col="white",xlab="",ylab="",main="lty=",xaxt="n",ylim=c(0,9.6),cex.main=2.5)
lnames = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash","51","9396","848481")
for (i in 1:10) {
  abline(h=i-1, lty=lnames[i],lwd=3)
  text(15, i-0.5, lnames[i], cex=2)
}

#
# different point sizes
#
plot(c(-0.5,1.5),c(0,3),col="white",xlab="",ylab="",main="cex=",xaxt="n",yaxt="n",cex.main=2.5)
for (i in seq(0,3,0.2)) {
  points(0,i, pch=16,cex=i)
  text(1,i, paste(i), cex=i)
}

#
# different line widths
#
plot(c(10,20),c(2^-3,2^5.5),col="white",xlab="",ylab="",main="lwd=",xaxt="n",log="y",cex.main=2.5,yaxp=c(0.1,50,2))
for (i in 1:9) {
  lines(c(11,19),c(2^(i-4),2^(i-4)), lwd=2^(i-4))
  text(15, 2^(i-4+0.5), 2^(i-4), cex=2) 
}


#
# different point symbols
#
plot(c(0,4.5),c(0,4),col="white",xlab="",ylab="",main="pch=",xaxt="n",yaxt="n",cex.main=2.5)
for (i in 0:24) {
  points(i %% 5, i %/% 5, pch=i,cex=2)
  text(0.3+ i %% 5, i %/% 5, i, cex=2)
}
