## Matrix ops exx
tempvec=c(30:33,70:73, 50:53)
amat = matrix( tempvec ,ncol=3)
colnames(amat) = c('AK','SFO','Chitown')
rownames(amat) = paste('Mar',14:17,sep='_') # paste concatenates, sep is the string to separate first entry from each of the nums

tempvec2=c(80:83,60:63, 90:93)
bmat = matrix( tempvec ,ncol=3)
colnames(bmat) = c('LAX','Seattle','Hawaii')
rownames(bmat) = paste('Mar',18:21,sep='_') 

# Merging tables together

addincoldir = cbind(amat,bmat)
addinrowdir = rbind(amat,bmat)

### Exx of adding a new row to a mat
newrow=matrix(c(1,2,3,4,5,6),nrow=1)
rownames(newrow) = 'Mar_18'
newrowmat=rbind(addincoldir,newrow)

### Exx of adding a new col to a mat
newcol=matrix(c(1:8),ncol =1)
colnames(newcol) = 'FreakyCity'
newcolmat=cbind(addinrowdir,newcol)

## Statl ops on the mat
mat = addincoldir
min(mat)
max(mat)
range(mat)
sd(mat)
rowMeans(mat)
colMeans(mat)
apply(mat,1,FUN=sd) # Apply standev per row
apply(mat,2,FUN=sd) # Apply standev per col
cor(mat) # Corrlns
summary(mat)    # Summary stats per col
summary(t(mat)) # Summary stats per row

        
