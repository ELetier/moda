

### R function for efficiently computing the expected value of partial perfect information (EVPPI) 
### Sadatsafavi et al. "Need for Speed: An efficient algorithm for calculation of single-parameter expected value of partial perfect information"
### Version 1.10. Last update: 06/03/2014


### For latest updats and bug fixes please refer to http://www.core.ubc.ca/software


### Usage: evppi.seg(param,nbs,...)
### INPUT: 
#Param: the NX1 vector of the stochastic parameter (_theta_hat in the manuscript). 
# nbs: the NXD matrix of corresponding net benefits for D decisions (NB_hat in the manuscript). If D=1, it is assumed nbs is the vector of incremental net benefits between two decisions. 
# optinal parameters: use to specify the number of segmentation points between pairs of decision. This information should be submitted in the form of c(a,b,k) where a and b specify the pairs of decisions,  and k (which should be 0, 1, or 2) is the specified number of segmentation points. For example, for a three-decision model, using the function evppi(x1,nbs,c(1,2,0),c(2,3,2)) forces the algorithm not to fit  any segmentation points between decisions 1 and 2, and to fit 2 segmentations between decisions 2 and 3 in calculating the EVPPI for parameter X1. The default is to fit 1 segmentation poin for each pairs of  decisions.


### OUTPUT: A list, with the first item being the calculated EVPPI, and the second item being the vector of segmentaion points.

evppi.seg<-function(param,nbs,...){  
  
  # EL: Disabling graphics
   evppi.graphics = FALSE
  
  if(length(unique(param))==1)
    stop("Error: input parameter is not stochastic")
  
  param<-as.matrix(param)
  nbs<-as.matrix(nbs)
  
  if(dim(param)[2]!=1)
    stop("Error: input parameter should be one column")
  
  
  n<-length(param)
  o<-order(param)
  param<-param[o]
  
  
  
  d<-dim(nbs)[2]
  if(is.vector(nbs) || d==1)
  {
    nbs<-cbind(nbs[o],0)
    d<-2
  } else
    nbs<-nbs[o,];
  
  nSegs<-matrix(1,d,d)
  
  exArgs<-list(...)
  for(obj in exArgs)
  {
    if(is.null(names(obj)))
      if (length(obj)==1)
      {nSegs[1,2]<-obj; nSegs[2,1]<-obj}
    else
    {nSegs[obj[1],obj[2]]<-obj[3]; nSegs[obj[2],obj[1]]<-obj[3];}
  }
  
  segPoints<-c()
  
  for(i in 1:(d-1))
    for(j in (i+1):d)
    {
      message(paste('Fitting ',nSegs[i,j],' segmentation points for decisions ',i,j))
      
      cm<-cumsum(nbs[,i]-nbs[,j])/n
      
      if(nSegs[i,j]==1)
      {
        l<-which.min(cm)
        u<-which.max(cm)
        if(cm[u]-max(cm[1],cm[n])>min(cm[1],cm[n])-cm[l])
          segPoint<-u
        else
          segPoint<-l
        if (segPoint>1 && segPoint<n)
          segPoints<-c(segPoints, segPoint)
      }
      
      if(nSegs[i,j]==2)
      {
        
        
        distMaxMin<-0
        distMinMax<-0
        minL<-Inf
        maxL<--Inf
        
        for(k in 1:n)
        {
          #max-min pattern
          if(cm[k]>maxL)
          {
            maxLP<-k
            maxL<-cm[k]
          }
          else
            if(maxL-cm[k]>distMaxMin)
            {
              distMaxMin<-maxL-cm[k]
              segMaxMinL<-maxLP	
              segMaxMinR<-k
            }	
          
          
          #min-max pattern
          if(cm[k]<minL)
          {
            minLP<-k
            minL<-cm[k]
          }
          else
            if(cm[k]-minL>distMinMax)
            {
              distMinMax<-cm[k]-minL
              segMinMaxL<-minLP	
              segMinMaxR<-k
            }	
        }
        
        siMaxMin<-cm[segMaxMinL]+distMaxMin+(cm[n]-cm[segMaxMinR])
        siMinMax<--cm[segMaxMinL]+distMinMax-(cm[n]-cm[segMinMaxR])
        
        if(siMaxMin>siMinMax)
        {
          segPoint<-c(segMaxMinL,segMaxMinR)
        }
        else
        {
          segPoint<-c(segMinMaxL,segMinMaxR)
        }
        
        if (segPoint[1]>1 && segPoint[1]<n)
          segPoints<-c(segPoints, segPoint[1])
        
        if (segPoint[2]>1 && segPoint[2]<n)
          segPoints<-c(segPoints, segPoint[2])
      }
      
      if(exists('evppi.graphics') && evppi.graphics==FALSE)
        message("Graphic output is disabled. Set evppi.graphics=TRUE to enable graphics")
      else
      {
        x11()
        plot(param,cm)
        for(k in 1:length(segPoint))
        {
          if (segPoint[k]>1 && segPoint[k]<n)
            lines(c(param[segPoint[k]],param[segPoint[k]]),c(min(cm),max(cm)))
        }
        title(paste("Decision",i,"vs.",j))
      }
      
    }
  
  if(length(segPoints)>0)
  {
    segPoints2<-unique(c(0, segPoints[order(segPoints)], n))
    
    evppi<-0
    for(j in 1:(length(segPoints2)-1))
      evppi<-evppi+max(colSums(matrix(nbs[(1+segPoints2[j]):segPoints2[j+1],],ncol=d)))/n    
    evppi<-evppi-max(colMeans(nbs))
    return(list(evppi=evppi,segPoints=param[segPoints2[-c(1,length(segPoints2))]]))
  }
  else
    evppi<-0
  return(list(evppi=evppi,segPoints=c()))
  
}


