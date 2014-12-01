
find.bin.equal.n<-function(x,nbin)
{ cutpoints<-quantile(x,(0:nbin)/nbin)
  return(cut(x,cutpoints,include.lowest=TRUE))
}  


makeCOVbin<-function(COV.data,N.covbin)
{ if(N.covbin<length(table(COV.data)))
{  COV.bin<-find.bin.equal.n(COV.data,N.covbin)
   COV.bin.ID<-names(table(COV.bin))
} else
{
  COV.bin<-cut(COV.data,as.numeric(names(table(COV.data))))
  COV.bin.ID<-names(table(COV.bin))
}  
return(list(COV.bin=COV.bin,COV.bin.ID=COV.bin.ID))
}


read.PKPDdata<-function(file.name)
{ 
  temp.data<-read.csv(file.name,na.strings=".")
  colnames(temp.data)<-toupper(colnames(temp.data))
  return(temp.data)
}

XYplot.output<-function(output.data,X.name,Y1.name,Y2.name,ID.name,x.lim,y.lim,optionsID)
{   X.t<-output.data[,X.name]
    ID.t<-output.data[,ID.name]
    if(!is.null(Y1.name))
    {  Y1.t<-output.data[,Y1.name]
    } else
    { Y1.t<-NULL
    }
    new.data<-data.frame(X.t=X.t,Y1.t=Y1.t,ID.t=ID.t)
    if(Y2.name!=" ")
    {  Y2.t<-output.data[,Y2.name]
       new.data<-data.frame(new.data,Y2.t=Y2.t)
    }
    if(sum(optionsID==1)==1)
    { low.temp<-lowess(X.t,Y1.t)
      low.x<-low.temp$x; low.y<-low.temp$y
      new.data<-data.frame(new.data,low.x=low.x,low.y=low.y)
    }
    ID.t<-output.data[,ID.name]
    
    P<-ggplot( data=new.data, aes(x=X.t,y=Y1.t))+geom_point()+xlab(X.name)+ylab(Y1.name)

    if(Y2.name!=" ")
    {P<-P+geom_line(aes(x=X.t,y=Y2.t,group=ID.t))+
                  annotate("text",x=x.lim[2]*0.9,y=y.lim[2]*0.9,label=Y2.name)
    }
    P<-P+xlim(x.lim)+ylim(y.lim)
    if(length(optionsID)!=0)
    {  for(i in optionsID)  
       { if(i==1)
         {  
            P<-P+geom_smooth(colour="red",size=1.2)#geom_line(aes(x=low.x,y=low.y),colour="red",size=1.2)
         } else if(i==2)
         {  P<-P+geom_abline(intercept=0,slope=0,colour="blue",size=1.2)               
         } else if(i==3)
         {  P<-P+geom_abline(intercept=0,slope=1,colour="blue",size=1.2)                
         }
       }
    }  
    P
}   

XYplot.orig.with.COV<-function(orig.data,X.name,Y1.name,ID.name,x.lim,y.lim,cov.bin)
{ X.t<-orig.data[,X.name]
  Y1.t<-orig.data[,Y1.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y1.t=Y1.t,ID.t=ID.t,cov.bin=cov.bin$COV.bin)
  ggplot( data=new.data, aes(x=X.t,y=Y1.t))+
    geom_point()+labs(x=X.name,y=Y1.name)+xlim(x.lim)+ylim(y.lim) + facet_wrap(~cov.bin)
  
}   

### Xyplot with ID profile
XYplotwithID.orig<-function(orig.data,X.name,Y1.name,ID.name,x.lim,y.lim)
{ X.t<-orig.data[,X.name]
  Y1.t<-orig.data[,Y1.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y1.t=Y1.t,ID.t=ID.t)
  ggplot( data=new.data, aes(x=X.t,y=Y1.t,group=ID.t))+
    geom_point()+labs(x=X.name,y=Y1.name)+xlim(x.lim)+ylim(y.lim)+geom_line()
  
}   

XYplotwithID.orig.with.COV<-function(orig.data,X.name,Y1.name,ID.name,x.lim,y.lim,cov.bin)
{ 
  X.t<-orig.data[,X.name]
  Y1.t<-orig.data[,Y1.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y1.t=Y1.t,ID.t=ID.t,cov.bin=cov.bin$COV.bin)
  ggplot( data=new.data, aes(x=X.t,y=Y1.t,group=ID.t))+
    geom_point()+labs(x=X.name,y=Y1.name)+xlim(x.lim)+ylim(y.lim)+geom_line()+ facet_wrap(~cov.bin)
  
}   
