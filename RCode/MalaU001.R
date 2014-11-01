#data_preprocess <- function(  XT,                                 # training X set
#                              XS,                                 # test X set
#                              YT,                                 # training Y traget
#                              miss.cutoff = 0.995,                # % of max missing values
#                              num.min.lvs = 7,                    # min number of unique values for numeric feats
#                              rare.cutoff = ceiling( nrow(XT)*0.01 ), # min freq for categorical feats
#			      cat.trans="None",                   # categorical transformation options
#			      num.trans="None",                   # numeric transformation options
#			      num.miss="None"){                   # impute missing values 


								
require(data.table)

miss.cutoff = 0.99
num.min.lvs = 20
rare.cutoff = ceiling( nrow(XT)*0.02 )

#cat.trans = "RelRisk"
cat.trans = "HotEncode"
#cat.trans = "None"

num.miss = "Median"	
num.skew.trans = "LogSkew"

ctree.ft = TRUE							
								
### MISSING FEATS #############################################################################

### remove all missing features
all.missing <- names(which(sapply(XT,function(x) length(which(is.na(x))))> nrow(XT)*miss.cutoff ))
if ( length(all.missing)>0 ){
	XT <- XT[,setdiff(names(XT),all.missing),with=FALSE]
	XS <- XS[,setdiff(names(XS),all.missing),with=FALSE]
}
cat("\nCheck01: removed",length(all.missing),"features (missing values)\n")
	

### feats type
cat.feats <- names(which(sapply(XT,class)=="character"))
num.feats <- setdiff( names(XT) , cat.feats )

### CTREE APPROACH
if ( ctree.ft==TRUE ){
	for ( j in num.feats ) { 
	require(party)
	model.data <- data.frame(cbind(Y=YT,X=XT[[j]]))
	model <- ctree(as.factor(Y)~X,data=model.data,controls=ctree_control(maxdepth=4))
	new.j <- unlist(lapply(predict(model,newdata=NULL,type="prob"),function(x) x[2])) 
	if( length(unique(new.j))==1 ){ next } else { 
		val.j <- unlist(lapply(predict(model,newdata=data.frame(X=XS[[j]]),type="prob"),function(x) x[2]))	
		set(XT,i=NULL,(paste0("CT.",j)), new.j )
		set(XS,i=NULL,(paste0("CT.",j)), val.j )	
	}	
	}
}
### update feats type
#num.feats <- setdiff( names(XT) , cat.feats )

### NUM FEATS (with low levels) ###############################################################

### evaluate number of unique values
num.minlvs.feats <- names(which(sapply(XT[,num.feats,with=FALSE],function(x) length(unique(x)) ) < num.min.lvs))
if ( length(num.minlvs.feats)>0 ){
	for ( j in num.minlvs.feats ) { 
			set(XT,i=NULL,j, as.character(XT[[j]]) )
			set(XS,i=NULL,j, as.character(XS[[j]]) )
	}
}
cat("\nCheck02: coerced",length(num.minlvs.feats),"numerical features in categorical ones (low unique values)\n")

### update feats type
cat.feats <- union( cat.feats , num.minlvs.feats )
num.feats <- setdiff( num.feats , num.minlvs.feats )


### NUM FEATS (with many missing values) ######################################################

### evaluate non missing presence (1)
num.dummy.miss.feats <- names( which( sapply(XT[,num.feats,with=FALSE],function(x) length(which(!is.na(x)))/nrow(XT) ) < 2*rare.cutoff/nrow(XT) ) ) 

if ( length(num.dummy.miss.feats)>0 ){
	for ( j in num.dummy.miss.feats ) { 
		set(XT,which(!is.na(XT[[j]])),j,1)
		set(XT,which(is.na(XT[[j]])),j,0)
		set(XS,which(!is.na(XS[[j]])),j,1)
		set(XS,which(is.na(XS[[j]])),j,0)
	}
}
cat("\nCheck03: coerced",length(num.dummy.miss.feats),"numerical features in dummy ones (missing/no missing values)\n")

### update feats type
num.feats <- setdiff( num.feats , num.dummy.miss.feats )

### evaluate non missing presence (2)
num.low.presence <- names( which( 	sapply(XT[,num.feats,with=FALSE],function(x) length(which(!is.na(x)))/nrow(XT) ) > 2*rare.cutoff/nrow(XT) &
									sapply(XT[,num.feats,with=FALSE],function(x) length(which(!is.na(x)))/nrow(XT) < 0.075 )	) ) 

if ( length(num.low.presence)>0 ){
	for ( j in num.low.presence ) { 
			XT.median.cutoff <- median( XT[[j]] ,na.rm=TRUE )
			new.j <- rep(NA,length(XT[[j]]))
			val.j <- rep(NA,length(XS[[j]]))
			new.j[which(XT[[j]]<=XT.median.cutoff)] <- "LOW"
			new.j[which(XT[[j]]>XT.median.cutoff)] <- "HIGH"
			val.j[which(XS[[j]]<=XT.median.cutoff)] <- "LOW"
			val.j[which(XS[[j]]>XT.median.cutoff)] <- "HIGH"			
			set(XT,i=NULL,j, new.j )
			set(XS,i=NULL,j, val.j )
	}
}
cat("\nCheck04: coerced",length(num.low.presence),"numerical features in categorical ones (missing values)\n")

### update feats type
cat.feats <- union( cat.feats , num.low.presence )
num.feats <- setdiff( num.feats , num.low.presence )
num.feats <- union( num.feats , num.dummy.miss.feats )


### CAT FEATS #################################################################################

for ( j in cat.feats ) { 
	
	# replace rare categories
	XT.rare.cat.feat <- names(which( table(XT[[j]]) < rare.cutoff ))
	if( length(XT.rare.cat.feat)>0 & (!j %in% c(num.low.presence)) ){ 
		set(XT,which( XT[[j]] %in% XT.rare.cat.feat ),j, "RARE" ) 
		set(XS,which( XS[[j]] %in% XT.rare.cat.feat ),j, "RARE" )
	} 
	
	# impute missing category
	XT.miss.cat.ids <- which(is.na(XT[[j]]))
	XT.rare.cat.ids <- which(XT[[j]]=="RARE")
	XS.miss.cat.ids <- which(is.na(XS[[j]]))
	XS.rare.cat.ids <- which(XS[[j]]=="RARE")
	if( length(XT.rare.cat.ids)+length(XT.miss.cat.ids) > 0 ) {	
		if ( (length(XT.rare.cat.ids) > rare.cutoff & length(XT.miss.cat.ids) > rare.cutoff) ) {
				set(XT,XT.miss.cat.ids,j, "MISS" )
				set(XS,XS.miss.cat.ids,j, "MISS" )
		} else {
				set(XT,XT.miss.cat.ids,j, "RARE" )
				set(XS,XS.miss.cat.ids,j, "RARE" )
		}
	}	
	
	# check XS levels
	XS.levels <- unique(XS[[j]])
	XT.levels <- unique(XT[[j]])
	XS.levels.diff <- setdiff(XS.levels,XT.levels)
	if( length(XS.levels.diff)>0 ){ 
		set(XS,which( XS[[j]] %in% XS.levels.diff ),j, "RARE" )
	}
}

### remove non informative categorical features
cat.noinfo <- names(which(sapply(XT[,cat.feats,with=FALSE],function(x) length(unique(x)))==1 ))
if ( length(cat.noinfo)>0 ){
	XT <- XT[,setdiff(names(XT),cat.noinfo),with=FALSE]
	XS <- XS[,setdiff(names(XS),cat.noinfo),with=FALSE]
}
cat("\nCheck05: removed",length(cat.noinfo),"categorical features (non informative)\n")

### update feats type
cat.feats <- setdiff( cat.feats , cat.noinfo )

### trasform 2-lvs cat feats to dummy
cat.2dummy <- names(which(sapply(XT[,cat.feats,with=FALSE],function(x) length(unique(x)))==2 ))
if ( length(cat.2dummy)>0 ){
	for ( j in cat.2dummy ) { 
		XT.minimum.lvs <- names(which.min(table( XT[[j]] )))
		new.j <- as.integer(rep(0,length(XT[[j]])))
		val.j <- as.integer(rep(0,length(XS[[j]])))
		new.j[which(XT[[j]]==XT.minimum.lvs)] <- 1
		val.j[which(XS[[j]]==XT.minimum.lvs)] <- 1	
		set(XT,i=NULL,j, new.j )
		set(XS,i=NULL,j, val.j )
	}
}
cat("\nCheck06: coerced",length(cat.2dummy),"categorical features in dummy ones (only 2 levels)\n")

### update feats type
cat.feats <- setdiff( cat.feats , cat.2dummy )
num.feats <- union( num.feats , cat.2dummy )



### CAT FEATS 2 DUMMY
if (cat.trans=="HotEncode"){
	dummy.feats <- c()
	for ( j in cat.feats ) { 	
		inds <- unique(XT[[j]])
		dummy.feats <- c(dummy.feats,paste0(j,".",inds))
		XT[,(paste0(j,".",inds)):=lapply(inds,function(x) (XT[[j]]==x)+0 )] 
		XS[,(paste0(j,".",inds)):=lapply(inds,function(x) (XS[[j]]==x)+0 )]
	}
	noinfo.dummy.feats <- c( 	names(which(sapply(XT[,dummy.feats,with=FALSE],sum) < rare.cutoff)) ,
								names(which(sapply(XT[,dummy.feats,with=FALSE],sum) == nrow(XT) )) )

	XT <- XT[,setdiff(names(XT),c(cat.feats,noinfo.dummy.feats)),with=FALSE]
	XS <- XS[,setdiff(names(XS),c(cat.feats,noinfo.dummy.feats)),with=FALSE]
}

### CAT FEATS 2 RELRISK
if (cat.trans=="RelRisk"){
	for ( j in cat.feats ) { 	
		inds <- unique(XT[[j]])
		new.j <- as.numeric(rep(NA,length(XT[[j]])))
		val.j <- as.numeric(rep(NA,length(XS[[j]])))
		for ( i in inds ) { 
			if( length( which( XT[[j]]==i ) ) > rare.cutoff ) { 
				YT.rel.risk <- mean(YT[ which(XT[[j]]==i) ])
				new.j[which( XT[[j]]==i )] <- YT.rel.risk
				val.j[which( XS[[j]]==i )] <- YT.rel.risk
			}  
		}
		new.j[is.na(new.j)] <- mean(YT)
		val.j[is.na(val.j)] <- mean(YT)
		set(XT,i=NULL,j, new.j )
		set(XS,i=NULL,j, val.j )
	}
}

# update feats type
cat.feats <- names(which(sapply(XT,class)=="character"))
num.feats <- setdiff( names(XT) , cat.feats )



							
### NUM FEATS #################################################################################

num.minlvs.feats <- names(which(sapply(XT[,num.feats,with=FALSE],function(x) length(unique(x)) ) < num.min.lvs)) 
num.skew <- names(which(sapply(XT[,setdiff(num.feats,num.minlvs.feats),with=FALSE],skewness,na.rm=TRUE)>2 ))

### LOGSKEW
if (num.skew.trans=="LogSkew"){
	for ( j in num.skew ) { 	
		if( min(na.omit(XT[[j]]))>=0 & min(na.omit(XS[[j]]))>=0 ){
			new.j <- as.numeric(rep(NA,length(XT[[j]])))
			val.j <- as.numeric(rep(NA,length(XS[[j]])))
			new.j[which( !is.na(XT[[j]]) )] <- log(XT[[j]][which( !is.na(XT[[j]]) )] +1e-06)
			val.j[which( !is.na(XS[[j]]) )] <- log(XS[[j]][which( !is.na(XS[[j]]) )] +1e-06)
			set(XT,i=NULL,j, new.j )
			set(XS,i=NULL,j, val.j )
		} else { next }
	}
}
					
### MISS NUM FEATS 2 MEAN
if (num.miss=="Mean"){
	for ( j in num.feats ) { 	
		set(XT,which(is.na(XT[[j]])),j, mean( XT[ ,XT[[j]] ], na.rm=TRUE) )
		set(XS,which(is.na(XS[[j]])),j, mean( XT[ ,XT[[j]] ], na.rm=TRUE) )
	}
}

### MISS NUM FEATS 2 MEDIAN
if (num.miss=="Median"){
	for ( j in num.feats ) { 	
		set(XT,which(is.na(XT[[j]])),j, median( XT[ ,XT[[j]] ], na.rm=TRUE) )
		set(XS,which(is.na(XS[[j]])),j, median( XT[ ,XT[[j]] ], na.rm=TRUE) )
	}
}

### MISS NUM FEATS 2 ZERO
if (num.miss=="Zero"){
	for ( j in num.feats ) { 	
		set(XT,which(is.na(XT[[j]])),j, 0 )
		set(XS,which(is.na(XS[[j]])),j, 0 )
	}
}
							


### CAT 2 FACTORS														
if ( length(cat.feats)>0 ){
	for ( j in cat.feats ) { 
			set(XT,i=NULL,j, as.factor(XT[[j]]) )
			set(XS,i=NULL,j, factor(XS[[j]], levels=levels(XT[[j]])) )
		}
	}	

cat("\nCheck07: coerced",length(cat.feats),"categorical features in factors\n")

