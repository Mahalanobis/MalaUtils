data_preprocess <- function(  XT,                                 # training X set
                              XS,                                 # test X set
                              YT,                                 # training Y traget
                              miss.cutoff = 0.995,                # % of max missing values
                              num.min.lvs = 7,                    # min number of unique values for numeric feats
                              rare.cutoff = ceiling( nrow(XT)*0.01 ), # min freq for categorical feats
			      cat.trans="None",                   # categorical transformation options
			      num.trans="None",                   # numeric transformation options
			      num.miss="None"){                   # impute missing values 


								
require(data.table)

### MISSING FEATS #############################################################################

### remove all missing features
all.missing <- names(which(sapply(XT,function(x) length(which(is.na(x))))> nrow(XT)*miss.cutoff ))
if ( length(all.missing)>0 ){
	XT <- XT[,setdiff(names(XT),all.missing),with=FALSE]
	XS <- XS[,setdiff(names(XS),all.missing),with=FALSE]
}
cat("\nStep 01: removed",length(all.missing),"features (missing values)\n")
	
### feats type
cat.feats <- names(which(sapply(XT,class)=="character"))
num.feats <- setdiff( names(XT) , cat.feats )


### NUM FEATS (with low levels) ###############################################################

### evaluate number of unique values
num.minlvs.feats <- names(which(sapply(XT[,num.feats,with=FALSE],function(x) length(unique(x)) ) < num.min.lvs))
if ( length(num.minlvs.feats)>0 ){
	for ( j in num.minlvs.feats ) { 
			set(XT,i=NULL,j, as.character(XT[[j]]) )
			set(XS,i=NULL,j, as.character(XS[[j]]) )
	}
}
cat("\nStep 02: coerced",length(num.minlvs.feats),"numerical features in categorical ones (low unique values)\n")

### update feats type
cat.feats <- union( cat.feats , num.minlvs.feats )
num.feats <- setdiff( num.feats , num.minlvs.feats )


### NUM FEATS (with many missing values) ######################################################

### evaluate non missing presence 
num.low.presence <- names(  which( sapply(XT[,num.feats,with=FALSE],
                            function(x) length(which(!is.na(x)))/nrow(XT) ) < 2*rare.cutoff/nrow(XT) ) ) 
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
cat("\nStep 03: coerced",length(num.low.presence),"numerical features in categorical ones (missing values)\n")

### update feats type
cat.feats <- union( cat.feats , num.low.presence )
num.feats <- setdiff( num.feats , num.low.presence )


### CAT FEATS #################################################################################

for ( j in cat.feats ) { 
	### replace rare categories
	XT.rare.cat.feat <- names(which( table(XT[[j]]) < rare.cutoff ))
	if( length(XT.rare.cat.feat)>0 & (!j %in% num.low.presence) ){ 
		set(XT,which( XT[[j]] %in% XT.rare.cat.feat ),j, "RARE" ) 
		set(XS,which( XS[[j]] %in% XT.rare.cat.feat ),j, "RARE" )
	} 
	### impute missing category
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
	### check XS levels
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
cat("\nStep 04: removed",length(cat.noinfo),"categorical features (non informative)\n")

### update feats type
cat.feats <- union( cat.feats , num.minlvs.feats )


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

### update feats type
cat.feats <- names(which(sapply(XT,class)=="character"))
num.feats <- setdiff( names(XT) , cat.feats )

### cat feats to factors
if ( length(cat.feats)>0 ){
	for ( j in cat.feats ) { 
			set(XT,i=NULL,j, as.factor(XT[[j]]) )
			set(XS,i=NULL,j, factor(XS[[j]], levels=levels(XT[[j]])) )
	}
}
cat("\nStep 05: coerced",length(cat.feats),"categorical features in factors\n")


summary(XT)

							
### NUM FEATS #################################################################################


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

}
							

