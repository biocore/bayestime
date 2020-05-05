# set the script directory as the working directly

# Three main functions
# 1. prepare_data
# 2. visu_data: time plots, by group also (to be added)
# 3. get spline basis

### warning: in basis_setup_sparse, K = # basis, Q= # PCs
### warning: need to check whether my basis results are the same as in basis_setup_sparse

### standarize.y and center.y cannot do separately; has to choose from one of them; thus revise code on 08/16/2019
### see application to MG's code

### prepare data for sfpca model
prepare_data = function(data, unique_subject_id, time_var, response, transform.y='standardize', scale.time=FALSE, group.var=NA){
	# data: target longitudinal data for analysis (must be a data frame)
	# unique_subject_id: the column name corresponding to unique subject id in the data (string)
	# time_var: the column name corresponding to the time variable in the data (string)
	# response: the column name of the intersted response variable
	# standardize.y: the option of whether or not to standardize response variable (True/False) with mean 0 and sd 1
	# scale.time: the option of whether or not to scale the time variable to be within [0, 1] (True/False)

  #print warnings
  data_check = data[, c(as.character(unique_subject_id), as.character(time_var))]
  if (sum(duplicated(data_check)) != 0) return("Each subject need to have unique measurement at each time point ")
  
	# create new ID
	data$ID = as.numeric(as.numeric(data[, unique_subject_id]))
	N = length(unique(data$ID)) # total number of unique subjects

	# convert group id to be numeric
	if (!is.na(group.var)){
	data[, group.var] = as.numeric(as.factor(data[, group.var]))
	}

	# create time 
	if (scale.time == TRUE){
		data$time = (data[, time_var] - min(data[, time_var])) / (max(data[, time_var]) - min(data[, time_var]))
	} else{
		data$time = data[, time_var]
	}

	T = length(unique(data$time)) # total number of sampling time points

	# transform response (code updated on 08/16/2019)
	if (transform.y == 'standardize'){
		data$response = (data[, response] - mean(data[, response], na.rm=T)) / sd(data[, response], na.rm=T)
	} else if (transform.y == 'center'){
		data$response = (data[, response] - mean(data[, response], na.rm=T)) 
	} else {
		data$response = data[, response]
	}	

	
	# re-order the data by ID and time
	data = data[order(data$ID, data$time), ]

	# create visits vector, response and time matrix
	ID.list = unique(data$ID)
	#visits.vector = matrix(rep(0, N*1), nrow=N)
	visits.vector = vector(mode = "numeric", length = N)
	response.list = NULL
	time.matrix = matrix(rep(0, N*T), nrow=N)
	# response.matrix = time.matrix = matrix(rep(0, N*T), nrow=N)
	# rownames(visits.vector) = rownames(response.matrix) = rownames(time.matrix) = ID.list
	# colnames(visits.vector) = 'n_visits'
	# rownames(response.matrix) = rownames(time.matrix) = ID.list
	# colnames(response.matrix) = colnames(time.matrix) = seq(1, T, 1)

	# visits index for each individual when stacking the data
	visits.stop = vector(mode = "numeric", length = N)

	# size index for each individual in covariance matrix
	cov.start = cov.stop = vector(mode = "numeric", length = N)

	# group id based on interested group
	id_group = vector(mode = "numeric", length = N)

	for(i in 1:N){ 
		# visits vector
		subject_i = data[data$ID==ID.list[i],]
		subject_i$n_visits = dim(subject_i)[1]	
		visits.vector[i] = unique(subject_i$n_visits)

		# visits index
		visits.stop[i] = sum(visits.vector)

		# covariance size index
		cov.stop[i] = sum(visits.vector^2)

	    # response matrix
	    # response.matrix[i, ] = c(subject_i$response, rep(0, T - unique(subject_i$n_visits)))
	    response.list = c(response.list, subject_i$response)

		# time matrix
		time.matrix[i, ] = c(subject_i$time, rep(0, T - unique(subject_i$n_visits)))

		# group id based on interested group
		if (!is.na(group.var)){
		id_group[i] = unique(subject_i[, group.var])
		}

		rm(subject_i)
	}	
	visits.start = c(1, visits.stop[-N] + 1)
	cov.start = c(1, cov.stop[-N] + 1)
	cov.size = sum(visits.vector^2)

	prepared_data = list(data=data, num_subjects=N, num_times=T, response.list=response.list, time.matrix=time.matrix,
		                 visits.vector=visits.vector, visits.start=visits.start, visits.stop=visits.stop,
		                 cov.start=cov.start, cov.stop=cov.stop, cov.size=cov.size, id_group=id_group)
	return(prepared_data)
}


### set up spline basis for sparse data
basis_setup_sparse = function(prepared_data, nknots, orth=TRUE, delta=1/10000){
	# prepared_data: longitudinal data after applying prepared_data() function
	# knots: user-defined number of knots
	# orth: default setting for orth should be TRUE (after discussed with Wes on 02/13/2019)

	time_var = prepared_data$data$time
	num_subjects = prepared_data$num_subjects
	num_times = prepared_data$num_times
	S = prepared_data$time.matrix
	V = prepared_data$visits.vector

	# continuous time interval
	time_unique = sort(unique(time_var))
	time_min = min(time_unique)
	time_max = max(time_unique)
	time_cont = seq(time_min, time_max / delta) * delta # chop the entire time interval into many small subintervals
	time_cont = round(time_cont / delta)*delta # to avoid rounding error?

	# specify placement of knots
	qs = 1/(nknots + 1)
	knots = quantile(time_unique, qs)
	if(nknots > 1){
		for(q in 2:nknots){
			knots = c(knots, q*quantile(time_unique,qs))
		}
	}

	knots = as.vector(knots)


	# obtain cubic spline basis
	library('splines')

	##### option 1: force all matrices to have the same size
	# ## 1. for densely sampled time points
	# phi_t_cont=list()
	# phi_t_cont = bs(time_cont, knots=knots, degree=3,intercept=TRUE) # cubic spline, degree=spline_degree

	# # Gram-Schmidt Orthonormalization
	# temp = phi_t_cont
	# orth = TRUE
	# K = nknots + 4 # num of spline basis 

	# for(k in 1:K){
	# 	if(orth==TRUE){
	# 		if(k > 1){
	# 			for(q in 1:(k-1)){
	# 				temp[,k]=temp[,k]-(sum(temp[,k]*temp[,k-q])/
	# 					sum(temp[,k-q]^2))*temp[,k-q];
	# 			}
	# 		}
	# 	}		
	#     temp[,k]=temp[,k]/sqrt(sum(temp[,k]*temp[,k]))
	# }

	# phi_t_cont=t(sqrt(1/delta)*temp)

	# ## 2. for sparsely sampled time points
	# phi_t=list()
	# for(i in 1:num_subjects){
	# 	phi_t[[i]] = array(0,dim=c(K, V[i])) # phi_t: K (number of basis function) * number of total visit for each subject

	# 	for(k in 1:K){
	# 		for(t in 1:V[i]){
	# 			phi_t[[i]][k, t] = phi_t_cont[k, abs(time_cont - S[i, t]) == min(abs(time_cont - S[i, t]))]
	# 		}
	# 	}

	# 	## fill up zeros to make matrices same size
	# 	miss_visits = num_times - V[i]
	# 	fill_zeros = matrix(rep(0, K*miss_visits), nrow=K)
	# 	phi_t[[i]] = cbind(phi_t[[i]], fill_zeros)

	# }

	#### option 2: stack subjects and visits
	## 1. for densely sampled time points
	phi_t_cont=list()
	phi_t_cont = bs(time_cont, knots=knots, degree=3,intercept=TRUE) # cubic spline, degree=spline_degree

	### the same as in setup_basis_sparse so far

	# Gram-Schmidt Orthonormalization
	temp = phi_t_cont
	K = nknots + 4 # num of spline basis 

	for(k in 1:K){
		if(orth==TRUE){
			if(k > 1){
				for(q in 1:(k-1)){
					temp[,k]=temp[,k]-(sum(temp[,k]*temp[,k-q])/
						sum(temp[,k-q]^2))*temp[,k-q];
				}
			}
		}		
	    temp[,k]=temp[,k]/sqrt(sum(temp[,k]*temp[,k]))
	}

	phi_t_cont=t(sqrt(1/delta)*temp)

	## 2. for sparsely sampled time points
	phi_t_stacked=NULL
	phi_t=list()
	for(i in 1:num_subjects){
		phi_t[[i]] = array(0,dim=c(K, V[i])) # phi_t: K (number of basis function) * number of total visit for each subject

		for(k in 1:K){
			for(t in 1:V[i]){
				phi_t[[i]][k, t] = phi_t_cont[k, abs(time_cont - S[i, t]) == min(abs(time_cont - S[i, t]))]
			}
		}

		# stack subjects and visits: number of visits as rows, and number of basis as columns
		phi_t_stacked = rbind(phi_t_stacked, t(phi_t[[i]]))
	}

	results_basis = list(knot_place=knots, time_cont=time_cont, orth_spline_basis_sparse=phi_t, 
						 orth_spline_basis_sparse_stacked=phi_t_stacked, orth_spline_basis_cont=phi_t_cont)
	return(results_basis)
}

###perform post hoc rotation
post_hoc_rotation <- function(sfpca_model, prepared_data, Nchains, Nsamples){
  sa <- sfpca_model$sa
  Sigma = rstan::extract(sa,"Sigma",permuted=FALSE)
  W = rstan::extract(sa,"W",permuted=FALSE)
  sigma_eps = rstan::extract(sa,"sigma_eps",permuted=FALSE)
  theta_mu = rstan::extract(sa,"theta_mu",permuted=FALSE)
  alpha = rstan::extract(sa,"alpha",permuted=FALSE)
  Theta = rstan::extract(sa,"Theta",permuted=FALSE)
  
  ## Reshape parameters and reorient loadings with PCA rotation 
  N = prepared_data$num_subjects
  K = sfpca_model$pc
  Q = sfpca_model$knot + 4
  
  theta_mu_new = array(0, dim=c(Q, Nchains*Nsamples/2))
  alpha_old = alpha_new = array(0, dim=c(K, N, Nchains*Nsamples/2)) 
  Theta_old = Theta_new = array(0, dim=c(Q, K, Nchains*Nsamples/2))
  W_old = array(0, dim=c(Q, Q, Nchains*Nsamples/2)) 
  
  ind = 0
  prop_var = NULL
  for(i in 1:dim(W)[1]){
   for(j in 1:dim(W)[2]){
      #print(ind)
      ind = ind + 1
      theta_mu_new[,ind] = array(theta_mu[i,j,])
      alpha_old[,,ind] = t(array(alpha[i,j,],dim=c(N, K)))
      Theta_old[,,ind] = array(Theta[i,j,],dim=c(Q, K))
      W_old[,,ind] = array(W[i,j,],dim=c(Q,Q)) 
      
      eigen_temp_sigma=eigen(W_old[,,ind])
      v_temp=eigen_temp_sigma$vectors
      d_temp=eigen_temp_sigma$values 
      prop_var = rbind(prop_var, d_temp/sum(d_temp)) # proportion of variance explained by each PC
      
      for(com in 1:length(d_temp)){
        if(!(d_temp[com]-Re(d_temp[com])==0)){
          d_temp[com]=-1*10^5
        }
      }
      pos_temp=array(0,dim=c(K,1))
      for(pos in 1:K){
        pos_temp[pos]=(1:length(d_temp))[max(d_temp)==d_temp]
        d_temp[pos_temp[pos]]=-1e+5
      }
      
      Theta_new[,,ind]=v_temp[,pos_temp]
      for(k in 1:K){
        Theta_new[, k, ind]=sign(Theta_new[1,k,ind]) * Theta_new[,k,ind]
      }
      
      alpha_new[,, ind] = t(Theta_new[,,ind]) %*% Theta_old[,,ind] %*% alpha_old[,,ind]
    }
  }
  prop_var_avg_origin = colMeans(prop_var)
  (prop_var_avg = paste(round(colMeans(prop_var)*100, 2), '%', sep=''))
  #rename Q
  li <- list(num_subjects = N, npcs = K, nknots = sfpca_model$knot, Q = Q, alpha_new = alpha_new, 
            theta_mu_new = theta_mu_new, Theta_new = Theta_new, prop_var_avg_origin = prop_var_avg_origin, 
            prop_var_avg = prop_var_avg)
  return(li)
}


#vars_select: selected variable names will be in the output dataframe result
output_results <- function(prepared_data, vars_select, results_rotation, results_basis){
  npcs <- results_rotation$npcs
  ALPHA_array = results_rotation$alpha_new
  MU_array = results_rotation$theta_mu_new
  THETA_array = results_rotation$Theta_new
  phi_t_cont = results_basis$orth_spline_basis_cont
  phi_t = results_basis$orth_spline_basis_sparse
  time_cont = results_basis$time_cont
  N = prepared_data$num_subjects
  
  nloop=dim(ALPHA_array)[3]
  first=1
  last=nloop
  
  MU_mean = MU_array[, first] #mean function across sampling sessions
  ALPHA_mean = ALPHA_array[,,first] # mean factor scores
  THETA_mean = THETA_array[,,first] # mean factor loading
  
  for(iter in 2:nloop){
    MU_mean = MU_mean + MU_array[, iter]
    ALPHA_mean = ALPHA_mean + ALPHA_array[,,iter]
    THETA_mean = THETA_mean + THETA_array[,,iter]
  }
  
  MU_mean=cbind(MU_mean/(last-first+1))
  ALPHA_mean=cbind(ALPHA_mean/(last-first+1))
  THETA_mean=cbind(THETA_mean/(last-first+1))
  
  Mu_functions = t(bdiag(cbind(phi_t_cont)))%*%MU_mean
  FPC_mean=t(phi_t_cont)%*%THETA_mean
  
  vars_complete <- c('ID', 'time', 'response', vars_select)
  tryCatch({
    df = prepared_data$data[, vars_complete]
  }, error=function(e){cat("ERROR :",'Selected variables not in data', "\n")})
  Y_sparse = list()
  time_sparse = list()
  scores = data.frame(t(ALPHA_mean)) 
  if(npcs == 1){
    ### create data frame containing needed information ####
    df$fpc1 = 0 # principle component scores
    
    i = 0
    for (pid in unique(df$ID)){
      i = i + 1
      Y_sparse[[i]] = df$response[df$ID == pid]
      time_sparse[[i]] = df$time[df$ID == pid]
      df$fpc1[df$ID == pid] = scores[i]
    }
    df$fpc1 = as.numeric(df$fpc1) # data type issue 
    
    Fits_sparse=list()
    for(i in 1:N){
      Fits_sparse[[i]] = t(phi_t[[i]]) %*% MU_mean + t(phi_t[[i]]) %*% THETA_mean %*% ALPHA_mean[i]
    }
    
    df$Y_sparse = unlist(Y_sparse) 
    df$Fits_sparse = unlist(Fits_sparse)
    df$residuals = df$Y_sparse - df$Fits_sparse
  } else {
    ### create data frame containing needed information ####
    for(k in 1:npcs){
      names(scores)[k] = paste('fpc', k, sep = '')
      df[names(scores)[k]] = 0 # principle component scores  # it depends of PCs (better to choose number of PCs as input)
    }
    
    i = 0
    for (pid in unique(df$ID)){
      i = i + 1
      Y_sparse[[i]] = df$response[df$ID == pid]
      time_sparse[[i]] = df$time[df$ID == pid]
      for(k in 1:npcs){
        df[,names(scores)[k]][df$ID == pid] = scores[i, k]
      }
    }
    
    Fits_sparse=list()
    for(i in 1:N){
      Fits_sparse[[i]] = t(phi_t[[i]]) %*% MU_mean + t(phi_t[[i]]) %*% THETA_mean %*% ALPHA_mean[, i]
    }
    
    df$Y_sparse = unlist(Y_sparse) # check: sum(df$Y_sparse != df$response) == 0
    df$Fits_sparse = unlist(Fits_sparse)
    df$residuals = df$Y_sparse - df$Fits_sparse
  }
  
  return_list = list(df = df, Mu_functions = Mu_functions, time_sparse = time_sparse,
                       Y_sparse = Y_sparse, FPC_mean = FPC_mean)
  return(return_list)
}

plot_qqplot <- function(response, path=NULL){
  if(!is.null(path)) pdf(path)
  par(mfrow=c(2,2))
  car::qqPlot(response)
  car::qqPlot(log(response))
  car::qqPlot(sqrt(response))
  car::qqPlot(response)
  if(!is.null(path)) dev.off()
}

plot_group <- function(data, time_name, response_name, id_name, group_name, path=NULL){
  if(!is.null(path)) pdf(path)
  time <- as.numeric(as.character(data[, time_name]))
  response <- data[, response_name]
  unique_id <- data[, id_name]
  group <- data[, group_name]
  for(i in 1:length(group_name)){
    print(ggplot2::ggplot(data, aes(x=time, y=response, group=unique_id, color=group)) + 
      geom_line(alpha=0.2) + 
      geom_smooth(se=F, size=2, aes(group=group, fill=group), level=0.95) +
      xlab(time_name) + ylab(response_name) + ggtitle(group_name) + 
      theme(axis.title.x = element_text(face="bold"),
            axis.title.y = element_text(face="bold"),
            legend.position="top"))
  }
  if(!is.null(path)) dev.off()
}

plot_k_diagnostic <- function(sfpca_model, prepared_data, Nsamples, Nchains, path=NULL){
  if(!is.null(path)) pdf(path)
  N = prepared_data$num_subjects
  loo_best <- sfpca_model$looic
  pkdf<-data.frame(pk=loo_best$diagnostics$pareto_k, id=1:N)
  print(ggplot2::ggplot(pkdf, aes(x=id,y=pk)) + geom_point(shape=3, color="blue") +
    labs(x="Observation left out", y="Pareto shape k") +
    geom_hline(yintercept = 0.7, linetype=2, color="red", size=0.2) +
    ggtitle("PSIS-LOO diagnostics") + theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"),
          axis.text.x= element_text(size=10, face="bold"),
          axis.text.y= element_text(size=10, face="bold"),
          axis.title.x= element_text(size=12, face="bold"),
          axis.title.y= element_text(size=12, face="bold")) )
  
  sa <- sfpca_model$sa
  Ynew <- rstan::extract(sa, "Ynew", permuted=FALSE)
  V <- prepared_data$visits.vector
  Ynew_transform = matrix(rep(0, Nsamples/2 * Nchains * sum(V)), ncol=sum(V))
  ind = 0
  for (i in 1:(Nsamples/2)){
    for (j in 1:Nchains){
      ind = ind + 1
      Ynew_transform[ind, ] = Ynew[i,j,]
    }
  }
  Ynew_mean = colMeans(Ynew_transform)
  bayesplot::color_scheme_set("brightblue")
  k <- sfpca_model$pc
  d <- sfpca_model$knot
  print(bayesplot::ppc_dens_overlay(prepared_data$data$response, Ynew_transform) + ggplot2::ggtitle(paste(k,'pc_',d,'knot')))
  if(!is.null(path)) dev.off()
}

plot_residual_analysis <- function(results_list, path=NULL){
  if(!is.null(path)) pdf(path)
  df <- results_list$df
  par(mfrow=c(1,2))
  plot(df$residuals)
  car::qqPlot(df$residuals)
  if(!is.null(path)) dev.off()
  par(mfrow=c(1,1))
}


plot_mean_curve <- function(results_basis, results_list, prepared_data, id_name, time_name,
                            response_name, vars_select, ymin=NULL, ymax=NULL, path=NULL){
  if(!is.null(path)) pdf(path)
  #xpar(mfrow=c(1,2))
  data <- prepared_data$data
  N = prepared_data$num_subjects
  data = data[, c(id_name, time_name, response_name, vars_select)]
  colnames(data) = c('ID_unique', 'Time', 'response', vars_select)
  sigma_y = sd(log(data$response))
  mu_y = mean(log(data$response))
  time_cont <- results_basis$time_cont
  Y_sparse <- results_list$Y_sparse
  Mu_functions <- results_list$Mu_functions
  time_sparse <- results_list$time_sparse
  
  if(is.null(ymin) & is.null(ymax)){
    ymin <- floor(min(unlist(Y_sparse)*sigma_y+ mu_y, min(Mu_functions*sigma_y + mu_y))) - 0.1
    ymax <- ceiling(max(unlist(Y_sparse)*sigma_y+ mu_y, max(Mu_functions*sigma_y + mu_y))) + 0.1
  }
  plot(time_cont*(max(data$Time) - min(data$Time)) + min(data$Time), Mu_functions*sigma_y + mu_y, type="l",ylim=c(ymin, ymax),
     xlab='Days of life', ylab='shannon diversity', lwd=5, col=4, font.lab=2, cex.lab=1.2)
  for(i in 1:N){
    lines(time_sparse[[i]]*(max(data$Time) - min(data$Time)) + min(data$Time),Y_sparse[[i]]*sigma_y + mu_y,type="l",lwd=.25)
  }
  if(!is.null(path)) dev.off()
}

#get variable names that not change with differnet timepoint
get_invariants <- function(data, var_names, id_name, time_name, var_exclude=NULL){
  if(!is.null(var_exclude)) data <- data[, -var_exclude]
  
  var_invariant <- c()
  for(i in 1:length(var_names)){
    var_temp <- var_names[i]
    
    #skip variables with only 1 value
    if(length(unique(data[,var_temp])) == 1) next
    
    #get wide format data
    data_temp <- data[,c(id_name, time_name, var_temp)]
    data_wide <- tidyr::spread(data_temp, time_name, var_temp)
    #count number of subject with same value
    count <- 0
    for(k in 1:nrow(data_wide)){
      vec_temp <- na.omit(unlist(data_wide[k, ])[-1])
      count <- ifelse(length(unique(vec_temp)) == 1, count+1, count)
    }
    if(count == nrow(data_wide)) var_invariant <- c(var_invariant, var_temp)
  } 
  #change variables to factor type
  data[, var_invariant] <- lapply(data[, var_invariant], as.factor)
  data_invar <- data[, var_invariant]
  if(id_name %in% colnames(data_invar)) {
    data_invar <- data_invar[, !colnames(data_invar) %in% id_name]
  }
  return(colnames(data_invar))
}

sfpcaClass <- function(pc=NULL, knot=NULL, sa=NULL, log_lik=NULL, looic=NULL){
  sfpca_stan <- list(
    pc = pc,
    knot = knot,
    sa = sa,
    log_lik = log_lik,
    looic = looic
  )
  
  ## Set the name for the class
  class(sfpca_stan) <- append(class(sfpca_stan),"sfpcaClass")
  return(sfpca_stan)
}

sfpca_stan <- function(PC_max, D_max, Nsamples, Nchain, smod, data){
  sfpca_results <- list()
  i = 0
  for (k in 1:PC_max){
    for (d in 1:D_max){
      i = i + 1
      sfpca <- sfpcaClass()
      sfpca$pc <- k
      sfpca$knot <- d
      print(paste('index i is:', i, 'number of PC:', k, 'number of knots:', d))
      results_basis = basis_setup_sparse(prepared_data=data, nknots=d, orth=TRUE)
      pca_data <- list(N = data$num_subjects, K = k, Q = d + 4, Y = data$response.list,
                       V = data$visits.vector, subject_starts = data$visits.start,
                       subject_stops = data$visits.stop, cov_starts = data$cov.start,
                       cov_stops = data$cov.stop, cov_size = data$cov.size,
                       B = results_basis$orth_spline_basis_sparse_stacked)
      
      set.seed(31)
      sa = rstan::sampling(smod, data= pca_data, iter=Nsamples, chains=Nchains, init="random")
      sfpca$sa = sa
      sfpca$log_lik <- rstan::extract(sa,"log_lik_marg")[[1]]
      sfpca$looic = loo::loo(sfpca$log_lik)
      sfpca_results[[i]] <- sfpca
      print("######### SFPCA on shannon Gut ###############")
    }
  }
  return(sfpca_results)
}

#compare sfpca models and return the best model
get_optimal_model <- function(sfpca_results){
  len <- length(sfpca_results)
  looic.list <- lapply(1:len, function(i){
    sfpca_results[[i]]$looic
  })
  looic.obj <- loo::loo_compare(looic.list)
  print(looic.obj)
  model.name <- rownames(looic.obj)[1]
  model.index <- as.numeric(gsub(".*?([0-9]+).*", "\\1", model.name))
  return(sfpca_results[[model.index]])
}  


sfpca_rda <- function(results_list, sfpca_model, vars_select, id.name, path=NULL){
  df <- results_list$df
  df <- df[complete.cases(df), ]
  
  if(id.name %in% vars_select){
    vars_select <- vars_select[!vars_select %in% id.name]
  }
  dat <- df[c(vars_select)]

  pc.names <- numeric(sfpca_model$pc)
  pc.names <- sapply(1:sfpca_model$pc, function(i){
    paste('fpc', i, sep = '')
  })
  pc = df[, pc.names]
  
  mod0 <- vegan::rda(pc ~ 1., dat)  # Model with intercept only
  mod1 <- vegan::rda(pc ~ ., dat)  # Model with all explanatory variables
  set.seed(111)
  step.res <- vegan::ordiR2step(mod0, mod1, perm.max = 1000)
  
  #add effect-size
  table = step.res$anova
  if(is.null(table)) return(print('no non-redundant variable'))
  table.row <- nrow(table)
  R2.adj <- c(table$R2.adj[1])
  for (i in 1:(table.row-1)){
    R2.adj <- c(R2.adj, table$R2.adj[i+1]-table$R2.adj[i])
  }
  table$ES.RDA <- R2.adj
  table = table[-table.row, ]
  print(step.res$call)
  covariates <- rownames(table)
  print(ggplot2::ggplot(table, aes(x=reorder(covariates, ES.RDA), y=ES.RDA, fill=covariates)) +
    labs(x = 'Non-redundant Covariants', y = 'Effect Size') +
    geom_bar(stat='identity') +
    theme(axis.text=element_text(size=10), 
          axis.title=element_text(size=14,face="bold"), 
          legend.position="none") + 
    coord_flip())
  if(!is.null(path)) ggsave(path)
  return(table)
}

