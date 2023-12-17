###############################################
# R code for running the statistical analyzes #
#               recommended in                #
#   "The design and analysis of small-scale   #
#        syntactic judgment experiments"      #
###############################################

###########################################
# Copy and paste all of this text into R. #
# Then follow the instructions in the     #
# appendix of the paper.                  #
###########################################

#################################################
#            Function for running               #
#  exact test for two binary factors and their  #
#  interaction in a between-groups experiment   #
#################################################

ext.fisher = function(x,N,fn1="Factor1",fn2="Factor2") {
  
  # (c) 2007 James Myers and National Chung Cheng University
  # GNU General Public License (www.gnu.org/licenses/gpl.html)
  # Original algorithm written in Matlab by Shih-Feng Huang.
  # For justification, see:
  # Myers, J., Huang, S.-F., & Tsay, J. (2007). Exact conditional
  #  inference for two-way randomized Bernoulli experiments.
  #  Journal of Statistical Software, 21, Code Snippet 1, 2007-09-02.
  #
  # x = 2x2 matrix, N = max cell size, fn1 & fn2 = factor names,
  #  where fn1 describes the columns and fn2 the rows of the data matrix
  # Output is matrix of one-tailed & two-tailed p values for two factors
  #  and their interaction
  
  pval = function(N,aplus,amin,bplus,bmin,cplus,cmin,obsn11) {
    # N = max cell size, obsn11 = size of the observed upper left cell,
    # A = factor being tested,
    # B & C = factors being conditioned on
    # p1pl = NA; p1mn = NA; p2 = NA # Set default p values (original algorithm)
    p1pl = 1; p1mn = 1; p2 = 1 # Set default p values (alternative algorithm)
    # define lower and upper bounds for sum
    L = max(c(0, bplus-N, cplus-bmin, cplus-N))
    U = min(c(N, bplus, cplus, N-bmin+cplus))
    if (L != U) { # if equal, p values cannot be computed
      combo = numeric(N); Aplus = numeric(N)
      factorialN = factorial(N) # saves redundancy below
      # sum all possible tables given conditions
      for (i in L:U) {
        combo[i-L+1] =
          factorialN/factorial(i)/factorial(N-i)*
          factorialN/factorial(bplus-i)/factorial(N-bplus+i)*
          factorialN/factorial(cplus-i)/factorial(N-cplus+i)*
          factorialN/factorial(bmin-cplus+i)/factorial(N-bmin+cplus-i)
        Aplus[i-L+1] = 2*i+bmin-cplus # A^+
      }
      # numerator sums subset of tables equal to or better than observed
      p1pl = sum(combo[(obsn11-L+1):(U-L+1)])/sum(combo)
      p1mn = sum(combo[1:(obsn11-L+1)])/sum(combo)
      # Estimated subdistribution
      E_Aplus = sum(Aplus*combo)/sum(combo) # E(A^+)
      p2=0
      ObsAplus = 2*obsn11+bmin-cplus # Observed A^+
      for (i in L:U) {
        if (abs(Aplus[i-L+1]-E_Aplus) >= abs(ObsAplus-E_Aplus)) {
          p2=p2+combo[i-L+1]/sum(combo)
        }
      }
    }
    return(c(p1pl,p1mn,p2))
  }
  # Pass data to the pval function
  Xplus = sum(x[,1]); Xmin = sum(x[,2]); Yplus = sum(x[1,]); Ymin = sum(x[2,])
  XYplus = sum(x[1,1],x[2,2]); XYmin = sum(x[1,2],x[2,1])
  n11 = x[1,1]
  Xp = pval(N,Xplus,Xmin,Yplus,Ymin,XYplus,XYmin,n11)
  Yp = pval(N,Yplus,Ymin,Xplus,Xmin,XYplus,XYmin,n11)
  XYp = pval(N,XYplus,XYmin,Xplus,Xmin,Yplus,Ymin,n11)
  # Format output
  p.values = round(rbind(Xp,Yp,XYp),4)
  dimnames(p.values) = list(c(fn1, fn2, paste(fn1, ":", fn2, sep="")),
                            c("Plus>Min","Plus<Min","Plus=/=Min"))
  return(p.values)
}

####################################################
# Function for running extended exact McNemar test #
#        for two binary factors and their          #
#    interaction in a within-groups experiment     #
####################################################

ext.mcnemar = function(x,N,fn1="Factor1",fn2="Factor2") {
  
  # x = Nx2x2 array, N = number of groups, fn1 & fn2 = factor names,
  #  where fn1 describes the columns and fn2 the rows of the data matrix
  #  for each group (e.g. speaker)
  # Output is matrix of two-tailed p values for two factors
  #  and their interaction
  
  pat = numeric(3)
  ex = numeric(3)
  ans = matrix(nrow=1,ncol=3)
  
  for (i in 1:N) {
    a = x[N,1,1]
    b = x[N,2,1]
    c = x[N,1,2]
    d = x[N,2,2]
    
    if (a+b > c+d) {
      pat[1] = pat[1] + 1
    }
    if (a+b < c+d) {
      ex[1] = ex[1] + 1
    }
    if (a+c > b+d) {
      pat[2] = pat[2] + 1
    }
    if (a+c < b+d) {
      ex[2] = ex[2] + 1
    }
    if (a+d > b+c) {
      pat[3] = pat[3] + 1
    }
    if (a+d < b+c) {
      ex[3] = ex[3] + 1
    }
  }
  
  for (i in 1:3) {
    ans[i] = min(1,2*pbinom(min(pat[i],ex[i]),sum(pat[i],ex[i]),0.5))
  }
  
  # Format output
  p.values = round(ans,4)
  dimnames(p.values) = list("p=",c(fn1, fn2, paste(fn1, ":", fn2, sep="")))
  return(p.values)
}

##############################################
#      Function for running exact tests      #
# small-scale experiments of different types #
#       (one- or two binary factors,         #
#     within-groups or between-groups)       #
##############################################

small.exp = function(dep,fact1,fact2=NULL,group=NULL) {
  # dep: dependent binary variable (e.g. Judgment)
  # fact1, fact2: factors (only fact1 is required)
  # group: grouping variable (e.g. Speaker); if none, a between-groups test is done
  
  if (is.null(fact2)) {
    # One-factor experiment
    if (is.null(group)) {
      # Between-groups test: Fisher's test
      a = sum(dep[fact1==1])
      b = length(dep[fact1==1]) - a
      c = sum(dep[fact1==-1])
      d = length(dep[fact1==-1]) - c
      ans = fisher.test(cbind(c(a,b),c(c,d)))$p.value
    } else {
      # Within-groups test: exact McNemar test
      pat = 0
      ex = 0
      subj = levels(as.factor(group))
      for (i in subj) {
        if (dep[fact1==1 & group==i] > dep[fact1==-1 & group==i]) {
          pat = pat + 1
        }
        if (dep[fact1==1 & group==i] < dep[fact1==-1 & group==i]) {
          ex = ex + 1
        }
      }
      ans = min(1,2*pbinom(min(pat,ex),sum(pat,ex),0.5))
    }
  } else {
    # Two-factor experiment
    if (is.null(group)) {
      # Between-groups test: extended Fisher's test
      aset = dep[fact1==1 & fact2==1]
      bset = dep[fact1==1 & fact2==-1]
      cset = dep[fact1==-1 & fact2==1]
      dset = dep[fact1==-1 & fact2==-1]
      N = min(c(length(aset),length(bset),length(cset),length(dset)))
      a = sum(aset[1:N])
      b = sum(bset[1:N])
      c = sum(cset[1:N])
      d = sum(dset[1:N])
      ans = ext.fisher(cbind(c(a,b),c(c,d)),N)[,3]
    } else {
      # Within-groups test: extended exact McNemar test 
      subj = levels(as.factor(group))
      N = length(subj)
      x = array(dim=c(N,2,2))
      for (i in 1:N) {
        x[i,1,1] = dep[fact1==1 & fact2==1 & group==subj[i]]
        x[i,2,1] = dep[fact1==1 & fact2==-1 & group==subj[i]]
        x[i,1,2] = dep[fact1==-1 & fact2==1 & group==subj[i]]
        x[i,2,2] = dep[fact1==-1 & fact2==-1 & group==subj[i]]
      }
      ans = ext.mcnemar(x,N)
    }
  }
  return(ans)
}