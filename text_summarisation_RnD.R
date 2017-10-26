compDF <- read.csv("D:/Data Science/Automobile complaint analysis/input/auto_complaint.csv",
                   stringsAsFactors = F)

library(stringi)
library(NLP)
library(tm)
library(mopsocd)

mopsocd_revised <- function (fn, gn = NULL, varcnt, fncnt, lowerbound, upperbound, 
                             opt, popsize = 100, maxgen = 100, archivesize = 250, verbosity = 0, 
                             pMut = 0.5, w = 0.4, c1 = 1, c2 = 1) 
{
  varcnt <- as.integer(varcnt)
  fncnt <- as.integer(fncnt)
  #lowerbound <- as.integer(lowerbound)
  #upperbound <- as.integer(upperbound)
  opt <- as.integer(opt)
  popsize <- as.integer(popsize)
  maxgen <- as.integer(maxgen)
  archivesize <- as.integer(archivesize)
  verbosity <- as.integer(verbosity)
  pMut <- as.numeric(pMut)
  # w <- as.numeric(w)
  # c1 <- as.numeric(c1)
  # c2 <- as.numeric(c2)
  minvalue <- vector(length = varcnt)
  maxvalue <- vector(length = varcnt)
  archiveVar <- matrix(nrow = archivesize, ncol = varcnt)
  archiveFit <- matrix(nrow = archivesize, ncol = fncnt)
  archiveVel <- matrix(nrow = archivesize, ncol = varcnt)
  popVar <- matrix(nrow = popsize, ncol = varcnt)
  popFit <- matrix(nrow = popsize, ncol = fncnt)
  pbestsVar <- matrix(nrow = popsize, ncol = varcnt)
  vbestsVar <- matrix(nrow = popsize, ncol = varcnt)
  pbestsFit <- matrix(nrow = popsize, ncol = fncnt)
  velocity <- matrix(nrow = popsize, ncol = varcnt)
  # minvaluetemp <- matrix(nrow = popsize, ncol = varcnt)
  # maxvaluetemp <- matrix(nrow = popsize, ncol = varcnt)
  ndomCtr <- 0
  # maxvalue <- upperbound
  # minvalue <- lowerbound
  initialize_pop <- function() {
    for (i in 1:popsize) {
      
      X <- c()
      while(length(X) < varcnt)
      {
        if(varcnt - length(X) < k_min & sum(X) < k_min)
        {
          X <- c(X,1)
        } else if(sum(X) < k)
        {
          X <- c(X,rbinom(1,1,prob=0.5))
        } else X <- c(X,0)
      }
      if (i %% 2 == 0)
      {
        popVar[i, 1:varcnt] <<- X[length(X):1]
      } else popVar[i, 1:varcnt] <<- X
    }
    
  }
  initialize_vel <- function() {
    temp_rand1 <- sample(c(1:varcnt-1),1)
    temp_rand2 <- temp_rand1
    while(temp_rand1 == temp_rand2)
    {
      temp_rand2 <- sample(c(1:varcnt-1),1)
    }
    
    if (temp_rand1 > temp_rand2)
    {
      alpha_v_low <- temp_rand2
      alpha_v_high <- temp_rand1
    } else {
      alpha_v_low <- temp_rand1
      alpha_v_high <- temp_rand2
    }
    
    for (i in 1:popsize) {
      
      alpha_v_t <- sample(c(alpha_v_low:alpha_v_high),1)
      V <- c()
      while(length(V) < varcnt)
      {
        if(sum(V) < alpha_v_t)
        {
          if(varcnt - length(V) < alpha_v_t)
          {
            V <- c(V,1)
          } else {
            V <- c(V,rbinom(1,1,prob=0.5))
          }
        } else V <- c(V,0)
      }
      if (i %% 2 == 0)
      {
        velocity[i, 1:varcnt] <<- V[length(V):1]
      } else velocity[i, 1:varcnt] <<- V
    }
      #velocity[1:popsize, 1:varcnt] <<- 0
  }
  evaluate <- function() {
    popFit[1:popsize, ] <<- t(apply(popVar, 1, fn))
  }
  store_pbests <- function() {
    pbestsVar[1:popsize, ] <<- popVar[1:popsize, ]
    pbestsFit[1:popsize, ] <<- popFit[1:popsize, ]
    vbestsVar[1:popsize, ] <<- velocity[1:popsize, ]
  }
  insert_nondom <- function() {
    if (!is.null(gn)) {
      z <- rowSums(t(matrix(!apply(popVar, 1, gn), ncol = popsize) *
                       1))
      minz <- min(z)
      ixfpop <- which(z == minz)
      # ix <- which(z == minz)
      # lenix <- length(ix)
      # ixfpop <- NULL
      # for (i in ix) {
      #   if (opt == 1) {
      #     x <- (t(matrix(rep(popFit[i, ], lenix), nrow = fncnt)) -
      #             popFit[ix, ] < 0) * 1
      #   }
      #   else {
      #     x <- (t(matrix(rep(popFit[i, ], lenix), nrow = fncnt)) -
      #             popFit[ix, ] > 0) * 1
      #   }
      #   if ((max(rowSums(x)) == fncnt) == FALSE) {
      #     ixfpop <- c(ixfpop, i)
      #   }
      # }
    }
    else {
      ixfpop = seq(popsize)
      # ix <- seq(popsize)
      # lenix <- length(ix)
      # ixfpop <- NULL
      # for (i in ix) {
      #   if (opt == 1) {
      #     x <- (t(matrix(rep(popFit[i, ], lenix), nrow = fncnt)) -
      #             popFit[ix, ] < 0) * 1
      #   }
      #   else {
      #     x <- (t(matrix(rep(popFit[i, ], lenix), nrow = fncnt)) -
      #             popFit[ix, ] > 0) * 1
      #   }
      #   if ((max(rowSums(x)) == fncnt) == FALSE) {
      #     ixfpop <- c(ixfpop, i)
      #   }
      # }
    }
    lenfpop <- length(ixfpop)
    if (lenfpop != 0) {
      if (ndomCtr + lenfpop <= archivesize) {
        archiveVar[(ndomCtr + 1):(ndomCtr + lenfpop), 
                   ] <<- popVar[ixfpop, ]
        archiveFit[(ndomCtr + 1):(ndomCtr + lenfpop), 
                   ] <<- popFit[ixfpop, ]
        archiveVel[(ndomCtr + 1):(ndomCtr + lenfpop), 
                    ] <<- velocity[ixfpop, ]
        ndomCtr <<- ndomCtr + lenfpop
      }
      else {
        cat("FATAL ERROR:\nArchive Size Value ", archivesize, 
            " too small\n")
        cat("Recommended Archive Size: 2.5*population size\n")
        cat("Press Enter to exit\n")
        scan(quiet = TRUE)
        cat("Goodbye !\n")
        stop()
      }
    }
    else {
      cat("No solution (Unreasonable constraints)\n")
      cat("Press Enter to exit\n")
      scan(quiet = TRUE)
      cat("Goodbye !\n")
      stop()
    }
  }
  crowding <- function() {
    
    sorted_archive <- sort(rowSums(archiveFit[1:ndomCtr, ]), 
                                                   index.return = TRUE)
    
    ## implementation of roulette selection function
    weight_sum <- sum(sorted_archive$x)
    prob <- c()
    for( i in sorted_archive$ix)
    {
      prob <- c(prob,(sum(sorted_archive$x[1:i]/weight_sum)))
    }
    sorted_cd <- c()
    for (n in 1:ndomCtr)
    {
      for(s in 1:2)
      {
        r <- sample(c(0:10),1)/10 # runif(1)
        for(j in sorted_archive$ix)
        {
          if( r <= prob[j])
          {
            sorted_cd <- c(sorted_cd,sorted_archive$ix[j])
            break()
          }
        }
      }
    }
    archiveVar[1:length(sorted_cd), ] <<- archiveVar[sorted_cd, 
                                                        ]
    archiveFit[1:length(sorted_cd), ] <<- archiveFit[sorted_cd, 
                                                        ]
    archiveVel[1:length(sorted_cd), ] <<- archiveVel[sorted_cd, 
                                                             ]
  }
  update_pbests <- function() {
    if (opt == 1) {
      x <- rowSums((popFit - pbestsFit > 0) * 1)
    }
    else {
      x <- rowSums((popFit - pbestsFit < 0) * 1)
    }
    ix1 <- which(x != 0 & x != fncnt)
    ix2 <- sample(ix1, floor(length(ix1)/2), replace = FALSE)
    ix <- union(which(x == fncnt), ix2)
    pbestsVar[ix, ] <<- popVar[ix, ]
    pbestsFit[ix, ] <<- popFit[ix, ]
    vbestsVar[ix, ] <<- velocity[ix, ]
  }
  update_archive <- function() {
    if (!is.null(gn)) {
      z <- rowSums(t(matrix(!apply(popVar, 1, gn), ncol = popsize) * 
                       1))
      minz <- min(z)
      ix <- which(z == minz)
      lenix <- length(ix)
      ixfpop <- NULL
      for (i in ix) {
        if (opt == 1) {
          x <- (t(matrix(rep(popFit[i, ], lenix), nrow = fncnt)) - 
                  popFit[ix, ] < 0) * 1
        }
        else {
          x <- (t(matrix(rep(popFit[i, ], lenix), nrow = fncnt)) - 
                  popFit[ix, ] > 0) * 1
        }
        if ((max(rowSums(x)) == fncnt) == FALSE) {
          ixfpop <- c(ixfpop, i)
        }
      }
    }
    else {
      ixfpop = seq(popsize)
    }
    lenfpop <- length(ixfpop)
    ixinspop <- NULL
    for (i in ixfpop) {
      if (opt == 1) {
        x <- (t(matrix(rep(popFit[i, ], ndomCtr), nrow = fncnt)) - 
                archiveFit[1:ndomCtr, ] < 0) * 1
      }
      else {
        x <- (t(matrix(rep(popFit[i, ], ndomCtr), nrow = fncnt)) - 
                archiveFit[1:ndomCtr, ] > 0) * 1
      }
      if ((max(rowSums(x)) == fncnt) == FALSE) {
        ixinspop <- c(ixinspop, i)
      }
    }
    leninspop <- length(ixinspop)
    if (leninspop != 0) {
      if (ndomCtr + leninspop <= archivesize) {
        archiveVar[(ndomCtr + 1):(ndomCtr + leninspop), 
                   ] <<- popVar[ixinspop, ]
        archiveFit[(ndomCtr + 1):(ndomCtr + leninspop), 
                   ] <<- popFit[ixinspop, ]
        ndomCtr <<- ndomCtr + leninspop
      }
      else {
        crowding()
        bottom <- floor(ndomCtr * 0.9)
        cnt <- min((archivesize - bottom), leninspop)
        ixinssel <- sample(ixinspop, cnt, replace = F)
        ixreplace <- sample((archivesize - cnt):archivesize, 
                            cnt, replace = F)
        archiveVar[(archivesize - cnt + 1):archivesize, 
                   ] <<- popVar[ixinssel, ]
        archiveFit[(archivesize - cnt + 1):archivesize, 
                   ] <<- popFit[ixinssel, ]
        ndomCtr <<- archivesize
      }
    }
    else {
      return(0)
    }
    ixarc <- seq(ndomCtr)
    ixndomarc <- NULL
    rmove <- NULL
    dup <- NULL
    for (i in ixarc) {
      if (i %in% dup) {
        if (verbosity >= 3) 
          cat("Duplicate Particle Found ", i, "\n")
        next
      }
      if (opt == 1) {
        x <- (t(matrix(rep(archiveFit[i, ], ndomCtr), 
                       nrow = fncnt)) - archiveFit[ixarc, ] < 0) * 
          1
      }
      else {
        x <- (t(matrix(rep(archiveFit[i, ], ndomCtr), 
                       nrow = fncnt)) - archiveFit[ixarc, ] > 0) * 
          1
      }
      rmove <- c(setdiff(which(rowSums(x) == 0), i), rmove)
      wich1 <- setdiff(which(rowSums(x) == 0), i)
      if (length(wich1) != 0) {
        if (!(i %in% wich1)) {
          for (j in wich1) {
            if (all(archiveFit[j, ] == archiveFit[i, 
                                                  ])) {
              dup <- c(j, dup)
            }
          }
        }
      }
    }
    rmove <- union(rmove, dup)
    lenrem <- length(rmove)
    if (lenrem != 0) {
      cor = setdiff(seq(ndomCtr), rmove)
      ndomCtr <<- length(cor)
      archiveVar[1:ndomCtr, ] <<- archiveVar[cor, ]
      archiveFit[1:ndomCtr, ] <<- archiveFit[cor, ]
    }
  }
  compute_velocity <- function() {
    top <- max(floor((ndomCtr * 0.1)), 1)
    gBest <- sample(1:top, 1, replace = TRUE)
    gBestmat <- t(matrix(rep(archiveVar[gBest, ], popsize), 
                         nrow = varcnt))
    randum1 <- matrix(round(runif(popsize * varcnt)), popsize, varcnt)
    randum2 <- matrix(round(runif(popsize * varcnt)), popsize, varcnt)
    velocity[1:popsize, ] <<- w * velocity[1:popsize, ] + 
      c1 * randum1 * (pbestsVar - popVar) + c2 * randum2 * 
      (gBestmat - popVar)
    popVar[1:popsize, ] <<- popVar[1:popsize, ] + velocity[1:popsize, 
                                                           ]
  }
  maintain_particles <- function() {
    for (j in seq(varcnt)) {
      wich1 <- which(popVar[, j] < minvalue[j])
      wich2 <- which(popVar[, j] > maxvalue[j])
      popVar[wich1, j] <<- minvalue[j]
      popVar[wich2, j] <<- maxvalue[j]
      wich12 <- union(wich1, wich2)
      velocity[wich12, j] <<- -1 * velocity[wich12, j]
    }
  }
  mutate <- function() {
    if (t >= maxgen * pMut) 
      return(0)
    rangev <- vector(length = varcnt)
    flip <- ifelse(runif(1) < (1 - (t/(maxgen * pMut)))^1.5, 
                   1, 0)
    if (flip) {
      dim <- sample(1:varcnt, 1, replace = TRUE)
      rangev[1:varcnt] <- (maxvalue[1:varcnt] - minvalue[1:varcnt]) * 
        0.5 * (1 - (t/(maxgen * pMut)))^1.5
      wich1 <- which((popVar[, dim] - rangev[dim]) < minvalue[dim])
      wich2 <- which((popVar[, dim] + rangev[dim]) > maxvalue[dim])
      if (length(wich1) != 0) {
        minvaluetemp[wich1, dim] <<- minvalue[dim]
        minvaluetemp[-wich1, dim] <<- popVar[-wich1, 
                                             dim] - rangev[dim]
      }
      else {
        minvaluetemp[, dim] <<- popVar[, dim] - rangev[dim]
      }
      if (length(wich2) != 0) {
        maxvaluetemp[wich2, dim] <<- maxvalue[dim]
        maxvaluetemp[-wich2, dim] <<- popVar[-wich2, 
                                             dim] + rangev[dim]
      }
      else {
        maxvaluetemp[, dim] <<- popVar[, dim] + rangev[dim]
      }
      popVar[, dim] <<- runif(popsize, min = minvaluetemp[, 
                                                          dim], max = maxvaluetemp[, dim])
    }
  }
  if (verbosity >= 1) 
    cat("Welcome to MOPSO-CD\n")
  t1 <- Sys.time()
  t <- 0
  initialize_pop()
  initialize_vel()
  evaluate()
  store_pbests()
  insert_nondom()
  while (t < maxgen) {
    t <- t + 1
    if (verbosity >= 1) 
      cat("Generation ", t, "\n")
    if (ndomCtr > 2 * fncnt) 
      crowding()
    compute_velocity()
    maintain_particles()
    mutate()
    evaluate()
    update_archive()
    update_pbests()
    if (verbosity >= 3) {
      cat("\nVariables\n")
      print(archiveVar[1:ndomCtr, ])
      cat("Objective Function Values:\n")
      print(archiveFit[1:ndomCtr, ])
      cat("\n")
    }
  }
  if (verbosity >= 2) {
    cat("###############################\n")
    cat("         FINAL VALUES\n")
    cat("###############################\n")
    cat("Variables:\n")
    print(archiveVar[1:ndomCtr, ])
    cat("\nObjective Function Values:\n")
    print(archiveFit[1:ndomCtr, ])
  }
  if (verbosity >= 1) {
    cat("\nNondominated Solutions: ", ndomCtr, "\n")
    cat("Number of Generations: ", t, "\n")
    cat("Computation Time: ", Sys.time() - t1, "\n")
  }
  if (!is.null(gn)) {
    pareto <- list(paramvalues = archiveVar[1:ndomCtr, ], 
                   objfnvalues = archiveFit[1:ndomCtr, ], numsols = ndomCtr, 
                   fn = fn, gn = gn, varcnt = varcnt, fncnt = fncnt, 
                   lowerbound = lowerbound, upperbound = upperbound, 
                   opt = opt, popsize = popsize, maxgen = maxgen, archivesize = archivesize, 
                   pMut = pMut, w = w, c1 = c1, c2 = c2)
  }
  else {
    pareto <- list(paramvalues = archiveVar[1:ndomCtr, ], 
                   objfnvalues = archiveFit[1:ndomCtr, ], numsols = ndomCtr, 
                   fn = fn, gn = NULL, varcnt = varcnt, fncnt = fncnt, 
                   lowerbound = lowerbound, upperbound = upperbound, 
                   opt = opt, popsize = popsize, maxgen = maxgen, archivesize = archivesize, 
                   pMut = pMut, w = w, c1 = c1, c2 = c2)
  }
  warnings()
  pareto$call <- match.call()
  class(pareto) <- "pareto"
  pareto
}

dtm.generate <- function(df,ng,
                         sparse=1,
                         spl_sym = "",
                         my_stop_word_file="",
                         keep.id=FALSE,
                         remove_non_english_char = TRUE,
                         removeNum = TRUE,
                         removePunc = TRUE,
                         removeStpWords = TRUE,
                         doStem = TRUE,
                         doIDF = TRUE,
                         doNormTf = TRUE)
{
  
  if (remove_non_english_char)
  {
    for(c in 1:ncol(df))
    {
      
      df[,colnames(df)[c]] <- gsub("[^\x20-\x7E]", "",
                                   df[,colnames(df)[c]])
    }
  }
  if(keep.id == TRUE)
  {
    # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
    m <- list(id = "ID", content = colnames(df)[2])
    myReader <- readTabular(mapping = m)
    
    corpus <- VCorpus(DataframeSource(df), readerControl = list(reader = myReader))
    
    # Manually keep ID information from http://stackoverflow.com/a/14852502/1036500
    #       for (i in 1:length(corpus)) {
    #         attr(corpus[[i]], "id") <- df$ID[i]
    #         #corpus[[i]]$meta$ID <- df$ID[i]
    #       }
  } else
  {
    corpus <- Corpus(VectorSource(df[,1])) # create corpus for TM processing
  }
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  if (removeStpWords) corpus <- tm_map(corpus, removeWords, 
                                       c(stopwords("SMART"),stopwords("english")))
  
  if (removeNum) corpus <- tm_map(corpus, removeNumbers) 
  if(length(spl_sym) > 0 & sum(nchar(spl_sym)) > 0)
  {
    corpus <- tm_map(corpus, content_transformer(gsub), 
                     pattern = paste(spl_sym,collapse="|"), replacement = " ", fixed=TRUE)
  }
  if (removePunc) corpus <- tm_map(corpus, removePunctuation)
  if (doStem) corpus <- tm_map(corpus, stemDocument)
  if(my_stop_word_file !="")
  {
    content_specific_stop_words <- read.csv(my_stop_word_file,
                                            header=F,stringsAsFactors = F)
    corpus <- tm_map(corpus, removeWords, content_specific_stop_words[,1])
  }
  #corpus <- tm_map(corpus, PlainTextDocument)
  if (ng >1)
  {
    options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
    # this stopped working in new server environment
    #BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng, max = ng)) # create n-grams
    nGramTokenizer <-
      function(x)
        unlist(lapply(ngrams(words(x), ng), paste, collapse = " "), use.names = FALSE)
    if(doIDF)
    {
      dtm <- DocumentTermMatrix(corpus, control = list(tokenize = nGramTokenizer,
                                                       weighting = function(x)                                                          
                                                         weightTfIdf(x, normalize = doNormTf))) # create tdm from n-grams
    } else
    {
      dtm <- DocumentTermMatrix(corpus, control = list(tokenize = nGramTokenizer,
                                                       weighting = weightTf)) # create tdm from n-grams
    }
    
  }
  else
  {
    if(doIDF)
    {
      dtm <- DocumentTermMatrix(corpus,control = list(weighting = function(x)                                                          
        weightTfIdf(x, normalize = doNormTf)))
    } else
    {
      dtm <- DocumentTermMatrix(corpus,control = list(weighting = weightTf))
    }
    
  }
  if(sparse != 1)
  {
    dtms <- removeSparseTerms(dtm, sparse)
  }
  else
  {
    dtms <- dtm
  }
  
  dtms
}

get_simil_matrix <- function(org_mat,dest_mat)
{
  org_col_names <- colnames(org_mat)
  dest_col_names <- colnames(dest_mat)
  col_diff <- setdiff(dest_col_names,org_col_names)
  
  new_mat <- cbind(org_mat,
                 matrix(0, nrow = nrow(org_mat), ncol = length(col_diff)))
  
  colnames(new_mat) <- c(org_col_names,col_diff)
  
  new_mat <- new_mat[,order(colnames(new_mat))]
  
  return(new_mat)
}


############# function definition #################
title = compDF[549,2] #defaut ""
text = compDF[549,4]
split = "\\.|!|\\?|\\\n{2,}"
min = 2
k_min = 2
alpha = 0.2 # k_max_pct
wght_val_fun = c(0.25,0.25,0.25,0.25)
num_gen = 200
num_init_poulation = 200

############# function ##############################

sentences <- unlist(strsplit(text, split = split, fixed = F))
sentences <- gsub("\\\n"," ",sentences)
sentences <- sentences[(stri_count(sentences,regex="\\S+") > min) &
                         (nchar(sentences) >
                            (stri_count(sentences,regex="\\S+") * 2))]

m <- length(sentences)
k <- k_min
if (ceiling(m *  alpha) > k_min)
{
  k <- ceiling(m *  alpha)
}

sentence_matrix <- as.data.frame(sentences,stringsAsFactors = FALSE)

if(m > k)
{
      
    #### creating weights
    
  w_t <- as.matrix(dtm.generate(sentence_matrix,
                                1,1,
                                doIDF = TRUE))
  
  if (sum(rowSums(w_t)== 0)>0)
  {
      sentence_matrix <- as.data.frame(sentence_matrix[which(rowSums(w_t)!=0),],
                                       stringsAsFactors=FALSE)
      colnames(sentence_matrix) <- "sentences"
      m <- m - (sum(rowSums(w_t)== 0)) 
      w_t <- w_t[which(rowSums(w_t)!=0),]
      
  }
  
  
  word_count <- stri_count(sentence_matrix[,"sentences"],regex="\\S+")
    len <- word_count/max(word_count)
    
    posp <- 1/(1:m)
    cpi_posp <- 1/(m - (1:m) +1)
    
    pos <- c()
    
    for(i in 1:length(posp))
    {
      pos <- c(pos,max(posp[i],cpi_posp[i]))
    }
    
    # sentence_matrix <- cbind.data.frame(sentence_matrix,word_count,len,pos,
    #                                     stringsAsFactors = FALSE)
    
    w_s <- rowSums(w_t)/max(rowSums(w_t))
    
    w_s_t <- as.matrix(dtm.generate(sentence_matrix,
                                    1,1,
                                    doIDF = FALSE))
    
    if(title != "")
    {
        w_title_t <- as.matrix(dtm.generate(as.data.frame(title),
                                            1,1,
                                            doIDF = FALSE))
    } else {
      w_title_t <- matrix(1,nrow=1, ncol=ncol(w_s_t)) 
      colnames(w_title_t) <- colnames(w_s_t)
    }
    
    w_s_t_e <- get_simil_matrix(w_s_t,w_title_t)
    w_title_t_e <- get_simil_matrix(w_title_t,w_s_t)
    sim_title <- t(t(w_s_t_e %*% w_title_t_e) %*% 
      diag(1/ sqrt(rowSums(w_s_t_e)*sum(w_title_t_e))))
    
    sentence_matrix <- cbind.data.frame(sentence_matrix,word_count,
                                        len,pos,w_s,sim_title,
                                        stringsAsFactors = FALSE)
    
    overlap <- (w_s_t %*% t(w_s_t)) %*% diag(1/(rowSums(w_s_t^2)))
    diag(overlap) <- 0
    
    #### mopso implementation

        ## Define Objectives
    objective <- function(x) {
      f1 <-  (1/sum(x %*% 
                   as.matrix(sentence_matrix[,3:ncol(sentence_matrix)]) %*% 
                   diag(wght_val_fun)))
      f2 <- x %*% t(x %*% overlap)
      return(c(f1,f2))
    }
    
    ## Define Constraints 
    constraints <- function(x) {
      g1 <- sum(x) <= k
      g2 <- sum(x) >= k_min
      return(c(g1,g2))
    }
    
    mopso_obj_orig <- mopsocd(objective, gn = constraints, varcnt=m, fncnt=2, 
                     lowerbound = rep(0, m), 
                     upperbound = rep(1, m), 
                     opt = 0, popsize = 200, maxgen = 200, archivesize = 500, verbosity = 0, 
                                 pMut = 0.5, w = 0.4, c1 = 1, c2 = 1) 
    
    
    print(mopso_obj_orig$numsols)
    print(mopso_obj_orig$objfnvalues)
    print(mopso_obj_orig$paramvalues)
    plot(mopso_obj_orig$objfnvalues[,1],mopso_obj_orig$objfnvalues[,2])
    
    mopso_obj <- mopsocd_revised(fn=objective, gn = constraints, varcnt=m, fncnt=2, 
                              lowerbound = rep(0, m), 
                              upperbound = rep(1, m), 
                              opt = 0, popsize = 200, maxgen = 200, 
                              archivesize = 500, verbosity = 0, 
                              pMut = 0.5, w = 0.4, c1 = 1, c2 = 1) 
    print(mopso_obj$numsols)
    print(mopso_obj$objfnvalues)
    print(mopso_obj$paramvalues)
    plot(mopso_obj$objfnvalues[,1],mopso_obj$objfnvalues[,2])
    
    summary.sentences_indx <- round(mopso_obj$paramvalues[nrow(mopso_obj$paramvalues),])
    summary.sentences <- sentence_matrix[which(summary.sentences_indx == TRUE),"sentences"]

} else{
  summary.sentences <- sentence_matrix[,"sentences"] 
}

