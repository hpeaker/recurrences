
bfd_frob <- function(...) {
  l <- list(...)
  # check gcd is 1
  if(gcd(...) != 1) {
    stop("GCD of values must be 1")
  }
  # vector of arguments in ascending order
  A <- sort(unlist(l))

  a_1 <- A[1]
  n <- length(A)
  a_n <- A[n]
  a_1a_n <- a_1 * a_n

  s <- c(0, rep(a_1a_n, a_1 - 1))

  # Vector A mod the minimum of A
  A_mod <- A %% a_1

  # initialise a vector p with p[1] set to n
  p <- numeric(a_1)
  p[1] <- n

  # initialise a queue as {1} (using a list for this)
  Q <- list(1)

  # While Q is non empty
  while(length(Q) > 0) {
    # set the current vertex v to the head of Q
    v <- Q[[1]]
    # and remove from Q
    Q <- Q[-1]

    for (j in 2:p[v]) {
      # set u to be the vertex at the end of the jth edge from v
      u <- v + A_mod[j]
      if(u > a_1) {
        u <- u - a_1
      }

      # compute the path weight
      w <- s[v] + A[j]

      if(w < s[u]) {
        s[u] <- w
        p[u] <- j

        # if u is not currently in Q then add u to the tail of Q
        if(!(u %in% Q)) {
          Q <- c(Q, u)
        }
      }
    }

  }
  print(p)
  print(s)
  return(max(s) - a_1)
}


dqqd_frob <- function(...) {
  l <- list(...)
  if(gcd(...) != 1) {
    stop("GCD of values must be 1")
  }
  A <- sort(unlist(l))

  a_1 <- A[1]
  n <- length(A)
  a_n <- A[n]

  s <- c(0, rep(a_n, a_1 - 1))

  p <- numeric(a_1)
  p[1] <- n - 1

  Q <- vector("list", 1000)
  Q[[1]] <- c(0)

  Z <- c(0)
  Zheap <- c()

  Amod <- A %% a_1
  Aquot <- floor(A / a_1)

  #browser()

  while(length(Z) > 0) {
    # remove weight quotient w from the head of Z
    #print(paste("removing", Z[[1]], "from the stack"))
    Z <- sort(Z)
    w <- Z[1]
    Z <- Z[-1]
    #print(paste("adding", w, "to the heap"))
    Zheap <- c(Zheap, w)

    # take the stack from the dynamic array at w
    #Qw <- Q[[w]]

    #print(Qw)
    #print(Q[[w+1]])
    Q_current <- Q[[w+1]]
    # while the stack is non empty
    while(length(Q[[w+1]]) > 0) {
      # take the first vertex from the stack
      v <- Q[[w+1]][1]
      Q[[w+1]] <- Q[[w+1]][-1]

      #print(p[v+1])
      for(j in 2:p[v+1]) {

        # compute the end vertex
        u <- v + Amod[j]

        # compute the new weight quotient
        w <- s[v+1] + Aquot[j]
        if(u >= a_1) {
          u <- u - a_1
          w <- w - 1
        }
        # print(
        #   paste("End vertex",
        #         u,
        #         "Weight quo",
        #         w)
        # )

        if(w < s[u+1]) {
          # push u onto stack Qw
          #print(paste("pushing", u, "onto Q", w))
          Q[[w]] <- c(u, Q[[w]])
          #print(paste("Putting", w, "at S", u))
          s[u+1] <- w
          #print(paste("Putting", j, "at P", u))
          p[u+1] <- j
          if(!(w %in% Zheap) && !(w %in% Z)) {
            #print(paste("Add", w, "to the priority queue"))
            Z <- c(Z, w)
          }
        }
      }
    }
  }
  print(paste("weights", s * a_1))
  print(p)
  print(s)
  return(max(s * a_1 + 0:(a_1-1)) - a_1)
}
