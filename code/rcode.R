#M_n^(j) (j=1,2)
m_n <- function(data, n, k, j){
  data_ordered <- sort(data, decreasing = FALSE)[(n-k):n]
  mean(((log(data_ordered)-log(data_ordered[1]))[-1])^j)
}

#moment estimator
gamma_m <- function(data, n, k) {
  m_n_1 <- m_n(data, n, k, 1)
  m_n_2 <- m_n(data, n, k, 2)
  m_n_1+1-0.5*(1-(m_n_1^2/m_n_2))^(-1)
}


#scale estimator
sigma_m <- function(data, n, k) {
  m_n_1 <- m_n(data, n, k, 1)
  m_n_2 <- m_n(data, n, k, 2)
  gamma_minus <- 1-0.5*(1-(m_n_1^2/m_n_2))^(-1)
  data_ordered <- sort(data, decreasing = FALSE)[(n-k):n]
  data_ordered[1]*m_n_1*(1-gamma_minus)
}

#prob. est.
p_estim <- function(data, n, k_1, k_2, t, h) {
  gamma <- gamma_m(data, n, k_1)
  data_ordered <- sort(data, decreasing = FALSE)[(n-k_2):n]
  (k_2/n)*max(0,(((t-h-data_ordered[1])*gamma/sigma_m(data, n, k_1))+1))^(-1/gamma)
}

# #quantile estimator for testing
# p_q <- function(data, n, k, p) {
#   gamma <- gamma_m(data, n, k)
#   data_ordered <- sort(data, decreasing = FALSE)[(n-k):n]
#   data_ordered[1]+sigma_m(data,n,k)*((k/(n*p))^gamma-1)/gamma
# }

# #probability estimator when gamma=0
# p_estim_zero <- function(data, n, k, t, h) {
#   data_ordered <- sort(data, decreasing = FALSE)[(n-k):n]
#   (k/n)*exp(-(t-h-data_ordered[1])/sigma_m(data,n,k))
# }