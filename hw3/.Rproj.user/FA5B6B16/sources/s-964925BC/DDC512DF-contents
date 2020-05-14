# 2 pt.1
alpha = 0.05
qt(p = 1-alpha/2, df = 9)

(76/15) * 1.5 + (79/15)*1.6

qf(p = 1-alpha, df1 = 2, df2 = 8)
# 2 pt.3
n=10
p=2
((n-1)*p) / (n-p) * qf(p = 1-alpha, df1 = 2, df2 = 8)

# 2 pt.4
rad1 = sqrt(5)*sqrt(((n-1)*p) / (n*(n-p)) * qf(p = 1-alpha, df1 = 2, df2 = 8))
rad2 = sqrt(3)*sqrt(((n-1)*p) / (n*(n-p)) * qf(p = 1-alpha, df1 = 2, df2 = 8))

# 3 pt. 2
set.seed(135)
sim_record = c()
for(i in 1:1000) {
  sim_data = rmvnorm(n = 25, mean = c(1,1), sigma = matrix(c(3,1,1,4), nrow = 2))
  one_vec = matrix(rep(1,25), ncol = 1)
  data_mean = matrix(apply(sim_data, 2, mean), nrow = 2)
  S = (1/24)*t(sim_data - one_vec%*%t(data_mean))%*%(sim_data - one_vec%*%t(data_mean))
  
  sim_record = c(sim_record, t((data_mean - c(1,1)))%*%solve(S)%*%(data_mean - c(1,1)))
}

hist(sim_record, main = TeX('$(\\bar{x}-\\mu)^TS^{-1}(\\bar{x}-\\mu)$'),
     xlab = 'Simulates Values')

# 4
p = 2
n = 10
(((n-1)*p) /(n-p)) * qf(p = 1-alpha, df1 = p, df2 = n-p)


