library(lpSolve)

obj_fun <- c(13, 23)

constr_eq <- matrix(c(5,15,4,4,35,20), ncol = 2, byrow = TRUE)
constr_dir <- c('<=','<=','<=')
constr_rhs <- c(480, 160, 1190)

solution <- lp('max',obj_fun, constr_eq, constr_dir, constr_rhs, compute.sens = TRUE)

solution$objval
solution$solution


obj_fun <- c(480, 160, 1190)
constr_eq <- matrix(c(5,4,35,15,4,20), ncol = 3, byrow = TRUE)
constr_dir <- c('>=','>=','>=')
constr_rhs <- c(13, 23)

solution <- lp('min',obj_fun, constr_eq, constr_dir, constr_rhs, compute.sens = TRUE)

solution$objval
solution$solution

# sensitivity analysis

solution$sens.coef.from
solution$sens.coef.to

# Finding dual solution
solution$duals


