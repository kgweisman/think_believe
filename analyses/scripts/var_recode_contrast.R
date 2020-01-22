# levels
levels_country <- c("US", "Ghana", "Thailand", "China", "Vanuatu")

# contrasts (effect-coding)
contrast_country <- cbind("_gh" = c(-1, 1, 0, 0, 0),
                           "_th" = c(-1, 0, 1, 0, 0),
                           "_ch" = c(-1, 0, 0, 1, 0),
                           "_vt" = c(-1, 0, 0, 0, 1))

contrast_category <- cbind("_RC" = c(1, 0, 0, 0, -1),
                           "_RB" = c(0, 1, 0, 0, -1),
                           "_FW" = c(0, 0, 1, 0, -1),
                           "_FE" = c(0, 0, 0, 0, -1))

contrast_super_cat = cbind("_relig" = c(1, -1))

# contrasts (orthogonal contrasts)
contrast_category_orth <- cbind("_relig_fact" = c(3, 3, -2, -2, -2),
                                "_relig_C_B" = c(1, -1, 0, 0, 0),
                                "_fact_WE_L" = c(0, 0, 1, 1, -2),
                                "_fact_W_E" = c(0, 0, 1, -1, 0))
