load("../Data/indexList_MAIN.RData")

n_matches = 560

# Step 3 -----------------------------------------------------------------------

p_val_df = rep(NA, 13)

set.seed(2023)

load('../Output/Global/global_t_stat_FINAL.dat')

for(k in 2:13) {
    
    load(paste0('../Output/origGridInfo/sim_orig_', k,".dat"))
    which_zeros_orig = which(sim_orig$DATA$n_off_1_prec == 0 | sim_orig$DATA$n_off_2_prec == 0)
    sim_orig$DATA$n_arr_1_prec[which_zeros_orig] = sim_orig$DATA$n_arr_1_prec[which_zeros_orig] + 1
    sim_orig$DATA$n_arr_2_prec[which_zeros_orig] = sim_orig$DATA$n_arr_2_prec[which_zeros_orig] + 1
    sim_orig$DATA$n_off_1_prec[which_zeros_orig] = sim_orig$DATA$n_off_1_prec[which_zeros_orig] + 1
    sim_orig$DATA$n_off_2_prec[which_zeros_orig] = sim_orig$DATA$n_off_2_prec[which_zeros_orig] + 1

    t_stat_orig = abs(sim_orig$DATA$n_arr_1_prec / sim_orig$DATA$n_off_1_prec
                      - sim_orig$DATA$n_arr_2_prec / sim_orig$DATA$n_off_2_prec)
    
    t_stat = max(t_stat_orig, na.rm = T)
    
    p_val_df[k] = mean(global_t_stat[[k]]$max_t_stat > t_stat)
    
}
print("Global p-values for each buffer width")
print(p_val_df)
