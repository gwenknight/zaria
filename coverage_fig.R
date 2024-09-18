#### Figure on coverage 

# Settings
library(tidyverse)
library(scico)
theme_set(theme_bw())

# Mock data
cov_syn = seq(0,0.99,0.001)
r_in_not_samp = seq(0,100,1)

vals = as.data.frame(cbind(rep(cov_syn, length(r_in_not_samp)), 
                           rep(r_in_not_samp, each = length(cov_syn))))

colnames(vals) = c("cov","r_out")

vals_plot <- vals %>% mutate(r_in = (20 - (1-cov) * r_out) / (cov))

ggplot(vals_plot %>% filter(r_in>0, r_in < 100), aes(x=r_in, y = cov, group = r_out)) + geom_point(aes(col = r_out)) + 
  scale_y_continuous("Proportion of patients with syndrome sampled\n(1 = all sampled, 0 = none)", expand = c(0, 0)) + 
  scale_x_continuous("Resistance prevalence in sample", expand = c(0, 0)) +
  scale_color_viridis_c("Percentage of\nthose not\nsampled that\nare resistant", option = "turbo") +  #scale_color_continuous("Percentage of\nthose not\nsampled that\nare resistant",type = "turbo") + 
  ggtitle("20% resistance threshold") + 
  geom_vline(xintercept = c(20,40), lty = "dashed") + 
  geom_hline(yintercept = c(0.5), lty = "dashed") 

##### 15% threshold 

vals_plot <- vals %>% mutate(r_in = (15 - (1-cov) * r_out) / cov)

ggplot(vals_plot %>% filter(r_in>0, r_in < 100), aes(x=r_in, y = cov, group = r_out)) + geom_point(aes(col = r_out)) + 
  scale_y_continuous("Threshold for sampling:\nProportion of patients with syndrome sampled\n(1 = all sampled, 0 = none)", expand = c(0, 0)) + 
  scale_x_continuous("Resistance prevalence in sample", expand = c(0, 0)) +
  scale_color_viridis_c("Maximum\npercentage of\nthose not\nsampled that\nare resistant", option = "turbo") +  #scale_color_continuous("Percentage of\nthose not\nsampled that\nare resistant",type = "turbo") + 
  ggtitle("15% resistance threshold") + 
  geom_vline(xintercept = c(10,30), lty = "dashed") + 
  geom_hline(yintercept = c(0.9, 0.65), lty = "dashed") 
ggsave("threshold_sample_plot_15.png")
