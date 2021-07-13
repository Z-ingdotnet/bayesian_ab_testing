

require(dplyr)
require(ggplot2)
require(cowplot)


Motencalo.trials <- 10000000
prior.alpha_offerAonly <-2
prior.beta_offerAonly <- 8
prior.alpha_offerANofferB <- 3
prior.beta_offerANofferB <- 7
prior.alphaofferBonly <- 8
prior.betaofferBonly<- 2




offerA_offerB_sim<- rbeta(Motencalo.trials,34+prior.alpha_offerANofferB,3+prior.beta_offerANofferB)

offerBonly_sim<- rbeta(Motencalo.trials,251+prior.alphaofferBonly,108+prior.betaofferBonly)

offerAonly_sim<- rbeta(Motencalo.trials,68+prior.alpha_offerAonly,6+prior.beta_offerAonly)

probability.offerA_offerB_superior <- sum(offerA_offerB_sim > offerBonly_sim)/Motencalo.trials

probability.offerAonly_superior <- sum(offerAonly_sim > offerBonly_sim)/Motencalo.trials

hist(offerA_offerB_sim/ offerBonly_sim)

hist(offerAonly_sim/ offerBonly_sim)

hist(offerAonly_sim/ offerA_offerB_sim)


plot(ecdf (offerA_offerB_sim/ offerBonly_sim))




size <- 1000000

offerAoffer <- data.frame(group = c('offerBonly', 'offerA_offerB','offerAonly'),
                           return.rate = c(0.92, 0.68,0.918),
                           stringsAsFactors = FALSE)

# get sample distributions
each.sample.size <- 250
#num.samples <- 10000
n.trials <- 1000000

getMean <- function(nIndex, num) {
  mean(sample_n(df %>% filter(group==offerAoffer$group[nIndex]), num)$returned)
}


offerA_offerB <- data.frame(promo=offerAoffer$group[2], 
                                  returned.rate=rbeta(Motencalo.trials,34+prior.alpha_offerANofferB,3+prior.beta_offerANofferB))

offerBonly <- data.frame(promo=offerAoffer$group[1], 
                              returned.rate=rbeta(Motencalo.trials,251+prior.alphaofferBonly,108+prior.betaofferBonly))

offerAonly <- data.frame(promo=offerAoffer$group[3], 
                                  returned.rate=rbeta(Motencalo.trials,34+prior.alpha_offerAonly,3+prior.beta_offerAonly))

combined.df <- rbind(offerBonly,offerA_offerB,offerAonly)

plot.density <- 
  combined.df %>% 
  ggplot(aes(x=returned.rate, colour=promo)) +
  geom_density() +
  theme(axis.text.y=element_blank()) +
  ggtitle("Motencalo Simulation simulated Return Rate Variance for three variants")
# boxplot using a custom box size 
#  where the box size is 0.05 < p < 0.95
#  and the whisker length is min, max
plot.boxplot <- 
  combined.df %>% 
  ggplot(aes(x=promo, y=returned.rate, colour=promo)) +
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.05, 0.5, 0.95, 1.00)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge") +
  coord_flip() +
  theme(axis.text.y=element_blank())
plot_grid(plot.density, plot.boxplot, ncol = 1, nrow = 2)





