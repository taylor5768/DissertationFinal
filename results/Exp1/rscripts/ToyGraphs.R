library(simstudy)
library(ggplot2)


party <- c("Democrat","Democrat","Republican","Republican","Democrat","Republican")
orientation <- c("liberal","conservative","conservative","liberal","neutral","neutral")
ratings <- c(.8,.2,.8,.2,.5,.5)


projection.data <- data.frame(party,orientation,ratings)

ggplot(projection.data, aes(x=orientation,y=ratings,fill=party),xpd=FALSE) +
  geom_bar(stat="identity",position=position_dodge(),width=.5) +
  scale_fill_manual(labels=c("Democrat\nspeaker","Republican\nspeaker"),values=c("#56B4E9", "tomato1"))+
  xlab("CC orientation") +
  ylab("mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal","neutral")) +
  ylim(0,1)+
  theme(legend.position="bottom",aspect.ratio=1,axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"),legend.title=element_blank(),legend.text=element_text(size=12),legend.margin = margin(t=-.1,l=.1,r=.1,b=.1,unit='cm'))

ggplot(projection.data, aes(x=orientation,y=ratings,fill=party),xpd=FALSE) +
  geom_bar(stat="identity",position=position_dodge(),width=.5) +
  scale_fill_manual(labels=c("Non-southern\nspeaker","Southern\nspeaker"),values=c("#56B4E9", "tomato1"))+
  xlab("CC orientation") +
  ylab("mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal","neutral")) +
  ylim(0,1)+
  theme(legend.position="bottom",aspect.ratio=1,axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"),legend.title=element_blank(),legend.text=element_text(size=12),legend.margin = margin(t=-.1,l=.1,r=.1,b=.1,unit='cm'))

spBeliefs <-c(seq(from=0,to=1,by=.1))
projection <-c(seq(from=0,to=1,by=.1))

set.seed(200)
scatter_data <- tibble(x_var = runif(500, min = 0, max = 1)
                       ,y_var = x_var + rnorm(500)
)

ggplot(data = scatter_data, aes(x = x_var, y = y_var)) +
  geom_point()+
  stat_smooth(method = 'lm')+
  ylab("certainty rating")+
  xlab("listener belief rating")+
  ylim(0,1)+
  xlim(0,1)

ggplot(data = scatter_data, aes(x = x_var, y = y_var)) +
  geom_point()+
  stat_smooth(method = 'lm')+
  ylab("certainty rating")+
  xlab("speaker belief rating")+
  ylim(0,1)+
  xlim(0,1)
  
set.seed(100)
scatter_data_neg <- tibble(x_var = runif(500, min = 0, max = 1)
                       ,y_var = -2*x_var - rnorm(500))

ggplot(data = scatter_data_neg , aes(x = x_var, y = y_var)) +
  geom_point()+
  stat_smooth(method = 'lm')+
  ylab("certainty rating")+
  xlab("democrat rating")+
  ylim(0,1)+
  xlim(0,1)+
  ggtitle("conservative CCs")

ggplot(data = scatter_data_neg , aes(x = x_var, y = y_var)) +
  geom_point()+
  stat_smooth(method = 'lm')+
  ylab("certainty rating")+
  xlab("republican rating")+
  ylim(0,1)+
  xlim(0,1)+
  ggtitle("liberal CCs")
  

ggplot(data = scatter_data, aes(x = x_var, y = y_var)) +
  geom_point()+
  stat_smooth(method = 'lm')+
  ylab("certainty rating")+
  xlab("democrat rating")+
  ylim(0,1)+
  xlim(0,1)+
  ggtitle("liberal CCs")

ggplot(data = scatter_data, aes(x = x_var, y = y_var)) +
  geom_point()+
  stat_smooth(method = 'lm')+
  ylab("certainty rating")+
  xlab("republican rating")+
  ylim(0,1)+
  xlim(0,1)+
  ggtitle("conservative CCs")
