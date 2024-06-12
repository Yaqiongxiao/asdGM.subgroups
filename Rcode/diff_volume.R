diff_volume <- function(gr, volume,ylab) {
	
	# violin plot
	p <- ggplot(datafile,aes_string(x = gr,volume))+ 
		theme(axis.title = element_blank()) + #delete x,y,title 
		geom_violin(trim = F,width = 1.0) +
		#geom_jitter(size = 4,width = 0.2, alpha = 0.4, col = "blue")+ # higher width, more scattered dots
		theme_classic() +# set y/x color as black, set backgroud as white
		#scale_x_discrete(labels=c("nonASD" = "non-ASD")) + 
		theme(legend.position  = c(1.5,0.5),
		      axis.text.y =element_text(size=16,face = "bold",color = 'black'),
		      axis.text.x =  element_blank(),
		      axis.title.x = element_blank(),
		      axis.title.y = element_text(size=18,face="bold"),
		      axis.line.x = element_line(size = 1,color = 'black'),
		      axis.line.y = element_line(size = 1,color = 'black'))+
		ylab(ylab)# change y label
	
	if (gr == "Dx") {
		a <- p + geom_boxplot(width=0.2, fill = c('#2297E6','#F94700'))
			
		
		ggsave(here(paste0("results/global_diff_",volume,".png")), height = 3, width = 4)
	} else {
		a <- p + geom_boxplot(width=0.2, fill = c('#F94700','#77933B','#498399'))
		
		ggsave(here(paste0("results/global_diff_",volume,".ASDsubgroups.png")), height = 3, width = 4)
	}
	
	print(a)
	
}
