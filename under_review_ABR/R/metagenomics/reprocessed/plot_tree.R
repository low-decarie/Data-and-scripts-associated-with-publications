require(ape)
#require(devtools)
#install_github("gjuggler/ggphylo")
require(ggphylo)



pdf("./Plots/test_tree.pdf")
MyTree <- read.tree("./Metagenomics data/Re_processed_data/algae_tree/phylo_tree.tree")

plot(MyTree)

plot(chronos(MyTree))



graphics.off()