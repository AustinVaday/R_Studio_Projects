data = read.delim("Survey Data.txt", header=TRUE, sep="\t")
dim(data)
data[1:3,1:5]
names(data)
attach(data)
head(data)
str(data$Q07_Age)

#initial plot
plot(data$Q07_Age)
title("Survey Age Index")

#fix the plot
plot(data$Q07_Age, ylim=c(0,250))
title("Survey Age Index Zoomed View")

#Make a table
summary(data$Q07_Age)

#Create mosaic plot
Age_Spread=table(data$Q07_Age)
mosaicplot(Age_Spread)

# 2
plot(data$Q02_Gender)
title("Survey Gender Representation")

Gender_Spread=table(data$Q02_Gender)
mosaicplot(Gender_Spread)
summary(data$Q02_Gender)


# 3
tbl = xtabs(~Q22_gays.and.lesbians+Q16_Political.Orientation, data=warpbreaks)
chisq.test(tbl)
barplot(tbl, legend=TRUE)
# 4 stem and leaf plot
stem(data$Q04_Hand.Span)
stem(data$Q06_Height)
#scatter plot
plot(data$Q04_Hand.Span, data$Q06_Height)
title("Hand Span and Height Scatter Plot")


#5
boxplot(Q07_Age~Q21_Death.Penalty, ylim=c(15,28))
title("Boxplots of Age and Death Penalty Variables")
