# here we use the R implementation of random forests with very slightly better results than bagging

library(randomForest)
wine = read.csv("../data/winequality-white.csv", sep=";");
N = nrow(wine);

bad = (wine$quality < 6)
ok = (wine$quality == 6)
good = (wine$quality > 6)
wine$quality[bad] = 1;
wine$quality[ok] = 2;
wine$quality[good] = 3;
wine$quality = as.factor(wine$quality)
winetrain = wine[1:(N/2),]
winetest = wine[(N/2+1):N,]

model = randomForest(quality ~ . , data = winetrain)
pred = predict(model, newdata = winetest)
error_rate = sum(pred != winetest$quality)/(N/2);
print(error_rate)
