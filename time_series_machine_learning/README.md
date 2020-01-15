# Time Series Machine Learning Analysis and Demand Forecasting with H2O & TSstudio

Traditional approaches to time series analysis and forecasting, like _Linear Regression_, _Holt-Winters Exponential Smoothing_, _ARMA/ARIMA/SARIMA_ and _ARCH/GARCH_, have been well-established for decades and find applications in fields as varied as __business and finance__ (e.g. predict stock prices and analyse trends in financial markets), the __energy sector__ (e.g. forecast electricity consumption) and __academia__ (e.g. measure socio-political phenomena).

In more recent times, the popularisation and wider availability of open source frameworks like __Keras__, __TensorFlow__ and __scikit-learn__ helped machine learning approaches like __Random Forest__, __Extreme Gradient Boosting__, __Time Delay Neural Network__ and __Recurrent Neural Network__ to gain momentum in time series applications. These techniques allow for historical information to be introduced as input to the model through a set of time delays.

In this project I go through the various steps needed to build a __time series machine learning pipeline__ and generate a __weekly revenue forecast__. I carry out a more “traditional” __exploratory time series analysis__ with _TSstudio_ and __create a number of predictors__ using the insight I gather. I then __train and validate__ an array of __machine learning models__ with the open source __library H2O__, and __compare the models’ accuracy__ using __performance metrics__ and __actual vs predicted plots__.

## The Data

In this post I’m simply loading up the compiled dataset but I’ve also written a post called [__Loading, Merging and Joining Datasets__](https://diegousai.io/2019/09/loading-merging-and-joining-datasets/) where I show how I’ve assembled the various data feeds and sorted out the likes of variable naming, new features creation and some general housekeeping tasks. You can find the full data code on this [__Github repository__](https://github.com/DiegoUsaiUK/Loading_Merging_and_Joining_Datasets).

## Links

You can find the final article on [__my website__](https://diegousai.io/2019/12/time-series-machine-learning-analysis-and-demand-forecasting/)

I've also published the article on [The Startup](https://medium.com/swlh/time-series-machine-learning-analysis-and-demand-forecasting-with-h2o-tsstudio-b21cd58749b1)
