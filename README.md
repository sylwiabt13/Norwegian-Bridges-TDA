# Applications of Topological Data Analysis to Multivariate Time Series Structural Health Monitoring Data

This project is largely associated with my master's dissertation, investigating the application of Topological Data Analysis to large multivariate time series data sets, specifically in the setting of Hardanger Bridge, the largest suspension bridge in Norway. 

# What is TDA?

The core intuitive idea behind Topological Data Analysis is to capture relevant topological information about the ‘shape’ of the dataset. In particular, one can see Topological Data Analysis as an extension of the analysis of density clusters, which focus solely on the 0-dimensional features, namely the ‘clusters’ or ‘connected components’ of the data points. Whereas the purpose of Topological Data Analysis is to give a more holistic view of the data by looking not only at the ‘clusters’ of the data, but also the higher dimensional features and other structures in our dataset.

# Norwegian Bridges TDA

We largely focus on data from the Hardanger Bridge Monitoring Project, with the aim of extracting holistic and qualitative information using topological data analysis.

There is a superfluous quantity of bridge data provided by the Norwegian University of Science and Technology (NTNU), with wind and acceleration data being monitored since 2013. (Fenerci et al. 2020) The Hardanger Bridge Monitoring project data consists of measurements from 20 accelerometers and 8 anemometers, sensors spatially distributed across the bridge. Each accelerometer measurement is a 3-dimensional time-series measuring the accelerations in the vertical direction, and the orthogonal horizontal transverse and longitudinal directions. Each anemometer measurement is a 4-dimensional time-series measuring weather measurements including temperature, horizontal, vertical windspeeds and wind direction angle.
