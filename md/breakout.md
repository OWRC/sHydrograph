
The plot above is the output of R's [Seasonal Decomposition of Time Series by Loess *(`stl`)* function](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/stl).

As suggested in [this post](https://thecodeforest.github.io/post/time_series_outlier_detection.html), the `stl` function provides a great means of (visually) detecting outliers in a time-series.

The plot above is shown in four parts:

1. __Hydrograph:__ shows the raw/original time-series;
1. __Seasonal:__ shows the cyclical seasonal signal within the raw trim-series;
1. __Trend:__ shows the long-term trend found in the data; and
1. __Remainder:__ is the difference of `hydrograph` - `seasonal` - `trend` used to show the residual noise of the time-series. Where this can be particularly helpful is to identify outliers that stick-out well beyond the noise.