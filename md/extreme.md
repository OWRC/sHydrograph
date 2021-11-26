
## Please Read:

The annual extreme plotting presented above has been taken directly from peak flow analysis used in hydrology. As with any technique, these methods come with a set of assumptions the user must deem acceptable. To the authors' knowledge, applying this method to groundwater data is unprecedented, and may not be appropriate. We have nonetheless placed these routines on this app in the hopes that, where and when appropriate, the method will give some insight. **PLEASE USE WITH CAUTION!**

Assumptions include:

- Stationarity: no significant background signal/trend has been changing over the long-term. *This will be violated under a changing climate, a changing pumping regime, etc.*
- Independence: originally, this analysis is to be applied to event-based phenomenon (i.e., rainfall) where it is assumed that all events are independent of each other. *Groundwater monitoring is continuous and thus violates this assumption.*
- the statistical characteristics of the sample represent those of the population.


#### Peak flow frequency
Peak flow frequency curves were modified (with gratitude) from [headwateranalytics.com](http://www.headwateranalytics.com/blog/flood-frequency-analysis-in-r) *(accessed December, 2016)*.

The method allows for the use of 5 distributions: Log-Pearson type 3 *(default)*, Weibull, Gumbel, Generalized Extreme Value (GEV), and the three-parameter lognormal models. (The user may change the distribution and refresh the plots.)

By default, 90% confidence intervals are then plotted based in the bootstrap technique from 25,000 samples assuming a Log-Pearson III distribution.