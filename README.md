# Downscaled_SIF_Comps

This project compares downscaled SIF products to actual SIF data. However, it has balooned into investigating the asynchrony of SIF and NIR(v) in the tropics.

## Calculating population weighted mean and SEM from gridded data

When using gridded data to calculate regional-scale means, such as using gridded data to investigate seasonality of SIF for South American tropical forests, we need to consider the number of soundings (n) in each gridcell when calculating the population mean. Since we know the gridcell-level n and standard deviation (std), we can derive the standard error of the mean (SEM) for the entire population. This will allow us to either create error bars for our plots, or when they are very small, we can plot the SEMs into a histogram or some other plot.

I created a function called get_ts in the error.R files, which is also in the time series comps.R files. This function calculates the population mean using weights and also the SEM for each time series. The equations are as follows, using three groups (gridcells) as an example:

![image](https://user-images.githubusercontent.com/31934468/174311110-cd523a41-599d-405b-bdfb-d397b81d9f46.png)

![image](https://user-images.githubusercontent.com/31934468/174311164-13257b8a-54a9-4669-863d-a2fe296252eb.png)

Where N is population number of samples (soundings) and μ is population mean (weighted).


## Downscaled products

### CSIF

Data:  https://osf.io/8xqy6/

Paper: https://bg.copernicus.org/articles/15/5779/2018/

Zhang, Y., Joiner, J., Alemohammad, S.H., Zhou, S. and Gentine, P., 2018. A global spatially contiguous solar-induced fluorescence (CSIF) dataset using neural networks. Biogeosciences, 15(19), pp.5779-5800.

### GOSIF

Data:  https://globalecology.unh.edu/data/GOSIF.html

Paper: https://www.mdpi.com/2072-4292/11/5/517

Li, Xing, and Jingfeng Xiao. "A global, 0.05-degree product of solar-induced chlorophyll fluorescence derived from OCO-2, MODIS, and reanalysis data." Remote Sensing 11, no. 5 (2019): 517.

### SIF-LUE

Data:  https://data.jrc.ec.europa.eu/dataset/21935ffc-b797-4bee-94da-8fec85b3f9e1

Paper: https://essd.copernicus.org/articles/12/1101/2020/

Duveiller, Gregory, Federico Filipponi, Sophia Walther, Philipp Köhler, Christian Frankenberg, Luis Guanter, and Alessandro Cescatti. "A spatially downscaled sun-induced fluorescence global product for enhanced monitoring of vegetation productivity." Earth System Science Data 12, no. 2 (2020): 1101-1116.

 
