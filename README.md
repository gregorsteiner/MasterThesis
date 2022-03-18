## Extreme Heat Events and Fatal Car Accidents

In my master thesis I want to explore the causal effect of extreme heat
events on fatal car accidents.

### Weather Data

Using data provided by the Daily Global Historical Climatology Network
(Menne et al. 2012), I identify extreme heat events in the historical
temperature time series. Historical weather data is available for about
1200 measurement stations in the US. Based on the latitude and longitude
coordinates I assign each station to the county it belongs to. In case
there is more than one station per county, I take the mean among those.

Following the National Climate Data Center (NCDC), I define an extreme
heat event as days in which the maximum or minimum temperature exceed
the 85th percentile of July and August maximum or minimum temperatures
(Habeeb, Vargo, and Stone 2015). Below you can see the historical daily
maximum temperatures for a few selected counties with such extreme heat
events shaded in red.

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

Below you can see the average (across all counties) number of heat days
based on minimum and maximum temperatures for each year.

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Car Crash Data

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Habeeb_2015" class="csl-entry">

Habeeb, Dana, Jason Vargo, and Brian Stone. 2015. “Rising Heat Wave
Trends in Large US Cities.” *Natural Hazards* 76 (3): 1651–65.
<https://doi.org/10.1007/s11069-014-1563-z>.

</div>

<div id="ref-Menne_2012" class="csl-entry">

Menne, Matthew J., Imke Durre, Bryant Korzeniewski, Shelley McNeill,
Kristy Thomas, Xungang Yin, Steven Anthony, et al. 2012. “Global
Historical Climatology Network - Daily (GHCN-Daily), Version 3.” NOAA
National Centers for Environmental Information.
<https://doi.org/10.7289/V5D21VHZ>.

</div>

</div>
