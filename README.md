    ## Warning: Paket 'data.table' wurde unter R Version 4.1.1 erstellt

# Daily Weather Data

Using data provided by the Daily Global Historical Climatology Network
(Menne et al. 2012), I identify extreme heat events in the historical
temperature time series. Historical weather data is available for about
1200 measurement stations in the US. Based on the latitude and longitude
coordinates I assign each station to the county it belongs to. In case
there is more than one station per county, I take the mean among those.

Following the National Climate Data Center (NCDC), I define an extreme
heat event as two or more consecutive days in which the minimum
temperature exceeds the 85th percentile of July and August minimum
temperatures (Habeeb, Vargo, and Stone 2015).

![Minimum daily temperatures for selected counties (extreme heat events
shaded in
red)](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

# References

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
