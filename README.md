# Natural Disasters and Education

In my thesis I explore the link between natural disasters and education
outcomes. The Stanford Education Data Archive (Reardon et al. 2021)
provides standardized test scores by county, year, grade and subject.
Data on natural disasters is available at the FEMA database and the
**rfema** package (Turner 2022) provides easy access.

Below you can see the number of cumulative disasters by county from
2008/2009 to 2017/2018.

<img src="Code & Data/DisasterMap.png" width="600" />

The outcomes of interest are mean test scores by county, and achievement
gaps between white and black, white and hispanic, male and female, and
economically advantaged and disadvantaged students.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Summary Statistics
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:left;">
N
</th>
<th style="text-align:left;">
Mean
</th>
<th style="text-align:left;">
Std. Dev.
</th>
<th style="text-align:left;">
Min
</th>
<th style="text-align:left;">
Pctl. 25
</th>
<th style="text-align:left;">
Pctl. 75
</th>
<th style="text-align:left;">
Max
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean test score
</td>
<td style="text-align:left;">
327358
</td>
<td style="text-align:left;">
-0.042
</td>
<td style="text-align:left;">
0.294
</td>
<td style="text-align:left;">
-3.196
</td>
<td style="text-align:left;">
-0.214
</td>
<td style="text-align:left;">
0.152
</td>
<td style="text-align:left;">
1.669
</td>
</tr>
<tr>
<td style="text-align:left;">
White-Black gap
</td>
<td style="text-align:left;">
130595
</td>
<td style="text-align:left;">
0.618
</td>
<td style="text-align:left;">
0.257
</td>
<td style="text-align:left;">
-0.754
</td>
<td style="text-align:left;">
0.454
</td>
<td style="text-align:left;">
0.771
</td>
<td style="text-align:left;">
2.358
</td>
</tr>
<tr>
<td style="text-align:left;">
White-Hispanic gap
</td>
<td style="text-align:left;">
140326
</td>
<td style="text-align:left;">
0.451
</td>
<td style="text-align:left;">
0.258
</td>
<td style="text-align:left;">
-1.713
</td>
<td style="text-align:left;">
0.282
</td>
<td style="text-align:left;">
0.611
</td>
<td style="text-align:left;">
2.213
</td>
</tr>
<tr>
<td style="text-align:left;">
Male-Female gap
</td>
<td style="text-align:left;">
306055
</td>
<td style="text-align:left;">
-0.131
</td>
<td style="text-align:left;">
0.199
</td>
<td style="text-align:left;">
-1.612
</td>
<td style="text-align:left;">
-0.258
</td>
<td style="text-align:left;">
0.001
</td>
<td style="text-align:left;">
1.248
</td>
</tr>
<tr>
<td style="text-align:left;">
Disadvantaged gap
</td>
<td style="text-align:left;">
282423
</td>
<td style="text-align:left;">
0.543
</td>
<td style="text-align:left;">
0.211
</td>
<td style="text-align:left;">
-0.995
</td>
<td style="text-align:left;">
0.413
</td>
<td style="text-align:left;">
0.669
</td>
<td style="text-align:left;">
2.052
</td>
</tr>
</tbody>
</table>

To identify a causal effect, I use an event study design. Due to likely
very heterogenous treatment effects, I employ the estimator by Sun and
Abraham (2021). Below you can see a plot of the treatment effects by
treatment timing.

<img src="Code & Data/ResultsPlot.png" width="700" />

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-SEDA" class="csl-entry">

Reardon, Sean, Demetra Kalogrides, Andrew Ho, Ben Shear, Erin Fahle,
Heewon Jang, and Belen Chavez. 2021. “Stanford Education Data Archive
(Version 4.1).” <http://purl.stanford.edu/db586ns4974>.

</div>

<div id="ref-Sun_2021" class="csl-entry">

Sun, Liyang, and Sarah Abraham. 2021. “Estimating Dynamic Treatment
Effects in Event Studies with Heterogeneous Treatment Effects.” *Journal
of Econometrics* 225 (2): 175–99.
https://doi.org/<https://doi.org/10.1016/j.jeconom.2020.09.006>.

</div>

<div id="ref-rfema" class="csl-entry">

Turner, Dylan. 2022. “Rfema: Access the openFEMA API.” *rOpenSci*.
<https://github.com/ropensci/rfema>.

</div>

</div>
