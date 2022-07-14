# Natural Disasters and Education

In my thesis I explore the link between natural disasters and education
outcomes. The Stanford Education Data Archive (Reardon et al. 2021)
provides standardized test scores by county, year, grade and subject.
Data on natural disasters is available at the FEMA database and the
**rfema** package (Turner 2022) provides easy access. The map below
shows natural disaster exposure by county for schoolyears 2008-09
through 2017-2018.

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

The outcomes of interest are mean test scores by county, and mean test
scores for black, hispanic, female, and economically disadvantaged
students.

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

To identify a causal effect, I use an event study design. Due to likely
very heterogenous treatment effects, I employ the estimator by Sun and
Abraham (2021). Below you can see the results of my main analysis,
indicating that natural disasters do have a negative short-term effect
on academic achievement in mathematics.

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

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
