About
================

This application estimates how likely an attorney used peremptory
challenges during jury selection to strike potential jurors based on
their race or gender. The analysis builds on prior work by Kadane (2018)
and Barrett (2007) on statistical analysis for *Batson* challenges.

To estimate such bias, this application relies on (1) user-inputted data
on how an attorney has used strikes in the case before them; and (2) if
available, jury-selection data on that attorney’s use of strikes in some
past cases during 2013-2017 in the federal district court for the
District of Connecticut.

Based on this data, this application graphs
![b](https://latex.codecogs.com/png.latex?b "b") – our measure of bias –
for the prosecution and the defense: **positive** values reflect bias
*against* the cognizable class, while **negative** values reflect bias
*for* the cognizable class. If ![b
= 0](https://latex.codecogs.com/png.latex?b%20%3D%200 "b = 0"), that
there is zero bias.

The plots show how likely each possible value of
![b](https://latex.codecogs.com/png.latex?b "b") is, given the data we
have. The curve’s spread (the distribution’s *variance*) depicts how
confident we should be in any particular value of
![b](https://latex.codecogs.com/png.latex?b "b") from the data we have.

The dashed lines indicate a pre-set *credible interval*, i.e., here the
values of ![d](https://latex.codecogs.com/png.latex?d "d") between which
80% of the likely values of ![b](https://latex.codecogs.com/png.latex?b
"b") fall. If zero falls within this interval, then we should be wary
about (have low confidence in) inferring bias (for or against the
cognizable class) from the strike data alone. If zero falls outside this
interval, we should feel confident in inferring bias from that strike
data.

The grey graph depicts ![b](https://latex.codecogs.com/png.latex?b "b")
based *only* on a pre-set distribution of
![b](https://latex.codecogs.com/png.latex?b "b") to reflect a starting
point that bias is unlikely, as adjusted by any historical strike data
we have for a particular selected attorney. This represents our *prior
beliefs* about an attorney’s possible bias in using peremptory
challenges, before we know anything about how any attorney uses
peremptory strikes in the case before us. The red (for prosecution) and
blue (for the defense) distributions in the foreground represent our
*updated* expectations after accounting for the user-inputted strike
data.

### References

<div id="refs" class="references">

<div id="ref-Barrett2007">

Barrett, Bruce E. 2007. “Detecting Bias in Jury Selection.” *The
American Statistician* 61 (4): 296–301.
<https://doi.org/10.1198/000313007X243629>.

</div>

<div id="ref-Kadane2018">

Kadane, Joseph B. 2018. “Statistics for Batson Challenges.” *Law,
Probability and Risk* 17 (1): 1–13.
<https://doi.org/10.1093/lpr/mgx016>.

</div>

</div>
