About this Prototype
================

### Aim

This application estimates how likely an attorney used peremptory
challenges during jury selection to strike potential jurors based on
their race or gender. The analysis builds on prior work by Kadane (2018)
and Barrett (2007) on statistical analysis for *Batson* challenges. To
estimate such bias, this application relies on two kinds of data: (1)
user-inputted data on how an attorney has used strikes in the case
before them; and (2) if available, pre-compiled historical data on that
attorney’s use of strikes in past cases.

### Analysis

In the above plots, ![d](https://latex.codecogs.com/png.latex?d "d") is
our measure of bias: **positive** values of
![d](https://latex.codecogs.com/png.latex?d "d") reflect bias *against*
the cognizable class, while **negative** values of
![d](https://latex.codecogs.com/png.latex?d "d") reflect bias *for* the
cognizable class. If ![d](https://latex.codecogs.com/png.latex?d "d") is
exactly **zero**, we can be completely certain that there is zero bias.

Here, we represent ![d](https://latex.codecogs.com/png.latex?d "d") as a
distribution of how likely each possible value of
![d](https://latex.codecogs.com/png.latex?d "d") is, given the data we
have. This lets us use the curve’s width (the distribution’s *standard
deviation*) to depict how confident we should be in inferring bias from
the data we have: The narrower the curve, the more confident we should
be.

The dashed lines indicate a pre-set *credible interval*, i.e., here the
values of ![d](https://latex.codecogs.com/png.latex?d "d") between which
80% of the likely values of ![d](https://latex.codecogs.com/png.latex?d
"d") fall. If zero falls within this interval, then we should be wary
about (have low confidence in) inferring bias (for or against the
cognizable class) from the strike data. If zero falls outside this
interval, we should feel confident in inferring bias from the strike
data. As we get more data, we should feel more (or less) confident about
particular values of ![d](https://latex.codecogs.com/png.latex?d "d").

The grey graph is the distribution of
![d](https://latex.codecogs.com/png.latex?d "d") values based *only* on
a pre-set distribution of ![d](https://latex.codecogs.com/png.latex?d
"d") to reflect a starting point that bias is unlikely, as adjusted by
historical strike data, if available. This represents our *prior
beliefs* about an attorney’s possible bias in using peremptory
challenges, before we know anything about how any attorney uses
peremptory strikes in the case before us. The red (for prosecution) and
blue (for the defense) distributions in the foreground represent our
*updated* expectations after accounting for the user-inputted strike
data (in the table on the left).

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
