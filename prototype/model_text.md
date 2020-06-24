Model
================

Like Kadane (2018) and Barrett (2007), we model the probability that the
struck person belonged (or not) to the cognizable class as a function of
(1) the number of cognizable class members (e.g., racial minorities,
women) and non-class members (e.g., White people, men) in the set of
people who could have been struck; and (2) an unobserved variable
![b](https://latex.codecogs.com/png.latex?b "b") that indicates how much
weight the striking party placed on the struck juror’s membership in the
cognizable class (e.g., whether that juror was racial minority or White;
women or man).

For any given jury selection ![i](https://latex.codecogs.com/png.latex?i
"i"), let ![j](https://latex.codecogs.com/png.latex?j "j") denote a
peremptory strike used, and let
![X\_{ij}](https://latex.codecogs.com/png.latex?X_%7Bij%7D "X_{ij}")
denote whether a party used that strike on a person who belongs to a
“cognizable class”, *i.e.*, ![X\_{ij}
= 0](https://latex.codecogs.com/png.latex?X_%7Bij%7D%20%3D%200
"X_{ij} = 0") if no, and ![X\_{ij}
= 1](https://latex.codecogs.com/png.latex?X_%7Bij%7D%20%3D%201
"X_{ij} = 1") if yes. If “race” is the bias type of interest, the
cognizable class (![X
= 1](https://latex.codecogs.com/png.latex?X%20%3D%201 "X = 1")) is
racial minority jurors (![X
= 0](https://latex.codecogs.com/png.latex?X%20%3D%200 "X = 0") for White
jurors). If “gender” is the bias of interest, the cognizable class is
female jurors (![X=0](https://latex.codecogs.com/png.latex?X%3D0 "X=0")
for male jurors).

We are interested in estimating the likely values of
![b](https://latex.codecogs.com/png.latex?b "b"), given the strike data
we have. There is zero bias only if the probability of striking a
cognizable class member is the same as the probability of striking a
non-cognizable class member. Accordingly,

  
![w = \\frac{Pr(X=1)}{Pr(X
= 0)}](https://latex.codecogs.com/png.latex?w%20%3D%20%5Cfrac%7BPr%28X%3D1%29%7D%7BPr%28X%20%3D%200%29%7D
"w = \\frac{Pr(X=1)}{Pr(X = 0)}")  
  
![w = e^b](https://latex.codecogs.com/png.latex?w%20%3D%20e%5Eb
"w = e^b")  
  
![b \\sim
\\text{Normal}(\\mu\_b,\\sigma\_b)](https://latex.codecogs.com/png.latex?b%20%5Csim%20%5Ctext%7BNormal%7D%28%5Cmu_b%2C%5Csigma_b%29
"b \\sim \\text{Normal}(\\mu_b,\\sigma_b)")  
  
![\\mu\_b = 0](https://latex.codecogs.com/png.latex?%5Cmu_b%20%3D%200
"\\mu_b = 0")  
  
![\\sigma\_b
= 2](https://latex.codecogs.com/png.latex?%5Csigma_b%20%3D%202
"\\sigma_b = 2")  

This way, if it is equally likely that a member and non-member of the
cognizable class will be stricken, then ![w = \\frac{Pr(X=1)=0.50}{Pr(X
= 0)=0.50}=1](https://latex.codecogs.com/png.latex?w%20%3D%20%5Cfrac%7BPr%28X%3D1%29%3D0.50%7D%7BPr%28X%20%3D%200%29%3D0.50%7D%3D1
"w = \\frac{Pr(X=1)=0.50}{Pr(X = 0)=0.50}=1"). If so, our measure of
bias is zero: given
![w=e^b](https://latex.codecogs.com/png.latex?w%3De%5Eb "w=e^b"), then
![e^b=1](https://latex.codecogs.com/png.latex?e%5Eb%3D1 "e^b=1") and
![b=log(1)=0](https://latex.codecogs.com/png.latex?b%3Dlog%281%29%3D0
"b=log(1)=0"). If ![b\>1](https://latex.codecogs.com/png.latex?b%3E1
"b\>1"), we infer that the the party has bias favoring a strike against
a juror falling within the cognizable class
(![X=1](https://latex.codecogs.com/png.latex?X%3D1 "X=1"), e.g., the
juror is a racial minority). Where
![b\<1](https://latex.codecogs.com/png.latex?b%3C1 "b\<1"), the party
has bias toward striking a juror falling outside the cognizable class
(![X=0](https://latex.codecogs.com/png.latex?X%3D0 "X=0"), e.g., the
juror is White).

In turn, let ![c](https://latex.codecogs.com/png.latex?c "c") denote the
number of cognizable class members; and
![m](https://latex.codecogs.com/png.latex?m "m") denote the number of
cognizable class non-members, such that
![c+m](https://latex.codecogs.com/png.latex?c%2Bm "c+m") is the total
number of jurors potentially subject to strike.

  
![&#10;Pr(X\_{ij}) =&#10;\\begin{cases}&#10;
\\frac{(e^b)c\_{ij}}{(e^b)c\_{ij}+m\_{ij}} & \\text{for }X=1\\\\ &#10;
\\frac{m\_{ij}}{(e^b)c\_{ij}+m\_{ij}} & \\text{for }X=0
&#10;\\end{cases}&#10;](https://latex.codecogs.com/png.latex?%0APr%28X_%7Bij%7D%29%20%3D%0A%5Cbegin%7Bcases%7D%0A%20%20%5Cfrac%7B%28e%5Eb%29c_%7Bij%7D%7D%7B%28e%5Eb%29c_%7Bij%7D%2Bm_%7Bij%7D%7D%20%26%20%5Ctext%7Bfor%20%7DX%3D1%5C%5C%20%20%20%20%0A%20%20%5Cfrac%7Bm_%7Bij%7D%7D%7B%28e%5Eb%29c_%7Bij%7D%2Bm_%7Bij%7D%7D%20%26%20%5Ctext%7Bfor%20%7DX%3D0%20%20%0A%5Cend%7Bcases%7D%0A
"
Pr(X_{ij}) =
\\begin{cases}
  \\frac{(e^b)c_{ij}}{(e^b)c_{ij}+m_{ij}} & \\text{for }X=1\\\\    
  \\frac{m_{ij}}{(e^b)c_{ij}+m_{ij}} & \\text{for }X=0  
\\end{cases}
")  

The likelihood function of b is   
![L(b|X) = \\prod\_{i=1}^{n\_i}
\\prod\_{j=1}^{n\_j}(\\frac{(e^b)c\_{ij}}{(e^b)c\_{ij}+m\_{ij}})^{\\delta\_{ij}}
(\\frac{m\_{ij}}{(e^b)c\_{ij}+m\_{ij}})^{1-\\delta\_{ij}}](https://latex.codecogs.com/png.latex?L%28b%7CX%29%20%3D%20%5Cprod_%7Bi%3D1%7D%5E%7Bn_i%7D%20%5Cprod_%7Bj%3D1%7D%5E%7Bn_j%7D%28%5Cfrac%7B%28e%5Eb%29c_%7Bij%7D%7D%7B%28e%5Eb%29c_%7Bij%7D%2Bm_%7Bij%7D%7D%29%5E%7B%5Cdelta_%7Bij%7D%7D%20%28%5Cfrac%7Bm_%7Bij%7D%7D%7B%28e%5Eb%29c_%7Bij%7D%2Bm_%7Bij%7D%7D%29%5E%7B1-%5Cdelta_%7Bij%7D%7D
"L(b|X) = \\prod_{i=1}^{n_i} \\prod_{j=1}^{n_j}(\\frac{(e^b)c_{ij}}{(e^b)c_{ij}+m_{ij}})^{\\delta_{ij}} (\\frac{m_{ij}}{(e^b)c_{ij}+m_{ij}})^{1-\\delta_{ij}}")  
where ![n\_{i}](https://latex.codecogs.com/png.latex?n_%7Bi%7D "n_{i}")
is the total number of jury selections and
![n\_j](https://latex.codecogs.com/png.latex?n_j "n_j") is the total
number of peremptory strikes; and where ![\\delta\_{ij}
= 1](https://latex.codecogs.com/png.latex?%5Cdelta_%7Bij%7D%20%3D%201
"\\delta_{ij} = 1") when ![X\_{ij}
= 1](https://latex.codecogs.com/png.latex?X_%7Bij%7D%20%3D%201
"X_{ij} = 1") and ![\\delta\_{ij}
= 0](https://latex.codecogs.com/png.latex?%5Cdelta_%7Bij%7D%20%3D%200
"\\delta_{ij} = 0") when
![X\_{ij}=0](https://latex.codecogs.com/png.latex?X_%7Bij%7D%3D0
"X_{ij}=0").

The posterior probability distribution is estimated with R 4.0 (R Core
Team 2020) and mcmc 0.9-7 (Geyer and Johnson 2020).

### References

<div id="refs" class="references">

<div id="ref-Barrett2007">

Barrett, Bruce E. 2007. “Detecting Bias in Jury Selection.” *The
American Statistician* 61 (4): 296–301.
<https://doi.org/10.1198/000313007X243629>.

</div>

<div id="ref-mcmc">

Geyer, Charles J., and Leif T. Johnson. 2020. *Mcmc: Markov Chain Monte
Carlo*. <https://CRAN.R-project.org/package=mcmc>.

</div>

<div id="ref-Kadane2018">

Kadane, Joseph B. 2018. “Statistics for Batson Challenges.” *Law,
Probability and Risk* 17 (1): 1–13.
<https://doi.org/10.1093/lpr/mgx016>.

</div>

<div id="ref-R">

R Core Team. 2020. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

</div>
