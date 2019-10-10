# qvb
A package for the generalized q-VBGF

## Generalized _q_-VBGF

### function

### Weight-based _q_-VBGF
** Growth function **
<img src="https://latex.codecogs.com/gif.latex?\large&space;w_t&space;=&space;\widehat{w}\tau^{r}[1-[\textrm{max}(0,1-(1-q)(t-t_0)/\tau)]^{\frac{1}{1-q}}]^{r}" title="\large w_t = \widehat{w}\tau^{r}[1-[\textrm{max}(0,1-(1-q)(t-t_0)/\tau)]^{\frac{1}{1-q}}]^{r}" />

** Reproductive investment function **
<img src="https://latex.codecogs.com/gif.latex?\large&space;\frac{dF}{dt}&space;=&space;wr(\frac{w}{\widehat{w}})^{-\frac{1}{r}}[1-[1-(\frac{1}{\tau})(\frac{w}{\widehat{w}})^{\frac{1}{r}}]^{q}]" title="\large \frac{dF}{dt} = wr(\frac{w}{\widehat{w}})^{-\frac{1}{r}}[1-[1-(\frac{1}{\tau})(\frac{w}{\widehat{w}})^{\frac{1}{r}}]^{q}]" />

### Length-based _q_-VBGF
** Growth function **
<img src="https://latex.codecogs.com/gif.latex?\large&space;L_t&space;=&space;\widehat{L}\tau^{r'}[1-[\textrm{max}(0,1-(1-q)(t-t_0)/\tau)]^{\frac{1}{1-q}}]^{r'}" title="\large L_t = \widehat{L}\tau^{r'}[1-[\textrm{max}(0,1-(1-q)(t-t_0)/\tau)]^{\frac{1}{1-q}}]^{r'}" />

** Reproductive investment function **
<img src="https://latex.codecogs.com/gif.latex?\large&space;\frac{dF}{dt}&space;=&space;aL^{b}br'(\frac{L}{\widehat{L}})^{-\frac{1}{r'}}[1-[1-(\frac{1}{\tau})(\frac{L}{\widehat{L}})^{\frac{1}{r'}}]^{q}]" title="\large \frac{dF}{dt} = aL^{b}br'(\frac{L}{\widehat{L}})^{-\frac{1}{r'}}[1-[1-(\frac{1}{\tau})(\frac{L}{\widehat{L}})^{\frac{1}{r'}}]^{q}]" />

### Parameters
| parameters | definition |
|---|---|
|<img src="https://latex.codecogs.com/gif.latex?\inline&space;\large&space;\widehat{w}" title="\large \widehat{w}" />| Growth scale factor (weight)|
|<img src="https://latex.codecogs.com/gif.latex?\inline&space;\large&space;\widehat{L}" title="\large \widehat{w}" />| Growth scale factor (length)|
|_r_| Growth exponent (weight)|
|_r'_| Growth exponent (length)|
|_q_| Growth indeterminacy exponent |
|<img src="https://latex.codecogs.com/gif.latex?\inline&space;\large&space;\widehat{\tau}" title="\large \widehat{\tau}" />| Maturation timing parameter |
|_t_0_ | Theoretical age at size zero|
