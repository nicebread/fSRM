pow3r.model <- "### lavaan syntax for family SRM
### ROLES:'Mother','Father','Older','Younger'
### VARID:'RR_UMS1_2','RR_UMS2_1'
 # Family effect:
FE =~ 1*Mother_Father_RR_UMS1_2 + 1*Mother_Older_RR_UMS1_2 + 1*Mother_Younger_RR_UMS1_2 + 1*Father_Mother_RR_UMS1_2 + 1*Father_Older_RR_UMS1_2 + 1*Father_Younger_RR_UMS1_2 + 1*Older_Mother_RR_UMS1_2 + 1*Older_Father_RR_UMS1_2 + 1*Older_Younger_RR_UMS1_2 + 1*Younger_Mother_RR_UMS1_2 + 1*Younger_Father_RR_UMS1_2 + 1*Younger_Older_RR_UMS1_2 + 1*Mother_Father_RR_UMS2_1 + 1*Mother_Older_RR_UMS2_1 + 1*Mother_Younger_RR_UMS2_1 + 1*Father_Mother_RR_UMS2_1 + 1*Father_Older_RR_UMS2_1 + 1*Father_Younger_RR_UMS2_1 + 1*Older_Mother_RR_UMS2_1 + 1*Older_Father_RR_UMS2_1 + 1*Older_Younger_RR_UMS2_1 + 1*Younger_Mother_RR_UMS2_1 + 1*Younger_Father_RR_UMS2_1 + 1*Younger_Older_RR_UMS2_1

# Actor effects:
AMother =~ 1*Mother_Father_RR_UMS1_2 + 1*Mother_Older_RR_UMS1_2 + 1*Mother_Younger_RR_UMS1_2 + 1*Mother_Father_RR_UMS2_1 + 1*Mother_Older_RR_UMS2_1 + 1*Mother_Younger_RR_UMS2_1
AFather =~ 1*Father_Mother_RR_UMS1_2 + 1*Father_Older_RR_UMS1_2 + 1*Father_Younger_RR_UMS1_2 + 1*Father_Mother_RR_UMS2_1 + 1*Father_Older_RR_UMS2_1 + 1*Father_Younger_RR_UMS2_1
AOlder =~ 1*Older_Mother_RR_UMS1_2 + 1*Older_Father_RR_UMS1_2 + 1*Older_Younger_RR_UMS1_2 + 1*Older_Mother_RR_UMS2_1 + 1*Older_Father_RR_UMS2_1 + 1*Older_Younger_RR_UMS2_1
AYounger =~ 1*Younger_Mother_RR_UMS1_2 + 1*Younger_Father_RR_UMS1_2 + 1*Younger_Older_RR_UMS1_2 + 1*Younger_Mother_RR_UMS2_1 + 1*Younger_Father_RR_UMS2_1 + 1*Younger_Older_RR_UMS2_1

# Partner effects:
PMother =~ 1*Father_Mother_RR_UMS1_2 + 1*Older_Mother_RR_UMS1_2 + 1*Younger_Mother_RR_UMS1_2 + 1*Father_Mother_RR_UMS2_1 + 1*Older_Mother_RR_UMS2_1 + 1*Younger_Mother_RR_UMS2_1
PFather =~ 1*Mother_Father_RR_UMS1_2 + 1*Older_Father_RR_UMS1_2 + 1*Younger_Father_RR_UMS1_2 + 1*Mother_Father_RR_UMS2_1 + 1*Older_Father_RR_UMS2_1 + 1*Younger_Father_RR_UMS2_1
POlder =~ 1*Mother_Older_RR_UMS1_2 + 1*Father_Older_RR_UMS1_2 + 1*Younger_Older_RR_UMS1_2 + 1*Mother_Older_RR_UMS2_1 + 1*Father_Older_RR_UMS2_1 + 1*Younger_Older_RR_UMS2_1
PYounger =~ 1*Mother_Younger_RR_UMS1_2 + 1*Father_Younger_RR_UMS1_2 + 1*Older_Younger_RR_UMS1_2 + 1*Mother_Younger_RR_UMS2_1 + 1*Father_Younger_RR_UMS2_1 + 1*Older_Younger_RR_UMS2_1

# Relationship effects:
RMF =~ 1* Mother_Father_RR_UMS1_2 + 1* Mother_Father_RR_UMS2_1
RMO =~ 1* Mother_Older_RR_UMS1_2 + 1* Mother_Older_RR_UMS2_1
RMY =~ 1* Mother_Younger_RR_UMS1_2 + 1* Mother_Younger_RR_UMS2_1
RFM =~ 1* Father_Mother_RR_UMS1_2 + 1* Father_Mother_RR_UMS2_1
RFO =~ 1* Father_Older_RR_UMS1_2 + 1* Father_Older_RR_UMS2_1
RFY =~ 1* Father_Younger_RR_UMS1_2 + 1* Father_Younger_RR_UMS2_1
ROM =~ 1* Older_Mother_RR_UMS1_2 + 1* Older_Mother_RR_UMS2_1
ROF =~ 1* Older_Father_RR_UMS1_2 + 1* Older_Father_RR_UMS2_1
ROY =~ 1* Older_Younger_RR_UMS1_2 + 1* Older_Younger_RR_UMS2_1
RYM =~ 1* Younger_Mother_RR_UMS1_2 + 1* Younger_Mother_RR_UMS2_1
RYF =~ 1* Younger_Father_RR_UMS1_2 + 1* Younger_Father_RR_UMS2_1
RYO =~ 1* Younger_Older_RR_UMS1_2 + 1* Younger_Older_RR_UMS2_1

# Method covariance: Correlations among error terms:
Mother_Father_RR_UMS1_2 ~~ MF1*Mother_Older_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF2*Mother_Younger_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF3*Father_Mother_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF4*Father_Older_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF5*Father_Younger_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF6*Older_Mother_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF7*Older_Father_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF8*Older_Younger_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF9*Younger_Mother_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF10*Younger_Father_RR_UMS1_2
Mother_Father_RR_UMS1_2 ~~ MF11*Younger_Older_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF12*Mother_Younger_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF13*Father_Mother_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF14*Father_Older_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF15*Father_Younger_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF16*Older_Mother_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF17*Older_Father_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF18*Older_Younger_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF19*Younger_Mother_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF20*Younger_Father_RR_UMS1_2
Mother_Older_RR_UMS1_2 ~~ MF21*Younger_Older_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF22*Father_Mother_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF23*Father_Older_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF24*Father_Younger_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF25*Older_Mother_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF26*Older_Father_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF27*Older_Younger_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF28*Younger_Mother_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF29*Younger_Father_RR_UMS1_2
Mother_Younger_RR_UMS1_2 ~~ MF30*Younger_Older_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF31*Father_Older_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF32*Father_Younger_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF33*Older_Mother_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF34*Older_Father_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF35*Older_Younger_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF36*Younger_Mother_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF37*Younger_Father_RR_UMS1_2
Father_Mother_RR_UMS1_2 ~~ MF38*Younger_Older_RR_UMS1_2
Father_Older_RR_UMS1_2 ~~ MF39*Father_Younger_RR_UMS1_2
Father_Older_RR_UMS1_2 ~~ MF40*Older_Mother_RR_UMS1_2
Father_Older_RR_UMS1_2 ~~ MF41*Older_Father_RR_UMS1_2
Father_Older_RR_UMS1_2 ~~ MF42*Older_Younger_RR_UMS1_2
Father_Older_RR_UMS1_2 ~~ MF43*Younger_Mother_RR_UMS1_2
Father_Older_RR_UMS1_2 ~~ MF44*Younger_Father_RR_UMS1_2
Father_Older_RR_UMS1_2 ~~ MF45*Younger_Older_RR_UMS1_2
Father_Younger_RR_UMS1_2 ~~ MF46*Older_Mother_RR_UMS1_2
Father_Younger_RR_UMS1_2 ~~ MF47*Older_Father_RR_UMS1_2
Father_Younger_RR_UMS1_2 ~~ MF48*Older_Younger_RR_UMS1_2
Father_Younger_RR_UMS1_2 ~~ MF49*Younger_Mother_RR_UMS1_2
Father_Younger_RR_UMS1_2 ~~ MF50*Younger_Father_RR_UMS1_2
Father_Younger_RR_UMS1_2 ~~ MF51*Younger_Older_RR_UMS1_2
Older_Mother_RR_UMS1_2 ~~ MF52*Older_Father_RR_UMS1_2
Older_Mother_RR_UMS1_2 ~~ MF53*Older_Younger_RR_UMS1_2
Older_Mother_RR_UMS1_2 ~~ MF54*Younger_Mother_RR_UMS1_2
Older_Mother_RR_UMS1_2 ~~ MF55*Younger_Father_RR_UMS1_2
Older_Mother_RR_UMS1_2 ~~ MF56*Younger_Older_RR_UMS1_2
Older_Father_RR_UMS1_2 ~~ MF57*Older_Younger_RR_UMS1_2
Older_Father_RR_UMS1_2 ~~ MF58*Younger_Mother_RR_UMS1_2
Older_Father_RR_UMS1_2 ~~ MF59*Younger_Father_RR_UMS1_2
Older_Father_RR_UMS1_2 ~~ MF60*Younger_Older_RR_UMS1_2
Older_Younger_RR_UMS1_2 ~~ MF61*Younger_Mother_RR_UMS1_2
Older_Younger_RR_UMS1_2 ~~ MF62*Younger_Father_RR_UMS1_2
Older_Younger_RR_UMS1_2 ~~ MF63*Younger_Older_RR_UMS1_2
Younger_Mother_RR_UMS1_2 ~~ MF64*Younger_Father_RR_UMS1_2
Younger_Mother_RR_UMS1_2 ~~ MF65*Younger_Older_RR_UMS1_2
Younger_Father_RR_UMS1_2 ~~ MF66*Younger_Older_RR_UMS1_2
Mother_Father_RR_UMS2_1 ~~ MF67*Mother_Older_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF68*Mother_Younger_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF69*Father_Mother_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF70*Father_Older_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF71*Father_Younger_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF72*Older_Mother_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF73*Older_Father_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF74*Older_Younger_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF75*Younger_Mother_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF76*Younger_Father_RR_UMS2_1
Mother_Father_RR_UMS2_1 ~~ MF77*Younger_Older_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF78*Mother_Younger_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF79*Father_Mother_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF80*Father_Older_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF81*Father_Younger_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF82*Older_Mother_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF83*Older_Father_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF84*Older_Younger_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF85*Younger_Mother_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF86*Younger_Father_RR_UMS2_1
Mother_Older_RR_UMS2_1 ~~ MF87*Younger_Older_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF88*Father_Mother_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF89*Father_Older_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF90*Father_Younger_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF91*Older_Mother_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF92*Older_Father_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF93*Older_Younger_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF94*Younger_Mother_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF95*Younger_Father_RR_UMS2_1
Mother_Younger_RR_UMS2_1 ~~ MF96*Younger_Older_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF97*Father_Older_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF98*Father_Younger_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF99*Older_Mother_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF100*Older_Father_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF101*Older_Younger_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF102*Younger_Mother_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF103*Younger_Father_RR_UMS2_1
Father_Mother_RR_UMS2_1 ~~ MF104*Younger_Older_RR_UMS2_1
Father_Older_RR_UMS2_1 ~~ MF105*Father_Younger_RR_UMS2_1
Father_Older_RR_UMS2_1 ~~ MF106*Older_Mother_RR_UMS2_1
Father_Older_RR_UMS2_1 ~~ MF107*Older_Father_RR_UMS2_1
Father_Older_RR_UMS2_1 ~~ MF108*Older_Younger_RR_UMS2_1
Father_Older_RR_UMS2_1 ~~ MF109*Younger_Mother_RR_UMS2_1
Father_Older_RR_UMS2_1 ~~ MF110*Younger_Father_RR_UMS2_1
Father_Older_RR_UMS2_1 ~~ MF111*Younger_Older_RR_UMS2_1
Father_Younger_RR_UMS2_1 ~~ MF112*Older_Mother_RR_UMS2_1
Father_Younger_RR_UMS2_1 ~~ MF113*Older_Father_RR_UMS2_1
Father_Younger_RR_UMS2_1 ~~ MF114*Older_Younger_RR_UMS2_1
Father_Younger_RR_UMS2_1 ~~ MF115*Younger_Mother_RR_UMS2_1
Father_Younger_RR_UMS2_1 ~~ MF116*Younger_Father_RR_UMS2_1
Father_Younger_RR_UMS2_1 ~~ MF117*Younger_Older_RR_UMS2_1
Older_Mother_RR_UMS2_1 ~~ MF118*Older_Father_RR_UMS2_1
Older_Mother_RR_UMS2_1 ~~ MF119*Older_Younger_RR_UMS2_1
Older_Mother_RR_UMS2_1 ~~ MF120*Younger_Mother_RR_UMS2_1
Older_Mother_RR_UMS2_1 ~~ MF121*Younger_Father_RR_UMS2_1
Older_Mother_RR_UMS2_1 ~~ MF122*Younger_Older_RR_UMS2_1
Older_Father_RR_UMS2_1 ~~ MF123*Older_Younger_RR_UMS2_1
Older_Father_RR_UMS2_1 ~~ MF124*Younger_Mother_RR_UMS2_1
Older_Father_RR_UMS2_1 ~~ MF125*Younger_Father_RR_UMS2_1
Older_Father_RR_UMS2_1 ~~ MF126*Younger_Older_RR_UMS2_1
Older_Younger_RR_UMS2_1 ~~ MF127*Younger_Mother_RR_UMS2_1
Older_Younger_RR_UMS2_1 ~~ MF128*Younger_Father_RR_UMS2_1
Older_Younger_RR_UMS2_1 ~~ MF129*Younger_Older_RR_UMS2_1
Younger_Mother_RR_UMS2_1 ~~ MF130*Younger_Father_RR_UMS2_1
Younger_Mother_RR_UMS2_1 ~~ MF131*Younger_Older_RR_UMS2_1
Younger_Father_RR_UMS2_1 ~~ MF132*Younger_Older_RR_UMS2_1

# Generalized reciprocity:
AMother ~~ PMother
AFather ~~ 0*PFather
AOlder ~~ POlder
AYounger ~~ PYounger

# Dyadic reciprocity:
RMF ~~ RFM
RMO ~~ ROM
RMY ~~ RYM
RFO ~~ ROF
RFY ~~ RYF
ROY ~~ 0*RYO

# Equality constraints

# intergenerational similarity:
AMother ~~ 0*AFather
PMother ~~ IGSIMP1*PFather
AOlder ~~ IGSIMA2*AYounger
POlder ~~ IGSIMP2*PYounger

 
## Reestimation adjustments:
 AFather ~~ 0*AFather 
 RYO ~~ 0*RYO"