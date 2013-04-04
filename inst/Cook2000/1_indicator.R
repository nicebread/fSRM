ind1 <- "
### lavaan syntax for family SRM
### ROLES:'c','f','m','y'
### VARID:'dep1'
 # Family effect:
FE =~ 1*c_f_dep1 + 1*c_m_dep1 + 1*c_y_dep1 + 1*f_c_dep1 + 1*f_m_dep1 + 1*f_y_dep1 + 1*m_c_dep1 + 1*m_f_dep1 + 1*m_y_dep1 + 1*y_c_dep1 + 1*y_f_dep1 + 1*y_m_dep1

# Actor effects:
Ac =~ 1*c_f_dep1 + 1*c_m_dep1 + 1*c_y_dep1
Af =~ 1*f_c_dep1 + 1*f_m_dep1 + 1*f_y_dep1
Am =~ 1*m_c_dep1 + 1*m_f_dep1 + 1*m_y_dep1
Ay =~ 1*y_c_dep1 + 1*y_f_dep1 + 1*y_m_dep1

# Partner effects:
Pc =~ 1*f_c_dep1 + 1*m_c_dep1 + 1*y_c_dep1
Pf =~ 1*c_f_dep1 + 1*m_f_dep1 + 1*y_f_dep1
Pm =~ 1*c_m_dep1 + 1*f_m_dep1 + 1*y_m_dep1
Py =~ 1*c_y_dep1 + 1*f_y_dep1 + 1*m_y_dep1

# Relationship effects:
Rcf =~ 1* c_f_dep1
Rcm =~ 1* c_m_dep1
Rcy =~ 1* c_y_dep1
Rfc =~ 1* f_c_dep1
Rfm =~ 1* f_m_dep1
Rfy =~ 1* f_y_dep1
Rmc =~ 1* m_c_dep1
Rmf =~ 1* m_f_dep1
Rmy =~ 1* m_y_dep1
Ryc =~ 1* y_c_dep1
Ryf =~ 1* y_f_dep1
Rym =~ 1* y_m_dep1



c_f_dep1 ~~ 0*c_f_dep1
c_m_dep1 ~~ 0*c_m_dep1
c_y_dep1 ~~ 0*c_y_dep1
f_c_dep1 ~~ 0*f_c_dep1
f_m_dep1 ~~ 0*f_m_dep1
f_y_dep1 ~~ 0*f_y_dep1
m_c_dep1 ~~ 0*m_c_dep1
m_f_dep1 ~~ 0*m_f_dep1
m_y_dep1 ~~ 0*m_y_dep1
y_c_dep1 ~~ 0*y_c_dep1
y_f_dep1 ~~ 0*y_f_dep1
y_m_dep1 ~~ 0*y_m_dep1



# Method covariance: Correlations among error terms:
#c_f_dep1 ~~ MF1*c_m_dep1
#c_f_dep1 ~~ MF2*c_y_dep1
#c_f_dep1 ~~ MF3*f_c_dep1
#c_f_dep1 ~~ MF4*f_m_dep1
#c_f_dep1 ~~ MF5*f_y_dep1
#c_f_dep1 ~~ MF6*m_c_dep1
#c_f_dep1 ~~ MF7*m_f_dep1
#c_f_dep1 ~~ MF8*m_y_dep1
#c_f_dep1 ~~ MF9*y_c_dep1
#c_f_dep1 ~~ MF10*y_f_dep1
#c_f_dep1 ~~ MF11*y_m_dep1
#c_m_dep1 ~~ MF12*c_y_dep1
#c_m_dep1 ~~ MF13*f_c_dep1
#c_m_dep1 ~~ MF14*f_m_dep1
#c_m_dep1 ~~ MF15*f_y_dep1
#c_m_dep1 ~~ MF16*m_c_dep1
#c_m_dep1 ~~ MF17*m_f_dep1
#c_m_dep1 ~~ MF18*m_y_dep1
#c_m_dep1 ~~ MF19*y_c_dep1
#c_m_dep1 ~~ MF20*y_f_dep1
#c_m_dep1 ~~ MF21*y_m_dep1
#c_y_dep1 ~~ MF22*f_c_dep1
#c_y_dep1 ~~ MF23*f_m_dep1
#c_y_dep1 ~~ MF24*f_y_dep1
#c_y_dep1 ~~ MF25*m_c_dep1
#c_y_dep1 ~~ MF26*m_f_dep1
#c_y_dep1 ~~ MF27*m_y_dep1
#c_y_dep1 ~~ MF28*y_c_dep1
#c_y_dep1 ~~ MF29*y_f_dep1
#c_y_dep1 ~~ MF30*y_m_dep1
#f_c_dep1 ~~ MF31*f_m_dep1
#f_c_dep1 ~~ MF32*f_y_dep1
#f_c_dep1 ~~ MF33*m_c_dep1
#f_c_dep1 ~~ MF34*m_f_dep1
#f_c_dep1 ~~ MF35*m_y_dep1
#f_c_dep1 ~~ MF36*y_c_dep1
#f_c_dep1 ~~ MF37*y_f_dep1
#f_c_dep1 ~~ MF38*y_m_dep1
#f_m_dep1 ~~ MF39*f_y_dep1
#f_m_dep1 ~~ MF40*m_c_dep1
#f_m_dep1 ~~ MF41*m_f_dep1
#f_m_dep1 ~~ MF42*m_y_dep1
#f_m_dep1 ~~ MF43*y_c_dep1
#f_m_dep1 ~~ MF44*y_f_dep1
#f_m_dep1 ~~ MF45*y_m_dep1
#f_y_dep1 ~~ MF46*m_c_dep1
#f_y_dep1 ~~ MF47*m_f_dep1
#f_y_dep1 ~~ MF48*m_y_dep1
#f_y_dep1 ~~ MF49*y_c_dep1
#f_y_dep1 ~~ MF50*y_f_dep1
#f_y_dep1 ~~ MF51*y_m_dep1
#m_c_dep1 ~~ MF52*m_f_dep1
#m_c_dep1 ~~ MF53*m_y_dep1
#m_c_dep1 ~~ MF54*y_c_dep1
#m_c_dep1 ~~ MF55*y_f_dep1
#m_c_dep1 ~~ MF56*y_m_dep1
#m_f_dep1 ~~ MF57*m_y_dep1
#m_f_dep1 ~~ MF58*y_c_dep1
#m_f_dep1 ~~ MF59*y_f_dep1
#m_f_dep1 ~~ MF60*y_m_dep1
#m_y_dep1 ~~ MF61*y_c_dep1
#m_y_dep1 ~~ MF62*y_f_dep1
#m_y_dep1 ~~ MF63*y_m_dep1
#y_c_dep1 ~~ MF64*y_f_dep1
#y_c_dep1 ~~ MF65*y_m_dep1
#y_f_dep1 ~~ MF66*y_m_dep1

# Generalized reciprocity:
Ac ~~ Pc
Af ~~ Pf
Am ~~ Pm
Ay ~~ Py

# Dyadic reciprocity:
Rcf ~~ Rfc
Rcm ~~ Rmc
Rcy ~~ Ryc
Rfm ~~ Rmf
Rfy ~~ Ryf
Rmy ~~ Rym
"