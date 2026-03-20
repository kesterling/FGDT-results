


use "C:\Users\kevin\Dropbox\TeCD Lab\Prytaneum\Focus Group Methods\read\LLM simulated responses\Experiments\Scripts\Kevin's Copy\Results\trial_id_4260266019\michael_validation_1.dta"


gen g_gender=gender=="Female"
gen m_gender=mgender=="female"

gen g_education=education=="Postgraduate"
replace g_education=1 if education=="College graduate/some postgrad"
gen m_education=meducation=="college"

gen m_pgreen=mpgreen=="not"
replace m_pgreen=1-m_pgreen

gen g_politics=politics=="Very conservative"
replace g_politics=1 if politics=="Conservative"
gen m_politics=mpolitics=="liberal"
replace m_politics=1-m_politics

tab standpointnum mstandpointnum, col chi
tab g_gender m_gender, col chi
tab g_education m_education, col chi
tab g_politics m_politics, col chi
reg pgreen m_pgreen


. tab standpointnum mstandpointnum, col chi

+-------------------+
| Key               |
|-------------------|
|     frequency     |
| column percentage |
+-------------------+

standpoint |          mstandpointNum
       Num |         1          2          3 |     Total
-----------+---------------------------------+----------
         1 |        35          1          0 |        36 
           |     97.22       5.56       0.00 |     60.00 
-----------+---------------------------------+----------
         2 |         0         17          0 |        17 
           |      0.00      94.44       0.00 |     28.33 
-----------+---------------------------------+----------
         3 |         1          0          6 |         7 
           |      2.78       0.00     100.00 |     11.67 
-----------+---------------------------------+----------
     Total |        36         18          6 |        60 
           |    100.00     100.00     100.00 |    100.00 

          Pearson chi2(4) = 105.1389   Pr = 0.000

. tab g_gender m_gender, col chi

+-------------------+
| Key               |
|-------------------|
|     frequency     |
| column percentage |
+-------------------+

female precision = 0.67
male precision = 0.60
female recall = 0.41
male recall = 0.81
female f1 = 0.51
male f1 = 0.69



           |       m_gender
  g_gender |         0          1 |     Total
-----------+----------------------+----------
         0 |        25          6 |        31 
           |     59.52      33.33 |     51.67 
-----------+----------------------+----------
         1 |        17         12 |        29 
           |     40.48      66.67 |     48.33 
-----------+----------------------+----------
     Total |        42         18 |        60 
           |    100.00     100.00 |    100.00 

          Pearson chi2(1) =   3.4610   Pr = 0.063

. tab g_education m_education, col chi

+-------------------+
| Key               |
|-------------------|
|     frequency     |
| column percentage |
+-------------------+

g_educatio |      m_education
         n |         0          1 |     Total
-----------+----------------------+----------
         0 |        12         13 |        25 
           |     63.16      31.71 |     41.67 
-----------+----------------------+----------
         1 |         7         28 |        35 
           |     36.84      68.29 |     58.33 
-----------+----------------------+----------
     Total |        19         41 |        60 
           |    100.00     100.00 |    100.00 

          Pearson chi2(1) =   5.2837   Pr = 0.022

. tab g_politics m_politics, col chi

+-------------------+
| Key               |
|-------------------|
|     frequency     |
| column percentage |
+-------------------+

           |      m_politics
g_politics |         0          1 |     Total
-----------+----------------------+----------
         0 |        23         20 |        43 
           |     85.19      60.61 |     71.67 
-----------+----------------------+----------
         1 |         4         13 |        17 
           |     14.81      39.39 |     28.33 
-----------+----------------------+----------
     Total |        27         33 |        60 
           |    100.00     100.00 |    100.00 

          Pearson chi2(1) =   4.4182   Pr = 0.036

. reg pgreen m_pgreen

      Source |       SS           df       MS      Number of obs   =        60
-------------+----------------------------------   F(1, 58)        =     15.62
       Model |  11314.3441         1  11314.3441   Prob > F        =    0.0002
    Residual |  42013.5892        58  724.372228   R-squared       =    0.2122
-------------+----------------------------------   Adj R-squared   =    0.1986
       Total |  53327.9333        59  903.863277   Root MSE        =    26.914

------------------------------------------------------------------------------
      pgreen | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
    m_pgreen |   27.60269   6.984216     3.95   0.000     13.62227    41.58312
       _cons |   37.54545   4.685155     8.01   0.000      28.1671    46.92381
------------------------------------------------------------------------------

