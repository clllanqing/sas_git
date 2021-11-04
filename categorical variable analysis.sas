
***********************************单因素分析*******************************************;
libname cao "D:\1a\learning\research\数据20210824\大肠癌筛查";
data b9;
set cao.analysis_data;
run;
proc means data=b9 p25 p50 p75 maxdec=2;
var waist hip whr whtr;
run;
data b9;
set b9;
if waist<75 then waistgr=1;
if 75<=waist<81 then waistgr=2;
if 81<=waist<88 then waistgr=3;
if waist>=88 then waistgr=4;
if hip<90 then hipgr=1;
if 90<=hip<94 then hipgr=2;
if 94<=hip<99 then hipgr=3;
if hip>=99 then hipgr=4;
if whr<0.82 then whrgr=1;
if 0.82<=whr<0.86 then whrgr=2;
if 0.86<=whr<0.91 then whrgr=3;
if whr>=0.91 then whrgr=4;
if whtr<0.47 then whtrgr=1;
if 0.47<=whtr<0.50 then whtrgr=2;
if 0.50<=whtr<0.54 then whtrgr=3;
if whtr>=0.54 then whtrgr=4;
run;
data b9;
set b9;
if whtrgr=1 then whtrgr1=1;
else whtrgr1=0;
if whtrgr=2 then whtrgr2=1;
else whtrgr2=0;
if whtrgr=3 then whtrgr3=1;
else whtrgr3=0;
if whtrgr=4 then whtrgr4=1;
else whtrgr4=0;
run;
proc phreg data=b9;
class  bmigr(ref="2") gender(ref='1');
MODEL d_py*d(0) =gender agegr bmigr  / RL; 
RUN;
*BMI有意义;
proc phreg data=b9;
class  education(ref='1') gender(ref='1');
MODEL d_py*d(0) =gender agegr education  / RL; 
RUN;
*教育水平有意义;
proc phreg data=b9;
class  gender(ref='1') quarterimcome(ref='1');
MODEL d_py*d(0) =gender agegr quarterimcome/ RL; 
RUN;
*季收入水平有意义;
proc phreg data=b9;
class  gender(ref='1') risk1(ref='1');
MODEL d_py*d(0) =gender agegr risk1/ RL; 
RUN;
*吸烟有意义;
proc phreg data=b9;
class  gender(ref='1') risk6(ref='1');
MODEL d_py*d(0) =gender agegr risk6/ RL; 
RUN;
*饮酒有意义;
proc phreg data=b9;
class  gender(ref='1') risk16(ref='2');
MODEL d_py*d(0) =gender agegr risk16/ RL; 
RUN;
*每周脂肪与肥肉的摄入无意义;
proc phreg data=b9;
class  gender(ref='1') risk17(ref='2');
MODEL d_py*d(0) =gender agegr risk17/ RL; 
RUN;
*烟熏油炸食品的摄入无意义;
proc phreg data=b9;
class  gender(ref='1') risk18(ref='2');
MODEL d_py*d(0) =gender agegr risk18/ RL; 
RUN;
*咸菜泡菜食品的摄入无意义;
proc phreg data=b9;
class  gender(ref='1') risk19(ref='2');
MODEL d_py*d(0) =gender agegr risk19/ RL; 
RUN;
*蔬菜水果的摄入有意义;
proc phreg data=b9;
class  gender(ref='1') dm(ref='0');
MODEL d_py*d(0) =gender agegr dm/ RL; 
RUN;
*糖尿病有意义;
proc phreg data=b9;
class  gender(ref='1') waistgr(ref='1');
MODEL d_py*d(0) =gender agegr waistgr/ RL; 
RUN;
*腰围有意义;
proc phreg data=b9;
class  gender(ref='1') hipgr(ref='1');
MODEL d_py*d(0) =gender agegr hipgr/ RL; 
RUN;
*臀围无意义;
proc phreg data=b9;
class  gender(ref='1') whtrgr(ref='1');
MODEL d_py*d(0) =gender agegr whtrgr/ RL; 
RUN;
*腰高比有意义;
proc phreg data=b9;
class  gender(ref='1') whrgr(ref='1');
MODEL d_py*d(0) =gender agegr whrgr/ RL; 
RUN;
*腰臀比有意义;
*单因素有意义的变量年龄、性别、BMI、教育水平、收入水平、吸烟、饮酒、蔬菜水果摄入、糖尿病;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') dm(ref='0') waistgr(ref='1') whrgr(ref='1') whtrgr(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr bmigr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm waistgr whtrgr  whrgr/selection=stepwise slstay=0.05 RL; 
RUN;
*保留的变量 gender agegr bmigr education quarterimcome risk1 risk6 risk19 dm  waist whr;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='2') agegr(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1') risk19(ref='2') dm(ref='0') whtrgr(ref='1') /ref=first;
MODEL d_py*d(0) =gender agegr bmigr education quarterimcome risk1 risk6 risk16 risk19  whtrgr dm/ RL; 
RUN;

proc phreg data=b9;
class bmigr(ref="2") education(ref="1") quarterimcome(ref="1");
model d_py*d(0)=bmigr agegr gender education quarterimcome risk1 risk6 risk19 dm  whtrgr
bmiat agegrat genderat  educationat quarterimcomeat  risk1at  risk6at  risk19at   dmat whtrgrat/rl;
bmiat=bmigr*log(d_py);
agegrat=agegr*log(d_py);
genderat=gender*log(d_py);
educationat=education*log(d_py);
quarterimcomeat=quarterimcome*log(d_py);
 risk1at= risk1*log(d_py);
 risk6at=risk6*log(d_py);
risk19at=risk19*log(d_py);
dmat=dm*log(d_py);
whtrgrat=whtrgr*log(d_py);
proportionality_test: test  bmiat,agegrat,genderat,educationat ,quarterimcomeat ,risk1at,risk6at,risk19at,dmat,whtrgrat;*检验是否符合ph假定;
run;
*ph-test: p=<.0001 模型不符合等比例，采用变系数模型;

proc means data=b9 min max p1 p5 p10 p50 p90 p95 p99 maxdec=2;
var d_py;
run;
 ********** with 3 knots located at; 
 ********** /*5%:8.48 10%:9.22 50%:11.11  90%:12.24  95%:12.34*/;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
MODEL d_py*d(0) =
  bmigr1  __1_1 __1_lin bmigr3 __2_1 __2_lin bmigr4 __3_1 __3_lin  
    education2 __5_1 __5_lin education3 __6_1 __6_lin education4 __7_1 __7_lin quarterimcome2 __8_1 __8_lin quarterimcome3 __9_1 __9_lin
risk1 __10_1 __10_lin risk6  __11_1 __11_lin risk19  __12_1 __12_lin  dm __14_1 __14_lin
gender1  __15_1 __15_lin  agegr __16_1 __16_lin  whtrgr2 __17_1 __17_lin whtrgr3 __18_1 __18_lin  whtrgr4 __19_1 __19_lin
  /RL;  
  __1_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__1_1=__1_1*bmigr1;
__1_lin=bmigr1*d_py;

__2_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__2_1=__2_1*bmigr3;
__2_lin=bmigr3*d_py;

__3_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__3_1=__3_1*bmigr4;
__3_lin=bmigr4*d_py;

__5_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__5_1=__5_1*education2;
__5_lin=education2*d_py;

__6_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__6_1=__6_1*education3;
__6_lin=education3*d_py;

__7_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__7_1=__7_1*education4;
__7_lin=education4*d_py;

__8_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__8_1=__8_1*quarterimcome2;
__8_lin=quarterimcome2*d_py;

__9_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__9_1=__9_1*quarterimcome3;
__9_lin=quarterimcome3*d_py;

__10_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__10_1=__10_1*risk1;
__10_lin=risk1*d_py;

__11_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__11_1=__11_1*risk6;
__11_lin=risk6*d_py;

__12_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__12_1=__12_1*risk19;
__12_lin=risk19*d_py;

__14_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__14_1=__14_1*dm;
__14_lin=dm*d_py;

__15_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__15_1=__15_1*gender1;
__15_lin=gender1*d_py;

__16_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__16_1=__16_1*agegr;
__16_lin=agegr*d_py;

__17_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__17_1=__17_1*whtrgr2;
__17_lin=whtrgr2*d_py;

__18_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__18_1=__18_1*whtrgr3;
__18_lin=whtrgr3*d_py;

__19_1=((d_py-9.22)**3)*(d_py>9.22) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.24-9.22)/(12.24-11.11) 
     +((d_py-12.24)**3)*(d_py>12.24) 
     *(11.11-9.22)/(12.24-11.11);
__19_1=__19_1*whtrgr4;
__19_lin=whtrgr4*d_py;
 EFFECT1: TEST  bmigr1, __1_LIN, __1_1;
 NONCON1: TEST  __1_LIN, __1_1;
 NONLIN1: TEST  __1_1;
*这个p<0.05表示这个变量对预后有意义;
  *这个p<0.05表示这个变量的HR随时间变化（即不满足PH假定）;
 *这个p<0.05表示这个变量的HR随时间变化是非线性的（即需要限制性立方样条来拟合），p>0.05表示这个变量的HR随时间变化是线性的，不需要样条，直接自变量乘以时间就够了;
 EFFECT2: TEST  bmigr3, __2_LIN, __2_1;
 NONCON2: TEST  __2_LIN, __2_1;
 NONLIN2: TEST  __2_1;

 EFFECT3: TEST  bmigr4, __3_LIN, __3_1;
 NONCON3: TEST  __3_LIN, __3_1;
 NONLIN3: TEST  __3_1;

 EFFECT5: TEST  education2, __5_LIN, __5_1;
 NONCON5: TEST  __5_LIN, __5_1;
 NONLIN5: TEST  __5_1;

 EFFECT6: TEST  education3, __6_LIN, __6_1;
 NONCON6: TEST  __6_LIN, __6_1;
 NONLIN6: TEST  __6_1;

 EFFECT7: TEST  education4, __7_LIN, __7_1;
 NONCON7: TEST  __7_LIN, __7_1;
 NONLIN7: TEST  __7_1;

 EFFECT8: TEST  quarterimcome2, __8_LIN, __8_1;
 NONCON8: TEST  __8_LIN, __8_1;
 NONLIN8: TEST  __8_1;

 EFFECT9: TEST  quarterimcome3, __9_LIN, __9_1;
 NONCON9: TEST  __9_LIN, __9_1;
 NONLIN9: TEST  __9_1;

 EFFECT10: TEST risk1, __10_LIN, __10_1;
 NONCON10: TEST  __10_LIN, __10_1;
 NONLIN10: TEST  __10_1;

 EFFECT11: TEST risk6, __11_LIN, __11_1;
 NONCON11: TEST  __11_LIN, __11_1;
 NONLIN11: TEST  __11_1;

 EFFECT12: TEST risk19, __12_LIN, __12_1;
 NONCON12: TEST  __12_LIN, __12_1;
 NONLIN12: TEST  __12_1;

 EFFECT14: TEST dm, __14_LIN, __14_1;
 NONCON14: TEST  __14_LIN, __14_1;
 NONLIN14: TEST  __14_1;

  EFFECT15: TEST gender1, __15_LIN, __15_1;
 NONCON15: TEST  __15_LIN, __15_1;
 NONLIN15: TEST  __15_1;

 EFFECT16: TEST agegr, __16_LIN, __16_1;
 NONCON16: TEST  __16_LIN, __16_1;
 NONLIN16: TEST  __16_1;

 EFFECT17: TEST whtrgr2, __17_LIN, __17_1;
 NONCON17: TEST  __17_LIN, __17_1;
 NONLIN17: TEST  __17_1;

 EFFECT18: TEST whtrgr3, __18_LIN, __18_1;
 NONCON18: TEST  __18_LIN, __18_1;
 NONLIN18: TEST  __18_1;

 EFFECT19: TEST whtrgr4, __19_LIN, __19_1;
 NONCON19: TEST  __19_LIN, __19_1;
 NONLIN19: TEST  __19_1;
run;
/*quarterimcome;risk6;dm;不符合PH假定*/
/*PROC PHREG DATA=b9 COVOUT OUTEST=__RCS;
class gender1(ref='0')  risk1(ref='1')  risk19(ref='2') agegr(ref='1');
MODEL  d_py*d(0) =gender  agegr  
  bmigr1  __1_lin  bmigr3  __2_lin bmigr4 __3_lin bmigr5 __4_lin
    education2  education3  education4  quarterimcome2  quarterimcome3 
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__1_lin=bmigr1*d_py;
__2_lin=bmigr3*d_py;
__3_lin=bmigr4*d_py;
__4_lin=bmigr5*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;

 contrast 'HR for bmi<=18.5 at year 1' bmigr1 1 __1_LIN 1/estimate=exp;
 contrast 'HR for bmi<=18.5 at year 5' bmigr1 1 __1_LIN 5/estimate=exp;
 contrast 'HR for bmi<=18.5 at year 10' bmigr1 1 __1_LIN 10/estimate=exp;
 contrast 'HR for 23<=bmi<=24.9 at year 1' bmigr3 1 __2_LIN 1/estimate=exp;
 contrast 'HR for 23<=bmi<=24.9 at year 5' bmigr3 1 __2_LIN 5/estimate=exp;
 contrast 'HR for 23<=bmi<=24.9 at year 10' bmigr3 1 __2_LIN 10/estimate=exp;
contrast 'HR for 25<=bmi<=29.9 at year 1' bmigr4 1 __3_LIN 1/estimate=exp;
 contrast 'HR for 25<=bmi<=29.9 at year 5' bmigr4 1 __3_LIN 5/estimate=exp;
 contrast 'HR for 25<=bmi<=29.9 at year 10' bmigr4 1 __3_LIN 10/estimate=exp;
contrast 'HR for bmi>=30 at year 1' bmigr5 1 __4_LIN 1/estimate=exp;
 contrast 'HR for bmi>=30 at year 5' bmigr5 1 __4_LIN 5/estimate=exp;
 contrast 'HR for bmi>=30 at year 10' bmigr5 1 __4_LIN 10/estimate=exp;

 EFFECT1: TEST  bmigr1, __1_LIN;
 NONCON1: TEST  __1_LIN;

 EFFECT2: TEST  bmigr3, __2_LIN;
 NONCON2: TEST  __2_LIN;

 EFFECT3: TEST  bmigr4, __3_LIN;
 NONCON3: TEST  __3_LIN;

 EFFECT4: TEST  bmigr5, __4_LIN;
 NONCON4: TEST  __4_LIN;

 EFFECT5: TEST  risk6, __11_LIN;
 NONCON5: TEST  __11_LIN;

EFFECT7: TEST  dm, __14_LIN;
 NONCON7: TEST  __14_LIN;

RUN;

/*bmi1
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=15;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ bmigr1 __1_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS1_1 VAR { F FE Z X };  APPEND;  CLOSE __RCS1_1; 
 QUIT; 
 /*bmi3
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=15;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ bmigr3 __2_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS2_1 VAR { F FE Z X };  APPEND;  CLOSE __RCS2_1; 
 QUIT; 
  /*bmi4
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=15;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ bmigr4 __3_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS3_1 VAR { F FE Z X };  APPEND;  CLOSE __RCS3_1; 
 QUIT; 
 /*bmi5
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=15;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ bmigr5 __4_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS4_1 VAR { F FE Z X };  APPEND;  CLOSE __RCS4_1; 
 QUIT; 
 proc sort data=__RCS1_1;
by x z;
run;
proc sort data=__RCS2_1;
by x z;
run;
proc sort data=__RCS3_1;
by x z;
run;
proc sort data=__RCS4_1;
by x z;
run;
proc transpose data=__rcs1_1 out=new1 prefix=z;
by x;
var fe;
id z;
run;
data new1;set new1;
rename z1=z1_1 z2=z2_1 z3=z3_1;
drop _NAME_;run;
run;
proc transpose data=__rcs2_1 out=new2 prefix=z;
by x;
var fe;
id z;
run;
data new2;set new2;
rename z1=z1_2 z2=z2_2 z3=z3_2;
drop _NAME_;run;
proc transpose data=__rcs3_1 out=new3 prefix=z;
by x;
var fe;
id z;
run;
data new3;set new3;
rename z1=z1_3 z2=z2_3 z3=z3_3;
drop _NAME_;run;
proc transpose data=__rcs4_1 out=new4 prefix=z;
by x;
var fe;
id z;
run;
data new4;set new4;
rename z1=z1_4 z2=z2_4 z3=z3_4;
drop _NAME_;run;
data bmiplot;merge new1 new2 new3 new4;by x;run;
/*Graph
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\bmi(All-cause mortality )"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of bmi' imagefmt=tiff ; 
proc sgplot data=bmiplot noautolegend;
refline 1 / lineattrs=(color=red pattern=dash) name='line' legendlabel='Normal weight(ref)';
band x=x upper=z3_1 lower=z2_1/fillattrs=(color=pink) transparency=0.7;
band x=x upper=z3_2 lower=z2_2/fillattrs=(color=gold) transparency=0.7;
band x=x upper=z3_3 lower=z2_3/fillattrs=(color=skyblue) transparency=0.7;
band x=x upper=z3_4 lower=z2_4/fillattrs=(color=plum) transparency=0.7;
series x=x y=z1_1/ lineattrs=(color=pink thickness=2 ) name='bmi1' legendlabel='Underweight';
series x=x y=z1_2 / lineattrs=(color=gold thickness=2 ) name='bmi3' legendlabel='Overweight';
series x=x y=z1_3/lineattrs=(color=skyblue thickness=2) name='bmi4' legendlabel='Class I obesity';
series x=x y=z1_4/lineattrs=(color=plum thickness=2) name='bmi5' legendlabel='Class II obesity';
xaxis label='Years from registry' values=(0.5 5 10 15) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI) ' values=( 0.5 1.0 1.5 2.0 2.5 3 3.5 4)valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'bmi1' 'bmi3' 'bmi4' 'bmi5'/ valueattrs=(size=10pt) location=outside position=bottom; 
title height=14pt 'Time-varying effect of BMI on All Cause Mortality' height=16pt justify=left 'A';  
run;
ods graphics off;*/



******************************糖尿病与全死因死亡*******************************;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2') bmigr(ref='2');
MODEL  d_py*d(0) =gender  age  
  bmigr  education2  education3  education4  quarterimcome2  __8_lin quarterimcome3 __9_lin
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;

contrast 'HR for dm=1  at year 1' dm 1 __14_LIN 1/estimate=exp;
 contrast 'HR for dm=1  at year 5' dm 1 __14_LIN 5/estimate=exp;
 contrast 'HR for dm=1 at year 10' dm 1 __14_LIN 10/estimate=exp;

EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT3: TEST  risk6, __11_LIN;
 NONCON3: TEST  __11_LIN;

EFFECT4: TEST  dm, __14_LIN;
 NONCON4: TEST  __14_LIN;

RUN;


/*dm*/
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=13;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ dm __14_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS_dm_all VAR { F FE Z X };  APPEND;  CLOSE __RCS_dm; 
 QUIT; 

 proc sort data=__RCS_dm_all;
by x z;
run;

proc transpose data=__rcs_dm_all out=new_dm_all prefix=z;
by x;
var fe;
id z;
run;

data new_dm_all;set new_dm_all;
rename z1=z1_dm_all z2=z2_dm_all z3=z3_dm_all;
drop _NAME_;run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\bmi(All-cause mortality )"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of dm' imagefmt=tiff ; 
proc sgplot data=new_dm_all noautolegend;
refline 1 / lineattrs=(color=red pattern=dash) name='line' legendlabel='no diabetes(ref)';
band x=x upper=z3_dm_all lower=z2_dm_all/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_dm_all/ lineattrs=(color=skyblue thickness=2 ) name='dm' legendlabel='diabetes';
xaxis label='Years from registry' values=(0.5 5 10 15) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 )valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'dm'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Diabetes on All Cause Mortality' height=16pt justify=left 'B';  
run;
ods graphics off;


*************************研究分类变量饮酒随着随访时间的变化的死亡风险************************;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2') bmigr(ref='2');
MODEL  d_py*d(0) =gender  age  
  bmigr  education2  education3  education4  quarterimcome2  __8_lin quarterimcome3 __9_lin
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;

contrast 'HR for risk6=1  at year 1' risk6 1 __11_LIN 1/estimate=exp;
 contrast 'HR for risk6=1  at year 5' risk6 1 __11_LIN 5/estimate=exp;
 contrast 'HR for risk6=1 at year 10' risk6 1 __11_LIN 10/estimate=exp;

EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT3: TEST  risk6, __11_LIN;
 NONCON3: TEST  __11_LIN;

EFFECT4: TEST  dm, __14_LIN;
 NONCON4: TEST  __14_LIN;

RUN;


/*risk6*/
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=13;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ risk6 __11_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS_risk6_all VAR { F FE Z X };  APPEND;  CLOSE __RCS_risk6_all; 
 QUIT; 

 proc sort data=__RCS_risk6_all;
by x z;
run;

proc transpose data=__rcs_risk6_all out=new_risk6_all prefix=z;
by x;
var fe;
id z;
run;

data new_risk6_all;set new_risk6_all;
rename z1=z1_risk6_all z2=z2_risk6_all z3=z3_risk6_all;
drop _NAME_;run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\bmi(All-cause mortality )"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of Drink' imagefmt=tiff ; 
proc sgplot data=new_risk6_all noautolegend;
refline 1 / lineattrs=(color=red pattern=dash) name='line' legendlabel='no drinking(ref)';
band x=x upper=z3_risk6_all lower=z2_risk6_all/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_risk6_all/ lineattrs=(color=skyblue thickness=2 ) name='Drinking' legendlabel='Drinking';
xaxis label='Years from registry' values=(0.5 5 10 15) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 )valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'Drinking'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Drinking on All Cause Mortality' height=16pt justify=left 'C';  
run;
ods graphics off;

*************************研究分类变量家庭收入随着随访时间的变化的死亡风险************************;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2') bmigr(ref='2');
MODEL  d_py*d(0) =gender  age  
  bmigr  education2  education3  education4  quarterimcome2  __8_lin quarterimcome3 __9_lin
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;

contrast 'HR for quarterimcome2=1  at year 1' risk6 1 __11_LIN 1/estimate=exp;
 contrast 'HR for risk6=1  at year 5' risk6 1 __11_LIN 5/estimate=exp;
 contrast 'HR for risk6=1 at year 10' risk6 1 __11_LIN 10/estimate=exp;

EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT3: TEST  risk6, __11_LIN;
 NONCON3: TEST  __11_LIN;

EFFECT4: TEST  dm, __14_LIN;
 NONCON4: TEST  __14_LIN;

RUN;


/*risk6*/
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=13;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ risk6 __11_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS_risk6_all VAR { F FE Z X };  APPEND;  CLOSE __RCS_risk6_all; 
 QUIT; 

 proc sort data=__RCS_risk6_all;
by x z;
run;

proc transpose data=__rcs_risk6_all out=new_risk6_all prefix=z;
by x;
var fe;
id z;
run;

data new_risk6_all;set new_risk6_all;
rename z1=z1_risk6_all z2=z2_risk6_all z3=z3_risk6_all;
drop _NAME_;run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\bmi(All-cause mortality )"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of Drink' imagefmt=tiff ; 
proc sgplot data=new_risk6_all noautolegend;
refline 1 / lineattrs=(color=red pattern=dash) name='line' legendlabel='no drinking(ref)';
band x=x upper=z3_risk6_all lower=z2_risk6_all/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_risk6_all/ lineattrs=(color=skyblue thickness=2 ) name='Drinking' legendlabel='Drinking';
xaxis label='Years from registry' values=(0.5 5 10 15) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 )valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'Drinking'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Drinking on All Cause Mortality' height=16pt justify=left 'C';  
run;
ods graphics off;

/*生成以肿瘤死亡与心血管死亡为结局的变量*/
data b10;
set b9;
tumor=prxmatch('/C|D3|D4/',DICD);
cvd=prxmatch('/I/',DICD);
run;
/*死于肿瘤人数为651*/
/*死于心血管疾病人数为459*/

proc freq data=b10;
table tumor cvd;
run;

/*生成以肿瘤死亡为结局进行分析*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*tumor(0) =gender agegr bmigr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  fam_his dm /selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk1 dm;
proc phreg data=b10;
class education(ref="1");
model d_py*tumor(0)= agegr gender education  risk1  dm 
 agegrat genderat  educationat   risk1at   dmat/rl;
agegrat=agegr*log(d_py);
genderat=gender*log(d_py);
educationat=education*log(d_py);
 risk1at= risk1*log(d_py);
dmat=dm*log(d_py);
proportionality_test: test  agegrat,genderat,educationat ,risk1at,dmat;*检验是否符合ph假定;
run;

/*cox等比例模型*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='2') education(ref='1')  risk1(ref='1')   dm(ref='0') /ref=first;
MODEL d_py*tumor(0) =gender agegr  education risk1  dm / RL; 
RUN;
proc means data=b10 p5 p10 p50 p90 p95 maxdec=2;
var d_py;
run;
 ********** with 3 knots located at; 
 ********** /*5%:8.48 10%:9.22 50%:11.11  90%:12.24  95%:12.34*/;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
MODEL d_py*tumor(0) =
    education2 __5_1 __5_lin education3 __6_1 __6_lin education4 __7_1 __7_lin 
risk1 __10_1 __10_lin dm __14_1 __14_lin
gender  __15_1 __15_lin  agegr __16_1 __16_lin
  /RL;  
__5_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__5_1=__5_1*education2;
__5_lin=education2*d_py;

__6_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__6_1=__6_1*education3;
__6_lin=education3*d_py;

__7_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__7_1=__7_1*education4;
__7_lin=education4*d_py;


__10_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__10_1=__10_1*risk1;
__10_lin=risk1*d_py;
__14_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__14_1=__14_1*dm;
__14_lin=dm*d_py;

__15_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__15_1=__15_1*gender;
__15_lin=gender*d_py;

__16_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__16_1=__16_1*agegr;
__16_lin=agegr*d_py;
*这个p<0.05表示这个变量对预后有意义;
  *这个p<0.05表示这个变量的HR随时间变化（即不满足PH假定）;
 *这个p<0.05表示这个变量的HR随时间变化是非线性的（即需要限制性立方样条来拟合），p>0.05表示这个变量的HR随时间变化是线性的，不需要样条，直接自变量乘以时间就够了;

 EFFECT5: TEST  education2, __5_LIN, __5_1;
 NONCON5: TEST  __5_LIN, __5_1;
 NONLIN5: TEST  __5_1;

 EFFECT6: TEST  education3, __6_LIN, __6_1;
 NONCON6: TEST  __6_LIN, __6_1;
 NONLIN6: TEST  __6_1;

 EFFECT7: TEST  education4, __7_LIN, __7_1;
 NONCON7: TEST  __7_LIN, __7_1;
 NONLIN7: TEST  __7_1;

 EFFECT10: TEST risk1, __10_LIN, __10_1;
 NONCON10: TEST  __10_LIN, __10_1;
 NONLIN10: TEST  __10_1;

 EFFECT14: TEST dm, __14_LIN, __14_1;
 NONCON14: TEST  __14_LIN, __14_1;
 NONLIN14: TEST  __14_1;

  EFFECT15: TEST gender, __15_LIN, __15_1;
 NONCON15: TEST  __15_LIN, __15_1;
 NONLIN15: TEST  __15_1;

 EFFECT16: TEST agegr, __16_LIN, __16_1;
 NONCON16: TEST  __16_LIN, __16_1;
 NONLIN16: TEST  __16_1;
run;
/*符合PH假定*/

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*cvd(0) =gender agegr bmigr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm /selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk6 dm risk19 risk16;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class   gender(ref='2') education(ref='1') risk6(ref='1')  risk16(ref='2')   risk19(ref='2') dm(ref='0') /ref=first;
MODEL d_py*cvd(0) =gender agegr  education risk6 risk16 risk19  dm /selection=backward slstay=0.05 RL; 
RUN;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
MODEL d_py*cvd(0) =
    education2 __5_1 __5_lin education3 __6_1 __6_lin education4 __7_1 __7_lin
 risk6  __11_1 __11_lin risk19  __12_1 __12_lin dm __14_1 __14_lin
gender  __15_1 __15_lin  agegr __16_1 __16_lin risk16 __17_1 __17_lin
  /RL;  
*这个p<0.05表示这个变量对预后有意义;
  *这个p<0.05表示这个变量的HR随时间变化（即不满足PH假定）;
 *这个p<0.05表示这个变量的HR随时间变化是非线性的（即需要限制性立方样条来拟合），p>0.05表示这个变量的HR随时间变化是线性的，不需要样条，直接自变量乘以时间就够了;

__5_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__5_1=__5_1*education2;
__5_lin=education2*d_py;

__6_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__6_1=__6_1*education3;
__6_lin=education3*d_py;

__7_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__7_1=__7_1*education4;
__7_lin=education4*d_py;

__11_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__11_1=__11_1*risk6;
__11_lin=risk6*d_py;

__12_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__12_1=__12_1*risk19;
__12_lin=risk19*d_py;

__14_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__14_1=__14_1*dm;
__14_lin=dm*d_py;

__15_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__15_1=__15_1*gender;
__15_lin=gender*d_py;

__16_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__16_1=__16_1*agegr;
__16_lin=agegr*d_py;

__17_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__17_1=__17_1*risk16;
__17_lin=risk16*d_py;

*这个p<0.05表示这个变量对预后有意义;
  *这个p<0.05表示这个变量的HR随时间变化（即不满足PH假定）;
 *这个p<0.05表示这个变量的HR随时间变化是非线性的（即需要限制性立方样条来拟合），p>0.05表示这个变量的HR随时间变化是线性的，不需要样条，直接自变量乘以时间就够了;


 EFFECT5: TEST  education2, __5_LIN, __5_1;
 NONCON5: TEST  __5_LIN, __5_1;
 NONLIN5: TEST  __5_1;

 EFFECT6: TEST  education3, __6_LIN, __6_1;
 NONCON6: TEST  __6_LIN, __6_1;
 NONLIN6: TEST  __6_1;

 EFFECT7: TEST  education4, __7_LIN, __7_1;
 NONCON7: TEST  __7_LIN, __7_1;
 NONLIN7: TEST  __7_1;

 EFFECT11: TEST risk6, __11_LIN, __11_1;
 NONCON11: TEST  __11_LIN, __11_1;
 NONLIN11: TEST  __11_1;

 EFFECT12: TEST risk19, __12_LIN, __12_1;
 NONCON12: TEST  __12_LIN, __12_1;
 NONLIN12: TEST  __12_1;

 EFFECT14: TEST dm, __14_LIN, __14_1;
 NONCON14: TEST  __14_LIN, __14_1;
 NONLIN14: TEST  __14_1;

  EFFECT15: TEST gender, __15_LIN, __15_1;
 NONCON15: TEST  __15_LIN, __15_1;
 NONLIN15: TEST  __15_1;

 EFFECT16: TEST agegr, __16_LIN, __16_1;
 NONCON16: TEST  __16_LIN, __16_1;
 NONLIN16: TEST  __16_1;

  EFFECT17: TEST risk16, __17_LIN, __17_1;
 NONCON17: TEST  __17_LIN, __17_1;
 NONLIN17: TEST  __17_1;
run;

/*性别不符合PH假定*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS;
class risk6(ref='1')  risk16(ref='2')  risk19(ref='2') agegr(ref='1');
MODEL  d_py*cvd(0) =gender1 __15_lin agegr  
    education2  education3  education4 
  risk6   risk19 dm risk16
  /RL; 
__15_lin=gender1*d_py;
 contrast 'HR for male at year 1' gender1 1 __15_LIN 1/estimate=exp;
 contrast 'HR for male at year 5' gender1 1 __15_LIN 5/estimate=exp;
 contrast 'HR for male at year 10' gender1 1 __15_LIN 10/estimate=exp;

EFFECT15: TEST gender1 , __15_LIN;
 NONCON15: TEST  __15_LIN;
RUN;

/*gender1*/
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=15;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ gender1 __15_LIN }; 
 USE __RCS;  READ ALL VAR HV INTO C; 
 READ ALL VAR { _NAME_ } INTO HC;  CLOSE __RCS; 
 B=C[1,]` ;  HC=REPEAT(HC,1,NCOL(HV)); 
 HV=REPEAT(HV,NROW(HC),1); 
 HV=(upcase(HC)=upcase(HV))[,+]; 
 HV=LOC(HV#(1:NROW(C))`);  C=C[HV,]; 
 F=XMAT*B; FU=XMAT*C*XMAT`; FREE XMAT;
 FU=SQRT(VECDIAG(FU));  FO=F+1.96*FU;  FU=F-1.96*FU; 
 Z=J(NROW(F),1,1)//J(NROW(F),1,2)//J(NROW(F),1,3); 
 F=F//FO//FU; FE=EXP(F);  X=REPEAT(X,3,1); 
 CREATE __RCS_cvd VAR { F FE Z X };  APPEND;  CLOSE __RCS_cvd; 
 QUIT; 

  proc sort data=__RCS_cvd;
by x z;
run;

proc transpose data=__rcs_cvd out=cvd1 prefix=z;
by x;
var fe;
id z;
run;
data cvd1;set cvd1;
rename z1=z1_1 z2=z2_1 z3=z3_1;
drop _NAME_;run;
run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\bmi(All-cause mortality )"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of gender' imagefmt=tiff ; 
proc sgplot data=cvd1 noautolegend;
refline 1 / lineattrs=(color=red pattern=dash) name='line' legendlabel='Female(ref)';
band x=x upper=z3_1 lower=z2_1/fillattrs=(color=skyblue) transparency=0.8;
series x=x y=z1_1/ lineattrs=(color=skyblue thickness=2 ) name='male' legendlabel='Male';
xaxis label='Years from registry' values=(0.5 5 10 15 20) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI) ' values=( 0.5 1.0 2.0  3 4 5 6 7 8 9 10)valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'male' / valueattrs=(size=10pt) location=outside position=bottom; 
title height=14pt 'Time-varying effect of Gender on CVD-specific Mortality' height=16pt;  
run;
ods graphics off;

proc means data=b9 max min p1 p99;
var d_py;
run;


