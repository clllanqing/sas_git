************************以死亡为结局的分析过程********************;
*************************************资料处理过程*****************************************;
libname cao "D:\1a\learning\research\数据20210824\大肠癌筛查\data and code";
data b9;
set cao.analysis_data;
run;
proc means data=b9 p25 p50 p75 maxdec=2;
var waist hip whr whtr;
run;
/*将BMI、腰围、臀围、腰臀比、腰高比四等分*/
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
/*将whtr二等分*/
data b9;
set b9;
if whtr<0.5 then binwhtr=1;
else binwhtr=2;
run;
/*将BMI二等分及三等分*/
data b9;
set b9;
if bmi<18.5 then tribmi=1;
if 18.5<=bmi<28 then tribmi=2;
if bmi>=28 then  tribmi=3;
if bmi<24 then binbmi=1;
if bmi>=24 then  binbmi=2;
if bmi<18.5 then tribmi2=1;
if 18.5<=bmi<24 then tribmi2=2;
if bmi>=24 then  tribmi2=3;
run;
data  b9;
set b9;
if tribmi=1 then tribmi1=1;
else tribmi1=0;
if tribmi=2 then tribmi2=1;
else tribmi2=0;
if tribmi=3 then tribmi3=1;
else tribmi3=0;
if binwhtr=1 then binwhtr1=1;
else binwhtr1=0;
if binwhtr=2 then binwhtr2=1;
else binwhtr2=0;
run;
data b9;
set b9;
if bmigr=1 and binwhtr=1 then con=1;if bmigr=1 and binwhtr=2 then con=2;
if bmigr=2 and binwhtr=1 then con=3;if bmigr=2 and binwhtr=2 then con=4;
if bmigr=3 and binwhtr=1 then con=5;if bmigr=3 and binwhtr=2 then con=6;
if bmigr=4 and binwhtr=1 then con=7;if bmigr=4 and binwhtr=2 then con=8;
run;
proc means data=b9 min max mean var std;
var age;
run;
**********************************************探索全死因死亡相关危险因素*************************************;
proc freq data=b9;
tables d tribmi binwhtr;
run;
proc phreg data=b9;
class  bmigr(ref="2") gender(ref='1');
MODEL d_py*d(0) =gender agegr bmigr  / RL; 
RUN;
*BMI有意义;
proc phreg data=b9;
class  tribmi(ref="2") gender(ref='1');
MODEL d_py*d(0) =gender agegr tribmi  / RL; 
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
class  gender(ref='1') binwhtr(ref='1');
MODEL d_py*d(0) =gender agegr binwhtr/ RL; 
RUN;
*腰高比二等分有意义;
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
*保留的变量 gender agegr bmigr education quarterimcome risk1 risk6 risk19 dm  whtr;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') dm(ref='0') binwhtr(ref='1') agegr(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr bmigr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm binwhtr /selection=stepwise slstay=0.05 RL; 
RUN;
*保留的变量 gender agegr tribmi education quarterimcome risk1 risk6 risk19 dm  binwhtr;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='2') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1') risk19(ref='2') dm(ref='0') binwhtr(ref='1') /ref=first;
MODEL d_py*d(0) =gender agegr bmigr education quarterimcome risk1 risk6  risk19  binwhtr dm/ RL; 
RUN;
/*检验是否存在交互作用*/
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1') risk19(ref='2') dm(ref='0') binwhtr(ref='1') /param=glm;
MODEL d_py*d(0) =gender agegr  education quarterimcome risk1 risk6 risk16 risk19  binwhtr|tribmi dm/ RL; 
RUN;
proc phreg data=b9;
class bmigr(ref="2") education(ref="1") quarterimcome(ref="1") binwhtr(ref='1') agegr(ref='1') gender(ref='2')  risk1(ref='1') risk6(ref='1') risk19(ref='2');
model d_py*d(0)=bmigr agegr gender education quarterimcome risk1 risk6 risk19 dm  binwhtr
bmigrat agegrat genderat  educationat quarterimcomeat  risk1at  risk6at  risk19at   dmat binwhtrat/rl;
bmigrat=bmigr*log(d_py);
agegrat=agegr*log(d_py);
genderat=gender*log(d_py);
educationat=education*log(d_py);
quarterimcomeat=quarterimcome*log(d_py);
 risk1at= risk1*log(d_py);
 risk6at=risk6*log(d_py);
risk19at=risk19*log(d_py);
dmat=dm*log(d_py);
binwhtrat=binwhtr*log(d_py);
proportionality_test: test  bmigrat,agegrat,genderat,educationat ,quarterimcomeat ,risk1at,risk6at,risk19at,dmat,binwhtrat;*检验是否符合ph假定;
run;
*ph-test: p=<.0001 模型不符合等比例，采用变系数模型;
proc means data=b9 min max p1 p5 p10 p50 p90 p95 p99 maxdec=2;
var d_py;
run;
 ********** with 3 knots located at; 
 ********** /*5%:8.48 10%:9.22 50%:11.11  90%:12.24  95%:12.34*/;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
MODEL d_py*d(0) =
 bmigr1  __1_1 __1_lin bmigr3 __2_1 __2_lin  bmigr4 __3_1 __3_lin 
    education2 __5_1 __5_lin education3 __6_1 __6_lin education4 __7_1 __7_lin quarterimcome2 __8_1 __8_lin quarterimcome3 __9_1 __9_lin
risk1 __10_1 __10_lin risk6  __11_1 __11_lin risk19  __12_1 __12_lin  dm __14_1 __14_lin
gender1  __15_1 __15_lin  agegr __16_1 __16_lin  binwhtr2 __17_1 __17_lin
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
__17_1=__17_1*binwhtr2;
__17_lin=binwhtr2*d_py;

 EFFECT1: TEST  bmigr1, __1_LIN, __1_1;
 NONCON1: TEST  __1_LIN, __1_1;
 NONLIN1: TEST  __1_1;
*这个p<0.05表示这个变量对预后有意义;
  *这个p<0.05表示这个变量的HR随时间变化（即不满足PH假定）;
 *这个p<0.05表示这个变量的HR随时间变化是非线性的（即需要限制性立方样条来拟合），p>0.05表示这个变量的HR随时间变化是线性的，不需要样条，直接自变量乘以时间就够了;
 EFFECT2: TEST   bmigr3, __2_LIN, __2_1;
 NONCON2: TEST  __2_LIN, __2_1;
 NONLIN2: TEST  __2_1;

  EFFECT3: TEST   bmigr4, __3_LIN, __3_1;
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

 EFFECT17: TEST binwhtr2, __17_LIN, __17_1;
 NONCON17: TEST  __17_LIN, __17_1;
 NONLIN17: TEST  __17_1;
run;
/*quarterimcome;risk6;dm;不符合PH假定*/
/*BMI符合假定*/

******************************糖尿病与全死因死亡*******************************;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2') bmigr(ref='2') binwhtr(ref='1');
MODEL  d_py*d(0) =gender  age   bmigr
    education2  education3  education4  quarterimcome2 __8_LIN  quarterimcome3 __9_LIN
risk1  risk6   __11_lin risk19   dm  __14_lin  binwhtr
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;

contrast 'HR for dm=1  at year 1' dm 1 __14_LIN 1/estimate=exp;
 contrast 'HR for dm=1  at year 5' dm 1 __14_LIN 5/estimate=exp;
 contrast 'HR for dm=1 at year 9' dm 1 __14_LIN 9/estimate=exp;
contrast 'HR for dm=1 at year 13' dm 1 __14_LIN 13/estimate=exp;
 EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT5: TEST  risk6, __11_LIN;
 NONCON5: TEST  __11_LIN;

EFFECT7: TEST  dm, __14_LIN;
 NONCON7: TEST  __14_LIN;

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
 CREATE __RCS_dm_all VAR { F FE Z X };  APPEND;  CLOSE __RCS_dm_all; 
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

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\All-cause mortality"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of dm' imagefmt=tiff ; 
proc sgplot data=new_dm_all noautolegend;
refline 1 / lineattrs=(color=black pattern=dash) name='line' legendlabel='no diabetes(ref)';
band x=x upper=z3_dm_all lower=z2_dm_all/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_dm_all/ lineattrs=(thickness=2 ) name='dm' legendlabel='diabetes';
xaxis label='Years from registry' values=(0.5 5 9 13 ) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0 0.5 1.0 1.5 2.0 2.5 3.0 )valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'dm'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Diabetes on All Cause Mortality' height=16pt justify=left 'A';  
run;
ods graphics off;

*************************研究分类变量饮酒随着随访时间的变化的死亡风险************************;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2') bmigr(ref='2') binwhtr(ref='1');
MODEL  d_py*d(0) =gender  age   bmigr  binwhtr
    education2  education3  education4  quarterimcome2 __8_LIN  quarterimcome3 __9_LIN
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;
contrast 'HR for risk6=1  at year 1' risk6 1 __11_LIN 1/estimate=exp;
 contrast 'HR for risk6=1  at year 5' risk6 1 __11_LIN 5/estimate=exp;
 contrast 'HR for risk6=1 at year 9' risk6 1 __11_LIN 9/estimate=exp;
 contrast 'HR for risk6=1 at year 12' risk6 1 __11_LIN 12/estimate=exp;
 EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT5: TEST  risk6, __11_LIN;
 NONCON5: TEST  __11_LIN;

EFFECT7: TEST  dm, __14_LIN;
 NONCON7: TEST  __14_LIN;

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

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\All-cause mortality"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of Drink' imagefmt=tiff ; 
proc sgplot data=new_risk6_all noautolegend;
refline 1 / lineattrs=(color=black pattern=dash) name='line' legendlabel='no drinking(ref)';
band x=x upper=z3_risk6_all lower=z2_risk6_all/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_risk6_all/ lineattrs=( thickness=2 ) name='Drinking' legendlabel='Drinking';
xaxis label='Years from registry' values=(0.5 5 9 13) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0 0.5 1.0 1.5 2.0 2.5 3.0)valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'Drinking'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Drinking on All Cause Mortality' height=16pt justify=left 'B';  
run;
ods graphics off;


/*研究分类变量家庭收入随着随访时间的变化的死亡风险*/;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2') bmigr(ref='2') binwhtr(ref='1');
MODEL  d_py*d(0) =gender  age   bmigr
    education2  education3  education4  quarterimcome2 __8_LIN  quarterimcome3 __9_LIN  binwhtr
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;
contrast 'HR for 2000<=quarterimcome2<=4000  at year 1' quarterimcome2 1 __8_LIN 1/estimate=exp;
 contrast 'HR for 2000<=quarterimcome2<=4000  at year 5' quarterimcome2 1 __8_LIN 5/estimate=exp;
 contrast 'HR for 2000<=quarterimcome2<=4000 at year 9' quarterimcome2 1 __8_LIN 9/estimate=exp;
 contrast 'HR for 2000<=quarterimcome2<=4000 at year 12' quarterimcome2 1 __8_LIN 12/estimate=exp;

 contrast 'HR for quarterimcome2>4000  at year 1' quarterimcome3 1 __9_LIN 1/estimate=exp;
 contrast 'HR for quarterimcome2>4000  at year 5' quarterimcome3 1 __9_LIN 5/estimate=exp;
 contrast 'HR for quarterimcome2>4000 at year 9' quarterimcome3 1 __9_LIN 9/estimate=exp;
 contrast 'HR for quarterimcome2>4000 at year 12' quarterimcome3 1 __9_LIN 12/estimate=exp;
 EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT5: TEST  risk6, __11_LIN;
 NONCON5: TEST  __11_LIN;

EFFECT7: TEST  dm, __14_LIN;
 NONCON7: TEST  __14_LIN;

RUN;

/*quarterimcome2*/
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=13;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ quarterimcome2 __8_LIN }; 
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
 CREATE __RCS_quarterimcome_1 VAR { F FE Z X };  APPEND;  CLOSE __ __RCS_quarterimcome_1; 
 QUIT; 
 /*quarterimcome3*/
PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0;     *Smallest value for X-axis; 
 UPPEREND=13;     *Largest value for X-axis; 
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 XMAT=J(NPOINTS,1,1)||X;
 HV={ quarterimcome3 __9_LIN }; 
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
 CREATE  __RCS_quarterimcome_2 VAR { F FE Z X };  APPEND;  CLOSE  __RCS_quarterimcome_2; 
 QUIT; 

proc sort data=__RCS_quarterimcome_1;
by x z;
run;

proc transpose data=__RCS_quarterimcome_1 out=__RCS_quarterimcome_1 prefix=z;
by x;
var fe;
id z;
run;

data __RCS_quarterimcome_1;set __RCS_quarterimcome_1;
rename z1=z1_quarterimcome_1 z2=z2_quarterimcome_1 z3=z3_quarterimcome_1;
drop _NAME_;run;

proc sort data=__RCS_quarterimcome_2;
by x z;
run;

proc transpose data=__RCS_quarterimcome_2 out=__RCS_quarterimcome_2 prefix=z;
by x;
var fe;
id z;
run;

data __RCS_quarterimcome_2;set __RCS_quarterimcome_2;
rename z1=z1_quarterimcome_2 z2=z2_quarterimcome_2 z3=z3_quarterimcome_2;
drop _NAME_;run;

data quarterimcomeplot;merge __RCS_quarterimcome_1  __RCS_quarterimcome_2;by x;run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\All-cause mortality"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of Quarterimcome' imagefmt=tiff ; 
proc sgplot data=quarterimcomeplot noautolegend;
refline 1 / lineattrs=(color=black pattern=dash) name='line' legendlabel='<2000元(ref)';
band x=x upper=z3_quarterimcome_1 lower=z2_quarterimcome_1/fillattrs=(color=skyblue) transparency=0.7;
band x=x upper=z3_quarterimcome_2 lower=z2_quarterimcome_2/fillattrs=(color=pink) transparency=0.7;
series x=x y=z1_quarterimcome_1/lineattrs=(color=skyblue thickness=2 ) name='quarterimcome2' legendlabel='2000-4000元';
series x=x y=z1_quarterimcome_2/lineattrs=(color=pink thickness=2 ) name='quarterimcome3' legendlabel='>4000元';
xaxis label='Years from registry' values=(0.5 5 9 13) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0 0.5 1.0 1.5 2.0 2.5 3.0 )valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'quarterimcome2' 'quarterimcome3'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=12pt 'Time-varying effect of Quarterimcome on All Cause Mortality' height=16pt justify=left 'C';  
run;
ods graphics off;
/*检验不符合PH假定的变量与年龄性别的交互作用*/
PROC PHREG DATA=b9; 
class  bmigr(ref="2") gender(ref='2') agegr(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*d(0) =gender bmigr education  risk1 risk19   agegr|quarterimcome agegr|risk6  agegr|dm/RL; 
RUN;
PROC PHREG DATA=b9; 
class  bmigr(ref="2") gender(ref='2') agegr(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*d(0) = agegr  bmigr education risk1  risk19  gender|quarterimcome gender|risk6   gender|dm/RL; 
RUN;
PROC PHREG DATA=b9; 
class  bmigr(ref="2") gender(ref='2') agegr(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*d(0) =gender agegr  bmigr education risk19  risk1|quarterimcome risk1|risk6   risk1|dm/RL; 
RUN;
PROC PHREG DATA=b9; 
class  bmigr(ref="2") gender(ref='2') agegr(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*d(0) =risk1|gender risk1|agegr  risk1|bmigr risk1|education risk1|risk19  risk1|quarterimcome risk1|risk6   risk1|dm/RL; 
RUN;
/*将年龄进行分层分析*/
PROC PHREG DATA=b9(where=(agegr=1)) COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='2')  education(ref='1')  risk1(ref='1')   risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*d(0) =gender  bmigr education quarterimcome risk1 risk6 risk19  dm/RL; 
RUN;
*糖尿病与全死因死亡;
PROC PHREG DATA=b9(where=(agegr=1)) COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2') ;
MODEL  d_py*d(0) =gender    bmigr1 bmigr3 bmigr4 
    education2  education3  education4  quarterimcome2 __8_LIN  quarterimcome3 __9_LIN
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;

contrast 'HR for dm=1  at year 1' dm 1 __14_LIN 1/estimate=exp;
 contrast 'HR for dm=1  at year 5' dm 1 __14_LIN 5/estimate=exp;
 contrast 'HR for dm=1 at year 9' dm 1 __14_LIN 9/estimate=exp;
contrast 'HR for dm=1 at year 13' dm 1 __14_LIN 13/estimate=exp;
 EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT5: TEST  risk6, __11_LIN;
 NONCON5: TEST  __11_LIN;

EFFECT7: TEST  dm, __14_LIN;
 NONCON7: TEST  __14_LIN;

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
 CREATE __RCS_dm_old VAR { F FE Z X };  APPEND;  CLOSE __RCS_old; 
 QUIT; 

 proc sort data=__RCS_dm_old;
by x z;
run;

proc transpose data=__rcs_dm_old out=new_dm_old prefix=z;
by x;
var fe;
id z;
run;

data new_dm_old;set new_dm_old;
rename z1=z1_dm_old z2=z2_dm_old z3=z3_dm_old;
drop _NAME_;run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\bmi(All-cause mortality )"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of dm(age<60)' imagefmt=tiff ; 
proc sgplot data=new_dm_old noautolegend;
refline 1 / lineattrs=(color=red pattern=dash) name='line' legendlabel='no diabetes(ref)';
band x=x upper=z3_dm_old lower=z2_dm_old/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_dm_old/ lineattrs=(color=skyblue thickness=2 ) name='dm' legendlabel='diabetes';
xaxis label='Years from registry' values=(0.5 5 9 13 ) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 )valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'dm'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Diabetes(age<60)' height=16pt justify=left 'B';  
run;
ods graphics off;


PROC PHREG DATA=b9(where=(agegr=2)) COVOUT OUTEST=__RCS;
class gender(ref='1')  risk1(ref='1')  risk1(ref='1') risk19(ref='2');
MODEL  d_py*d(0) =gender    bmigr1 bmigr3 bmigr4 
    education2  education3  education4  quarterimcome2 __8_LIN  quarterimcome3 __9_LIN
risk1  risk6   __11_lin risk19   dm  __14_lin
  /RL; 
__8_lin=quarterimcome2*d_py;
__9_lin=quarterimcome3*d_py;
__11_lin=risk6*d_py;
__14_lin=dm*d_py;

contrast 'HR for dm=1  at year 1' dm 1 __14_LIN 1/estimate=exp;
 contrast 'HR for dm=1  at year 5' dm 1 __14_LIN 5/estimate=exp;
 contrast 'HR for dm=1 at year 9' dm 1 __14_LIN 9/estimate=exp;
contrast 'HR for dm=1 at year 13' dm 1 __14_LIN 13/estimate=exp;
 EFFECT1: TEST  quarterimcome2, __8_LIN;
 NONCON1: TEST  __8_LIN;

 EFFECT2: TEST  quarterimcome3, __9_LIN;
 NONCON2: TEST  __9_LIN;

 EFFECT5: TEST  risk6, __11_LIN;
 NONCON5: TEST  __11_LIN;

EFFECT7: TEST  dm, __14_LIN;
 NONCON7: TEST  __14_LIN;

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
 CREATE __RCS_dm_young VAR { F FE Z X };  APPEND;  CLOSE  __RCS_dm_young; 
 QUIT; 

 proc sort data=__RCS_dm_young;
by x z;
run;

proc transpose data=__rcs_dm_young out=new_dm_young prefix=z;
by x;
var fe;
id z;
run;

data new_dm_young;set new_dm_young;
rename z1=z1_dm_young z2=z2_dm_young z3=z3_dm_young;
drop _NAME_;run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\bmi(All-cause mortality )"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of dm(age≥60)' imagefmt=tiff ; 
proc sgplot data=new_dm_young noautolegend;
refline 1 / lineattrs=(color=red pattern=dash) name='line' legendlabel='no diabetes(ref)';
band x=x upper=z3_dm_young lower=z2_dm_young/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_dm_young/ lineattrs=(color=skyblue thickness=2 ) name='dm' legendlabel='diabetes';
xaxis label='Years from registry' values=(0.5 5 9 13 ) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 )valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'dm'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Diabetes(age≥60)' height=16pt justify=left 'B';  
run;
ods graphics off;



*************************************************生成以肿瘤死亡为结局进行分析********************************;
/*生成以肿瘤死亡与心血管死亡为结局的变量*/
data b10;
set b9;
tumor=prxmatch('/C|D3|D4/',DICD);
cvd=prxmatch('/I/',DICD);
run;
/*死于肿瘤人数为650*/
/*死于心血管疾病人数为459*/

proc freq data=b10;
table tumor cvd;
run;
/*单因素分析*/
proc phreg data=b10;
class bmigr(ref="2") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr bmigr;
run;
/*BMI无意义*/
proc phreg data=b10;
class binwhtr(ref="2") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr binwhtr;
run;
/*whtr无意义*/
proc phreg data=b10;
class education(ref="1") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr education;
run;
/*education有意义*/
proc phreg data=b10;
class quarterimcome(ref="1") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr quarterimcome;
run;
/*quarterimcome有意义*/
proc phreg data=b10;
class risk1(ref="1") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr risk1;
run;
/*risk1有意义*/
proc phreg data=b10;
class risk6(ref="1") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr risk6;
run;
/*risk6无意义*/
proc phreg data=b10;
class risk16(ref="2") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr risk16;
run;
/*risk16无意义*/
proc phreg data=b10;
class risk17(ref="2") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr risk17;
run;
/*risk17无意义*/
proc phreg data=b10;
class risk18(ref="2") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr risk18;
run;
/*risk18无意义*/
proc phreg data=b10;
class risk19(ref="2") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr risk19;
run;
/*risk19有意义*/
proc phreg data=b10;
class dm(ref="0") gender(ref='1') agegr(ref='1');
model d_py*tumor(0)=gender agegr dm;
run;
/*dm有意义*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')   risk19(ref='2')  dm(ref='0')  bmigr(ref='2') binwhtr(ref='1')/ref=first;
MODEL d_py*tumor(0) =gender agegr education quarterimcome risk1 risk6 risk19 dm bmigr binwhtr /selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk1 dm risk6 bmigr;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  gender(ref='1') education(ref='1')  risk1(ref='1')   risk19(ref='2')  dm(ref='0')  binbmi(ref='1') binwhtr(ref='1')/ref=first;
MODEL d_py*tumor(0) =gender agegr education   risk1 dm binwhtr /selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk1 dm risk6 bmigr;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')   risk19(ref='2')  dm(ref='0') binwhtr(ref='1')/ref=first;
MODEL d_py*tumor(0) =gender agegr education quarterimcome risk1 risk6 risk19 dm binwhtr /selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk1 dm risk6 bmigr;
proc phreg data=b10;
class gender(ref='1') education(ref='1') risk1(ref='1') risk6(ref='1')  dm(ref='0')  bmigr(ref='2');
model d_py*tumor(0)= agegr gender education  risk1  risk6 dm bmigr
 agegrat genderat  educationat   risk1at   risk6at dmat bmigrat/rl;
agegrat=agegr*log(d_py);
genderat=gender*log(d_py);
educationat=education*log(d_py);
 risk1at= risk1*log(d_py);
  risk6at= risk6*log(d_py);
dmat=dm*log(d_py);
bmigrat=bmigr*log(d_py);
proportionality_test: test  agegrat,genderat,educationat ,risk1at,risk6at,bmigrat,dmat;*检验是否符合ph假定;
run;

/*cox等比例模型*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") agegr(ref='1')gender(ref='2') education(ref='1')  risk1(ref='1') risk6(ref='1')   dm(ref='0') /ref=first;
MODEL d_py*tumor(0) =gender agegr  education risk1 risk6  dm bmigr/ RL; 
RUN;
proc means data=b10 p5 p10 p50 p90 p95 maxdec=2;
var d_py;
run;
 ********** with 3 knots located at; 
 ********** /*5%:8.48 10%:9.22 50%:11.11  90%:12.24  95%:12.34*/;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
MODEL d_py*tumor(0) =
  bmigr1  __1_1 __1_lin bmigr3  __2_1 __2_lin  bmigr4  __3_1 __3_lin education2 __5_1 __5_lin education3 __6_1 __6_lin education4 __7_1 __7_lin 
risk1 __10_1 __10_lin  risk6 __11_1 __11_lin  dm __14_1 __14_lin
gender  __15_1 __15_lin  agegr __16_1 __16_lin
  /RL;  
__1_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__1_1=__1_1*bmigr1;
__1_lin=bmigr1*d_py;

__2_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__2_1=__2_1*bmigr3;
__2_lin=bmigr3*d_py;

__3_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__3_1=__3_1*bmigr4;
__3_lin=bmigr4*d_py;

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

__11_1=((d_py-8.48)**3)*(d_py>8.48) 
     -((d_py-11.11)**3)*(d_py>11.11) 
     *(12.34-8.48)/(12.34-11.11) 
     +((d_py-12.34)**3)*(d_py>12.34) 
     *(11.11-8.48)/(12.34-11.11);
__11_1=__11_1*risk6;
__11_lin=risk6*d_py;

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
EFFECT1: TEST  bmigr1, __1_LIN, __1_1;
 NONCON1: TEST  __1_LIN, __1_1;
 NONLIN1: TEST  __1_1;
*这个p<0.05表示这个变量对预后有意义;
  *这个p<0.05表示这个变量的HR随时间变化（即不满足PH假定）;
 *这个p<0.05表示这个变量的HR随时间变化是非线性的（即需要限制性立方样条来拟合），p>0.05表示这个变量的HR随时间变化是线性的，不需要样条，直接自变量乘以时间就够了;
 EFFECT2: TEST   bmigr3, __2_LIN, __2_1;
 NONCON2: TEST  __2_LIN, __2_1;
 NONLIN2: TEST  __2_1;

  EFFECT3: TEST   bmigr4, __3_LIN, __3_1;
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

 EFFECT10: TEST risk1, __10_LIN, __10_1;
 NONCON10: TEST  __10_LIN, __10_1;
 NONLIN10: TEST  __10_1;

  EFFECT11: TEST risk6, __11_LIN, __11_1;
 NONCON11: TEST  __11_LIN, __11_1;
 NONLIN11: TEST  __11_1;

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
/*risk6 不符合PH假定*/

*************************研究分类变量饮酒随着随访时间的变化的死亡风险************************;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
MODEL d_py*tumor(0) =
  bmigr education risk6 __11_lin  dm risk1
gender agegr 
  /RL; 
__11_lin=risk6*d_py;

contrast 'HR for risk6=1  at year 1' risk6 1 __11_LIN 1/estimate=exp;
 contrast 'HR for risk6=1  at year 5' risk6 1 __11_LIN 5/estimate=exp;
 contrast 'HR for risk6=1 at year 9' risk6 1 __11_LIN 9/estimate=exp;
 contrast 'HR for risk6=1 at year 12' risk6 1 __11_LIN 12/estimate=exp;

 EFFECT5: TEST  risk6, __11_LIN;
 NONCON5: TEST  __11_LIN;
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
 CREATE __RCS_risk6_tumor VAR { F FE Z X };  APPEND;  CLOSE __RCS_risk6_tumor; 
 QUIT; 

 proc sort data=__RCS_risk6_tumor;
by x z;
run;

proc transpose data=__rcs_risk6_tumor out=new_risk6_tumor prefix=z;
by x;
var fe;
id z;
run;

data new_risk6_tumor;set new_risk6_tumor;
rename z1=z1_risk6_all z2=z2_risk6_all z3=z3_risk6_all;
drop _NAME_;run;

ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\ALL-cancer mortality"  dpi=300 ;
ods graphics on/border=off imagename='time-varing effect of Drink' imagefmt=tiff ; 
proc sgplot data=new_risk6_tumor noautolegend;
refline 1 / lineattrs=(color=black pattern=dash) name='line' legendlabel='no drinking(ref)';
band x=x upper=z3_risk6_all lower=z2_risk6_all/fillattrs=(color=skyblue) transparency=0.7;
series x=x y=z1_risk6_all/ lineattrs=( thickness=2 ) name='Drinking' legendlabel='Drinking';
xaxis label='Years from registry' values=(0.5 5 9 13) valueattrs=(size=14pt) labelattrs=(size=14pt);
yaxis label='HR(95%CI)' values=(0 0.5 1.0 1.5 2.0 2.5 3.0)valueattrs=(size=14pt) labelattrs=(size=14pt) ;  
keylegend 'line' 'Drinking'/ valueattrs=(size=10pt) location=outsideside position=bottom; 
title height=14pt 'Time-varying effect of Drinking on All Cancer Mortality' height=16pt justify=left 'A';  
run;
ods graphics off;

/*研究影响CVD死亡的因素*/
/*生存曲线图*/
proc lifetest data=b10 method=km outsurv=estimatesm;
time d_py*cvd(0);
strata bmigr;
run;
*logrank p=<.0001;
proc contents data=estimatesm;
run;
data estimatesm;set estimatesm;
survt_year=d_py;
if bmigr=1 then subtype='Underweight';
if bmigr=2 then subtype='Normalweight';
if bmigr=3 then subtype='Overweight';
if bmigr=4 then subtype='Obese';
run;
data line;
input id $ value $13. linecolor :$20.;
datalines;      
group Underweight   darkorange  Bold   
group Normal weight    black      
group Overweight      grey               
group Obese         mediumvividblue    
;
run;
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\survival curve" dpi=300;
ods graphics/border=off imagename="CVD  mortality,by BMI" imagefmt=tiff;
proc sgplot data=estimatesm  dattrmap=line ;
step x=survt_year y=survival/group=subtype attrid=group lineattrs=(thickness=2);
symbol1 line=1 width=1.5;
symbol2 line=2 width=1.5;
symbol3 line=3 width=1.5;
symbol4 line=4 width=1.5;
yaxis values=(0.9 0.92 0.94 0.96 0.98  1) label='Cumulative survival'  LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
xaxis label='Years from Registry'  LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
keylegend /location=inside position=bottomleft;
inset 'Log-rank chi-square=16.1247, P<0.0011'/position=left textattrs=(Family=Arial Size=9 Style=Italic Weight=Bold);
title justify=medium height=14pt   'CVD mortality,by BMI';
run;


/*生存曲线图*/
proc lifetest data=b10 method=km outsurv=estimatesm;
time d_py*cvd(0);
strata binwhtr;
run;
*logrank p=<.0001;
proc contents data=estimatesm;
run;
data estimatesm;
length subtype $15.;
set estimatesm;
survt_year=d_py;
if binwhtr=1 then subtype='WHtR<0.5';
if binwhtr=2 then subtype='WHtR≥0.5';
run;
data line;
input id $ value $10. linecolor :$20.;
datalines;      
group WHtR<0.5   darkorange  Bold   
group WHtR≥0.5   black      
run;
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\survival curve" dpi=300;
ods graphics/border=off imagename="CVD  mortality,by WHtR" imagefmt=tiff;
proc sgplot data=estimatesm  dattrmap=line ;
step x=survt_year y=survival/group=subtype attrid=group lineattrs=(thickness=2);
symbol1 line=1 width=1.5;
symbol2 line=2 width=1.5;
yaxis values=(0.9 0.92 0.94 0.96 0.98  1) label='Cumulative survival'  LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
xaxis label='Years from Registry'  LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
keylegend /location=inside position=bottomleft;
inset 'Log-rank chi-square=65.0231, P<.0001'/position=left textattrs=(Family=Arial Size=9 Style=Italic Weight=Bold);
title justify=medium height=14pt   'CVD mortality,by WHtR';
run;

/*查看BMI与腰高比的分布*/
proc freq data=b10;
tables bmigr*binwhtr;
run;

/*单因素分析*/
proc phreg data=b10;
class tribmi2(ref="2") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr tribmi2/rl;
run;
/*BMI有意义*/
proc phreg data=b10;
class binwhtr(ref="2") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr binwhtr;
run;
/*whtr有意义*/
proc phreg data=b10;
class education(ref="1") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr education;
run;
/*education有意义*/
proc phreg data=b10;
class quarterimcome(ref="1") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr quarterimcome;
run;
/*quarterimcome有意义*/
proc phreg data=b10;
class risk1(ref="1") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr risk1;
run;
/*risk1无意义*/
proc phreg data=b10;
class risk6(ref="1") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr risk6;
run;
/*risk6无意义*/
proc phreg data=b10;
class risk16(ref="2") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr risk16;
run;
/*risk16有意义*/
proc phreg data=b10;
class risk17(ref="2") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr risk17;
run;
/*risk17无意义*/
proc phreg data=b10;
class risk18(ref="2") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr risk18;
run;
/*risk18无意义*/
proc phreg data=b10;
class risk19(ref="2") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr risk19;
run;
/*risk19有意义*/
proc phreg data=b10;
class dm(ref="0") gender(ref='1') agegr(ref='1');
model d_py*cvd(0)=gender agegr dm;
run;
/*dm有意义*/

proc freq data=b11;
table con;
run;
proc phreg data=b11;
class agegr(ref="1") gender(ref='1') con(ref='3');
model d_py*cvd(0)=gender agegr con;
run;
/*con有意义*/

/*计算发病人数*/
proc freq data=b11;
tables  cvd bmigr*cvd binwhtr*cvd con*cvd;
run;
/*计算随访人年*/
proc sql;
create table  bmigr1 as
select 
sum(d_py) as sum_d_py1
from b11 where  bmigr=1;
quit;
proc sql;
create table  bmigr2 as
select 
sum(d_py) as sum_d_py1
from b11 where  bmigr=2;
quit;
proc sql;
create table  bmigr3 as
select 
sum(d_py) as sum_d_py1
from b11 where  bmigr=3;
quit;
proc sql;
create table  bmigr4 as
select 
sum(d_py) as sum_d_py1
from b11 where  bmigr=4;
quit;

proc sql;
create table binwhtr1 as
select 
sum(d_py) as sum_d_py
from b11 where  binwhtr=1;
quit;

proc sql;
create table binwhtr2 as
select 
sum(d_py) as sum_d_py
from b11 where  binwhtr=2;
quit;

proc sql;
create table con1 as
select 
sum(d_py) as sum_d_py
from b11 where  con=1;
quit;

proc sql;
create table con2 as
select 
sum(d_py) as sum_d_py
from b11 where  con=2;
quit;

proc sql;
create table con3 as
select 
sum(d_py) as sum_d_py
from b11 where  con=3;
quit;

proc sql;
create table con4 as
select 
sum(d_py) as sum_d_py
from b11 where  con=4;
quit;

proc sql;
create table con5 as
select 
sum(d_py) as sum_d_py
from b11 where  con=5;
quit;

proc sql;
create table con6 as
select 
sum(d_py) as sum_d_py
from b11 where  con=6;
quit;

proc sql;
create table con7 as
select 
sum(d_py) as sum_d_py
from b11 where  con=7;
quit;

proc sql;
create table con8 as
select 
sum(d_py) as sum_d_py
from b11 where  con=8;
quit;
proc means data=b11 mean;
var d_py;
where bmigr=1;
run;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') binwhtr(ref='1') /ref=first;
MODEL d_py*cvd(0) =gender agegr bmigr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm  binwhtr/selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk6 dm risk19 risk16 bmigr binwhtr;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class   gender(ref='2') education(ref='1') risk6(ref='1')  risk16(ref='2')   risk19(ref='2') dm(ref='0') bmigr(ref='2') binwhtr(ref='1')/ref=first;
MODEL d_py*cvd(0) =gender agegr  education risk6 risk16 risk19  dm  bmigr binwhtr/ RL; 
RUN;

PROC PHREG DATA=b11 COVOUT OUTEST=__RCS; 
class gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') con(ref='3') /ref=first;
MODEL d_py*cvd(0) =gender agegr  education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm  con /selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk6 dm risk19 risk16 con;
PROC PHREG DATA=b11 COVOUT OUTEST=__RCS; 
class gender(ref='1') education(ref='1') risk6(ref='1')  risk16(ref='2') risk19(ref='2')  dm(ref='0') con(ref='3') /ref=first;
MODEL d_py*cvd(0) =gender agegr  education  risk6 risk16  risk19  dm  con / RL; 
RUN;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; 
class   gender(ref='2') education(ref='1') risk6(ref='1')  risk16(ref='2')   risk19(ref='2') dm(ref='0') /ref=first;
MODEL d_py*cvd(0) =gender agegr  education risk6 risk16 risk19  dm  bmi|whtr/ RL; 
RUN;

proc means data=b10 p1 p10 p50 p90 p99;
var bmi;
run;
/*将BMI与whtr以连续型变量的形式纳入*/
proc phreg data=b10;
class  gender(ref='1') agegr(ref='1')  risk6(ref='1') risk16(ref='1')  risk19(ref='1') dm(ref='0') education(ref='1')  ;
model d_py*cvd(0)= agegr gender risk6 risk16 risk19  dm education  bmi whtr
agegrat genderat  risk6at   risk16at risk19at  dmat  educationat bmiat whtrat/rl;
agegrat=agegr*log(d_py);
genderat=gender*log(d_py);
 risk6at= risk6*log(d_py);
 risk16at= risk16*log(d_py);
risk19at= risk19*log(d_py);
dmat=dm*log(d_py);
educationat=education*log(d_py);
bmiat=bmi*log(d_py);
whtrat=whtr*log(d_py);
proportionality_test: test agegrat,genderat,risk6at,risk16at,risk19at,dmat,educationat,bmiat,whtrat;*检验是否符合ph假定;
run; 
/*符合PH假定*/
proc phreg data=b10  COVOUT OUTEST=__RCS;
model d_py*cvd(0)=bmi  __1_1  gender agegr  education risk6 risk16 risk19 whtr dm;
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 20.20 24.030  28.40; 
 __1_1=((bmi-20.20)**3)*(bmi>20.20) 
     -((bmi-24.03)**3)*(bmi>24.03) 
     *(28.40-20.20)/(28.40-24.03) 
     +((bmi-28.40)**3)*(bmi>28.40) 
     *(24.03-20.20)/(28.40-24.03); 
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-20.20)##3)#(X>20.20) 
     -((X-24.03)##3)#(X>24.03) 
     #(28.40-20.20)/(28.40-24.03) 
     +((X-28.40)##3)#(X>28.40) 
     #(24.03-20.20)/(28.40-24.03) 
     -((REF-20.20)##3)#(REF>20.20) 
     +((REF-24.03)##3)#(REF>24.03)#(28.40-20.20)/(28.40-24.03) 
     -((REF-28.40)##3)#(REF>28.40)#(24.03-20.20)/(28.40-24.03); 
 XMAT=(X-REF)||S1;
 HV={ bmi __1_1 }; 
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
 CREATE __RCS_bmi_cvd VAR { F FE Z X };  APPEND;  CLOSE __RCS_bmi_cvd; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_bmi_cvd;
by x z;
run;
proc transpose data=__RCS_bmi_cvd out =bmi_cvd prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;
/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\cvd mortality" dpi=300;
ods graphics on/border=off imagename="BMI on CVD Mortality" imagefmt=tiff;
proc sgplot data=bmi_cvd noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.0231'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'BMI on CVD Mortality';
run;

proc means data=b10 p1 p10 p50 p90 p99 maxdec=2;
var whtr;
run;
/*符合PH假定*/
proc phreg data=b10  COVOUT OUTEST=__RCS;
model d_py*cvd(0)=bmi    gender agegr  education risk6 risk16 risk19 whtr __1_1 dm;
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.43 0.50  0.58; 
 __1_1=((whtr-0.43)**3)*(whtr>0.43) 
     -((whtr-0.5)**3)*(whtr>0.5) 
     *(0.58-0.43)/(0.58-0.5) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.5-0.43)/(0.58-0.5); 
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.43)##3)#(X>0.43) 
     -((X-0.5)##3)#(X>0.5) 
     #(0.58-0.43)/(0.58-0.5) 
     +((X-0.58)##3)#(X>0.58) 
     #(0.5-0.43)/(0.58-0.5) 
     -((REF-0.43)##3)#(REF>0.43) 
     +((REF-0.5)##3)#(REF>0.5)#(0.58-0.43)/(0.58-0.5) 
     -((REF-0.58)##3)#(REF>0.58)#(0.5-0.43)/(0.58-0.5); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_cvd VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_cvd; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr_cvd;
by x z;
run;
proc transpose data=__RCS_whtr_cvd out =whtr_cvd prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;
/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\cvd mortality" dpi=300;
ods graphics on/border=off imagename="Whtr on CVD Mortality" imagefmt=tiff;
proc sgplot data=whtr_cvd noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.4887'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'Whtr on CVD Mortality';
run;

/*分组分析*/
proc means data=b10 p1 p10 p50 p90 p99 maxdec=2;
var whtr;
class bmigr;
run;
proc phreg data=b10 COVOUT OUTEST=__RCS;
model d_py*cvd(0)= gender agegr  education risk6 risk16 risk19 whtr __1_1 dm;
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.37 0.41 0.46; 
 __1_1=((whtr-0.37)**3)*(whtr>0.37) 
     -((whtr-0.41)**3)*(whtr>0.41) 
     *(0.46-0.37)/(0.46-0.41) 
     +((whtr-0.46)**3)*(whtr>0.46) 
     *(0.41-0.37)/(0.46-0.41); 
	 where bmigr=1;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.5;     *p99 value for X-axis; 
 REF=0.4;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.37*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.37)##3)#(X>0.37) 
     -((X-0.41)##3)#(X>0.41) 
     #(0.46-0.37)/(0.46-0.41) 
     +((X-0.46)##3)#(X>0.46) 
     #(0.41-0.37)/(0.46-0.41) 
     -((REF-0.37)##3)#(REF>0.37) 
     +((REF-0.41)##3)#(REF>0.41)#(0.46-0.37)/(0.46-0.41) 
     -((REF-0.46)##3)#(REF>0.46)#(0.41-0.37)/(0.46-0.41); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_bmi1 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_bmi1; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whtr_bmi1;
by x z;
run;
proc transpose data=__RCS_whtr_bmi1 out =whtr_bmi1 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;
/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\cvd mortality" dpi=300;
ods graphics on/border=off imagename="BMI<18.5" imagefmt=tiff;
proc sgplot data=whtr_bmi1 noautolegend;
inset 'p-overall=0.1583' 'p-non-linearity=0.8074'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHTR' values=(0.3 to 0.5 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt ' BMI<18.5'justify=left 'A';
run;

proc phreg data=b10 COVOUT OUTEST=__RCS;
model d_py*cvd(0)= gender agegr  education risk6 risk16 risk19 whtr __1_1 dm;
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.42 0.47 0.52; 
 __1_1=((whtr-0.42)**3)*(whtr>0.42) 
     -((whtr-0.47)**3)*(whtr>0.47) 
     *(0.52-0.42)/(0.52-0.47) 
     +((whtr-0.52)**3)*(whtr>0.52) 
     *(0.47-0.42)/(0.52-0.47); 
	 where bmigr=2;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.6;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.42*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.42)##3)#(X>0.42) 
     -((X-0.47)##3)#(X>0.47) 
     #(0.52-0.42)/(0.52-0.47) 
     +((X-0.52)##3)#(X>0.52) 
     #(0.47-0.42)/(0.52-0.47) 
     -((REF-0.42)##3)#(REF>0.42) 
     +((REF-0.47)##3)#(REF>0.47)#(0.52-0.42)/(0.52-0.47) 
     -((REF-0.52)##3)#(REF>0.52)#(0.47-0.42)/(0.52-0.47); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_bmi2 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_bmi2; 
 QUIT; 

proc sort data=__RCS_whtr_bmi2;
by x z;
run;
proc transpose data=__RCS_whtr_bmi2 out =whtr_bmi2 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;
/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\cvd mortality" dpi=300;
ods graphics on/border=off imagename="18.5<=BMI<23.9" imagefmt=tiff;
proc sgplot data=whtr_bmi2 noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.7494'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHTR' values=(0.4 to 0.6 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt ' 18.5<=BMI<23.9'justify=left 'B';
run;


proc phreg data=b10 COVOUT OUTEST=__RCS;
model d_py*cvd(0)= gender agegr  education risk6 risk16 risk19 whtr __1_1 dm;
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.48 0.53 0.58; 
 __1_1=((whtr-0.48)**3)*(whtr>0.48) 
     -((whtr-0.53)**3)*(whtr>0.53) 
     *(0.58-0.48)/(0.58-0.53) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.53-0.48)/(0.58-0.53); 
	 where bmigr=3;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.48*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.48)##3)#(X>0.48) 
     -((X-0.53)##3)#(X>0.53) 
     #(0.58-0.48)/(0.58-0.53) 
     +((X-0.58)##3)#(X>0.58) 
     #(0.53-0.48)/(0.58-0.53) 
     -((REF-0.48)##3)#(REF>0.48) 
     +((REF-0.53)##3)#(REF>0.53)#(0.58-0.48)/(0.58-0.53) 
     -((REF-0.58)##3)#(REF>0.58)#(0.53-0.48)/(0.58-0.53); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_bmi3 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_bmi3; 
 QUIT; 

proc sort data=__RCS_whtr_bmi3;
by x z;
run;
proc transpose data=__RCS_whtr_bmi3 out =whtr_bmi3 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;
/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\cvd mortality" dpi=300;
ods graphics on/border=off imagename="24<=BMI<27.9" imagefmt=tiff;
proc sgplot data=whtr_bmi3 noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.7827'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt ' 24<=BMI<27.9'justify=left 'C';
run;


proc phreg data=b10 COVOUT OUTEST=__RCS;
model d_py*cvd(0)= gender agegr  education risk6 risk16 risk19 whtr __1_1 dm;
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.53 0.58 0.65; 
 __1_1=((whtr-0.53)**3)*(whtr>0.53) 
     -((whtr-0.58)**3)*(whtr>0.58) 
     *(0.65-0.53)/(0.65-0.58) 
     +((whtr-0.65)**3)*(whtr>0.65) 
     *(0.58-0.53)/(0.65-0.58); 
	 where bmigr=4;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.45;     *p1 value for X-axis; 
 UPPEREND=0.75;     *p99 value for X-axis; 
 REF=0.55;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.53)##3)#(X>0.53) 
     -((X-0.58)##3)#(X>0.58) 
     #(0.65-0.53)/(0.65-0.58) 
     +((X-0.65)##3)#(X>0.65) 
     #(0.58-0.53)/(0.65-0.58) 
     -((REF-0.53)##3)#(REF>0.53) 
     +((REF-0.58)##3)#(REF>0.58)#(0.65-0.53)/(0.65-0.58) 
     -((REF-0.65)##3)#(REF>0.65)#(0.58-0.53)/(0.65-0.58); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_bmi4 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_bmi4; 
 QUIT; 

proc sort data=__RCS_whtr_bmi4;
by x z;
run;
proc transpose data=__RCS_whtr_bmi4 out =whtr_bmi4 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;
/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\cvd mortality" dpi=300;
ods graphics on/border=off imagename="BMI>=28" imagefmt=tiff;
proc sgplot data=whtr_bmi4 noautolegend;
inset 'p-overall=0.0001' 'p-non-linearity=0.8244'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHTR' values=(0.45 to 0.75 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt ' BMI>=28'justify=left 'D';
run;
/*分析在糖尿病人群中肥胖等危险因素与全死因死亡的关系*/
proc freq data=b9;
tables dm;
run;
*糖尿病4239人;

data dm;
set b9;
if dm=1 then output;
run;
proc freq data=dm;
tables d;
run;
/*全死因死亡人数482人*/

proc means data=dm p50;
var whtr whr;
run;
/*取whr近似中位数*/
data dm;
set dm;
if whr<0.90 then binwhr=1;
else binwhr=2;
run;

*单因素分析;
PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender whr / RL; 
RUN;
/*腰臀比有意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') binwhtr(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender binwhr / RL; 
RUN;
/*分组腰臀比有意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender whtr / RL; 
RUN;
/*腰高比有意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') binwhtr(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender binwhtr / RL; 
RUN;
/*分组腰高比有意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender bmi / RL; 
RUN;
/*BMI无意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') risk1(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender risk1 / RL; 
RUN;
/*吸烟无意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') risk6(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender risk6 / RL; 
RUN;
/*饮酒无意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') education(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender education / RL; 
RUN;
/*教育水平有意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') quarterimcome(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender quarterimcome / RL; 
RUN;
/*收入水平有意义*/

PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') risk16(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender risk16 / RL; 
RUN;
PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') risk17(ref='2')/ref=first;
MODEL d_py*d(0) = agegr gender risk17 / RL; 
RUN;
/*油炸熏肉有意义*/
PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') risk18(ref='2')/ref=first;
MODEL d_py*d(0) = agegr gender risk18 / RL; 
RUN;
PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') risk19(ref='2')/ref=first;
MODEL d_py*d(0) = agegr gender risk19 / RL; 
RUN;
/*死亡人数*/
proc freq data=dm;
table d*binwhtr d*binwhr;
run;
/*计算随访人年*/
proc sql;
create table  dm_binwhr1 as
select 
sum(d_py) as sum_d_py
from dm where  binwhr=1;
quit;
proc sql;
create table  dm_binwhr2 as
select 
sum(d_py) as sum_d_py
from dm where  binwhr=2;
quit;
proc sql;
create table  dm_binwhtr1 as
select 
sum(d_py) as sum_d_py
from dm where  binwhtr=1;
quit;
proc sql;
create table  dm_binwhtr2 as
select 
sum(d_py) as sum_d_py
from dm where  binwhtr=2;
quit;
/*变量筛选*/
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') /ref=first;
MODEL d_py*d(0) =gender agegr bmi  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6 whr/selection=stepwise slstay=0.05 RL; 
RUN;
/*age gender education whr risk6 bmi */

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') /ref=first;
MODEL d_py*d(0) =gender agegr  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6 bmi whtr/selection=stepwise slstay=0.05 RL; 
RUN;
/*age gender education whtr risk6 bmi*/

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') /ref=first;
MODEL d_py*d(0) =gender agegr  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6 bmi|whtr/selection=stepwise slstay=0.05 RL; 
RUN;
/*bmi与whr 存在交互作用*/

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') /ref=first;
MODEL d_py*d(0) =gender agegr  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6 bmi|whtr/selection=stepwise slstay=0.05 RL; 
RUN;
/*bmi与whtr 存在交互作用*/

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  binwhr(ref='1') /ref=first;
MODEL d_py*d(0) =gender agegr bmi  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6 binwhr/selection=stepwise slstay=0.05 RL; 
RUN;
/*age gender education  quarterimcome whr risk6 */

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1')  binwhtr(ref='1')  education(ref='1') risk6(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr   binwhtr bmi risk6/selection=stepwise slstay=0.05 RL; 
RUN;
/*age gender  whtr risk6*/

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk6(ref='1')   binwhr(ref='1') /ref=first;
MODEL d_py*d(0) =gender agegr education quarterimcome risk6 binwhr/ RL; 
RUN;
/*age gender education  quarterimcome whr risk6 */

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1')  binwhtr(ref='1')  education(ref='1') risk6(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr   binwhtr risk6/ RL; 
RUN;
/*age gender  whtr risk6*/

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') /ref=first;
MODEL d_py*d(0) =gender agegr bmi  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6/selection=stepwise slstay=0.05 RL; 
RUN;

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') risk6(ref='1')   risk17(ref='2') /ref=first;
MODEL d_py*d(0) =gender agegr bmi  education risk17  risk6 whr/ RL; 
RUN;
/*age gender education risk17 whr risk6 bmi */



/*研究腰高比在糖尿病人群中对全死因死亡的影响*/
proc phreg data=dm;
class agegr(ref="1") education(ref="1") gender(ref='1') risk17(ref='2' ) risk6(ref='2');
model d_py*d(0)=bmi agegr gender education risk6    whtr 
bmiat agegrat genderat  educationat   risk6at whtrat/rl;
bmiat=bmi*log(d_py);
agegrat=agegr*log(d_py);
genderat=gender*log(d_py);
educationat=education*log(d_py);
 risk6at=risk6*log(d_py);
whtrat=whtr*log(d_py);
proportionality_test: test  bmiat,agegrat,genderat,educationat ,risk6at,whtrat;*检验是否符合ph假定;
run;

proc means data=dm p1 p10 p50 p90 p99 maxdec=2;
var whtr;
run;
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") gender(ref='1') risk6(ref='2') /ref=first;
MODEL d_py*d(0) =bmi agegr gender education risk6  whtr  __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 0.46 0.53 0.61; 
 __1_1=((whtr-0.46)**3)*(whtr>0.46) 
     -((whtr-0.53)**3)*(whtr>0.53) 
     *(0.61-0.46)/(0.61-0.53) 
     +((whtr-0.61)**3)*(whtr>0.61) 
     *(0.53-0.46)/(0.61-0.53); 

 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.46)##3)#(X>0.46) 
     -((X-0.53)##3)#(X>0.53) 
     #(0.61-0.46)/(0.61-0.53) 
     +((X-0.61)##3)#(X>0.61) 
     #(0.53-0.46)/(0.61-0.53) 
     -((REF-0.46)##3)#(REF>0.46) 
     +((REF-0.53)##3)#(REF>0.53)#(0.61-0.46)/(0.61-0.53) 
     -((REF-0.61)##3)#(REF>0.61)#(0.53-0.46)/(0.61-0.53); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr;
by x z;
run;
proc transpose data=__RCS_whtr out =dm_whtr prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
	ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
	ods graphics on/border=off imagename="All-cause Mortality in dm,WHTR" imagefmt=tiff;
	proc sgplot data=dm_whtr noautolegend;
	inset 'p-overall<.0001' 'p-non-linearity=0.1407'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
	refline 1/lineattrs=(pattern=dash color=black);
	band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
	series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
	xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
	yaxis label='HR (95%CI)' values=( 0 to 12 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
	/*keylegend 'HR' 'BMI'/position=top location=inside;*/
	title justify=medium height=14pt 'All-cause Mortality in dm,WHTR';
	run;

/*研究腰臀比在糖尿病人群中对全死因死亡的影响*/
proc phreg data=dm;
class agegr(ref="1") education(ref="1") gender(ref='1')risk6(ref='2');
model d_py*d(0)=bmi agegr gender education risk6 whr
bmiat agegrat genderat  educationat   risk6at  whrat/rl;
bmiat=bmi*log(d_py);
agegrat=agegr*log(d_py);
genderat=gender*log(d_py);
educationat=education*log(d_py);
 risk6at=risk6*log(d_py);
whrat=whr*log(d_py);
proportionality_test: test  bmiat,agegrat,genderat,educationat ,risk6at,whrat;*检验是否符合ph假定;
run;
proc means data=dm p1 p10 p50 p90 p99 maxdec=2;
var whr;
run;
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") gender(ref='1') risk6(ref='2');
model d_py*d(0)=bmi agegr gender education risk6  whr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.81 0.89  0.96; 
 __1_1=((whr-0.81)**3)*(whr>0.81) 
     -((whr-0.89)**3)*(whr>0.89) 
     *(0.96-0.81)/(0.96-0.89) 
     +((whr-0.96)**3)*(whr>0.96) 
     *(0.89-0.81)/(0.96-0.89); 
 *--------- Testing variable: whr ---------; 
 EFFECT1: TEST  whr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.75;     *p1 value for X-axis; 
 UPPEREND=1.05;     *p99 value for X-axis; 
 REF=0.90;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.81*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.81)##3)#(X>0.81) 
     -((X-0.89)##3)#(X>0.89) 
     #(0.96-0.81)/(0.96-0.89) 
     +((X-0.96)##3)#(X>0.96) 
     #(0.89-0.81)/(0.96-0.89) 
     -((REF-0.81)##3)#(REF>0.81) 
     +((REF-0.89)##3)#(REF>0.89)#(0.96-0.81)/(0.96-0.89) 
     -((REF-0.96)##3)#(REF>0.96)#(0.89-0.81)/(0.96-0.89); 
 XMAT=(X-REF)||S1;
 HV={ whr __1_1 }; 
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
 CREATE __RCS_whr VAR { F FE Z X };  APPEND;  CLOSE __RCS_whr; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whr;
by x z;
run;
proc transpose data=__RCS_whr out =dm_whr prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
ods graphics on/border=off imagename="All-cause Mortality in dm,WHR" imagefmt=tiff;
proc sgplot data=dm_whr noautolegend;
inset 'p-overall=0.0003' 'p-non-linearity=0.5270'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHR' values=(0.75 to 1.05 by 0.05) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 6 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All-cause Mortality in dm,WHR';
run;


/*分组分析*/
data dm;
set dm;
if bmi<24 then binbmi=1;
else binbmi=2;
run;


PROC PHREG DATA=dm; 
class  agegr(ref='1')  gender(ref='1') BINbmi(ref='1')/ref=first;
MODEL d_py*d(0) = agegr gender binbmi / RL; 
RUN;
/*BMI无意义*/

/*当WHTR与BMI共同存在时才有意义*/
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  binbmi(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr binbmi  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6 whr/selection=stepwise slstay=0.05 RL; 
RUN;
/*age gender education  quarterimcome whr risk6 bmi */
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class   gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') binbmi(ref='1') /ref=first;
MODEL d_py*d(0) =gender agegr  education quarterimcome risk16 risk17 risk18 risk19 risk1 risk6 binbmi whtr/selection=stepwise slstay=0.05 RL; 
RUN;
/*age gender education whtr risk6 bmi quarterimcome*/


proc means data=dm p1 p10 p50 p90 p99 maxdec=2;
var whtr;
class binbmi;
run;
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") gender(ref='1') risk6(ref='2')  quarterimcome(ref='1')/ref=first;
MODEL d_py*d(0) =agegr gender education risk6 quarterimcome  whtr  __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 0.44 0.48 0.53; 
 __1_1=((whtr-0.44)**3)*(whtr>0.44) 
     -((whtr-0.48)**3)*(whtr>0.48) 
     *(0.53-0.44)/(0.53-0.48) 
     +((whtr-0.53)**3)*(whtr>0.53) 
     *(0.48-0.44)/(0.53-0.48); 
where binbmi=1;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.44)##3)#(X>0.44) 
     -((X-0.48)##3)#(X>0.48) 
     #(0.53-0.44)/(0.53-0.48) 
     +((X-0.53)##3)#(X>0.53) 
     #(0.48-0.44)/(0.53-0.48) 
     -((REF-0.44)##3)#(REF>0.44) 
     +((REF-0.48)##3)#(REF>0.48)#(0.53-0.44)/(0.53-0.48) 
     -((REF-0.53)##3)#(REF>0.53)#(0.48-0.44)/(0.53-0.48); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_binbmi1 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_binbmi1; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr_binbmi1;
by x z;
run;
proc transpose data=__RCS_whtr_binbmi1 out =dm_whtr_binbmi1 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
	ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
	ods graphics on/border=off imagename="BMI<24" imagefmt=tiff;
	proc sgplot data=dm_whtr_binbmi1 noautolegend;
	inset 'p-overall=0.0493' 'p-non-linearity=0.4275'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
	refline 1/lineattrs=(pattern=dash color=black);
	band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
	series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
	xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
	yaxis label='HR (95%CI)' values=( 0 to 12 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
	/*keylegend 'HR' 'BMI'/position=top location=inside;*/
	title justify=medium height=14pt 'BMI<24' justify=left 'A';
	run;

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") gender(ref='1') risk6(ref='2')  quarterimcome(ref='1')/ref=first;
MODEL d_py*d(0) =agegr gender education risk6 quarterimcome  whtr  __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 0.50 0.55 0.63; 
 __1_1=((whtr-0.50)**3)*(whtr>0.50) 
     -((whtr-0.55)**3)*(whtr>0.55) 
     *(0.63-0.50)/(0.63-0.55) 
     +((whtr-0.63)**3)*(whtr>0.63) 
     *(0.55-0.50)/(0.63-0.55); 
where binbmi=2;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.50)##3)#(X>0.50) 
     -((X-0.55)##3)#(X>0.55) 
     #(0.63-0.50)/(0.63-0.55) 
     +((X-0.63)##3)#(X>0.63) 
     #(0.55-0.50)/(0.63-0.55) 
     -((REF-0.50)##3)#(REF>0.50) 
     +((REF-0.55)##3)#(REF>0.55)#(0.63-0.50)/(0.63-0.55) 
     -((REF-0.63)##3)#(REF>0.63)#(0.55-0.50)/(0.63-0.55); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_binbmi2 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_binbmi2; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr_binbmi2;
by x z;
run;
proc transpose data=__RCS_whtr_binbmi2 out =dm_whtr_binbmi2 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
	ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
	ods graphics on/border=off imagename="BMI>=24" imagefmt=tiff;
	proc sgplot data=dm_whtr_binbmi2 noautolegend;
	inset 'p-overall<.0001' 'p-non-linearity=0.7978'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
	refline 1/lineattrs=(pattern=dash color=black);
	band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
	series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
	xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
	yaxis label='HR (95%CI)' values=( 0 to 12 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
	/*keylegend 'HR' 'BMI'/position=top location=inside;*/
	title justify=medium height=14pt 'BMI>=24' justify=left 'B';
	run;

proc means data=dm p1 p10 p50 p90 p99 maxdec=2;
var whr;
class binbmi;
run;
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") gender(ref='1') risk6(ref='2') quarterimcome(ref='1');
model d_py*d(0)=agegr gender education risk6   quarterimcome whr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.80 0.86  0.93; 
 __1_1=((whr-0.80)**3)*(whr>0.80) 
     -((whr-0.86)**3)*(whr>0.86) 
     *(0.93-0.80)/(0.93-0.86) 
     +((whr-0.93)**3)*(whr>0.93) 
     *(0.86-0.80)/(0.93-0.86); 
	 where binbmi=1;
 *--------- Testing variable: whr ---------; 
 EFFECT1: TEST  whr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.75;     *p1 value for X-axis; 
 UPPEREND=1.05;     *p99 value for X-axis; 
 REF=0.90;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.80*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.80)##3)#(X>0.80) 
     -((X-0.86)##3)#(X>0.86) 
     #(0.93-0.80)/(0.93-0.86) 
     +((X-0.93)##3)#(X>0.93) 
     #(0.86-0.80)/(0.93-0.86) 
     -((REF-0.80)##3)#(REF>0.80) 
     +((REF-0.86)##3)#(REF>0.86)#(0.93-0.80)/(0.93-0.86) 
     -((REF-0.93)##3)#(REF>0.93)#(0.86-0.80)/(0.93-0.86); 
 XMAT=(X-REF)||S1;
 HV={ whr __1_1 }; 
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
 CREATE __RCS_whr_binbmi1 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whr_binbmi1; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whr_binbmi1;
by x z;
run;
proc transpose data=__RCS_whr_binbmi1 out =dm_whr_binbmi1 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
ods graphics on/border=off imagename="BMI<24" imagefmt=tiff;
proc sgplot data=dm_whr_binbmi1 noautolegend;
inset 'p-overall=0.0714' 'p-non-linearity=0.6980'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHR' values=(0.75 to 1.05 by 0.05) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 6 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'BMI<24'justify=left 'A';
run;


PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") gender(ref='1') risk6(ref='2') quarterimcome(ref='1');
model d_py*d(0)=agegr gender education risk6   quarterimcome whr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.83 0.90  0.97; 
 __1_1=((whr-0.83)**3)*(whr>0.83) 
     -((whr-0.90)**3)*(whr>0.90) 
     *(0.97-0.83)/(0.97-0.90) 
     +((whr-0.97)**3)*(whr>0.97) 
     *(0.90-0.83)/(0.97-0.90); 
	 where binbmi=2;
 *--------- Testing variable: whr ---------; 
 EFFECT1: TEST  whr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.75;     *p1 value for X-axis; 
 UPPEREND=1.05;     *p99 value for X-axis; 
 REF=0.90;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.83*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.83)##3)#(X>0.83) 
     -((X-0.90)##3)#(X>0.90) 
     #(0.97-0.83)/(0.97-0.90) 
     +((X-0.97)##3)#(X>0.97) 
     #(0.90-0.83)/(0.97-0.90) 
     -((REF-0.83)##3)#(REF>0.83) 
     +((REF-0.90)##3)#(REF>0.90)#(0.97-0.83)/(0.97-0.90) 
     -((REF-0.97)##3)#(REF>0.97)#(0.90-0.83)/(0.97-0.90); 
 XMAT=(X-REF)||S1;
 HV={ whr __1_1 }; 
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
 CREATE __RCS_whr_binbmi2 VAR { F FE Z X };  APPEND;  CLOSE __RCS_whr_binbmi2; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whr_binbmi2;
by x z;
run;
proc transpose data=__RCS_whr_binbmi2 out =dm_whr_binbmi2 prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
ods graphics on/border=off imagename="BMI>=24" imagefmt=tiff;
proc sgplot data=dm_whr_binbmi2 noautolegend;
inset 'p-overall=0.0015' 'p-non-linearity=0.8371'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHR' values=(0.75 to 1.05 by 0.05) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 6 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'BMI>=24'justify=left 'B';
run;

/*whtr gender*/
proc means data=dm p1 p10 p50 p90 p99 maxdec=2;
var whtr whr;
class gender;
run;
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1")  risk6(ref='2') /ref=first;
MODEL d_py*d(0) =bmi agegr  education risk6  whtr  __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 0.46 0.53 0.59; 
 __1_1=((whtr-0.46)**3)*(whtr>0.46) 
     -((whtr-0.53)**3)*(whtr>0.53) 
     *(0.59-0.46)/(0.59-0.53) 
     +((whtr-0.59)**3)*(whtr>0.59) 
     *(0.53-0.46)/(0.59-0.53); 
where gender=1;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.46)##3)#(X>0.46) 
     -((X-0.53)##3)#(X>0.53) 
     #(0.59-0.46)/(0.59-0.53) 
     +((X-0.59)##3)#(X>0.59) 
     #(0.53-0.46)/(0.59-0.53) 
     -((REF-0.46)##3)#(REF>0.46) 
     +((REF-0.53)##3)#(REF>0.53)#(0.59-0.46)/(0.59-0.53) 
     -((REF-0.59)##3)#(REF>0.59)#(0.53-0.46)/(0.59-0.53); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_men VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_men; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr_men;
by x z;
run;
proc transpose data=__RCS_whtr_men out =dm_whtr_men prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
	ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
	ods graphics on/border=off imagename="MEN" imagefmt=tiff;
	proc sgplot data=dm_whtr_men noautolegend;
	inset 'p-overall<.0001' 'p-non-linearity=0.1709'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
	refline 1/lineattrs=(pattern=dash color=black);
	band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
	series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
	xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
	yaxis label='HR (95%CI)' values=( 0 to 12 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
	/*keylegend 'HR' 'BMI'/position=top location=inside;*/
	title justify=medium height=14pt 'MEN' justify=left 'A';
	run;



	PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1")  risk6(ref='2') /ref=first;
MODEL d_py*d(0) =bmi agegr  education risk6  whtr  __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 0.46 0.53 0.62; 
 __1_1=((whtr-0.46)**3)*(whtr>0.46) 
     -((whtr-0.53)**3)*(whtr>0.53) 
     *(0.62-0.46)/(0.62-0.53) 
     +((whtr-0.62)**3)*(whtr>0.62) 
     *(0.53-0.46)/(0.62-0.53); 
where gender=2;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.46)##3)#(X>0.46) 
     -((X-0.53)##3)#(X>0.53) 
     #(0.62-0.46)/(0.62-0.53) 
     +((X-0.62)##3)#(X>0.62) 
     #(0.53-0.46)/(0.62-0.53) 
     -((REF-0.46)##3)#(REF>0.46) 
     +((REF-0.53)##3)#(REF>0.53)#(0.62-0.46)/(0.62-0.53) 
     -((REF-0.62)##3)#(REF>0.62)#(0.53-0.46)/(0.62-0.53); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_WOMEN VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_WOMEN; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr_WOMEN;
by x z;
run;
proc transpose data=__RCS_whtr_WOMEN out =dm_whtr_WOMEN prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
	ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
	ods graphics on/border=off imagename="WOMEN" imagefmt=tiff;
	proc sgplot data=dm_whtr_WOMEN noautolegend;
	inset 'p-overall=0.0001' 'p-non-linearity=0.8720'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
	refline 1/lineattrs=(pattern=dash color=black);
	band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
	series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
	xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
	yaxis label='HR (95%CI)' values=( 0 to 12 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
	/*keylegend 'HR' 'BMI'/position=top location=inside;*/
	title justify=medium height=14pt 'WOMEN' justify=left 'B';
	run;

/*whr gender*/
	PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") risk6(ref='2');
model d_py*d(0)=bmi agegr  education risk6  whr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.85 0.91  0.97; 
 __1_1=((whr-0.85)**3)*(whr>0.85) 
     -((whr-0.91)**3)*(whr>0.91) 
     *(0.97-0.85)/(0.97-0.91) 
     +((whr-0.97)**3)*(whr>0.97) 
     *(0.91-0.85)/(0.97-0.91); 
	 where gender=1;
 *--------- Testing variable: whr ---------; 
 EFFECT1: TEST  whr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.75;     *p1 value for X-axis; 
 UPPEREND=1.05;     *p99 value for X-axis; 
 REF=0.90;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.85*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.85)##3)#(X>0.85) 
     -((X-0.91)##3)#(X>0.91) 
     #(0.97-0.85)/(0.97-0.91) 
     +((X-0.97)##3)#(X>0.97) 
     #(0.91-0.85)/(0.97-0.91) 
     -((REF-0.85)##3)#(REF>0.85) 
     +((REF-0.91)##3)#(REF>0.91)#(0.97-0.85)/(0.97-0.91) 
     -((REF-0.97)##3)#(REF>0.97)#(0.91-0.85)/(0.97-0.91); 
 XMAT=(X-REF)||S1;
 HV={ whr __1_1 }; 
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
 CREATE __RCS_whr_men VAR { F FE Z X };  APPEND;  CLOSE __RCS_whr_men; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whr_men;
by x z;
run;
proc transpose data=__RCS_whr_men out =dm_whr_men prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
ods graphics on/border=off imagename="MEN" imagefmt=tiff;
proc sgplot data=dm_whr_men noautolegend;
inset 'p-overall=0.0137' 'p-non-linearity=0.5072'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHR' values=(0.75 to 1.05 by 0.05) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 6 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'MEN' justify=left 'A';
run;

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class agegr(ref="1") education(ref="1") risk6(ref='2');
model d_py*d(0)=bmi agegr  education risk6  whr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.8 0.87  0.94; 
 __1_1=((whr-0.8)**3)*(whr>0.8) 
     -((whr-0.87)**3)*(whr>0.87) 
     *(0.94-0.8)/(0.94-0.87) 
     +((whr-0.94)**3)*(whr>0.94) 
     *(0.87-0.8)/(0.94-0.87); 
	 where gender=2;
 *--------- Testing variable: whr ---------; 
 EFFECT1: TEST  whr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.75;     *p1 value for X-axis; 
 UPPEREND=1.05;     *p99 value for X-axis; 
 REF=0.90;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.8*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.8)##3)#(X>0.8) 
     -((X-0.87)##3)#(X>0.87) 
     #(0.94-0.8)/(0.94-0.87) 
     +((X-0.94)##3)#(X>0.94) 
     #(0.87-0.8)/(0.94-0.87) 
     -((REF-0.8)##3)#(REF>0.8) 
     +((REF-0.87)##3)#(REF>0.87)#(0.94-0.8)/(0.94-0.87) 
     -((REF-0.94)##3)#(REF>0.94)#(0.87-0.8)/(0.94-0.87); 
 XMAT=(X-REF)||S1;
 HV={ whr __1_1 }; 
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
 CREATE __RCS_whr_women VAR { F FE Z X };  APPEND;  CLOSE __RCS_whr_women; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whr_women;
by x z;
run;
proc transpose data=__RCS_whr_women out =dm_whr_women prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
ods graphics on/border=off imagename="WOMEN" imagefmt=tiff;
proc sgplot data=dm_whr_women noautolegend;
inset 'p-overall=0.0067' 'p-non-linearity=0.3994'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHR' values=(0.75 to 1.05 by 0.05) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 6 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'WOMEN' justify=left 'B';
run;

/*agegr whtr*/
proc means data=dm p1 p10 p50 p90 p99 maxdec=2;
var whtr whr;
class agegr;
run;
PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class gender(ref="1") education(ref="1")  risk6(ref='2') /ref=first;
MODEL d_py*d(0) =bmi gender  education risk6  whtr  __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 0.46 0.52 0.59; 
 __1_1=((whtr-0.46)**3)*(whtr>0.46) 
     -((whtr-0.52)**3)*(whtr>0.52) 
     *(0.59-0.46)/(0.59-0.52) 
     +((whtr-0.59)**3)*(whtr>0.59) 
     *(0.52-0.46)/(0.59-0.52); 
where agegr=1;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.46)##3)#(X>0.46) 
     -((X-0.52)##3)#(X>0.52) 
     #(0.59-0.46)/(0.59-0.52) 
     +((X-0.59)##3)#(X>0.59) 
     #(0.52-0.46)/(0.59-0.52) 
     -((REF-0.46)##3)#(REF>0.46) 
     +((REF-0.52)##3)#(REF>0.52)#(0.59-0.46)/(0.59-0.52) 
     -((REF-0.59)##3)#(REF>0.59)#(0.52-0.46)/(0.59-0.52); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_young VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_young; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr_young;
by x z;
run;
proc transpose data=__RCS_whtr_young out =dm_whtr_young prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
	ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
	ods graphics on/border=off imagename="age<60岁" imagefmt=tiff;
	proc sgplot data=dm_whtr_young noautolegend;
	inset 'p-overall=0.5155' 'p-non-linearity=0.7053'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
	refline 1/lineattrs=(pattern=dash color=black);
	band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
	series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
	xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
	yaxis label='HR (95%CI)' values=( 0 to 12 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
	/*keylegend 'HR' 'BMI'/position=top location=inside;*/
	title justify=medium height=14pt 'age<60岁' justify=left 'A';
	run;



	PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class gender(ref="1") education(ref="1")  risk6(ref='2') /ref=first;
MODEL d_py*d(0) =bmi gender  education risk6  whtr  __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 0.47 0.54 0.62; 
 __1_1=((whtr-0.47)**3)*(whtr>0.47) 
     -((whtr-0.54)**3)*(whtr>0.54) 
     *(0.62-0.47)/(0.62-0.54) 
     +((whtr-0.62)**3)*(whtr>0.62) 
     *(0.54-0.47)/(0.62-0.54); 
where agegr=2;
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.4;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.43*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.47)##3)#(X>0.47) 
     -((X-0.54)##3)#(X>0.54) 
     #(0.62-0.47)/(0.62-0.54) 
     +((X-0.62)##3)#(X>0.62) 
     #(0.54-0.47)/(0.62-0.54) 
     -((REF-0.47)##3)#(REF>0.47) 
     +((REF-0.54)##3)#(REF>0.54)#(0.62-0.47)/(0.62-0.54) 
     -((REF-0.62)##3)#(REF>0.62)#(0.54-0.47)/(0.62-0.54); 
 XMAT=(X-REF)||S1;
 HV={ whtr __1_1 }; 
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
 CREATE __RCS_whtr_old VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_old; 
 QUIT; 


/*作图*/ 
proc sort data=__RCS_whtr_old;
by x z;
run;
proc transpose data=__RCS_whtr_old out =dm_whtr_old prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
	ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
	ods graphics on/border=off imagename="age>=60岁" imagefmt=tiff;
	proc sgplot data=dm_whtr_old noautolegend;
	inset 'p-overall<.0001' 'p-non-linearity=0.1387'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
	refline 1/lineattrs=(pattern=dash color=black);
	band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
	series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
	xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
	yaxis label='HR (95%CI)' values=( 0 to 12 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
	/*keylegend 'HR' 'BMI'/position=top location=inside;*/
	title justify=medium height=14pt 'age>=60岁' justify=left 'B';
	run;


	
/*whr agegr*/
	PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class gender(ref="1") education(ref="1") risk6(ref='2');
model d_py*d(0)=bmi gender  education risk6  whr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.81 0.88  0.95; 
 __1_1=((whr-0.81)**3)*(whr>0.81) 
     -((whr-0.88)**3)*(whr>0.88) 
     *(0.95-0.81)/(0.95-0.88) 
     +((whr-0.95)**3)*(whr>0.95) 
     *(0.88-0.81)/(0.95-0.88); 
	 where agegr=1;
 *--------- Testing variable: whr ---------; 
 EFFECT1: TEST  whr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.75;     *p1 value for X-axis; 
 UPPEREND=1.05;     *p99 value for X-axis; 
 REF=0.90;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.81*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.81)##3)#(X>0.81) 
     -((X-0.88)##3)#(X>0.88) 
     #(0.95-0.81)/(0.95-0.88) 
     +((X-0.95)##3)#(X>0.95) 
     #(0.88-0.81)/(0.95-0.88) 
     -((REF-0.81)##3)#(REF>0.81) 
     +((REF-0.88)##3)#(REF>0.88)#(0.95-0.81)/(0.95-0.88) 
     -((REF-0.95)##3)#(REF>0.95)#(0.88-0.81)/(0.95-0.88); 
 XMAT=(X-REF)||S1;
 HV={ whr __1_1 }; 
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
 CREATE __RCS_whr_young VAR { F FE Z X };  APPEND;  CLOSE __RCS_whr_young; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whr_young;
by x z;
run;
proc transpose data=__RCS_whr_young out =dm_whr_young prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
ods graphics on/border=off imagename="age<60岁" imagefmt=tiff;
proc sgplot data=dm_whr_young noautolegend;
inset 'p-overall=0.2995' 'p-non-linearity=0.5539'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHR' values=(0.75 to 1.05 by 0.05) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 6 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'age<60岁' justify=left 'A';
run;

PROC PHREG DATA=dm COVOUT OUTEST=__RCS; 
class gender(ref="1") education(ref="1") risk6(ref='2');
model d_py*d(0)=bmi gender  education risk6  whr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 0.8 0.89  0.96; 
 __1_1=((whr-0.82)**3)*(whr>0.82) 
     -((whr-0.89)**3)*(whr>0.89) 
     *(0.96-0.82)/(0.96-0.89) 
     +((whr-0.96)**3)*(whr>0.96) 
     *(0.89-0.82)/(0.96-0.89); 
	 where agegr=2;
 *--------- Testing variable: whr ---------; 
 EFFECT1: TEST  whr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.75;     *p1 value for X-axis; 
 UPPEREND=1.05;     *p99 value for X-axis; 
 REF=0.90;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.8*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-0.82)##3)#(X>0.82) 
     -((X-0.89)##3)#(X>0.89) 
     #(0.96-0.82)/(0.96-0.89) 
     +((X-0.96)##3)#(X>0.96) 
     #(0.89-0.82)/(0.96-0.89) 
     -((REF-0.82)##3)#(REF>0.82) 
     +((REF-0.89)##3)#(REF>0.89)#(0.96-0.82)/(0.96-0.89) 
     -((REF-0.96)##3)#(REF>0.96)#(0.89-0.82)/(0.96-0.89); 
 XMAT=(X-REF)||S1;
 HV={ whr __1_1 }; 
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
 CREATE __RCS_whr_old VAR { F FE Z X };  APPEND;  CLOSE __RCS_whr_old; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whr_old;
by x z;
run;
proc transpose data=__RCS_whr_old out =dm_whr_old prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\dm(All-cause mortality)" dpi=300;
ods graphics on/border=off imagename="age>=60岁" imagefmt=tiff;
proc sgplot data=dm_whr_old noautolegend;
inset 'p-overall=0.0009' 'p-non-linearity=0.6727'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHR' values=(0.75 to 1.05 by 0.05) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 6 by 1) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'age>=60岁' justify=left 'B';
run;


/*生存曲线图*/
*总人群分析;
proc lifetest data=dm method=km outsurv=estimatesm;
time d_py*d(0);
strata binwhtr;
run;
*logrank p=0.0024;
proc contents data=estimatesm;
run;
data estimatesm;
length subtype $15.;
set estimatesm;
survt_year=d_py;
if binwhtr=1 then subtype='WhtR<0.5';
if binwhtr=2 then subtype='WhtR≥0.5';
run;
data line;
input id $ value $10. linecolor :$20.;
datalines;      
group WhtR<0.5    darkorange Bold   
group WhtR≥0.5    black      
;
run;
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\survival curve" dpi=300;
ods graphics/border=off imagename="All-cause mortality in dm,by WHtR " imagefmt=tiff;
proc sgplot data=estimatesm  dattrmap=line ;
step x=survt_year y=survival/group=subtype attrid=group lineattrs=(thickness=2);
symbol1 line=1 width=1.5;
symbol2 line=2 width=1.5;
yaxis values=(0.6  0.7  0.8   0.9  1) label='Cumulative survival'  LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
xaxis label='Years from Registry'  LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
keylegend /location=inside position=bottomleft;
inset 'Log-rank chi-square=9.2406, P=0.0024'/position=left textattrs=(Family=Arial Size=9 Style=Italic Weight=Bold);
title justify=medium height=14pt   'All-cause mortality in dm,by WHtR';
run;
ods graphics off;


/*生存曲线图*/
*总人群分析;
proc lifetest data=dm method=km outsurv=estimatesm;
time d_py*d(0);
strata binwhr;
run;
*logrank p=0.0024;
proc contents data=estimatesm;
run;
data estimatesm;
length subtype $15.;
set estimatesm;
survt_year=d_py;
if binwhr=1 then subtype='WhR<0.9';
if binwhr=2 then subtype='WhR≥0.9';
run;
data line;
input id $ value $10. linecolor :$20.;
datalines;      
group WhR<0.9    darkorange Bold   
group WhR≥0.9    black      
;
run;
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\survival curve" dpi=300;
ods graphics/border=off imagename="All-cause mortality in dm,by WHR " imagefmt=tiff;
proc sgplot data=estimatesm  dattrmap=line ;
step x=survt_year y=survival/group=subtype attrid=group lineattrs=(thickness=2);
symbol1 line=1 width=1.5;
symbol2 line=2 width=1.5;
yaxis values=(0.6  0.7  0.8   0.9  1) label='Cumulative survival'  LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
xaxis label='Years from Registry'  LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
keylegend /location=inside position=bottomleft;
inset 'Log-rank chi-square=11.1250, P=0.0009'/position=left textattrs=(Family=Arial Size=9 Style=Italic Weight=Bold);
title justify=medium height=14pt   'All-cause mortality in dm,by WHR';
run;
ods graphics off;


