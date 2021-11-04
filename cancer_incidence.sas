/*研究肿瘤发病与肿瘤发病的关联*/
libname cao "D:\1a\learning\research\colorectal cancer\result\sas,data,program";
data a1;
set cao.crc_info;
run;
proc contents data=a1 varnum;
run;
/*查看肿瘤发病人数为1425人 频数缺失397人*/
proc freq data=a1;
tables cancer;
run;
proc univariate data=a1;
var ca_py;
run;
proc univariate data=a1;
histogram ca_py cancer;
var ca_py cancer;
run;
/*生成年龄、腰臀比、腰高比*/
data a3;
set a2;
whr=waist/hip;
whtr=waist/height;
age=(screenday-birthday)/365.25;
run;

/*将危险因素变量重新进行分类*/
data a4;
set a3;
if education=5 then education=4;
if quarterimcome=2 then quarterimcome=1;
if quarterimcome=3 then quarterimcome=2;
if quarterimcome=4|quarterimcome=5|quarterimcome=6 then quarterimcome=3;
if risk1=3 then risk1=2;
if risk6=3 then risk6=2;
if risk16=2|risk16=3 then risk16=1;
if risk16=4 then risk16=2;
if risk16=5 then risk16=.;
if risk17=2|risk17=3 then risk17=1;
if risk17=4 then risk17=2;
if risk17=5 then risk17=.;
if risk18=2|risk18=3 then risk18=1;
if risk18=4 then risk18=2;
if risk18=5 then risk18=.;
if risk19=2 then risk19=1;
if risk19=3|risk19=4 then risk19=2;
run;
/*添加变量标签*/
data a5 ;
set a4;
label education="1：小学 2：初中 3：高中、中专、技校 4：大专及以上";
label quarterimcome="1:<2000 2:2000-4000 3:>4000";
label risk1="1:不吸烟 2:吸烟";
label risk6="1:不饮酒 2:饮酒";
label risk16="每周饮食脂肪,肥肉 1：至少有一天2：一次都没有";
label risk17="油炸熏肉1：至少有一天2：一次都没有";
label risk18="咸菜/酱菜/泡菜/咸鱼/咸肉登1：至少有一天2：一次都没有";
label risk19="平均每天吃多少蔬菜水果1：300g以下2:300g以上";
run;
/*确定以下变量的分位数*/
proc means data=a5 p25 p50 p75 maxdec=2;
var waist hip whr whtr;
run;
/*将连续型变量根据分位数进行分类*/
data a5;
set a5;
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
if bmi<18.50 then bmigr=1;
if bmi>=18.50 and bmi<24 then bmigr=2;
if bmi>=24 and bmi<28 then bmigr=3; 
if bmi>=28 then bmigr=4;
if age<60 then agegr=1;
else agegr=2;
run;
data a5;
set a5;
if whtr<0.5 then binwhtr=1;
else binwhtr=2;
run;
*单因素分析,bmi等变量以连续性纳入;
PROC PHREG DATA=a5; 
class gender(ref='1')  /ref=first;
MODEL ca_py*cancer(0) =gender agegr bmi  / RL; 
RUN;
*BMI有意义;
PROC PHREG DATA=a5; 
class gender(ref='1')    /ref=first;
MODEL ca_py*cancer(0) =gender agegr whr/ RL; 
RUN;
*腰臀比有意义;
PROC PHREG DATA=a5; 
class gender(ref='1')   /ref=first;
MODEL ca_py*cancer(0) =gender agegr whtr / RL; 
RUN;
*腰高比有意义;
PROC PHREG DATA=a5; 
class gender(ref='1') education(ref='1') /ref=first;
MODEL ca_py*cancer(0) =gender age education / RL; 
RUN;
*教育水平无意义;
PROC PHREG DATA=a5; 
class gender(ref='1')   risk1(ref='1') /ref=first;
MODEL ca_py*cancer(0) = gender age risk1 / RL; 
RUN;
*吸烟无意义;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk6(ref='1') /ref=first;
MODEL ca_py*cancer(0) = gender age risk6 / RL; 
RUN;
*饮酒无意义;
PROC PHREG DATA=a5; 
class gender(ref='1')  quarterimcome(ref='1');
MODEL ca_py*cancer(0) =gender age quarterimcome / RL; 
RUN;
*家庭收入无意义;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk16(ref='2');
MODEL ca_py*cancer(0) =gender age risk16/ RL; 
RUN;
*每周饮食脂肪,肥肉无意义;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk17(ref='2') 
MODEL ca_py*cancer(0) =gender age risk17/ RL; 
RUN;
*油炸熏肉无意义;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk18(ref='2') /ref=first;
MODEL ca_py*cancer(0) =gender age  risk18/ RL; 
RUN;
*咸菜/酱菜/泡菜/咸鱼/咸肉无意义;
PROC PHREG DATA=a5; 
class gender(ref='1') risk19(ref='2')/ref=first;
MODEL ca_py*cancer(0) =gender age risk19/ RL; 
RUN;
*蔬菜水果有意义;
PROC PHREG DATA=a5; 
class gender(ref='1')   dm(ref='0') /ref=first;
MODEL ca_py*cancer(0) =gender age dm/ RL; 
RUN;
*糖尿病有意义;
*有意义的变量 BMI 、腰高比 、蔬菜水果、糖尿病;

proc corr data=a5 ;
var  whtr bmi;
run;
proc gplot data=a5;
plot whtr*bmi;
title"scatter plot of whtr and bmi";
run;
/*筛选变量*/
PROC PHREG DATA=a5; 
class gender(ref='1')   risk19(ref='2') dm(ref='0')  risk1(ref='1')  risk6(ref='1') agegr(ref='1');
MODEL ca_py*cancer(0) =gender agegr    risk1  risk6   risk19  dm whtr/ selection=stepwise sle=0.05 sls=0.05 RL; 
RUN;
*  agegr whtr risk1 dm;
PROC PHREG DATA=a5; 
class gender(ref='1')   risk19(ref='2') dm(ref='0')  risk1(ref='1')  risk6(ref='1') risk19(ref='1') agegr(ref='1');
MODEL ca_py*cancer(0) =gender agegr  risk19  risk1   dm bmi/ selection=stepwise sle=0.05 sls=0.05 RL; 
RUN;
*  agegr risk19 risk1 dm bmi;

PROC PHREG DATA=a5; 
class gender(ref='1')   risk19(ref='2') dm(ref='0')  risk1(ref='1')  risk6(ref='1') agegr(ref='1');
MODEL ca_py*cancer(0) =gender agegr    risk1  risk19  dm whtr/RL; 
RUN;
*  agegr whtr risk1 dm;

/*研究腰高比对全肿瘤发病的影响*/
proc phreg data=a5;
class  gender(ref='1')   risk19(ref='2') dm(ref='0') risk1(ref='1') agegr(ref='1');
model ca_py*cancer(0)= agegr gender risk1  dm whtr
agegrat genderat  risk1at   dmat  whtrat/rl;
agegrat=agegr*log(ca_py);
genderat=gender*log(ca_py);
 risk1at= risk1*log(ca_py);
dmat=dm*log(ca_py);
whtrat=whtr*log(ca_py);
proportionality_test: test agegrat,genderat,risk1at,dmat,whtrat;*检验是否符合ph假定;
run;
/*满足PH假定*/
proc means data=a5 p1  p10 p50 p90  p99 maxdec=2;
var whtr whr bmi;
run;

PROC PHREG DATA=a5 COVOUT OUTEST=__RCS; 
class agegr(ref="1") gender(ref='1')   risk19(ref='2') dm(ref='0') risk1(ref='1') agegr(ref='1');
model ca_py*cancer(0)=whtr agegr gender risk1 dm  __1_1/RL; 
 
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
 CREATE __RCS_whtr_all VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_all; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whtr_all;
by x z;
run;
proc transpose data=__RCS_whtr_all out =whtr_all prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;
/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\incidence of cancer" dpi=300;
ods graphics on/border=off imagename="Incidence of All Cancer,WHTR" imagefmt=tiff;
proc sgplot data=whtr_all noautolegend;
inset 'p-overall=0.0118' 'p-non-linearity=0.8243'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.6 0.8 1 1.2 1.4 1.6 1.8 2.0 2.2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'Incidence of All Cancer,WHTR';
run;
*  agegr risk19 risk1 dm bmi;
/*研究BMI对肿瘤发病的影响*/
PROC PHREG DATA=a5  COVOUT OUTEST=__RCS; 
model ca_py*cancer(0)= agegr gender risk1  dm risk19 bmi 
bmiat/rl;
bmiat=bmi*log(ca_py);
proportionality_test: test bmiat;*检验是否符合ph假定;
run;
/*不符合PH假定*/
proc means data=a5 p1 p10 p50 p90 p99 maxdec=2;
var ca_py;
run;
proc phreg data=a5 COVOUT OUTEST=__RCS;
 model ca_py*cancer(0)=agegr gender bmi  risk1 dm risk19 __4_1 __4_lin;
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 6.61 8.59 9.73; 

__4_1=((ca_py-6.61)**3)*(ca_py>6.61) 
     -((ca_py-8.59)**3)*(ca_py>8.59) 
     *(9.73-6.61)/(9.73-8.59) 
     +((ca_py-9.73)**3)*(ca_py>9.73) 
     *(8.59-6.61)/(9.73-8.59);
__4_1=__4_1*bmi;
__4_lin=bmi*ca_py;
 EFFECT4: TEST bmi, __4_LIN, __4_1;
 NONCON4: TEST  __4_LIN, __4_1;
 NONLIN4: TEST  __4_1;
 run;
 /*符合PH假定*/
 
proc means data=a5 p1 p10 p50 p90  p99 maxdec=2;
var bmi;
run;
PROC PHREG DATA=a5 COVOUT OUTEST=__RCS; 
class agegr(ref="1") gender(ref='1')   risk19(ref='2') dm(ref='0') risk1(ref='1') agegr(ref='1');
model ca_py*cancer(0)=bmi agegr gender risk1 risk19 dm  __1_1/RL; 
 
 ********** spline modelling of fixed covariate whr; 
 ********** with 3 knots located at; 
 ********** 20.20 24.03  28.40; 
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
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.89*/
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
 HV={ BMI __1_1 }; 
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
 CREATE __RCS_BMI VAR { F FE Z X };  APPEND;  CLOSE __RCS_BMI; 
 QUIT; 
/*中国BMI分类标准，大于等于24为肥胖*/
/*作图*/ 
proc sort data=__RCS_BMI;
by x z;
run;
proc transpose data=__RCS_BMI out =BMI prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\incidence of cancer" dpi=300;
ods graphics on/border=off imagename="'Incidence of All Cancer,BMI" imagefmt=tiff;
proc sgplot data=BMI noautolegend;
inset 'p-overall=0.0132' 'p-non-linearity=0.1040'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.6 0.8 1 1.2 1.4 1.6 1.8 2.0 2.2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'Incidence of All Cancer,BMI';
run;


/*分性别做单因素分析*/
data men women;
set a5;
if gender=1 then output men;
if gender=2 then  output women;
run;
/*分析男性危险因素与肿瘤发病的关系*/
PROC PHREG DATA=men; 
class  agegr(ref='1') bmigr(ref='2')/ref=first;
MODEL ca_py*cancer(0) = agegr bmigr  / RL; 
RUN;
*BMI无意义;
PROC PHREG DATA=men; 
class  agegr(ref='1')  waistgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  waistgr / RL; 
RUN;
*腰围无意义;
PROC PHREG DATA=men; 
class  agegr(ref='1')  hipgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  hipgr / RL; 
RUN;
*臀围无意义;
PROC PHREG DATA=men; 
class   agegr(ref='1')  whrgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  whrgr / RL; 
RUN;
*腰臀比无意义;
PROC PHREG DATA=men; 
class  agegr(ref='1')  whtrgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  whtrgr / RL; 
RUN;
*腰高比有意义;
PROC PHREG DATA=men; 
class  education(ref='1') agegr(ref='1')  /ref=first;
MODEL ca_py*cancer(0) = agegr education / RL; 
RUN;
*教育水平有意义;
PROC PHREG DATA=men; 
class  risk1(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk1 / RL; 
RUN;
*吸烟有意义;
PROC PHREG DATA=men; 
class  risk6(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk6 / RL; 
RUN;
*饮酒无意义;
PROC PHREG DATA=men; 
class   quarterimcome(ref='1') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr quarterimcome / RL; 
RUN;
*家庭收入无意义;
PROC PHREG DATA=men; 
class risk16(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk16/ RL; 
RUN;
*每周饮食脂肪,肥肉无意义;
PROC PHREG DATA=men; 
class  risk17(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk17/ RL; 
RUN;
*油炸熏肉无意义;
PROC PHREG DATA=men; 
class risk18(ref='2') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  risk18/ RL; 
RUN;
*咸菜/酱菜/泡菜/咸鱼/咸肉有意义;
PROC PHREG DATA=men; 
class  agegr(ref='1') risk19(ref='2')/ref=first;
MODEL ca_py*cancer(0) = agegr risk19/ RL; 
RUN;
*蔬菜水果有意义;
PROC PHREG DATA=men; 
class  dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr dm/ RL; 
RUN;
*糖尿病有意义;
/*有意义的变量 年龄、腰高比、教育水平、吸烟、咸菜/酱菜/泡菜/咸鱼/咸肉、蔬菜水果 、糖尿病*/
PROC PHREG DATA=men; 
class  agegr(ref='1')  risk19(ref='2') risk18(ref='2') dm(ref='0') risk1(ref='1')  education(ref='1') whtrgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =agegr risk1   whtrgr education   risk18  risk19 dm/selection=stepwise sle=0.05 slstay=0.05 RL; 
RUN;
/*保留的变量 agegr education dm risk1*/
PROC PHREG DATA=men; 
class   risk1(ref='1')   dm(ref='0') agegr(ref='1') education(ref='1');
MODEL ca_py*cancer(0) =agegr risk1 dm education/ RL; 
RUN;

proc phreg data=men;
class risk1(ref='1')   dm(ref='0') agegr(ref='1') education(ref="1") ;
model ca_py*cancer(0)= agegr  education risk1  dm 
agegrat educationat  risk1at  dmat/rl;
agegrat=agegr*log(ca_py);
educationat=education*log(ca_py);
 risk1at= risk1*log(ca_py);
dmat=dm*log(ca_py);
proportionality_test: test agegrat,educationat ,risk1at,dmat;*检验是否符合ph假定;
run;
/*符合	PH假定*/

/*分析女性危险因素与肿瘤发病的关系*/
PROC PHREG DATA=women; 
class  education(ref='1') agegr(ref='1')  /ref=first;
MODEL ca_py*cancer(0) = agegr education / RL; 
RUN;
*教育水平无意义;
PROC PHREG DATA=women; 
class  risk1(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk1 / RL; 
RUN;
*吸烟无意义;
PROC PHREG DATA=women; 
class  risk6(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk6 / RL; 
RUN;
*饮酒无意义;
PROC PHREG DATA=women; 
class   quarterimcome(ref='1') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr quarterimcome / RL; 
RUN;
*家庭收入无意义;
PROC PHREG DATA=women; 
class risk16(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk16/ RL; 
RUN;
*每周饮食脂肪,肥肉无意义;
PROC PHREG DATA=women; 
class  risk17(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk17/ RL; 
RUN;
*油炸熏肉无意义;
PROC PHREG DATA=women; 
class risk18(ref='2') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  risk18/ RL; 
RUN;
*咸菜/酱菜/泡菜/咸鱼/咸肉无意义;
PROC PHREG DATA=women; 
class  agegr(ref='1') risk19(ref='2')/ref=first;
MODEL ca_py*cancer(0) = agegr risk19/ RL; 
RUN;
*蔬菜水果无意义;
PROC PHREG DATA=women; 
class  dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr dm/ RL; 
RUN;
*糖尿病有意义;
PROC PHREG DATA=women; 
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr bmi / RL; 
RUN;
*BMI有意义;
PROC PHREG DATA=women; 
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr waist / RL; 
RUN;
*腰围有意义;
PROC PHREG DATA=women; 
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr  hip/ RL; 
RUN;
*臀围有意义;
PROC PHREG DATA=women;
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr  whr / RL; 
RUN;
*腰臀比无意义;
PROC PHREG DATA=women; 
MODEL ca_py*cancer(0) = agegr  whtr / RL; 
RUN;
*腰高比有意义;


proc gplot data=women ;
plot waist*hip*whtr*bmi;
run;
ods graphics on;
proc corr data=women plots=matrix(histogram);
var waist hip whtr bmi;
run;
/*研究女性腰围对肿瘤发病的影响*/
PROC PHREG DATA=women; 
class  agegr(ref='1')    dm(ref='0') /ref=first;
MODEL ca_py*cancer(0) =agegr waist  dm / RL; 
RUN;

proc phreg data=women;
class agegr(ref='1')  dm(ref='0')  ;
model ca_py*cancer(0)= agegr  dm  waist
dmat waistat agegrat/rl;
dmat=dm*log(ca_py);
waistat=waist*log(ca_py);
agegrat=agegr*log(ca_py);
proportionality_test: test agegrat, dmat,waistat;*检验是否符合ph假定;
run;
/*符合PH假定*/
proc means data=women min  p1 p10 p50 p90 p99 max;
var waist;
run;
PROC PHREG DATA=women COVOUT OUTEST=__RCS; 
class dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  waist dm agegr __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 68 79 91; 
 __1_1=((waist-68)**3)*(waist>68) 
     -((waist-79)**3)*(waist>79) 
     *(91-68)/(91-79) 
     +((waist-91)**3)*(waist>91) 
     *(79-68)/(91-79); 
 *--------- Testing variable: bmi ---------; 
 EFFECT1: TEST  waist, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 


 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=60;     *p1 value for X-axis; 
 UPPEREND=105;     *p99 value for X-axis; 
 REF=80;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.89*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-68)##3)#(X>68) 
     -((X-79)##3)#(X>79) 
     #(91-68)/(91-79) 
     +((X-91)##3)#(X>91) 
     #(79-68)/(91-79) 
     -((REF-68)##3)#(REF>68) 
     +((REF-79)##3)#(REF>79)#(91-68)/(91-79) 
     -((REF-91)##3)#(REF>91)#(79-68)/(91-79); 
 XMAT=(X-REF)||S1;
 HV={ waist __1_1 }; 
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
 CREATE __RCS_waist VAR { F FE Z X };  APPEND;  CLOSE __RCS_waist; 
 QUIT; 
/*选择80为参考值是因为建议我国成年女性腰围保持在80cm以下*/

/*作图*/ 
proc sort data=__RCS_waist;
by x z;
run;
proc transpose data=__RCS_waist out =waist prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;

/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\incidence of cancer" dpi=300;
ods graphics on/border=off imagename="Incidence of Cancer in women,WC" imagefmt=tiff;
proc sgplot data=waist noautolegend;
inset 'p-overall=0.0008' 'p-non-linearity=0.4629'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WC' values=(60 to 105 by 5) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.6 0.8 1 1.2 1.4 1.6 1.8 2.0 2.2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'Incidence of Cancer in women,WC.';
run;


/*研究女性BMI对肿瘤发病的影响*/
PROC PHREG DATA=women; 
class  agegr(ref='1')    dm(ref='0') /ref=first;
MODEL ca_py*cancer(0) =agegr bmi  dm / RL; 
RUN;

proc phreg data=women;
class agegr(ref='1')  dm(ref='0')  ;
model ca_py*cancer(0)= agegr  dm  bmi
dmat bmiat agegrat/rl;
dmat=dm*log(ca_py);
bmiat=bmi*log(ca_py);
agegrat=agegr*log(ca_py);
proportionality_test: test agegrat, dmat,bmiat;*检验是否符合ph假定;
run;
/*符合PH假定*/
proc means data=women min  p1 p10 p50 p90 p99 max;
var bmi;
run;
PROC PHREG DATA=women COVOUT OUTEST=__RCS; 
class dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  bmi dm agegr __1_1/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 19.98 23.73 28.38; 
 __1_1=((bmi-19.98)**3)*(bmi>19.98) 
     -((bmi-23.73)**3)*(bmi>23.73) 
     *(28.38-19.98)/(28.38-23.73) 
     +((bmi-28.38)**3)*(bmi>28.38) 
     *(23.73-19.98)/(28.38-23.73); 
 *--------- Testing variable: bmi ---------; 
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 


 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33.32;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.89*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-19.98)##3)#(X>19.98) 
     -((X-23.73)##3)#(X>23.73) 
     #(28.38-19.98)/(28.38-23.73) 
     +((X-28.38)##3)#(X>28.38) 
     #(23.73-19.98)/(28.38-23.73) 
     -((REF-19.98)##3)#(REF>19.98) 
     +((REF-23.73)##3)#(REF>23.73)#(28.38-19.98)/(28.38-23.73) 
     -((REF-28.38)##3)#(REF>28.38)#(23.73-19.98)/(28.38-23.73); 
 XMAT=(X-REF)||S1;
 HV={ BMI __1_1 }; 
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
 CREATE __RCS_BMI VAR { F FE Z X };  APPEND;  CLOSE __RCS_BMI; 
 QUIT; 
/*中国BMI分类标准，大于等于24为肥胖*/
/*作图*/ 
proc sort data=__RCS_BMI;
by x z;
run;
proc transpose data=__RCS_BMI out =BMI prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\incidence of cancer" dpi=300;
ods graphics on/border=off imagename="Incidence of Cancer in women,BMI" imagefmt=tiff;
proc sgplot data=BMI noautolegend;
inset 'p-overall=0.0031' 'p-non-linearity=0.1658'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.6 0.8 1 1.2 1.4 1.6 1.8 2.0 2.2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'Incidence of Cancer in women,BMI';
run;

/*研究女性臀围对肿瘤发病的影响*/
PROC PHREG DATA=women; 
class  agegr(ref='1')    dm(ref='0') /ref=first;
MODEL ca_py*cancer(0) =agegr hip  dm / RL; 
RUN;

proc means data=women min  p1 p10 p50 p90 p99 max;
var hip;
run;
PROC PHREG DATA=women COVOUT OUTEST=__RCS; 
class dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  hip dm agegr __1_1/RL; 
 
 ********** spline modelling of fixed covariate hip; 
 ********** with 3 knots located at; 
 ********** 85 93 103; 
 __1_1=((hip-89)**3)*(hip>89) 
     -((hip-93)**3)*(hip>93) 
     *(103-89)/(103-93) 
     +((hip-103)**3)*(hip>103) 
     *(93-89)/(103-93); 
 *--------- Testing variable: hip ---------; 
 EFFECT1: TEST  hip, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 


 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=79;     *p1 value for X-axis; 
 UPPEREND=114;     *p99 value for X-axis; 
 REF=93;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.89*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-89)##3)#(X>89) 
     -((X-93)##3)#(X>93) 
     #(103-89)/(103-93) 
     +((X-103)##3)#(X>103) 
     #(93-89)/(103-93) 
     -((REF-89)##3)#(REF>89) 
     +((REF-93)##3)#(REF>93)#(103-89)/(103-93) 
     -((REF-103)##3)#(REF>103)#(93-89)/(103-93); 
 XMAT=(X-REF)||S1;
 HV={ hip __1_1 }; 
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
 CREATE __RCS_hip VAR { F FE Z X };  APPEND;  CLOSE __RCS_hip; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_hip;
by x z;
run;
proc transpose data=__RCS_hip out =hip prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="D:\1a\learning\research\数据20210824\大肠癌筛查\results\incidence of cancer" dpi=300;
ods graphics on/border=off imagename="Incidence of Cancer in women,HC" imagefmt=tiff;
proc sgplot data=hip noautolegend;
inset 'p-overall=0.0004' 'p-non-linearity=0.8713'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='HC' values=(79 to 114 by 5) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.6 0.8 1 1.2 1.4 1.6 1.8 2.0 2.2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'Incidence of Cancer in women,HC';
run;


/*研究女性腰高比对肿瘤发病的影响*/
PROC PHREG DATA=women; 
class  agegr(ref='1')    dm(ref='0') /ref=first;
MODEL ca_py*cancer(0) =agegr WHTR  dm / RL; 
RUN;

proc means data=women min  p1 p10 p50 p90 p99 max  maxdec=2;
var whtr;
run;
PROC PHREG DATA=women COVOUT OUTEST=__RCS; 
class dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  whtr dm agegr __1_1/RL; 
 
 ********** spline modelling of fixed covariate whtr; 
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
 CREATE __RCS_whtr VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr; 
 QUIT; 

/*作图*/ 
proc sort data=__RCS_whtr;
by x z;
run;
proc transpose data=__RCS_whtr out =whtr prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="d:\figure" dpi=300;
ods graphics on/border=off imagename="Fig3-a" imagefmt=tiff;
proc sgplot data=whtr noautolegend;
inset 'p-overall=0.0261' 'p-non-linearity=0.6138'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='WHTR' values=(0.4 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.6 0.8 1 1.2 1.4 1.6 1.8 2.0 2.2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All-cause Mortality,WHTR';
run;



data a6;/*根据ICD10编码，根据分析需要生成不同肿瘤是否发病的变量名，发病为1，不发病为0*/
set a5;
if icd="C16" then stomach=1;else stomach=0;
if icd="C18" |  icd="C19" | icd="C20"   then crc=1;else crc=0;
if icd="C22" then liver=1;else liver=0;
if icd="C33" |  icd="C34" then lung=1;else lung=0;
if icd="C50" then breast=1;else breast=0;
if icd="C61" then prostate=1;else prostate=0;
if icd="C73" then thyroid=1;else thyroid=0;
run;
/*研究大肠癌发病的影响因素*/
data a6;
set a6;
if crc=1 then fol_yr=(caenddate-screenday)/365.25;
else fol_yr=(denddate-screenday)/365.25;
run;

data a7;
set a6;
if disease7=1|disease8=1|disease9=1|disease10=1|disease11=1|disease12=1 then  rel_sym=1;
else rel_sym=0;
if disease13=1|disease14=1|disease15=1|disease16=1  then  rel_dis=1;
else rel_dis=0;
if disease17=1|disease19=1|disease21=1|disease23=1|disease25=1   then  fam_his=1;
else fam_his=0;
run;

/*大肠癌发病人数144人*/
proc freq data=a7;
table crc;
run;
/*单因素分析*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1') dm(ref='0');
MODEL fol_yr*crc(0) =gender agegr  dm/ RL; 
RUN;
/*糖尿病无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk1(ref='1');
MODEL fol_yr*crc(0) =gender agegr  risk1/ RL; 
RUN;
/*吸烟无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk6(ref='1');
MODEL fol_yr*crc(0) =gender agegr  risk6/ RL; 
RUN;
/*饮酒无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk16(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk16/ RL; 
RUN;
/*脂肪肥肉无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk17(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk17/ RL; 
RUN;
/*油炸熏肉无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk18(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk18/ RL; 
RUN;
/*油炸熏肉无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk19(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk19/ RL; 
RUN;
/*蔬菜水果无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1');
MODEL fol_yr*crc(0) =gender agegr  bmi/ RL; 
RUN;
/*BMI无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1');
MODEL fol_yr*crc(0) =gender agegr  whtr/ RL; 
RUN;
/*whtr无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1') fam_his(ref='0');
MODEL fol_yr*crc(0) =gender agegr fam_his/ RL; 
RUN;
/*fam_his无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1') rel_dis(ref='0');
MODEL fol_yr*crc(0) =gender agegr rel_dis/ RL; 
RUN;
/*rel_dis无意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  education(ref='1');
MODEL fol_yr*crc(0) =gender agegr education/ RL; 
RUN;
/*education意义*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')dm(ref='0')  risk1(ref='1')  risk6(ref='1')  risk16(ref='2')  risk17(ref='2')  risk18(ref='2')  risk19(ref='2')  agegr(ref='1') rel_sym(ref='0')  rel_dis(ref='0')  fam_his(ref='0');
MODEL fol_yr*crc(0) =gender agegr  risk1  risk6  risk16 risk17 risk18 risk19  dm whtr bmi education quarterimcome  rel_sym rel_dis fam_his/ selection=stepwise sle=0.05 sls=0.05 RL; 
RUN;

/*研究乳腺癌发病的危险因素*/
data breast;
set a7;
if gender=2 then output;
run;

/*总人数15041人，乳腺癌发病人数176人*/
 proc freq data=breast;
 table breast;
 run;
 /*生成乳腺癌随访人年*/
data breast;
set breast;
if breast=1 then fol_yr=(caenddate-screenday)/365.25;
else fol_yr=(denddate-screenday)/365.25;
run;

 /*单因素分析*/
PROC PHREG DATA=breast; 
class  agegr(ref='1') dm(ref='0');
MODEL fol_yr*breast(0) =agegr  dm/ RL; 
RUN;
/*糖尿病无意义*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk1(ref='1');
MODEL fol_yr*breast(0) = agegr  risk1/ RL; 
RUN;
/*吸烟无意义*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk6(ref='1');
MODEL fol_yr*breast(0) = agegr  risk6/ RL; 
RUN;
/*饮酒无意义*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk16(ref='2');
MODEL fol_yr*breast(0) =agegr  risk16/ RL; 
RUN;
/*脂肪肥肉有意义*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk17(ref='2');
MODEL fol_yr*breast(0) =agegr  risk17/ RL; 
RUN;
/*油炸熏肉无意义*/
PROC PHREG DATA=breast; 
class   agegr(ref='1')  risk18(ref='2');
MODEL fol_yr*breast(0) = agegr  risk18/ RL; 
RUN;
/*油炸熏肉无意义*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk19(ref='2');
MODEL fol_yr*breast(0) = agegr  risk19/ RL; 
RUN;
/*蔬菜水果无意义*/
PROC PHREG DATA=breast; 
class   agegr(ref='1');
MODEL fol_yr*breast(0) =agegr  bmi/ RL; 
RUN;
/*BMI无意义*/
PROC PHREG DATA=breast; 
class  agegr(ref='1');
MODEL fol_yr*breast(0) =agegr  whtr/ RL; 
RUN;
/*whtr无意义*/
PROC PHREG DATA=breast; 
class   agegr(ref='1')  education(ref='1');
MODEL fol_yr*breast(0) = agegr education/ RL; 
RUN;
/*education有意义*/
PROC PHREG DATA=breast; 
class   agegr(ref='1') quarterimcome(ref='1');
MODEL fol_yr*breast(0) = agegr quarterimcome/ RL; 
RUN;
/*quarterimcome有意义*/
PROC PHREG DATA=breast; 
class   agegr(ref='1') quarterimcome(ref='1');
MODEL fol_yr*breast(0) = agegr quarterimcome/ RL; 
RUN;
/*quarterimcome有意义*/
PROC PHREG DATA=breast; 
class   agegr(ref='1');
MODEL fol_yr*breast(0) = agegr whr/ RL; 
RUN;
/*whr无意义*/


data dm_cancer;
set a7;
if dm=1 then output;
run;

/*糖尿病人群 肿瘤发病人数325人，全死因死亡人数283人*/
proc freq datd=dm_cancer;
table cancer d;
run;
