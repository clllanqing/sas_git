/*�о��������������������Ĺ���*/
libname cao "D:\1a\learning\research\colorectal cancer\result\sas,data,program";
data a1;
set cao.crc_info;
run;
proc contents data=a1 varnum;
run;
/*�鿴������������Ϊ1425�� Ƶ��ȱʧ397��*/
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
/*�������䡢���αȡ����߱�*/
data a3;
set a2;
whr=waist/hip;
whtr=waist/height;
age=(screenday-birthday)/365.25;
run;

/*��Σ�����ر������½��з���*/
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
/*��ӱ�����ǩ*/
data a5 ;
set a4;
label education="1��Сѧ 2������ 3�����С���ר����У 4����ר������";
label quarterimcome="1:<2000 2:2000-4000 3:>4000";
label risk1="1:������ 2:����";
label risk6="1:������ 2:����";
label risk16="ÿ����ʳ֬��,���� 1��������һ��2��һ�ζ�û��";
label risk17="��ըѬ��1��������һ��2��һ�ζ�û��";
label risk18="�̲�/����/�ݲ�/����/�����1��������һ��2��һ�ζ�û��";
label risk19="ƽ��ÿ��Զ����߲�ˮ��1��300g����2:300g����";
run;
/*ȷ�����±����ķ�λ��*/
proc means data=a5 p25 p50 p75 maxdec=2;
var waist hip whr whtr;
run;
/*�������ͱ������ݷ�λ�����з���*/
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
*�����ط���,bmi�ȱ���������������;
PROC PHREG DATA=a5; 
class gender(ref='1')  /ref=first;
MODEL ca_py*cancer(0) =gender agegr bmi  / RL; 
RUN;
*BMI������;
PROC PHREG DATA=a5; 
class gender(ref='1')    /ref=first;
MODEL ca_py*cancer(0) =gender agegr whr/ RL; 
RUN;
*���α�������;
PROC PHREG DATA=a5; 
class gender(ref='1')   /ref=first;
MODEL ca_py*cancer(0) =gender agegr whtr / RL; 
RUN;
*���߱�������;
PROC PHREG DATA=a5; 
class gender(ref='1') education(ref='1') /ref=first;
MODEL ca_py*cancer(0) =gender age education / RL; 
RUN;
*����ˮƽ������;
PROC PHREG DATA=a5; 
class gender(ref='1')   risk1(ref='1') /ref=first;
MODEL ca_py*cancer(0) = gender age risk1 / RL; 
RUN;
*����������;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk6(ref='1') /ref=first;
MODEL ca_py*cancer(0) = gender age risk6 / RL; 
RUN;
*����������;
PROC PHREG DATA=a5; 
class gender(ref='1')  quarterimcome(ref='1');
MODEL ca_py*cancer(0) =gender age quarterimcome / RL; 
RUN;
*��ͥ����������;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk16(ref='2');
MODEL ca_py*cancer(0) =gender age risk16/ RL; 
RUN;
*ÿ����ʳ֬��,����������;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk17(ref='2') 
MODEL ca_py*cancer(0) =gender age risk17/ RL; 
RUN;
*��ըѬ��������;
PROC PHREG DATA=a5; 
class gender(ref='1')  risk18(ref='2') /ref=first;
MODEL ca_py*cancer(0) =gender age  risk18/ RL; 
RUN;
*�̲�/����/�ݲ�/����/����������;
PROC PHREG DATA=a5; 
class gender(ref='1') risk19(ref='2')/ref=first;
MODEL ca_py*cancer(0) =gender age risk19/ RL; 
RUN;
*�߲�ˮ��������;
PROC PHREG DATA=a5; 
class gender(ref='1')   dm(ref='0') /ref=first;
MODEL ca_py*cancer(0) =gender age dm/ RL; 
RUN;
*����������;
*������ı��� BMI �����߱� ���߲�ˮ��������;

proc corr data=a5 ;
var  whtr bmi;
run;
proc gplot data=a5;
plot whtr*bmi;
title"scatter plot of whtr and bmi";
run;
/*ɸѡ����*/
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

/*�о����߱ȶ�ȫ����������Ӱ��*/
proc phreg data=a5;
class  gender(ref='1')   risk19(ref='2') dm(ref='0') risk1(ref='1') agegr(ref='1');
model ca_py*cancer(0)= agegr gender risk1  dm whtr
agegrat genderat  risk1at   dmat  whtrat/rl;
agegrat=agegr*log(ca_py);
genderat=gender*log(ca_py);
 risk1at= risk1*log(ca_py);
dmat=dm*log(ca_py);
whtrat=whtr*log(ca_py);
proportionality_test: test agegrat,genderat,risk1at,dmat,whtrat;*�����Ƿ����ph�ٶ�;
run;
/*����PH�ٶ�*/
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

/*��ͼ*/ 
proc sort data=__RCS_whtr_all;
by x z;
run;
proc transpose data=__RCS_whtr_all out =whtr_all prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\incidence of cancer" dpi=300;
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
/*�о�BMI������������Ӱ��*/
PROC PHREG DATA=a5  COVOUT OUTEST=__RCS; 
model ca_py*cancer(0)= agegr gender risk1  dm risk19 bmi 
bmiat/rl;
bmiat=bmi*log(ca_py);
proportionality_test: test bmiat;*�����Ƿ����ph�ٶ�;
run;
/*������PH�ٶ�*/
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
 /*����PH�ٶ�*/
 
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
/*�й�BMI�����׼�����ڵ���24Ϊ����*/
/*��ͼ*/ 
proc sort data=__RCS_BMI;
by x z;
run;
proc transpose data=__RCS_BMI out =BMI prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;


/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\incidence of cancer" dpi=300;
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


/*���Ա��������ط���*/
data men women;
set a5;
if gender=1 then output men;
if gender=2 then  output women;
run;
/*��������Σ�����������������Ĺ�ϵ*/
PROC PHREG DATA=men; 
class  agegr(ref='1') bmigr(ref='2')/ref=first;
MODEL ca_py*cancer(0) = agegr bmigr  / RL; 
RUN;
*BMI������;
PROC PHREG DATA=men; 
class  agegr(ref='1')  waistgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  waistgr / RL; 
RUN;
*��Χ������;
PROC PHREG DATA=men; 
class  agegr(ref='1')  hipgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  hipgr / RL; 
RUN;
*��Χ������;
PROC PHREG DATA=men; 
class   agegr(ref='1')  whrgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  whrgr / RL; 
RUN;
*���α�������;
PROC PHREG DATA=men; 
class  agegr(ref='1')  whtrgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  whtrgr / RL; 
RUN;
*���߱�������;
PROC PHREG DATA=men; 
class  education(ref='1') agegr(ref='1')  /ref=first;
MODEL ca_py*cancer(0) = agegr education / RL; 
RUN;
*����ˮƽ������;
PROC PHREG DATA=men; 
class  risk1(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk1 / RL; 
RUN;
*����������;
PROC PHREG DATA=men; 
class  risk6(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk6 / RL; 
RUN;
*����������;
PROC PHREG DATA=men; 
class   quarterimcome(ref='1') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr quarterimcome / RL; 
RUN;
*��ͥ����������;
PROC PHREG DATA=men; 
class risk16(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk16/ RL; 
RUN;
*ÿ����ʳ֬��,����������;
PROC PHREG DATA=men; 
class  risk17(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk17/ RL; 
RUN;
*��ըѬ��������;
PROC PHREG DATA=men; 
class risk18(ref='2') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  risk18/ RL; 
RUN;
*�̲�/����/�ݲ�/����/����������;
PROC PHREG DATA=men; 
class  agegr(ref='1') risk19(ref='2')/ref=first;
MODEL ca_py*cancer(0) = agegr risk19/ RL; 
RUN;
*�߲�ˮ��������;
PROC PHREG DATA=men; 
class  dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr dm/ RL; 
RUN;
*����������;
/*������ı��� ���䡢���߱ȡ�����ˮƽ�����̡��̲�/����/�ݲ�/����/���⡢�߲�ˮ�� ������*/
PROC PHREG DATA=men; 
class  agegr(ref='1')  risk19(ref='2') risk18(ref='2') dm(ref='0') risk1(ref='1')  education(ref='1') whtrgr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =agegr risk1   whtrgr education   risk18  risk19 dm/selection=stepwise sle=0.05 slstay=0.05 RL; 
RUN;
/*�����ı��� agegr education dm risk1*/
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
proportionality_test: test agegrat,educationat ,risk1at,dmat;*�����Ƿ����ph�ٶ�;
run;
/*����	PH�ٶ�*/

/*����Ů��Σ�����������������Ĺ�ϵ*/
PROC PHREG DATA=women; 
class  education(ref='1') agegr(ref='1')  /ref=first;
MODEL ca_py*cancer(0) = agegr education / RL; 
RUN;
*����ˮƽ������;
PROC PHREG DATA=women; 
class  risk1(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk1 / RL; 
RUN;
*����������;
PROC PHREG DATA=women; 
class  risk6(ref='1') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) =  agegr risk6 / RL; 
RUN;
*����������;
PROC PHREG DATA=women; 
class   quarterimcome(ref='1') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr quarterimcome / RL; 
RUN;
*��ͥ����������;
PROC PHREG DATA=women; 
class risk16(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk16/ RL; 
RUN;
*ÿ����ʳ֬��,����������;
PROC PHREG DATA=women; 
class  risk17(ref='2') agegr(ref='1');
MODEL ca_py*cancer(0) = agegr risk17/ RL; 
RUN;
*��ըѬ��������;
PROC PHREG DATA=women; 
class risk18(ref='2') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr  risk18/ RL; 
RUN;
*�̲�/����/�ݲ�/����/����������;
PROC PHREG DATA=women; 
class  agegr(ref='1') risk19(ref='2')/ref=first;
MODEL ca_py*cancer(0) = agegr risk19/ RL; 
RUN;
*�߲�ˮ��������;
PROC PHREG DATA=women; 
class  dm(ref='0') agegr(ref='1')/ref=first;
MODEL ca_py*cancer(0) = agegr dm/ RL; 
RUN;
*����������;
PROC PHREG DATA=women; 
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr bmi / RL; 
RUN;
*BMI������;
PROC PHREG DATA=women; 
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr waist / RL; 
RUN;
*��Χ������;
PROC PHREG DATA=women; 
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr  hip/ RL; 
RUN;
*��Χ������;
PROC PHREG DATA=women;
class agegr(ref='1');
MODEL ca_py*cancer(0) = agegr  whr / RL; 
RUN;
*���α�������;
PROC PHREG DATA=women; 
MODEL ca_py*cancer(0) = agegr  whtr / RL; 
RUN;
*���߱�������;


proc gplot data=women ;
plot waist*hip*whtr*bmi;
run;
ods graphics on;
proc corr data=women plots=matrix(histogram);
var waist hip whtr bmi;
run;
/*�о�Ů����Χ������������Ӱ��*/
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
proportionality_test: test agegrat, dmat,waistat;*�����Ƿ����ph�ٶ�;
run;
/*����PH�ٶ�*/
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
/*ѡ��80Ϊ�ο�ֵ����Ϊ�����ҹ�����Ů����Χ������80cm����*/

/*��ͼ*/ 
proc sort data=__RCS_waist;
by x z;
run;
proc transpose data=__RCS_waist out =waist prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;

/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\incidence of cancer" dpi=300;
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


/*�о�Ů��BMI������������Ӱ��*/
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
proportionality_test: test agegrat, dmat,bmiat;*�����Ƿ����ph�ٶ�;
run;
/*����PH�ٶ�*/
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
/*�й�BMI�����׼�����ڵ���24Ϊ����*/
/*��ͼ*/ 
proc sort data=__RCS_BMI;
by x z;
run;
proc transpose data=__RCS_BMI out =BMI prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;


/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\incidence of cancer" dpi=300;
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

/*�о�Ů����Χ������������Ӱ��*/
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

/*��ͼ*/ 
proc sort data=__RCS_hip;
by x z;
run;
proc transpose data=__RCS_hip out =hip prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;


/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\incidence of cancer" dpi=300;
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


/*�о�Ů�����߱ȶ�����������Ӱ��*/
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

/*��ͼ*/ 
proc sort data=__RCS_whtr;
by x z;
run;
proc transpose data=__RCS_whtr out =whtr prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;


/* All death male ����ͼ*/
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



data a6;/*����ICD10���룬���ݷ�����Ҫ���ɲ�ͬ�����Ƿ񷢲��ı�����������Ϊ1��������Ϊ0*/
set a5;
if icd="C16" then stomach=1;else stomach=0;
if icd="C18" |  icd="C19" | icd="C20"   then crc=1;else crc=0;
if icd="C22" then liver=1;else liver=0;
if icd="C33" |  icd="C34" then lung=1;else lung=0;
if icd="C50" then breast=1;else breast=0;
if icd="C61" then prostate=1;else prostate=0;
if icd="C73" then thyroid=1;else thyroid=0;
run;
/*�о��󳦰�������Ӱ������*/
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

/*�󳦰���������144��*/
proc freq data=a7;
table crc;
run;
/*�����ط���*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1') dm(ref='0');
MODEL fol_yr*crc(0) =gender agegr  dm/ RL; 
RUN;
/*����������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk1(ref='1');
MODEL fol_yr*crc(0) =gender agegr  risk1/ RL; 
RUN;
/*����������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk6(ref='1');
MODEL fol_yr*crc(0) =gender agegr  risk6/ RL; 
RUN;
/*����������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk16(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk16/ RL; 
RUN;
/*֬������������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk17(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk17/ RL; 
RUN;
/*��ըѬ��������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk18(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk18/ RL; 
RUN;
/*��ըѬ��������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  risk19(ref='2');
MODEL fol_yr*crc(0) =gender agegr  risk19/ RL; 
RUN;
/*�߲�ˮ��������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1');
MODEL fol_yr*crc(0) =gender agegr  bmi/ RL; 
RUN;
/*BMI������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1');
MODEL fol_yr*crc(0) =gender agegr  whtr/ RL; 
RUN;
/*whtr������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1') fam_his(ref='0');
MODEL fol_yr*crc(0) =gender agegr fam_his/ RL; 
RUN;
/*fam_his������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1') rel_dis(ref='0');
MODEL fol_yr*crc(0) =gender agegr rel_dis/ RL; 
RUN;
/*rel_dis������*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')  education(ref='1');
MODEL fol_yr*crc(0) =gender agegr education/ RL; 
RUN;
/*education����*/
PROC PHREG DATA=a7; 
class gender(ref='1')  agegr(ref='1')dm(ref='0')  risk1(ref='1')  risk6(ref='1')  risk16(ref='2')  risk17(ref='2')  risk18(ref='2')  risk19(ref='2')  agegr(ref='1') rel_sym(ref='0')  rel_dis(ref='0')  fam_his(ref='0');
MODEL fol_yr*crc(0) =gender agegr  risk1  risk6  risk16 risk17 risk18 risk19  dm whtr bmi education quarterimcome  rel_sym rel_dis fam_his/ selection=stepwise sle=0.05 sls=0.05 RL; 
RUN;

/*�о����ٰ�������Σ������*/
data breast;
set a7;
if gender=2 then output;
run;

/*������15041�ˣ����ٰ���������176��*/
 proc freq data=breast;
 table breast;
 run;
 /*�������ٰ��������*/
data breast;
set breast;
if breast=1 then fol_yr=(caenddate-screenday)/365.25;
else fol_yr=(denddate-screenday)/365.25;
run;

 /*�����ط���*/
PROC PHREG DATA=breast; 
class  agegr(ref='1') dm(ref='0');
MODEL fol_yr*breast(0) =agegr  dm/ RL; 
RUN;
/*����������*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk1(ref='1');
MODEL fol_yr*breast(0) = agegr  risk1/ RL; 
RUN;
/*����������*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk6(ref='1');
MODEL fol_yr*breast(0) = agegr  risk6/ RL; 
RUN;
/*����������*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk16(ref='2');
MODEL fol_yr*breast(0) =agegr  risk16/ RL; 
RUN;
/*֬������������*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk17(ref='2');
MODEL fol_yr*breast(0) =agegr  risk17/ RL; 
RUN;
/*��ըѬ��������*/
PROC PHREG DATA=breast; 
class   agegr(ref='1')  risk18(ref='2');
MODEL fol_yr*breast(0) = agegr  risk18/ RL; 
RUN;
/*��ըѬ��������*/
PROC PHREG DATA=breast; 
class  agegr(ref='1')  risk19(ref='2');
MODEL fol_yr*breast(0) = agegr  risk19/ RL; 
RUN;
/*�߲�ˮ��������*/
PROC PHREG DATA=breast; 
class   agegr(ref='1');
MODEL fol_yr*breast(0) =agegr  bmi/ RL; 
RUN;
/*BMI������*/
PROC PHREG DATA=breast; 
class  agegr(ref='1');
MODEL fol_yr*breast(0) =agegr  whtr/ RL; 
RUN;
/*whtr������*/
PROC PHREG DATA=breast; 
class   agegr(ref='1')  education(ref='1');
MODEL fol_yr*breast(0) = agegr education/ RL; 
RUN;
/*education������*/
PROC PHREG DATA=breast; 
class   agegr(ref='1') quarterimcome(ref='1');
MODEL fol_yr*breast(0) = agegr quarterimcome/ RL; 
RUN;
/*quarterimcome������*/
PROC PHREG DATA=breast; 
class   agegr(ref='1') quarterimcome(ref='1');
MODEL fol_yr*breast(0) = agegr quarterimcome/ RL; 
RUN;
/*quarterimcome������*/
PROC PHREG DATA=breast; 
class   agegr(ref='1');
MODEL fol_yr*breast(0) = agegr whr/ RL; 
RUN;
/*whr������*/


data dm_cancer;
set a7;
if dm=1 then output;
run;

/*������Ⱥ ������������325�ˣ�ȫ������������283��*/
proc freq datd=dm_cancer;
table cancer d;
run;
