************************������Ϊ��ֵķ�������********************;
*************************************���ϴ������*****************************************;
libname cao "D:\sas_git";
data b9;
set cao.analysis_data;
run;
proc means data=b9 p25 p50 p75 maxdec=2;
var waist hip whr whtr;
run;
/*��BMI����Χ����Χ�����αȡ����߱��ĵȷ�*/
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
/*��whtr���ȷ�*/
data b9;
set b9;
if whtr<0.5 then binwhtr=1;
else binwhtr=2;
run;
/*��BMI���ȷּ����ȷ�*/
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

data b9;
set b9;
if binbmi=1 and binwhtr=1 then con2=1;if binbmi=1 and binwhtr=2 then con2=2;
if binbmi=2 and binwhtr=1 then con2=3;if binbmi=2 and binwhtr=2 then con2=4;
run;
********************************************************BMI�����߱ȶ�ȫ����������Ӱ��*****************************************;
*cox�ع�;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; *��BMI�����߱Ⱦ�����ģ�� bmi�����߱Ⱦ�������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') dm(ref='0') binwhtr(ref='1') agegr(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr binbmi education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm binwhtr /selection=stepwise slstay=0.05 RL; 
RUN;
/*age  gender education dm quarterimcome risk1 risk6 risk19 bmi whtr*/

PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; *��BMI����ģ��  bmi������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') dm(ref='0') binwhtr(ref='1') agegr(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr binbmi education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm  /selection=stepwise slstay=0.05 RL; 
RUN;
/*age  gender education dm quarterimcome risk1 risk6 risk19 bmi*/

PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; *��whtr����ģ��  whtr������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') dm(ref='0') binwhtr(ref='1') agegr(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm binwhtr  /selection=stepwise slstay=0.05 RL; 
RUN;
/*age  gender education dm quarterimcome risk1 risk6 risk19*/

PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; *�鿴�����������Ƿ������� ����������������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') dm(ref='0') binwhtr(ref='1') agegr(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm binwhtr|bmi  /selection=stepwise slstay=0.05 RL; 
RUN;
/*age  gender education dm quarterimcome risk1 risk6 risk19*/

PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; *��ϱ������������;
class gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2') dm(ref='0') agegr(ref='1') con2(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm con2/selection=stepwise slstay=0.05 RL; 
RUN;
/*age  gender education dm quarterimcome risk1 risk6 risk19 con*/

/*�������HR value and  95% ci*/
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
class  bmigr(ref="2") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk19(ref='2') dm(ref='0') binwhtr(ref='1') agegr(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr binbmi education quarterimcome risk1 risk6  risk19  dm binwhtr / RL; 
RUN;
PROC PHREG DATA=b9 COVOUT OUTEST=__RCS; 
class gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk19(ref='2') dm(ref='0') agegr(ref='1') con2(ref='1')/ref=first;
MODEL d_py*d(0) =gender agegr  education quarterimcome risk1 risk6  risk19  dm  con2/ RL; 
RUN;
/*���㷢������*/
proc  freq data=b9;
tables binbmi*d binwhtr*d con2*d;
run;
/*�����������*/
proc sql;
select sum(d_py) from b9 
where  binbmi=1;
quit;
proc sql;
select sum(d_py) from b9 
where  binbmi=2;
quit;
proc sql;
select sum(d_py) from b9 
where  binwhtr=1;
quit;
proc sql;
select sum(d_py) from b9 
where  binwhtr=2;
quit;
proc sql;
select sum(d_py) from b9 
where  con2=1;
quit;
proc sql;
select sum(d_py) from b9 
where  con2=2;
quit;
proc sql;
select sum(d_py) from b9 
where  con2=3;
quit;
proc sql;
select sum(d_py) from b9 
where  con2=4;
quit;

/*����ЧӦͼ*/
proc means data=b9 p1 p10 p50 p90 p99;
var bmi whtr;
run;
proc phreg data=b9 COVOUT OUTEST=__RCS;  *BMI ��ȫ��������;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk19  dm bmi whtr __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
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
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_bmi_allcause  VAR { F FE Z X };  APPEND;  CLOSE __RCS_bmi_allcause ; 
 QUIT; 

proc sort data=__RCS_bmi_allcause ;
by x z;
run;
proc transpose data=__RCS_bmi_allcause  out =bmi_allcause prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cause Mortality on BMI" imagefmt=tiff;
proc sgplot data=bmi_allcause noautolegend;
inset 'p-overall<.0001' 'p-non-linearity<.0001'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All Cause Mortality on BMI';
run;


proc phreg data=b9 COVOUT OUTEST=__RCS;  *BMI ��ȫ��������.����WHTR���з���;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk19  dm bmi  __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
 __1_1=((bmi-20.20)**3)*(bmi>20.20) 
     -((bmi-24.03)**3)*(bmi>24.03) 
     *(28.40-20.20)/(28.40-24.03) 
     +((bmi-28.40)**3)*(bmi>28.40) 
     *(24.03-20.20)/(28.40-24.03); 
 *--------- Testing variable: whtr ---------; 
	 where binwhtr=1;
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_bmi_whtr1  VAR { F FE Z X };  APPEND;  CLOSE __RCS_bmi_whtr1 ; 
 QUIT; 

proc sort data=__RCS_bmi_whtr1  ;
by x z;
run;
proc transpose data=__RCS_bmi_whtr1   out =bmi_whtr1 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cause Mortality on BMI,where whtr<0.5" imagefmt=tiff;
proc sgplot data=bmi_whtr1  noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.1908'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All Cause Mortality on BMI,where whtr<0.5'justify=left 'A';
run;

proc phreg data=b9 COVOUT OUTEST=__RCS;  *BMI ��ȫ��������.����WHTR���з���;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk19  dm bmi __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
 __1_1=((bmi-20.20)**3)*(bmi>20.20) 
     -((bmi-24.03)**3)*(bmi>24.03) 
     *(28.40-20.20)/(28.40-24.03) 
     +((bmi-28.40)**3)*(bmi>28.40) 
     *(24.03-20.20)/(28.40-24.03); 
 *--------- Testing variable: whtr ---------; 
	 where binwhtr=2;
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_bmi_whtr2  VAR { F FE Z X };  APPEND;  CLOSE __RCS_bmi_whtr2 ; 
 QUIT; 

proc sort data=__RCS_bmi_whtr2  ;
by x z;
run;
proc transpose data=__RCS_bmi_whtr2   out =bmi_whtr2 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cause Mortality on BMI,where whtr��0.5" imagefmt=tiff;
proc sgplot data=bmi_whtr2  noautolegend;
inset 'p-overall<.0001' 'p-non-linearity<.0001'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All Cause Mortality on BMI,where whtr��0.5'justify=left 'B';
run;

proc means data=b9 p1 p10 p50 p90 p99;
var bmi whtr;
run;

proc phreg data=b9 COVOUT OUTEST=__RCS;  *whtr ��ȫ��������;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk19  dm bmi whtr __1_1 / RL; 
 ********** 0.43  0.5 0.58; 
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
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_bmi1  VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_bmi1 ; 
 QUIT; 

proc sort data=__RCS_whtr_bmi1 ;
by x z;
run;
proc transpose data=__RCS_whtr_bmi1  out =whtr_bmi1 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cause Mortality on WHtR " imagefmt=tiff;
proc sgplot data=whtr_bmi1 noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.0003'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt 'All Cause Mortality on WHtR';
run;

proc phreg data=b9 COVOUT OUTEST=__RCS;  *whtr ��ȫ�������� �ֲ�ͬ��bmi��;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk19  dm  whtr __1_1 / RL; 
 ********** 0.43  0.5 0.58; 
 __1_1=((whtr-0.43)**3)*(whtr>0.43) 
     -((whtr-0.5)**3)*(whtr>0.5) 
     *(0.58-0.43)/(0.58-0.5) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.5-0.43)/(0.58-0.5); 
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
where binbmi=1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_allcause  VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_allcause ; 
 QUIT; 

proc sort data=__RCS_whtr_allcause ;
by x z;
run;
proc transpose data=__RCS_whtr_allcause  out =whtr_allcause prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cause Mortality on WHtR where BMI<24" imagefmt=tiff;
proc sgplot data=whtr_allcause noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.0112'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt 'All Cause Mortality on WHtR where BMI<24'justify=left 'A';
run;


proc phreg data=b9 COVOUT OUTEST=__RCS;  *whtr ��ȫ�������� �ֲ�ͬ��bmi��;
MODEL d_py*d(0) =gender agegr education quarterimcome risk1 risk6 risk19  dm  whtr __1_1 / RL; 
 ********** 0.43  0.5 0.58; 
 __1_1=((whtr-0.43)**3)*(whtr>0.43) 
     -((whtr-0.5)**3)*(whtr>0.5) 
     *(0.58-0.43)/(0.58-0.5) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.5-0.43)/(0.58-0.5); 
 *--------- Testing variable: whtr ---------; 
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
where binbmi=2;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_bmi2  VAR { F FE Z X };  APPEND;  CLOSE __RCS_whtr_bmi2 ; 
 QUIT; 

proc sort data=__RCS_whtr_bmi2 ;
by x z;
run;
proc transpose data=__RCS_whtr_bmi2  out =whtr_bmi2 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cause Mortality on WHtR where BMI��24" imagefmt=tiff;
proc sgplot data=whtr_bmi2 noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.2122'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt 'All Cause Mortality on WHtR where BMI��24'justify=left 'B';
run;

*************************************************������������������Ѫ������Ϊ��ֽ��з���********************************;
/*������������������Ѫ������Ϊ��ֵı���*/
data b10;
set b9;
tumor=prxmatch('/C|D3|D4/',DICD);
cvd=prxmatch('/I/',DICD);
run;
/*������������Ϊ650*/
/*������Ѫ�ܼ�������Ϊ459*/


********************************************************BMI�����߱ȶ���Ѫ������������Ӱ��*****************************************;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI�����߱�ͬʱ����ģ�ͣ�BMI�����߱Ⱦ�������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') binwhtr(ref='1') /ref=first;
MODEL d_py*cvd(0) =gender agegr binbmi education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm  binwhtr/selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk6 dm risk19 risk16 binbmi binwhtr;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI����ģ�ͣ�BMI������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') /ref=first;
MODEL d_py*cvd(0) =gender agegr binbmi education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm/selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk6 dm risk19 risk16 binbmi binwhtr;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *���߱�����ģ�ͣ����߱�������;
class gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') binwhtr(ref='1') /ref=first;
MODEL d_py*cvd(0) =gender agegr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm  binwhtr/selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk6 dm risk19 risk16 binbmi binwhtr;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��ϱ�������ģ�ͣ���ϱ���������;
class gender(ref='1') education(ref='1') quarterimcome(ref='1') risk1(ref='1') risk6(ref='1')  risk16(ref='2') risk17(ref='2') risk18(ref='2') risk19(ref='2')  dm(ref='0') con2(ref='1') /ref=first;
MODEL d_py*cvd(0) =gender agegr education quarterimcome risk1 risk6 risk16 risk17 risk18 risk19  dm  con2/selection=backward slstay=0.05 RL; 
RUN;
*gender agegr   education risk6 dm risk19 risk16 binbmi binwhtr;

/*�������HR value and  95% ci*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI�����߱�ͬʱ����ģ�ͣ�BMI�����߱Ⱦ�������;
class  gender(ref='1') agegr(ref='1') education(ref='1')  risk6(ref='1')  risk16(ref='2')   risk19(ref='2') dm(ref='0') binwhtr(ref='1')  binbmi(ref="1")/ref=first;
MODEL d_py*cvd(0) =gender agegr binbmi education  risk6 risk16 risk19  dm  binwhtr/ RL; 
RUN;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI�����߱�ͬʱ����ģ�ͣ�BMI�����߱Ⱦ�������;
class  gender(ref='1') agegr(ref='1') education(ref='1')  risk6(ref='1')  risk16(ref='2')   risk19(ref='2') dm(ref='0') con2(ref='1')/ref=first;
MODEL d_py*cvd(0) =gender agegr education  risk6 risk16 risk19  dm con2/ RL; 
RUN;
/*������������*/
proc freq data=b10;
tables binbmi*cvd binwhtr*cvd con2*cvd;
run;


/*����ЧӦֵ*/
proc phreg data=b10 COVOUT OUTEST=__RCS;  *BMI ����Ѫ����������;
MODEL d_py*cvd(0) =gender agegr education  risk6 risk16 risk19  dm bmi  whtr __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
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
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS__cvd_bmi VAR { F FE Z X };  APPEND;  CLOSE __RCS__cvd_bmi; 
 QUIT; 

proc sort data= __RCS__cvd_bmi;
by x z;
run;
proc transpose data= __RCS__cvd_bmi   out =cvd_bmi prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="CVD Mortality on BMI" imagefmt=tiff;
proc sgplot data=cvd_bmi noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.0231'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'CVD Mortality on BMI';
run;

proc phreg data=b10 COVOUT OUTEST=__RCS;  *BMI ����Ѫ���������� ����WHTR;
MODEL d_py*cvd(0) =gender agegr education  risk6 risk16 risk19  dm bmi __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
 __1_1=((bmi-20.20)**3)*(bmi>20.20) 
     -((bmi-24.03)**3)*(bmi>24.03) 
     *(28.40-20.20)/(28.40-24.03) 
     +((bmi-28.40)**3)*(bmi>28.40) 
     *(24.03-20.20)/(28.40-24.03); 
 *--------- Testing variable: whtr ---------; 
	 where binwhtr=1;
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS__cvd_bmi_whtr1 VAR { F FE Z X };  APPEND;  CLOSE __RCS__cvd_bmi_whtr1; 
 QUIT; 

proc sort data= __RCS__cvd_bmi_whtr1;
by x z;
run;
proc transpose data= __RCS__cvd_bmi_whtr1   out =cvd_bmi_whtr1 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="CVD Mortality on BMI,where WHtR<0.5" imagefmt=tiff;
proc sgplot data=cvd_bmi_whtr1 noautolegend;
inset 'p-overall=0.0030' 'p-non-linearity=0.6906'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'CVD Mortality on BMI,where WHtR<0.5' justify=left'A';
run;

proc phreg data=b10 COVOUT OUTEST=__RCS;  *BMI ����Ѫ���������� ����WHTR;
MODEL d_py*cvd(0) =gender agegr education  risk6 risk16 risk19  dm bmi __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
 __1_1=((bmi-20.20)**3)*(bmi>20.20) 
     -((bmi-24.03)**3)*(bmi>24.03) 
     *(28.40-20.20)/(28.40-24.03) 
     +((bmi-28.40)**3)*(bmi>28.40) 
     *(24.03-20.20)/(28.40-24.03); 
 *--------- Testing variable: whtr ---------; 
	 where binwhtr=2;
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS__cvd_bmi_whtr2 VAR { F FE Z X };  APPEND;  CLOSE __RCS__cvd_bmi_whtr2; 
 QUIT; 

proc sort data= __RCS__cvd_bmi_whtr2;
by x z;
run;
proc transpose data= __RCS__cvd_bmi_whtr2  out =cvd_bmi_whtr2 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="CVD Mortality on BMI,where WHtR��0.5" imagefmt=tiff;
proc sgplot data=cvd_bmi_whtr2 noautolegend;
inset 'p-overall=0.0038' 'p-non-linearity=0.0024'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'CVD Mortality on BMI,where WHtR��0.5' justify=left'B';
run;


proc phreg data=b10 COVOUT OUTEST=__RCS;  *whtr ����Ѫ����������;
MODEL d_py*cvd(0) =gender agegr education risk6 risk16 risk19  dm bmi whtr __1_1 / RL; 
 ********** 0.43  0.5 0.58; 
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
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_CVD VAR { F FE Z X };  APPEND;  CLOSE  __RCS_whtr_CVD  ; 
 QUIT; 

proc sort data= __RCS_whtr_CVD  ;
by x z;
run;
proc transpose data= __RCS_whtr_CVD   out =whtr_CVD prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="CVD Mortality on WHtR " imagefmt=tiff;
proc sgplot data=whtr_CVD noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.4887'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt 'CVD Mortality on WHtR';
run;

proc phreg data=b10 COVOUT OUTEST=__RCS;  *whtr ����Ѫ����������BMI��;
MODEL d_py*cvd(0) =gender agegr education risk6 risk16 risk19  dm whtr __1_1 / RL; 
 ********** 0.43  0.5 0.58; 
 __1_1=((whtr-0.43)**3)*(whtr>0.43) 
     -((whtr-0.5)**3)*(whtr>0.5) 
     *(0.58-0.43)/(0.58-0.5) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.5-0.43)/(0.58-0.5); 
 *--------- Testing variable: whtr ---------; 
	 where binbmi=1;
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_CVD_bmi1 VAR { F FE Z X };  APPEND;  CLOSE  __RCS_whtr_CVD_bmi1  ; 
 QUIT; 

proc sort data= __RCS_whtr_CVD_bmi1  ;
by x z;
run;
proc transpose data= __RCS_whtr_CVD_bmi1   out =whtr_CVD_bmi1 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="CVD Mortality on WHtR where BMI<24" imagefmt=tiff;
proc sgplot data=whtr_CVD_bmi1 noautolegend;
inset 'p-overall=0.0002' 'p-non-linearity=0.4656'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt 'CVD Mortality on WHtR where BMI<24' justify=left'A';
run;

proc phreg data=b10 COVOUT OUTEST=__RCS;  *whtr ����Ѫ����������BMI��;
MODEL d_py*cvd(0) =gender agegr education risk6 risk16 risk19  dm whtr __1_1 / RL; 
 ********** 0.43  0.5 0.58; 
 __1_1=((whtr-0.43)**3)*(whtr>0.43) 
     -((whtr-0.5)**3)*(whtr>0.5) 
     *(0.58-0.43)/(0.58-0.5) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.5-0.43)/(0.58-0.5); 
 *--------- Testing variable: whtr ---------; 
	 where binbmi=2;
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_CVD_bmi2 VAR { F FE Z X };  APPEND;  CLOSE  __RCS_whtr_CVD_bmi2  ; 
 QUIT; 

proc sort data= __RCS_whtr_CVD_bmi2  ;
by x z;
run;
proc transpose data= __RCS_whtr_CVD_bmi2   out =whtr_CVD_bmi2 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="CVD Mortality on WHtR where BMI��24" imagefmt=tiff;
proc sgplot data=whtr_CVD_bmi2 noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.9382'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt 'CVD Mortality on WHtR where BMI��24' justify=left'B';
run;
********************************************************BMI�����߱ȶ���������������Ӱ��*****************************************;
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI�����߱�ͬʱ����ģ�ͣ�BMI�����߱Ⱦ�������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') risk1(ref='1') risk6(ref='1')  dm(ref='0') binwhtr(ref='1') /ref=first;
MODEL d_py*tumor(0) =gender agegr binbmi   risk1   dm  binwhtr/selection=backward slstay=0.05 RL; 
RUN;
/*age gender bmi whtr dm risk1*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI����ģ�ͣ�BMI������;
class  binbmi(ref="1") gender(ref='1') education(ref='1') risk1(ref='1') risk6(ref='1')  dm(ref='0') /ref=first;
MODEL d_py*tumor(0) =gender agegr binbmi   risk1   dm  /selection=backward slstay=0.05 RL; 
RUN;
/*age gender bmi whtr dm risk1*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��whtr����ģ�ͣ�whtr������;
class gender(ref='1') education(ref='1') risk1(ref='1')   dm(ref='0') binwhtr(ref='1') /ref=first;
MODEL d_py*tumor(0) =gender agegr binwhtr   risk1   dm  /selection=backward slstay=0.05 RL; 
RUN;
/*age gender bmi whtr dm risk1*/

/*����HR��95%CI*/
PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI�����߱�ͬʱ����ģ�ͣ�BMI�����߱Ⱦ�������;
class  binbmi(ref="1") gender(ref='1') risk1(ref='1')  dm(ref='0') binwhtr(ref='1') /ref=first;
MODEL d_py*tumor(0) =gender agegr binbmi   risk1   dm  binwhtr/RL; 
RUN;

PROC PHREG DATA=b10 COVOUT OUTEST=__RCS; *��BMI�����߱�ͬʱ����ģ�ͣ�BMI�����߱Ⱦ�������;
class gender(ref='1') risk1(ref='1')  dm(ref='0')  con2(ref='1') /ref=first;
MODEL d_py*tumor(0) =gender agegr   risk1   dm con2  /RL; 
RUN;
/*���㷢������*/
proc freq data=b10;
table binbmi*tumor binwhtr*tumor con2*tumor;
run;

/*��ͼ*/
proc phreg data=b10 COVOUT OUTEST=__RCS;  *BMI ��������������;
MODEL d_py*tumor(0) =gender agegr   risk1  dm bmi  whtr __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
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
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS__tumor_bmi VAR { F FE Z X };  APPEND;  CLOSE __RCS__tumor_bmi; 
 QUIT; 

proc sort data= __RCS__tumor_bmi;
by x z;
run;
proc transpose data= __RCS__tumor_bmi   out =tumor_bmi prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cancer Mortality on BMI" imagefmt=tiff;
proc sgplot data=tumor_bmi noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.0014'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All Cancer Mortality on BMI';
run;


proc phreg data=b10 COVOUT OUTEST=__RCS;  *BMI �������������� �ֲ�ͬ��WHTR��;
MODEL d_py*tumor(0) =gender agegr   risk1  dm bmi __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
 __1_1=((bmi-20.20)**3)*(bmi>20.20) 
     -((bmi-24.03)**3)*(bmi>24.03) 
     *(28.40-20.20)/(28.40-24.03) 
     +((bmi-28.40)**3)*(bmi>28.40) 
     *(24.03-20.20)/(28.40-24.03); 
 *--------- Testing variable: whtr ---------; 
	 where binwhtr=1;
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS__tumor_bmi_whtr1 VAR { F FE Z X };  APPEND;  CLOSE __RCS__tumor_bmi_whtr1; 
 QUIT; 

proc sort data= __RCS__tumor_bmi_whtr1;
by x z;
run;
proc transpose data= __RCS__tumor_bmi_whtr1   out =tumor_bmi_whtr1 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cancer Mortality on BMI Where WHtR<0.5" imagefmt=tiff;
proc sgplot data=tumor_bmi_whtr1 noautolegend;
inset 'p-overall=0.1412' 'p-non-linearity=0.2346'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All Cancer Mortality on BMI Where WHtR<0.5' justify=left'A';
run;

proc phreg data=b10 COVOUT OUTEST=__RCS;  *BMI ���������������ֲ�ͬ��WHTR��;
MODEL d_py*tumor(0) =gender agegr   risk1  dm bmi __1_1 / RL; 
 ********** 20.20  24.03 28.40; 
 __1_1=((bmi-20.20)**3)*(bmi>20.20) 
     -((bmi-24.03)**3)*(bmi>24.03) 
     *(28.40-20.20)/(28.40-24.03) 
     +((bmi-28.40)**3)*(bmi>28.40) 
     *(24.03-20.20)/(28.40-24.03); 
 *--------- Testing variable: whtr ---------; 
	 where binwhtr=2;
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17;     *p1 value for X-axis; 
 UPPEREND=33;     *p99 value for X-axis; 
 REF=24;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS__tumor_bmi_whtr2 VAR { F FE Z X };  APPEND;  CLOSE __RCS__tumor_bmi_whtr2; 
 QUIT; 

proc sort data= __RCS__tumor_bmi_whtr2;
by x z;
run;
proc transpose data= __RCS__tumor_bmi_whtr2   out =tumor_bmi_whtr2 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cancer Mortality on BMI Where WHtR��0.5" imagefmt=tiff;
proc sgplot data=tumor_bmi_whtr2 noautolegend;
inset 'p-overall=0.0077' 'p-non-linearity=0.0024'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(17 to 33 by 4) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All Cancer Mortality on BMI Where WHtR��0.5' justify=left'B';
run;


proc phreg data=b10 COVOUT OUTEST=__RCS;  *whtr ��������������;
MODEL d_py*tumor(0) =gender agegr education risk1  dm bmi whtr __1_1 / RL; 
 ********** 0.43  0.5 0.58; 
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
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_tumor VAR { F FE Z X };  APPEND;  CLOSE  __RCS_whtr_tumor  ; 
 QUIT; 

proc sort data= __RCS_whtr_tumor  ;
by x z;
run;
proc transpose data= __RCS_whtr_tumor   out =whtr_tumor prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cancer Mortality on WHtR " imagefmt=tiff;
proc sgplot data=whtr_tumor noautolegend;
inset 'p-overall<.0001' 'p-non-linearity=0.1912'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt ' All Cancer Mortality on WHtR';
run;

proc phreg data=b10 COVOUT OUTEST=__RCS;  *whtr ��������������,�ֲ�ͬ��bmi��;
MODEL d_py*tumor(0) =gender agegr education risk1  dm whtr __1_1  / RL; 
 ********** 0.43  0.5 0.58; 
 __1_1=((whtr-0.43)**3)*(whtr>0.43) 
     -((whtr-0.5)**3)*(whtr>0.5) 
     *(0.58-0.43)/(0.58-0.5) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.5-0.43)/(0.58-0.5); 
 *--------- Testing variable: whtr ---------; 
	 where binbmi=1;
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_tumor_bmi1 VAR { F FE Z X };  APPEND;  CLOSE  __RCS_whtr_tumor_bmi1  ; 
 QUIT; 

proc sort data= __RCS_whtr_tumor_bmi1  ;
by x z;
run;
proc transpose data= __RCS_whtr_tumor_bmi1   out =whtr_tumor_bmi1 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cancer Mortality on WHtR where BMI<24" imagefmt=tiff;
proc sgplot data=whtr_tumor_bmi1 noautolegend;
inset 'p-overall=0.0068' 'p-non-linearity=0.2641'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt ' All Cancer Mortality on WHtR where BMI<24' justify=left 'A';
run;


proc phreg data=b10 COVOUT OUTEST=__RCS;  *whtr ��������������,�ֲ�ͬ��bmi��;
MODEL d_py*tumor(0) =gender agegr education risk1  dm whtr __1_1  / RL; 
 ********** 0.43  0.5 0.58; 
 __1_1=((whtr-0.43)**3)*(whtr>0.43) 
     -((whtr-0.5)**3)*(whtr>0.5) 
     *(0.58-0.43)/(0.58-0.5) 
     +((whtr-0.58)**3)*(whtr>0.58) 
     *(0.5-0.43)/(0.58-0.5); 
 *--------- Testing variable: whtr ---------; 
	 where binbmi=2;
 EFFECT1: TEST  whtr, __1_1;
 NONLIN1: TEST  __1_1;
 RUN;

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=0.3;     *p1 value for X-axis; 
 UPPEREND=0.7;     *p99 value for X-axis; 
 REF=0.5;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.0.53*/
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
 CREATE __RCS_whtr_tumor_bmi2 VAR { F FE Z X };  APPEND;  CLOSE  __RCS_whtr_tumor_bmi2  ; 
 QUIT; 

proc sort data= __RCS_whtr_tumor_bmi2 ;
by x z;
run;
proc transpose data= __RCS_whtr_tumor_bmi2   out =whtr_tumor_bmi2 prefix=FE; /*ָ��ת�ú�ı���ǰ׺���������COL*/
var FE;
by x;  /*byָ�����ǲ���Ҫת�õı���*/
run;
/* All death male ����ͼ*/
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\All-cause mortality" dpi=300;
ods graphics on/border=off imagename="All Cancer Mortality on WHtR where BMI��24" imagefmt=tiff;
proc sgplot data=whtr_tumor_bmi2 noautolegend;
inset 'p-overall=0.0035' 'p-non-linearity=0.5902'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='whtr';
xaxis label='whtr' values=(0.3 to 0.7 by 0.1) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0 to 12 by 2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'whtr'/position=top location=inside;*/
title justify=medium height=14pt ' All Cancer Mortality on WHtR where BMI��24' justify=left 'B';
run;


/*��������ͼ*/
proc lifetest data=b9 method=km outsurv=estimatesm;
time d_py*d(0);
strata binbmi;
run;
*logrank p=<.0001;
data estimatesm;
length subtype $20.;
set estimatesm;
survt_year=d_py;
if binbmi=1 then subtype='BMI<24';
if binbmi=2 then subtype='BMI��24';
run;
data line;
input id $ value $10. linecolor :$20.;
datalines;      
group BMI<24   darkorange  Bold   
group BMI��24    black       
;
run;
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\survival curve" dpi=300;
ods graphics/border=off imagename="All-Cause mortality,by BMI" imagefmt=tiff;
proc sgplot data=estimatesm  dattrmap=line ;
step x=survt_year y=survival/group=subtype attrid=group lineattrs=(thickness=2);
symbol1 line=1 width=1.5;
symbol2 line=2 width=1.5;
yaxis values=(0.7 0.8 0.9   1) label='Cumulative survival'  LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
xaxis label='Years from Registry'  LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
keylegend /location=inside position=bottomleft;
inset 'Log-rank chi-square=5.6270, P=0.0177'/position=left textattrs=(Family=Arial Size=9 Style=Italic Weight=Bold);
title justify=medium height=14pt   'All-Cause mortality,by BMI';
run;


/*��������ͼ*/
proc lifetest data=b9 method=km outsurv=estimatesm;
time d_py*d(0);
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
if binwhtr=2 then subtype='WHtR��0.5';
run;
data line;
input id $ value $10. linecolor :$20.;
datalines;      
group WHtR<0.5   darkorange  Bold   
group WHtR��0.5   black      
run;
ods listing gpath="D:\1a\learning\research\����20210824\�󳦰�ɸ��\results\survival curve" dpi=300;
ods graphics/border=off imagename="CVD  mortality,by WHtR" imagefmt=tiff;
proc sgplot data=estimatesm  dattrmap=line ;
step x=survt_year y=survival/group=subtype attrid=group lineattrs=(thickness=2);
symbol1 line=1 width=1.5;
symbol2 line=2 width=1.5;
yaxis values=(0.7 0.8 0.9  1) label='Cumulative survival'  LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
xaxis label='Years from Registry'  LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=10 Weight=Bold);  
keylegend /location=inside position=bottomleft;
inset 'Log-rank chi-square=99.6190, P<.0001'/position=left textattrs=(Family=Arial Size=9 Style=Italic Weight=Bold);
title justify=medium height=14pt   'All-Cause mortality,by WHtR';
run;
