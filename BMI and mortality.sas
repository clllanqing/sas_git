PROC PHREG DATA=dmd1 COVOUT OUTEST=__RCS; 
class fam(ref='2') hp ca1 physical(ref='2') /ref=first;
MODEL py*d(0) =  bmi  __1_1 dmage fam hp ca1 physical/RL; 
 
 ********** spline modelling of fixed covariate bmi; 
 ********** with 3 knots located at; 
 ********** 19.53 24.22 30; 
 __1_1=((bmi-19.53)**3)*(bmi>19.53) 
     -((bmi-24.22)**3)*(bmi>24.22) 
     *(30-19.53)/(30-24.22) 
     +((bmi-30)**3)*(bmi>30) 
     *(24.22-19.53)/(30-24.22); 

 
 where gender=2 ;
 *--------- Testing variable: bmi ---------; 
 EFFECT1: TEST  bmi, __1_1;
 NONLIN1: TEST  __1_1;
 RUN; 

 PROC IML; 
 NPOINTS=101;   * Number of points to build the graphic;
 LOWEREND=17.71;     *Smallest value for X-axis; 
 UPPEREND=32.89;     *Largest value for X-axis; 
 REF=25;     /*Reference value for X-axis; *male p1 17.72 p25 22.49 p75 26.20 p99 32.28  female p1 17.71 p25 22.10 p75 26.56 p99 32.89*/
 X=T(DO(LOWEREND,UPPEREND,(UPPEREND-LOWEREND)/(NPOINTS-1))); 
 S1=((X-19.53)##3)#(X>19.53) 
     -((X-24.22)##3)#(X>24.22) 
     #(30-19.53)/(30-24.22) 
     +((X-30)##3)#(X>30) 
     #(24.22-19.53)/(30-24.22) 
     -((REF-19.53)##3)#(REF>19.53) 
     +((REF-24.22)##3)#(REF>24.22)#(30-19.53)/(30-24.22) 
     -((REF-30)##3)#(REF>30)#(24.22-19.53)/(30-24.22); 
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
 CREATE __RCS1 VAR { F FE Z X };  APPEND;  CLOSE __RCS1; 
 QUIT; 

data loghr;
set __rcs1;
where Z=1;
run;
proc sort data=loghr;
by F;  
run;


/*作图*/ 
proc sort data=__RCS1;
by x z;
run;
proc transpose data=__RCS1 out =bmi_graph prefix=FE; /*指定转置后的变量前缀，否则就是COL*/
var FE;
by x;  /*by指定的是不需要转置的变量*/
run;


/* All death male 样条图*/
ods listing gpath="d:\figure" dpi=300;
ods graphics on/border=off imagename="Fig3-a" imagefmt=tiff;
proc sgplot data=bmi_graph noautolegend;
inset 'p-overall<0.01' 'p-non-linearity<0.01'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold) ;
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.4;    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(18 to 32 by 3.5) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.6 0.8 1 1.2 1.4 1.6 1.8 2.0 2.2) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All-cause Mortality,Male';
run;

/* All death female 样条图*/
ods graphics on/border=off;
ods listing gpath="d:\figure" dpi=300;
ods graphics on/border=off imagename="Fig3-b" imagefmt=tiff;
proc sgplot data=bmi_graph noautolegend;
inset 'p-overall<0.01' 'p-non-linearity<0.01'/ position=top textattrs=(Family=Arial Size=12 Style=Italic Weight=Bold);
refline 1/lineattrs=(pattern=dash color=black);
band x=x upper=fe2 lower=fe3 /transparency=0.8 fillattrs=(color=mediumstrongred);    *legendlabel='95% CI' name='band';
series x=x y=fe1/lineattrs=(thickness=2 color=mediumstrongred) ;    *legendlabel='HR' name='BMI';
xaxis label='BMI' values=(18 to 32 by 3.5) LABELATTRS=(Family=Arial Size=12 Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);   
yaxis label='HR (95%CI)' values=(0.8 1 1.2 1.4 1.6 1.8 2.0) LABELATTRS=(Family=Arial Size=12  Weight=Bold) VALUEATTRS=(Family=Arial Size=12 Weight=Bold);  
/*keylegend 'HR' 'BMI'/position=top location=inside;*/
title justify=medium height=14pt 'All-cause Mortality,Female';
run;
