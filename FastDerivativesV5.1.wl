(* ::Package:: *)

(* ::Section:: *)
(*Fast Derivatives*)
(**)


(* Revision History
5.0     Version used as of 10/6/2021
11/2/2022   Did a lot of work on CreateAdjointText to get it to work for fragmented 
	input files. 
11/8/2022   Added hessian soubroutine to reverse derivative output in 
		MakeBackwardsFortRoutine
12/9/2022   Fixed a problem having to do with converting things like
        * -10   to *(-10) in MakeBackwardsFortRoutine
2/14/2023   Fixed a few problems in MakeBackwardsFortRoutine that were needed
        to address problems concerning & and the 511 line limit (see also 
        correction in PolynomialPruningAddingV10.1)
*)


ConvertChenDerivatives[fortranname_]:=Module[
{ifraga,natomsa,rijnamesa,rijnamesnow,polystarta,polyenda,npolya,
nmonoa,nvariables,nmono,npoly,xpn,xfirst,xsec,xsimp,
CharStringF,dpcoef,skipj,dpcoefadd,vlength,lxcoefm,
retpos,charxpn,varpositions,lvp,xcoefm,addpos,print,
skiploop,CharStringdmm,outM,AnotherRoundM,
dpcoeftab,iend,rhs,ltab,newdpcoeftab,dpcoeftxt2,dpcoeftxt1,
CSFText,dpcoeftext,dpcoeflines,newfortran,natom,ndpcoef,
dpcoeflinesch,jsave,CSMappend,CSMremaining,nq,positions,
useversion4,skiprhsreplacements,skiprhs,diagnose,mlastwithx,
mforx,xform,z,qnumber1,pm1,plast
},
print=True;
diagnose=False;
filename[[1]]=fortranname;
useversion4=False; (* if useversion4=false, it uses version3 *)
skiprhsreplacements=False; (* if true, no replacements on the rhs *)

Print["Getting monomials and polynomials"];
(* Get monomials and polynomials info*)
(*{rijnamesnow,nvariables,nmono,npoly}=
	GetDefinitionstoMathematicaFromFortranOutput[];*)
{rijnamesnow,nvariables,nmono,npoly,qnumber1}=
	GetDefinitionstoMathematicaFromFortranOutputPure[];
(* get the last m value (following the m(1)) that has an x( on the rhs *)
 pm1=Quiet[Position[bemsaTabc[[All,1]],"m(1)"][[1,2]]];
 Do[(
If[!StringContainsQ[bemsaTabc[[i,3]],"x("],
plast=i-1;
Break[];
];
),{i,pm1,pm1+nvariables}];
mlastwithx=ToExpression[StringDrop[StringDrop[bemsaTabc[[plast,1]],2],-1]];
Print["The number of monomials with x on the rhs is ",mlastwithx];

(* now get the derivatives *)
Print[DateString[]];
Print["Getting Derivatives from fortranname file"];
{CharStringdmono,CharStringdpoly,CharStringdmm}=
	GetDerivDefinitionstoMathematicaFromFortranOutput[nvariables,mlastwithx];
CharStringdmono=
	ModifyCharStringdmonoForConversion[CharStringdmono,nvariables,mlastwithx];
dmlist=Table["dm"<>ToString[i],{i,0,nmono}];(* needs to be global *)
dplist=Table["dp"<>ToString[i],{i,0,npoly}];  (* needs to be global *)
natom=natomsparent;
If[diagnose,
	PLHnmono=nmono;PLHnpoly=npoly;
	PLHCSdM=CharStringdmono;
	PLHCSdP=CharStringdpoly;
	PLHCSdmm=CharStringdmm;
];

(* get nq *)
(*Print["Getting nq"];
If[StringContainsQ[bemsaTxtc,")::q"],
	positions=StringPosition[bemsaTxtc,"dimension(1"~~___~~")::q"];
	nq=ToExpression[StringTake[bemsaTxtc,{12+positions[[1,1]],positions[[1,2]]-4}]];
	,
	nq=-1;
];*)
nq=qnumber1;
Print["   nq = ",nq];


Print[DateString[]];
Do[(
	ToExpression["Clear[m"<>ToString[i]<>"]"];
),{i,1,nmono}];
Do[(
	ToExpression["Clear[p"<>ToString[i]<>"]"];
),{i,1,npoly}];
Do[(
	ToExpression["Clear[q"<>ToString[i]<>"]"];
),{i,1,nq}];
Print["Evaluating mono and poly strings for symbolic results"];
ToExpression[CreateEvaldMono[bemsaToMathematica[StringDelete[CharStringdmono,"dm(0)=0.\n"]]]];
ToExpression[CreateEvaldPoly[bemsaToMathematica[StringDelete[CharStringdpoly,"dp(0) = 0.
    "]]]];
    
Print[DateString[]];
Print["Evaluating dmono and dpoly strings for symbolic results"];
dEM=EvaldMono[];
dEM=Drop[dEM,1];
dEP=EvaldPoly[];
dEP=Drop[dEP,1];
If[diagnose,PLHdEM=dEM;PLHdEP=dEP;];

(* get mforx and xform *)
(*{mforx,xform}=makemforxform[nvariables];*)

Print[DateString[]];
Print["Starting big double loop over i (the polynomial index) and j (the dm index, of len nvar)"];
CharStringF="";
dpcoef="";
n=0;
Do[( (* over i *)
xsimp=dEP[[i]];
Do[( (* over j *)
idyn=i; (* global *)
jdyn=j; (* global *)
xcoef=Coefficient[xsimp,ToExpression["dmm"<>ToString[j]]];
If[xcoef==0,Goto[skipj];];
n=n+1;
CharStringF=CharStringF<>
	"    indx("<>ToString[n]<>","<>ToString[1]<>") = "
			<>ToString[i]<>"\n";
CharStringF=CharStringF<>
	"    indx("<>ToString[n]<>","<>ToString[2]<>") = "
			<>ToString[j]<>"\n";
dpcoefadd="    dpcoef("<>ToString[n]<>") = "<>
	StringDelete[ToString[xcoef,InputForm],"\n"]<>"\n";
dpcoef=dpcoef<>dpcoefadd;
Label[skipj];
(*),{j,1,nvariables}];*)
),{j,1,mlastwithx}];
),{i,1,Length[dEP]}];
ndpcoef=n;
Print[DateString[]];
Print["     double loop has ended"];
Print["     ndpcoeff = ",ndpcoef];
Print[DateString[]];


Print["Starting string replacements"];
jdyn=0;
Do[(
idyn=i;
dpcoef=StringReplace[dpcoef,"p"<>ToString[i]-> "p("<>ToString[i]<>")"];
),{i,npoly,1,-1}];
Do[(
idyn=i;
dpcoef=StringReplace[dpcoef,"m"<>ToString[i]-> "m("<>ToString[i]<>")"];
),{i,nmono,1,-1}];
dpcoef=StringReplace[dpcoef,"= 1"->"= 1.d0"];
dpcoef=StringReplace[dpcoef,"^"->"**"];
If[nq>0,
 Do[(
   idyn=i;
   dpcoef=StringReplace[dpcoef,"q"<>ToString[i]->"q("<>ToString[i]<>")"];
 ),{i,nq,1,-1}];
 ];

If[skiprhsreplacements,Goto[skiprhs];];
(* Convert the files back to output form of text *)
Export[DataDir<>"DeleteMe.txt",CharStringF,"Text"];
CSFText=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Export[DataDir<>"DeleteMe.txt",dpcoef,"Text"];
dpcoeftext=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];


(* Convert dpcoeftext to Tabular form and find duplicate rh sides *)
(* replace duplicate rhs by first dpcoef *)
Print["Finding duplicate rh sides and replacing"];
nstar=StringCount[dpcoeftext,"*"];
nplus=StringCount[dpcoeftext,"+"];
nminus=StringCount[dpcoeftext,"-"];
Print["     starting {mult,add/sub} = ", {nstar,nplus+nminus}];
Export[DataDir<>"DeleteMe.txt",dpcoef,"Text"];
dpcoeftab=Import[DataDir<>"DeleteMe.txt",{"Table"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
(* make list of rhs's *)
iend=Length[dpcoeftab];
rhs=Table[0,{j,1,iend}];
If[useversion4,
Do[(
idyn=iend-i; (* global *)
ltab=Length[dpcoeftab[[i]]];
rhs[[i]]=StringJoin[Take[dpcoeftab[[i]],{3,ltab}]];
),{i,1,iend}];
StringReplace[rhs,"StringJoin[1.]"->"1.d0"];
,
Do[(
ltab=Length[dpcoeftab[[i]]];
rhs[[i]]=Take[dpcoeftab[[i]],{3,ltab}];
),{i,1,iend}];
];
(* replace duplicates *)
If[useversion4,
  Do[(
  idyn=i;
   Do[(
   jdyn=j;
    If[StringContainsQ[rhs[[j]],rhs[[i]]],
    rhs[[j]]=StringReplace[rhs[[j]],rhs[[i]]->dpcoeftab[[i,1]]];
    ];
   ),{j,i+1,iend}];
  ),{i,1,iend}];
,
  Do[(
  idyn=i;
   Do[(
    jdyn=j;
    (*If[StringContainsQ[rhs[[j]],"dpcoef"],Goto[skipj];];*)
    If[rhs[[j]]==rhs[[i]] ,
    rhs[[j]]=dpcoeftab[[i,1]];
   ];
  (*Label[skipj];*)
  ),{j,i+1,iend}];
),{i,1,iend}];
];
newdpcoeftab=dpcoeftab;
Do[(
newdpcoeftab[[i]]=Flatten[{"    "<>dpcoeftab[[i,1]],"=",rhs[[i]]}];
),{i,1,iend}];
(* return to text *)
Export[DataDir<>"DeleteMe.txt",newdpcoeftab,"Table"];
dpcoeftxt1=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
dpcoeftxt2=StringReplace[dpcoeftxt1,"\t"->" "];
dpcoef=dpcoeftxt2;
nstar=StringCount[dpcoef,"*"];
nplus=StringCount[dpcoef,"+"];
nminus=StringCount[dpcoef,"-"];
Print["     ending {mult,add/sub} = ",{nstar,nplus+nminus}];
Label[skiprhs];

(* convert dpcoef to lines of text adding ampersands*)
Print[DateString[]];
If[diagnose,PLHdpcoef444=dpcoef];
Print["Adding line carry ampersands"];
dpcoef=StringDelete[dpcoef,"\t"];
dpcoef=StringReplace[dpcoef,"+"->" + "];
dpcoef=StringReplace[dpcoef,"-"->" - "];
If[diagnose,PLHdpcoef445=dpcoef];
Export[DataDir<>"DeleteMe.txt",dpcoef,"Text"];
dpcoeflines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
If[diagnose,PLHdpcoeflines446=dpcoeflines];
DeleteFile[DataDir<>"DeleteMe.txt"];
newfortran=Ampersand[dpcoeflines];
If[diagnose,PLHnewfortran447=newfortran];


Print[DateString[]];
Print["Making the Fortran file"];
dpcoef=newfortran;
natom=natomsparent;
(* nmono, npoly and ndpcoef are defined earlier *)

z=MakeExportFortranFastDerivatives[natom,nmono,npoly,ndpcoef,nvariables,
		nq,dpcoef,CharStringF,CharStringdmm,mlastwithx];

{z,CharStringF,dpcoef,CharStringdmm}
];


Getdpolymonostartendpoints[ifraga_]:=Module[
{bemsaa,dmonostarta,dmonoenda,dpolystarta,dpolyenda,isave,diagnose
},
diagnose=False;
(* get mono and poly strings for fragment a *)
If[ifraga==-1,
	bemsaa=bemsaTabc;  (* uses the Fortran output as input *)
	,
	bemsaa=ToExpression["bemsaTabc"<>ToString[ifraga]];
];
If[diagnose,PLHbemsaa1=bemsaa];
Do[(
	If[Length[bemsaa[[i]]]>0 && bemsaa[[i,1]]=="dm(0)",isave=i;Break[];];
),{i,1,Length[bemsaa]}];
dmonostarta=isave;
Do[(
	If[Or[Length[bemsaa[[i]]]==0 , StringTake[bemsaa[[i,1]],3]!="dm("(*<>ToString[i-monostarta]<>")"*)],
		isave=i;Break[];];
),{i,dmonostarta+1,Length[bemsaa]}];
dmonoenda=isave-1;
Do[(
	If[Length[bemsaa[[i]]]>0 && bemsaa[[i,1]]=="dp(0)",isave=i;Break[];];
	),{i,1,Length[bemsaa]}];
dpolystarta=isave;
Do[(
	(*If[Or[Length[bemsaa[[i]]]==0 , StringTake[bemsaa[[i,1]],3]!="dp("(*<>ToString[i-polystarta]<>")"*)],*)
	If[Length[bemsaa[[i]]]==0,
		isave=i;
		Break[];
		];
),{i,dpolystarta+1,Length[bemsaa]}];
dpolyenda=isave-1;
If[diagnose,PLHstartend={dmonostarta,dmonoenda,dpolystarta,dpolyenda};];
{dmonostarta,dmonoenda,dpolystarta,dpolyenda}
];



Getdmonopolycharstrings[ifraga_,dmonostarta_,dmonoenda_,
				dpolystarta_,dpolyenda_]:=Module[
{bemsaa,CharStringdmonoa,CharStringdpolya,diagnose},
diagnose=False;
If[ifraga==-1,
	bemsaa=bemsaTxtLnc;  (* uses the Fortran output as input *)
	,
	bemsaa=ToExpression["bemsaTxtLnc"<>ToString[ifraga]];
];
If[diagnose,PLHbemsaa=bemsaa;];
CharStringdmonoa=bemsaa[[dmonostarta]];
Do[(
CharStringdmonoa=CharStringdmonoa<>"
"<>bemsaa[[i]];
),{i,dmonostarta+1,dmonoenda}];
CharStringdpolya=bemsaa[[dpolystarta]];
Do[(
CharStringdpolya=CharStringdpolya<>"
"<>bemsaa[[i]];
),{i,dpolystarta+1,dpolyenda}];

{CharStringdmonoa,CharStringdpolya}
];




ModifyCharStringdmonoForConversion[CharStringdmono_,nvariables_,mlastwithx_]:=Module[
{CharStringdmono1,bemsaC,ll,diagnose},
Export[DataDir<>"DeleteMe.txt",CharStringdmono,"Text"];
        Bx=Import[DataDir<>"DeleteMe.txt","Table"];
        DeleteFile[DataDir<>"DeleteMe.txt"];
diagnose=False;
Do[(
Bx[[1+i,3]]="dmm("<>ToString[i]<>")";
If[Length[Bx[[1+i]]]>3,
	Do[(
		Bx[[1+i]]=Drop[Bx[[1+i]],{j}];
	),{j,Length[Bx[[1+i]]],4,-1}];
];
),{i,1,mlastwithx}];
Bx[[1,3]]="0.";
If[diagnose,PLHBx534=Bx;];
Do[(
ll=Length[Bx[[i]]];
Bx[[i,ll]]=Bx[[i,ll]]<>"\n";
),{i,1,Length[Bx]}];
If[diagnose,PLHBx535=Bx;];
StringJoin[Bx]
];


GetDerivDefinitionstoMathematicaFromFortranOutput[nvariables_,mlastwithx_]:=Module[
{ifrag1,dmonostart1,dmonoend1,dpolystart1,dpolyend1,CharStringdmono1,
CharStringdpoly1,ndpoly,ndmono,CharStringdmono,CharStringdpoly,tijn,subbesma,
rijnamesnow,xsum,msum,psum,ifraga,natomsa,rijnamesa,npoly,nmono,
CharStringAssignx,CharStringAdd,CharStringdmm,diagnose
},

diagnose=False;
(* value of ifrag-1 tells these two functions to use the Fortran output file as an input *)
ifrag1=-1; 
CreatebemsaFiles[ifrag1];
{dmonostart1,dmonoend1,dpolystart1,dpolyend1}=
	Getdpolymonostartendpoints[ifrag1];
{CharStringdmono1,CharStringdpoly1}=
	Getdmonopolycharstrings[ifrag1,dmonostart1,dmonoend1,dpolystart1,dpolyend1];
If[diagnose,Print[{dmonostart1,dmonoend1,dpolystart1,dpolyend1}];];	
CharStringdmm=CharStringdmono1;
If[diagnose,PLHCharStringpolya=CharStringdmm;];

Export[DataDir<>"DeleteMe.txt",CharStringdmm,"Text"];
dmmlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
dmmlines=Take[dmmlines,{2,1+mlastwithx}];
If[diagnose,PLHdmmlines1=dmmlines];
Do[(
dmmlines[[i]]=StringReplace[dmmlines[[i]],"dm"-> "    dmm"]<>"\n";
dmmlines[[i]]=StringReplace[dmmlines[[i]],"flag"-> "iflag"];
),{i,1,mlastwithx}];
CharStringdmm=StringJoin[dmmlines];
If[diagnose,PLHCharStringdmm1=CharStringdmm];


CharStringdmono=CharStringdmono1;
CharStringdmono=StringReplace[CharStringdmono,"dm(0) = 0.D0"->"dm(0) = 0."];
CharStringdmono=StringReplace[CharStringdmono,"dm(0) = 0.0D0"->"dm(0) = 0."];

CharStringdpoly=CharStringdpoly1;
CharStringdpoly=StringReplace[CharStringdpoly,"dp(0) = 0.d0"->"dp(0) = 0."];
CharStringdpoly=StringReplace[CharStringdpoly,"dp(0) = 0.0d0"->"dp(0) = 0."];
CharStringdpoly=StringReplace[CharStringdpoly,"dp(0) = 0.d0"->"dp(0) = 0."];
CharStringdpoly=StringReplace[CharStringdpoly,"dp(0) = 0.0d0"->"dp(0) = 0."];
If[diagnose,
	PLHCharStrringdmono533=CharStringdmono;
	PLHCharStrringdpoly533=CharStringdpoly;
	PLHCharStringdmm533=CharStringdmm;
];
{CharStringdmono,CharStringdpoly,CharStringdmm}
];




CreateEvaldMono[StringList_]:=Module[
{Stringout},

Stringout="EvaldMono[]:=Module[{}, a=2;  "<>StringList<>"ToExpression[dmlist]];"
];


CreateEvaldPoly[StringList_]:=Module[
{Stringout},

Stringout="EvaldPoly[]:=Module[{},  "<>StringList<>"ToExpression[dplist]];"
];


MakeExportFortranFastDerivatives[natom_,nmono_,npoly_,ndpcoef_,nvariables_,
		nq_,dpcoef_,CharStringF_,CharStringdmm_,mlastwithx_]:=Module[
{newpartend1,newpartend2,xending,FinishUp,newfortranstrt,newfortran,
newfortranstrt2,z,newpart2,diagnose
},
diagnose=False;

Print[""];
Print["Preparing to make and export Fortran file with fast derivatives"];





(* enter fortran template pieces*)
newfortranstrt="
!   instructions: 
!      a. add the two subroutines below somewhere in the main part 
!         of the bemsa module
!      b. in the calling main program, add at the beginning the statement
!           integer::indx(xxxxx,2)
!         The correct version of this statement is found on line 2 of 
!         subroutine init(indx), just below these instructions  
!      c. in the calling main program, add 
!	        call init(indx)
!         to be called once at the beginning of the program.
  subroutine init(indx)
integer::indx(dpcoef1047,2)
";
newfortran=newfortranstrt;
newfortranstrt2="
    return
  end subroutine init

  real function gemsavfast(x,m,p,c,xyz,indx) result(gf)
    implicit none
    integer::iflag,matom,xyzind,ndpcoef,natm,idpcoef,i,k
    real(wp)::sum
    real(wp),dimension(1:x1047)::x
    real(wp),dimension(xyz1047,3)::xyz
    real(wp),dimension(0:m1047)::m
    real(wp),dimension(0:p1047)::p
    real(wp),dimension(0:p1047)::c
    real(wp),dimension(1:gf1047)::gf
    real(wp),dimension(1:dpcoef1047)::dpcoef
    integer::indx(dpcoef1047,2)
    real(wp),dimension(1:dmmzxcv)::dmm
    real(wp),dimension(1:dmmzxcv)::sumfordmm

    ndpcoef=dpcoef1047
    natm=xyz1047
";
  (*dpcoef=StringReplace[dpcoef,"StringJoin[1.]"->"1.d0"];*)
  newfortran=newfortran<>CharStringF<>newfortranstrt2<>dpcoef;
	newpart2="
!	replace dpcoef(idpcoef) by c(indx(idpcoef,1)*dpcoef(idpcoef)
!	also group and add the new dpcoef(idpcoef) by the dmm that they multiply
    sumfordmm(:)=0.d0
    do idpcoef=1,ndpcoef
            sumfordmm(indx(idpcoef,2))=sumfordmm(indx(idpcoef,2))+ &
		       c(indx(idpcoef,1))*dpcoef(idpcoef)
    enddo    
";
newfortran=newfortran<>newpart2;
    newpartend1="
    gf(:)=0.d0
    do iflag = 1, 3*natm
";
     
 newpartend2="
    gf(iflag)=dot_product(sumfordmm,dmm)
	enddo
    return
  end function gemsavfast ";
newfortran=newfortran<>newpartend1<>"\n"<>CharStringdmm<>newpartend2;

(* fix dimensions *)

z=StringReplace[newfortran,"x1047"->  ToString[nvariables]];
z=StringReplace[z,"m1047"->  ToString[nmono]];
z=StringReplace[z,"p1047"->  ToString[npoly]];
z=StringReplace[z,"dpcoef1047"-> ToString[ndpcoef]];
z=StringReplace[z,"dmmzxcv"->  ToString[mlastwithx]];
z=StringReplace[z,"gf1047"->  ToString[3*natom]];
z=StringReplace[z,"xyz1047"->  ToString[natom]];
z=StringReplace[z,"12z1047"->  ToString[natomsparent]];


Label[FinishUp];
Print["Done"];
(*Export[foutname,z,"Text"];*)
Print["Fortran File Exported"];
z
];


 ConvertToExpansionInX[fortranname_]:=Module[
{print,nvariables,nmono,npoly,CharStringdmono,
CharStringdpoly,CharStringdmm,natom,nq,positions,
newfortran,qlines,q(*,exp,
EMFort,EPFort,dEMFort,dEPFort,EM,EP,dEM,dEP*)
},
print=False;
filename[[1]]=fortranname;


Print["Getting monomials and polynomials"];
(* Get monomials and polynomials info*)
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
EM=EvalMono[];
EM=Drop[EM,1];
EP=EvalPoly[];
EP=Drop[EP,1];

(* now get the derivatives *)
Print[DateString[]];
Print["Getting Derivatives from fortranname file"];
{CharStringdmono,CharStringdpoly,CharStringdmm}=
	GetDerivDefinitionstoMathematicaFromFortranOutput[nvariables];
(*CharStringdmono=ModifyCharStringdmonoForConversion[CharStringdmono,nvariables];*)
dmlist=Table["dm"<>ToString[i],{i,0,nmono}];(* needs to be global *)
dplist=Table["dp"<>ToString[i],{i,0,npoly}];  (* needs to be global *)
natom=natomsparent;
PLHCSM=CharStringdmono;
PLHCSP=CharStringdpoly;

(* get nq *)
Print["Getting nq"];
If[StringContainsQ[bemsaTxtc,")::q"],
	positions=StringPosition[bemsaTxtc,"dimension(1"~~___~~")::q"];
	nq=ToExpression[StringTake[bemsaTxtc,{12+positions[[1,1]],positions[[1,2]]-4}]];
	,
	nq=-1;
];
Print["   nq = ",nq];


Print[DateString[]];
Print["Evaluating mono and poly strings for symbolic results"];
(*ToExpression[CreateEvaldMono[bemsaToMathematica[StringDelete[CharStringdmono,"dm(0)=0.\n"]]]];*)
ToExpression[CreateEvaldPoly[bemsaToMathematica[StringDelete[CharStringdpoly,"dp(0) = 0.
    "]]]];
    
Print[DateString[]];
Print["Evaluating dmono and dpoly strings for symbolic results"];
(* do monos differently *)
CharStringdmm=GetFMonoDerivativesInx[];
Export[DataDir<>"DeleteMe.txt",CharStringdmm,"Text"];
dmmlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
dmmlines=StringDelete[dmmlines,"("];
dmmlines=StringDelete[dmmlines,")"];
dmmlines=StringDelete[dmmlines," "];
exp="";
Do[(
	exp=exp<>dmmlines[[i]]<>"; ";
),{i,1,Length[dmmlines]}];
dEM=Drop[ToExpression[dmlist],1];
ToExpression[exp];
dEP=EvaldPoly[];
dEP=Drop[dEP,1];

EM=Expand[EM];
EP=Expand[EP];
dEM=Expand[dEM];
dEP=Expand[dEP];

(* convert EM to fortran *)
Print["    Converting EM to Fortran"];
q="";
Do[(
q=q<>"m("<>ToString[i]<>") = "<>ToString[EM[[i]],InputForm]<>"\n";
),{i,1,Length[EM]}];
Do[(
q=StringReplace[q,"x"<>ToString[i]->"x("<>ToString[i]<>") "]
),{i,1,nvariables}];
q=StringReplace[q,"^"->"**"];
q=StringReplace[q," "->""];
q=StringReplace[q,"="->" = "];
q=StringReplace[q,"+"->" + "];
q=StringReplace[q,"-"->" - "];
(* convert q to lines of text adding ampersands*)
Export[DataDir<>"DeleteMe.txt",q,"Text"];
qlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
EMFort=Ampersand[qlines];

(* convert EP to fortran *)
Print["    Converting EP to Fortran"];
q="";
Do[(
q=q<>"p("<>ToString[i]<>") = "<>ToString[EP[[i]],InputForm]<>"\n"
),{i,1,Length[EP]}];
Do[(
q=StringReplace[q,"x"<>ToString[i]->"x("<>ToString[i]<>") "]
),{i,1,3}];
q=StringReplace[q,"^"->"**"];
q=StringReplace[q," "->""];
q=StringReplace[q,"="->" = "];
q=StringReplace[q,"+"->" + "];
q=StringReplace[q,"-"->" - "];
(* convert dpcoef to lines of text adding ampersands*)
Export[DataDir<>"DeleteMe.txt",q,"Text"];
qlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
EPFort=Ampersand[qlines];

(* convert dEM to fortran *)
Print["    Converting dEM to Fortran"];
q="";
Do[(
q=q<>"dm("<>ToString[i]<>") = "<>ToString[dEM[[i]],InputForm]<>"\n"
),{i,1,Length[dEM]}];
Do[(
q=StringReplace[q,"x"<>ToString[i]->"x("<>ToString[i]<>") "]
),{i,1,3}];
q=StringReplace[q,"^"->"**"];
q=StringReplace[q," "->""];
q=StringReplace[q,"="->" = "];
q=StringReplace[q,"+"->" + "];
q=StringReplace[q,"-"->" - "];
(* convert dpcoef to lines of text adding ampersands*)
Export[DataDir<>"DeleteMe.txt",q,"Text"];
qlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
dEMFort=Ampersand[qlines];

(* convert dEP to fortran *)
Print["    Converting dEP to Fortran"];
q="";
Do[(
q=q<>"dp("<>ToString[i]<>") = "<>ToString[dEP[[i]],InputForm]<>"\n"
),{i,1,Length[dEP]}];
Do[(
q=StringReplace[q,"x"<>ToString[i]->"x("<>ToString[i]<>") "]
),{i,1,3}];
q=StringReplace[q,"^"->"**"];
q=StringReplace[q," "->""];
q=StringReplace[q,"="->" = "];
q=StringReplace[q,"+"->" + "];
q=StringReplace[q,"-"->" - "];
(* convert dpcoef to lines of text adding ampersands*)
Export[DataDir<>"DeleteMe.txt",q,"Text"];
qlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
dEPFort=Ampersand[qlines];

Print["Done"];
{EM,EP,dEM,dEP,EMFort,EPFort,dEMFort,dEPFort}

];


 GetFMonoDerivativesInx[]:=Module[
{natoms1,rijnames1,ifrag1,nvariables1,monostart1,monoend1,polystart1,
polyend1,CharStringmono1,CharStringpoly1,CharStringmono,Bx,
first,second,C3,Skipj,Fmonoderivatives,
xsum,ifraga,natomsa,rijnamesa,nvariablesa,monostarta,monoenda,
polystarta,polyenda,rijnamesnow,nvariables,nmono,npoly,FMDlines,
nstar,xterms,diagnose
},

diagnose=False;

(* get character strings CharStringmono and Charstringpoly from Fortran output file *)
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
nvariablesa=natomsa (natomsa-1)/2;
If[ifraga==-1,nvariablesa=Length[rijnamesa];];
(* get mono and poly strings for fragment a *)
{monostarta,monoenda,polystarta,polyenda}=
	Getpolymonostartendpoints[ifraga];
{CharStringmono1,CharStringpoly1}=
	Getmonopolycharstrings[ifraga,monostarta,monoenda,polystarta,polyenda];
	
(*
(*  this one is for testing against Chen's output gets Fortran from bemsa *)
(* get character strings CharStringmono and Charstringpoly *)
MakerijnamesAndnatomsfrag[];
Do[(CreatebemsaFiles[ifrag]),{ifrag,1,nfragments}];
natoms1=natomsfrag[[1]];
rijnames1=rijnames[[1]];
ifrag1=1;
nvariables1=natoms1 (natoms1-1)/2;
(* get mono and poly strings for fragment a *)
{monostart1,monoend1,polystart1,polyend1}=
	Getpolymonostartendpoints[ifrag1];
{CharStringmono1,CharStringpoly1}=
	Getmonopolycharstrings[ifrag1,monostart1,monoend1,polystart1,polyend1];
*)	

(* make Bx, tabular form of monomials *)
CharStringmono=CharStringmono1;
If[diagnose,PLHCSM1=CharStringmono1;];
CharStringmono=StringReplace[CharStringmono,"**"->"^"];
Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
Bx=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
Bx=Drop[Bx,1];  (* get rid of monomial 0 *)

(* doctor up what can be done simply *)
CharStringmono=CharStringmono1;
CharStringmono=StringReplace[CharStringmono,"**"->"^"];
CharStringmono=StringReplace[CharStringmono,"    m(0) = 1.0D0"->"    dm(0) = 0.0D0"];
CharStringmono=StringReplace[CharStringmono,"    m("->"    dm("];
If[diagnose,PLHCSM2=CharStringmono;];

(* fix the x values  *)
Do[(
	If[StringTake[Bx[[i,3]],1]=="x",
		(*CharStringmono=StringReplace[CharStringmono,Bx[[i,3]]->
			 "-"<>Bx[[i,1]]<>"/a*drdx(flag,"<>StringDrop[Bx[[i,3]],2]]*)
			 CharStringmono=StringReplace[CharStringmono,Bx[[i,3]]->
			 "d"<>Bx[[i,3]],1];
	];
),{i,1,Length[Bx]}];

(* change multiplied monomials to derivative form *)
Do[(  
	If[StringCount[Bx[[i,3]],"*"]==1,
		first={};
		C3=Characters[Bx[[i,3]]];
		Do[(If[C3[[j]]!="*",first=Append[first,C3[[j]]];,
			Break[];]),{j,1,Length[C3]}];
		first=StringJoin[first];
		second={};
		Do[(	
			If[C3[[j]]!="*",
				Goto[Skipj];
				,
				jsave=j;
				Break[];
			];
		Label[Skipj];
		),{j,1,Length[C3]}];
		second=StringDrop[StringJoin[C3],jsave];
		(*Print[first<>"*"<>second<>"   \[Rule]   "<>
			first<>"*d"<>second<>"+"<>second<>"* d"<>first];*)
		CharStringmono=StringReplace[CharStringmono,first<>"*"<>second->
		"d"<>first<>" * "<>second<>" + "<>first<>" * d"<>second];
		];
		If[StringCount[Bx[[i,3]],"*"]>1,
			nstar=StringCount[Bx[[i,3]],"*"];
			xterms=parseterms[nstar,Bx[[i,3]]];
			(*Print["Bx[[i]] = ",Bx[[i]]];
			Print["Bx[[i,3]] = ",Bx[[i,3]]];
			Print["nstar = ",nstar];
			Print["xterms = ",xterms];
			Print["productderivative = ",productderivative[nstar+1,xterms]];
			Abort[];*)
			CharStringmono=StringReplace[CharStringmono,Bx[[i,3]]-> 
				productderivative[nstar+1,xterms]];
		];
),{i,1,Length[Bx]}];
If[diagnose,PLHCSM5=CharStringmono;];
CharStringmono=StringReplace[CharStringmono,"^"->"**"];
CharStringmono=StringReplace[CharStringmono," * "->"*"];
Fmonoderivatives=CharStringmono;

(* convert Fgroup to Lines of Text *)
Export[DataDir<>"DeleteMe.txt",Fmonoderivatives,"Text"];
FMDlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	If[StringContainsQ[FMDlines[[i]],"drdx"],
		FMDlines[[i]]=
			StringDrop[FMDlines[[i]],-1]<>",xyz)";
	];
	FMDlines[[i]]=FMDlines[[i]]<>"\n";
),{i,1,Length[FMDlines]}];

Fmonoderivatives=StringJoin[FMDlines];
If[diagnose,PLHFmonoder=Fmonoderivatives];

Fmonoderivatives
];



CreateAdjointText[mnumber_,pnumber_,qnumber_]:=Module[
{polystarta,polyenda,npoly,nmono,nq,mstrln,mtxt,pstrln,ptxt,mpqtxt,mpqtxttab,
pliststr,mliststr,ltr,natoms,nvar,fouttxt,mforx,rcombos,xform,
k,ifrag1,monostart1,monoend1,polystart1,polyend1,CSMforBackward,CSPforBackward,
qliststr,mtxttab,ptxttab,lastp,iatom,icart,ivar,skipj,txttr,
diagnose,nx,continueon,txttabred,txtlns,nexti,istart,iend,txt1047,txttab,
kx,nm,xnumber,mnumber1,pnumber1,qnumber1,nextj,skipi,numbers1,numbers2
},

diagnose=True;

natoms=natomsparent;
nmono=mnumber;
nq=qnumber;
npoly=pnumber;
nvar=natoms (natoms-1)/2;
{xnumber,mnumber1,pnumber1,qnumber1}=Getxmpq[inputfname];
If[xnumber<nvar,
	Print["It appears that inputfname is for a fragmented basis set."];
	Print["  Proceeding on that assumption."];
	nvar=xnumber;
	Print["Current {nvar,mnumber,pnumber,qnumber} =",{nvar,mnumber,pnumber,qnumber}];
	];
If[Or[mnumber!=mnumber1,pnumber!=pnumber1,qnumber!qnumber1],
	Print["Difficulty with definitions of mnumber,pnumber, or qnumber."];
	Print["Aborting"];
	Abort[];
	];

Print["Getting mono/poly character strings from fortranname"];
(*{polystarta, polyenda, npoly, nmono}=GetAssignEVMonoPoly[-1,natomsparent,rijnames[[1]]];*)
ifrag1=-1; 
CreatebemsaFiles[ifrag1];
{monostart1,monoend1,polystart1,polyend1}=
	GetpolymonostartendpointsPure[ifrag1];
{CSMforBackward,CSPforBackward}=
	GetmonopolycharstringsPure[ifrag1,monostart1,monoend1,polystart1,polyend1];
CSMforBackward=StringReplace[StringReplace[StringReplace[StringReplace[
	CSMforBackward,"*"->" * "],"+"->" + "],"-"->" -"],"  "->" "];	
CSPforBackward=StringReplace[StringReplace[StringReplace[StringReplace[
	CSPforBackward,"*"->" * "],"+"->" + "],"-"->" -"],"  "->" "];	
If[diagnose,PLHCSMforBackward=CSMforBackward;];
If[diagnose,PLHCSPforBackward=CSPforBackward;];
		
Print["Making lists"];
Export[DataDir<>"DeleteMe.txt",CSMforBackward,"Text"];
mstrln=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}]; (* includes m(0) *)
mtxttab=Import[DataDir<>"DeleteMe.txt","Table"]; (* includes m(0), in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
(*mtxt=Drop[bemsaToMathematica[mstrln],1];*)
mtxt=bemsaToMathematica[mstrln]; (* contains m0 *)
Export[DataDir<>"DeleteMe.txt",CSPforBackward,"Text"];
pstrln=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
ptxttab=Import[DataDir<>"DeleteMe.txt","Table"]; (* in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
(*ptxt=Drop[bemsaToMathematica[pstrln],1];*)
ptxt=bemsaToMathematica[pstrln]; (* contains p(0) if no q's; does not if q's present *)
If[diagnose,PLHptxt=ptxt;];

If[nq==0,
    mpqtxttab=Append[mtxttab,{"p(0)","=","m(0)"}];
	mpqtxttab=Join[mpqtxttab,ptxttab];
	mpqtxt=Append[mtxt,"p0=m0; "];
	mpqtxt=Join[mpqtxt,ptxt];
	,
	mpqtxttab=Append[mtxttab,{"p(0)","=","m(0)"}];
	mpqtxttab=Join[mpqtxttab,ptxttab];
	mpqtxt=Append[mtxt,"p0=m0; "];
	mpqtxt=Join[mpqtxt,ptxt];
];
If[diagnose,PLHmpqtxttab1=mpqtxttab;PLHmpqtxt1=mpqtxt;];

(* get rid of entries to ptxt that are above the last pxxx= statement *)
Print["get rid of entries to ptxt that are above the last pxxx= statement"];
lastp=1;
Do[(
If[StringContainsQ[mpqtxt[[i]],"p"],lastp=i;];
),{i,1,Length[mpqtxt]}];
If[lastp<Length[mpqtxt],
mpqtxt=Drop[mpqtxt,{lastp+1,Length[mpqtxt]}];
mpqtxttab=Drop[mpqtxttab,{lastp+1,Length[mpqtxttab]}];
];
nq=nq-((Length[mpqtxt]-(lastp+1))+1);
Print["   nq is now = ",nq];
If[diagnose,PLHmpqtxttab2=mpqtxttab;PLHmpqtxt2=mpqtxt;];

(*Print[ptxt];*)
pliststr="";
Do[(
pliststr=pliststr<>"p"<>ToString[i-1]<>",";
),{i,1,npoly+1}];
qliststr="";
If[nq>0,
Do[(
qliststr=qliststr<>"q"<>ToString[i]<>",";
),{i,1,nq}];
];
mliststr="";
Do[(
mliststr=mliststr<>"m"<>ToString[i-1]<>",";
),{i,1,nmono+1}];
If[diagnose,Print["{nmono,npoly,nq} = ",{nmono,npoly,nq}];];

fouttxt="";
Print["Starting loop over mpqtxt/mpqtxttab"];
Monitor[Do[(
	jdyn=jval; (* global *)
(*Print["jval = ",jval];*)
fouttxt=fouttxt<>GetAdjoint[jval,pliststr,mliststr,
	qliststr,mpqtxt,mpqtxttab,natoms,npoly,nq,nmono,nvar];
	(*Print[fouttxt];*)
),{jval,Length[mpqtxt],1,-1}],
	ProgressIndicator[jdyn,{Length[mpqtxt],1}]];
Print["Completed loop over mpqtxt/mpqtxttab"];
If[diagnose,PLHfouttxt1=fouttxt;];
(* accumulate contributions from x's *)
(* make mforx; mforx[[i]] gives index of m where m[[index]\[Equal]x[[i]] *)
txt1047=Import[fortranname,"Text"];
Export[DataDir<>"DeleteMe.txt",txt1047,"Text"];
txttab=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
istart=Position[txttab,"m(0)"][[1,1]];
(*Do[(
	If[!StringContainsQ[txttab[[i,3]],"x("],
		iend=i-1;
	Goto[continueon];
	];
),{i,istart+1,istart+2*nvar}];
Print["iend could not be found properly; Aborting"];Abort[];
Label[continueon];*)

If[diagnose, 
	PLHtxttab=txttab;
	PLHistart=istart;
	];
(*iend=istart+nvar;*)
Do[(
If[ txttab[[i]]=={},
iendplus=i;
Break[];];
),{i,istart,istart+nmono+5}];
iend=iendplus-1;
txttabred=Take[txttab,{istart+1,iend}];
If[diagnose,PLHtxttabred=txttabred;PLHiend=iend;];
Print["Adding contributions from x's"];
nm=Length[txttabred];
(*Do[(ToExpression[txtlns[[j]]]),{j,1,nm}];
Do[(ToExpression["x"<>ToString[i]<>"="<>ToString[i]]),{i,1,nm}];*)
(*Print["{x1,x2,x3} = ",{x1,x2,x3}];*)
mforx={};
Do[(
	idyn=i;
Do[(
	jdyn=j;
	If[Quiet[ToExpression[StringDrop[StringDrop[txttabred[[j,3]],2],-1]]]==i,
		mforx=Append[mforx,j];
		Goto[nexti];
		,
		Goto[nextj];
	];
	Label[nextj];
	),{j,1,nm}];
	mforx=Append[mforx,-1];
	Label[nexti];
),{i,1,natoms (natoms+1)/2}];
xform={};
Do[(
If[IntegerQ[Quiet[ToExpression[StringDrop[StringDrop[txttabred[[i,3]],2],-1]]]],
	xform=Append[xform,ToExpression[StringDrop[StringDrop[txttabred[[i,3]],2],-1]]];
	,
	xform=Append[xform,-1];
];	
),{i,1,mnumber}];
kx=0;
Do[(
If[mforx[[i]]!=-1,kx=kx+1;]; 
),{i,1,Length[mforx]}];

If[diagnose,PLHmforx=mforx;PLHxform=xform;];
Print["number of x's in use = ",kx];
Print["mforx = ",mforx];
Print["xform (first 2 nvar values) = ",Take[xform,2 nvar]];

(*rcombos=Flatten[Table[{i,j},{i,1,natoms-1},{j,i+1,natoms}],1];
Print["rcombos = ",rcombos];
Print["xxp(iflag) is the derivative of the energy for iflag=1 to 3N"];*)

(* add x derivatives *)
Print["adding x derivatives, depending on xtransform values"];
(* first figure out which {iatom,jatom} pairs are used, because not all are used if
the inputfname file came from fragmentation *)
{xnumber,mnumber1,pnumber1,qnumber1}=Getxmpq[inputfname];
infile=Import[inputfname,"Text"];
infilelines=textconvert[infile,"text","textlines",DataDir];
pairsinuse={};
Do[(
If[!StringContainsQ[infilelines[[i]],"!"],Goto[skipi];];
numbers1=StringDrop[StringTake[infilelines[[i]],-4],-2];
numbers2=StringTake[infilelines[[i]],-2];
pairsinuse=Append[pairsinuse,{ToExpression[numbers1],ToExpression[numbers2]}];
Label[skipi]
),{i,1,xnumber}];
Print["There are ",xnumber," pairs in use. In order of x, they are:"];
Print[pairsinuse];


Do[(
iatom=IntegerPart[(k-.0000000000001)/3]+1;(* this cycles 1,1,1,2,2,2,3,3,3,... *)
fouttxt=fouttxt<>"xxp"<>ToString[k]<>" = ";
Do[(
If[jatom==iatom,Goto[skipj];];
If[!MemberQ[pairsinuse,Sort[{iatom,jatom}]],Goto[skipj];];
icart=Mod[k,3,1]; (* this cycles 1,2,3,1,2,3... *)
ivar=Flatten[Position[pairsinuse,Sort[{iatom,jatom}]],1][[1]]; (* gives the variable corresponding
		to the current combination of iatom and jatom *)
(*Print["{k,iatom,jatom,icart,ivar} = ",{k,iatom,jatom,icart,ivar}];*)
Which[
	xtransform[[ivar]]==1,txttr="*(-m"<>ToString[mforx[[ivar]]]<>"/a)";,
	xtransform[[ivar]]==2,txttr="*(-m"<>ToString[mforx[[ivar]]]<>"**2)";
];
(*Print["{i,j,k} = ",{i,j,k}];*)
If[mforx[[ivar]]==-1,Goto[skipj];];
If[!MemberQ[xform,ivar],Goto[skipj]]; (* added 10/29/2022 *)
(*If[diagnose,Print["+ mp"<>ToString[mforx[[ivar]]]<>
		txttr<>"*drdx("<>ToString[k]<>","<>ToString[ivar]<>",xyz)"];];*)
fouttxt=fouttxt<>"+ mp"<>ToString[mforx[[ivar]]]<>
		txttr<>"*drdx("<>ToString[k]<>","<>ToString[ivar]<>",xyz)";
Label[skipj];
),{jatom,1,natoms}];
fouttxt=fouttxt<>"\n";
),{k,1,3*natoms}];
If[diagnose,PLHfouttxt2=fouttxt;];
{fouttxt,natoms,npoly,nmono,nq}
];







GetAdjoint[jval_,pliststr_,mliststr_,qliststr_,mpqtxt_,mpqtxttab_,
	natoms_,npoly_,nq_,nmono_,nvar_]:=Module[
	{fouttxtadd,jend,skipm,return,ltri,ltrj,dolist,eqpos,numi,numj,
	lookfor,lasti}, (* local variables *)
(* 
jval is the index of mpqtxt or mpqtttab for which you are trying to find the adjoint.

pliststr, mliststr, and qliststr are text strings of the list of p's, m's, and q's 
if present. They are created in the calling function by these commands:
(*Print[ptxt];*)
pliststr="";
Do[(
pliststr=pliststr<>"p"<>ToString[i-1]<>",";
),{i,1,npoly+1}];
qliststr="";
If[nq>0,
Do[(
qliststr=qliststr<>"q"<>ToString[i]<>",";
),{i,1,nq}];
];
mliststr="";
Do[(
mliststr=mliststr<>"m"<>ToString[i-1]<>",";
),{i,1,nmono+1}];
 
mpqtxt is the Mathematica code for all m's, p's and q's (if present); its format
looks something like this: 
{"m0=1.0D0","m1=x3;","m2=x2;","m3=x1;","m4=m1*m2;","p0=m0; ","p1=m1+m2;","p2=m3;",
"p3=m4;","p4=p2*p1;","p5=p1*p1-p3-p3;","p6=p2*p2;","p7=p2*p3;","p8=p3*p1;",
"p9=p2*p5;","p10=p2*p4;","p11=p1*p5-p8;","p12=p2*p6;"}

mpqtxttab is the Fortran code for m's, p's and q's in a Table form of text entries, eg.
each term is like {"m(99)","=","m(8)","*","m(24)"}.
  
natoms, npoly, nq, nmono, and nvar are, respectively the numbers of atoms (in the
parent), the numbers of p polynomials, the number of q polynomials, the number of 
monomials, and the number of variables (x).
 
fouttxtadd is be the Fortran code output, in text format. The adjoints have the 
format of pp(i), qp(j), mp(k), xxp(n) for the p's, q's, m's, and x's, respectively.  
*)

(* get starting part of fouttxtadd, i.e., add c(i) if
the conjugate variable of the adjoint is a p *)
ltrj=StringTake[mpqtxt[[jval]],1];
eqpos=StringPosition[mpqtxt[[jval]],"="][[1,1]];
numj=ToExpression[StringDrop[StringDrop[mpqtxt[[jval]],
	{eqpos,StringLength[mpqtxt[[jval]]]}],1]];
fouttxtadd=ltrj<>"p"<>ToString[numj]<>" = ";
If[ltrj=="p",fouttxtadd=fouttxtadd<>"+ c"<>ToString[numj];];

(* accumulate indicies of all mpqtxttab elements that have 
ltrj<>"("<>ToString[jval]<>")" on the rhs. These are the ones whose partial
derivatives will need to be evaluatied *)
dolist={};
lookfor=ltrj<>"("<>ToString[numj]<>")";
lasti=Position[mpqtxttab[[All,1]],lookfor][[1,1]];
Do[(
	If[MemberQ[Drop[mpqtxttab[[i]],2],lookfor],
		dolist=Append[dolist,i];
	];
),{i,Length[mpqtxttab],lasti,-1}];
(*Print["dolist = ",dolist];Pause[.1];*)
If[dolist=={}, 
	If[StringTake[fouttxtadd,-2]=="= ",fouttxtadd="";];
	Goto[return];
];

(* perform doloop over the members of dolist*)
Do[(
idyn=i; (* global  for dynamic display of progress*)
Quiet[
ToExpression["Clear["<>pliststr<>"];"]; (* these much be cleared  *)
ToExpression["Clear["<>mliststr<>"];"]; (* so that derivatives will be *)
If[nq!=0,ToExpression["Clear["<>qliststr<>"];"];]; (* symbolic  *)
];
ltri=StringTake[mpqtxt[[i]],1];
eqpos=StringPosition[mpqtxt[[i]],"="][[1,1]];
numi=ToExpression[StringDrop[StringDrop[mpqtxt[[i]],
	{eqpos,StringLength[mpqtxt[[i]]]}],1]];
ToExpression[mpqtxt[[i]]];
(* this statement adds the new text in FortranForm after
solving symbolically for the derivative: *)
fouttxtadd=fouttxtadd<>" + "<>ltri<>"p"<>ToString[numi]<>"*"<>
		ToString[FortranForm[D[ToExpression[ltri<>ToString[numi]],
		ToExpression[ltrj<>ToString[numj]]]]];
),{i,dolist}];  (* end of do loop over dolist *)
(*if no rhs, then no new text, thus: *)
If[StringTake[fouttxtadd,-2]=="= ",fouttxtadd="";]; 

(* finish up *)
Label[return];
If[fouttxtadd!="",fouttxtadd=fouttxtadd<>"\n";];
fouttxtadd (* output *)
];
	


MakeBackwardsFortRoutine[mnumber_,pnumber_,qnumber_]:=Module[
{skipit,len,tout,touttab,touttabstar,text1,text1a,text2,z31047,nvar,natoms,npoly,nmono,nq,
qlines,diagnose,positions,kstart,kend,xnumber,mnumber1,pnumber1,qnumber1,
text3,toutx
},

diagnose=True;
natoms=natomsparent;
nmono=mnumber;
nq=qnumber;
npoly=pnumber;
nvar=natoms (natoms-1)/2;
{xnumber,mnumber1,pnumber1,qnumber1}=Getxmpq[inputfname];
If[xnumber<nvar,
	Print["It appears that inputfname is for a fragmented basis set."];
	Print["  Proceeding on that assumption."];
	nvar=xnumber;
	Print["Current {nvar,mnumber,pnumber,qnumber} =",{nvar,mnumber,pnumber,qnumber}];
	];
If[Or[mnumber!=mnumber1,pnumber!=pnumber1,qnumber!qnumber1],
	Print["Difficulty with definitions of mnumber,pnumber, or qnumber."];
	Print["Aborting"];
	Abort[];
	];

{tout,natoms,npoly,nmono,nq}=CreateAdjointText[mnumber,pnumber,qnumber];
(*nvar=natoms (natoms-1)/2; (* this is not true for fragmented basis, see above*)*)
Print["{nvar,npoly,nmono,nq} = ",{nvar,npoly,nmono,nq}];
Print["making reverse fortran routine"];
tout=textconvert[tout,"text","textstandard",DataDir];
If[diagnose,PLHtouttab0=touttab;];

(*
(* this seems not to work perfectly, cant do the Drop *)
(* make all numbers except exponents into strings *)
touttabstar=textconvert[tout,"text","tablestar",DataDir];
Print["   make all numbers into strings"];
Do[(
len=Length[touttabstar[[i]]];
Do[(
If[NumberQ[touttabstar[[i,j]]],
	If[touttabstar[[i,j-2]]\[Equal]"*" &&
		touttabstar[[i,j-1]]\[Equal]"-",
	touttabstar[[i,j]]="(-"<>ToString[touttabstar[[i,j]]]<>".d0)";
	touttabstar[[i]]=Drop[touttabstar[[i]],{j-1}];
	,
	touttabstar[[i,j]]="("<>ToString[touttabstar[[i,j]]]<>".d0)";
	];
];
),{j,3,len}];
),{i,1,Length[touttabstar]}];
tout=textconvert[touttabstar,"tablestar","textstandard",DataDir]
*)
(* try this instead *)
tout=StringReplace[tout,"* - 10"->"*(-10.d0)"];
tout=StringReplace[tout,"* - 11"->"*(-11.d0)"];
tout=StringReplace[tout,"* - 12"->"*(-12.d0)"];
tout=StringReplace[tout,"* - 13"->"*(-13.d0)"];
tout=StringReplace[tout,"* - 14"->"*(-14.d0)"];
tout=StringReplace[tout,"* - 15"->"*(-15.d0)"];
tout=StringReplace[tout,"* - 16"->"*(-16.d0)"];
tout=StringReplace[tout,"* - 17"->"*(-17.d0)"];
tout=StringReplace[tout,"* - 18"->"*(-18.d0)"];
tout=StringReplace[tout,"* - 1"->"*(-1.d0)"];
tout=StringReplace[tout,"* - 2"->"*(-2.d0)"];
tout=StringReplace[tout,"* - 3"->"*(-3.d0)"];
tout=StringReplace[tout,"* - 4"->"*(-4.d0)"];
tout=StringReplace[tout,"* - 5"->"*(-5.d0)"];
tout=StringReplace[tout,"* - 6"->"*(-6.d0)"];
tout=StringReplace[tout,"* - 7"->"*(-7.d0)"];
tout=StringReplace[tout,"* - 8"->"*(-8.d0)"];
tout=StringReplace[tout,"* - 9"->"*(-9.d0)"];

If[diagnose,PLHtout1a=tout;];

(* make a regular table form of tout *)
Export[DataDir<>"DeleteMe.txt",tout,"Text"];
touttab=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[diagnose,PLHtouttab1=touttab;];


(* multiply terms in between + signs that do not have * *)
(* these are terms that span lines, 
like {...  "+","mp7120*",3},{"m6","m8","m9","+","mp7119*",3}, *)
(*first, join elements of touttab that do not have a = to the next lowest one 
that does *)
Print["   multiply terms in between + signs that do not have *"];
Do[(
If[!MemberQ[touttab[[i+1]],"="],
	touttab[[i]]=Join[touttab[[i]],touttab[[i+1]]];
	touttab=Drop[touttab,{i+1}];
];
),{i,Length[touttab]-1,1,-1}]; (* go backwards from end *)
If[diagnose,PLHtouttab2=touttab;];

(*
(* try it without this but with FortranForm - This seemed to work, but I'll 
leave this here in case ...*)
(* next, note that all terms have leading + signs.  Look at terms between + signs and
make sure that everything between them is multiplied together (some * signs may 
be missing *)
Do[( (* over i elemgent of touttab *)
len=Length[touttab[[i]]];
(* multiply terms in between + signs that do not have * *)
(* these are terms 
like {...  "+","mp7120*",3,"m6","m8","m9","+","mp7119*",3}, 
NB the numbers like 3 in the above example are numbers not quoted numbers, but 
have been turned into strings above *)
positions=Flatten[Position[touttab[[i]],"+"]];
If[Length[positions]\[GreaterEqual]1,
	If[!MemberQ[positions,3],Print["First + sign is not in position 3, i = ",i];Abort[];];
	Do[( (* over j members of positions list *)
		kstart=positions[[j]]+1;
		If[j==Length[positions]-1,
			kend=positions[[Length[positions]]]-1;
			,
			kend=positions[[j+1]]-1;
		];
		Do[( (* over k elements of touttab[[i]] *)
			If[!StringContainsQ[touttab[[i,k]],"*"],
				touttab[[i,k]]=touttab[[i,k]]<>"*";
			];
		),{k,kstart,kend-1}]; (* work from end backwards *)
	),{j,Length[positions]-1,1,-1}]; (* work from end backwards *)
];
),{i,1,Length[touttab]}];
If[diagnose,PLHtouttab3=touttab;];
*)

(*
(* this works but screws up the things just above ! *)
(* next take care of things like: pp6*2 p2 *)
Print["   next take care of things like: pp6*2 p2"];
Do[(
len=Length[touttab[[i]]];
Do[(
If[StringContainsQ[touttab[[i,j]],"*"] &&
	!StringContainsQ[touttab[[i,j+1]],"+"] &&
	StringTake[touttab[[i,j+1]],2]!="mp",
	touttab[[i,j+1]]="*"<>touttab[[i,j+1]];
];
),{j,1,len-1}];
),{i,1,Length[touttab]}];
*)

(* get rid of leading + signs *)
Print["   get rid of leading + signs"];
Do[(
	len=Length[touttab[[i]]];
	Do[(
	If[touttab[[i,j]]=="=" && touttab[[i,j+1]]=="+",
		touttab[[i]]=Drop[touttab[[i]],{j+1}];
	];
	),{j,1,len-1}];
),{i,1,Length[touttab]}];
If[diagnose,PLHtouttab4=touttab;];

(* convert back to text form *)
Print["   convert back to text form"];
tout=textconvert[touttab,"table","textstandard",DataDir];
(*Export[DataDir<>"DeleteMe.txt",touttab,"Table"];
tout=Import[DataDir<>"DeleteMe.txt","Text"];
DeleteFile[DataDir<>"DeleteMe.txt"];*)
(*tout=StringDelete[tout,"\t"];
tout=StringReplace[tout,"="->" = "];
tout=StringReplace[tout,"+"->" + "];*)
If[diagnose,PLHtout2=tout;];

(* add parentheses *)
Print["   add parentheses"];
Print["      to pp and c"];
Monitor[Do[(
idyn=i;
tout=StringReplace[tout,"pp"<>ToString[i]->"pp("<>ToString[i]<>")"];
tout=StringReplace[tout,"c"<>ToString[i]->"c("<>ToString[i]<>")"];
),{i,npoly,0,-1}],ProgressIndicator[{i,{npoly,0}}]];
If[diagnose,PLHtout3=tout;];
Print["      to qp and q"];
If[nq>0,
	Monitor[Do[(
	idyn=i;
	tout=StringReplace[tout,"qp"<>ToString[i]->"qp("<>ToString[i]<>")"];
	tout=StringReplace[tout,"q"<>ToString[i]->"q("<>ToString[i]<>")"];
	),{i,nq,0,-1}],ProgressIndicator[{i,{nq,0}}]];
];
If[diagnose,PLHtout4=tout;];
Print["      to mp and m"];
Monitor[Do[(
idyn=i;
tout=StringReplace[tout,"mp"<>ToString[i]->"mp("<>ToString[i]<>")"];
tout=StringReplace[tout,"m"<>ToString[i]->"m("<>ToString[i]<>")"];
),{i,nmono,0,-1}],ProgressIndicator[{i,{nmono,0}}]];
Print["      to xxp"];
Monitor[Do[(
idyn=i;
tout=StringReplace[tout,"xp"<>ToString[i]->"xp("<>ToString[i]<>")"];
),{i,3*natomsparent,1,-1}],ProgressIndicator[{i,{3*natomsparent,1}}]];
(* NB you have to do this last *)
Print["      to p"];
Monitor[Do[(
idyn=i;
tout=StringReplace[tout,"p"<>ToString[i]->"p("<>ToString[i]<>")"];
),{i,npoly,0,-1}],ProgressIndicator[i,{npoly,0}]];
If[diagnose,PLHtout5=tout;];

(* convert to table form *)
Print["   convert to table form"];
Export[DataDir<>"DeleteMe.txt",tout,"Text"];
touttab=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];

(* get rid of *1, add leading spaces  *)
Print["   get rid of *1, add leading spaces"];
Do[(
len=Length[touttab[[i]]];
touttab[[i,1]]="    "<>touttab[[i,1]];
Do[(
If[touttab[[i,j]]=="=" || touttab[[i,j]]=="+",Goto[skipit];];
If[StringLength[touttab[[i,j]]]>=2,
If[StringTake[touttab[[i,j]],-2]=="*1",
touttab[[i,j]]=StringDrop[touttab[[i,j]],-2];
];
];
Label[skipit];
),{j,1,len}];
),{i,1,Length[touttab]}];
If[diagnose,PLHtouttab6=touttab;];

(* turn back to text format *)
Print["   turn back to text format"];
tout=textconvert[touttab,"table","textstandard",DataDir];
(*Export[DataDir<>"DeleteMe.txt",touttab,"Table"];
tout=Import[DataDir<>"DeleteMe.txt","Text"];
DeleteFile[DataDir<>"DeleteMe.txt"];*)
tout=StringDelete[tout,"\t"];
tout=StringReplace[tout,"="->" = "];
tout=StringReplace[tout,"+"->" + "];
tout=StringReplace[tout,"^"->"**"];
tout=StringReplace[tout,"*-1"->"*(-1.d0)"];
tout=StringReplace[tout,"*-2"->"*(-2.d0)"];
tout=StringReplace[tout,"*-3"->"*(-3.d0)"];
tout=StringReplace[tout,"*-4"->"*(-4.d0)"];
If[diagnose,PLHtout7=tout;];

(* deal with & and breaking up sum for the adjoints at this point *)
(* Add ampersands as needed *)
Export[DataDir<>"DeleteMe.txt",tout,"Text"];
qlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
tout=AmpersandPlusOnly[qlines];
If[diagnose,PLHtout8=tout;];

(* now add the subroutine items *)
Print["   add the subroutine items"];
text1="
  subroutine derivative_reverse(c,m,p,xyz,r,xxp)
    real(wp)::c(0:npoly1047),m(0:nmono1047),p(0:npoly1047)
    real(wp)::xyz(natoms1047,3),r(natoms1047,natoms1047)
    !::::::::::::::::::::
    real(wp)::pp(0:npoly1047),mp(0:nmono1047),xxp(1:xxxxx)
";
text1a="    real(wp)::qp(1:nq1047)

    qp(:)=0.d0
    pp(:)=0.d0
    mp(:)=0.d0
    xxp(:)=0.d0
";
text2="
    return
  end subroutine derivative_reverse
";

text3="

  subroutine hessianrev(coeff,xyz,H)
!	calculates hessian using reverse derivative routine
    real(wp),dimension(natoms1047,3),intent(in)::xyz
    real(wp),dimension(xxxxx,xxxxx),intent(inout)::H
    real(wp)::x(nvar1047)
    real(wp)::coeff(0:npoly1047),m(0:nmono1047),p(0:npoly1047)
    real(wp)::eps
    real(wp),dimension(1:xxxxx,1:xxxxx)::gd1,gd2
    real(wp),dimension(1:natoms1047,1:3)::xyzp,xyzm
    integer::i,j,xyzind,matom

    eps=0.005d0
    H=0.d0

    do i=1,xxxxx
      xyzind=Mod(i-1,3)+1
      matom=INT((dble(i)-0.00001d0)/3)+1
      xyzp(:,:)=xyz(:,:)
      xyzp(matom,xyzind)=xyz(matom,xyzind)+eps
	call get_x (xyzp,x)
   	call evmono (x,m)
   	call evpoly (m,p)
      call derivative_reverse(coeff,m,p,xyzp,r,gd1(:,i))
      xyzm(:,:)=xyz(:,:)
      xyzm(matom,xyzind)=xyz(matom,xyzind)-eps
	call get_x (xyzm,x)
   	call evmono (x,m)
   	call evpoly (m,p)
      call derivative_reverse(coeff,m,p,xyzm,r,gd2(:,i))
    end do


    do i=1,xxxxx
      do j=i+1,size(xyz)
        H(i,j)=(gd1(i,j)-gd2(i,j))/(4.0*eps)+(gd1(j,i)-gd2(j,i))/(4.0*eps)
        H(j,i)=H(i,j)
      end do
    end do

    do i=1,size(xyz)
       H(i,i)=(gd1(i,i)-gd2(i,i))/(2.0*eps)
    end do
    return
  end subroutine hessianrev

";

(* Fix dimensions in each text segment where they are needed*)
Print["   fix dimensions in the relevant text segments"];
toutx=text1;
z31047=3*natoms;
toutx=StringReplace[toutx,"nmono1047"->ToString[nmono]];
toutx=StringReplace[toutx,"npoly1047"->ToString[npoly]];
toutx=StringReplace[toutx,"natoms1047"->ToString[natoms]];
toutx=StringReplace[toutx,"xxxxx"->ToString[z31047]];
toutx=StringReplace[toutx,"nq1047"->ToString[nq]];
toutx=StringReplace[toutx,"nvar1047"->ToString[nvar]];
text1=toutx;
toutx=text1a;
z31047=3*natoms;
toutx=StringReplace[toutx,"nmono1047"->ToString[nmono]];
toutx=StringReplace[toutx,"npoly1047"->ToString[npoly]];
toutx=StringReplace[toutx,"natoms1047"->ToString[natoms]];
toutx=StringReplace[toutx,"xxxxx"->ToString[z31047]];
toutx=StringReplace[toutx,"nq1047"->ToString[nq]];
toutx=StringReplace[toutx,"nvar1047"->ToString[nvar]];
text1a=toutx;
toutx=text3;
z31047=3*natoms;
toutx=StringReplace[toutx,"nmono1047"->ToString[nmono]];
toutx=StringReplace[toutx,"npoly1047"->ToString[npoly]];
toutx=StringReplace[toutx,"natoms1047"->ToString[natoms]];
toutx=StringReplace[toutx,"xxxxx"->ToString[z31047]];
toutx=StringReplace[toutx,"nq1047"->ToString[nq]];
toutx=StringReplace[toutx,"nvar1047"->ToString[nvar]];
text3=toutx;

(* combine subroutine pieces with the adjoints, currently in tout *)
If[nq>0,
	tout=text1<>text1a<>tout<>text2<>text3;
	,
	tout=text1<>tout<>text2<>text3;
];

(*Export[OutputSubroutineFilename,tout,"Text"];*)
Print["Done"];
If[diagnose,PLHtout10=tout;];

tout
];


DetermineUnusedqm[]:=Module[
{tin,natoms,nmono,nq,npoly,nvar,tintab,
mdroplist,qdroplist,mkeeplist,qkeeplist},
tin=Import[OutputSubroutineFilename,"Text"];
natoms=natomsparent;
nmono=mnumber;
nq=qnumber;
npoly=pnumber;
nvar=natoms (natoms-1)/2;

qdroplist={};
qkeeplist={};
Do[(
If[!StringContainsQ[tin,"qp("<>ToString[i]<>") ="],
	qdroplist=Append[qdroplist,i];
	,
	qkeeplist=Append[qkeeplist,i];
];
),{i,1,nq}];
(*Print["qdroplist = ",qdroplist];*)

mdroplist={};
mkeeplist={};
Do[(
If[!StringContainsQ[tin,"mp("<>ToString[i]<>") ="],
	mdroplist=Append[mdroplist,i];
	,
	mkeeplist=Append[mkeeplist,i];
];
),{i,1,nmono}];
(*Print["mdroplist = ",mdroplist];*)
Print["Length of {qdroplist,mdroplist} =",
	{Length[qdroplist],Length[mdroplist]}];
Print["Length of {qkeeplist,mkeeplist} =",
	{Length[qkeeplist],Length[mkeeplist]}];
	Print["These keeps do not include those whose rhs evaluates to \n",
		"zero because they contain multiplication by those on droplists."];

{qdroplist,qkeeplist,mdroplist,mkeeplist}
];


makemforxform[nvar_]:=Module[
{txt1047,txttab,istart,iend,continueon,txttabred,diagnose,nm,
mforx,xform,nexti,kx},
diagnose=False;


(* make mforx; mforx[[i]] gives index of m where m[[index]\[Equal]x[[i]] *)
txt1047=Import[fortranname,"Text"];
Export[DataDir<>"DeleteMe.txt",txt1047,"Text"];
txttab=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
istart=Position[txttab,"m(0)"][[1,1]];
Do[(
	If[!StringContainsQ[txttab[[i,3]],"x("],
		iend=i-1;
	Goto[continueon];
	];
),{i,istart+1,istart+2*nvar}];
Print["iend could not be found properly; Aborting"];Abort[];
Label[continueon];
txttabred=Take[txttab,{istart+1,iend}];
If[diagnose,PLHtxttabred=txttabred];
Print["Making mforx and xform"];
nm=Length[txttabred];
(*Do[(ToExpression[txtlns[[j]]]),{j,1,nm}];
Do[(ToExpression["x"<>ToString[i]<>"="<>ToString[i]]),{i,1,nm}];*)
(*Print["{x1,x2,x3} = ",{x1,x2,x3}];*)
mforx={};
Do[(
Do[(
	If[ToExpression[StringDrop[StringDrop[txttabred[[j,3]],2],-1]]==i,
		mforx=Append[mforx,j];
		Goto[nexti];
	];
	),{j,1,nm}];
	mforx=Append[mforx,-1];
	Label[nexti];
),{i,1,nvar}];
xform={};
Do[(
xform=Append[xform,
	ToExpression[StringDrop[StringDrop[txttabred[[i,3]],2],-1]]];
),{i,1,nm}];
kx=0;
Do[(
If[mforx[[i]]!=-1,kx=kx+1;];
),{i,1,Length[mforx]}];
Print["number of x's in use = ",kx];
Print["mforx = ",mforx];
Print["xform (first ",2 nvar," entries) = ",Take[xform,2 nvar]]; (* changed to shorten 18 Feb 2023 *)

{mforx,xform}
];


Getxmpq::usage="
gets the max of x's, m's, p's, and (possibly) q's from the file
inputfname
Assumes DataDir is a global variable
";
Getxmpq[inputfname_]:=Module[
{doneq,doneq1,donem,donep,mnumber,pnumber,qnumber,xnumber,donex},
infile=Import[inputfname,"Text"];
infilelines=textconvert[infile,"text","textlines",DataDir];

(* find q if there *)
Do[(
	If[StringContainsQ[infilelines[[i]],"::q"],
	isave=i;
	Goto[doneq];
	];
),{i,1,Length[infilelines]}];
(* ::q not found *)
qnumber=0;
Goto[doneq1];
Label[doneq];
qnumber=ToExpression[StringDrop[StringDrop[infilelines[[isave]],-4],23]];
Label[doneq1];
(* find m if there *)
Do[(
	If[StringContainsQ[infilelines[[i]],"::m"],
	isave=i;
	Goto[donem];
	];
),{i,1,Length[infilelines]}];
(* ::m not found *)
Print["::m not found"];Abort[];
Label[donem];
mnumber=ToExpression[StringDrop[StringDrop[infilelines[[isave]],-4],25]];

(* find number of x *)
Do[(
	If[StringContainsQ[infilelines[[i]],"::x"],
	isave=i;
	Goto[donex];
	];
),{i,1,Length[infilelines]}];
(* ::x not found *)
Print["::x not found"];Abort[];
Label[donex];
xnumber=ToExpression[StringDrop[StringDrop[infilelines[[isave]],-4],25]];

(* find p if there *)
Do[(
	If[StringContainsQ[infilelines[[i]],"::p"],
	isave=i;
	Goto[donep];
	];
),{i,1,Length[infilelines]}];
(* ::p not found *)
Print["::p not found"];Abort[];
Label[donep];
pnumber=ToExpression[StringDrop[StringDrop[infilelines[[isave]],-4],25]];

{xnumber,mnumber,pnumber,qnumber}
];
