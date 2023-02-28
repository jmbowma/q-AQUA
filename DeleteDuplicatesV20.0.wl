(* ::Package:: *)

(* ::Title:: *)
(*(*Delete Duplicates Package*)*)


(* ::Code:: *)
(*(* Revision History*)*)
(*(**)*)
(*(*20.0    Version in use as of 11/6/2021*)*)
(*(*		11/16/2021: retrofit to allow for use of mixed variable transforms in *)*)
(*(*		    MakeFortranWithChenDerivatives*)*)
(*(*		12/1/2021:  modifications so that this would run with compacted files, *)*)
(*(*		particularly in GetDefinitionstoMathematicaFromFortranOutput[] and functions*)*)
(*(*		within it.*)*)
(*(*17-Feb-2022  added option of mixed or 1/r transformations (in addition*)*)
(*(*		to Morse variables)*)*)
(*(*1-Nov-2022  fixed a problem in MakeExportFortran that made nvariables too large*)*)
(*(*		for fragmented bases. Also in MakeExportFortranDDWithChenDerivatives *)*)
(*(*		put the "renumbering if needed" section in GenerateFortranCommentWithXAssignments*)*)
(*(*3-Nov-2022    Changed a line in DeleteAllFortranDefinitionsStartingd to have p(0) become 1.0D0*)*)
(*(*4-Nov-2022    Modified the do loops that assign r(i,j) in MakeExportFortranDDWithChenDerivatives*)*)
(*(*10 Feb 2023   Fixed a problem in SequentialBulidup and PairwiseBuildup that happens at high *)*)
(*(*        orders when msa output has x's on the rhs of m definitions beyoun the ones for the*)*)
(*(*        variables.*)*)
(*(**)*)
(*(**)*)


CreateEvalMono[StringList_]:=Module[
{Stringin,Stringout},
Stringin=StringList;
Stringin=StringReplace[StringReplace[Stringin," "->""],"\n"->"; "];
Stringin=StringDelete[Stringin,"d0"];
Stringin=StringDelete[Stringin,"D0"];
Stringin=StringReplace[Stringin,"**"->"^"]; (* added 18 Feb 2023 *)
(* note that zero-order term needs to be put back *)
Stringout="EvalMono[]:=Module[{}, a=2; m0=1.0; "<>Stringin<>"ToExpression[mlist]];"
];


CreateEvalAllMonoDerivs[StringList_]:=Module[
{Stringin,Stringout},
Stringin=StringList;
Stringin=StringDelete[Stringin,"d0"];
Stringin=StringDelete[Stringin,"D0"];
Stringin=StringReplace[Stringin,"**"->"^"]; (* added 18 Feb 2023 *)
(* note that zero-order term needs to be put back *)
Stringout="EvalAllMonoDerivs[flag_]:=Module[{}, a=2; dm0=0; "<>Stringin<>"ToExpression[dmlist]];"
];


CreateEvalPoly[StringList_]:=Module[
{Stringin,Stringout},
Stringin=StringList;
Stringin=StringReplace[StringReplace[Stringin," "->""],"\n"->"; "];
Stringin=StringReplace[Stringin,"d0"->""];
Stringin=StringReplace[Stringin,"D0"->""];
Stringin=StringReplace[Stringin,"**"->"^"];  (* added 19 Feb 2023 *)
p0=1.;

Stringout="EvalPoly[]:=Module[\n{},\n"<>Stringin<>"ToExpression[plist]];"
];


CreateEvalAllPolyDerivs[StringList_]:=Module[
{Stringin,Stringout},
Stringin=StringList;
Stringin=StringDelete[Stringin,"d0"];
Stringin=StringDelete[Stringin,"D0"];
(* note that zero-order term needs to be put back *)
Stringout="EvalAllPolyDerivs[]:=Module[{}, dp0=dm0; "<>Stringin<>"ToExpression[dplist]];"
];


bemsaToMathematica::useage="bemsaToMathematica[StringList_]
If you take the bemsa list of monomials or polynomials, enclose them
in quotes, and name them StringList_, then this will convert them to
Mathematica (with out the line feeds ), though still with quotes.
";
bemsaToMathematica[StringList_]:=Module[
{Anew,Aold},
Aold=StringList;
Anew=StringDelete[StringList," "];
Aold=Anew;
If[Or[StringContainsQ[Aold,"m(0)="],StringContainsQ[Aold,"m(0) ="]],
Anew=StringDelete[Aold,"m(0)=1.0D0\n"];
Aold=Anew;
Anew=StringDelete[Aold,"m(0)=1.0d0\n"];
Aold=Anew;
Anew=StringDelete[Aold,"m(0)=1.d0\n"];
Aold=Anew;
Anew=StringDelete[Aold,"m(0)=1.D0\n"];
Aold=Anew;
];
If[StringContainsQ[Aold,"p(0)="],
Anew=StringDelete[Aold,"p(0)=m(0)"];
Aold=Anew;
];
Anew=StringReplace[Aold," "->""];
Aold=Anew;
Anew=StringReplace[Aold," "->""];
Aold=Anew;
Anew=StringReplace[Aold," "->""];
Aold=Anew;
(* this was the major 'symmetry' problem,  there were tabs from result
of Getmonopolycharstrings that did not get taken out, and when these occurred following
line continuations in the Fortran, they remained and prevented correct interpretation *)
Anew=StringReplace[Aold,"	"->""]; (* the blank space here is a tab *)
Aold=Anew;
Anew=StringReplace[Aold,")="->"="];
Aold=Anew;
Anew=StringReplace[Aold,"("->""];
Aold=Anew;
Anew=StringReplace[Aold,")**"->"^"];
Aold=Anew;
Anew=StringReplace[Aold,")*"->"*"];
Aold=Anew;
Anew=StringReplace[Aold,")+"->"+"];
Aold=Anew;
Anew=StringReplace[Aold,")/"->"/"];
Aold=Anew;
Anew=StringReplace[Aold,")-"->"-"];
Aold=Anew;
Anew=StringReplace[Aold,")"->";"];
Aold=Anew;
Anew=StringReplace[Aold,"\n"->" "];
Aold=Anew;
Anew=StringReplace[Aold,"^2 "->"^2; "];
Aold=Anew;
Anew=StringReplace[Aold,"^3 "->"^3; "];
Aold=Anew;
Anew=StringReplace[Aold,"^4 "->"^4; "];
Aold=Anew;
Anew=StringReplace[Aold,"^5 "->"^5; "];
Aold=Anew;
Anew=StringReplace[Aold,"0.D0 "->"0.D0; "];
Aold=Anew;
Anew=StringReplace[Aold,"0.d0 "->"0.d0; "];
Aold=Anew;
Anew=StringReplace[Aold,"dp0 = 0."->"dp0=0.; "];
Aold=Anew;
Anew=StringReplace[Aold,"dm0 = 0. "->"dm0=0.; "];
Aold=Anew;
Anew=StringReplace[Aold,"m0=1.0 "->"m0=1.0; "];
Aold=Anew;
Anew=StringReplace[Aold,"m0=1. "->"m0=1.; "];
Aold=Anew;
Anew=StringReplace[Aold,"p0=0.0D0 "->"p0=0.0D0; "];
Aold=Anew;

Aold
];




AnalyzeFrags::usage="
Inputs: ifraga and ifragb are the numbers of the fragments to be compared

Outputs:
Rcommon is a list of the polynomials that frag b has in common with frag a based on the 
	numbering for frag a
Runique is a list of the polynomials that frag a has unique to itself based on the 
	numbering for frag a
Tabdupsab is a table of the duplicated variables in the form {{avar1,bvar1},...}, where avar1 is the variable 
	number of a that is equivalent to bvar2, the variable number of b
Mcommon is the list of monomials that frag b has in common with frag a based on the 
	numbering for frag a

Global inputs assumed: 
filename[[X]] : the directory where the bemsa file for fragment X can be found, where X is the fragment 
	corresponding to ifraga
filename[[Y]] : the directory where the bemsa file for fragment Y can be found, where Y is the fragment 
	corresponding to ifragb
natomsfrag[[X]] : the number of atoms in fragment X, where X is the fragment corresponding to ifraga
natomsfrag[[Y]] : the number of atoms in fragment Y, where Y is the fragment corresponding to ifragb
rijnames[[X]] : the list of bond names in fragment X, where X is the fragment corresponding to ifraga
rijnames[[y]] : the list of bond names in fragment y, where y is the fragment corresponding to ifragb

The program determines which coordinates are in common, reads from the besma file for frag a the 
	monomial list and the polynomial list, and uses these lists to define EvalMono[] and EvalPoly[] 
	(which are evaluation programs for the monomials and polynomials). It then assigns random numbers on the range
     {0.1,1} to the variables, resets those that in common to one another, and enumerates all the polynomials 
	that are in common.
";
AnalyzeFrags[ifraga_,ifragb_]:=Module[
{fnamea,fnameb,natomsa,natomsb,nvariablesa,nvariablesb,
rijnamesa,rijnamesb,dups,Tdupsab,monostarta,monoenda,
polystarta,polyenda,
bemsaa,isave,CharStringmonoa,CharStringpolya,
CharString11a,CharString2a,
CharStringAsgn,
EMDcommon,Rcommon,Lcommonpolys,EMDunique,Runique,Luniquepolys,
nmonoa,npolya,EMDmonocommon,Mcommon,Munique
},

fnamea=filename[[ifraga]];
natomsa=natomsfrag[[ifraga]];
rijnamesa=rijnames[[ifraga]];
fnameb=filename[[ifragb]];
natomsb=natomsfrag[[ifragb]];
rijnamesb=rijnames[[ifragb]];
nvariablesa=natomsa (natomsa-1)/2;
nvariablesb=natomsb (natomsb-1)/2;
(*Print["nvariablesa = ",nvariablesa];*)

(* Check nvariables and rijnames *)
If[
	Or[nvariablesa!=Length[rijnamesa],nvariablesb!=Length[rijnamesb]],
	Print["nvariables and Length[rijnames] are not consistent"];Abort[];
];

(* count common variables *)
{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesb];	
(* dups now gives number of common variables *)
(* Tdupsab is a table with entries {ia,ib} of matching names *)

{Mcommon,Munique}=GetCommonMonomialValues[ifraga,natomsa,rijnamesa,nvariablesa,
	ifragb,natomsb,rijnamesb,nvariablesb];
{Rcommon,Runique}=GetCommonPolynomialValues[ifraga,natomsa,rijnamesa,nvariablesa,
	ifragb,natomsb,rijnamesb,nvariablesb];
	
{Rcommon,Runique,Tdupsab,Mcommon}
];



GetCommonMonomialValues::usage="GetCommonMonomialValuesForDeletaion[]

";
GetCommonMonomialValues[ifraga_,natomsa_,rijnamesa_,nvariablesa_,
	ifragb_,natomsb_,rijnamesb_,nvariablesb_]:=Module[
{EMa,EMb,Ra,Rb,EMTa,EMTb,EMDa,EMDb,polystarta,polyenda,npolya,nmonoa,
polystartb,polyendb,npolyb,nmonob,Mcommon, dups, Tdupsab,Munique,
EMax,positions},

{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesb];

(* evaluate monomials of frag b and store randomly assigned x values in xb *)
{polystartb, polyendb, npolyb, nmonob}=GetAssignEVMonoPoly[ifragb,natomsb,rijnamesb];
dist=Table[0,{i,1,nvariablesb}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,nvariablesb}];
Assignx[];
EMb=IntegerPart[10^10*EvalMono[]];
xb=Assignx[];
(*Print["EMb = ",EMb];*)

(* assign random numbers to x, reset the common ones to the corresponding
xb value using Tdupsab, and then evaluate monomials *)
{polystarta, polyenda, npolya, nmonoa}=GetAssignEVMonoPoly[ifraga,natomsa,rijnamesa];
dist=Table[0,{i,1,nvariablesa}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,nvariablesa}];
{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesb];
(*Print[Tdupsab];*)
(* set common xvalues to one another *)
Assignx[];
Do[(
ToExpression["x"<>ToString[Tdupsab[[i,1]]]<>" = xb[["<>ToString[Tdupsab[[i,2]]]<>"]];"];
),{i,1,Length[Tdupsab]}];
EMa=IntegerPart[10^10*EvalMono[]];
EMax=Drop[EMa,1];  (* get rid of m(0) *)
EMbx=Drop[EMb,1];  (* get rid of m(0) *)
(*Print["EMa = ",EMa];*)
(*
xa=ToExpression[xlist];
Print["Intersection[xa,xb] = ",Intersection[xa,xb]];
Print["Intersection[EMa,EMb] = ",Intersection[EMa,EMb]];
*)

(*
(* compare monomials for a to those in b, recording which ones are in common
and which ones are unique *)
Mcommon={};
Munique={};
Do[(
	If[MemberQ[EMbx,EMax[[i]]],
		Mcommon=Append[Mcommon,i];
		,
		Munique=Append[Munique,i];
	];
),{i,1,Length[EMax]}];
(*Print["number of common monomials: ",Length[Mcommon]];
Print[Mcommon];*)
*)


Mcommon={};
Munique={};
Do[(
	positions=Flatten[Position[Abs[EMbx],Abs[EMax[[i]]]]]; (* added Abs functions 10/21/2020 *)
	(*Print[{i,Length[positions],positions}];*)
	Do[(
		If[Length[positions]>0,
			Mcommon=Append[Mcommon,{i,positions[[j]]}];
			,
			Munique=Append[Munique,{i,positions[[j]]}];
		];
	),{j,1,Length[positions]}];
),{i,1,Length[EMax]}];



(*Print["number of common monomials: ",Length[Mcommon]];*)
(*Print[Mcommon];*)


{Mcommon,Munique}
];





GetCommonMonomialValuesWithPrevious::usage="GetCommonMonomialValuesForDeletaion[]

";
GetCommonMonomialValuesWithPrevious[ifraga_,natomsa_,rijnamesa_,nvariablesa_,
xsum_,msum_,psum_,rijnamesnow_,CharStringmono_,CharStringpoly_]:=Module[
{EMa,EMb,Ra,Rb,EMTa,EMTb,EMDa,EMDb,polystarta,polyenda,npolya,nmonoa,
Mcommon, dups, Tdupsab,Munique,EMax,positions},

(* evaluate monomials of previous combination of fragments
	 and store randomly assigned x values in xb *)
GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,xsum}];
Assignx[];
EMb=IntegerPart[10^10*EvalMono[]];
xb=Assignx[];
(*EMbx=Drop[EMb,1];  (* get rid of m(0) *)*)
(*Print["EMb = ",EMb];*)
(* xb now has the assigned xvalues and EMb has the evaluated monomials from all previously
combined fragments *)

(* assign random numbers to x, reset the common ones to the corresponding
xb value using Tdupsab, and then evaluate monomials *)
{polystarta, polyenda, npolya, nmonoa}=GetAssignEVMonoPoly[ifraga,natomsa,rijnamesa];
dist=Table[0,{i,1,nvariablesa}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,nvariablesa}];
{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesnow];
(*Print[Tdupsab];*)
(* set common xvalues to one another *)
Assignx[];
Do[(
ToExpression["x"<>ToString[Tdupsab[[i,1]]]<>" = xb[["<>ToString[Tdupsab[[i,2]]]<>"]];"];
),{i,1,Length[Tdupsab]}];
EMa=IntegerPart[10^10*EvalMono[]];
EMax=Drop[EMa,1];  (* get rid of m(0) *)
(*Print["EMa = ",EMa];*)

(* compare monomials for a to those in b, recording which ones are in common
and which ones are unique *)
Mcommon={};
Munique={};
(* I'm not sure why this should be Emb rather than Embx, but this is what works! *)
Do[(
	positions=Flatten[Position[Abs[EMb],Abs[EMax[[i]]]]]; (* added Abs functinos 10/21/2020 *)
	(*Print[{i,Length[positions],positions}];*)
	Do[(
		If[Length[positions]>0,
			Mcommon=Append[Mcommon,{i,positions[[j]]}];
			,
			Munique=Append[Munique,{i,positions[[j]]}];
		];
	),{j,1,Length[positions]}];
),{i,1,Length[EMax]}];
(*Print["number of common monomials: ",Length[Mcommon]];*)
(*Print[Mcommon];*)


{Mcommon,Munique}
];




GetDuplicationTable[rijnamesa_,rijnamesb_]:=Module[
{Tdupsab,dups},
(* count common variables *)
dups=0;
Tdupsab={};
Do[(
Do[(
If[StringMatchQ[rijnamesa[[ia]],rijnamesb[[ib]]],
dups=dups+1;
Tdupsab=Append[Tdupsab,{ia,ib}];
];
),{ia,1,Length[rijnamesa]}];
),{ib,1,Length[rijnamesb]}];
If[Tdupsab!={},
	Tdupsab=Sort[Tdupsab,#1[[1]]<#2[[1]] &];
];	
(* dups now gives number of common variables *)
(* Tdupsab is a table with entries {ia,ib} of matching names *)

{dups,Tdupsab}
];


Getpolymonostartendpoints[ifraga_]:=Module[
{bemsaa,monostarta,monoenda,polystarta,polyenda,isave
},

(* get mono and poly strings for fragment a *)
If[ifraga==-1,
	bemsaa=bemsaTabc;  (* uses the Fortran output as input *)
	,
	bemsaa=ToExpression["bemsaTabc"<>ToString[ifraga]];
];
Do[(
	If[Length[bemsaa[[i]]]>0 && bemsaa[[i,1]]=="m(0)",isave=i;Break[];];
),{i,1,Length[bemsaa]}];
monostarta=isave;
Do[(
	If[Or[Length[bemsaa[[i]]]<2 , StringTake[bemsaa[[i,1]],2]!="m("(*<>ToString[i-monostarta]<>")"*)],
		isave=i;Break[];];
),{i,monostarta+1,Length[bemsaa]}];
monoenda=isave-1;
Do[(
	If[Length[bemsaa[[i]]]>0 && bemsaa[[i,1]]=="p(0)",isave=i;Break[];];
	),{i,1,Length[bemsaa]}];
polystarta=isave;
Do[(
	If[Or[Length[bemsaa[[i]]]<2 , (StringTake[bemsaa[[i,1]],2]!="p("
	 && StringTake[bemsaa[[i,1]],2]!="q(") ],  (* the && was added 12/1/2021 for compacted filed *)
		isave=i;Break[];];
),{i,polystarta+1,Length[bemsaa]}];
polyenda=isave-1;

{monostarta,monoenda,polystarta,polyenda}
];



GetCommonPolynomialValues::usage="GetCommonPolynomialValues[]

";
GetCommonPolynomialValues[ifraga_,natomsa_,rijnamesa_,nvariablesa_,
	ifragb_,natomsb_,rijnamesb_,nvariablesb_]:=Module[
{EMa,EMb,Ra,Rb,EMTa,EMTb,EMDa,EMDb,polystarta,polyenda,npolya,nmonoa,
polystartb,polyendb,npolyb,nmonob,Rcommon, dups, Tdupsab,Runique,
EMax,positions},

{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesb];

(* evaluate polynomials of frag b and store randomly assigned x values in xb *)
{polystartb, polyendb, npolyb, nmonob}=GetAssignEVMonoPoly[ifragb,natomsb,rijnamesb];
dist=Table[0,{i,1,nvariablesb}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,nvariablesb}];
Assignx[];
EvalMono[];
EMb=IntegerPart[10^10*EvalPoly[]];
xb=Assignx[];

(* assign random numbers to x, reset the common ones to the corresponding
xb value using Tdupsab, and then evaluate polynomials *)
{polystarta, polyenda, npolya, nmonoa}=GetAssignEVMonoPoly[ifraga,natomsa,rijnamesa];
dist=Table[0,{i,1,nvariablesa}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,nvariablesa}];
{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesb];
(* set common xvalues to one another *)
Assignx[];
Do[(
ToExpression["x"<>ToString[Tdupsab[[i,1]]]<>" = xb[["<>ToString[Tdupsab[[i,2]]]<>"]];"];
),{i,1,Length[Tdupsab]}];
EvalMono[];
EMa=IntegerPart[10^10*EvalPoly[]];
EMax=Drop[EMa,1];  (* get rid of p(0) *)
EMbx=Drop[EMb,1];  (* get rid of p(0) *)
(*PLHEMax=EMax;*)

(*
(* compare polynomials for a to those in b, recording which ones are in common
and which ones are unique *)
Rcommon={};
Runique={};
Do[(
	If[MemberQ[EMbx,EMax[[i]]],
		Rcommon=Append[Rcommon,i];
		,
		Runique=Append[Runique,i];
	];
),{i,1,Length[EMax]}];
*)



Rcommon={};
Runique={};
(*PLHEMbx=EMbx;*)
Do[(
	positions=Flatten[Position[Abs[EMbx],Abs[EMax[[i]]]]]; (* added Abs functions 10/21/2020 *)
(*Print[positions,Length[positions]];*)
	Do[(
		If[Length[positions]>0,
			Rcommon=Append[Rcommon,{i,positions[[j]]}];
			,
			Runique=Append[Runique,{i,positions[[j]]}];
		];
	),{j,1,Length[positions]}];
),{i,1,Length[EMax]}];



(*Print[Length[Rcommon]];
Print[Rcommon];*)


{Rcommon,Runique}
];





GetCommonPolynomialValuesWithPrevious::usage="GetCommonPolynomialValuesForDeletaion[]

";
GetCommonPolynomialValuesWithPrevious[ifraga_,natomsa_,rijnamesa_,nvariablesa_,
xsum_,msum_,psum_,rijnamesnow_,CharStringmono_,CharStringpoly_]:=Module[
{EMa,EMb,Ra,Rb,EMTa,EMTb,EMDa,EMDb,polystarta,polyenda,npolya,nmonoa,
Rcommon, dups, Tdupsab,Runique,EMbx,EMax,positions},
(*
Print["Entered GetCommonPolynomialValuesWithPrevious"];
Print["{xsum,msum,psum} = ",{xsum,msum,psum}];
*)
(* evaluate polynomials of frag b and store randomly assigned x values in xb *)
(* ?? this step does not have to be done because it was done in GetCommonMonomialValuesWithPrevious *)
GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,xsum}];
(*Print["dist = ",dist];*)
Assignx[];
EvalMono[];
EMb=IntegerPart[10^10*EvalPoly[]];
(*EMbx=Drop[EMb,1];  (* get rid of p(0) *)*)
xb=Assignx[];
(*Print["xb = ",xb];*)
(* xb now has the assigned xvalues and EMb has the evaluated polynomials from all previously
combined fragments *)

(* assign random numbers to x, reset the common ones to the corresponding
xb value using Tdupsab, and then evaluate polynomials *)
{polystarta, polyenda, npolya, nmonoa}=GetAssignEVMonoPoly[ifraga,natomsa,rijnamesa];
dist=Table[0,{i,1,nvariablesa}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,nvariablesa}];
(*Print["rijnamesa = ",rijnamesa];
Print["rijnamesnow = ",rijnamesnow];*)
{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesnow];
(* set common xvalues to one another *)
Assignx[];
(*Print["xlist = ",xlist];
Print["Tdupsab = ",Tdupsab];*)
Do[(
ToExpression["x"<>ToString[Tdupsab[[i,1]]]<>" = xb[["<>ToString[Tdupsab[[i,2]]]<>"]];"];
),{i,1,Length[Tdupsab]}];
EvalMono[];
EMa=IntegerPart[10^10*EvalPoly[]];
EMax=Drop[EMa,1];  (* get rid of p(0) *)

Rcommon={};
Runique={};
(* I'm not sure why this should be Emb rather than Embx, but this is what works! *)
Do[(
	positions=Flatten[Position[Abs[EMb],Abs[EMax[[i]]]]]; (* added Abs functions 10/21/2020 *)
(*Print[positions,Length[positions]];*)
	Do[(
		If[Length[positions]>0,
			Rcommon=Append[Rcommon,{i,positions[[j]]}];
			,
			Runique=Append[Runique,{i,positions[[j]]}];
		];
	),{j,1,Length[positions]}];
),{i,1,Length[EMax]}];
(*Print[Length[Rcommon]];
Print[Rcommon];*)


{Rcommon,Runique}
];




Getmonopolycharstrings[ifraga_,monostarta_,monoenda_,polystarta_,polyenda_]:=Module[
{bemsaa,CharStringmonoa,CharStringpolya},

If[ifraga==-1,
	bemsaa=bemsaTxtLnc;  (* uses the Fortran output as input *)
	,
	bemsaa=ToExpression["bemsaTxtLnc"<>ToString[ifraga]];
];
CharStringmonoa=bemsaa[[monostarta]];
Do[(
CharStringmonoa=CharStringmonoa<>"
"<>bemsaa[[i]];
),{i,monostarta+1,monoenda}];
CharStringpolya=bemsaa[[polystarta]];
Do[(
CharStringpolya=CharStringpolya<>"
"<>bemsaa[[i]];
),{i,polystarta+1,polyenda}];

{CharStringmonoa,CharStringpolya}
];



JoinAndRenumberTwoMonoAndXStrings[CharStringmono1_,CharStringmono2_,nmono1_,nmono2_,
	nvariables1_,nvariables2_]:=Module[
{CharStringmono2x,CharStringpoly2x,CharStringmono,CharStringpoly},

(* do the monos *)
CharStringmono2x=CharStringmono2;
CharStringmono2x=StringDelete[CharStringmono2x,"    m(0) = 1.0D0
"];
Do[(
CharStringmono2x=StringReplace[CharStringmono2x,"m("<>ToString[i]<>")"-> "m("<>ToString[i+nmono1]<>")"];
),{i,1,nmono2}];

(* do the x *)
Do[(
CharStringmono2x=StringReplace[CharStringmono2x,"x("<>ToString[i]<>")"-> "x("<>ToString[i+nvariables1]<>")"];
),{i,1,nvariables2}];


CharStringmono=CharStringmono1<>"
"<>CharStringmono2x;


CharStringmono
];



JoinAndRenumberTwoPolyStrings[CharStringpoly1_,CharStringpoly2_,nmono1_,nmono2_,npoly1_,npoly2_,
	nvariables1_,nvariables2_]:=Module[
{CharStringmono2x,CharStringpoly2x,CharStringmono,CharStringpoly},

(* do the polys *)
CharStringpoly2x=CharStringpoly2;
CharStringpoly2x=StringDelete[CharStringpoly2x,"    p(0) = m(0)
"];
Do[(
CharStringpoly2x=StringReplace[CharStringpoly2x,"p("<>ToString[i]<>")"-> "p("<>ToString[i+npoly1]<>")"];
),{i,1,npoly2}];
Do[(
CharStringpoly2x=StringReplace[CharStringpoly2x,"m("<>ToString[i]<>")"-> "m("<>ToString[i+nmono1]<>")"];
),{i,1,nmono2}];

CharStringpoly=CharStringpoly1<>"
"<>CharStringpoly2x;

CharStringpoly
];



JoinAndRenumberTwoStrings[CharStringmono1_,CharStringmono2_,CharStringpoly1_,
		CharStringpoly2_,nmono1_,nmono2_,npoly1_,npoly2_,nvariables1_,nvariables2_]:=Module[
{CharStringmono2x,CharStringpoly2x,CharStringmono,CharStringpoly},
(*Print["{nmono1,nmono2,npoly1,npoly2,nvariables1,nvariables2} = ",
	{nmono1,nmono2,npoly1,npoly2,nvariables1,nvariables2}];*)
	
(********
	NB:  I'm doing the Do loops with replacements from the end to the beginning.  When you do it from the
	beginning and the number in the Do loop is larger than the value you are replacing to, you wind up
	replacing numbers that you've already replaced!
************)	

(* do the monos *)
CharStringmono2x=CharStringmono2;
CharStringmono2x=StringDelete[CharStringmono2x,"    m(0) = 1.0D0
"];
Do[(
CharStringmono2x=StringReplace[CharStringmono2x,"m("<>ToString[i]<>")"-> "m("<>ToString[i+nmono1]<>")"];
),{i,nmono2,1,-1}];

(* do the x *)
Do[(
CharStringmono2x=StringReplace[CharStringmono2x,"x("<>ToString[i]<>")"-> "x("<>ToString[i+nvariables1]<>")"];
),{i,nvariables2,1,-1}];

(* do the polys *)
CharStringpoly2x=CharStringpoly2;
CharStringpoly2x=StringDelete[CharStringpoly2x,"    p(0) = m(0)
"];

Do[(
(*If[i<10,Print["Replacing p("<>ToString[i]<>")"-> "p("<>ToString[i+npoly1]<>")"];];*)
CharStringpoly2x=StringReplace[CharStringpoly2x,"p("<>ToString[i]<>")"-> "p("<>ToString[i+npoly1]<>")"];
),{i,npoly2,1,-1}];
Do[(
CharStringpoly2x=StringReplace[CharStringpoly2x,"m("<>ToString[i]<>")"-> "m("<>ToString[i+nmono1]<>")"];
),{i,nmono2,1,-1}];

CharStringmono=CharStringmono1<>"
"<>CharStringmono2x;
CharStringpoly=CharStringpoly1<>"
"<>CharStringpoly2x;
CharStringmono=StringReplace[CharStringmono,"\n\n"-> "\n"];
CharStringpoly=StringReplace[CharStringpoly,"\n\n"-> "\n"];

{CharStringmono,CharStringpoly}
];



GetAssignEVMonoPoly::usage="
Inputs: 
fnamea is the full filename of the bemsa file for the symmetry of the
	fragment of interest.
natomsa is the number of atoms in the fragment of interest
rijnamesa is the names of the bonds in the appropriate order (see user input)

Outputs:
polystarta:  The line of the bemsa file that has the first polynomial definition
	which is p(0) = m(0)
polyenda:  The line of the bemsa file that has the last polynomial definition
npolya: the number of polynomials used in the besma file, not including p(0)
nmonoa: the number of monomials used in the bemsa file, not including m(0)

Global Functions Defined:
Assign[]    Assigns X values to be the appropriate values of dist[[i]]
EvalMono[]  Evaluates the Monomials for the current assignments of X
EvalPoly[]  Evaluates the Polynomials for the current monomials

";
GetAssignEVMonoPoly[ifraga_,natomsa_,rijnamesa_]:=Module[
{bemsaa,isave,monostarta,monoenda,polystarta,polyenda,
CharStringmonoa,CharStringpolya,CharString2a,CharString11a,
npolya, nmonoa,CharStringAsgn,nvariablesa,diagnose
},
diagnose=False;
nvariablesa=natomsa (natomsa-1)/2;
If[ifraga==-1,nvariablesa=Length[rijnamesa];];

(* get mono and poly strings for fragment a *)
{monostarta,monoenda,polystarta,polyenda}=
	Getpolymonostartendpoints[ifraga];
{CharStringmonoa,CharStringpolya}=
	Getmonopolycharstrings[ifraga,monostarta,monoenda,polystarta,polyenda];
	PPCharStringmono=CharStringmonoa;  (* Global for Polynomial pruning program *)
	PPCharStringpoly=CharStringpolya;  (* Global for Polynomial pruning program *)
(* Develop EVMono and EVPoly based on fragment a *)
CharString2a=bemsaToMathematica[CharStringmonoa];
If[diagnose,PLHCharString2a=CharString2a;];
ToExpression[CreateEvalMono[CharString2a]];

CharString11a=bemsaToMathematica[CharStringpolya];
If[diagnose,PLHCharString11a=CharString11a;];
ToExpression[CreateEvalPoly[CharString11a]];
(*Print["EVMono and EVPoly have been defined"]
Print["     EVPoly has ",StringCount[CharString11a,"="]," terms"];
Print[CreateEvalPoly[CharString11a]];*)

npolya=polyenda-polystarta;  (* this does not include the zero-order term *)
nmonoa=monoenda-monostarta;  (* this does not include the zero-order term *)
(* note that plist and mlist must run from zero so as to get m0 and p0 evaluated *)
plist=Table["p"<>ToString[i],{i,0,npolya}];  (* needs to be global *)
mlist=Table["m"<>ToString[i],{i,0,nmonoa}];  (* needs to be global *)
xlist=Table["x"<>ToString[i],{i,1,nvariablesa}];  (* needs to be global *)

(* Make Assignx based on nvariablesa *)
CharStringAsgn="Assignx[]:=Module[\[IndentingNewLine]{},
";
Do[(
CharStringAsgn=CharStringAsgn<>"x"<>ToString[i]<>" = Exp[-dist[["<>ToString[i]<>"]]/2.];
";
),{i,1,nvariablesa}];
CharStringAsgn=CharStringAsgn<>"ToExpression[xlist]
];";
ToExpression[CharStringAsgn];
(*PLHCSA=CharStringAsgn;*)

{polystarta, polyenda, npolya, nmonoa}
];




GetAssignEVMonoPolyPrev::usage="
Inputs: 
xsum: the number of unique interatomic distances in the combined prebious fragments
msum: the number of unique monomials in the combined prebious fragments
psum: the number of unique polynomials in the combined prebious fragments
CharStringmono: the Fortran character string of the unique monomial functions
CharStringpoly: the Fortran charactrer string of the unique polynomial functions

Outputs:
none

Global Functions Defined:
Assign[]    Assigns X values to be the appropriate values of dist[[i]]
EvalMono[]  Evaluates the Monomials for the current assignments of X
EvalPoly[]  Evaluates the Polynomials for the current monomials

";
GetAssignEVMonoPolyPrev[xsum_,msum_,psum_,rijnamesnow_,CharStringmono_,CharStringpoly_]:=Module[
{bemsaa,isave,monostarta,monoenda,polystarta,polyenda,
CharStringmonoa,CharStringpolya,CharString2a,CharString11a,
npolya, nmonoa,CharStringAsgn,nvariablesa
},


(* Develop EVMono and EVPoly based on fragment a *)
CharString2a=bemsaToMathematica[CharStringmono];
ToExpression[CreateEvalMono[CharString2a]];
CharString11a=bemsaToMathematica[CharStringpoly];
ToExpression[CreateEvalPoly[CharString11a]];

nvariablesa=xsum;
npolya=psum;  (* this does not include the zero-order term *)
nmonoa=msum;  (* this does not include the zero-order term *)

(*  NB:  these should b e from 1 to npolya and nmonoa because p(0)and m(0) terms
have been added in CreateEvalPoly and CreateEvalMono *)
plist=Table["p"<>ToString[i],{i,1,npolya}];  (* needs to be global *)
mlist=Table["m"<>ToString[i],{i,1,nmonoa}];  (* needs to be global *)
xlist=Table["x"<>ToString[i],{i,1,nvariablesa}];  (* needs to be global *)

(* Make Assignx based on nvariablesa *)
CharStringAsgn="Assignx[]:=Module[\[IndentingNewLine]{},
";
Do[(
CharStringAsgn=CharStringAsgn<>"x"<>ToString[i]<>" = dist[["<>ToString[i]<>"]];
";
),{i,1,nvariablesa}];
CharStringAsgn=CharStringAsgn<>"ToExpression[xlist]
];";
ToExpression[CharStringAsgn];

(* no outputs *)
];



CreatebemsaFiles::usage="
Inputs are ifrag, the number of the fragment, and fname, the file location
of the bemsa file appropriate for that fragment

The problem this corrects is that the bemsa files often have Fortran ampersands (&)
These need to be removed so that Mathematica can use the list of monomials and 
polynomials to form the functions EvanMono and EvalPoly.  

In addition, we need the bemsa file in several formats: text, lines of text, table
These versions of bemsa are made global and are called (e.g. for fragment 1):
bemsaTxtc1
bemsaTxtLnc1
bemsaTabc1
where the c indicated that the bemsa files have been corrected for the ampersands
";
CreatebemsaFiles[ifrag_]:=Module[
{nexti,dstart,dend,dgroup,nextk,cont8,diagnose},
(* strip character string of ampersand pairs and everything in between them
staring at the end of the string and moving to the beginning *)
diagnose=False;
If[ifrag==-1,
	(* use Fortran output as input *)
	XXX14850=Import[fortranname,"Text"];
	,
	XXX14850=Import[filename[[ifrag]],"Text"];
];

(* strip out continuation marks and join lines *)
(*
(* this was for the case when there were two & *)
dgroup=False;
Do[(
j=LC+1-i;
If[bemsaC[[j]]=="&" && dgroup== False,dend=j;dgroup=True;Goto[nexti];];
If[bemsaC[[j]]!= "&" && dgroup==True , dstart=j;Goto[nexti];];
If[bemsaC[[j]]== "&" && dgroup==True,
	dstart=j;
	(*Print[{dstart,dend}];*)
	bemsaC=Drop[bemsaC,{dstart,dend}];
	dgroup=False;
];
Label[nexti];
),{i,1,Length[bemsaC]}];
*)
f6=XXX14850;
If[diagnose,Print["           stripping ampersands and carriage returns"];];
f6=StringDelete[f6,"&            \n"];
f6=StringDelete[f6,"&           \n"];
f6=StringDelete[f6,"&          \n"];
f6=StringDelete[f6,"&         \n"];
f6=StringDelete[f6,"&        \n"];
f6=StringDelete[f6,"&       \n"];
f6=StringDelete[f6,"&      \n"];
f6=StringDelete[f6,"&     \n"];
f6=StringDelete[f6,"&    \n"];
f6=StringDelete[f6,"&   \n"];
f6=StringDelete[f6,"&  \n"];
f6=StringDelete[f6,"& \n"];
f6=StringDelete[f6,"&\n"];
If[StringCount[f6,"&"]!=0,
	Print["Ampersand problem in CreateBemsa; aborting"];
	Abort[];
	,
	XXX14850=f6;
	];

(* This method is way too slow! *)
(*
Monitor[Do[(
j=LC+1-i;
	If[bemsaC[[j]]==  "&",
		dstart=j;
		Do[(
		If[Or[bemsaC[[k]]=="\n",bemsaC[[k]]==" "],
			Goto[nextk];
			,
			dend=k-1;
			Goto[cont8];
		];
		Label[nextk];
		),{k,j+1,j+30}];
		Print["In CreatebemsaFiles failed to find end of continuation, aborting"];
		Abort[];
		Label[cont8];
	bemsaC=Drop[bemsaC,{dstart,dend}];
	];
),{i,1,Length[bemsaC]}],ProgressIndicator[i]];*)

If[diagnose,Print["           making bemsaTxtc, bemsaTxtLnc, and bemsaTabc"];];
If[ifrag==-1,
(*XXX14850=StringJoin[bemsaC];	*)
ToExpression["bemsaTxtc"<>"=XXX14850;"]; (* bemsaTxtc is global *)
Export[DataDir<>"DeleteMe.txt",ToExpression["bemsaTxtc"],"Text"];
XXX14850=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
ToExpression["bemsaTxtLnc"<>"=XXX14850;"]; (* bemsaTxtLnc is global *)
XXX14850=Import[DataDir<>"DeleteMe.txt","Table"];
ToExpression["bemsaTabc"<>"=XXX14850;"]; (* bemsaTabc is global *)
DeleteFile[DataDir<>"DeleteMe.txt"];
,	
XXX14850=StringJoin[bemsaC];	
ToExpression["bemsaTxtc"<>ToString[ifrag]<>"=XXX14850;"]; (* bemsaTxtc is global *)
Export[DataDir<>"DeleteMe.txt",ToExpression["bemsaTxtc"<>ToString[ifrag]],"Text"];
XXX14850=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
ToExpression["bemsaTxtLnc"<>ToString[ifrag]<>"=XXX14850;"]; (* bemsaTxtLnc is global *)
XXX14850=Import[DataDir<>"DeleteMe.txt","Table"];
ToExpression["bemsaTabc"<>ToString[ifrag]<>"=XXX14850;"]; (* bemsaTabc is global *)
DeleteFile[DataDir<>"DeleteMe.txt"];
];
(*
Export[DataDir<>"bemsaTxtc"<>FileNameTake[fortranname,-1],bemsaTxtc,"Text"];
Export[DataDir<>"bemsaTxtLnc"<>FileNameTake[fortranname,-1],bemsaTxtLnc,{"Text","Lines"}];
Export[DataDir<>"bemsaTabc"<>FileNameTake[fortranname,-1],bemsaTabc,"Table"];
*)
(* no output *)
];



SequentialBuildup::usage="SequentialBuildup[]
This is the  main program for the deletion of duplicates via the sequential method, 
but using the original Chen method of evaluating derivatives. 
Here is the Structure of the Program:

SequentialBuildup[]
  Initiation steps:
     MakerijnamesAndnatomsfrag[];
     SortIndividualInteratomicDistanceNames[];
     CreatebemsaFiles[ifrag]  - do for all fragments
     Getpolymonostartendpoints[ifrag1]
     Getmonopolycharstrings[ifrag1,monostart1,monoend1,polystart1,polyend1];
  Then do successive steps of the following:
     Getpolymonostartendpoints[ifraga];
     Getmonopolycharstrings[ifraga,monostarta,monoenda,polystarta,polyenda];
    {Mcommon,Munique}=GetCommonMonomialValuesWithPrevious[ifraga,natomsa,rijnamesa,nvariablesa,
	         xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
		Consists of:
         GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
			above command makes Assignx, EvalMono, EvalPoly\[IndentingNewLine]        (* evaluate mononomials of frag b and store randomly assigned x values in xb *)\[IndentingNewLine]        (* assign random numbers to x, reset the common ones to the corresponding
             xb value using Tdupsab, and then evaluate polynomials *)\[IndentingNewLine]        (* compare mononomials for a to those in b, recording which ones are in common
             and which ones are unique *)\[IndentingNewLine]        Output: {Mcommon,Munique}
    {Rcommon,Runique}=GetCommonPolynomialValuesWithPrevious[ifraga,natomsa,rijnamesa,nvariablesa,
		    xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
		Consists of:
        GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
           above command makes Assignx, EvalMono, EvalPoly\[IndentingNewLine]       (* evaluate polynomials of frag b and store randomly assigned x values in xb *)\[IndentingNewLine]       (* assign random numbers to x, reset the common ones to the corresponding
          xb value using Tdupsab and then evaluate polynomials *)\[IndentingNewLine]       (* compare polynomials for a to those in b, recording which ones are in common
          and which ones are unique *)\[IndentingNewLine]       Output: {Rcommon,Runique}
    {dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesnow];
    JoinAndRenumberTwoStrings[CharStringmono,CharStringmonoa,CharStringpoly,CharStringpolya,
		msum,nmonoa,psum,npolya,xsum,nvariablesa];
    (* renumber duplicate x values in new mvalues *)
    (* consolidate and renumber the m values in mono and poly lists *)
    (* consolidate and renumber the p values in the poly lists, renaming common p's to d's *)
    (* delete lines that have d definitions on the lhs of poly and mono*)
    (* rename remaining d(z) to p(z)  *)
    (* renumber the polynomials  *)
    RenumberFortranNonsequentialx[CharStringmono,xsum,nvariablesa,Length[Tdupsab]];
	get ready for next possible additional fragment
  Finish up:
    CommentXAssignments=GenerateFortranCommentWithXAssignments[];
    MakeExportFortran[CommentXAssignments,CharStringmono,CharStringpoly];
	  NB: this creates a Fortran file that can then run with the Chen derivative method
	  If you want the batch derivative method, you need at the end to execute
	  MakeExportFortranDDWithBatchDerivatives[CommentXAssignments,CharStringmono,CharStringpoly];
      Thus, the complete command string following the (yellow) input section, should look like:
      time=Timing[{CommentXAssignments,CharStringmono,CharStringpoly}=SequentialBuildup[]][[1]];\[IndentingNewLine]      Print['The program took ',time,' sec.']  (* change single quotes to double *)\[IndentingNewLine]      RunTests[];\[IndentingNewLine]      MakeExportFortranDDWithBatchDerivatives[CommentXAssignments,CharStringmono,CharStringpoly];
";
SequentialBuildup[]:=Module[
{natomsa,rijnamesa,nvariablesa,monostarta,monoenda,
polystarta,polyenda,CharStringmonoa,CharStringpolya,CharStringpoly1,
CharStringmono1,natoms1,rijnames1,nvariables1,ifrag1,monostart1,monoend1,
polystart1,polyend1,
CharStringmono,CharStringpoly,nmono1,npoly1,
FinishUp,DeletionList,dups,Tdupsab,
CharString2a,CharString11a, CharStringAsgn,
Runique,nmonoa,npolya,
Rcommon,Mcommon,CommentXAssignments,
Ccommon,TCcom,xsum,msum,psum,used,unused,ifraga,rijnamesP,
rijnamesnow,Munique,diagnose
},
diagnose=False;
(* ***************  Initiation  **************** *)
xsum=0;
msum=0;
psum=0;
MakerijnamesAndnatomsfrag[];
SortIndividualInteratomicDistanceNames[];

(* Check nvariables and rijnames *)
Do[(
	If[
		natomsfrag[[ifrag]]*(natomsfrag[[ifrag]]-1)/2 != Length[rijnames[[ifrag]]],
		Print["nvariables and Length[,rijnames] are not consistent for ifrag = ",ifrag];Abort[];
	];
),{ifrag,1,nfragments}];
(* create corrected bemsa files for all fragments *)
Do[(CreatebemsaFiles[ifrag]),{ifrag,1,nfragments}];

(* get the first fragment  call it 1 *)
natoms1=natomsfrag[[1]];
rijnames1=rijnames[[1]];
ifrag1=1;
nvariables1=natoms1 (natoms1-1)/2;
(* get mono and poly strings for fragment a *)
{monostart1,monoend1,polystart1,polyend1}=
	Getpolymonostartendpoints[ifrag1];
{CharStringmono1,CharStringpoly1}=
	Getmonopolycharstrings[ifrag1,monostart1,monoend1,polystart1,polyend1];
If[diagnose,PLHCSM1seq=CharStringmono1;PLHCSP1seq=CharStringpoly1];

npoly1=polyend1-polystart1;  (* this does not include the zero-order term *)
nmono1=monoend1-monostart1;  (* this does not include the zero-order term *)

(* check here to see if msa uses x's on rhs of m definitions, as it does for large 
orders.  If found, change the x's to the equivalent m's. Added 10 Feb 2023 *)
countx=StringCount[CharStringmono1,"x("];
If[countx>nvariables1,
	Print["Taking care of x's on rhs of m definitions for high order msa input"];
	{mforx,xform}=makemforxformDD[nvariables1, CharStringmono1];
	CSMtable=textconvert[CharStringmono1,"text","tablestar",DataDir];
	istart=Position[CSMtable[[All,1]],"m("<>ToString[nvariables1]<>")"][[1,1]]+1;
	Do[(
	  idyn=Length[CSMtable]-i;
      Do[(
        If[StringContainsQ[CSMtable[[i,j]],"x("],
          open=StringPosition[CSMtable[[i,j]],"("][[1,1]];
          close=StringPosition[CSMtable[[i,j]],")"][[1,1]];
          num=ToExpression[StringTake[CSMtable[[i,j]],{open+1,close-1}]];
          CSMtable[[i,j]]=StringReplace[CSMtable[[i,j]],
             "x("<>ToString[num]<>")"->"m("<>ToString[mforx[[num]]]<>")"];
          ];
        ),{j,3,Length[CSMtable[[i]]]}];
       ),{i,istart,Length[CSMtable]}];
       CSMlines=textconvert[CSMtable,"table","textlines",DataDir];
       Do[(
	     CSMlines[[i]]="    "<>CSMlines[[i]];
          ),{i,2,Length[CSMlines]}];
CharStringmono1=textconvert[CSMlines,"textlines","text",DataDir];
CharStringmono1=StringReplace[CharStringmono1," * "->"*"];
CharStringmono1=StringReplace[CharStringmono1," ** "->"**"];
];	

(* starting values for successive steps:  *)
CharStringpoly=CharStringpoly1;
CharStringmono=CharStringmono1;
(*CSMforBackward=CharStringmono;
CSPforBackward=CharStringpoly;*)

xsum=xsum+nvariables1;
msum=msum+nmono1;
psum=psum+npoly1;
rijnamesnow=rijnames[[1]];
Print["For Fragment 1, the nma number of {x,m,p} is ",{nvariables1,nmono1,npoly1},
	"(excl. m(0),p(0))"];
If[nfragments==1,Goto[FinishUp];];	

(* ***************  Steps of Successive Addition  **************** *)
(* add successive fragments, eliminating duplicate values *)
(* loop over all the other fragments in sequence *)
Do[(
	(*Print["{xsum,msum,psum} = ",{xsum,msum,psum}];*)
	(* get the next fragment, we'll call it fragment a *)
	ifraga=ifrag;
	(*Print["fnamea = ",fnamea];*)
	natomsa=natomsfrag[[ifraga]];
	(*Print["natomsa = ",natomsa];*)
	rijnamesa=rijnames[[ifraga]];
	(*Print["rignamesa = ",rijnamesa];*)
	nvariablesa=natomsa (natomsa-1)/2;
	(* now get mono and poly strings for fragment a *)
	{monostarta,monoenda,polystarta,polyenda}=
		Getpolymonostartendpoints[ifraga];
	{CharStringmonoa,CharStringpolya}=
		Getmonopolycharstrings[ifraga,monostarta,monoenda,polystarta,polyenda];
	npolya=polyenda-polystarta;  (* this does not include the zero-order term *)
	nmonoa=monoenda-monostarta;  (* this does not include the zero-order term *)
(*If[ifrag\[Equal]4,
	Print["{natomsa,rijnamesa,nvariablesa,nmonoa,npolya} = ",
	{natomsa,rijnamesa,nvariablesa,nmonoa,npolya}];
];*)
	(* output some analysis *)
	Print["For Fragment ",ifrag,
		", the nma number of {x,m,p} is ",{nvariablesa,nmonoa,npolya},
			"(excl. m(0),p(0))"]; 	
			
	(* determine which numbered polynomials in frag a are in common with those in the 
	combination of all previous fragments *)	
	{Mcommon,Munique}=GetCommonMonomialValuesWithPrevious[ifraga,natomsa,rijnamesa,nvariablesa,
	xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
	(*Print["{xsum,msum,psum} = ",{xsum,msum,psum}];*)
(*If[ifraga\[Equal]4,
Print["{Mcommon,Munique} = ",{Mcommon,Munique}];
];*)
(*
PLHP0=CharStringpoly;
PLHM0=CharStringmono;	
*)
	{Rcommon,Runique}=GetCommonPolynomialValuesWithPrevious[ifraga,natomsa,rijnamesa,nvariablesa,
		xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];
(*If[ifraga\[Equal]4,
Print["{Rcommon,Runique} = ",{Rcommon,Runique}];
];*)
	(* Rcommon now gives the polynomial numbers of frag ifrag that are in common with those in all
		previous fragments *)
	(* Mcommon now gives the monomial numbers of frag ifrag that are in common with those in all
		previous fragments *)
	(* The numbering in Rcommon is the numbering of frag a*)
	(* get some information for analysys *)
	{dups,Tdupsab}=GetDuplicationTable[rijnamesa,rijnamesnow];
(*
Print["{dups,Tdupsab} = ",{dups,Tdupsab}];
Print["rijnamesa = ",rijnamesa];
Print["rijnamesnow = ",rijnamesnow];
*)
	Print["     Frag. ",ifrag," shares ",Length[Tdupsab],
		" coordinates, ",Length[Mcommon]," monomials, and ",Length[Rcommon],
		" polynomials with previous fragments "];
	Print["     After deleting duplicates, Frag. ",ifrag," shares \n     ",
		Length[Tdupsab]," coordinates, ",
		Length[Mcommon]," monomials, and ",Length[Rcommon],
		" polynomials with the previous fragment(s)"];
	Print["     The number of new {x,m,p} for frag ",ifrag," is ",
		{nvariablesa-Length[Tdupsab],nmonoa-Length[Mcommon],npolya-Length[Rcommon]}];
(*If[ifraga\[Equal]4, Abort[]];*)
	(* Join CharStrings with offset renumbering of x, monos, polys *)
	(*Print["LRcommon = ", Length[Rcommon]];
	Print["LMcommon = ", Length[Mcommon]];
	Print["{msum,nmonoa,psum,npolya,xsum,nvariablesa} = ",{msum,nmonoa,psum,npolya,xsum,nvariablesa}];*)
	{CharStringmono,CharStringpoly}=
		JoinAndRenumberTwoStrings[CharStringmono,CharStringmonoa,CharStringpoly,CharStringpolya,
		msum,nmonoa,psum,npolya,xsum,nvariablesa];
		
	(* CharStringmono and CharStringpoly are the complete renumbered lists
		as character strings *)
(*
PLHP1=CharStringpoly;	
PLHM1=CharStringmono;
*)
(********
	NB:  I'm doing the Do loops with replacements from the end to the beginning.  When you do it from the
	beginning and the number in the Do loop is larger than the value you are replacing to, you wind up
	replacing numbers that you've already replaced!
************)
	(* renumber duplicate x values in new mvalues *)
	Do[(
		CharStringmono=StringReplace[CharStringmono,"x("<>ToString[Tdupsab[[i,1]]+xsum]<>")"-> 
			"x("<>ToString[Tdupsab[[i,2]]]<>")"];
	),{i,Length[Tdupsab],1,-1}];
	
	(* consolidate and renumber the m values in mono and poly lists *)
	Do[(
		CharStringmono=StringReplace[CharStringmono,"m("<>ToString[Mcommon[[i,1]]+msum]<>")"->   
			"d("<>ToString[Mcommon[[i,2]]]<>")"];	
		CharStringpoly=StringReplace[CharStringpoly,"m("<>ToString[Mcommon[[i,1]]+msum]<>")"->   
			"m("<>ToString[Mcommon[[i,2]]]<>")"];	
	),{i,Length[Mcommon],1,-1}];
(*	
PLHrijnamesa=rijnamesa;
PLHrijnamesnow=rijnamesnow;
PLHTdupsab=Tdupsab;
PLHMcommon=Mcommon;
PLHRcommon=Rcommon;
PLHP2=CharStringpoly;
PLHM2=CharStringmono;
*)
	(* consolidate and renumber the p values in the poly lists, renaming common p's to d's *)
	Do[(
		CharStringpoly=StringReplace[CharStringpoly,"p("<>ToString[Rcommon[[i,1]]+psum]<>")"-> 
			"d("<>ToString[Rcommon[[i,2]]]<>")"];			
	),{i,Length[Rcommon],1,-1}];
(*
PLHP3=CharStringpoly;
PLHM3=CharStringmono;
*)
	(*Print["L poly before deletions = ",StringCount[CharStringpoly,"="]-1];*)	
	(* delete lines that have d definitions on the lhs of poly and mono*)
	CharStringmono=DeleteAllFortranDefinitionsStartingd[CharStringmono];
	CharStringpoly=DeleteAllFortranDefinitionsStartingd[CharStringpoly];
	(*Print["L poly after deletions = ",StringCount[CharStringpoly,"="]-1];*)
(*
PLHP4=CharStringpoly;	
PLHM4=CharStringmono;
*)
	(* rename remaining d(z) to p(z)  *)
	CharStringmono=StringReplace[CharStringmono,"d"-> "m"];
	CharStringpoly=StringReplace[CharStringpoly,"d"-> "p"];

	(*Print["L poly before renumbering = ",StringCount[CharStringpoly,"="]-1];*)
		
	(* renumber the polynomials  *)	
	{CharStringmono,CharStringpoly}=RenumberFortranExistingmInMonoAndPoly[CharStringmono,CharStringpoly];

	(*Print["L poly after renumbering = ",StringCount[CharStringpoly,"="]-1];*)
(*
PLHP5=CharStringpoly;
PLHM5=CharStringmono;
PLHifrag=ifrag;
*)
	CharStringpoly=RenumberFortranExistingp[CharStringpoly];
(*
PLHP6=CharStringpoly;	
PLHM6=CharStringmono;
*)
	(*Print[CharStringmono];*)
	CharStringmono=
		RenumberFortranNonsequentialx[CharStringmono,xsum,nvariablesa,Length[Tdupsab]];
(*
PLHP7=CharStringpoly;	
PLHM7=CharStringmono;	
*)
	(* calculate new values *)
	xsum=xsum+nvariablesa-Length[Tdupsab];
	msum=msum+nmonoa-Length[Mcommon];
	psum=psum+npolya-Length[Rcommon];
	(* Important:  Do not use Union below.  The order is important and Union sorts the results  *)
	(* 1/29/20 I think that this next statement is incorrect
	rijnamesnow=DeleteDuplicates[Join[rijnames[[1]],rijnames[[2]]]]
	and that it should be the next one *)
	rijnamesnow=DeleteDuplicates[Join[rijnamesnow,rijnamesa]]
	(*Print["{xsum, Length[rijnamesnow]} =",{xsum, Length[rijnamesnow]}];*)
	
),{ifrag,2,nfragments}];


(* ***************  Finish Up  **************** *)
Label[FinishUp];
Print["The number of all generated   monomials, including m(0), is ",StringCount[CharStringmono,"="]];
Print["The number of all generated polynomials, including p(0), is ",StringCount[CharStringpoly,"="]];
Print["The summed number of all new   monomials, including m(0), is ",msum+1];
Print["The summed number of all new polynomials, including p(0), is ",psum+1];
CommentXAssignments=GenerateFortranCommentWithXAssignments[];
Print["The number of final coordinates is ",StringCount[CommentXAssignments,"="]];
Print["They are: "];
Print[StringDrop[StringReplace[StringDelete[CommentXAssignments,"!     "],"\n"->", "],-2]];
rijnamesP=Flatten[Table[
	  If[i<10,"0"<>ToString[i],ToString[i]]<>
	If[j<10,"0"<>ToString[j],ToString[j]],
{i,1,natomsparent},{j,i+1,natomsparent}]];
used={};
Do[(
	used=Union[used,rijnames[[ifrag]]];
),{ifrag,1,nfragments}];
unused=Complement[rijnamesP,used];
Print["The following interatomic distances in the parent are omitted: ",unused];
Globalunused=unused; (* Global *)
(* make and export to file the new Fortran code *)

	
MakeExportFortran[CommentXAssignments,CharStringmono,CharStringpoly];

{CommentXAssignments,CharStringmono,CharStringpoly}
];



PairwiseMethod::usage="PairwiseMethod[]
This is the pairwise version of the delete duplicates program.  

In the pairwise version, each fragment is added and adjusted for the commonality with the
fragments that have been previously used, keeping track of the offsets.  After all fragments
have been added, the appropriate duplicates are deleted and then all the monomials and polynomials
are renumbered.

The structure of the program is as follows:
SortIndividualInteratomicDistanceNames[];
Do[(CreatebemsaFiles[ifrag]),{ifrag,1,nfragments}];
(* get the first fragment from bemsa file and convert to Mathematica *)
	Then loop over all subsequent fragments doing the following:
    Get the next fragment and convert to Mathematica
   (* Join CharStrings with offset renumbering of x, monos, polys *)
    Look at all the fragments up to one less than the current fragment and
       determine how many x,m,p are duplicates and how many of those for the 
       current fragment are unique:
	   This procedure uses AnalyzeFrags, GetCommonMonomialValues, GetCommonPolynomialValues
       (* renumber duplicate x values in new mvalues using offsets *)\[IndentingNewLine]       (* consolidate and renumber the m values in mono and poly lists using offsets*)\[IndentingNewLine]       (* consolidate and renumber the p values in the poly lists, renaming common 
			p's to d's using offsets*)
		(* get ready for next fragment *)
Finish Up:
	After all the fragments have been added:\[IndentingNewLine]   (* delete lines that have d definitions on the lhs of poly and mono*)\[IndentingNewLine]   (* rename remaining d(z) to p(z)  *)\[IndentingNewLine]   (* renumber the polynomials  *)
	MakeExportFortran[CommentXAssignments,CharStringmono,CharStringpoly];

";
PairwiseMethod[]:=Module[

{fnamea,natomsa,rijnamesa,nvariablesa,nvariablesb,monostarta,monoenda,
polystarta,polyenda,CharStringmonoa,CharStringpolya,CharStringpoly1,
CharStringmono1,CharStringpoly2,CharStringmono2,nmono2,npoly2,nvariables2,
npoly1,nmono1,nvariables1,offset1,offset2,xsum,
CharStringmono,CharStringpoly,FinishUp,DeletionList,dups,Tdupsab,rijnamesb,
CharString2a,CharString11a, CharStringAsgn,
nmono,npoly,nvariables,Runique,nmonoa,npolya,
Rcommon,Rcommonx,Mcommon,Mcommonx,varnames,CommentXAssignments,
Ccommon,TCcom,used,unused,ifraga,fname,rijnamesP
},



MakerijnamesAndnatomsfrag[];
SortIndividualInteratomicDistanceNames[];

(* Check nvariables and rijnames *)
Do[(
	If[
		natomsfrag[[ifrag]]*(natomsfrag[[ifrag]]-1)/2 != Length[rijnames[[ifrag]]],
		Print["nvariables and Length[,rijnames] are not consistent for ifrag = ",ifrag];Abort[];
	];
),{ifrag,1,nfragments}];

(* create corrected (ampersand deleted) bemsa files for all fragments *)
Do[(CreatebemsaFiles[ifrag]),{ifrag,1,nfragments}];

(* get the first fragment *)
fnamea=filename[[1]];
natomsa=natomsfrag[[1]];
rijnamesa=rijnames[[1]];
ifraga=1;
nvariablesa=natomsa (natomsa-1)/2;
(* get mono and poly strings for fragment a *)
{monostarta,monoenda,polystarta,polyenda}=
	Getpolymonostartendpoints[ifraga];
{CharStringmonoa,CharStringpolya}=
	Getmonopolycharstrings[ifraga,monostarta,monoenda,polystarta,polyenda];
CharStringpoly1=CharStringpolya;
CharStringmono1=CharStringmonoa;
npoly1=polyenda-polystarta;  (* this does not include the zero-order term *)
nmono1=monoenda-monostarta;  (* this does not include the zero-order term *)

(* check here to see if msa uses x's on rhs of m definitions, as it does for large 
orders.  If found, change the x's to the equivalent m's. Added 10 Feb 2023 *)
countx=StringCount[CharStringmono1,"x("];
If[countx>nvariables1,
	Print["Taking care of x's on rhs of m definitions for high order msa input"];
	{mforx,xform}=makemforxformDD[nvariables1, CharStringmono1];
	CSMtable=textconvert[CharStringmono1,"text","tablestar",DataDir];
	istart=Position[CSMtable[[All,1]],"m("<>ToString[nvariables1]<>")"][[1,1]]+1;
	Do[(
	  idyn=Length[CSMtable]-i;
      Do[(
        If[StringContainsQ[CSMtable[[i,j]],"x("],
          open=StringPosition[CSMtable[[i,j]],"("][[1,1]];
          close=StringPosition[CSMtable[[i,j]],")"][[1,1]];
          num=ToExpression[StringTake[CSMtable[[i,j]],{open+1,close-1}]];
          CSMtable[[i,j]]=StringReplace[CSMtable[[i,j]],
             "x("<>ToString[num]<>")"->"m("<>ToString[mforx[[num]]]<>")"];
          ];
        ),{j,3,Length[CSMtable[[i]]]}];
       ),{i,istart,Length[CSMtable]}];
	    CSMlines=textconvert[CSMtable,"table","textlines",DataDir];
	    Do[(
	      CSMlines[[i]]="    "<>CSMlines[[i]];
	     ),{i,2,Length[CSMlines]}];
        CharStringmono1=textconvert[CSMlines,"textlines","text",DataDir];
        CharStringmono1=StringReplace[CharStringmono1," * "->"*"];
        CharStringmono1=StringReplace[CharStringmono1," ** "->"**"];
	];	
	
nvariables1=nvariablesa;
CharStringpoly=CharStringpoly1;
CharStringmono=CharStringmono1;

nmono=Table[0,{i,1,nfragments}];
npoly=Table[0,{i,1,nfragments}];
nvariables=Table[0,{i,1,nfragments}];
npoly[[1]]=npoly1;
nmono[[1]]=nmono1;
nvariables[[1]]=nvariables1;
xsum=0;

Print["For Fragment 1, the nma number of {x,m,p} is ",{nvariables1,nmono1,npoly1},
	"(excl. m(0),p(0))"];
(* add successive fragments, eliminating duplicate values *)
varnames={};
Do[(
	(* get the next fragment *)
	fnamea=filename[[ifrag]];
	ifraga=ifrag;
	(*Print["fnamea = ",fnamea];*)
	natomsa=natomsfrag[[ifraga]];
	(*Print["natomsa = ",natomsa];*)
	rijnamesa=rijnames[[ifraga]];
	(*Print["rignamesa = ",rijnamesa];*)
	nvariablesa=natomsa (natomsa-1)/2;
	(* get mono and poly strings for fragment a *)
	{monostarta,monoenda,polystarta,polyenda}=
		Getpolymonostartendpoints[ifraga];
	{CharStringmonoa,CharStringpolya}=
		Getmonopolycharstrings[ifraga,monostarta,monoenda,polystarta,polyenda];
	CharStringpoly2=CharStringpolya;
	CharStringmono2=CharStringmonoa;
	npoly2=polyenda-polystarta;  (* this does not include the zero-order term *)
	nmono2=monoenda-monostarta;  (* this does not include the zero-order term *)
	nvariables2=nvariablesa;
	nmono[[ifrag]]=nmono[[ifrag-1]]+nmono2;
	npoly[[ifrag]]=npoly[[ifrag-1]]+npoly2;
	nvariables[[ifrag]]=nvariables[[ifrag-1]]+nvariables2;
	xsum=xsum+nvariables[[ifrag-1]];
	Print["For Fragment ",ifrag,
		", the nma number of {x,m,p} is ",{nvariables2,nmono2,npoly2},
			"(excl. m(0),p(0))"]; 	
	(* determine which numbered polynomials in frag 2 are in common with those in the combined frag *)
	(* NB: This must be done in pairwise fashion *)
	(* count common variables; do all pairs of ifrag with any frag of lower number*)

	(* Join CharStrings with offset renumbering of x, monos, polys *)	
		{CharStringmono,CharStringpoly}=
		JoinAndRenumberTwoStrings[CharStringmono,CharStringmono2,CharStringpoly,CharStringpoly2,
		nmono[[ifrag-1]],nmono2,npoly[[ifrag-1]],npoly2,nvariables[[ifrag-1]],nvariables2];
		(* CharStringmono and CharStringpoly are the complete renumbered lists
		as character strings *)
		
(*PLHCSM1=CharStringmono;
PLHCSP1=CharStringpoly;*)
	
	
	
	Rcommon={};
	Mcommon={};
	Ccommon={};
	Do[(  (* over previous fragments j *)
		{Rcommonx,Runique,Tdupsab,Mcommonx}=AnalyzeFrags[ifrag,j];
		Print["     Frag. ",ifrag," shares ",Length[Tdupsab],
			" coordinates, ",Length[Mcommonx]," monomials, and ",Length[Rcommonx],
			" polynomials with Frag. ",j];
		TCcom=Table[Tdupsab[[i,1]],{i,1,Length[Tdupsab]}];
		Rcommon=Rcommonx;
		Mcommon=Mcommonx;
		Ccommon=TCcom;
(*
		Print["     After deleting duplicates, Frag. ",ifrag," shares \n     ",
			Length[Ccommon]," coordinates, ",
			Length[Mcommon]," monomials, and ",Length[Rcommon],
			" polynomials with the previous fragment(s)"];
		Print["     The number of new {x,m,p} for frag ",ifrag," is ",
			{nvariables2-Length[Ccommon],nmono2-Length[Mcommon],npoly2-Length[Rcommon]}];
	
*)	
		
	(* Rcommon now gives the polynomial numbers of frag ifrag that are in common with those in the previous 
		fragment j *)
	(* Mcommon now gives the monomial numbers of frag ifrag that are in common with those in the previous
		fragment j *)
	(* The numbering in Rcommon/Mcommon is the numbering of frag a *)
	
	(*Print["LRcommon = ", Length[Rcommon]];
	Print["LMcommon = ", Length[Mcommon]];*)
	
(*PLHRcommon=Rcommon;
PLHMcommon=Mcommon;
PLHTdupsab=Tdupsab;
PLHnmono=nmono;
PLHnvariables=nvariables;
PLHnpoly=npoly;*)

	

		
		(* renumber duplicate x values in new m values *)
		offset1=Which[ifrag==1,0,ifrag==2,nvariables[[1]],ifrag==3,nvariables[[2]],
			ifrag==4,nvariables[[3]],ifrag==5,nvariables[[4]],ifrag==6,nvariables[[5]],
			ifrag==7,Print["too many fragments for current settings; aborting"];Abort[];];
		offset2=Which[j==1,0,j==2,nvariables[[1]],j==3,nvariables[[2]],
			j==4,nvariables[[3]],j==5,nvariables[[4]],j==6,nvariables[[5]],j==7,
			Print["too many fragments for current settings; aborting"];Abort[];];
		Do[(
			CharStringmono=StringReplace[CharStringmono,"x("<>ToString[Tdupsab[[i,1]]+offset1]<>")"-> 
			"x("<>ToString[Tdupsab[[i,2]]+offset2]<>")"];
		),{i,Length[Tdupsab],1,-1}];
	
(*PLHCSM2=CharStringmono;
PLHCSP2=CharStringpoly;*)
	
		(* consolidate and renumber the m values in mono and poly lists *)
		offset1=Which[ifrag==1,0,ifrag==2,nmono[[1]],ifrag==3,nmono[[2]],ifrag==4,nmono[[3]],
			ifrag==5,nmono[[4]],ifrag==6,nmono[[5]],ifrag==7,Print["too many fragments for current settings; aborting"];Abort[];];
		offset2=Which[j==1,0,j==2,nmono[[1]],j==3,nmono[[2]],j==4,nmono[[3]],j==5,nmono[[4]],j==6,nmono[[5]],j==7,
			Print["too many fragments for current settings; aborting"];Abort[];];
		Do[(
			CharStringmono=StringReplace[CharStringmono,"m("<>ToString[Mcommon[[i,1]]+offset1]<>")"-> 
			"d("<>ToString[Mcommon[[i,2]]+offset2]<>")"];	
			CharStringpoly=StringReplace[CharStringpoly,"m("<>ToString[Mcommon[[i,1]]+offset1]<>")"-> 
			"m("<>ToString[Mcommon[[i,2]]+offset2]<>")"];	
		),{i,Length[Mcommon],1,-1}];
		
(*PLHCSM3=CharStringmono;
PLHCSP3=CharStringpoly;*)
	
	    (* consolidate and renumber the p values in the poly lists, renaming common p's to d's *)
		offset1=Which[ifrag==1,0,ifrag==2,npoly[[1]],ifrag==3,npoly[[2]],ifrag==4,npoly[[3]],
			ifrag==5,npoly[[4]],ifrag==6,npoly[[5]],ifrag==7,Print["too many fragments for current settings; aborting"];Abort[];];
		offset2=Which[j==1,0,j==2,npoly[[1]],j==3,npoly[[2]],j==4,npoly[[3]],j==5,npoly[[4]],j==6,npoly[[5]],j==7,
			Print["too many fragments for current settings; aborting"];Abort[];];
		Do[(
			CharStringpoly=StringReplace[CharStringpoly,"p("<>ToString[Rcommon[[i,1]]+offset1]<>")"-> 
			"d("<>ToString[Rcommon[[i,2]]+offset2]<>")"];			
		),{i,Length[Rcommon],1,-1}];
		
(*PLHCSM4=CharStringmono;
PLHCSP4=CharStringpoly;*)

	),{j,1,ifrag-1}];
	
					
	(* get ready for next fragment *)
	xsum=xsum+nvariables[[ifrag-1]];
	
),{ifrag,2,nfragments}];

Label[FinishUp];
	
	(* delete lines that have d definitions on the lhs of poly and mono*)
	CharStringmono=DeleteAllFortranDefinitionsStartingd[CharStringmono];
	CharStringpoly=DeleteAllFortranDefinitionsStartingd[CharStringpoly];
		
(*PLHCSM5=CharStringmono;
PLHCSP5=CharStringpoly;*)
	
	(* rename remaining d(z) to p(z)  *)
	CharStringmono=StringReplace[CharStringmono,"d"->"m"];
	CharStringpoly=StringReplace[CharStringpoly,"d"->"p"];
		
(*PLHCSM6=CharStringmono;
PLHCSP6=CharStringpoly;*)
	
	(* renumber the polynomials  *)
	{CharStringmono,CharStringpoly}=RenumberFortranExistingmInMonoAndPoly[CharStringmono,CharStringpoly];
	CharStringpoly=RenumberFortranExistingp[CharStringpoly];
	
	CharStringmono=
		RenumberFortranNonsequentialx[CharStringmono,xsum,nvariablesa,Length[Tdupsab]];
		
(*PLHCSM7=CharStringmono;
PLHCSP7=CharStringpoly;*)




Print["The number of all generated   monomials, including m(0), is ",StringCount[CharStringmono,"="]];
Print["The number of all generated polynomials, including p(0), is ",StringCount[CharStringpoly,"="]];

CommentXAssignments=GenerateFortranCommentWithXAssignments[];
Print["The number of final coordinates is ",StringCount[CommentXAssignments,"="]];
Print["They are: "];
Print[StringDrop[StringReplace[StringDelete[CommentXAssignments,"!     "],"\n"->", "],-2]];
rijnamesP=Flatten[Table[
	  If[i<10,"0"<>ToString[i],ToString[i]]<>
	If[j<10,"0"<>ToString[j],ToString[j]],
{i,1,natomsparent},{j,i+1,natomsparent}]];
used={};
Do[(
	used=Union[used,rijnames[[ifrag]]];
),{ifrag,1,nfragments}];
unused=Complement[rijnamesP,used];
Print["The following interatomic distances in the parent are omitted: ",unused];
Globalunused=unused; (* Global *)
(* make and export to file the new Fortran code *)

MakeExportFortran[CommentXAssignments,CharStringmono,CharStringpoly];

{CommentXAssignments,CharStringmono,CharStringpoly}
];



MakeExportFortran[CommentXAssignments_,CharStringmono_,CharStringpoly_]:=Module[
{fnamea,natomsa,rijnamesa,bemsaa,monostarta,monoenda,StringAssigns,
polystarta,polyenda,newfortran,xstart1,xstart2,xstart3,xmiddle,xending,CSMlines,
AnotherRoundM,AnotherRoundP,CSPlines,CSMlinesch,CSPlinesch,jsave,outM,outP,
CSMappend,CSMremaining,CSPappend,CSPremaining,nvar,nmn,npol,z,Tijnums,
linedist,xform,nvariables,diagnose},

diagnose=False;
(*nvariables=natomsparent (natomsparent-1)/2;  (* this is wrong because 
	there are now usualy fewer variables in use for fragmentation *) *)
	nvariables=StringCount[CommentXAssignments,"="];
If[diagnose,Print["nvariables = ",nvariables];];
(* enter fortran template pieces*)
xstart1= "
module bemsa
  implicit none
  integer, parameter :: wp  = kind(1.0D0)
  real(wp)::r(12z1047,12z1047)
  real(wp), parameter :: a = 2.d0 
contains
  function emsav(x,c) result(v)
    implicit none
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:578)::p
    real(wp),dimension(0:578)::c
    real(wp)::v
    
    call bemsav(x,p)
    v = dot_product(p,c)
    
    return
  end function emsav
  
  subroutine bemsav(x,p)
    implicit none
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:239)::m
    real(wp),dimension(0:578)::p
    ! ::::::::::::::::::::

    
    call evmono(x,m)
    call evpoly(m,p)
    
    return
  end subroutine bemsav
  
  ";
xstart3="subroutine evmono(x,m)
    implicit none
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:239)::m
    !::::::::::::::::::::
    ";
xmiddle="
return
end subroutine evmono

subroutine evpoly(m,p)
implicit none
real(wp),dimension(0:619)::m
real(wp),dimension(0:3027)::p
!::::::::::::::::::::

";
xending="\n    return\n  end subroutine evpoly\n\n end module bemsa";

(* assemble fortran *)
(* entry to determine distances and x variables *)
xstart2="
      subroutine get_x(xyz,x)
!   xyz must be in Bohr
      implicit none
      integer :: i,j,k
        real(wp) :: xyz(12z,3)
        real(wp),dimension(1:21)::x

";
If[diagnose,Print["Calculating Tijnums"];];
Tijnums=GenerateTijnums[];
linedist=Table["",{i,1,Length[Tijnums]},{j,1,3}];
Do[(
linedist[[i,1]]=
	"       x( "<>ToString[i]<>" ) = sqrt((xyz( "<>ToString[Tijnums[[i,1]]]<>
	" , 1 )-xyz( "<>ToString[Tijnums[[i,2]]]<>" , 1 ))**2+ &\n";
linedist[[i,2]]="             (xyz("<>ToString[Tijnums[[i,1]]]<>
	",2)-xyz("<>ToString[Tijnums[[i,2]]]<>",2))**2+ &\n";
linedist[[i,3]]="             (xyz("<>ToString[Tijnums[[i,1]]]<>
	",3)-xyz("<>ToString[Tijnums[[i,2]]]<>",3))**2)\n";
xstart2=xstart2<>linedist[[i,1]]<>linedist[[i,2]]<>linedist[[i,3]];
),{i,1,Length[Tijnums]}];
xstart2=xstart2<>"\n\n";
If[diagnose,Print["Done Tijnums"];];
(*Do[(
xstart2=xstart2<>"          x("<>ToString[i]<>")=x("<>ToString[i]<>")/0.5291772083_dp\n";
xstart2=xstart2<>"          x("<>ToString[i]<>")=exp(-x("<>ToString[i]<>")/a1)\n";
),{i,1,Length[Tijnums]}];
*)
(*
xstart2=xstart2<>"     x(:) = x(:)/a \n";
xstart2=xstart2<>"     x(:) = dexp (-x(:)) \n";
*)
xform="";
Do[(
If[xtransform[[i]]==1,
	xform=xform<>"    x("<>ToString[i]<>") = dexp(-x("<>ToString[i]<>")/a) \n";
	,
	xform=xform<>"    x("<>ToString[i]<>") = 1.d0/x("<>ToString[i]<>") \n";
];
),{i,1,nvariables}];
xstart2=xstart2<>xform<>"     end subroutine\n\n\n";

newfortran=CommentXAssignments<>xstart1<>xstart2<>xstart3;

(* convert CharStringmono to lines of text *)
If[diagnose,Print["convert CharStringmono to lines of text"];];
Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
CSMlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	Label[AnotherRoundM];
	If[Length[Characters[CSMlines[[i]]]]<= 60,
		newfortran=newfortran<>CSMlines[[i]]<>"\n";
		,
		CSMlinesch=Characters[CSMlines[[i]]];
		Do[(
			If[Or[CSMlinesch[[61-j]]=="-",CSMlinesch[[61-j]]=="+"],
				jsave=j;
				Goto[outM];
			];	
		),{j,1,60}];
		Print["Fortran output mono line longer than 60 char w/o + or -"];
		Print["i = ",i," in CharStringmono conversion"];
		Print["CSMlinesch = ",CSMlinesch];
		Abort[];
		Label[outM];
		CSMappend=StringJoin[Take[CSMlinesch,61-jsave+1]]<>" &\n";
		CSMremaining="         "<>StringJoin[Drop[CSMlinesch,61-jsave+1]];
		newfortran=newfortran<>CSMappend;
		CSMlines[[i]]=CSMremaining;
		Goto[AnotherRoundM];		
	];
),{i,1,Length[CSMlines]}];
newfortran=newfortran<>xmiddle;

(* convert CharStringpoly to lines of text *)
If[diagnose,Print["convert CharStringpoly to lines of text"];];
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
CSPlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
(*Print[i];
Print["      ",newfortran];*)
	Label[AnotherRoundP];
	If[Length[Characters[CSPlines[[i]]]]<= 60,
		newfortran=newfortran<>CSPlines[[i]]<>"\n";
		,
	(*Print["Long Line =",CSPlines[[i]]];*)
		CSPlinesch=Characters[CSPlines[[i]]];
		Do[(
			If[Or[CSPlinesch[[61-j]]=="-",CSPlinesch[[61-j]]=="+"],
		(*Print["            Found +/-  ",j];*)
				jsave=j;
				Goto[outP];
			];	
		),{j,1,60}];
		Print["Fortran output poly line longer than 60 char w/o + or -"];
		Print["i = ",i," in CharStringpoly conversion"];
		Print["CSPlinesch = ",CSPlinesch];
		Abort[];
		Label[outP];
		CSPappend=StringJoin[Take[CSPlinesch,60-jsave+1]]<>" &\n";
		CSPremaining="          "<>StringJoin[Drop[CSPlinesch,60-jsave+1]];
		newfortran=newfortran<>CSPappend;
		CSPlines[[i]]=CSPremaining;
		Goto[AnotherRoundP];		
	];
),{i,1,Length[CSPlines]}];

newfortran=newfortran<>xending;

(* fix dimensions *)
If[diagnose,Print["fix dimensions"];];
nvar=StringCount[CommentXAssignments,"="];
nmn=StringCount[CharStringmono,"="]-1;
npol=StringCount[CharStringpoly,"="]-1;
(*Print["{nvar,nmn,npol} = ",{nvar,nmn,npol}];*)
z=StringReplace[newfortran,"1:21"-> "1:"<>ToString[nvar]];
z=StringReplace[z,"0:239"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:619"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:578"->  "0:"<>ToString[npol]];
z=StringReplace[z,"0:3027"->  "0:"<>ToString[npol]];
z=StringReplace[z,":: xyz(9,3)"->
	 ":: xyz("<>ToString[natomsparent]<>",3)"];
z=StringReplace[z,"12z,3"-> ToString[natomsparent]<>",3"];
z=StringReplace[z,"1:21z"-> "1:"<>ToString[nvar]];
z=StringReplace[z,":: coeff(3096)"->
	 ":: coeff("<>ToString[npol]<>")"];
z=StringReplace[z,":: x(36)"-> ":: x("<>ToString[nvar]<>")"];
z=StringReplace[z,"do i = 1, 3096"->
	 "do i = 1, "<>ToString[npol]<>""];


Export[fortranname,z,"Text"];
z
];



FixXAssignmentsInMonoList[CharStringmono_]:=Module[
{Axold,Bxold,Txold,xx,xxx,NumberCorrelations,xxyy},

(* make correspondence table for final assignments *)
Axold=GenerateFortranCommentWithXAssignments[];
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
Txold=Table[{ToExpression[StringDrop[StringDrop[Bxold[[i,2]],2],-1]],Bxold[[i,4]]},
	{i,1,Length[Bxold]}];
(* make correspondence table for initial assignments *)
xx={};
Do[(xx=Join[xx,rijnames[[ifrag]]]),{ifrag,1,nfragments}];
xxx=Table[{i,ToExpression[xx[[i]]]},{i,1,Length[xx]}];
(* combine tables to get numerical correlations = {initial,final} *)
NumberCorrelations={};
Do[(
	Do[(
		If[xxx[[j,2]]==Txold[[i,2]],
			NumberCorrelations=Append[NumberCorrelations,{xxx[[j,1]],Txold[[i,1]]}];
		];
	),{j,1,Length[xxx]}];
),{i,1,Length [Txold]}];
(* fix CharStringmono *)
(*Print[NumberCorrelations];*)
xxyy=CharStringmono;
Do[(
	If[NumberCorrelations[[i,1]]!=NumberCorrelations[[i,2]],
	xxyy=StringReplace[xxyy,"x("<>ToString[NumberCorrelations[[i,1]]]<>")"-> 
		"x("<>ToString[NumberCorrelations[[i,2]]]<>")"];
	];
),{i,1,Length[NumberCorrelations]}];

xxyy
];



GenerateFortranCommentWithXAssignments[]:=Module[
{xx,xxx,xxxdel,xxxold,xxxnew,xxxx,xxxxx,diagnose,num},
diagnose=False;

xx={};
Do[(xx=Join[xx,rijnames[[ifrag]]]),{ifrag,1,nfragments}];
xxx=Table[{"!     x("<>ToString[i]<>") = ",xx[[i]]},{i,1,Length[xx]}];
If[diagnose,PLHxxx=xxx;];

xxxdel={};
Do[(
Do[(
If[xxx[[j,2]]== xxx[[i,2]],xxxdel=Append[xxxdel,j];];
),{j,i+1,Length[xxx]}];
),{i,1,Length[xxx]}];
xxxdel=Sort[DeleteDuplicates[xxxdel]];
If[diagnose,PLHxxxdel=xxxdel;];

xxxold=xxx;
Do[(
xxxnew =Drop[xxxold,{xxxdel[[Length[xxxdel]+1-i]]}];
xxxold=xxxnew;
),{i,1,Length[xxxdel]}];
xxxx=xxxold;
If[diagnose,PLHxxxx=xxxx];

(* renumber x's if needed*)
Do[(
num=ToExpression[StringDrop[StringDrop[xxxx[[i,1]],8],-4]];
If[num!=i,
xxxx[[i,1]]=StringTake[xxxx[[i,1]],8]<>ToString[i]<>StringTake[StringDrop[xxxx[[i,1]],8],-4]];
),{i,1,Length[xxxx]}];
If[diagnose,PLHxxxxrenum=xxxx];

xxxxx="";
Do[(
xxxxx=xxxxx<>StringJoin[xxxx[[i,1]],xxxx[[i,2]],"\n"];
),{i,1,Length[xxxx]}];
If[diagnose,PLHxxxxx=xxxxx;];

xxxxx
];



GenerateTijnums[]:=Module[
{xx,xxx,xxxdel,xxxold,xxxnew,xxxx,Tijnums},
xx={};
Do[(xx=Join[xx,rijnames[[ifrag]]]),{ifrag,1,nfragments}];
xxx=Table[{"!     x("<>ToString[i]<>") = ",xx[[i]]},{i,1,Length[xx]}];

xxxdel={};
Do[(
Do[(
If[xxx[[j,2]]== xxx[[i,2]],xxxdel=Append[xxxdel,j];];
),{j,i+1,Length[xxx]}];
),{i,1,Length[xxx]}];
xxxdel=Sort[DeleteDuplicates[xxxdel]];

xxxold=xxx;
Do[(
xxxnew =Drop[xxxold,{xxxdel[[Length[xxxdel]+1-i]]}];
xxxold=xxxnew;
),{i,1,Length[xxxdel]}];
xxxx=xxxold;

Tijnums=Table[{0,0},{i,1,Length[xxxx]}];
Do[(
g=xxxx[[i,2]];
h1=StringDrop[g,-2];
h2=StringTake[g,-2];
If[StringTake[h1,1]=="0",h1=StringDrop[h1,1];];
If[StringTake[h2,1]=="0",h2=StringDrop[h2,1];];
Tijnums[[i,1]]=ToExpression[h1];
Tijnums[[i,2]]=ToExpression[h2];
),{i,1,Length[xxxx]}];


Tijnums
];



DeleteAllFortranDefinitionsStartingd[CharString_]:=Module[
{PPCharStringpoly1,PPDatapoly1,PPDatapolyX,xnew},
PPCharStringpoly1=CharString;
(*PPDatapoly1=ImportString[PPCharStringpoly1,{"Text","Data"}]; *)
PPDatapoly1=ImportString[PPCharStringpoly1,"Table"];
If[PPDatapoly1[[1]]=={},PPDatapoly1=Drop[PPDatapoly1,1];];
If[PPDatapoly1[[1,1]]=="m(0)" && PPDatapoly1[[1,2]]=="=",
	PPDatapoly1[[1]]={"m(0)","=","1.0D0"}];
If[PPDatapoly1[[1,1]]=="p(0)" && PPDatapoly1[[1,2]]=="=",
	PPDatapoly1[[1]]={"p(0)","=","m(0)"}];  (* 11/3/2022 changed this to m(0) from 0.0D0 *)
Do[(
If[StringContainsQ[PPDatapoly1[[i,1]],"d("],
PPDatapoly1=Drop[PPDatapoly1,{i}];
];
),{i,Length[PPDatapoly1],1,-1}];
(* reassemble the lines of text by joining the remaining terms in each line *)
PPDatapolyX=PPDatapoly1;

Do[(
	xnew=StringJoin[PPDatapolyX[[i]]];  
	xnew="    "<>xnew;  (* PPCharStringpoly needs to have 4 leading spaces *)
	PPDatapolyX[[i]]={xnew};
),{i,1,Length[PPDatapolyX]}];
PPCharStringpoly1=ExportString[Flatten[PPDatapolyX],{"Text","Lines"}];
PPCharStringpoly1=StringReplace[PPCharStringpoly1,"+"->" + "];
PPCharStringpoly1=StringReplace[PPCharStringpoly1,"-"->" - "];
PPCharStringpoly1=StringReplace[PPCharStringpoly1,"="->" = "];
PPCharStringpoly1
];



DeleteAllFortranDefinitionsStartingdOld[CharString_]:=Module[
{csp,droplist,listout,dloc,nextn,prevn,eqfound,nexti},
csp=Characters[CharString];
droplist={};
Do[(
	idyn=Length[csp]-i; (* global for dynamic *)
	If[csp[[i]]== "d",
		dloc=i;
		(*Print["dloc = ",dloc];*)
		eqfound=False;
		Do[(
			nextn=j;
			(*Print["csp[[j]] = ",csp[[j]]];*)
			If[csp[[nextn]]=="=",eqfound=True;];
			If[csp[[nextn]]=="\n",Break[];];
		),{j,dloc+1,Min[dloc+1000,Length[csp]]}];
		(*Print["Break or end happened in first loop, nextn = ",nextn];*)
		If[!eqfound,Goto[nexti];];
		Do[(
			prevn=dloc-j;
			(*Print["csp[[j]] = ",csp[[j]]];*)
			If[csp[prevn]=="=",Goto[nexti];];
			If[csp[[prevn]]=="\n",Break[];];
		),{j,1,Min[1000,dloc-1]}];
		(*Print["Break or end happened in second loop, prevn = ",prevn];*)	
		Do[(
			droplist=Append[droplist,j];
		),{j,prevn+1,nextn}];
	];
	Label[nexti];
),{i,1,Length[csp]}];

Do[(
idyn=Length[droplist]-i;
csp=Drop[csp,{droplist[[Length[droplist]+1-i]]}];
(*Print["Dropped ",droplist[[Length[droplist]+1-i]]];*)
),{i,1,Length[droplist]}];

listout=StringJoin[csp]
];



RenumberFortranExistingmInMonoAndPoly::usage="RenumberFortranExistingmInMonoAndPoly[CharListmono,CharListpoly]
Takes all the lines beginning m(i)= in StringList
renumbers it and all the m(i) throughout in consecutive
numerical order
";
RenumberFortranExistingmInMonoAndPoly[CharListmono_,CharListpoly_]:=Module[
{Axold,Axnew,Bxold,Temppoly},
Axold=CharListmono;
Temppoly=CharListpoly;
(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
PLHAxold=Axold;
PLHBxold=Bxold;
PLHCLP=CharListpoly;
(* replace m(i) if needed throughout monomials so as to have numerical order *)
(* Bxold (table format) is used for the criterion, but Axold is what is changed *)
(* the i+1 in Bxold is due to the zero-order term there *)
(* In addition, we want the monomial numbering in the polynomials to be the same as in the
monomials, so every time we change something in the monomials, we do a change of those
monomial numbers in the polynomials *)
Axnew=Axold;
Do[(
idyn=Length[Bxold]-1-i;
If["m("<>ToString[i]<>")"!= Bxold[[i+1,1]],
Axnew=StringReplace[Axold,Bxold[[i+1,1]]->  
		"m("<>ToString[i]<>")"];
Temppoly=StringReplace[Temppoly,Bxold[[i+1,1]]->  
		"m("<>ToString[i]<>")"];
Axold=Axnew;
];
),{i,1,Length[Bxold]-1}];

{Axold,Temppoly}
];





RenumberFortranExistingp::usage="RenumberExistingp[StringList_]
Takes all the lines beginning p(X)= in StringList
renumbers it and all the p(X) throughout in consecutive
numerical order
";
RenumberFortranExistingp[StringList_]:=Module[
{Axold,Axnew,Bxold},
Axold=StringList;

(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];

(* replace p(i) if needed throughout so as to have numerical order *)
(* Bxold (table format) is used for the criterion, but Axold is what is changed *)
(* the i+1 in Bxold is due to the zero-order term there *)
Axnew=Axold;
Bxold=Drop[Bxold,1]; (* get rid of p(0) term *)

(*Print["Length[Bxold] = ",Length[Bxold]];*)
Do[(
	idyn=Length[Bxold]-i;
	(*Print[{i,Bxold[[i,1]]}];*)
	If["p("<>ToString[i]<>")"!= Bxold[[i,1]],
		(*Print[Bxold[[i,1]]," replaced by p(",i,")"];*)
		Axnew=StringReplace[Axold,Bxold[[i,1]]-> 
			"p("<>ToString[i]<>")"];
		Axold=Axnew;
	];
),{i,1,Length[Bxold]}];
(*Print[Bxold[[Length[Bxold]]]];
Print[Bxold[[Length[Bxold],1]]," being replaced by p(",Length[Bxold],")"];*)
Axold=Axnew;
(*Print["Exiting RenumberFortranExistingp"];*)
(*
(* Convert StringList from Text to Lines format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxoldln=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Bxnewln=Bxoldln;
(*Print["Length of Bxoldln = ",Length[Bxoldln]];
Print["Length of Bxold = ",Length[Bxold]];*)
Do[(
	If["p("<>ToString[i]<>")"!= Bxold[[i+1,1]],
	(*Print[Bxold[[i+1,1]]," replaced by p(",i,")"];*)
		Do[(
			Bxnewln[[j]]=StringReplace[Bxnewln[[j]],Bxold[[i+1,1]]\[Rule]  
				"p("<>ToString[i]<>")"];
		),{j,Length[Bxnewln],i,-1}];
	];
),{i,1,Length[Bxold]-1}];
(* Convert StringList from Lines to Text format *)
Export[DataDir<>"DeleteMe.txt",Bxnewln,{"Text","Lines"}];
Bxnew=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
(*Print["Length of Bxnew = ",StringCount[Bxnew,"="]];*)
Bxnew*)
Axold
];





SortIndividualInteratomicDistanceNames[]:=Module[
{namea,nameb,namelist},
Do[(
namelist=rijnames[[ifrag]]; 
(*Print[namelist];*)
	Do[(
		namea=StringDrop[namelist[[jname]],-2];
		nameb=StringDrop[namelist[[jname]],2];
		If[ToExpression[namea]>ToExpression[nameb],
			namelist[[jname]]=nameb<>namea;
		];
	),{jname,1,Length[namelist]}];
	rijnames[[ifrag]]=namelist;
),{ifrag,1,nfragments}];

];



RenumberFortranExistingx::usage="RenumberExistingp[StringList_]
Takes all the lines beginning m(i)= in StringList
renumbers it and all the m(i) throughout in consecutive
numerical order
";
RenumberFortranExistingx[StringList_]:=Module[
{Axold,Axnew,Bxold},
Axold=StringList;
(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
(*Print[Bxold];*)
Axnew=Axold;
Do[(
If["x("<>ToString[i]<>")"!= Bxold[[i,2]],
Axnew=StringReplace[Axold,Bxold[[i,2]]-> 
		"x("<>ToString[i]<>")"];
Axold=Axnew;
];
),{i,1,Length[Bxold]}];

Axold
];






RenumberFortranNonsequentialx::usage="RenumberExistingp[...]
Takes all the lines beginning m(i)= in StringList
renumbers it and all the m(i) throughout in consecutive
numerical order
";
RenumberFortranNonsequentialx[StringList_,xsum_,nvariablesa_,LTdupsab_]:=Module[
{Axold,Axnew,xnew,xmaxinsequence,xnext},
Axold=StringList;
xnew=xsum+nvariablesa-LTdupsab;
xmaxinsequence=0;
(*Print[{xsum,nvariablesa,LTdupsab}];*)
Do[(
	If[!StringContainsQ[StringList,"x("<>ToString[i]<>")"],
		(*Print["i = ",i,"  True"];*)
		xmaxinsequence=i-1;
		Break[];
	];
),{i,1,xsum+nvariablesa}];
xnext=xmaxinsequence;
(*Print["xnext = ",xnext];*)
Do[(
	(*Print["i = ",i];*)
	If[StringContainsQ[Axold,"x("<>ToString[i]<>")"],
		(*Print["True"];*)
		xnext=xnext+1;
		(*Print[{i,xnext}];*)
		Axnew=StringReplace[Axold,"x("<>ToString[i]<>")"-> "x("<>ToString[xnext]<>")"];
		Axold=Axnew;
	];
),{i,xmaxinsequence+1,xsum+nvariablesa}];

Axold
];






RunTests[]:=Module[
{ifrag1,monostart1,monoend1,polystart1,polyend1,CharStringmono1,CharStringpoly1,
npoly1,nmono1,xsum,rijnamesnow,npoly,nmono,nvariables},


Print[];
Print[];
Print["Running Tests"];


(* **************** Get Fortran Output Definitions   *******************  *)
(* get mono poly fortran lists from pruned fortran file *)
(*{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];*) (*
commented out on 20 Feb 2023 because it is already called in TestForRemainingDuplicates *)

(* **************** Test for remaining duplicates   *******************  *)
TestForRemainingDuplicates[];

(* **************** Test for permutational invariance   *******************  *)
TestPermInv[];

(* **************** Test time for basis set evaluation   *******************  *)
(* check timing of basis set evaluation  *)
xsum=Length[rijnamesnow];
timesum=0;
n=5;
Do[(
	Do[(
		dist[[i]]=RandomReal[{0.1,1}];
	),{i,1,xsum}];
	tim=Timing[
		Assignx[];
		EM=IntegerPart[EvalMono[]];
		EP=IntegerPart[EvalPoly[]];
		][[1]];
	timesum=timesum+tim;
),{i,1,n}];
timeav=timesum/n;
Print["Average time in sec. for basis set evaluation = ",timeav];
Print["Tests have been completed"];
(* no output *)
];




rijcombos[list_]:=Module[
{combos},
combos={};
Do[(
Do[(

combos=Append[combos,{list[[i]],list[[j]]}];
),{j,i+1,Length[list]}];
),{i,1,Length[list]}];
DeleteDuplicates[combos]
];



MakerijnamesAndnatomsfrag[]:=Module[
{combos,x,y},
Do[(
natomsfrag[[ifrag]]=Length[atoms[[ifrag]]];
combos=rijcombos[atoms[[ifrag]]];
rijnames[[ifrag]]={};
Do[(
x=ToString[combos[[i,1]]];
If[Length[Characters[x]]==1,x="0"<>ToString[combos[[i,1]]];];
y=ToString[combos[[i,2]]];
If[Length[Characters[y]]==1,y="0"<>ToString[combos[[i,2]]];];
rijnames[[ifrag]]=Append[rijnames[[ifrag]],x<>y];
),{i,1,Length[combos]}];
),{ifrag,1,nfragments}];

(* no output *)
];




MakePermTables[]:=Module[
{nexti,permtable,y,z,nadd},

(*permtable={{}};*)
permtable=Table[{},{i,1,nfragments}];
Do[( 
nadd=0;
(*permtable[[ifrag]]={};*)
Do[(
If[permsym[[ifrag,i]]==1,
y=1;
Goto[nexti];
];
y=permsym[[ifrag,i]];
(*Print["{i,y} = ",{i,y}];*)
z={};
Do[(
z=Append[z,atoms[[ifrag,j+nadd]]];
),{j,1,y}];
(*Print["z = ",z];*)
permtable[[ifrag]]=Append[permtable[[ifrag]],z];
Label[nexti];
nadd=nadd+y;
),{i,1,Length[permsym[[ifrag]]]}];
If[permtable[[ifrag]]!={},
	permtable=Append[permtable,permtable[[ifrag]]];
];
 ),{ifrag,1,nfragments}]; 

DeleteDuplicates[Flatten[DeleteCases[permtable,{}],1]]
];


ExtractrijnamesFrombemsaTabc[]:=Module[
{xstart,xend,rijnamesnow,c1,c2},

Do[(
If[StringContainsQ[bemsaTxtLnc[[i]],"x( 1 ) = sqrt"],
xstart=i;
Break[];
];
),{i,1,Length[bemsaTxtLnc]}];
(*xstart;*)
Do[(
If[!StringContainsQ[bemsaTxtLnc[[i]],"sqrt(("],
xend=i;
Break[];
];
),{i,xstart,Length[bemsaTxtLnc]}];
xend=xend-1;
rijnamesnow={};
Do[(
c1=ToString[bemsaTabc[[i,6]]];
c2=ToString[bemsaTabc[[i,10]]];
If[Length[Characters[c1]]==1,c1="0"<>c1];
If[Length[Characters[c2]]==1,c2="0"<>c2];
rijnamesnow=Append[rijnamesnow,c1<>c2];
),{i,xstart,xend}];

rijnamesnow
];



TestPermInvOld[]:=Module[
{permtable,ifraga,natomsa,rijnamesa,permdone,failures,rijnamesnew,
zq1,zq2,rijc,zqtemp,namelist,namea,nameb,EMnew,EPnew,success,xsum,
EM,EP,rijnamesnow,nvariables,nmono,npoly
},
(* assumes Assignx, EvalMono and EvalPoly have already been defined *)
(* get lists of permutable atoms *)

success=False;
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
xsum=Length[rijnamesnow];
Print["xsum = ",xsum];
permtable=MakePermTables[];
If[permtable=={},Print["There are no permutable atoms"];Goto[permdone];];
Print["Testing permutations of these atoms: ",permtable];
(*GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];*)
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
GetAssignEVMonoPoly[ifraga,natomsa_,rijnamesa];
(* evaluate EM and EP *)


dist=Table[0,{i,1,xsum}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,xsum}];
Assignx[];
EM=IntegerPart[10^10*EvalMono[]];
EP=IntegerPart[10^10*EvalPoly[]];
xb=Assignx[];
(*Print["Length of xb = ",Length[xb]];*)

(* check that monos and polys are the same for any pairwise permutation *)
failures={};
Do[( (* over permtable atoms *)
rijc=rijcombos[permtable[[i]]]; (* has pairs of permutable atoms *)
(*Print["rijcombos = ",rijc];*)
	Do[(  (* over rijc elements *)
		(*Print["rijnamesnow = ",rijnamesnow];*)
		rijnamesnew=rijnamesnow;
		zq1=If[Length[Characters[ToString[rijc[[j,1]]]]]==2,
			ToString[rijc[[j,1]]],"0"<>ToString[rijc[[j,1]]]];
		zq2=If[Length[Characters[ToString[rijc[[j,2]]]]]==2,
			ToString[rijc[[j,2]]],"0"<>ToString[rijc[[j,2]]]];
		zqtemp="xx";
		(*Print[{zq1,zq2,zqtemp}];*)
			(* interchange permutable atoms in rijnamesnew elements*)
			(* introduce hyphens *)
			namelist=rijnamesnew;
			Do[(
				namea=StringDrop[namelist[[jname]],-2];
				nameb=StringDrop[namelist[[jname]],2];
				namelist[[jname]]=namea<>"-"<>nameb;
			),{jname,1,Length[namelist]}];
			rijnamesnew=namelist;
			(*Print["rijnamesnew = ",rijnamesnew];*)
			(* permute *)
			rijnamesnew=StringReplace[rijnamesnew,zq1 -> zqtemp];
			rijnamesnew=StringReplace[rijnamesnew,zq2 -> zq1];
			rijnamesnew=StringReplace[rijnamesnew,zqtemp -> zq2];
			(* get rid of hyphens *)
			namelist=rijnamesnew;
			Do[(
				namea=StringDrop[namelist[[jname]],-3];
				nameb=StringDrop[namelist[[jname]],3];
				namelist[[jname]]=namea<>nameb;
			),{jname,1,Length[namelist]}];
			rijnamesnew=namelist;

			(* sort rijnamesnew to standard order, eg 0112 rather than 1201 *)
			(*Print["rijnamesnew = ",rijnamesnew];*)
			namelist=rijnamesnew;
			Do[(
				namea=StringDrop[namelist[[jname]],-2];
				nameb=StringDrop[namelist[[jname]],2];
				If[ToExpression[namea]>ToExpression[nameb],
					namelist[[jname]]=nameb<>namea;
				];
			),{jname,1,Length[namelist]}];
			rijnamesnew=namelist;
			
			(*Print["rijnamesnew = ",rijnamesnew];*)
			(*Print["{i,j} =",{i,j}];*)
			(* the xb assignments are in the order of rijnamesnow *)
			(* we want to switch around the x assignments so that, for example,
			the x1 is now rijnamesnew[[1]] rather than rijnamesnow[[1]] *)
			Assignx[];
			(*
			Print["Length of xb = ",Length[xb]];
			Print["Length of Assignx = ",Length[Assignx[]]];
			Print["Length of rijnamesnew = ",Length[rijnamesnew]];
			Print["xb = ",xb];
			Print["Assignx = ",Assignx[]];
			*)
			Do[(
				(*Print["x"<>ToString[m]<>" = xb[["<>
					ToString[Position[rijnamesnow,rijnamesnew[[m]]][[1,1]]]<>"]];      ",
					{ToExpression["x"<>ToString[m]],ToExpression["xb[["<>
					ToString[Position[rijnamesnow,rijnamesnew[[m]]][[1,1]]]<>"]]"]}];*)
				ToExpression["x"<>ToString[m]<>" = xb[["<>
					ToString[Position[rijnamesnow,rijnamesnew[[m]]][[1,1]]]<>"]];"];
			),{m,1,Length[rijnamesnew]}];
			
			EMnew=IntegerPart[10^10*EvalMono[]];
			EPnew=IntegerPart[10^10*EvalPoly[]];
			If[Sum[EM[[i]],{i,1,Length[EM]}]== Sum[EMnew[[i]],{i,1,Length[EMnew]}] &&
			   Sum[EP[[i]],{i,1,Length[EP]}]== Sum[EPnew[[i]],{i,1,Length[EPnew]}],
			    success=True;
			    ,
				success=False; 
				(*failures=Append[failures,{i,j}];*)
				failures=Append[failures,rijcombos[permtable[[i]]][[j]]];
				(*Print[{i,j}]; 
				Print[Sum[EM[[i]],{i,1,Length[EM]}]\[Equal] Sum[EMnew[[i]],{i,1,Length[EMnew]}]];
				Print[Sum[EP[[i]],{i,1,Length[EP]}]\[Equal] Sum[EPnew[[i]],{i,1,Length[EPnew]}]];
				Abort[];*)
			];		
	),{j,1,Length[rijc]}];
),{i,2,Length[permtable]}];
If[success,
	Print["Fortran output passed all permutation tests"];
	,
	Print["Fortran output failed at least one permutation test"];
	Print["failures = ",failures];
];


Label[permdone];

success
]



TestPermInv[]:=Module[
{permtable,ifraga,natomsa,rijnamesa,permdone,failures,rijnamesnew,
zq1,zq2,rijc,zqtemp,namelist,namea,nameb,success,xsum,
EM,EP,EMnew,EPnew,rijnamesnow,nvariables,nmono,npoly,xyzorig,
xyztemp,Perrors,DetailedDiagnosis
},
(* assumes Assignx, EvalMono and EvalPoly have already been defined *)
(* get lists of permutable atoms *)
DetailedDiagnosis=False;
success=True;
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)
permtable=MakePermTables[];
If[permtable=={},Print["There are no permutable atoms"];Goto[permdone];];
Print["Testing permutations of these atoms: ",permtable];
(*GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];*)
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
GetAssignEVMonoPoly[ifraga,natomsa_,rijnamesa];

(* evaluate EM and EP *)
(* assign x from xyz *)
xyz=Table[RandomReal[{2,4}],{i,1,natomsparent},{j,1,3}];  (* global *)
xyzorig=xyz;
Assignxfromxyz[];
EM=IntegerPart[10^10*EvalMono[]];
EP=IntegerPart[10^10*EvalPoly[]];
(*xb=Assignx[];*)
(*Print["Length of xb = ",Length[xb]];*)
(*PLHEM=EM;*)
(* check that monos and polys are the same for any pairwise permutation *)
failures={};
Do[( (* over permtable elements *)
rijc=rijcombos[permtable[[i]]]; (* has pairs of permutable atoms *)
If[DetailedDiagnosis,Print["rijcombos = ",rijc]];
	Do[(  (* over rijc elements *)
			xyz=xyzorig;
			xyztemp=xyzorig[[rijc[[j,1]]]];
			xyz[[rijc[[j,1]]]]=xyzorig[[rijc[[j,2]]]];
			xyz[[rijc[[j,2]]]]=xyztemp;
			Assignxfromxyz[];
			EMnew=IntegerPart[10^10*EvalMono[]];
			EPnew=IntegerPart[10^10*EvalPoly[]];
			Perrors={};
			Do[(
				If[N[Abs[EP[[k]]- EPnew[[k]]]/Abs[EP[[k]]]]>10^-10,
					Perrors=Append[Perrors,k];
					(*If[DetailedDiagnosis,*)
						Print["{{atom1,atom2},k,EP[[k]],EPnew[[k]]} =
						 ",{rijcombos[permtable[[i]]][[j]],k,EP[[k]],EPnew[[k]]}];
					(*];*)
				];
			),{k,1,Length[EP]}];
			(*Perrors;*)
			If[Sum[EM[[i]],{i,1,Length[EM]}]== 
				Sum[EMnew[[i]],{i,1,Length[EMnew]}] && Length[Perrors]==0,
				success=success;  (* no change *)
				,
				success=False; 
				failures=Append[failures,rijcombos[permtable[[i]]][[j]]];
				Print["Summary for combination ",rijcombos[permtable[[i]]][[j]]];
				If[DetailedDiagnosis,
					Print["xyzorig = ",xyzorig];
					Print["xyz now = ", xyz];
					Print["Perrors = ",Perrors];
				];
				If[Sum[EM[[i]],{i,1,Length[EM]}]== Sum[EMnew[[i]],{i,1,Length[EMnew]}],
					Print["monos: passed"];
					,
					Print["monos: ",
					{Sum[EM[[i]],{i,1,Length[EM]}], Sum[EMnew[[i]],{i,1,Length[EMnew]}]}];
				];
				Print["polys: Length of Perrors = ",Length[Perrors]];
				(*Abort[];*)
				Print[" -----------------------"];
				Print[""];
				Print[""];
			];		
	),{j,1,Length[rijc]}];
),{i,1,Length[permtable]}];
If[success,
	Print["Fortran output passed all permutation tests"];
	,
	Print["Fortran output failed at least one permutation test"];
	Print["failures in the following permutations: ",failures];
];


Label[permdone];

success
]



TestForRemainingDuplicatesOld[]:=Module[
{success,xb,positions,Duplicates,xsum,rijnamesnow,
nvariables,nmono,npoly,EM,EP,diagnose
},
diagnose=False;
(* Check for Duplicate values, given random assignments *)
success=False;

{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[]; (*
GetDefinitionstoMathematicaFromFortanOutput makes PPCharStringmono and PPCharStringpoly
as global variables *)
If[diagnose,Print["{rijnamesnow,nvariables,nmono,npoly} = ",
           {rijnamesnow,nvariables,nmono,npoly}];];
(* added 12/4/2021 to address Tag Times problem *)
CharStringmono=PPCharStringmono;
CharStringpoly=PPCharStringpoly;
If[diagnose,PLHCSMTestDups=CharStringmono;PLHCSPTestDups=CharStringpoly;];
CharStringmono=StringReplace[CharStringmono,"D0"->""];
CharStringmono=StringReplace[CharStringmono,"d0"->""];
CharStringpoly=StringReplace[CharStringpoly,"D0"->""];
CharStringpoly=StringReplace[CharStringpoly,"d0"->""];
PPCharStringmono=CharStringmono;
PPCharStringpoly=CharStringpoly;
If[diagnose, PLHPPCSP543=PPCharStringpoly;PLHPPCSM543=PPCharStringmono;];

(*Print["rijnamesnow = ",rijnamesnow];
Print["{nvariables,nmono,npoly} = ",{nvariables,nmono,npoly}];*)
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)

(* mlist,plist, and xlist are defined in GetDefinitionstoMathematicaFromFortranOutput *)
CreateEvalMono[PPCharStringmono]; 
CreateEvalPoly[PPCharStringpoly]; 


dist=Table[0,{i,1,xsum}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,xsum}];
Assignx[];
EM=IntegerPart[10^10*EvalMono[]];
EP=IntegerPart[10^10*EvalPoly[]];
xb=Assignx[];
(* xb now has the assigned xvalues  *)
(*Print["EM = ",EM];*)
(*PLHEM=EM;
PLHEP=EP;
Abort[];*)

If[Length[EM]!=Length[DeleteDuplicates[EM]],
  Print["There are ",Length[EM]-Length[DeleteDuplicates[EM]]," mono duplicates."];
  Duplicates={};
  Do[(
	positions=Position[EM,EM[[i]]][[All,1]];
  (*Print[positions,Length[positions]];*)
	Do[(
		If[Length[positions]>1,Duplicates=Append[Duplicates,positions];]
	),{j,1,Length[positions]}];
  ),{i,1,Length[EM]}];   
  Duplicates=DeleteDuplicates[Duplicates];
  If[diagnose,PLHmonoduplilcates=Duplicates;];

  (*Print["Duplicates = ",Duplicates];*)
  If[Duplicates!={},
    Duplicates=Duplicates-1;  (* this is because the index in the previous loop starts at 1,
	      but the lowest polynomial is m(0), changed 13-Feb-2023 *)
	Print["Fortran output failed uniqueness of monomials ",Length[Duplicates]," times"];
	Print["Duplicate locations in monomials are: ",Duplicates];
	(*Print[Table[{i,EM[[i]]},{i,1,Length[EM]}]];*)
	,
	success=True;
  ];
  If[success,Print["Fortran ouptut passed uniqueness of monomials"];];
  ,
  success=True;
  Print["Fortran ouptut passed uniqueness of monomials"];
  ];

  If[Length[EP]!=Length[DeleteDuplicates[EP]],
  Print["There are ",Length[EP]-Length[DeleteDuplicates[EP]]," poly duplicates."];
  success=False;
  Duplicates={};
  Do[(
	positions=Position[EP,EP[[i]]][[All,1]];
  (*Print[positions,Length[positions]];*)
	Do[(
		If[Length[positions]>1,Duplicates=Append[Duplicates,positions];]
	),{j,1,Length[positions]}];
  ),{i,1,Length[EP]}];  
  Duplicates=DeleteDuplicates[Duplicates];
  If[diagnose,PLHpolyduplilcates=Duplicates;];

  If[Duplicates!={},
    Duplicates=Duplicates-1;  (* this is because the index in the previous loop starts at 1,
	      but the lowest polynomial is p(0), changed 13-Feb-2023 *)
	Print["Fortran output failed uniqueness of polynomials ",Length[Duplicates]," times"];
	Print["Duplicate locations in polynomials are: ",Duplicates];
	Do[(
	Print["{Duplicates[[nd,1]],EP[[Duplicates[[nd,1]]]]} = ",{Duplicates[[nd,1]],EP[[Duplicates[[nd,1]]]]}];
	Print["{Duplicates[[nd,2]],EP[[Duplicates[[nd,2]]]]} = ",{Duplicates[[nd,2]],EP[[Duplicates[[nd,2]]]]}];
	),{nd,1,Length[Duplicates]}];
	,
	success=True;
  ];
  If[success,Print["Fortran ouptut passed uniqueness of polynomials"];];
  ,
  success=True;
  Print["Fortran ouptut passed uniqueness of polynomials"]
  ];

success
];


TestForRemainingDuplicates[]:=Module[
{success,xb,positions,Duplicates,xsum,rijnamesnow,
nvariables,nmono,npoly,EM,EP,diagnose,gatheredEP,gatheredEM,
gatheredEMpairs, gatheredEPpairs,duplicateEM,duplicateEP,runs
},
diagnose=False;
(* Check for Duplicate values, given random assignments *)
success=False;
(* 
The test to see if there are monomials or polynomials that always have the same 
value is a bit complicated.  If there are a small number of monnomials/polynomials in
the basis, then it will be unlikely that two 9-digit integers will be the same unless
the two monomials/polynomials really are the same.  But if there are a lot of them, then
by chance it will occur that some have the same 9 digit integers, even if they don't identically
have the same value.  However, those accidental coincidences will not occur for EVERY set
of random x values.  So if we run the test a few times and we notice that the monomials/polynomials
that have the same value are not the same for different sets of random x values, then there is
no problem. We make use of the Gather function in Mathematica to group similar in the list EM or EP.
If there are duplicates, than the group will have length>1 and we can use the value to find the pair
of EM[[i]] or EP[[i]] that have the same value.  We keep track of these pairs from run to run, where
the runs have different sets of random x's.  If they are all different, we're ok -- there are no 
real duplications.  If some pairs are always the same, then we note these.
*)
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[]; (*
GetDefinitionstoMathematicaFromFortanOutput makes PPCharStringmono and PPCharStringpoly
as global variables *)
If[diagnose,Print["{rijnamesnow,nvariables,nmono,npoly} = ",
           {rijnamesnow,nvariables,nmono,npoly}];];
(* added 12/4/2021 to address Tag Times problem *)
CharStringmono=PPCharStringmono;
CharStringpoly=PPCharStringpoly;
If[diagnose,PLHCSMTestDups=CharStringmono;PLHCSPTestDups=CharStringpoly;];
CharStringmono=StringReplace[CharStringmono,"D0"->""];
CharStringmono=StringReplace[CharStringmono,"d0"->""];
CharStringpoly=StringReplace[CharStringpoly,"D0"->""];
CharStringpoly=StringReplace[CharStringpoly,"d0"->""];
PPCharStringmono=CharStringmono;
PPCharStringpoly=CharStringpoly;
If[diagnose, PLHPPCSP543=PPCharStringpoly;PLHPPCSM543=PPCharStringmono;];

(*Print["rijnamesnow = ",rijnamesnow];
Print["{nvariables,nmono,npoly} = ",{nvariables,nmono,npoly}];*)
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)

(* mlist,plist, and xlist are defined in GetDefinitionstoMathematicaFromFortranOutput *)
CreateEvalMono[PPCharStringmono]; 
CreateEvalPoly[PPCharStringpoly]; 

duplicateEM={};
duplicateEP={};
runs=4;
Do[(  (* this is the loop over runmum runs *)
  dist=Table[0,{i,1,xsum}];  (* needs to be global *)
  Do[(
     dist[[i]]=RandomReal[{0.1,1}];
     ),{i,1,xsum}];
  Assignx[];
  EM=IntegerPart[10^10*EvalMono[]];
  EP=IntegerPart[10^10*EvalPolyN[]];
  xb=Assignx[];
  (* xb now has the assigned xvalues  *)
  (* examine EM *)
  If[Length[EM]!=Length[DeleteDuplicates[EM]],
    (*Print["There are ",Length[EM]-Length[DeleteDuplicates[EM]]," mono duplicates."];*)
     gatheredEM=Gather[EM];
     If[diagnose,PLHgatheredEM=gatheredEM;];
     Do[(
        If[Length[gatheredEM[[k]]]>1,
           positions=Position[EM,gatheredEM[[k,1]]][[All,1]];
           duplicateEM=Append[duplicateEM,{positions,gatheredEM[[k,1]]}];
          ];
       ),{k,1,Length[gatheredEM]}];
    ];
  If[Length[EP]!=Length[DeleteDuplicates[EP]],
    (*Print["There are ",Length[EP]-Length[DeleteDuplicates[EP]]," poly duplicates."];*)
    gatheredEP=Gather[EP];
    If[diagnose,PLHgatheredEP=gatheredEP;];
    Do[(
      If[Length[gatheredEP[[k]]]>1,
         positions=Position[EP,gatheredEP[[k,1]]][[All,1]];
         duplicateEP=Append[duplicateEP,{positions,gatheredEP[[k,1]]}];
        ];
       ),{k,1,Length[gatheredEP]}];
    ];
),{runnum,1,runs}];
(* now analyze the results *)
If[Length[duplicateEM[[All,1]]]==Length[DeleteDuplicates[duplicateEM[[All,1]]]],
	Print["Fortran ouptut passed uniqueness of monomials"];
	,
	gatheredEMpairs=Gather[duplicateEM[[All,1]]];
	If[diagnose,PLHgatheredEMpairs=gatheredEMpairs;];
	Print["Fortran failed uniqueness of monomials"];
	Print["Duplicate pairs are given below:"];
	Do[(
	  If[Length[gatheredEMpairs[[k]]]>1,
	     Print[gatheredEMpairs[[k,1,1]]];
	    ];
	  ),{k,1,Length[gatheredEMpairs]}];
  ];
  If[Length[duplicateEP[[All,1]]]==Length[DeleteDuplicates[duplicateEP[[All,1]]]],
	Print["Fortran ouptut passed uniqueness of polynomials"];
	,
	gatheredEPpairs=Gather[duplicateEP[[All,1]]];
	If[diagnose,PLHgatheredEPpairs=gatheredEPpairs;];
	Print["Fortran failed uniqueness of polynomials"];
	Print["Duplicate pairs are given below:"];
	Do[(
	  If[Length[gatheredEPpairs[[k]]]>1,
	     Print[gatheredEPpairs[[k,1,1]]];
	    ];
	  ),{k,1,Length[gatheredEPpairs]}];
  ];
Print["End of Test of Duplicate monomials and polynomials"];
(* no output *)
];


GetDefinitionstoMathematicaFromFortranOutput[]:=Module[
{ifrag1,monostart1,monoend1,polystart1,polyend1,CharStringmono1,
CharStringpoly1,npoly1,nmono1,CharStringmono,CharStringpoly,tijn,subbesma,
rijnamesnow,xsum,msum,psum,ifraga,natomsa,rijnamesa,nvariables,npoly,nmono,
CharStringAssignx,CharStringAdd,CSPtab,gotnq,diagnose  (*,nq must be global *)
},
diagnose=False;
If[diagnose,Print["       Executing GetDefinitionstoMathematicaFromFortranOutput"];]; 
(* value of ifrag-1 tells these two functions to use the Fortran output file as an input *)
ifrag1=-1;
If[diagnose,Print["       Creating bemsa files"];];
CreatebemsaFiles[ifrag1];
If[diagnose,Print["       Finished creating bemsa files"];];
{monostart1,monoend1,polystart1,polyend1}=
	Getpolymonostartendpoints[ifrag1];
If[diagnose,Print["       Geting mono/poly character strings"];]; 
{CharStringmono1,CharStringpoly1}=
	Getmonopolycharstrings[ifrag1,monostart1,monoend1,polystart1,polyend1];
(*PPCharStringmono=CharStringmono1 and PPcharStringpoly=CharStringpoly1 are
made in above command (in GetAssignEVmonopoly) *)
CSPtab=textconvert[PPCharStringpoly,"text","table",DataDir];
npoly1=ToExpression[StringDrop[StringDrop[CSPtab[[Length[CSPtab],1]],2],-1]];
(*npoly1=polyend1-polystart1;  (* this does not include the zero-order term *)*)
nmono1=monoend1-monostart1;  (* this does not include the zero-order term *)

(* Get dimensions *)
If[diagnose,Print["       Getting dimensions"];]; 
subbesma=Flatten[Position[bemsaTxtLnc,"  subroutine bemsav(x,p)"]][[1]];
If[!StringContainsQ[bemsaTxtLnc[[subbesma+2]],"::x"],
	Print["Probelm getting dimensions in GetDefinitionstoMathematicaFromFortranOutput"];
	Abort[];
];
nvariables=ToExpression[StringDrop[StringDrop[bemsaTxtLnc[[subbesma+2]],-4],25]];
If[!StringContainsQ[bemsaTxtLnc[[subbesma+3]],")::m"],
	Print["Probelm getting dimensions in GetDefinitionstoMathematicaFromFrtranOutput"];
	Abort[];
];
nmono=ToExpression[StringDrop[StringDrop[bemsaTxtLnc[[subbesma+3]],-4],25]];
If[!StringContainsQ[bemsaTxtLnc[[subbesma+4]],"::p"],
	Print["Probelm getting dimensions in GetDefinitionstoMathematicaFromFrtranOutput"];
	Abort[];
];
npoly=ToExpression[StringDrop[StringDrop[bemsaTxtLnc[[subbesma+4]],-4],25]];
Do[(
If[StringContainsQ[bemsaTxtLnc[[i]],"::q"],
	nq=ToExpression[StringDrop[StringDrop[bemsaTxtLnc[[i]],-4],23]]; (* global *)
	Goto[gotnq];
	];
),{i,1,140}];
nq=0;
Label[gotnq];
Print["{nmono, npoly, nq, nvariables} = ",{nmono,npoly,nq,nvariables}];
If[diagnose,PrintTemporary["       Making Changes to mono/poly character strings"];];
(* get EvalMono and EvalPoly *)
CharStringmono=CharStringmono1;
CharStringpoly=CharStringpoly1;
CharStringmono=StringReplace[CharStringmono,"D0"->""];
CharStringmono=StringReplace[CharStringmono,"d0"->""];
CharStringmono=StringReplace[CharStringmono,"**"->"^"];  (* added 17 Feb 2023 *)
CharStringpoly=StringReplace[CharStringpoly,"D0"->""];
CharStringpoly=StringReplace[CharStringpoly,"d0"->""];
CharStringpoly=StringReplace[CharStringpoly,"**"->"^"];  (* added 17 Feb 2023 *)
(*Print["   nmono in Fortan Output = ",nmono1+1," = in mono: ",StringCount[CharStringmono,"="]];
Print["   npoly in Fortan Output = ",npoly1+1," = in poly: ",StringCount[CharStringpoly,"="]];*)


(*CSP3=CharStringpoly;*)

rijnamesnow=ExtractrijnamesFrombemsaTabc[];
xsum=Length[rijnamesnow];

(* define xyz and define Mathematica xi interms of xyz *)
If[diagnose,Print["       Define mathematica xi interms of xyz coordinates"];];
Do[(
If[StringContainsQ[bemsaTxtLnc[[i]],"x( 1 ) = sqrt"],
xstart=i;
Break[];
];
),{i,1,Length[bemsaTxtLnc]}];
xstart;
Do[(
If[!StringContainsQ[bemsaTxtLnc[[i]],"sqrt(("],
xend=i;
Break[];
];
),{i,xstart,Length[bemsaTxtLnc]}];
xend=xend-1;

xyz=Table[0,{i,1,natomsparent},{j,1,3}];

CharStringAssignx={};
Do[(
x444=StringReplace[bemsaTxtLnc[[i]],"     x( "->"x"];
x444=StringReplace[x444," ) ="->" ="];
CharStringAssignx=CharStringAssignx<>x444<>";\n";
),{i,xstart,xend}];
If[diagnose,PLHCSAx1=CharStringAssignx;];

CharStringAssignx=StringReplace[CharStringAssignx,"xyz( "->"xyz[["];
CharStringAssignx=StringReplace[CharStringAssignx," )-xyz"->"]]-xyz"];
CharStringAssignx=StringReplace[CharStringAssignx,"))**"->"]])^"];
CharStringAssignx=StringReplace[CharStringAssignx,"sqrt((xyz"->"Sqrt[(xyz"];
CharStringAssignx=StringReplace[CharStringAssignx,"xyz("->"xyz[["];
CharStringAssignx=StringReplace[CharStringAssignx,"xyz("->"xyz[["];
CharStringAssignx=StringReplace[CharStringAssignx,"^2)"->"^2]"];
CharStringAssignx=StringReplace[CharStringAssignx,"(("->"("];
CharStringAssignx=StringReplace[CharStringAssignx,")-xyz"->"]]-xyz"];
CharStringAssignx=StringReplace[CharStringAssignx,"x( "->"x"];
If[diagnose,PLHCSAx2=CharStringAssignx;];

(* Develop Function Assignxfromxyz *)

(* NB:  It is assumed that xyz are in \[Angstrom].  use of variables x(i)\[Rule] Exp(-x(i)/a1)
with a1=2 assumes that the distance xi is in Bohr.  Thus x(1) the distance in Angstroms
is divided by 0.52... Ang/Bohr to get x(1) in Bohr before converting to Morse *)

(*
This is no longer good because we changed the get_x function in the Fortran
Do[(
If[StringContainsQ[bemsaTxtLnc[[i]],"x(1)=x(1)"],
xstart=i;
Break[];
];
),{i,1,Length[bemsaTxtLnc]}];
xstart;
Do[(
If[StringContainsQ[bemsaTxtLnc[[i]],"subroutine"],
xend=i-1;
Break[];
];
),{i,xstart,Length[bemsaTxtLnc]}];
CharStringAdd={"a1=2.;\n"};
Do[(
x444=StringReplace[bemsaTxtLnc[[i]],"x("->"  x" ];
x444=StringReplace[x444,")="->" ="];
x444=StringReplace[x444,")/"->"/"];
x444=StringReplace[x444,"exp("->"Exp["];
x444=StringReplace[x444,"a1)"->"a1]"];
x444=StringReplace[x444,"_dp"->""];
x444=StringReplace[x444,"-  "-> "-"];
CharStringAdd=CharStringAdd<>x444<>";\n";

),{i,xstart,xend}];
*)


(* NB:  It is assumed that xyz are in \[Angstrom].  use of variables x(i)\[Rule] Exp(-x(i)/a1)
with a1=2 assumes that the distance xi is in Bohr.  Thus x(1) the distance in Angstroms
is divided by 0.52... Ang/Bohr to get x(1) in Bohr before converting to Morse *)
CharStringAdd="
a1=2.0;\n";

Do[(
	CharStringAdd=CharStringAdd<>"x"<>ToString[i]<>"=x"<>ToString[i]<>"/a1/AperBohr;\n";
    CharStringAdd=CharStringAdd<>"x"<>ToString[i]<>"=Exp[-x"<>ToString[i]<>"];\n";
),{i,1,nvariables}];





CharStringAssignx=CharStringAssignx<>CharStringAdd;
If[diagnose,PLHCSAx3=CharStringAssignx;];
ToExpression["Assignxfromxyz[]:=Module[{},"<>CharStringAssignx<>"];"];

(*GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];*)
msum=nmono1;
psum=npoly1;
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
If[diagnose,Print["       GetAssignEVMoloPoly"];];
GetAssignEVMonoPoly[ifraga,natomsa,rijnamesa];  (* creates PPCharStringmono and PPCharStringpoly
as global variables *)
If[diagnose,Print["       Completed GetDefinitionstoMathematicaFromFortranOutput"];];

{rijnamesnow,nvariables,nmono,npoly}
];



GetFMonoDerivativesOld[]:=Module[
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
If[diagnose,PLHCSM127=CharStringmono1;PLHCSP127=CharStringpoly1];	
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
CharStringmono=StringReplace[CharStringmono,"**"->"^"];
Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
Bx=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
Bx=Drop[Bx,1];  (* get rid of monomial 0 *)

(* doctor up what can be done simply *)
CharStringmono=CharStringmono1;
CharStringmono=StringReplace[CharStringmono,"**"->"^"];
CharStringmono=StringReplace[CharStringmono,"    m(0) = 1.0D0"->"    dm(0) = 0.0D0"];
CharStringmono=StringReplace[CharStringmono,"    m(0) = 1.0"->"    dm(0) = 0.0D0"];
CharStringmono=StringReplace[CharStringmono,"    m("->"    dm("];
If[diagnose,PLHCSM=CharStringmono;];

(* fix the x values  *)
Do[(
	If[StringTake[Bx[[i,3]],1]=="x",
		CharStringmono=StringReplace[CharStringmono,Bx[[i,3]]->
			 "-"<>Bx[[i,1]]<>"/a*drdx(flag,"<>
			 StringDrop[StringDrop[Bx[[i,3]],2],-1]];
	];
),{i,1,nvariables}]; (* 9 Feb 2023, changed this from Length[Bx] to nvariables so 
that it does not do any x's that are used in the m defs for very high orders *)
If[diagnose,PLHCSM128=CharStringmono];

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
			(*CharStringmono=StringReplace[CharStringmono,Bx[[i,3]]-> 
				productderivative[nstar+1,xterms]];
			Bx[[i,3]]=productderivative[nstar+1,xterms];*)
		];
),{i,1,Length[Bx]}];

CharStringmono=StringReplace[CharStringmono,"^"->"**"];
CharStringmono=StringReplace[CharStringmono," * "->"*"];
Fmonoderivatives=CharStringmono;
If[diagnose,PLHCSM129=CharStringmono];

(* convert Fgroup to Lines of Text *)
Export[DataDir<>"DeleteMe.txt",Fmonoderivatives,"Text"];
FMDlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	If[StringContainsQ[FMDlines[[i]],"drdx"],
		FMDlines[[i]]=
			FMDlines[[i]]<>",xyz)";
	];
	FMDlines[[i]]=FMDlines[[i]]<>"\n";
),{i,1,Length[FMDlines]}];


Fmonoderivatives=StringJoin[FMDlines]
];



GetFMonoDerivatives[]:=Module[
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
If[diagnose,PLHCSM127=CharStringmono1;PLHCSP127=CharStringpoly1];	
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
If[diagnose,PLHCSMxx1=CharStringmono;];
CharStringmono=StringDelete[CharStringmono,"	    m(0) = 0.\n"];
CharStringmono=StringDelete[CharStringmono,"	    m(0) = 0.\n"];
CharStringmono=StringDelete[CharStringmono,"        "];
CharStringmono=StringDelete[CharStringmono,"	    m(0) = 1.\n"];
CharStringmono=StringReplace[CharStringmono,"0.D0"->"0.d0"];
CharStringmono=StringReplace[CharStringmono,"**"->"^"];
(*CharStringmono=StringReplace[CharStringmono,
	"m(1) = x(66)"->"    m(1) = x(66)"];*)
(* doctor up what can be done simply *)
(*CharStringmono=StringReplace[CharStringmono,"**"->"^"];*)
CharStringmono=StringReplace[CharStringmono,
	"    m(0) = 1.d0"->"    dm(0) = 0.d0"];
(*CharStringmono=StringReplace[CharStringmono,"dm(0) = 1.d0"->"dm(0) = 0.d0"];
CharStringmono=StringReplace[CharStringmono,"    dm(1) = x(66)"->"dm(1) = x(66)"];*)
If[diagnose,PLHCharStringmono1x=CharStringmono;];

Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
Bx=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[diagnose,PLHBx5=Bx;];

(* fix the x values. NB this expects the m(0) term to be there *)
Print["fixing the x values in dm, nvariables = ",nvariables];
Do[(
	If[StringTake[Bx[[i,3]],1]=="x",
	xnum1=ToExpression[StringDrop[StringDrop[Bx[[i,3]],2],-1]];
	(*Print["i-1 = ",i-1,"    xnum1 = ",xnum1];
	Print["xtransform[[xnum1]] =",xtransform[[xnum1]]];
	Print["Bx[[i-1]] = ",Bx[[i-1,3]]];
	Print[""];*)
		
			(*Print["i-1 =",i-1,"  Bx[[i-1,3]] = ",Bx[[i-1,3]]];*)
			CharStringmono=StringReplace[CharStringmono,Bx[[i,3]]->
			 "-"<>Bx[[i,1]]<>"/a*drdx(flag,"<>
			 StringDrop[StringDrop[Bx[[i,3]],2],-1]];
			 Bx[[i,3]]= "-"<>Bx[[i,1]]<>"/a*drdx(flag,"<>
			 StringDrop[StringDrop[Bx[[i,3]],2],-1];
			
		
	];
),{i,2,nvariables+2}];
If[diagnose,PLHCSM5a=CharStringmono;PLHBx5a=Bx;];

(* get rid of m(0) term, but only in Bx, not in CharStringmono *)
(* this is done because the next step expects the m(0) not to be there *)
If[StringContainsQ[Bx[[1,1]],"m(0)"],
	Bx=Drop[Bx,1];
];
(* change leading m( to dm( *)
CharStringmono=StringReplace[CharStringmono,"    m("->"    dm("];
Do[(
	Bx[[i,1]]="d"<>Bx[[i,1]];
),{i,1,Length[Bx]}];

(* change multiplied monomials to derivative form *)
(* I am currently doing this both in Bx and in CharStringmono because the
CharStringmonno replacement seems not always to work properly; the Bx method 
worked, so I'm keeping that  *)
Print["changing multiplied monomials to derivative form"];
Do[(
    idyn=Length[Bx]-i; 
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
		(*CharStringmono=StringReplace[CharStringmono,first<>"*"<>second->
		"d"<>first<>" * "<>second<>" + "<>first<>" * d"<>second];*)
		Bx[[i,3]]="d"<>first<>"*"<>second<>" + "<>first<>"*d"<>second;
		,
		If[StringCount[Bx[[i,3]],"*"]>1,
			nstar=StringCount[Bx[[i,3]],"*"];
			xterms=parseterms[nstar,Bx[[i,3]]];
			(*Print["Bx[[i]] = ",Bx[[i]]];
			Print["Bx[[i,3]] = ",Bx[[i,3]]];
			Print["nstar = ",nstar];
			Print["xterms = ",xterms];
			Print["productderivative = ",productderivative[nstar+1,xterms]];
			Abort[];*)
			(*CharStringmono=StringReplace[CharStringmono,Bx[[i,3]]-> 
				productderivative[nstar+1,xterms]];*)
			Bx[[i,3]]=productderivative[nstar+1,xterms];
		];
	];
),{i,nvariables+1,Length[Bx]}];
CharStringmono=textconvert[Bx,"table","textstandard",DataDir];
If[diagnose,PLHCharStringmono76=CharStringmono;PLHBx76=Bx];

CharStringmono=StringReplace[CharStringmono,"^"->"**"];
(*CharStringmono="\n    dm(0) = 0.d0\n"<>CharStringmono;*)
CharStringmono=StringReplace[CharStringmono," * "->"*"];
CharStringmono=StringReplace[CharStringmono," ** "->"**"];
Fmonoderivatives=CharStringmono;
If[diagnose,PLHFmonoderivatives=Fmonoderivatives];

(* convert Fgroup to Lines of Text *)
Export[DataDir<>"DeleteMe.txt",Fmonoderivatives,"Text"];
FMDlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	If[StringContainsQ[FMDlines[[i]],"drdx"],
		FMDlines[[i]]=
			FMDlines[[i]]<>",xyz)";
	];
	FMDlines[[i]]=FMDlines[[i]]<>"\n";
),{i,1,Length[FMDlines]}];
If[diagnose,PLHFMDlines=FMDlines];

If[diagnose,PLHMDout=StringJoin[FMDlines];];(* Abort[]; *)
Fmonoderivatives=StringJoin[FMDlines]
];




GetFPolyDerivatives[]:=Module[
{natoms1,rijnames1,ifrag1,nvariables1,monostart1,monoend1,polystart1,
polyend1,CharStringmono1,CharStringpoly1,CharStringmono,Bx,CharStringpoly,
first,second,C3,Skipj,nstar,nplus,nminus,nopen,nclose,Lx,
xsum,ifraga,natomsa,rijnamesa,nvariablesa,monostarta,monoenda,
polystarta,polyenda,rijnamesnow,nvariables,nmono,npoly,diagnose,xterms
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
If[diagnose,
	Export[DataDir<>"CharStringmonoinGetFPolyDerivatives",CharStringmono1,"Text"];
	Export[DataDir<>"CharStringpolyinGetFPolyDerivatives",CharStringpoly1,"Text"];
];
(* Here is a correction that fixes a problem*)
CharStringpoly1=StringReplace[CharStringpoly1,")+"->") +"];	
If[diagnose,
	Export[DataDir<>"CharStringpolyinGetFPolyDerivatives2",CharStringpoly1,"Text"];
];
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


(* make Bx, tabular form of polynomials *)
CharStringpoly=CharStringpoly1;
CharStringpoly=StringReplace[CharStringpoly,"**"->"^"];
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
Bx=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
Bx=Drop[Bx,1];  (* get rid of polymial 0 *)

(* doctor up what can be done simply *)
CharStringpoly=CharStringpoly1;
CharStringpoly=StringReplace[CharStringpoly,"**"->"^"];
CharStringpoly=StringReplace[CharStringpoly,"    p("->"    dp("];

(*Now turn CharStringpoly into a text line file *)
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
Lx=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Lx=Drop[Lx,1];  (* get rid of zero order term *)


	(* work on polys line by line, using Bx as criteria but changing Lx *)
Do[(  
(* we are now working on the ith line *)
	Do[(
		(* we are now working on the jth part of Bx, starting at 3 and going to the end *)
		nstar=StringCount[Bx[[i,j]],"*"];
		nplus=StringCount[Bx[[i,j]],"+"];
		nminus=StringCount[Bx[[i,j]],"-"];
		nopen=StringCount[Bx[[i,j]],"("];
		nclose=StringCount[Bx[[i,j]],")"];
		(*Print["i,j =",{i,j}];
		Print[Bx[[i]]];
		Print[Bx[[i,j]]];
		Print[{nstar,nplus,nminus,nopen,nclose}];*)
		If[nstar==0 && nplus==0 && nminus==0 && nopen==1 && nclose==1,
			Lx[[i]]=StringReplace[Lx[[i]],Bx[[i,j]]->"d"<>Bx[[i,j]]];
			Lx[[i]]=StringReplace[Lx[[i]],"dddd"->"d"];
			Lx[[i]]=StringReplace[Lx[[i]],"ddd"->"d"];
			Lx[[i]]=StringReplace[Lx[[i]],"dd"->"d"];
		];
		If[nstar==1 && nplus==0 && nminus==0 && nopen==2 && nclose==2,
			first={};
			C3=Characters[Bx[[i,j]]];
			Do[(
				If[C3[[j]]!="*",
					first=Append[first,C3[[j]]];,
					Break[];
				];
			),{j,1,Length[C3]}];
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
			Lx[[i]]=StringReplace[Lx[[i]],first<>"*"<>second->
				"d"<>first<>" * "<>second<>" + "<>first<>" * d"<>second];
		];
		If[nstar>1,
			xterms=parseterms[nstar,Bx[[i,j]]];
			Lx[[i]]=StringReplace[Lx[[i]],Bx[[i,j]]->productderivative[nstar+1,xterms]];		
		];	
	),{j,3,Length[Bx[[i]]]}];
	Lx[[i]]=Lx[[i]]<>"\n";
),{i,1,Length[Bx]}];

CharStringpoly=StringReplace[StringJoin[Lx]," * "->"*"];
CharStringpoly=StringReplace[CharStringpoly,"^"->"**"]
];




GetMathematicaVersionOfMonoDerivatives[]:=Module[
{CharStringdmono1,CharStringdmono,Lx},
CharStringdmono1=GetFMonoDerivatives[];
(* make Bx, tabular form of monomials *)
CharStringdmono=CharStringdmono1;
(*Now turn CharStringpoly into a text line file *)

Export[DataDir<>"DeleteMe.txt",CharStringdmono,"Text"];
Lx=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Lx=Drop[Lx,1];  (* get rid of zero order term *)
Do[Lx[[i]]=Lx[[i]]<>"\n";,{i,1,Length[Lx]}];

Do[(
If[StringContainsQ[Lx[[i]],"drdx"],
Lx[[i]]=StringReplace[Lx[[i]],"drdx(flag"->"drdx[flag"];
Lx[[i]]=StringReplace[Lx[[i]],")\n"->"];\n"];
];
),{i,1,Length[Lx]}];
CharStringdmono=StringJoin[Lx];
CharStringdmono=bemsaToMathematica[CharStringdmono];
CharStringdmono=StringReplace[CharStringdmono,";/"->"/"];

StringReplace[CharStringdmono,";/"->"/"];
StringDelete[CharStringdmono,",xyz"]
];



GetMathematicaVersionOfPolyDerivatives[]:=Module[
{},

bemsaToMathematica[GetFPolyDerivatives[]]
];



drdx::usage="drdx[flag,xindex]
This function also requires that xyz be set with the Cartesian coordinates in 
natoms by 3 format.

";
drdx[flag_,xindex_]:=Module[
{i,j,m,output,matom,xyzind,rijnamesnow},

(*NB: xyz is global *)
rijnamesnow=ExtractrijnamesFrombemsaTabc[];
i=ToExpression[StringTake[rijnamesnow[[xindex]],2]];
j=ToExpression[StringTake[rijnamesnow[[xindex]],-2]];
(* i and j are the atom numbers in rij *)
(*Print["{i,j} = ",{i,j}];*)
m=flag; (* m is a number from 1 to 3N, so the atom number is IntegerPart[m/3]+1 *)
matom=IntegerPart[(m-0.000001)/3]+1;
(*Print["matom = ",matom];*)
xyzind=Mod[m-1,3]+1;
(*Print["xyzind = ",xyzind];*)
(*Print["{matom,xyzind} = ",{matom,xyzind}];*)
If[matom==i, output=(xyz[[i,xyzind]]-xyz[[j,xyzind]])/
	Sqrt[(xyz[[i,1]]-xyz[[j,1]])^2+
	        (xyz[[i,2]]-xyz[[j,2]])^2+
                  (xyz[[i,3]]-xyz[[j,3]])^2];
                  (*Print[{xyz[[i,xyzind]],xyz[[i,xyzind]]}];*)
];
If[matom==j, output=(xyz[[j,xyzind]]-xyz[[i,xyzind]])/
	Sqrt[(xyz[[i,1]]-xyz[[j,1]])^2+
	        (xyz[[i,2]]-xyz[[j,2]])^2+
                  (xyz[[i,3]]-xyz[[j,3]])^2];
                 (*Print[{xyz[[j,xyzindex]],xyz[[j,xyzindex]]}];*)
];
If[matom!=i && matom!=j,output=0;];

output
];




GetTabLnStringFormsofMonoderAndPolyder[]:=Module[
{x},
monoderivsstring=GetMathematicaVersionOfMonoDerivatives[]; 
x=StringReplace[monoderivsstring,";"->";\n"];
x=StringReplace[x,"*"->" "];
x=StringReplace[x,"+"->" "];
x=StringReplace[x,"-"->" "];
x=StringReplace[x,"="->" "];
x=StringReplace[x,";"->" "];
Export[DataDir<>"DeleteMe.txt",x,"Text"];
monoderTab=Import[DataDir<>"DeleteMe.txt","Table"]; 
DeleteFile[DataDir<>"DeleteMe.txt"];
Export[DataDir<>"DeleteMe.txt",StringReplace[monoderivsstring,";"->";\n"],"Text"];
monoderLn=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}]; 
DeleteFile[DataDir<>"DeleteMe.txt"];

polyderivsstring=GetMathematicaVersionOfPolyDerivatives[]; 
x=StringReplace[polyderivsstring,";"->";\n"];
x=StringReplace[x,"*"->" "];
x=StringReplace[x,"+"->" "];
x=StringReplace[x,"-"->" "];
x=StringReplace[x,"="->" "];
x=StringReplace[x,";"->" "];
Export[DataDir<>"DeleteMe.txt",x,"Text"];
polyderTab=Import[DataDir<>"DeleteMe.txt","Table"]; 
DeleteFile[DataDir<>"DeleteMe.txt"];
Export[DataDir<>"DeleteMe.txt",StringReplace[polyderivsstring,";"->";\n"],"Text"];
polyderLn=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}]; 
DeleteFile[DataDir<>"DeleteMe.txt"];

{monoderivsstring,monoderTab,monoderLn,polyderivsstring,polyderTab,polyderLn}
];



GetEvaluationSchemefordp[dpno_]:=Module[
{xxc,xxd,evaluate,repeat,lets,num,droplist,xxclist,
FinishUp
},
{monoderivsstring,monoderTab,monoderLn,polyderivsstring,polyderTab,polyderLn}=
	GetTabLnStringFormsofMonoderAndPolyder[];
xxc=polyderTab[[dpno]];
(*Print[xxc];
Print[polyderLn[[dpno]]];*)

xxd=polyderLn[[dpno]];
(*xxd=StringReplace[xxd,"="\[Rule]" = "];
xxd=StringReplace[xxd,"*"\[Rule]" * "];
xxd=StringReplace[xxd,"+"\[Rule]" + "];
xxd=StringReplace[xxd,"-"\[Rule]" - "];*)
evaluate={xxd};
(*Print[evaluate];*)

Label[repeat];
Do[(
lets=StringTake[xxc[[i]],2];
(*Print[{xxc[[i]],lets}];*)
If[Or[lets=="dm",lets=="dp"],
num=ToExpression[StringDrop[xxc[[i]],2]];
(*Print[{xxc[[i]],lets,num}];*)
If[lets=="dm",
xxd=monoderLn[[num]];
(*xxd=StringDelete[xxd,lets<>ToString[num]<>"="];
xxd=StringTrim[StringDrop[xxd,-1]]<>" ";*)
];
If[lets=="dp",
xxd=polyderLn[[num]];
(*Print["xxd = ",xxd];*)
(*xxd=StringDelete[xxd,lets<>ToString[num]<>"="];
xxd=StringTrim[StringDrop[xxd,-1]]<>" ";*)
];
];
(*Print["{xxd,lets,num} =",{xxd,lets,num}];
Print[lets<>ToString[num]<>"="];*)
(*polyderLn[[dpno]]=StringReplace[polyderLn[[1777]],lets<>ToString[num]\[Rule]"( "<>xxd<>" )"];*)

evaluate=Prepend[evaluate,xxd];
(*Print["evaluate = ",evaluate];Pause[.2];*)
),{i,1,Length[xxc]}];
xxc=DeleteDuplicates[evaluate];
xxc=StringReplace[xxc,"="->" = "];
xxc=StringReplace[xxc,"*"->" * "];
xxc=StringReplace[xxc,"+"->" + "];
xxc=StringReplace[xxc,"-"->" - "];
xxc=StringReplace[xxc,";"->" ;"];
Export["DeleteMe.txt",xxc,"Text"];
xxc=Flatten[Import["DeleteMe.txt","Table"],1];

Quiet[
Do[(
If[xxc[[i+1]]== "=",
(*Print[{i,xxc[[i]]}];*)
droplist=Flatten[Position[xxc,xxc[[i]]]];
Do[(
(*Print[{j,droplist[[j]]}];*)
xxc=Drop[xxc,{droplist[[j]]}];
),{j,Length[droplist],1,-1}];
];
),{i,1,Length[xxc]-1}];
];
xxclist={};
Do[(
If[StringLength[xxc[[i]]]>1 && 
Or[StringTake[xxc[[i]],2]=="dp",StringTake[xxc[[i]],2]=="dm"],
xxclist=Append[xxclist,xxc[[i]]];
];
),{i,1,Length[xxc]}];
xxclist=DeleteDuplicates[xxclist];
xxc=xxclist;
Pause[.2];
(*Print["xxc = ",xxc];Pause[1];*)
If[xxc=={},Goto[FinishUp];];
(*Print["Repeating"];*)
Goto[repeat];

Label[FinishUp];

DeleteDuplicates[evaluate]
];



GetEVMonoPolyDerivatives[ndmono_,ndpoly_]:=Module[
{monoderivsstring,polyderivsstring},

(* note that dplist and dmlist need to run from zero inorder to get these evaluated *)
dplist=Table["dp"<>ToString[i],{i,0,ndpoly}];  (* needs to be global *)
dmlist=Table["dm"<>ToString[i],{i,0,ndmono}];  (* needs to be global *)
monoderivsstring=GetMathematicaVersionOfMonoDerivatives[];

ToExpression[CreateEvalAllMonoDerivs[monoderivsstring]];

polyderivsstring=GetMathematicaVersionOfPolyDerivatives[];
ToExpression[CreateEvalAllPolyDerivs[polyderivsstring]];
(*PLHmonoderivsstring=monoderivsstring;
PLHpolyderivsstring=polyderivsstring;*)
];



CreateGroup[dmkeep_,dpkeep_,Evdm_,Evdp_]:=Module[
{x,Stab,Group},



{monoderivsstring,monoderTab,monoderLn,polyderivsstring,polyderTab,polyderLn}=
	GetTabLnStringFormsofMonoderAndPolyder[];
Group={};
Do[(
Group=Append[Group,monoderLn[[dmkeep[[i]]]]];
),{i,1,Length[dmkeep]}];
Do[(
Group=Append[Group,polyderLn[[dpkeep[[i]]]]];
),{i,1,Length[dpkeep]}];


x=StringReplace[StringJoin[Group],"*"->" * "];
x=StringReplace[x,"+"->" + "];
x=StringReplace[x,"-"->" - "];
x=StringReplace[x,"="->" = "];
x=StringReplace[x,";"->" ; "];
Export[DataDir<>"DeleteMe.txt",x,"Text"];
Stab=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
Stab=Flatten[Stab];
(* Stab is now a tabular version of the group *)

dmlistmod=Drop[dmlist,1];
dplistmod=Drop[dplist,1];
Do[(
	If[Evdm[[i]]==0,
		Do[(
			If[Stab[[j]]==dmlistmod[[i]] && Stab[[j+1]]!="=",
				Stab[[j]]="0.";
			];
		),{j,1,Length[Stab]-1}];
	];
),{i,1,Length[Evdm]}];

Do[(
	If[Evdp[[i]]==0,
		Do[( 
			If[Stab[[j]]==dplistmod[[i]] && Stab[[j+1]]!="=",
				Stab[[j]]="0."
			];
		),{j,1,Length[Stab]-1}];
	];
),{i,1,Length[Evdp]}];

x=StringJoin[Stab];
x=StringReplace[x,"="->" = "];
x=StringReplace[x,"+"->" + "];
x=StringReplace[x,"-"->" - "];
x=StringReplace[x,";"->" ; "];

Group=x
];


GetRidOfZeros[CharString_]:=Module[
{df,dfTab,nexti,skipnext,im1iseq,nextone,delim1,delip1,
dfnew,dfnewer
},

(* this is designed to operate on Mgroup items, the output from CreateGroup *)

df=CharString;


(* get tabular form *)
x=StringReplace[df,"+"->" + "];
x=StringReplace[x,"-"->" - "];
x=StringReplace[x,"="->" = "];
df=StringReplace[x,";"->" ; "];
Export[DataDir<>"DeleteMe.txt",df,"Text"];
dfTab=Flatten[Import[DataDir<>"DeleteMe.txt","Table"]];
DeleteFile[DataDir<>"DeleteMe.txt"];

(* set to 0. terms with zeros multiplying something *)
Do[(
If[!StringQ[dfTab[[i]]],Goto[nexti];];
If[StringQ[dfTab[[i]]] &&StringContainsQ[dfTab[[i]],"0."],
	dfTab[[i]]=0.;
];
Label[nexti];
),{i,Length[dfTab],1,-1}];

(*get rid of terms with zeros alone *)
Do[(
(*im1iseq=False; delim1=False;delip1=False;skipnext=False;*)
If[dfTab[[i]]== 0.,
	dfTab=Drop[dfTab,{i}];
];
),{i,Length[dfTab],1,-1}];


(* put string back together and fix a few problems *)
dfnew="";
Do[(
If[StringQ[dfTab[[i]]],
	dfnew=dfnew<>dfTab[[i]];
	,
	dfnew=dfnew<>ToString[dfTab[[i]]];
];
),{i,1,Length[dfTab]}];
dfnew=StringReplace[dfnew,";"->"; "];
dfnew=StringReplace[dfnew,"0.m"->"m"];
dfnew=StringReplace[dfnew,"0.d"->"d"];
dfnew=StringReplace[dfnew,"0.p"->"p"];

dfnew=StringReplace[dfnew,"++"->"+"];
dfnew=StringReplace[dfnew,"+-"->"-"];
dfnew=StringReplace[dfnew,"--"->"-"];
dfnew=StringReplace[dfnew,"-+"->"+"];

dfnew=StringReplace[dfnew,"+++"->"+"];
dfnew=StringReplace[dfnew,"++-"->"-"];
dfnew=StringReplace[dfnew,"+--"->"-"];
dfnew=StringReplace[dfnew,"+-+"->"+"];

dfnew=StringReplace[dfnew,"---"->"-"];
dfnew=StringReplace[dfnew,"--+"->"+"];
dfnew=StringReplace[dfnew,"-++"->"+"];
dfnew=StringReplace[dfnew,"-+-"->"-"];

dfnew=StringReplace[dfnew,"+;"->"; "];
dfnew=StringReplace[dfnew,"-;"->"; "];
dfnew=StringReplace[dfnew,";"->";\n"];


(* turn it into line text *)
Export[DataDir<>"DeleteMe.txt",dfnew,"Text"];
dfnewer=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];

(* join it into the final string *)
StringJoin[dfnewer]
];



GetdmkeepAnddpkeep[flag_]:=Module[
{rijnamesnow,nvariables,nmono,npoly,m,matom,Evdm,Evdp,dpkeep,dmkeep},

{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
GetEVMonoPolyDerivatives[nmono,npoly];
xyz=Table[0,{i,1,natomsparent},{j,1,3}];  (* global *)
Assignxfromxyz[];


(*drdx[flag,18,xyz];  (* for flag = 1, all of these are zero except for 1 through 6 *)*)
m=flag;
matom=IntegerPart[(m-0.000001)/3]+1;
xyz[[matom]]={RandomReal[{0.1,1}],RandomReal[{0.1,1}],RandomReal[{0.1,1}]};
(*xyz=Table[RandomReal[{0.1,1}],{i,1,natomsparent},{j,1,3}];  (* global *)*)
Assignxfromxyz[];
EvalMono[];
EvalPoly[];
Evdm=EvalAllMonoDerivs[flag];
Evdp=EvalAllPolyDerivs[];

(* get rid of zero-order terms from Evdm and Evdp *)
Evdm=Drop[Evdm,1];  (* z-order terms will disappear until end of EvaldpBasisSet *)
Evdp=Drop[Evdp,1];  (* z-order terms will disappear until end of EvaldpBasisSet *)
(* now position i in Evdm or Evdp will correspond to dm(i) or dp(i)  *)

dmkeep={};
Do[(
If[Abs[Evdm[[i]]]>0,
dmkeep=Append[dmkeep,i];
];
),{i,1,Length[Evdm]}];

dpkeep={};
Do[(
If[Abs[Evdp[[i]]]>0,
dpkeep=Append[dpkeep,i];
];
),{i,1,Length[Evdp]}];



{dmkeep,dpkeep,Evdm,Evdp}
];




MakeGroupFandM[matom_]:=Module[
{Mgroup,Fgroup,flag,dmkeep,dpkeep,Evdm,Evdp},
flag=3(matom-1)+2;  (* flag doesn't really matter, since the Group for all flags for a given 
atom are the same *)

{dmkeep,dpkeep,Evdm,Evdp}=GetdmkeepAnddpkeep[flag];
Mgroup=CreateGroup[dmkeep,dpkeep,Evdm,Evdp];  (* this creates a group of definations in M that can be 
turned into an expression*)

(* Here's a F version: *)
Fgroup="      "<>StringReplace[Mgroup,";"->"\n     "];
Fgroup=StringReplace[StringReplace[Fgroup,"["->"("],"]"->")"];
(*Fgroup=StringReplace[StringReplace[Fgroup,"m"->"m("]," ="->") ="];  This is what was there *)
Fgroup=StringReplace[StringReplace[Fgroup,"m"->"m("],"="->")="];
Fgroup=StringReplace[StringReplace[Fgroup,"*"->")*"],"p"->"p("];
Fgroup=StringReplace[StringReplace[Fgroup,"- 0."->""],"+ 0."->""];
Fgroup=StringReplace[StringReplace[Fgroup,"0.  + "->""],"\n"->")\n"];
Fgroup=StringReplace[StringReplace[Fgroup,"0. + "->""]," )"->")"];
(*Fgroup=StringReplace[StringReplace[Fgroup," + "->") + "]," - "->") - "]; This is what was there *)
Fgroup=StringReplace[StringReplace[Fgroup,"+"->")+"],"-"->")-"];
Fgroup=StringReplace[Fgroup,"))"->")"];
Fgroup=StringReplace[Fgroup,"  )"->")"];
Fgroup=StringReplace[Fgroup,"=)"->"="];   (* Added this *)
Fgroup=StringReplace[Fgroup,"/a)"->")/a"]; (* Added this *)
Fgroup=StringReplace[Fgroup,"+)+"->"+"];  (* Added this *)
Fgroup=StringReplace[Fgroup,"+)"->""];  (* Added this *)
Mgroup=StringReplace[Mgroup,"++"->"+"];  (* Added this *)

{Mgroup,Fgroup,dmkeep,dpkeep}
];


MakeFdrdxfunction[]:=Module[
{x,y,Fdrdxfunction},

(* here's a Fortran function for drdx:  *)
MakerijnamesAndnatomsfrag[];
x=GenerateFortranCommentWithXAssignments[];
x=StringReplace[x,"!"->" "];
x=StringReplace[x,"x("->"if (xindex.eq."];
x=StringReplace[x,"="->"then \n"];
Export[DataDir<>"DeleteMe.txt",x,"Text"];
y=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
y[[i]]=StringTrim[y[[i]]];
y[[i]]="\n        i = "<>ToString[ToExpression[StringTake[y[[i]],2]]]<>"\n"<>
"        j = "<>ToString[ToExpression[StringTake[y[[i]],-2]]]<>"\n";
),{i,2,Length[y],2}];
y=StringJoin[y];
Fdrdxfunction=
"   real function drdx (flag,xindex,xyz)
        implicit none
        integer i,j,flag,xindex,xyzind,matom,m
        real (wp) :: xyz(12z1047,3)

";
Fdrdxfunction=Fdrdxfunction<>y;
Fdrdxfunction=Fdrdxfunction<>"
      endif
      m=flag
      matom=INT((dble(m)-0.00001d0)/3.d0)+1
      xyzind=MOD(m-1,3)+1


       drdx = 0.d0
      if (matom.eq.i.or.matom.eq.j) then
          drdx=(xyz(i,xyzind)-xyz(j,xyzind))/r(i,j)
       ! Chen: pass r(i,j) into this function rather than computing them
       !       inside the function
          if (matom.eq.j) then
            drdx = -drdx
          endif
      endif

     return
     end function
";

Fdrdxfunction=StringReplace[Fdrdxfunction,"if (xindex.eq" -> 
	"elseif (xindex.eq"];
Fdrdxfunction=StringReplace[Fdrdxfunction,"elseif (xindex.eq.1) then" -> 
	"if (xindex.eq.1) then"];


Fdrdxfunction
];



PreparedpBasisSetInfo[]:=Module[
{},
Do[(
	Print["       Analyzing Group ",iatom," of ",natomsparent];
	{Mgroup,Fgroup,dmkeep,dpkeep}=MakeGroupFandM[iatom];  (* global *)
	Mgroup=GetRidOfZeros[Mgroup];
	Mgroup=StringReplace[Mgroup,"=+"->"="];  (* Added this 1/24/20 *)
	ToExpression["Mgroup"<>ToString[iatom]<>"=Mgroup"];  (* global *)
	ToExpression["dmkeep"<>ToString[iatom]<>"=dmkeep"];  (* global *)
	ToExpression["dpkeep"<>ToString[iatom]<>"=dpkeep"];  (* global *)
),{iatom,1,natomsparent}];

(* no output *)
];


PreparedpBasisSetInfoFromFortran[]:=Module[
{subMgroupnow,endsubMgroupnow,ch,Skipc,xmy,cparen,ch1,
num},
ifrag1=-1; 
CreatebemsaFiles[ifrag1];

Do[(
subMgroupnow=
	Flatten[Position[bemsaTxtLnc,"      Subroutine MGroup"<>ToString[i]<>" (flag,x,m,p,dp,xyz)"]][[1]];
endsubMgroupnow=
	Flatten[Position[bemsaTxtLnc,"      End Subroutine MGroup"<>ToString[i]]][[1]];
(*Print[{i,subMgroupnow,endsubMgroupnow}];*)
	Mgroup={};
	Do[(
		Mgroup=Append[Mgroup,bemsaTxtLnc[[j]]];
),{j,subMgroupnow+11,endsubMgroupnow-4}];
(*Mgroup*)

dmkeep={};
dpkeep={};
Do[(
	ch=Characters[Mgroup[[m]]];
	(*Print[ch];*)
	Do[(
		If[ch[[my]]==" ",Goto[Skipc];];
		xmy=my;
		If[ch[[my]]=="d" && ch[[my+2]]=="(",
			(*Print["my+2 = ",my+2];*)
			(* find close paren *)
			Do[(
				If[ch[[my+mz]]==")",cparen=mz;Break[];];
			),{mz,3,60}];	
			(*num=ToExpression[StringTake[ch,{my+3,cparen+my}]];*)
			(*Print["cparen = ",cparen];*)
			ch1=Drop[ch,my+2];
			ch1=Take[ch1,cparen-3];
			num=ToExpression[StringJoin[ch1]];
			(*Print[num];*)
			Break[];
		];
	Label[Skipc];
	),{my,1,60}];
	If[ch[[xmy+1]]== "m",dmkeep=Append[dmkeep,num];];
	If[ch[[xmy+1]]== "p",dpkeep=Append[dpkeep,num];];	
),{m,1,Length[Mgroup]}];
Mgroup=bemsaToMathematica[StringJoin[Mgroup]];
Mgroup=StringReplace[Mgroup,";"->"; "];
Mgroup=StringReplace[Mgroup,"drdxflag"->"drdx[flag"];
Mgroup=StringReplace[Mgroup,",xyz"->"]"];
Mgroup=StringReplace[Mgroup,"; /"->"/"];
Mgroup=StringReplace[Mgroup,"; ;"->";"];
Mgroup=StringReplace[Mgroup,"++"->"+"];  (* Added this *)
ToExpression["Mgroup"<>ToString[i]<>"=Mgroup;"];  (* global *)
ToExpression["dmkeep"<>ToString[i]<>"=dmkeep;"];  (* global *)
ToExpression["dpkeep"<>ToString[i]<>"=dpkeep;"];  (* global *)
),{i,1,natomsparent}];

(* no output *)
];


EvaldpBasisSet::usage="EvaldpBasisSet[...]
Inputs: 
m:      the xyz coordinate that you want the derivative wrt;  deriv = dE/dm
xyz:    this is global and has the current cart coordinates of the geometry in groups of 3 in 
		the order of 1,2,3,4, etc, where these numbers have been assigned to the atoms
		of the parent compound.  {{1x,1y,1z},{2x,2y,2z},...}
EM,EP:  The monomials and polynomials for this geometry shouldhave already been evaluated and
			are input as EM and EP, respectively
		
Assumption: PreparedpBasisSetInfo[] has been executed
";
EvaldpBasisSet[m_,EM_,EP_]:=Module[
{},
(*tim1=Timing[*)
flag=m;  (* flag needs to be global *)
matom=IntegerPart[(m-0.000001)/3]+1;  (* associates an atom with a flag *)

ToExpression["Mgroupnow=Mgroup"<>ToString[matom]<>";"];
ToExpression["dmkeepnow=dmkeep"<>ToString[matom]<>";"];
ToExpression["dpkeepnow=dpkeep"<>ToString[matom]<>";"];
(* the above commands associate the correct Mgroup, dmkeep,dpkeep with the versions
Mgroupnow, dmkeepnow,dpkeemnow used below -- it is the equivalent of a Which command *)
(*];*)


(*tim2=Timing[*)
(* zero out the basis set to start with *)
(*Evdm=Table[0,{i,1,Length[EM]-1}];  (* NB these lists do not include the zero order term *)*)
Evdp=Table[0,{i,1,Length[EP]-1}];  (* NB these lists do not include the zero order term *)


ToExpression[Mgroupnow]; (* evaluates all the dm and dp that need to be evaluated for this group*)
(*];*)

(*
(* next, load up Evdm, the list of evaluated monomials *)
Do[(
	bfn=dmkeepnow[[i]];
	ToExpression["Evdm[["<>ToString[bfn]<>"]]=dm"<>ToString[dmkeepnow[[i]]]];
),{i,1,Length[dmkeepnow]}];
*)

(*tim3=Timing[*)
(* then, load up Evdp, the list of evaluated polynomials *)
Do[(
	bfn=dpkeepnow[[i]];
	ToExpression["Evdp[["<>ToString[bfn]<>"]]=dp"<>ToString[dpkeepnow[[i]]]];
),{i,1,Length[dpkeepnow]}];
(*];*)
(*tottim1=tottim1+tim1;
tottim2=tottim2+tim2;
tottim3=tottim3+tim3;*)

 (* add back the zero-order term and exit with the evaluated polynomial basis set *)
Evdp=Prepend[Evdp,0.]   
];



WriteFListofNumbers[nolist_]:=Module[
{nbuf,i,buf,bu},
nbuf=15;
i=1;
While[i<Length[nolist]-nbuf+1,
Do[(
If[i==1,
buf=ToString[nolist[[i]]]<>",";
,
buf=buf<>ToString[nolist[[i]]]<>",";
];
i=i+1;
),{j,1,nbuf}];
buf=buf<>"$\n        ";
];
Do[(
If[j<Length[nolist],
buf=buf<>ToString[nolist[[i]]]<>",";
i=i+1;
,
buf=buf<>ToString[nolist[[i]]];
];
),{j,i,Length[nolist]}];

buf
];




MakeExportFortranDDWithBatchDerivatives::usage="MakeExportFortranDDWithBatchDerivatives[...]
Called From SequentialBulidupWithBatchDerivatives[]

";
MakeExportFortranDDWithBatchDerivatives[CommentXAssignments_,CharStringmono_,CharStringpoly_]:=Module[
{fnamea,natomsa,rijnamesa,bemsaa,monostarta,monoenda,StringAssigns,
polystarta,polyenda,newfortran,xstart1a,xstart1b,xstart2,xstart3,
xmiddle,xending,CSMlines,AnotherRoundM,AnotherRoundP,CSPlines,
CSMlinesch,CSPlinesch,jsave,outM,outP,CSMappend,CSMremaining,
CSPappend,CSPremaining,nvar,nmn,npol,z,Tijnums,xstart1,FinishUp,
natoms,rijnamesnow,nvariables,nmono,npoly,xsum,ifraga,xyz,xfinal,linedist
},

Print[""];
Print["Preparing to make and export Fortran file with batch derivatives"];
Print["   Getting Basis Set Information "];
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];

xsum=Length[rijnamesnow];
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
GetAssignEVMonoPoly[ifraga,natomsa_,rijnamesa];
GetEVMonoPolyDerivatives[nmono,npoly];
xyz=Table[0,{i,1,natomsparent},{j,1,3}];  (* global *)
natoms=natomsparent;
Print["   Performing Basis Set Initiation (takes time!) "];
PreparedpBasisSetInfo[];
(*
Do[(
	ToExpression["Ldpk"<>ToString[i]<>"="<>ToString[Length[ToExpression["dpkeep"<>ToString[i]]]]<>";"];
	(*Print["Ldpk"<>ToString[i]<>" = ",ToExpression["Ldpk"<>ToString[i]]];*)
),{i,1,natomsparent}];
*)

(* enter fortran template pieces*)
xstart1a= "module bemsa
  implicit none
  integer, parameter :: wp  = kind(1.0D0)
  real(wp)::r(12z1047,12z1047)
   real(wp), parameter :: a = 2.d0 
  ";
(*xstart1a=xstart1a<>"     real(wp),dimension(1:21z)::x\n";
xstart1a=xstart1a<>"     real(wp),dimension(0:239z)::m\n";
xstart1a=xstart1a<>"     real(wp),dimension(0:578z)::p\n"; 
xstart1a=xstart1a<>"     real(wp),dimension(0:578z)::dp\n"; *)
(*
Do[(
xstart1a=xstart1a<>"    real(wp),dimenison(1:Ldpk"<>ToString[i]<>
	")::dpkeep"<>ToString[i]<>"\n";
),{i,1,natomsparent}];
xstart1a=xstart1a<>"    Common /keep/ &\n";
*)
(*
Do[(
	xstart1a=xstart1a<>"      dpkeep"<>ToString[i]<>", &\n";	
),{i,1,natomsparent-1}];
xstart1a=xstart1a<>"      dpkeep"<>ToString[natomsparent]<>"\n";
*)
(*xstart1a=xstart1a<>"    Common /basis/ x,m,p,dp \n";*)
(*xstart1a=xstart1a<>"! ::::::::::::::::::::\n";*)

(*
Do[(
xstart1a=xstart1a<>"    DATA dpkeep"<>ToString[i]<>"/ ";
xstart1a=xstart1a<>WriteFListofNumbers[ToExpression["dpkeep"<>ToString[i]]];
xstart1a=xstart1a<>"/\n";
),{i,1,natomsparent}];
*)

xstart1b="
contains

  function emsav(x,c) result(v)
    implicit none
    real(wp),dimension(1:21z)::x
    real(wp),dimension(0:578z)::p
    real(wp),dimension(0:578z)::c
    real(wp)::v 

    call bemsav(x,p)
    v = dot_product(p,c)
    return
  end function emsav

  function gemsav(flag,x,m,p,dp,c,xyz) result(g)
    implicit none
    integer :: flag
    real(wp),dimension(1:21z)::x
    real(wp),dimension(0:239z)::m
    real(wp),dimension(0:578z)::p
    real(wp),dimension(0:578z)::dp
    real(wp),dimension(0:578z)::c
    real(wp)::xyz(12z,3)
    real(wp)::g 
	call EvaldpBasisSet(flag,x,m,p,dp,xyz)
    g = dot_product(dp,c)
    return
  end function gemsav

  subroutine bemsav(x,p)
    implicit none
    real(wp),dimension(1:21z)::x
    real(wp),dimension(0:239z)::m
    real(wp),dimension(0:578z)::p
    ! ::::::::::::::::::::
    
    call evmono(x,m)
    call evpoly(m,p)
    return
  end subroutine bemsav
 
  ";
xstart1=xstart1a<>xstart1b;
xstart3="
  subroutine evmono(x,m)
    implicit none
    real(wp),dimension(1:21z)::x
    real(wp),dimension(0:239z)::m
!::::::::::::::::::::
    ";
    
xmiddle="
    return
end subroutine evmono

  subroutine evpoly(m,p)
    implicit none
    real(wp),dimension(0:239z)::m
    real(wp),dimension(0:578z)::p
!::::::::::::::::::::

";
xending="\n    return\n  end subroutine evpoly\n \n";
xfinal="\n end module bemsa";

(* assemble fortran *)
(* entry to determine distances and x variables *)
xstart2="
      subroutine get_x(xyz,x)
!   xyz must be in Bohr
        implicit none
        integer :: i,j,k
        real(wp) :: xyz(12z,3)
        real(wp),dimension(1:21z)::x

";
Tijnums=GenerateTijnums[];
linedist=Table["",{i,1,Length[Tijnums]},{j,1,3}];
Do[(
linedist[[i,1]]=
	"       x( "<>ToString[i]<>" ) = sqrt((xyz( "<>ToString[Tijnums[[i,1]]]<>
	" , 1 )-xyz( "<>ToString[Tijnums[[i,2]]]<>" , 1 ))**2+ &\n";
linedist[[i,2]]="             (xyz("<>ToString[Tijnums[[i,1]]]<>
	",2)-xyz("<>ToString[Tijnums[[i,2]]]<>",2))**2+ &\n";
linedist[[i,3]]="             (xyz("<>ToString[Tijnums[[i,1]]]<>
	",3)-xyz("<>ToString[Tijnums[[i,2]]]<>",3))**2)\n";
xstart2=xstart2<>linedist[[i,1]]<>linedist[[i,2]]<>linedist[[i,3]];
),{i,1,Length[Tijnums]}];
xstart2=xstart2<>"\n\n";
(*
Do[(
xstart2=xstart2<>"          x("<>ToString[i]<>")=x("<>ToString[i]<>")/0.5291772083_dp\n";
xstart2=xstart2<>"          x("<>ToString[i]<>")=exp(-x("<>ToString[i]<>")/a1)\n";
),{i,1,Length[Tijnums]}];
*)
xstart2=xstart2<>"     x(:) = x(:)/a \n";
xstart2=xstart2<>"     x(:) = dexp (-x(:)) \n";
xstart2=xstart2<>"     end subroutine\n\n\n";

newfortran=CommentXAssignments<>xstart1<>xstart2<>xstart3;
 
(* convert CharStringmono to lines of text *)
Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
CSMlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	Label[AnotherRoundM];
	If[Length[Characters[CSMlines[[i]]]]<= 60,
		newfortran=newfortran<>CSMlines[[i]]<>"\n";
		,
		CSMlinesch=Characters[CSMlines[[i]]];
		Do[(
			If[Or[CSMlinesch[[61-j]]=="-",CSMlinesch[[61-j]]=="+"],
				jsave=j;
				Goto[outM];
			];	
		),{j,1,60}];
		Print["Fortran output mono line longer than 60 char w/o + or -"];
		Abort[];
		Label[outM];
		CSMappend=StringJoin[Take[CSMlinesch,61-jsave+1]]<>" &\n";
		CSMremaining="         "<>StringJoin[Drop[CSMlines[[i]],61-jsave+1]];
		newfortran=newfortran<>CSMappend;
		CSMlines[[i]]=CSMremaining;
		Goto[AnotherRoundM];		
	];
),{i,1,Length[CSMlines]}];
newfortran=newfortran<>xmiddle;

(* convert CharStringpoly to lines of text *)
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
CSPlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
(*Print[i];
Print["      ",newfortran];*)
	Label[AnotherRoundP];
	If[Length[Characters[CSPlines[[i]]]]<= 60,
		newfortran=newfortran<>CSPlines[[i]]<>"\n";
		,
	(*Print["Long Line =",CSPlines[[i]]];*)
		CSPlinesch=Characters[CSPlines[[i]]];
		Do[(
			If[Or[CSPlinesch[[61-j]]=="-",CSPlinesch[[61-j]]=="+"],
		(*Print["            Found +/-  ",j];*)
				jsave=j;
				Goto[outP];
			];	
		),{j,1,60}];
		Print["Fortran output poly line longer than 60 char w/o + or -"];
		Abort[];
		Label[outP];
		CSPappend=StringJoin[Take[CSPlinesch,60-jsave+1]]<>" &\n";
		CSPremaining="          "<>StringJoin[Drop[CSPlinesch,60-jsave+1]];
		newfortran=newfortran<>CSPappend;
		CSPlines[[i]]=CSPremaining;
		Goto[AnotherRoundP];		
	];
),{i,1,Length[CSPlines]}];

newfortran=newfortran<>xending;

newfortran=newfortran<>MakeFdrdxfunction[];
newfortran=newfortran<>MakeFEvaldpBasisSetfunction[];
newfortran=newfortran<>"\n\n";

Do[(
newfortran=newfortran<>ConvertMgroupToFgroup[iatom,ToExpression["Mgroup"<>ToString[iatom]]];
newfortran=newfortran<>"\n\n";
),{iatom,1,natomsparent}];


(* fix dimensions *)
nvar=StringCount[CommentXAssignments,"="];
nmn=StringCount[CharStringmono,"="]-1;
npol=StringCount[CharStringpoly,"="]-1;
z=StringReplace[newfortran,"1:21z"-> "1:"<>ToString[nvar]];
z=StringReplace[z,"0:239z"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:578z"->  "0:"<>ToString[npol]];
z=StringReplace[z,"LEP"->  ToString[npol]];
z=StringReplace[z,"12z,3"-> ToString[natomsparent]<>",3"];
z=StringReplace[z,":: xyz(9,3)"->
	 ":: xyz("<>ToString[natomsparent]<>",3)"];
z=StringReplace[z,":: coeff(3096)"->
	 ":: coeff("<>ToString[npol]<>")"];
z=StringReplace[z,":: x(36)"-> ":: x("<>ToString[nvar]<>")"];
z=StringReplace[z,"do i = 1, 3096"->
	 "do i = 1, "<>ToString[npol]<>""];
(*z=StringDelete[x,"\[IDoubleDot]\[CapitalZHacek]\[Sterling]"];*)
(*
Do[(
	z=StringReplace[z,"Ldpk"<>ToString[i]\[Rule] ToString[ToExpression["Ldpk"<>ToString[i]]]];
),{i,1,natomsparent}];
*)


z=z<>xfinal;


Label[FinishUp];
Print["Done"];
Export[fortranname,z,"Text"];
z
];


MakeExportFortranDDWithChenDerivatives::usage="MakeExportFortranDDWithChenDerivatives[...]
Called From SequentialBulidupWithBatchDerivatives[]

";
MakeExportFortranDDWithChenDerivatives[CommentXAssignments_,CharStringmono_,CharStringpoly_]:=
	Module[
{fnamea,natomsa,rijnamesa,bemsaa,monostarta,monoenda,StringAssigns,
polystarta,polyenda,newfortran,xstart1,xstart2,xstart3,xmiddle,xending,CSMlines,
AnotherRoundM,AnotherRoundP,CSPlines,CSMlinesch,CSPlinesch,jsave,outM,outP,
CSMappend,CSMremaining,CSPappend,CSPremaining,nvar,nmn,npol,z,Tijnums,linedist,
demsav,dbemsav,devmonointro,devmonopoly,devpolyend,FinishUp,xpolyending,
MD,MDlines,PD,PDlines,MDlinesch,PDlinesch,MDappend,MDremaining,PDappend,PDremaining,
xform,nvariables1
},

Print[""];
Print["Preparing to make and export Fortran file with analytic derivatives"];

(* This follows MakeExportFortran file, but it replaces the regular Fortran file with one
that has the derivative functions as well *)



(* enter fortran template pieces*)
xstart1= "
module bemsa
  implicit none
  integer, parameter :: wp  = kind(1.0D0)
  real(wp)::r(12z1047,12z1047)
  real(wp), parameter :: a = 2.d0 
contains
  function emsav(x,c) result(v)
    implicit none
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:578)::p
    real(wp),dimension(0:578)::c
    real(wp)::v
    
    call bemsav(x,p)
    v = dot_product(p,c)
    
    return
  end function emsav

function gemsav(flag,x,m,p,dp,c,xyz) result(g)
    implicit none
    integer :: flag
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:239)::m
    real(wp),dimension(0:578)::p
    real(wp),dimension(0:578)::dp
    real(wp),dimension(0:578)::c
    real(wp)::xyz(12z,3)
    real(wp)::g 
	call EvaldpBasisSet(flag,x,m,p,dp,xyz)
    g = dot_product(dp,c)
    return
  end function gemsav  

Subroutine EvaldpBasisSet(flag,x,m,p,dp,xyz)

        implicit none
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:239)::m
    real(wp),dimension(0:578)::p
    real(wp),dimension(0:578)::dp
    real(wp),dimension(0:578)::c
    real(wp)::xyz(12z,3)
    integer::flag

    call dbemsav(dp,m,p,flag,xyz)
      Return

      END Subroutine EvaldpBasisSet


  subroutine bemsav(x,p)
    implicit none
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:239)::m
    real(wp),dimension(0:578)::p
    ! ::::::::::::::::::::

    
    call evmono(x,m)
    call evpoly(m,p)
    
    return
  end subroutine bemsav
  
  ";
xstart3="
subroutine evmono(x,m)
    implicit none
    real(wp),dimension(1:21)::x
    real(wp),dimension(0:239)::m
   !::::::::::::::::::::\n    ";
xmiddle="
return
end subroutine evmono

subroutine evpoly(m,p)
implicit none
real(wp),dimension(0:239)::m
real(wp),dimension(0:578)::p
!::::::::::::::::::::

";

(* assemble fortran *)
(* entry to determine distances and x variables *)
xstart2="
      subroutine get_x(xyz,x)
!   xyz must be in Bohr
      implicit none
      integer :: i,j,k
      real(wp) :: xyz(12z,3)
      real(wp),dimension(1:21)::x

";
Tijnums=GenerateTijnums[];
linedist=Table["",{i,1,Length[Tijnums]},{j,1,3}];
Do[(
linedist[[i,1]]=
	"       x( "<>ToString[i]<>" ) = sqrt((xyz( "<>ToString[Tijnums[[i,1]]]<>
	" , 1 )-xyz( "<>ToString[Tijnums[[i,2]]]<>" , 1 ))**2+ &\n";
linedist[[i,2]]="             (xyz("<>ToString[Tijnums[[i,1]]]<>
	",2)-xyz("<>ToString[Tijnums[[i,2]]]<>",2))**2+ &\n";
linedist[[i,3]]="             (xyz("<>ToString[Tijnums[[i,1]]]<>
	",3)-xyz("<>ToString[Tijnums[[i,2]]]<>",3))**2)\n";
xstart2=xstart2<>linedist[[i,1]]<>linedist[[i,2]]<>linedist[[i,3]];
),{i,1,Length[Tijnums]}];
xstart2=xstart2<>"\n\n";
(*
Do[(
xstart2=xstart2<>"          x("<>ToString[i]<>")=x("<>ToString[i]<>")/0.5291772083_dp\n";
xstart2=xstart2<>"          x("<>ToString[i]<>")=exp(-x("<>ToString[i]<>")/a1)\n";
),{i,1,Length[Tijnums]}];
*)
xstart2=xstart2<>"
     
     do i=1,12z1047
       r(i,i)=0.d0
       do j=i+1,12z1047
         r(i,j)=sqrt( (xyz(i,1)-xyz(j,1))**2 + (xyz(i,2)-xyz(j,2))**2 + &
			(xyz(i,3)-xyz(j,3))**2 )
         r(j,i)=r(i,j)
       enddo
      enddo

";  (* modified this do loop on 4-Nov-2022 *)
(* modified 11/16/2021 to allow for mixed variable transforms *)
(* we hope that this is backward compatable *)
nvariables1=StringCount[CommentXAssignments,"="];
If[!ValueQ[xtransform],
	xstart2=xstart2<>"     x(:) = x(:)/a \n";
	xstart2=xstart2<>"     x(:) = dexp (-x(:)) \n";
	xstart2=xstart2<>"     end subroutine\n";
	,
	xform="";
	Do[(
	If[xtransform[[i]]==1,
		xform=xform<>"    x("<>ToString[i]<>") = dexp(-x("<>ToString[i]<>")/a) \n";
		,
		xform=xform<>"    x("<>ToString[i]<>") = 1.d0/x("<>ToString[i]<>") \n";
	];
	),{i,1,nvariables1}];
	xstart2=xstart2<>xform<>"\n  end subroutine\n";
];

newfortran=CommentXAssignments<>xstart1<>xstart2<>xstart3;

(* convert CharStringmono to lines of text *)
Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
CSMlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	Label[AnotherRoundM];
	If[Length[Characters[CSMlines[[i]]]]<= 60,
		newfortran=newfortran<>CSMlines[[i]]<>"\n";
		,
		CSMlinesch=Characters[CSMlines[[i]]];
		Do[(
			If[Or[CSMlinesch[[61-j]]=="-",CSMlinesch[[61-j]]=="+"],
				jsave=j;
				Goto[outM];
			];	
		),{j,1,60}];
		Print["1. Fortran output mono line longer than 60 char w/o + or -"];
		Abort[];
		Label[outM];
		CSMappend=StringJoin[Take[CSMlinesch,61-jsave+1]]<>" &\n";
		CSMremaining="         "<>StringJoin[Drop[CSMlinesch,61-jsave+1]];
		newfortran=newfortran<>CSMappend;
		CSMlines[[i]]=CSMremaining;
		Goto[AnotherRoundM];		
	];
),{i,1,Length[CSMlines]}];
newfortran=newfortran<>xmiddle;

(* convert CharStringpoly to lines of text *)
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
CSPlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
(*Print[i];
Print["      ",newfortran];*)
	Label[AnotherRoundP];
	If[Length[Characters[CSPlines[[i]]]]<= 60,
		newfortran=newfortran<>CSPlines[[i]]<>"\n";
		,
	(*Print["Long Line =",CSPlines[[i]]];*)
		CSPlinesch=Characters[CSPlines[[i]]];
		Do[(
			If[Or[CSPlinesch[[61-j]]=="-",CSPlinesch[[61-j]]=="+"],
		(*Print["            Found +/-  ",j];*)
				jsave=j;
				Goto[outP];
			];	
		),{j,1,60}];
		Print["2. Fortran output poly line longer than 60 char w/o + or -"];
		Abort[];
		Label[outP];
		CSPappend=StringJoin[Take[CSPlinesch,60-jsave+1]]<>" &\n";
		CSPremaining="          "<>StringJoin[Drop[CSPlinesch,60-jsave+1]];
		newfortran=newfortran<>CSPappend;
		CSPlines[[i]]=CSPremaining;
		Goto[AnotherRoundP];		
	];
),{i,1,Length[CSPlines]}];
xpolyending="\n    return\n  end subroutine evpoly\n";
newfortran=newfortran<>xpolyending;

(* put in the derivative functions here *)

demsav="
function demsav(c,m,p,flag,xyz) result(grad)
    implicit none
    real(wp),dimension(0:578)::c
    real(wp),dimension(0:239)::m
    real(wp),dimension(0:578)::p
    real(wp)::grad
    real(wp) :: xyz(12z,3)
    integer::flag
    ! ::::::::::::::::::::
    real(wp),dimension(0:578)::dp

    call dbemsav(dp,m,p,flag,xyz)
    grad = dot_product(dp,c)

    return
  end function demsav
";
newfortran=newfortran<>demsav;

dbemsav="
  subroutine dbemsav(dp,m,p,flag,xyz)
    implicit none
    real(wp),dimension(0:578)::dp
    real(wp),dimension(0:239)::m
    real(wp),dimension(0:578)::p
    real(wp) :: xyz(12z,3)
    integer::flag
    ! ::::::::::::::::::::
    real(wp),dimension(0:239)::dm

    call devmono(dm,m,flag,xyz)
    call devpoly(dm,p,dp)

    return
  end subroutine dbemsav
";
newfortran=newfortran<>dbemsav;


devmonointro="
  subroutine devmono(dm,m,flag,xyz)
    implicit none
    real(wp),dimension(0:239)::dm
    real(wp),dimension(0:239)::m
	real(wp) :: xyz(12z,3)
    integer::flag
    !::::::::::::::::::::
    
";
newfortran=newfortran<>devmonointro;

(* get mono derivatives and insert & if needed *)
MD=GetFMonoDerivatives[];
(* convert CharStringmono to lines of text *)
Export[DataDir<>"DeleteMe.txt",MD,"Text"];
MDlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	Label[AnotherRoundM];
	If[Length[Characters[MDlines[[i]]]]<= 60,
		newfortran=newfortran<>MDlines[[i]]<>"\n";
		,
		MDlinesch=Characters[MDlines[[i]]];
		Do[(
			If[Or[MDlinesch[[61-j]]=="-",MDlinesch[[61-j]]=="+"],
				jsave=j;
				Goto[outM];
			];	
		),{j,1,60}];
		Print["3. Fortran output mono line longer than 60 char w/o + or -"];
		Print[MDlines[[i]]];
		Abort[];
		Label[outM];
		MDappend=StringJoin[Take[MDlinesch,61-jsave+1]]<>" &\n";
		MDremaining="         "<>StringJoin[Drop[MDlinesch,61-jsave+1]];
		newfortran=newfortran<>MDappend;
		MDlines[[i]]=MDremaining;
		Goto[AnotherRoundM];		
	];
),{i,1,Length[MDlines]}];
 

devmonopoly="

    return
  end subroutine devmono

  subroutine devpoly(dm,p,dp)
    implicit none
    real(wp),dimension(0:239)::dm
    real(wp),dimension(0:578)::p
    real(wp),dimension(0:578)::dp
!::::::::::::::::::::

    dp(0) = 0.d0
";
newfortran=newfortran<>devmonopoly;

(* get poly derivatives and insert & if needed *)
PD=GetFPolyDerivatives[];
(* convert CharStringmono to lines of text *)
Export[DataDir<>"DeleteMe.txt",PD,"Text"];
PDlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
	Label[AnotherRoundM];
	If[Length[Characters[PDlines[[i]]]]<= 60,
		newfortran=newfortran<>PDlines[[i]]<>"\n";
		,
		PDlinesch=Characters[PDlines[[i]]];
		Do[(
			If[Or[PDlinesch[[61-j]]=="-",PDlinesch[[61-j]]=="+"],
				jsave=j;
				Goto[outM];
			];	
		),{j,1,60}];
		Print["4. Fortran output poly line longer than 60 char w/o + or -"];
		Abort[];
		Label[outM];
		PDappend=StringJoin[Take[PDlinesch,61-jsave+1]]<>" &\n";
		PDremaining="         "<>StringJoin[Drop[PDlinesch,61-jsave+1]];
		newfortran=newfortran<>PDappend;
		PDlines[[i]]=PDremaining;
		Goto[AnotherRoundM];		
	];
),{i,1,Length[PDlines]}];


devpolyend="

    return
  end subroutine devpoly

";
newfortran=newfortran<>devpolyend;
newfortran=newfortran<>MakeFdrdxfunction[];
xending="\n end module bemsa";
newfortran=newfortran<>xending;

(* fix dimensions *)
nvar=StringCount[CommentXAssignments,"="];
nmn=StringCount[CharStringmono,"="]-1;
npol=StringCount[CharStringpoly,"="]-1;
z=StringReplace[newfortran,"1:21"-> "1:"<>ToString[nvar]];
z=StringReplace[z,"0:239"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:578"->  "0:"<>ToString[npol]];
z=StringReplace[z,"12z,3"-> ToString[natomsparent]<>",3"];
z=StringReplace[z,":: xyz(9,3)"->
	 ":: xyz("<>ToString[natomsparent]<>",3)"];
z=StringReplace[z,":: coeff(3096)"->
	 ":: coeff("<>ToString[npol]<>")"];
z=StringReplace[z,":: x(36)"-> ":: x("<>ToString[nvar]<>")"];
z=StringReplace[z,"do i = 1, 3096"->
	 "do i = 1, "<>ToString[npol]<>""];
z=StringReplace[z,"12z1047"->  ToString[natomsparent]];

Export[fortranname,z,"Text"];

Label[FinishUp];
Print["Done"];
Export[fortranname,z,"Text"];
z
];


MakeFEvaldpBasisSetfunction::usage="MakeFEvaldpBasisSetfunction,
PrepareBasisSetInfo[] must have been executed first

";
MakeFEvaldpBasisSetfunction[]:=Module[
{f,function,function1,function2},

function1="    Subroutine EvaldpBasisSet(flag,x,m,p,dp,xyz)
!Inputs: 
!flag:   the xyz coordinate that you want the derivative wrt;
!        deriv = dE/dm
!xyz:    this is global and has the current cart coordinates 
!    of the geometry in groups of 3 in 
!   the order of 1,2,3,4, etc,
!   where these numbers have been assigned to the atoms
!   of the parent compound.  {{1x,1y,1z},{2x,2y,2z},...}
!		
!   Assumption: PreparedpBasisSetInfo[] has been executed

        implicit none
        integer i,flag,matom,xyzind
        real(wp),dimension(1:21z)::x
        real(wp),dimension(0:239z)::m
        real(wp),dimension(0:578z)::p
        real(wp),dimension(0:578z)::dp
        real(wp)::xyz(12z,3)

";
(*
function1=function1<>"        Common /keep/ &\n";
Do[(
	function1=function1<>"          dpkeep"<>ToString[i]<>", &\n";	
),{i,1,natomsparent-1}];
function1=function1<>"          dpkeep"<>ToString[natomsparent]<>"\n";
*)


function2="
        matom=INT((dble(flag)-0.00001d0)/3)+1
        xyzind=MOD(flag-1,3)+1

";
f="
		Do i=0,LEP
			dp(i)=0.D0
		End Do
        CHOOSE_GROUP: SELECT CASE (matom)
";
Do[(
	ToExpression["dpkeepnow=dpkeep"<>ToString[i]];
	f=f<>"        CASE("<>ToString[i]<>")\n";
	f=f<>"          Call Mgroup"<>ToString[i]<>" (flag,x,m,p,dp,xyz)\n";
(*	f=f<>"          dkeepnow = dkeep"<>ToString[i]<>"\n";*)
(*	f=f<>"          Lengthdpkeepnow="<>ToString[Length[dpkeepnow]]<>"\n";*)
),{i,1,natomsparent}];  
f=f<>"        END SELECT CHOOSE_GROUP";      
       function=function1<>function2<>f;
function=function<>"

!Calling the Mgroupx evaluates all of the variables necessay
!to evaluate the next command
      Return

      END Subroutine EvaldpBasisSet
";

function 
];



ConvertMgroupToFgroup[iatom_,Mgroup_]:=Module[
	{Fgroup,Fgroupstart,Fgroupend,Fgrouplines,Fgroupremaining,
Fgroupappend,newFgroup,Fgrouplinesch,outM,AnotherRoundM,jsave
},

	(* Here's a F version: *)
	Fgroup="      "<>StringReplace[Mgroup,";"->"\n     "];
  Fgroup=StringReplace[Fgroup,"["->"("];
	Fgroup=StringReplace[Fgroup,"]"->")"]; 
  Fgroup=StringReplace[Fgroup,"m"->"m("];
	(*Fgroup=StringReplace[Fgroup,"="->") = "]; This is what was there *)
	Fgroup=StringReplace[Fgroup,"="->")="];
	Fgroup=StringReplace[Fgroup,"p"->"p("];
	Fgroup=StringReplace[Fgroup,"+ 0."->""];
   Fgroup=StringReplace[Fgroup,"- 0."->""];
   Fgroup=StringReplace[Fgroup,"*"->")*"];
	Fgroup=StringReplace[StringReplace[Fgroup,"0.  + "->""],"\n"->")\n"];
	Fgroup=StringReplace[StringReplace[Fgroup,"0. + "->""]," )"->")"];
	(*Fgroup=StringReplace[StringReplace[Fgroup," + "->") + "]," - "->") - "]; This is what was there *)
	Fgroup=StringReplace[StringReplace[Fgroup,"+"->")+"],"-"->")-"];
	Fgroup=StringReplace[Fgroup,"))"->")"];
	Fgroup=StringReplace[Fgroup,"=)"->"="];  (* I added this *)
	Fgroup=StringReplace[Fgroup,"  )"->")"];
Fgroup=StringReplace[Fgroup,"= +"->"= "];
Fgroup=StringReplace[Fgroup,"=+"->"="];  (* I added this *)
Fgroup=StringReplace[Fgroup,"+"->")+"];
Fgroup=StringReplace[Fgroup,"-"->")-"];
Fgroup=StringReplace[Fgroup,"= )-"->"= -"];
Fgroup=StringReplace[Fgroup,"=)-"->"=-"];  (* I added this *)
Fgroup=StringReplace[Fgroup,"/a)"->")/a"];
Fgroup=StringReplace[Fgroup,"))"->")"];  (* I added this *)
Fgroup=StringReplace[Fgroup,"=)"->"="];    (* Added this *)
Fgroup=StringReplace[Fgroup,"+)+"->"+"];  (* Added this *)
Fgroup=StringReplace[Fgroup,"+)"->""];  (* Added this *)
(*PLHFG=Fgroup;*)
(* convert Fgroup to Lines of Text *)
Export[DataDir<>"DeleteMe.txt",Fgroup,"Text"];
Fgrouplines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
(*PLHFGL=Fgrouplines;*)
Do[(
	If[StringContainsQ[Fgrouplines[[i]],"drdx"],
		Fgrouplines[[i]]=
			StringDrop[Fgrouplines[[i]],-1]<>",xyz)";
	];
),{i,1,Length[Fgrouplines]}];

(* insert continuation marks,  if needed *)
newFgroup="";
Do[(
	Label[AnotherRoundM];
	If[Length[Characters[Fgrouplines[[i]]]]<= 60,
		newFgroup=newFgroup<>Fgrouplines[[i]]<>"\n";
		,
		Fgrouplinesch=Characters[Fgrouplines[[i]]];
		Do[(
			If[Or[Fgrouplinesch[[61-j]]=="-",Fgrouplinesch[[61-j]]=="+"],
				jsave=j;
				Goto[outM];
			];	
		),{j,1,60}];
		Print["Fortran output Fgroup line longer than 60 char w/o + or -"];
		Abort[];
		Label[outM];
		Fgroupappend=StringJoin[Take[Fgrouplinesch,61-jsave]]<>" &\n";
		Fgroupremaining="         "<>StringJoin[Drop[Fgrouplinesch,61-jsave]];
		newFgroup=newFgroup<>Fgroupappend;
		Fgrouplines[[i]]=Fgroupremaining;
		Goto[AnotherRoundM];		
	];
      ),{i,1,Length[Fgrouplines]}];

Fgroupstart="
      Subroutine MGroup"<>ToString[iatom]<>" (flag,x,m,p,dp,xyz)
      implicit none
      integer :: flag
      real(wp),dimension(0:239z)::dm
      real(wp),dimension(1:21z)::x
      real(wp),dimension(0:239z)::m
      real(wp),dimension(0:578z)::p
      real(wp),dimension(0:578z)::dp
      real(wp)::xyz(12z,3)


";
Fgroupend="
      Return
      End Subroutine MGroup"<>ToString[iatom]<>"
";
Fgroup=Fgroupstart<>newFgroup<>Fgroupend;

Fgroup
];



MakePermTablesForFrags[]:=Module[
{nexti,permtable,y,z,nadd},

permtable=Table[{},{i,1,nfragments}];
Do[( 
nadd=0;
Do[(
If[permsym[[ifrag,i]]==1,
y=1;
Goto[nexti];
];
y=permsym[[ifrag,i]];
(*Print["{i,y} = ",{i,y}];*)
z={};
Do[(
z=Append[z,atoms[[ifrag,j+nadd]]];
),{j,1,y}];
(*Print["z = ",z];*)
permtable[[ifrag]]=Append[permtable[[ifrag]],Sort[z]];
Label[nexti];
nadd=nadd+y;
),{i,1,Length[permsym[[ifrag]]]}];

(*permtable=Append[permtable,permtable[[ifrag]]];*)

 ),{ifrag,1,nfragments}]; 

(*DeleteDuplicates[Flatten[DeleteCases[permtable,{}],1]]*)
permtable
];




CheckForPermProblems[permfrags_]:=Module[
{abort,nextfrag,lifragj,lifrag,passed},
abort=False;
Do[(
If[permfrags[[ifrag]]=={},
	Goto[nextfrag];
	,
	lifrag=Length[permfrags[[ifrag]]];
	Do[( (* over j, the index of the number of perm terms for this ifrag *)
	lifragj=Length[permfrags[[ifrag,j]]];
	Do[( (* over k the number of numbers in this term *)
		Do[(  (* over m, the remaining fragments *)
		(*Print["Now considering ",permfrags[[ifrag,j,k]]," in frag ",ifrag," term ",j];
		Print["Comparing it to fragment ",m];*)
		If[MemberQ[atoms[[m]],permfrags[[ifrag,j,k]]] &&
		!MemberQ[permfrags[[m]],permfrags[[ifrag,j]]],
	Print["Fatal: There is a problem in fragment ",ifrag];
	Print["Atom ",permfrags[[ifrag,j,k]]," permutes in fragment ",ifrag,","];
	Print["but in fragment ",m," it either doesn't permute or it permutes"];
	Print["in a different way.  As a result, the final set of basis functions "];
Print["cannot possibly pass the permutation test."];
Print["This situation needs to be be fixed"];
abort=True;
	];
	),{m,ifrag+1,nfragments}];
	),{k,1,lifragj}];
),{j,1,lifrag}];
];
Label[nextfrag]
),{ifrag,1,nfragments}];
passed=If[abort,False,True;]
];



DeleteDuplicatesAndProvideDerivativesIfDesired[]:=Module[
{time,jointatomlist,fname,perms,ats,sym,tx,tsumfname,tsumsym,
tsumatoms,tn,passed,warn,yn},
passed=True;
warn=False;
(* Test input to see if there are mistakes that we can catch early *)
If[!ValueQ[natomsparent],Print["Fatal: natomsparent unassigned"];passed=False;];
If[!ValueQ[natomsfrag],Print["Fatal: natomsfrag unassigned"];passed=False;];
If[!ValueQ[rijnames],Print["Fatal: rijnames unassigned"];passed=False;];
If[!ValueQ[atoms],Print["Fatal: atoms unassigned"];passed=False;];
If[!ValueQ[permsym],Print["Fatal: permsym unassigned"];passed=False;];
If[!ValueQ[filename],Print["Fatal: filename unassigned"];passed=False;];
Do[(
If[!FileExistsQ[filename[[1]]],Print["Fatal: ",filename[[1]]," cannot be found"];
	passed=False;];
),{i,1,nfragments}];
If[!ValueQ[nfragments],Print["Fatal: nfragments unassigned"];passed=False;];
If[!ValueQ[xtransform],Print["Warning: xtransform unassigned"];
	Print["Assuming all Morse"];
	xtransform=Table[1,{i,1,natomsparent (natomsparent-1)/2}];
];
If[!ValueQ[UseSequentialBuildupMethod],Print["Fatal: UseSequentialBuildupMethod unassigned"];passed=False;];
If[!ValueQ[ProvideDerivatives],Print["Fatal: ProvideDerivatives unassigned"];passed=False;];
If[!ValueQ[UseBatchDerivatives],Print["Fatal: UseBatchDerivatives unassigned"];passed=False;];
If[!ValueQ[fortranname],Print["Fatal: fortranname unassigned"];passed=False;];
If[FileExistsQ[fortranname],Print["Warning, continuing will overwrite ",fortranname];
	warn=True;];
If[!ValueQ[UseRunTests],Print["Warning: UseRunTests unassigned"];
	Print["Assuming UseRunTests=True"];
	UseRunTests=True;warn=True;];
If[!ValueQ[DataDir],Print["Fatal: DataDir unassigned"];passed=False;];

jointatomlist={};
Do[(
fname=filename[[j]];
perms=permsym[[j]];
ats=atoms[[j]];
sym=StringDrop[fname,StringPosition[fname,"bemsa"][[1,2]]];
tx=StringPosition[sym,"_"];
sym=StringDrop[sym,-(StringLength[sym]-tx[[1,2]]+1)];
       tn=ToExpression[Characters[sym]];
tsumfname=Sum[tn[[i]],{i,1,Length[tn]}];
tsumsym=Sum[perms[[i]],{i,1,Length[perms]}];
tsumatoms=Length[ats];
(*Print[{tsumfname,tsumsym,tsumatoms}];*)
If[Or[tsumfname!=tsumsym ,tsumfname!=tsumatoms],
Print["Fatal: Data entry incorrect for fragment ",j];
	If[tsumfname!=tsumsym,
		Print["bemsa file appears not to be the one needed for permsym[[",j,"]]"];
	];
	If[tsumfname!=tsumatoms,
	Print["bemsa file appears not be the one needed for the number of atoms in atoms[[",j,
	"]]"];
	];
	If[tsumsym!=tsumatoms,
		Print["The number of atoms in permsym[[",j,
		"]] does not equal the number of atoms in atoms[[",j,"]]"];
	];
passed=False;
];
jointatomlist=Append[jointatomlist,atoms[[j]]];
),{j,1,nfragments}];


jointatomlist=Sort[DeleteDuplicates[Flatten[jointatomlist]]];
If [Or[Max[jointatomlist]!=natomsparent,Length[jointatomlist]!= natomsparent],
	Print[""];
	Print[""];
	Print[" Warning: at least one atom is missing from the joint atom list *******"];
	Print["jointatomlist = ",jointatomlist];
	warn=True;
];

(* test for bad permutation assignments *)
permfrags=MakePermTablesForFrags[];
If[!CheckForPermProblems[permfrags],passed=False;];

If[!passed,
	Print["Check of input found fatal flaws -- Aborting"];
Abort[];
,
If[warn,
Print["See Dialog Box"];
yn=InputString[Style["Warnings were found: Should the program continue: y/n, n to abort",Red,Bold,FontSize->14]];
If[yn=="n", Print["Aborting so as to allow change"];Abort[];];
,
Print["Input passed Checks -- Continuing"];
];
];




(* End of tests *)
(* __________________________________________________________*)


If[UseSequentialBuildupMethod,
	time=Timing[{CommentXAssignments,CharStringmono,CharStringpoly}=SequentialBuildup[]][[1]];
	,
	time=Timing[{CommentXAssignments,CharStringmono,CharStringpoly}=PairwiseMethod[]][[1]];
];
Print["The program took ",time," sec. = ",time/60," min."];

If[ProvideDerivatives,
	If[UseBatchDerivatives,
		time=Timing[
		MakeExportFortranDDWithBatchDerivatives[CommentXAssignments,CharStringmono,CharStringpoly];
		][[1]];
		,
		time=Timing[
		MakeExportFortranDDWithChenDerivatives[CommentXAssignments,CharStringmono,CharStringpoly];
		][[1]];	
	];
	Print["The derivatives program took ",time," sec. = ",time/60," min."];
];
If[UseRunTests,RunTests[];];
(* no output*)
];



MorseDist::usage="
	Produces output file of {k,i,j,m} where k is the data point of the data set,
    i and j signify the atom numbers of the morse variables, and m is the value of the morse variable.  Thus, mv[[k,i,j,3]] gives the morse variable value for the kth geometry and the ith and jth atoms (j>i)

";
MorseDist[natoms_,data_]:=Module[
{npoints,Skipk,n,mv,r,datapt},
npoints=Length[data]/(natoms+2);
If[npoints!=IntegerPart[npoints],
	Print["Problem with npoints; aborting"];
	Abort[];
];
(* this table is inefficient, but the elements will correspond to the indices *)
mv=Table[{i,j,0},{k,1,npoints},{i,1,natoms},{j,1,natoms}];
n=0;
r=Table[{0,0,0},{p,1,natoms}];
datapt=0;
Do[( (* over k to length of data *)
n=n+1;
(*Print["{k,n} = ",{k,n}];*)
If[n==1,Goto[Skipk]];
If[n==2,Goto[Skipk]];

Do[(
If[n==m,
r[[m-2]]={data[[k,2]],data[[k,3]],data[[k,4]]}/AperBohr;
(*Print["{k,n,m,r[[m-2]]} = ",{k,n,m,r[[m-2]]}];*)
];
),{m,3,natoms+2}];

If[n==natoms+2,
datapt=datapt+1;
n=0;
Do[( (* over i *)
Do[( (* over j *)
mv[[datapt,i,j,3]]=Exp[-EuclideanDistance[r[[i]],r[[j]]]/2];
),{j,i+1,natoms}];
),{i,1, natoms-1}];
r=Table[{0,0,0},{p,1,natoms}];
];

Label[Skipk];
),{k,1,Length[data]}];
mv
];




MorseDistGM::usage="
	Produces output file of {k,i,j,m} where k is the data point of the data set,
    i and j signify the atom numbers of the morse variables, and m is the value of the morse variable.  Thus, mv[[k,i,j,3]] gives the morse variable value for the kth geometry and the ith and jth atoms (j>i).  When we assume that k=1  is the GM, then the output file gives the morse variables for the GM
	This looks just like MorseDist, except it only calculates the Morse variables for the 
	first entry, which is assumed to be the GM
";
MorseDistGM[natoms_,data_]:=Module[
{npoints,Skipk,n,mv,r,datapt},
npoints=Length[data]/(natoms+2);
If[npoints!=IntegerPart[npoints],
	Print["Problem with npoints; aborting"];
	Abort[];
];
(* this table is inefficient, but the elements will correspond to the indices *)
mv=Table[{i,j,0},{k,1,npoints},{i,1,natoms},{j,1,natoms}];
n=0;
r=Table[{0,0,0},{p,1,natoms}];
datapt=0;
Do[( (* over k to length of data *)
n=n+1;
(*Print["{k,n} = ",{k,n}];*)
If[n==1,Goto[Skipk]];
If[n==2,Goto[Skipk]];

Do[(
If[n==m,
r[[m-2]]={data[[k,2]],data[[k,3]],data[[k,4]]}/AperBohr;
(*Print["{k,n,m,r[[m-2]]} = ",{k,n,m,r[[m-2]]}];*)
];
),{m,3,natoms+2}];

If[n==natoms+2,
datapt=datapt+1;
n=0;
Do[( (* over i *)
Do[( (* over j *)
mv[[datapt,i,j,3]]=Exp[-EuclideanDistance[r[[i]],r[[j]]]/2];
),{j,i+1,natoms}];
),{i,1, natoms-1}];
r=Table[{0,0,0},{p,1,natoms}];
];

Label[Skipk];
),{k,1,natoms+2}];
mv
];



MorseDistGML::usage="
	Produces output file of {k,i,j,m} where k is the data point of the data set,
    i and j signify the atom numbers of the morse variables, and m is the value of the morse variable.  Thus, mv[[k,i,j,3]] gives the morse variable value for the kth geometry and the ith and jth atoms (j>i).  When we assume that k=1  is the GM, then the output file gives the morse variables for the GM
    loc is the location of the line containing the minimum energy in data
";
MorseDistGML[natoms_,data_,loc_]:=Module[
{npoints,Skipk,n,mv,r,datapt},
npoints=Length[data]/(natoms+2);
If[npoints!=IntegerPart[npoints],
	Print["Problem with npoints; aborting"];
	Abort[];
];
(* this table is inefficient, but the elements will correspond to the indices *)
mv=Table[{i,j,0},{k,1,npoints},{i,1,natoms},{j,1,natoms}];
n=0;
r=Table[{0,0,0},{p,1,natoms}];
datapt=0;
Do[( (* over k to length of data *)
(*Print[data[[k]]];*)
n=n+1;
(*Print["{k,n} = ",{k,n}];*)
If[n==1,Goto[Skipk]];
If[n==2,Goto[Skipk]];

Do[(
If[n==m,
r[[m-2]]={data[[k,2]],data[[k,3]],data[[k,4]]}/AperBohr;
(*Print["{k,n,m,r[[m-2]]} = ",{k,n,m,r[[m-2]]}];*)
];
),{m,3,natoms+2}];

If[n==natoms+2,
datapt=datapt+1;
n=0;
Do[( (* over i *)
Do[( (* over j *)
mv[[datapt,i,j,3]]=Exp[-EuclideanDistance[r[[i]],r[[j]]]/2];
),{j,i+1,natoms}];
),{i,1, natoms-1}];
r=Table[{0,0,0},{p,1,natoms}];
];

Label[Skipk];
),{k,loc-1,loc+natoms}];
mv
];



GetMorseAvSDMaxOrderedMarkUnused[mv_,gm_,natoms_]:=Module[
{npoints,Av,SD,Tout,xx,uu,str,st1,st2},
npoints=Dimensions[mv][[1]];
Tout=Table[{i,j,0,0,0,0,0},{i,1,natoms},{j,1,natoms}];
uu={};
Do[(
str=Globalunused[[i]];
st1=StringDrop[str,-2];
st2=StringDrop[str,2];
uu=Append[uu,{ToExpression[st1],ToExpression[st2]}];
),{i,1,Length[Globalunused]}];
Do[(
Do[(
Av=Mean[mv[[All,i,j,3]]];
SD=StandardDeviation[mv[[All,i,j,3]]];
max=Max[mv[[All,i,j,3]]];
If[MemberQ[uu,{i,j}],
Tout[[i,j,1]]="**"<>ToString[i];
];
Tout[[i,j,3]]=gm[[1,i,j,3]];
Tout[[i,j,4]]=Av;
Tout[[i,j,5]]=SD;
Tout[[i,j,6]]=NumberForm[SD/Av,{3,2}];
Tout[[i,j,7]]=max;
),{j,i+1,natoms}];
),{i,1,natoms-1}];
Tout=Partition[Flatten[Tout],7];
xx=Sort[Tout,#1[[4]]>#2[[4]] &];
Do[(
If[xx[[i,4]]==0,xx=Drop[xx,{i}]];
),{i,Length[xx],1,-1}];
xx
];



GetMorseAvSDOrderedMarkUnused[mv_,gm_,natoms_]:=Module[
{npoints,Av,SD,Tout,xx,uu,str,st1,st2},
npoints=Dimensions[mv][[1]];
Tout=Table[{i,j,0,0,0,0},{i,1,natoms},{j,1,natoms}];
uu={};
Do[(
str=Globalunused[[i]];
st1=StringDrop[str,-2];
st2=StringDrop[str,2];
uu=Append[uu,{ToExpression[st1],ToExpression[st2]}];
),{i,1,Length[Globalunused]}];
Do[(
Do[(
Av=Mean[mv[[All,i,j,3]]];
SD=StandardDeviation[mv[[All,i,j,3]]];
If[MemberQ[uu,{i,j}],
Tout[[i,j,1]]="**"<>ToString[i];
];
Tout[[i,j,3]]=gm[[1,i,j,3]];
Tout[[i,j,4]]=Av;
Tout[[i,j,5]]=SD;
Tout[[i,j,6]]=NumberForm[SD/Av,{3,2}];
),{j,i+1,natoms}];
),{i,1,natoms-1}];
Tout=Partition[Flatten[Tout],6];
xx=Sort[Tout,#1[[4]]>#2[[4]] &];
Do[(
If[xx[[i,4]]==0,xx=Drop[xx,{i}]];
),{i,Length[xx],1,-1}];
xx
];



GetMorseAvSDOrdered[mv_,gm_,natoms_]:=Module[
{npoints,Av,SD,Tout,xx},
npoints=Dimensions[mv][[1]];
Tout=Table[{i,j,0,0,0,0},{i,1,natoms},{j,1,natoms}];
Do[(
Do[(
Av=Mean[mv[[All,i,j,3]]];
SD=StandardDeviation[mv[[All,i,j,3]]];
Tout[[i,j,4]]=Av;
Tout[[i,j,3]]=gm[[1,i,j,3]];
Tout[[i,j,5]]=SD;
Tout[[i,j,6]]=NumberForm[SD/Av,{3,2}];
),{j,i+1,natoms}];
),{i,1,natoms-1}];
Tout=Partition[Flatten[Tout],6];
xx=Sort[Tout,#1[[4]]>#2[[4]] &];
Do[(
If[xx[[i,4]]==0,xx=Drop[xx,{i}]];
),{i,Length[xx],1,-1}];
xx
];



GetMorseAvSD[mv_,natoms_]:=Module[
{npoints,Av,SD,Tout,xx},
npoints=Dimensions[mv][[1]];
Tout=Table[{i,j,0,0},{i,1,natoms},{j,1,natoms}];
Do[(
Do[(
Av=Mean[mv[[All,i,j,3]]];
SD=StandardDeviation[mv[[All,i,j,3]]];
Tout[[i,j,3]]=Av;
Tout[[i,j,4]]=SD;
),{j,i+1,natoms}];
),{i,1,natoms-1}];
Tout
];



GetMorseAvSDMax[mv_,natoms_]:=Module[
{npoints,Av,SD,Tout,xx,max},
npoints=Dimensions[mv][[1]];
Tout=Table[{i,j,0,0,0},{i,1,natoms},{j,1,natoms}];
Do[(
Do[(
Av=Mean[mv[[All,i,j,3]]];
SD=StandardDeviation[mv[[All,i,j,3]]];
max=Max[mv[[All,i,j,3]]];
Tout[[i,j,3]]=Av;
Tout[[i,j,4]]=SD;
Tout[[i,j,5]]=max;
),{j,i+1,natoms}];
),{i,1,natoms-1}];
Tout
];




GetMorseAvMinMax[mv_,natoms_]:=Module[
{npoints,Av,Tout,xx,max,min},
npoints=Dimensions[mv][[1]];
Tout=Table[{i,j,0,0,0},{i,1,natoms},{j,1,natoms}];
Do[(
Do[(
Av=Mean[mv[[All,i,j,3]]];
min=Min[mv[[All,i,j,3]]];
max=Max[mv[[All,i,j,3]]];
Tout[[i,j,3]]=Av;
Tout[[i,j,4]]=min;
Tout[[i,j,5]]=max;
),{j,i+1,natoms}];
),{i,1,natoms-1}];
Tout
];




errAnalysis[databasename_,errname_,natoms_]:=Module[
{print,table,data,err,mtab,xx,etab,Av,min,max,eval,table2,errfound,nummulterr,
multerr},
print=False;
data=Import[databasename,"Table"];
err=Import[errname,"Table"];
mtab=MorseDist[natoms,data];
xx=GetMorseAvMinMax[mtab,natoms];
etab=MorseDist[natoms,err];
table={{"geom","i","j","dbase min","dbase max","err value"}};
table2={};
nummulterr=0;
Do[( 
idyn-(Length[err]/(natoms+2))-ierr; (* global for dynamic *)
errfound=False;
multerr=False;
Do[( 
Do[( 
Av=xx[[i,j,3]];
min=xx[[i,j,4]];
max=xx[[i,j,5]];
eval=etab[[ierr,i,j,3]];
If[Or[eval<min,eval>max],
	If[print,Print["err.xyz point ",ierr,"; dset min/max = ",
		min,"/",max,"; errval(",i,",",j,") = ",eval];];
	table=Append[table,{ierr,i,j,min,max,eval}];
	If[errfound,multerr=True;];
	errfound=True;
	];
),{j,i+1,natoms}];
		),{i,1, natoms-1}];
If[errfound!=True,table2=Append[table2,ierr];];
If[multerr,nummulterr=nummulterr+1]
),{ierr,1,Length[err]/(natoms+2)}];
Print["The number of error geometries with multiple errors is ",nummulterr];
If[Length[table2]>1,
	Print["List of error geometries where all variables were within the database limits:"];
	Print[table2];
	,
	Print["There are no error geometries where all variables were within the database limits"];
];

table
];



errSummary[tab1_]:=Module[
{tab2,errmin,errmax,summary,ipos,igeom},
(* NB tab1 is generated by errAnalysis *)
(* This sorts tab1 by part 2, the value of i *)
tab2=Sort[Drop[tab1,1],#1[[2]]<#2[[2]] &];
(* This splits tab2 into elements based on the value of part 2 *)
tab2=SplitBy[tab2,#[[2]] &];
(* now consider each of these elements and sort and split them based on part 3 *)
summary={{"#errors","i","j","dbmin","dbmax","errmin","errmax; errgeom:","min","med","max"}};
Do[(
(* This sorts tab2[[i]] by part 3, the value of j *)
tab2[[ival]]=Sort[tab2[[ival]],#1[[3]]<#2[[3]] &];
(* This splits tab2 into elements based on the value of part 3 *)
tab2[[ival]]=SplitBy[tab2[[ival]],#[[3]] &];
Do[(
errmin=Min[tab2[[ival,jval,All,6]]];
errmax=Max[tab2[[ival,jval,All,6]]];
(*ipos=Position[tab2[[ival,jval]],errmin][[1,1]];
igeom=tab2[[ival,jval,ipos,1]];*)
tabx=Sort[tab2[[ival,jval]],#1[[6]]<#2[[6]] &];
lx=Length[tabx];
(*Print[tabx[[1,6]]];
Print[tabx[[IntegerPart[lx/2],6]]];
Print[tabx[[lx,6]]];*)
geomin=tabx[[1,1]];
geomed=tabx[[Max[IntegerPart[lx/2],1],1]];
geomax=tabx[[lx,1]];
summary=Append[summary,{Length[tab2[[ival,jval]]],tab2[[ival,jval,1,2]],
	tab2[[ival,jval,1,3]],tab2[[ival,jval,1,4]],tab2[[ival,jval,1,5]],
	errmin,errmax,geomin,geomed,geomax}];
),{jval,1,Length[tab2[[ival]]]}];
),{ival,1,Length[tab2]}];
summary
];



GetMorseAvSDMaxMin[mv_,natoms_]:=Module[
{npoints,Av,SD,Tout,xx,max,min},
npoints=Dimensions[mv][[1]];
Tout=Table[{i,j,0,0,0,0},{i,1,natoms},{j,1,natoms}];
Do[(
Do[(
Av=Mean[mv[[All,i,j,3]]];
SD=StandardDeviation[mv[[All,i,j,3]]];
max=Max[mv[[All,i,j,3]]];
min=Min[mv[[All,i,j,3]]];
Tout[[i,j,3]]=Av;
Tout[[i,j,4]]=SD;
Tout[[i,j,5]]=max;
Tout[[i,j,6]]=min;
),{j,i+1,natoms}];
),{i,1,natoms-1}];
Tout
];





parseterms[nstar_,xallterms_]:=Module[
{xout,positions,xterms,iskip},
(* parses xallterms to find the *'s and separates it into terms *)
positions=DeleteDuplicates[Flatten[StringPosition[xallterms,"*"]]];
xterms=Table["",{i,1,nstar+1}];
Do[(
If[i==1,
xterms[[i]]=StringTake[xallterms,{1,positions[[i]]-1}];
Goto[iskip];
];
If[i==nstar+1 ,
xterms[[i]]=StringTake[xallterms,{positions[[i-1]]+1,StringLength[xallterms]}];
,
xterms[[i]]=StringTake[xallterms,{positions[[i-1]]+1,positions[[i]]-1}];
];
Label[iskip];
),{i,1,nstar+1}];
xterms
];




powerderivative[xterm_]:=Module[
{xin,xout,caretposition,xtermlast,power,newpower,term},
xin=xterm;
caretposition=StringPosition[xterm,"^"][[1,1]];
xtermlast=StringLength[xterm];
power=ToExpression[StringTake[xterm,{caretposition+1,xtermlast}]];
newpower=power-1;
xout=StringReplace[xterm,"^"<>ToString[power]->"^"<>ToString[newpower]];
term=StringTake[xterm,caretposition-1];

xout=ToString[power]<>"*"<>xout<>"*d"<>term
];




productderivative[nterms_,xterms_]:=Module[
{xout,xin},
xout="";
Do[(
xin=xterms;
If[!StringContainsQ[xin[[i]],"^"],
xin[[i]]="d"<>xin[[i]];
,
xin[[i]]=powerderivative[xin[[i]]];
];
xout=xout<>" + ";
Do[(
If[j!=nterms,
xout=xout<>xin[[j]]<>"*";
,
xout=xout<>xin[[j]];
];
),{j,1,nterms}];
),{i,1,nterms}];
xout
];




textconvert::usage="
Converts text strings to different forms
input: textin must be standard Fortran assignments (no Do loops,
	dimension statements, etc.)
input: DataDir is where files can be written, read, deleted

Input options (all entered in quotations.  Quotations omitted in examples):
texttypein=
textnoamp
(or text)     (including textstandard, but in any case, no ampersands)
table         (tables of text strings eg. {m(34),=,x(1)*x(2),+,x(3)*x(4)})
tablestar     (similar to table but mult signs are separated: eg 
                		{m(34),=,x(1),*,x(2),+,x(3),*,x(4)})	
textlines      (each line of text a separate entry)
textstandard   (standard format for lines of Fortran, with no ampersands)
textwithamp    (like textnoamp but may have ampersands)

Output options (all entered in quotations.  Quotations omitted in examples):
texttypeout=
textnoamp
( or text)     (see above)
table          (see above)
tablestar      (see above)
textlines      (see above)
textstandard   (see above, output has no ampersands)

";
textconvert[textin_,texttypein_,texttypeout_,DataDir_]:=Module[
{z,a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5,
e1,e2,e3,e4,e5,f1,f2,f3,f4,f5,finish
},

z=textin;

textintypes={"text","textnoamp","table","tablestar","textlines","textstandard","textwithamp"};
textouttypes={"text","textnoamp","table","tablestar","textlines","textstandard"};
If[!MemberQ[textintypes,texttypein], 
	Print["texttypein = ",texttypein," is not recognized"];Abort[];];
If[!MemberQ[textouttypes,texttypeout], 
	Print["texttypeout = ",texttypeout," is not recognized"];Abort[];];
If[Length[z]!=0 && Or[texttypein=="text", texttypein=="textnoamp",
	texttypein=="textstandard",texttypein=="textwithamp"],
	Print["textin is not of ",texttypein," type"];Abort;];
If[Length[z]== 0 && Or[texttypein=="table", texttypein=="tablestar",
	texttypein=="textlines"],
	Print["textin is not of ",texttypein," type"];Abort;];
	
If[texttypein=="textnoamp" || texttypein=="text",
	If[texttypeout=="textnoamp",Goto[a1];];
	If[texttypeout=="table",Goto[a2];];
	If[texttypeout=="tablestar",Goto[a3];];
	If[texttypeout=="textlines",Goto[a4];];
	If[texttypeout=="textstandard",Goto[a5];];
	,
	If[texttypein== "table",
		If[texttypeout=="textnoamp" || texttypeout=="text",Goto[b1];];
		If[texttypeout=="table",Goto[b2];];
		If[texttypeout=="tablestar",Goto[b3];];
		If[texttypeout=="textlines",Goto[b4];];
		If[texttypeout=="textstandard",Goto[b5];];
		,
		If[texttypein== "tablestar",
			If[texttypeout=="textnoamp" || texttypeout=="text",Goto[c1];];
			If[texttypeout=="table",Goto[c2];];
			If[texttypeout=="tablestar",Goto[c3];];
			If[texttypeout=="textlines",Goto[c4];];
			If[texttypeout=="textstandard",Goto[c5];];
			,
			If[texttypein== "textlines",
				If[texttypeout=="textnoamp" || texttypeout=="text",Goto[d1];];
				If[texttypeout=="table",Goto[d2];];
				If[texttypeout=="tablestar",Goto[d3];];
				If[texttypeout=="textlines",Goto[d4];];
				If[texttypeout=="textstandard",Goto[d5];];
				,
				If[texttypein== "textstandard",		
					If[texttypeout=="textnoamp" || texttypeout=="text",Goto[e1];];
					If[texttypeout=="table",Goto[e2];];
					If[texttypeout=="tablestar",Goto[e3];];
					If[texttypeout=="textlines",Goto[e4];];
					If[texttypeout=="textstandard",Goto[e5];];
					,
					If[texttypein== "textwithamp",		
						If[texttypeout=="textnoamp" || texttypeout=="text",Goto[f1];];
						If[texttypeout=="table",Goto[f2];];
						If[texttypeout=="tablestar",Goto[f3];];
						If[texttypeout=="textlines",Goto[f4];];
						If[texttypeout=="textstandard",Goto[f5];];
];];];];];];

Label[a1];
Goto[finish];

Label[a2]; (* textnoamp to table *)
Export[DataDir<>"DeleteMe.txt",z,"Text"];
z=Import[DataDir<>"DeleteMe.txt",{"Table"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Goto[finish];

Label[a3];  (* textnoamp to tablestar *)
z=StringReplace[z,"**"->"^"];
z=StringReplace[z,"*"->" * "];
z=StringReplace[z,"^"->"**"];
Export[DataDir<>"DeleteMe.txt",z,"Text"];
z=Import[DataDir<>"DeleteMe.txt",{"Table"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Goto[finish];

Label[a4];  (* textnoamp to textlilnes *)
Export[DataDir<>"DeleteMe.txt",z,"Text"];
z=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Goto[finish];

Label[a5];  (* textnoamp to textstandard *)
z=StringDelete[z,"\t"];
z=StringReplace[z," "->""];
z=StringReplace[z,"="->" = "];
z=StringReplace[z,"+"->" + "];
z=StringReplace[z,"-"->" - "];
z=StringReplace[z,"( - "->"(-"];
z=StringReplace[z,"**"->" ^ "];
Export[DataDir<>"DeleteMe.txt",z,"Text"];
z=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Do[(
z[[i]]="    "<>z[[i]];
),{i,1,Length[z]}];
Export[DataDir<>"DeleteMe.txt",z,{"Text","Lines"}];
z=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Goto[finish];

Label[b1];  (* table to textnoamp *)
Export[DataDir<>"DeleteMe.txt",z,{"Table"}];
z=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
z=StringReplace[z,"\t"->" "];
Goto[finish];

Label[b2];  (* table to table *)
Goto[finish];

Label[b3];  (* table to tablestar *)
z=textconvert[z,"table","textnoamp",DataDir];
Goto[a3];

Label[b4];  (* table to textlines *)
z=textconvert[z,"table","textnoamp",DataDir];
Goto[a4];

Label[b5];  (* table to textstandard *)
z=textconvert[z,"table","textnoamp",DataDir];
Goto[a5];

Label[c1];  (* tablestar to textnoamp *)
Export[DataDir<>"DeleteMe.txt",z,{"Table"}];
z=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
z=StringReplace[z,"\t"->" "];
z=StringReplace[z," * "->"*"];
z=StringReplace[z,"( - "->"(-"];
z=StringReplace[z," ^ "->"^"];
Goto[finish];

Label[c2];  (* tablestar to table *)
z=textconvert[z,"tablestar","textnoamp",DataDir];
Goto[a2];

Label[c3];  (* tablestar to tablestar *)
Goto[finish];

Label[c4];  (* tablestar to textlines *)
z=textconvert[z,"tablestar","textnoamp",DataDir];
Goto[a4];

Label[c5];  (* tablestar to textstandard *)
z=textconvert[z,"tablestar","textnoamp",DataDir];
Goto[a5];

Label[d1];  (* textlines to textnoamp *)
Export[DataDir<>"DeleteMe.txt",z,{"Text","Lines"}];
z=Import[DataDir<>"DeleteMe.txt",{"Text"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
Goto[finish];

Label[d2];  (* textlines to table *)  
z=textconvert[z,"textlines","textnoamp",DataDir];
Goto[a2];

Label[d3];  (* textlines to tablestar *)
z=textconvert[z,"textlines","textnoamp",DataDir];
Goto[a3];

Label[d4];  (* textlines to textlines *)
Goto[finish];

Label[d5];(* textlines to textstandard *)
z=textconvert[z,"textlines","textnoamp",DataDir];
Goto[a5];

Label[e1];  (* textstandard to textnoamp *)
Goto[finish];

Label[e2];  (* textstandard to table *)
Goto[a2];

Label[e3];  (* textstandard to tablestar *)
Goto[a3];

Label[e4];  (* textstandard to textlines *)
Goto[a4];

Label[e5];  (* textstandard to textstandard *)
Goto[finish];


Label[f1];  (* textwithamp to textstandard *)
z=StringReplace[z,"\n"->";"];
z=StringReplace[z,"&;"->""];
z=StringReplace[z,";"->"\n"];
z=textconvert[z,"textnoamp","textstandard",DataDir];
Goto[finish];

Label[f2];  (* textwithamp to tab;e *)
z=textconvert[z,"textwithamp","textnoamp",DataDir];
Goto[a2];

Label[f3];  (* textwithamp to tablestar *)
z=textconvert[z,"textwithamp","textnoamp",DataDir];
Goto[a3];

Label[f4];  (* textwithamp to textlines *)
z=textconvert[z,"textwithamp","textnoamp",DataDir];
Goto[a4];

Label[f5];  (* textwithamp to textstandard *)
z=textconvert[z,"textwithamp","textnoamp",DataDir];
Goto[a5];

Label[finish];
z
];



getdist[xyzfile_,index_]:=Module[
{distindex,xyz},
(* assigns internuclear distances from xyz file and index *)
distindex=0;
xyz=Table[0,{i,1,natomsparent+2},{j,1,4}];
Do[(
Do[(
Do[(
xyz[[i,k-1]]=xyzfile[[index,i+2,k]];
xyz[[j,k-1]]=xyzfile[[index,j+2,k]];
),{k,2,4}];
(*Print["xyz[[",i,"]] = ",xyz[[i]]];
Print["xyz[[",j,"]] = ",xyz[[j]]];*)
distindex=distindex+1;
dist[[distindex]]=Sqrt[(xyz[[i,1]]-xyz[[j,1]])^2+
	        (xyz[[i,2]]-xyz[[j,2]])^2+
                  (xyz[[i,3]]-xyz[[j,3]])^2];
(*Print["dist[[",distindex,"]] = ",dist[[distindex]]];*)
),{j,i+1,natomsparent}];
),{i,1,natomsparent}];
(* no output *)
];







makemforxformDD[nvar_, CharStringmono_]:=Module[
{txt1047,txttab,istart,iend,continueon,txttabred,diagnose,nm,
mforx,xform,nexti,kx},
diagnose=False;


(* make mforx; mforx[[i]] gives index of m where m[[index]\[Equal]x[[i]] *)
(*txt1047=Import[fortranname,"Text"];
Export[DataDir<>"DeleteMe.txt",txt1047,"Text"];
txttab=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];*)
txttab=textconvert[CharStringmono,"textnoamp","table",DataDir];
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
(*Print["Making mforx and xform"];*)
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
(*Print["number of x's in use = ",kx];
Print["mforx = ",mforx];
Print["xform = ",xform];*)

{mforx,xform}
];
