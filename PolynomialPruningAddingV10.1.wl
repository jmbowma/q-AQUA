(* ::Package:: *)

(* Revision History"
10.0     Version in use as of 11/6/2021
		 11/13/2021 changed 60 character limit to 70 in Ampersand (but not
		 in AmpersandPlus)
		 11/18-20/2021 fixed some problems in getting rid or rhs zeros in 
		 PolynomialPrining[]
		 11/23-25/2021 developed a better method for determining the qkeep list, one that gets all the q's that
		 are needed for the q's used in defining the p's. Introduced functions getmkeepviadefinitions, 
		 getmkeepviamcalculation, and getqkeepviadefinitions.  Made p(0) equal to 0.d0.
		 11/30/2021 corrected problem where p was replaced by p with a space after it in Step 2
		 11/30/2021 improved qkeep/mkeep procedure by adding other checks until list stabilizes
		 12/6/2021 finished major changes to PolynomialPruning in order for it to work
		 with purified files.
		 12/7/2021 added recompaction option to PolynomialPruning 
		 1/22/2022 modified to use permutationally invariant water groups
		 (only for the 4b so far) so that we would have fewer polynomials
		 and not need to use the permutationally replicated data set.
		 1/31/2021 fixed some problems encountered when running with non-purified input
		 11/1/2022 fixed small probleem in checking of xtransform length for fragmented bases.
		 11/1/2022 disabled check for Checkxtransformvsinputfname[] in 
		      CheckAppendReverseDerivativesInputs because it does not work when the inputfname
		      file was created by fragmentation.
	     4-Nov-2022  Modified do loops in MakeExportFortranDDWithChenDerivativesPP and
	           MakeExportFortranPure
	     1-Feb-2023  Fixed two bugs concerning reading files when skipgroups=True
10.1     7-Feb-2023  Fixed problem that resulted from puttting q's before p's.  Now they are left intertwined
               so that, even if the q's depend on p's, the p's are calculated first.
         10-Feb-2023  Fixed cmall problem  conceringin ^ in CompactionPure
         13-Feb-2023  Fortran can read at most 511 lines connected with &. The rev 
               derivatives sometimes have more than this.  I changed AmpersandPlusOnly
               to break the sums up into sums of lines no more than a max, currently 
               set to 500 lines.
*)



PolynomialPruning[]:=Module[
{print,pesdata,natoms,data,nenergies,mtab,gm,Tout,
(*xx,ijnow,inow,jnow,PPCharStringpoly,PPCharStringmono, - for some reason, these cannot be local variables*)
xsum,EPsorted,limit,deletelist,keeplist,deletelistgroups,keeplistgroups,
rijnamesP,used,unused,diagnose,nfadd,tabpzero,string,klist,xnew,PPDatapoly,
Skiprhs1,Skiprhs2,time,skip1,skipk2,iinow,nexti,pdeletelist,pnumber,qnumber,mnumber,
skiptoPPCS3,T1,T1sorted,EPd,stopafterT1sorted,skiptopure
},
print=True;
diagnose=False;
If[!ValueQ[stopafterT1sorted],
	stopafterT1sorted=False;  (* if True, aborts after T1sorted table has been written *)
];
If[!purified,nq=0;qnumber=0,qnumbernow=0;];


(* Perform some simple checks of the input data *)
CheckPolynomialPruningInputs[];


	(* The following works, but it is very slow *)
	(*Print["Getting definitions to Mathematica from fortranname"];
	{rijnamesnow,nvariables,mnumber,pnumber,qnumber}=
	GetDefinitionstoMathematicaFromFortranOutputPure[];
	npoly=pnumber;
	nmono=mnumber;
	nq=qnumber;
	(* This one works faster (because it doesn't translate to Mathematica), 
	so as long as you don't need to evaluate anything, it should do *)
	Print["Getting mono/poly character strings from fortranname"];
	{PPCharStringmono,PPCharstringpoly,mnumber,pnumber,qnumber}=
		getCSMCSPfromfortranname[];
	Print["{mnumber,pnumber,qnumber} = ",{mnumber,pnumber,qnumber}];
	(*Goto[skiptopure];*)
];
*)
(* this is the procedure for non-purified or already grouped input files *)
(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print[Style[
"Get Distribution and averages of Morse Variables for ab initio Data Set",
Blue,FontSize->14]]];
(* Initiation *)
pesdata=Import[pesfile,"Table"];
natoms=natomsparent;
data=pesdata;
nenergies=Length[data]/(natoms+2);
Globalunused={};
(* Get the Morse distributions *)
mtab=MorseDist[natoms,data];
gm=MorseDistGM[natoms,data];
(*Get the Averages and Standard Deviations ordered by largest average *)
xx=GetMorseAvMinMax[mtab,natoms];
If[diagnose,Export[DataDir<>"xx",xx,"Table"];];
(* Format of xx is i,j,Av,SD,max,min , where i,j are atom numbers *)
(* *************** *)
(* xxindex 3 of xx is the average value of this variable 
in the ab initio data *)
(* xxindex 5 of xx is the maximum value of this variable 
in the ab initio data *)
(* xxindex 4 of xx is the minimum value of this variable 
in the ab initio data *)
(* If xxindex has been declared in the inpute template, then that value will be used,
otherwise, it will be defined as in the next statement: *)
If[!ValueQ[xxindex],xxindex=5];



(* *************** *)
(* Get Distribution of basis function values *)
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print[Style["Get Distribution of basis function values",Blue,FontSize->14]]];
(* **************** Get Fortran Output Definitions   *******************  *)
	(* get mono poly fortran lists from  fortran file *)
If[ValueQ[purified] && purified,
	{rijnamesnow,nvariables,mnumber,pnumber,qnumber}=
		GetDefinitionstoMathematicaFromFortranOutputPure[];
		npoly=pnumber;
		nmono=mnumber;
		nq=qnumber;
	,
	{rijnamesnow,nvariables,nmono,npoly}=
		GetDefinitionstoMathematicaFromFortranOutput[];
		pnumber=npoly;
		mnumber=nmono;
		qnumber=0;
		nq=0;
];
If[diagnose,Export[DataDir<>"PPCharStringmono1a",PPCharStringmono,"Text"];];
If[diagnose,Export[DataDir<>"PPCharStringpoly1a",PPCharStringpoly,"Text"];];
rijnames[[1]]=rijnamesnow;
If[print,Print["Coordinates now in use:"]];
If[print,Print[rijnamesnow]];
xsum=Length[rijnamesnow];
plist=Table["p"<>ToString[i],{i,0,npoly}];  (* needs to be global *)
mlist=Table["m"<>ToString[i],{i,0,nmono}];  (* needs to be global *)
xlist=Table["x"<>ToString[i],{i,1,nvariables}];  (* needs to be global *)
If[ValueQ[purified] && purified,
	mlistpur={"m0"};
	Do[( 
	 mlistpur=Append[mlistpur,"m"<>ToString[i]];
	),{i,1,mnumber}];
	plistpur={"p0"};
	Do[( 
	 plistpur=Append[plistpur,"p"<>ToString[i]];
	),{i,1,pnumber}];
];
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
(* assign the maximum value of the appropriate morse variable to each 
of the x variables whose atom pairs are in the rijnames now list *)
(* we won't use Assignx[] which creates x values from the appropriate distances *)
(* in stead, we'll assign the xvalues themselves, using information 
from the Morse variable distributions *)
Do[(
ijnow=rijnamesnow[[k]];
inow=ToExpression[StringDrop[ijnow,-2]];
jnow=ToExpression[StringDrop[ijnow,2]];
(*Print["{inow,jnow} = ",{inow,jnow}];*)
ToExpression["x"<>ToString[k]<>"=xx[[inow,jnow,xxindex]];"];  
(* xxindex either comes in through template values or is assigned above to be 5 *)
),{k,1,xsum}];
EM=EvalMono[];
If[ValueQ[purified] && purified,
	EM=EvalMonoNew[PPCharStringmono,mlistpur];
	EP=EvalPolyNew[PPCharStringpoly,plistpur];
	,
	EM=EvalMono[];
	EP=EvalPoly[];
];
If[print,Print["{npoly,maxpoly,minpoly} = ",{Length[EP],Max[EP],Min[EP]}];];




(* *************** *)
(* Cull basis functions with small values *)
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Cull basis functions with small values"]];
(* 
Now cull some of the basis functions by setting to zero the coefficients of EP values that are less than limit
First,set EP to the values it has when it uses the Morse variable max values 
*)
Do[(
ijnow=rijnamesnow[[k]];
inow=ToExpression[StringDrop[ijnow,-2]];
jnow=ToExpression[StringDrop[ijnow,2]];
(*Print["{inow,jnow} = ",{inow,jnow}];*)
ToExpression["x"<>ToString[k]<>"=xx[[inow,jnow,xxindex]];"];
),{k,1,xsum}];
(* Now identify those polynomials whose value is below the limiting value,limit *)
(* old method:
EPsorted=Sort[EP, #1 < #2 &];
If[diagnose, Export[DataDir<>"EPsorted",EPsorted,"Table"];];

limit=EPsorted[[npoly-ncoeffdesired+1]];
deletelist={};
Do[(
If[EP[[i]]<limit,
(*Print[EP[[i]]];*)
deletelist=Append[deletelist,i];
];
),{i,1,npoly}];
*)
(* new method: *)
EPd=Drop[EP,1];
T1=Table[{i,EPd[[i]]},{i,1,npoly}];
T1sorted=Sort[T1, #1[[2]] < #2[[2]] &];
deletelist={};
Do[(
deletelist=Append[deletelist,T1sorted[[i,1]]];
),{i,1,npoly-ncoeffdesired}];
If[print,Print["Length of delete list is ",Length[deletelist]];];
If[diagnose,
	Export[DataDir<>"deletelist",deletelist,"Table"];
	Export[DataDir<>"PPCharStringpoly1",PPCharStringpoly,"Table"];
	Export[DataDir<>"T1sortedTab",T1sorted,"Table"];
];
deletelist=Sort[deletelist];
If[stopafterT1sorted,
Export[DataDir<>"T1sortedTab",T1sorted,"Table"];
Print["Stopping after T1sorted"];Abort[];];

(*Label[skiptopure];*)
(*
(* replace deletelist if it is being otherwise supplied *)
If[ValueQ[deletelistfname], (* this is to maintain backward compatability *)
	Print[Style["Importing deletelist from deletelistfname",Red,FontSize->14]];
	deletelist=Import[deletelistfname,"List"];
	,
	If[purified, (* this in the way new programs should go if purified=True *)
		deletelist=deletelistgroups;
	];
];
*)

(* *************** *)
(* Delete and renumber polynomials (many long steps) *)
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print[Style["Delete and renumber polynomials (many long steps)",
Blue,FontSize->14]];];
If[ValueQ[shortcutfname] && FileExistsQ[shortcutfname],
	PPCharStringpoly=Import[shortcutfname,"Text"];
	If[print,Print[Style[
	"Getting PPCharStringpoly from file shortcutfname",Red]]];
	Goto[skiptoPPCS3];
];
(*If[print,Print["       The time scales as (# deletions)*(# original polys) !"]];
If[print,Print["       This section may take  about ",N[npoly*Length[deletelist]*4 10^-7,{4,2}]," min."]];*)
(*
We now need to mark these polynomials for deletion,delete them,and renumber the results,then generate the derivatives and finally write out a new program (with a new name) that can be used for a fit.
*)
(* PPCharStringpoly and PPCharStrinmono are created as a global string in GetAssignEVMonoPoly  *)
(* consolidate and renumber the p values in the poly lists, renaming common p's to d's *)
If[print,Print["       Change p to d for those on deletion list - see idyn countdown"]];
Do[(
	idyn=i; (* global for dynamic *)
	PPCharStringpoly=StringReplace[PPCharStringpoly,
"p("<>ToString[deletelist[[i]]]<>")"-> 
		"d("<>ToString[deletelist[[i]]]<>")"
];			
),{i,Length[deletelist],1,-1}];
If[diagnose,Export[DataDir<>"PPCharStringpoly1qr",PPCharStringpoly,"text"];];
(* delete lines that have d definitions on the lhs of poly and mono*)
If[print,Print["       Delete all Fortran definitions starting d - see idyn countdown"]];	
PPCharStringpoly=DeleteAllFortranDefinitionsStartingd[PPCharStringpoly];
(* 
What we want to do is to approximate the deleted polynomials by zero
*)
If[print,Print["       Repalce other d's by zero - see idyn countdown"]];
Do[(
	idyn=Length[deletelist]-i;
PPCharStringpoly=StringReplace[PPCharStringpoly,"d("<>ToString[deletelist[[i]]]<>")"-> "(0)"];
),{i,1,Length[deletelist]}];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly2",PPCharStringpoly,"Text"];
];

(* get rid of the zeros *)
If[print,Print["       Get rid of zeros"]];
PPCharStringpoly= StringReplace[PPCharStringpoly,"+ (0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"- (0)"->""];
If[ValueQ[shortcutfname],
	Export[shortcutfname,PPCharStringpoly,"Text"];
];
Label[skiptoPPCS3];

(*  Run1 *************************************************  *)



(* We also need to get rid of terms that have "*(0)" on the rhs; this is 
more difficult since we don't know what comes before "*(0)" *)
(* here's a method *)
(*11/19/2021 Changed:
PPDatapoly=ImportString[PPCharStringpoly,{"Text","Data"}];
to: *) 
PPDatapoly=textconvert[PPCharStringpoly,"text","textlines",DataDir];
(* converts 
PPCharStringpoly to data with each element separated by commas *)
(* get a list of the locations where "*(0)" occurs *)
If[diagnose,
	Export[DataDir<>"PPCharStringpoly3a",PPDatapoly,"Text"];
];
tabpzero={};
Do[(
If[Or[StringContainsQ[PPDatapoly[[i]],"*(0)"]=={True},
StringContainsQ[PPDatapoly[[i]],"*(0)"]==True,
StringContainsQ[PPDatapoly[[i]],"= (0)"]==True],
tabpzero=Append[tabpzero,i];
];
),{i,1,Length[PPDatapoly]}];
Print["     Length of tabpzero = ", Length[tabpzero]];
If[Length[tabpzero]==0, Goto[Skiprhs1];];
(* for each element in tabpzero, find the location(s) of "*(0)"  *)
(* 12/19/2021 added: *)
string=textconvert[PPCharStringpoly,"text","table",DataDir];
If[string[[1,3]]==0,string[[1,3]]="0"];
If[print,Print["       Eliminating ...*(0)... and ...(0)*... on rh sides"]];
Do[(
	(* deleted on 11/18/2021 This:
	string=ImportString[PPDatapoly[[tabpzero[[i]]]][[1]],{"Text","Words"}]; *)
	klist={};
	iinow=tabpzero[[i]];
	Do[(
		(* added or to do *(0) and (0)*   : *)
		If[Or[StringContainsQ[string[[iinow,k]],"*(0)"],
			StringContainsQ[string[[iinow,k]],"(0)*"]],
			klist=Append[klist,k];
		];
	),{k,1,Length[string[[iinow]]]}];
	(* added 11/19/2021 : *)
	(*If[print,Print["    Length of klist = ",Length[klist]]];*)
	If[Length[klist]==0,Goto[skipk1];];
	(*  Delete the contents of that location *)
	(*Print[string[[iinow]]];*)
	Do[(
		string[[iinow]]=Delete[string[[iinow]],{klist[[j]]}];
	),{j,Length[klist],1,-1}];
	(*Print[string[[iinow]]];*)
	Label[skipk1];
	(* Deleted on 11/19/2021 This:
	PPDatapoly[[tabpzero[[i]]]][[1]]=ExportString[string,{"Text","Words"}];*)
	(*Print[PPDatapoly[[tabpzero[[i]]]][[1]]];*)
),{i,1,Length[tabpzero]}];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly3b",string,"Text"];
];
(* The switch between Text and {Text,Data} screws up multiline statements; 
the next loop fixes it *)
(* 12/19/2021 added: *)
PPDatapoly=string;
Do[(
	If[Length[PPDatapoly[[i]]]!=1,
		xnew=PPDatapoly[[i,1]];
		Do[(
			xnew=StringJoin[xnew,PPDatapoly[[i,k]]];
		),{k,2,Length[PPDatapoly[[i]]]}];
		(* 12/19/2021 changed 
		PPDatapoly[[i]]={xnew};  to *)
		PPDatapoly[[i]]=xnew;
	];
),{i,1,Length[PPDatapoly]}];
(* PPCharStringpoly needs to have 4 leading spaces *)
(* 11/19/2021 changed
Do[(
PPDatapoly[[i]]="    "<>PPDatapoly[[i]];
),{i,1,Length[PPDatapoly]}];
(* Get rid of ones with nothing on rhs *)
Do[(
	If[!Or[
		StringContainsQ[PPDatapoly[[i]],"= "],
		StringContainsQ[PPDatapoly[[i]],"=+"],
		StringContainsQ[PPDatapoly[[i]],"=-"]],
		PPDatapoly=Drop[PPDatapoly,{i}];
	];
),{i,Length[PPDatapoly],1,-1}];
PPCharStringpoly=ExportString[Flatten[PPDatapoly],{"Text","Lines"}];
to: *)
 (* replace PPCharStringpoly with terms containing "*(0)" deleted *)
 PPCharStringpoly=textconvert[PPDatapoly,"table","textstandard",DataDir];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly3w",PPCharStringpoly,"Text"];
];
(* there will still be several polys whose rhs evaluates to zero.  *)
string=textconvert[PPCharStringpoly,"text","table",DataDir];
If[string[[1,3]]==0,string[[1,3]]="0"];
Print["   marking empty rhs p() to d() for later deletion "];
deletelist={};
Do[(
	If[string[[ i,Length[string[[i]]] ]]=="(0)" &&
	   string[[ i,Length[string[[i]]]-1 ]]=="=",
	   deletelist=Append[deletelist,i]; 
	   string[[i,1]]=StringReplace[string[[i,1]],"p"->"d"];
	   Goto[nexti];
	  ];
	If[string[[ i,Length[string[[i]]] ]]=="(0)" &&
	  Or[
	    string[[ i,Length[string[[i]]]-1 ]]=="+",
	    string[[ i,Length[string[[i]]]-1 ]]=="-",
	    string[[ i,Length[string[[i]]]-1 ]]=="*"
	    ] && 
	    string[[i,Length[string[[i]]-2]]]=="=",
	    deletelist=Append[deletelist,i]; 
	    string[[i,1]]=StringReplace[string[[i,1]],"p"->"d"];
	    Goto[nexti];
	  ]; 
	If[string[[ i,Length[string[[i]]] ]]=="=",
		deletelist=Append[deletelist,i];
		string[[i,1]]=StringReplace[string[[i,1]],"p"->"d"];
	];
	Label[nexti];
),{i,1,Length[string]}];
(* The delete list is a list of positions in string that need to be deleted. 
It is not a list of the p's.  Convert it to a list of p's: *)
pdeletelist={};
Do[(
	pdeletelist=Append[pdeletelist,
		ToExpression[ StringDrop[StringDrop[string[[deletelist[[i]],1]],2],-1]  ]];
),{i,1,Length[deletelist]}];
Print[" Length of pdeletelist = ",Length[pdeletelist]];
Print[" pdeletelist = ",pdeletelist];
If[diagnose,PLHpdeletelist=pdeletelist];

PPCharStringpoly=textconvert[string,"table","textstandard",DataDir];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly3x",PPCharStringpoly,"Text"];
];
(* mark other p's on the deletelist as (0) *)
Do[(
	PPCharStringpoly=
		StringReplace[PPCharStringpoly,"p("<>ToString[i]<>")"-> "(0)"];
),{i,pdeletelist}];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly4",PPCharStringpoly,"Text"];
];
If[print,Print["       Delete all Fortran definitions starting d - see idyn countdown"]];	
PPCharStringpoly=DeleteAllFortranDefinitionsStartingd[PPCharStringpoly];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly4a",PPCharStringpoly,"Text"];
];
(* get rid of the zeros *)
If[print,Print["       Get rid of  +/- zeros"]];
PPCharStringpoly= StringReplace[PPCharStringpoly,"=  - (0)"->"= (0)"];
PPCharStringpoly= StringReplace[PPCharStringpoly,"=  + (0)"->"= (0)"];
PPCharStringpoly= StringReplace[PPCharStringpoly,"+ (0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"- (0)"->""];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly4b",PPCharStringpoly,"Text"];
];
Label[Skiprhs1];



(*  Run2 *************************************************  *)


If[print,Print["      running through routine to get rid of rhs zeros again, if needed "];];
(* Now do this "whole get rid of rhz zeros" again *)
(* We also need to get rid of terms that have "*(0)" on the rhs; this is 
more difficult since we don't know what comes before "*(0)" *)
(* here's a method *)
(*11/19/2021 Changed:
PPDatapoly=ImportString[PPCharStringpoly,{"Text","Data"}];
to: *) 
PPDatapoly=textconvert[PPCharStringpoly,"text","textlines",DataDir];
(* converts 
PPCharStringpoly to data with each element separated by commas *)
(* get a list of the locations where "*(0)" occurs *)
If[diagnose,
	Export[DataDir<>"PPCharStringpoly3c",PPDatapoly,"Text"];
];
tabpzero={};
Do[(
If[Or[StringContainsQ[PPDatapoly[[i]],"*(0)"]=={True},
StringContainsQ[PPDatapoly[[i]],"*(0)"]==True,
StringContainsQ[PPDatapoly[[i]],"= (0)"]==True],
tabpzero=Append[tabpzero,i];
];
),{i,1,Length[PPDatapoly]}];
(* added next two lines 12/19/2021 *)
Print["     Length of tabpzero = ", Length[tabpzero]];
If[Length[tabpzero]==0, Goto[Skiprhs2];];
(* for each element in tabpzero, find the location(s) of "*(0)"  *)
(* 12/19/2021 added: *)
string=textconvert[PPCharStringpoly,"text","table",DataDir];
If[string[[1,3]]==0,string[[1,3]]="0"];
If[print,Print["       Eliminating ...*(0)... and ...(0)*... on rh sides"]];
Do[(
	(* deleted on 11/18/2021 This:
	string=ImportString[PPDatapoly[[tabpzero[[i]]]][[1]],{"Text","Words"}]; *)
	klist={};
	iinow=tabpzero[[i]];
	Do[(
		(* added or to do *(0) and (0)*   : *)
		If[Or[StringContainsQ[string[[iinow,k]],"*(0)"],
			StringContainsQ[string[[iinow,k]],"(0)*"]],
			klist=Append[klist,k];
		];
	),{k,1,Length[string[[iinow]]]}];
	(* added 11/19/2021 : *)
	(*If[print,Print["    Length of klist = ",Length[klist]]];*)
	If[Length[klist]==0,Goto[skipk2];];
	(*  Delete the contents of that location *)
	(* Print[string[[iinow]]]; *)
	Do[(
		string[[iinow]]=Delete[string[[iinow]],{klist[[j]]}];
	),{j,Length[klist],1,-1}];
	(* Print[string[[iinow]]]; *)
	Label[skipk2];
	(* Deleted on 11/19/2021 This:
	PPDatapoly[[tabpzero[[i]]]][[1]]=ExportString[string,{"Text","Words"}];*)
	(*Print[PPDatapoly[[tabpzero[[i]]]][[1]]];*)
),{i,1,Length[tabpzero]}];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly3d",string,"Text"];
];
(* The switch between Text and {Text,Data} screws up multiline statements; 
the next loop fixes it *)
(* 12/19/2021 added: *)
PPDatapoly=string;
Do[(
	If[Length[PPDatapoly[[i]]]!=1,
		xnew=PPDatapoly[[i,1]];
		Do[(
			xnew=StringJoin[xnew,PPDatapoly[[i,k]]];
		),{k,2,Length[PPDatapoly[[i]]]}];
		(* 12/19/2021 changed 
		PPDatapoly[[i]]={xnew};  to *)
		PPDatapoly[[i]]=xnew;
	];
),{i,1,Length[PPDatapoly]}];
(* PPCharStringpoly needs to have 4 leading spaces *)
(* 11/19/2021 changed
Do[(
PPDatapoly[[i]]="    "<>PPDatapoly[[i]];
),{i,1,Length[PPDatapoly]}];
(* Get rid of ones with nothing on rhs *)
Do[(
	If[!Or[
		StringContainsQ[PPDatapoly[[i]],"= "],
		StringContainsQ[PPDatapoly[[i]],"=+"],
		StringContainsQ[PPDatapoly[[i]],"=-"]],
		PPDatapoly=Drop[PPDatapoly,{i}];
	];
),{i,Length[PPDatapoly],1,-1}];
PPCharStringpoly=ExportString[Flatten[PPDatapoly],{"Text","Lines"}];
to: *)
 (* replace PPCharStringpoly with terms containing "*(0)" deleted *)
 PPCharStringpoly=textconvert[PPDatapoly,"table","textstandard",DataDir];
(* there will still be several polys whose rhs evaluates to zero.  *)
string=textconvert[PPCharStringpoly,"text","table",DataDir];
If[string[[1,3]]==0,string[[1,3]]="0"];
Print["   marking empty rhs d() to be deleted "];
deletelist={};
Do[(
	If[string[[ i,Length[string[[i]]] ]]=="(0)" &&
	   string[[ i,Length[string[[i]]]-1 ]]=="=",
	   deletelist=Append[deletelist,i]; 
	   string[[i,1]]=StringReplace[string[[i,1]],"p"->"d"];
	   Goto[nexti];
	  ];
	If[string[[ i,Length[string[[i]]] ]]=="(0)" &&
	  Or[
	    string[[ i,Length[string[[i]]]-1 ]]=="+",
	    string[[ i,Length[string[[i]]]-1 ]]=="-",
	    string[[ i,Length[string[[i]]]-1 ]]=="*"
	    ] && 
	    string[[i,Length[string[[i]]-2]]]=="=",
	    deletelist=Append[deletelist,i]; 
	    string[[i,1]]=StringReplace[string[[i,1]],"p"->"d"];
	    Goto[nexti];
	  ]; 
	If[string[[ i,Length[string[[i]]] ]]=="=",
		deletelist=Append[deletelist,i];
		string[[i,1]]=StringReplace[string[[i,1]],"p"->"d"];
	];
	Label[nexti];
),{i,1,Length[string]}];
PLHdeletelist=deletelist;
(* The delete list is a list of positions in string that need to be deleted. 
It is not a list of the p's.  Convert it to a list of p's: *)
pdeletelist={};
Do[(
	pdeletelist=Append[pdeletelist,
		ToExpression[ StringDrop[StringDrop[string[[deletelist[[i]],1]],2],-1]  ]];
),{i,1,Length[deletelist]}];
Print[" Length of pdeletelist = ",Length[pdeletelist]];
Print[" pdeletelist = ",pdeletelist];
PPCharStringpoly=textconvert[string,"table","textstandard",DataDir];
(* mark other p's on the pdeletelist as (0) *)
Do[(
	PPCharStringpoly=
		StringReplace[PPCharStringpoly,"p("<>ToString[i]<>")"-> "(0)"];
),{i,pdeletelist}];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly4g",PPCharStringpoly,"Text"];
];
If[print,Print["       Delete all Fortran definitions starting d - see idyn countdown"]];	
PPCharStringpoly=DeleteAllFortranDefinitionsStartingd[PPCharStringpoly];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly4h",PPCharStringpoly,"Text"];
];
(* get rid of the zeros *)
If[print,Print["       Get rid of +/- zeros"]];
PPCharStringpoly= StringReplace[PPCharStringpoly,"=  - (0)"->"= (0)"];
PPCharStringpoly= StringReplace[PPCharStringpoly,"=  + (0)"->"= (0)"];
PPCharStringpoly= StringReplace[PPCharStringpoly,"+ (0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"- (0)"->""];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly4i",PPCharStringpoly,"Text"];
];
Label[Skiprhs2];

(* hopefully, this has gotten rid of ALL of the rhs zeros *)

(*  End of Run 2 *********************************** *)


(* renumber the polynomials  *)
(*        Renumber polynomials - see idyn countdown *)
If[print,Print[Style["       Renumber polynomials - see idyn countdown",Blue,FontSize->14]]];
{PPCharStringmono,PPCharStringpoly}=
	RenumberFortranExistingmInMonoAndPoly[PPCharStringmono,PPCharStringpoly];
	If[diagnose,
	Export[DataDir<>"PPCharStringmono5m",PPCharStringmono,"Text"];
	Export[DataDir<>"PPCharStringpoly5p",PPCharStringpoly,"Text"];
	];
If[nq==0,
	PPCharStringpoly=RenumberFortranExistingp[PPCharStringpoly];
	,
	{PPCharStringpoly,qnumber}=RenumberFortranExistingq[PPCharStringpoly];
	{PPCharStringpoly,pnumber}=RenumberFortranExistingpPP[PPCharStringpoly];
	nq=qnumber;
	npoly=pnumber;
];

If[diagnose,
	Export[DataDir<>"PPCharStringpoly5",PPCharStringpoly,"Text"];
];
If[!purified,
	PPDatapoly=textconvert[PPCharStringpoly,"text","table",DataDir];
	pnumber=0;
	Do[(If[StringContainsQ[PPDatapoly[[i,1]],"p("],pnumber=pnumber+1];),
		{i,1,Length[PPDatapoly]}];
	If[StringContainsQ[PPDatapoly[[1,1]],"p(0)"],pnumber=pnumber-1;];
	npoly=pnumber;
];
If[print,Print["{mnumber,pnumber,qnumber} = ",{mnumber,pnumber,qnumber}];];
(* Important:  we need to add back the p(0) term.  Best is probably to set it to zero *)
If[!StringContainsQ[PPCharStringpoly,"p(0) ="],
	PPCharStringpoly="    p(0) = 0.d0\n"<>PPCharStringpoly;
];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly52",PPCharStringpoly,"Text"];
];




(* *************** *)
(* Perform recompaction, if desired *)
If[print,Print[Style["***Performing recompaction, if desired: ",
	Blue,FontSize->14],DateString[]]];
If[ValueQ[performrecompaction]&& performrecompaction,
	If[print,Print["{mnumber,pnumber,qnumber} = ",{mnumber,pnumber,qnumber}];];
	{PPCharStringmono,PPCharStringpoly,{mnumber,qnumber,pnumber}}=
		CompactionPure[mnumber,pnumber,qnumber];
	,
	If[print,Print[Style["Recompaction not requested: ",Blue,FontSize->14],DateString[]]];
];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly5q",PPCharStringpoly,"Text"];
	Export[DataDir<>"PPCharStringmono5r",PPCharStringmono,"Text"];
];



(* *************** *)
(* Create and Export new fortran file named in OutputFortranname *)
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print[Style["Create and Export new fortran file named in OutputFortranname",
Blue,FontSize->14]]];
fortranname=OutputFortranname; 
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
If[diagnose, Export[DataDir<>"PPCharStringpoly5h",PPCharStringpoly,"Text"];];
If[diagnose, Export[DataDir<>"PPCharStringmono5h",PPCharStringmono,"Text"];];
If[diagnose, Export[DataDir<>"ComentXAssignments5h",CommentXAssignments,"Text"];];
If[nq==0,
Print["nq = 0; MakeExportFortranPP called"];
MakeExportFortranPP[CommentXAssignments,PPCharStringmono,PPCharStringpoly];
,
Print["nq =",nq,"; MakeExportFortranPure called"];
If[print,Print["{mnumber,pnumber,qnumber} = ",{mnumber,pnumber,qnumber}];];
MakeExportFortranPure[CommentXAssignments,PPCharStringmono,PPCharStringpoly, 
	pnumber,qnumber,mnumber];
];
fortranname=fortrannamesave;
If[diagnose,
	Export[DataDir<>"PPCharStringpoly5x",PPCharStringpoly,"Text"];
];


(*  If all is ok, this section should not be needed 

(* *************** *)
(* Delete polynomials whose rhs evaluates to zero, renumber, and write file again *)
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print[Style[
"Delete polynomials whose rhs evaluates to zero, renumber, and write file again",
Blue,FontSize->14]]];
fortranname=OutputFortranname; 
If[nq==0,
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
,
{rijnamesnow,nvariables,mnumber,pnumber,qnumber}=
		GetDefinitionstoMathematicaFromFortranOutputPure[];
];
Print["{nmono,npoly} = ",{nmono,npoly}];
If[print,Print["rijnamesnow = ",rijnamesnow];];
If[print,Print["{nvariables,nmono,npoly,nq} = ",{nvariables,nmono,npoly,nq}];];
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,xsum}];
Assignx[];
EM=IntegerPart[10^10*EvalMono[]];
If[ValueQ[purified] && purified,
EP=IntegerPart[10^10*EvalPolyNew[PPCharStringpoly,plistpur]];
,
EP=IntegerPart[10^10*EvalPoly[]]; (* tag times problem is here *)
];
(*xb=Assignx[];*)
EPm1=Drop[EP,1];
deletelist=Flatten[Position[EPm1,0]];
If[diagnose,
	Export[DataDir<>"deletelist2",deletelist,"Table"];
	Export[DataDir<>"PPCharStringpoly6",PPCharStringpoly,"Text"];
];
If[deletelist=={},Goto[skip534];];
(* consolidate and renumber the p values in the poly lists, renaming common p's to d's *)
Do[(
	PPCharStringpoly=StringReplace[PPCharStringpoly,
"p("<>ToString[deletelist[[i]]]<>")"-> 
		"d("<>ToString[deletelist[[i]]]<>")"
];			
),{i,Length[deletelist],1,-1}];
(* delete lines that have d definitions on the lhs of poly and mono*)	
PPCharStringpoly=DeleteAllFortranDefinitionsStartingd[PPCharStringpoly];
(* 
What we want to do is to approximate the deleted polynomials by zero
*)
Do[(
PPCharStringpoly=StringReplace[PPCharStringpoly,
	"d("<>ToString[deletelist[[i]]]<>")"-> "(0)"];
),{i,1,Length[deletelist]}];
(* get rid of the zeros *)
If[diagnose,
	Export[DataDir<>"PPCharStringpoly7",PPCharStringpoly,"Text"];
];
PPCharStringpoly= StringReplace[PPCharStringpoly,"+ (0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"- (0)"->""];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly8",PPCharStringpoly,"Text"];
];

(* We also need to get rid of terms that have "*(0)" on the rhs; this is more difficult since we don't know what comes before "*(0)" *)
(* here's a method *)
PPDatapoly=ImportString[PPCharStringpoly,{"Text","Data"}]; (* converts PPCharStringpoly to data with each element separated by commas *)
(* get a list of the locations where "*(0)" occurs *)
tabpzero={};
Do[(
	If[StringContainsQ[PPDatapoly[[i]],"*(0)"]=={True},
		tabpzero=Append[tabpzero,i];
	];
),{i,1,Length[PPDatapoly]}];
(* for each element in tabpzero, find the location(s) of "*(0)"  *)
Do[(
	string=ImportString[PPDatapoly[[tabpzero[[i]]]][[1]],{"Text","Words"}];
	klist={};
	Do[(
		If[StringContainsQ[string[[k]],"*(0)"],
			klist=Append[klist,k];
		];
	),{k,1,Length[string]}];
(*  Delete the contents of that location *)
	Do[(
		string=Delete[string,{klist[[j]]}];
	),{j,Length[klist],1,-1}];
	PPDatapoly[[tabpzero[[i]]]][[1]]=ExportString[string,{"Text","Words"}];
	(*Print[PPDatapoly[[tabpzero[[i]]]][[1]]];*)
),{i,1,Length[tabpzero]}];
(*
(* The switch between Text and {Text,Data} screws up multiline statements; the next loop fixes it *)
Do[(
	If[Length[PPDatapoly[[i]]]\[NotEqual]1,
		xnew=PPDatapoly[[i,1]];
		Do[(
			xnew=StringJoin[xnew,PPDatapoly[[i,k]]];
		),{k,2,Length[PPDatapoly[[i]]]}];
		PPDatapoly[[i]]={xnew};
	];
),{i,1,Length[PPDatapoly]}];
*)

If[diagnose,
	Export[DataDir<>"PPCharStringpoly9",PPCharStringpoly,"Text"];
];
(*
(* PPCharStringpoly needs to have 4 leading spaces *)
Do[(
PPDatapoly[[i]]="    "<>PPDatapoly[[i]];
),{i,1,Length[PPDatapoly]}];
*)
PPCharStringpoly=ExportString[Flatten[PPDatapoly],{"Text","Lines"}]; (* replace PPCharSTringpoly with terms containing "*(0)" deleted *)
(* there will still be several polys whose rhs evaluates to zero. These will show up as duplicates in the test.  We get rid of them  *)
(* renumber the polynomials  *)
{PPCharStringmono,PPCharStringpoly}=RenumberFortranExistingmInMonoAndPoly[PPCharStringmono,PPCharStringpoly];
{PPCharStringpoly,ix}=RenumberFortranExistingp[PPCharStringpoly];
If[print,Print["The number of all current polynomials, including p(0), is ",
	StringCount[PPCharStringpoly,"="]];];
	pnumber=StringCount[PPCharStringpoly,"="];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly10",PPCharStringpoly,"Text"];
];
fortranname=OutputFortranname; 
CommentXAssignments=GenerateFortranCommentWithXAssignments[];
If[print,Print["The number of final coordinates is ",StringCount[CommentXAssignments,"="]]];
If[print,Print["They are: "];];
If[print,Print[StringDrop[StringReplace[StringDelete[CommentXAssignments,"!     "],"\n"->", "],-2]];];
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
If[nq==0,
Print["nq=0; MakeExportFortranPP called "];
MakeExportFortranPP[CommentXAssignments,PPCharStringmono,PPCharStringpoly];
,
Print["nq=",nq,"; MakeExportFortranPure called "];
If[print,Print["{mnumber,pnumber,qnumber} = ",{mnumber,pnumber,qnumber}];];
MakeExportFortranPure[CommentXAssignments,PPCharStringmono,PPCharStringpoly, 
	pnumber,qnumber,mnumber];
];

If[diagnose,
	Export[DataDir<>"CommentXAssignments",CommentXAssignments,"Text"];
	Export[DataDir<>"PPCharStringmono",PPCharStringmono,"Text"];
	Export[DataDir<>"PPCharStringpoly11",PPCharStringpoly,"Text"];
];
Label[skip534];
fortranname=fortrannamesave;
If[diagnose,
	Export[DataDir<>"PPCharStringpoly5y",PPCharStringpoly,"Text"];
];
*)


(* *************** *)
(* Add the derivatives, if desired, to the output fortran file *)
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print[Style["Add the derivatives, if desired, to the output fortran file",
Blue,FontSize->14]]];
If[print && !ProvideDerivatives,Print["Derivatives not requested"];];
fortranname = OutputFortranname;
If[ProvideDerivatives,
	If[nq==0,
		Print["MakeExportFortranDDWithChenDerivativesPP called"];
		time=Timing[
		MakeExportFortranDDWithChenDerivativesPP[
			CommentXAssignments,PPCharStringmono,PPCharStringpoly];
			][[1]];
		,
		Print["MakeExportFortranDDWithChenDerivativesPure called"];
		time=Timing[
		MakeExportFortranDDWithChenDerivativesPure[
			CommentXAssignments,PPCharStringmono,PPCharStringpoly,
			  pnumber, qnumber, mnumber];
		][[1]];	
	];	
	Print["The derivatives program took ",time," sec. = ",time/60," min."];
];
fortranname=fortrannamesave;
If[diagnose,
	Export[DataDir<>"PPCharStringpoly5z",PPCharStringpoly,"Text"];
];

(* *************** *)
(* Run tests, if desired *)
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Run tests, if desired"]];
fortranname = OutputFortranname;
If[ UseRunTests== True, 
	If[nq==0,
	Print["nq=0; Running RunTests"];
	RunTests[];
	,
	Print["nq = ",nq,"; Running RunTestsPure"];
	If[print,Print["{mnumber,pnumber,qnumber} = ",{mnumber,pnumber,qnumber}];];
	RunTestsPure[mnumber,pnumber,qnumber];
	];
];
fortranname=fortrannamesave;

If[print,Print["***Finished: ",DateString[]]];
(* no output *)

];





PolynomialAdding[]:=Module[
{print,pesdata,natoms,data,nenergies,mtab,gm,
(*xx,ijnow,inow,jnow,xxindex,PPCharStringpoly,PPCharStringmono, - for some reason, these cannot be local variables*)
EPtab,EPsorted,minep,nvariablesb,addlist,nadd,nadddesired,po1,po2,
used,unused,check,skipj,done,PPCharStringpolyorig,nm,np,nq
},

CheckPolynomialAddingInputs[];
Print["Input passed CheckPolynomialAddingInputs"];

print=True;
(* Format of xx is i,j,Av,SD,max,min , where i,j are atom numbers *)
(* *************** *)
(* xxindex 3 of xx is the average value of this variable 
in the ab initio data *)
(* xxindex 5 of xx is the maximum value of this variable 
in the ab initio data *)
(* xxindex 4 of xx is the minimum value of this variable 
in the ab initio data *)
(* If xxindex has been declared in the inpute template, than that value will be used,
otherwise, it will be defined as in the next statement *)
If[!ValueQ[xxindex],xxindex=5];


(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Initiation: Get Distribution and Averages of Morse Variables for Data set"]];
pesdata=Import[pesfile,"Table"];
natoms=natomsparent;
data=pesdata;
nenergies=Length[data]/(natoms+2);
Globalunused={};
(* Get the Morse distributions  *)
mtab=MorseDist[natoms,data];
gm=MorseDistGM[natoms,data];
(* Get the Averages and Standard Deviations ordered by largest average *)
xx=GetMorseAvSDMaxMin[mtab,natoms];

(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Get Distribution of basis function values"]];
(* **************** Get Fortran Output Definitions   *******************  *)
(* get mono poly fortran lists from  fortran file *)
{nm,np,nq}=Getmpq[fortranname];
If[nq==0,
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
,
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutputPure[];
];
(* above command also creates global PPCharStringmono and PPCharStringpoly *)
If[Or[nm!=nmono,np!=npoly],Print["Aborting: pblms with nm,np in PolynomialAdding"];Abort[];];

rijnames[[1]]=rijnamesnow;
If[print,Print["Coordinates now in use:"]];
If[print,Print[rijnamesnow]];
xsum=Length[rijnamesnow];
plist=Table["p"<>ToString[i],{i,0,npoly}];  (* needs to be global *)
mlist=Table["m"<>ToString[i],{i,0,nmono}];  (* needs to be global *)
xlist=Table["x"<>ToString[i],{i,1,nvariables}];  (* needs to be global *)
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
(* assign the maximum value of the appropriate morse variable to each of the x variables whose atom pairs are in the rijnames now list *)
(* we won't use Assignx[] which creates x values from the appropriate distances *)
(*
Do[(
	dist[[i]]=RandomReal[{0.1,1}];
	),{i,1,xsum}];	
	Assignx[]
*)
(* in stead, we'll assign the xvalues themselves, using information from the Morse variable distributions *)
Do[(
ijnow=rijnamesnow[[k]];
inow=ToExpression[StringDrop[ijnow,-2]];
jnow=ToExpression[StringDrop[ijnow,2]];
(*Print["{inow,jnow} = ",{inow,jnow}];*)
ToExpression["x"<>ToString[k]<>"=xx[[inow,jnow,xxindex]];"];  
),{k,1,xsum}];
EM=EvalMono[];	
EP=EvalPoly[];
PPCharStringpolyorig=PPCharStringpoly;
If[print,Print["{npoly,maxpoly,minpoly} = ",{Length[EP],Max[EP],Min[EP]}];];

(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Find new basis functions with large values"]];
Do[(
ijnow=rijnamesnow[[k]];
inow=ToExpression[StringDrop[ijnow,-2]];
jnow=ToExpression[StringDrop[ijnow,2]];
(*Print["{inow,jnow} = ",{inow,jnow}];*)
ToExpression["x"<>ToString[k]<>"=xx[[inow,jnow,xxindex]];"];
),{k,1,xsum}];
EM=EvalMono[];
EP=EvalPoly[];
EPm1=Drop[EP,1];
minep=Min[EPm1];
EPtab=Table[{i,EPm1[[i]]},{i,1,Length[EPm1]}];
EPtab=Drop[EPtab,1];
(* Now identify those polynomials whose value is below the limiting value, limit *)
EPsorted=Sort[EPtab, #1[[2]] > #2 [[2]]&]; (* decreasing order, evaluated at max value of Morse variables *)
(*If[nq==0,
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
,
{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutputPure[];
];*)

nvariablesb=Length[rijnamesnow];
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,nvariablesb}];
Assignx[];
EvalMono[];
EPb=IntegerPart[10^10*EvalPoly[]];
EPb=Drop[EPb,1];
If[print,Print["npoly is ",Length[EPb]];];
addlist={};
nadd=0;
ndyn=nadd;
nadddesired=ncoeffdesired-npoly;
If[print,Print["{Length[EPsorted],nadddesired} = ",{Length[EPsorted],nadddesired}];];
If[print,Print["{idyn,jdyn,ndyn} give the indices of the current polynomial being evaluated "];];
If[print,Print["(in order of decreasing value), where ndyn gives the number of new  "];];
If[print,Print["polynomials found."];];
(* 
  Here is a brief description of how this works. We have a sorted list of the current 
polynomial basis functions, EPsorted, where the polynomials have been evaluated using the Morse
vairables for the data set, and the lowest index in EPsorted corresponds to the polynomial with the
highest value.  We want to make new polynomials by taking multiplicative pairs of the current 
polynomials and keeping those results with the highest value until we have gotten enough new 
polynomials to give the total desired number.  However, some of the polynomials we generate by 
the pairwise multiplication will already be in the data set, so we want to skip those. In order
do the investigations most efficiently, we start with the largest polynomial, i=1, and we multiply it 
first by polynomial j=1. This gives the square of the largest polynomial.  If the new polynomial
is already being used, we skip to the next j; if not we put it on the add list.  The next i is 2
and we look at j values 1 and 2 because poly[[1]]*poly[[2]  and poly[[2]]*poly[[2]] have not 
yet been tested.  In the next round i is 3 and we look at j=1,2,3, etc.  Thus, we are always looking
at the unique pairs that will have the highest product value.  
  Of course, we could look at unique triplets, quadruplets, etc. as well.  That should probably be 
implemented.  However, it is possible to get there by doing several PA steps to get to the desired
number rather than one big one.
  Note that the maximum order of the polynomials might be (in the pair case) twice the order of the
original.  
  Note also that there are some polynomials of, for example, order one higher than the original 
that will not be accessible by this method.  
*)
Do[(
idyn=i; (* global for dynamic *)
Do[(
jdyn=j; (* global for dynamic *)
check={EPsorted[[j,1]],EPsorted[[i,1]],EPsorted[[j,2]]*EPsorted[[i,2]]};
(*If[check[[3]]>.03,*)
If[MemberQ[  EPb,IntegerPart[ 10^10*EvalPoly[][[EPsorted[[j,1]]+1]]*EvalPoly[][[EPsorted[[i,1]]+1]] ]  ],
	Goto[skipj]; (* the proposed polynomial is already included in the EP polynomials *)
	,
	addlist=Append[addlist,check]; (* the proposed polynomial is a new one *)   
	EPb=Append[EPb,
		IntegerPart[ 10^10*EvalPoly[][[EPsorted[[j,1]]+1]]*EvalPoly[][[EPsorted[[i,1]]+1]] ]];
	nadd=nadd+1;
	ndyn=nadd;
];
Label[skipj];
If[nadd==nadddesired,Goto[done];];
),{j,1,i}];
),{i,1,Length[EPsorted]}];
Label[done];

If[print,Print["Length of add list is ",Length[addlist]];];
addlist=Take[Sort[addlist,#1[[3]]>#2[[3]] &],Min[nadddesired,Length[addlist]]];
If[print,Print["addlist is: ",addlist];];

(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Add polynomials"]];
PPCharStringpoly=PPCharStringpolyorig;
PPCharStringpoly=PPCharStringpoly<>"\n";
Do[(
po1=addlist[[i,1]];
po2=addlist[[i,2]];
PPCharStringpoly=PPCharStringpoly<>"    p("<>ToString[npoly+i]<>") = p("<>ToString[po1]<>")*p("<>ToString[po2]<>")\n";
),{i,1,Length[addlist]}];
If[print,Print["The number of all current polynomials, including p(0), is ",
	StringCount[PPCharStringpoly,"="]];];

(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Create and Export new fortran file"]];
fortranname=OutputFortranname; 
CommentXAssignments=GenerateFortranCommentWithXAssignments[];
If[print,Print["The number of final coordinates is ",StringCount[CommentXAssignments,"="]];];
If[print,Print["They are: "];];
If[print,Print[StringDrop[StringReplace[StringDelete[CommentXAssignments,"!     "],"\n"->", "],-2]];];
rijnamesP=Flatten[Table[
	  If[i<10,"0"<>ToString[i],ToString[i]]<>
	If[j<10,"0"<>ToString[j],ToString[j]],
{i,1,natomsparent},{j,i+1,natomsparent}]];
used={};
Do[(
	used=Union[used,rijnames[[ifrag]]];
),{ifrag,1,nfragments}];
unused=Complement[rijnamesP,used];
If[print,Print["The following interatomic distances in the parent are omitted: ",unused];];
Globalunused=unused; (* Global *)
MakeExportFortranPP[CommentXAssignments,PPCharStringmono,PPCharStringpoly];
fortranname=fortrannamesave;

(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Add derivatives if desired"]];
(* make and export to file the new Fortran code *)
fortranname = OutputFortranname;
If[ProvideDerivatives,
	If[UseBatchDerivatives,
		time=Timing[
		MakeExportFortranDDWithBatchDerivativesPP[CommentXAssignments,PPCharStringmono,PPCharStringpoly];
		][[1]];
		,
		time=Timing[
		MakeExportFortranDDWithChenDerivativesPP[CommentXAssignments,PPCharStringmono,PPCharStringpoly];
		][[1]];	
	];
	Print["The derivatives program took ",time," sec. = ",time/60," min."];
];
fortranname=fortrannamesave;

(* *************** *)
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Run tests, if desired"]];
fortranname = OutputFortranname;
If[ UseRunTests== True, RunTests[]];
fortranname=fortrannamesave;

Print["Finished"];

(* no output *)
];


(* ::Section:: *)
(*These functions replace similar ones in DeleteDuplicates*)


MakeFdrdxfunctionPP[CommentXAssignments_]:=Module[
{x,y,Fdrdxfunction},

(* here's a Fortran function for drdx:  *)

x=CommentXAssignments;
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
        real (wp) :: xyz(natoms1047,3)
	
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





(* I seem to have two of these. I marked this one as old.  Not sure which 
one is the operative one *)
MakeExportFortranDDWithBatchDerivativesPPold::usage="MakeExportFortranDDWithBatchDerivatives[...]
Called From SequentialBulidupWithBatchDerivatives[]

";
MakeExportFortranDDWithBatchDerivativesPPold[CommentXAssignments_,CharStringmono_,CharStringpoly_]:=Module[
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
		Print["i = ",i," in CharStringmono conversion"];
		Print["CSMlinesch = ",CSMlinesch];
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

newfortran=newfortran<>MakeFdrdxfunctionPP[CommentXAssignments];
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
If[abortatend,Abort[];];
z
];


MakeExportFortranDDWithChenDerivativesPP::usage="MakeExportFortranDDWithChenDerivatives[...]
Called From SequentialBulidupWithBatchDerivatives[]

";
MakeExportFortranDDWithChenDerivativesPP[CommentXAssignments_,CharStringmono_,CharStringpoly_]:=
	Module[
{fnamea,natomsa,rijnamesa,bemsaa,monostarta,monoenda,StringAssigns,
polystarta,polyenda,newfortran,xstart1,xstart2,xstart3,xmiddle,xending,CSMlines,
AnotherRoundM,AnotherRoundP,CSPlines,CSMlinesch,CSPlinesch,jsave,outM,outP,
CSMappend,CSMremaining,CSPappend,CSPremaining,nvar,nmn,npol,z,Tijnums,linedist,
demsav,dbemsav,devmonointro,devmonopoly,devpolyend,FinishUp,xpolyending,
MD,MDlines,PD,PDlines,MDlinesch,PDlinesch,MDappend,MDremaining,PDappend,PDremaining
},

Print[""];
Print["Preparing to make and export Fortran file with Chen derivatives"];

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
    real(wp),dimension(1:nvar1047)::x
    real(wp),dimension(0:monos1047)::m
    real(wp),dimension(0:polys1047)::p
    real(wp),dimension(0:polys1047)::dp
    real(wp),dimension(0:polys1047)::c
    real(wp)::xyz(natoms1047,3)
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

"; (* modified this section 4-Nov-2022 *)
xstart2=xstart2<>"     x(:) = x(:)/a \n";
xstart2=xstart2<>"     x(:) = dexp (-x(:)) \n";
xstart2=xstart2<>"     end subroutine\n";

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
PD=GetFPolyDerivativesPure[];
(* convert CharStringpoly to lines of text *)
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
newfortran=newfortran<>MakeFdrdxfunctionPP[CommentXAssignments];
(*Print[CommentXAssignments];
Print[MakeFdrdxfunctionPP[CommentXAssignments]];*)
xending="\n end module bemsa";
newfortran=newfortran<>xending;

(* fix dimensions *)
nvar=StringCount[CommentXAssignments,"="];
nmn=StringCount[CharStringmono,"="]-1;
npol=StringCount[CharStringpoly,"="]-1;
z=StringReplace[newfortran,"1:21"-> "1:"<>ToString[nvar]];
z=StringReplace[z,"0:239"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:578"->  "0:"<>ToString[npol]];
z=StringReplace[z,"12z1047"->  ToString[natomsparent]];
z=StringReplace[z,"12z"-> ToString[natomsparent]];
z=StringReplace[z,":: xyz(9,3)"->
	 ":: xyz("<>ToString[natomsparent]<>",3)"];
z=StringReplace[z,":: coeff(3096)"->
	 ":: coeff("<>ToString[npol]<>")"];
z=StringReplace[z,":: x(36)"-> ":: x("<>ToString[nvar]<>")"];
z=StringReplace[z,"do i = 1, 3096"->
	 "do i = 1, "<>ToString[npol]<>""];
(* added 11/19/2021 : *)
z-StringReplace[z,"d(0)"->"(0)"];
(* added 1/31/2021 *)
z=StringReplace[z,"0:monos1047"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:polys1047"->  "0:"<>ToString[npol]];
z=StringReplace[z,"real(wp),dimension(0:580)::r\n"->  ""];
z=StringReplace[z,"natoms1047"-> ToString[natomsparent]];
z=StringReplace[z,"nvar1047"-> ToString[nvar]];

Export[fortranname,z,"Text"];

Label[FinishUp];
Print["Done"];
Export[fortranname,z,"Text"];
z
];





MakeExportFortranDDWithChenDerivativesPure::usage="MakeExportFortranDDWithChenDerivatives[...]
";
MakeExportFortranDDWithChenDerivativesPure[CommentXAssignments_,CharStringmono_,
	CharStringpoly_,pnumber1_,qnumber1_,mnumber1_]:=
	Module[
{fnamea,natomsa,rijnamesa,bemsaa,monostarta,monoenda,StringAssigns,
polystarta,polyenda,newfortran,xstart1,xstart2,xstart3,xmiddle,xending,CSMlines,
AnotherRoundM,AnotherRoundP,CSPlines,CSMlinesch,CSPlinesch,jsave,outM,outP,
CSMappend,CSMremaining,CSPappend,CSPremaining,nvar,nmn,npol,z,Tijnums,linedist,
demsav,dbemsav,devmonointro,devmonopoly,devpolyend,FinishUp,xpolyending,
MD,MDlines,PD,PDlines,MDlinesch,PDlinesch,MDappend,MDremaining,PDappend,PDremaining,
xform,diagnose
},
diagnose=False;
Print[""];
Print["Preparing to make and export Fortran file with Chen derivatives (Pure)"];

(* This follows MakeExportFortran file, but it replaces the regular Fortran 
file with one
that has the derivative functions as well *)



newfortran=Import[OutputCptFilename,"Text"];
newfortran=newfortran<>"
  function gemsav(flag,x,m,p,dp,c,xyz) result(g)
    implicit none
    integer :: flag
    real(wp),dimension(1:nvar1047)::x
    real(wp),dimension(0:monos1047)::m
    real(wp),dimension(0:polys1047)::p
    real(wp),dimension(0:polys1047)::dp
    real(wp),dimension(0:polys1047)::c
    real(wp)::xyz(natoms1047,3)
    real(wp)::g 
	call EvaldpBasisSet(flag,x,m,p,dp,xyz)
    g = dot_product(dp,c)
    return
  end function gemsav
";
  newfortran=newfortran<>"

  Subroutine EvaldpBasisSet(flag,x,m,p,dp,xyz)

        implicit none
    real(wp),dimension(1:nvar1047)::x
    real(wp),dimension(0:monos1047)::m
    real(wp),dimension(0:polys1047)::p
    real(wp),dimension(0:polys1047)::dp
    real(wp),dimension(0:polys1047)::c
    real(wp)::xyz(natoms1047,3)
    integer::flag

    call dbemsav(dp,m,p,flag,xyz)
      Return

      END Subroutine EvaldpBasisSet";
newfortran=StringDelete[newfortran," end module bemsa"];
(* put in the derivative functions here *)

demsav="

 function demsav(c,m,p,flag,xyz) result(grad)
    implicit none
    real(wp),dimension(0:polys1047)::c
    real(wp),dimension(0:monos1047)::m
    real(wp),dimension(0:polys1047)::p
    real(wp)::grad
    real(wp) :: xyz(natoms1047,3)
    integer::flag
    ! ::::::::::::::::::::
    real(wp),dimension(0:polys1047)::dp

    call dbemsav(dp,m,p,flag,xyz)
    grad = dot_product(dp,c)

    return
  end function demsav
";
newfortran=newfortran<>demsav;

dbemsav="
  subroutine dbemsav(dp,m,p,flag,xyz)
    implicit none
    real(wp),dimension(0:polys1047)::dp
    real(wp),dimension(0:monos1047)::m
    real(wp),dimension(0:polys1047)::p
    real(wp) :: xyz(natoms1047,3)
    integer::flag
    ! ::::::::::::::::::::
    real(wp),dimension(0:monos1047)::dm

    call devmono(dm,m,flag,xyz)
    call devpoly(dm,p,dp)

    return
  end subroutine dbemsav
";
newfortran=newfortran<>dbemsav;


devmonointro="
  subroutine devmono(dm,m,flag,xyz)
    implicit none
    real(wp),dimension(0:monos1047)::dm
    real(wp),dimension(0:monos1047)::m
	real(wp) :: xyz(natoms1047,3)
    integer::flag
    !::::::::::::::::::::

    dm(0) = 0.d0
";
newfortran=newfortran<>devmonointro;

(* get mono derivatives and insert & if needed *)
MD=GetFMonoDerivativesPure[];
If[diagnose,PLHMD=MD;];
(* convert CharStringmono to lines of text *)
MDlines=textconvert[MD,"textnoamp","textlines",DataDir];
If[diagnose,PLHMDlines10=MDlines;];
(*Export[DataDir<>"DeleteMe.txt",MD,"Text"];
MDlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];*)

newfortran=newfortran<>Ampersand[MDlines];

If[diagnose,PLHnewfortran6=newfortran;];

(* 22 Feb 2023: I took out the line "    real(wp),dimension(0:nq1047)::dq" because a) it 
shouldn't be 0:xxx, and b) it shouldn't be there because it's in the module *)
devmonopoly="

    return
  end subroutine devmono

  subroutine devpoly(dm,p,dp)
    implicit none
    real(wp),dimension(0:monos1047)::dm
    real(wp),dimension(0:polys1047)::p
    real(wp),dimension(0:polys1047)::dp
    real(wp),dimension(1:nq1047)::dq
!::::::::::::::::::::

    dp(0) = 0.d0
";
newfortran=newfortran<>devmonopoly;

(* get poly derivatives and insert & if needed *)
PD=GetFPolyDerivativesPure[];
If[diagnose,PLHPD=PD;];
(* convert CharStringpoly to lines of text *)
PDlines=textconvert[PD,"textnoamp","textlines",DataDir];
If[diagnose,PLHPDlines10=PDlines;];

newfortran=newfortran<>Ampersand[PDlines];

If[diagnose,PLHnewfortran7=newfortran;];

devpolyend="

    return
  end subroutine devpoly

";
newfortran=newfortran<>devpolyend;
newfortran=newfortran<>MakeFdrdxfunctionPP[CommentXAssignments];
(*Print[CommentXAssignments];
Print[MakeFdrdxfunctionPP[CommentXAssignments]];*)
xending="\n end module bemsa";
newfortran=newfortran<>xending;

(* fix dimensions *)
nvar=natomsparent (natomsparent-1)/2;
nmn=mnumber1;
npol=pnumber1;
Print["{mnumber,pnumber,qnumber,nvar} =",{mnumber1,pnumber1,qnumber1,nvar}];
z=StringReplace[newfortran,"1:nvar1047"-> "1:"<>ToString[nvar]];
z=StringReplace[z,"0:monos1047"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:polys1047"->  "0:"<>ToString[npol]];
z=StringReplace[z,"nq1047"->ToString[qnumber1]];
z=StringReplace[z,"real(wp),dimension(0:580)::r\n"->  ""];
z=StringReplace[z,"natoms1047"-> ToString[natomsparent]];


If[diagnose,PLHz10=z;];
Export[fortranname,z,"Text"];

Label[FinishUp];
Print["Done"];
Export[fortranname,z,"Text"];
z
];





MakeExportFortranPP[CommentXAssignments_,CharStringmono_,CharStringpoly_]:=Module[
{fnamea,natomsa,rijnamesa,bemsaa,monostarta,monoenda,StringAssigns,
polystarta,polyenda,newfortran,xstart1,xstart2,xstart3,xmiddle,xending,CSMlines,
AnotherRoundM,AnotherRoundP,CSPlines,CSMlinesch,CSPlinesch,jsave,outM,outP,
CSMappend,CSMremaining,CSPappend,CSPremaining,nvar,nmn,npol,z,Tijnums,
linedist,xform,nvariables},
nvariables=natomsparent (natomsparent-1)/2;

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
z=StringReplace[z,"12z1047"-> ToString[natomsparent]];
z=StringReplace[z,"1:21z"-> "1:"<>ToString[nvar]];
z=StringReplace[z,"natoms1047"-> ToString[natomsparent]];
z=StringDelete[z,";"];
z=StringReplace[z,"12z"-> ToString[natomsparent]];
z=StringReplace[z,":: x(36)"-> ":: x("<>ToString[nvar]<>")"];
z=StringReplace[z,"do i = 1, 3096"->
	 "do i = 1, "<>ToString[npol]<>""];


Export[fortranname,z,"Text"];
z
];






PolynomialPurification[]:=Module[
{
(*xx,ijnow,inow,jnow,PPCharStringpoly,PPCharStringmono,xxindex,xsum - 
for some reason, these cannot be local variables*)
(*qlist,*)
addlist,addx,CharStringdmm,CharStringF,check,CommentXAssignments2,
csmonopurecpt,cspolypurecpt,currentatoms,data,deletelist,diagnose,
dname,dname1,dname2,dolimite,dolimitq,dpcoef,EPb,EPm1,EPsorted,EPtab,
Etab,Globalunused,gm,groupsskipped,
limit,lmdrop,lpdrop,minep,mnumber,mnumbernow,
monodroplist,mtab,nadd,nadddesired,natoms,nenergies,newfortran,nf,
nmono,nmononow,nneeded,nnow,npoly,npolynow,nqnow,nsnow,pappend,pcharadd,
pesdata,plast,plistpur,pnewlist,pnow,pnumber,pnumbernow,
po1,po2,polydroplist,
ppos,PPCSP44tab,PPCSPtab,PPCSPtab1,ppossorted,print,qnumber,qnumbernow,
qnumbersave,qval,qvalsorted,rijnamesP,
rnumber,sct,skip,skipaddition,skipcompact,Skipcpct,skipderivatives,skipf,
skiptosect4b,time,tout,Tout,startcpct,
unumber,unumbernow,unused,used,xsum,xyzperms,z},
(* Check Input *)
Print["Checking Input"];
CheckPurificationInputs[];

If[!skipgroups,xyzperms=Partition[Import[xyzpermsfilename,"Table"],2+natomsparent]];
print=True; 
diagnose=False;
(*skipcompaction=False;  (* not advised to have this True *) -- should be controlled by input *)
If[usingpurecompactedinputfile,
	Goto[skiptosect4b];
];

(* *************** *)
Print["\n"];
If[print,Print[DateString[]];];
If[print,Print[Style["Starting Part One: get basis functions and purify",Blue,FontSize->14]];];
If[print,Print["Get Basis functions "]];

(* **************** Part One - Get Fortran Output Definitions   *******************  *)
	(* get mono poly fortran lists from  fortran file *)
	{rijnamesnow,nvariables,nmono,npoly}=GetDefinitionstoMathematicaFromFortranOutput[];
rijnames[[1]]=rijnamesnow; 
If[print,Print["Coordinates now in use:"]];
If[print,Print[rijnamesnow]];
xsum=Length[rijnamesnow];
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
If[diagnose,
	Export[DataDir<>"PPCharStringmono1 ",PPCharStringmono,"Text"];
	Export[DataDir<>"PPCharStringpoly1 ",PPCharStringpoly,"Text"];
];

(*
(* get groups, groupswithdups, groupvalauessorted files NB: these will 
all be Global variables.  This program assumes that PPCharStringpoly and pnumber 
are defined as global variables*)
pnumber=npoly;
mnumber=nmono;
If[print,Print["Getting groups, etc."];];
If[numofnmers==4,
{groups,groupswithdups,groupvaluessorted}=
	findevaluatepermsymgroups24[fortranname,natomsparent,xyzperms,pesfile,
	groupsfname,groupswithdupsfname,groupvaluessortedfname,DataDir];
,
Print["Implemented currently only for numofnmers=4; Aborting."];Abort[];
];
*)

(* determine monodroplist and polydroplist *)
(* monodroplist={}; mdroplist is not used *)
polydroplist={};
addx=100.;
Do[(  (* nmerid = 1 to Length[nmeratoms] *)
currentatoms=nmeratoms[[nmerid]];
(*Assign \[OpenCurlyDoubleQuote]normal\[CloseCurlyDoubleQuote] but random distances to all possibilities*)
	Do[( 
		dist[[i]]=RandomReal[{1,2}];
	),{i,1,xsum}];
(* assign data distances to dist *)
Do[( 
  dname=rijnamesnow[[i]];
  dname1=ToExpression[StringDrop[dname,-2]];
  dname2=ToExpression[StringDrop[dname,   2]];
  (* now, for the current nmer(s), pull it away from the rest *)
  (* add addx to x & y data coordinates if these are in current atoms *)
  If[MemberQ[currentatoms,dname1] &&MemberQ[currentatoms,dname2],
   Goto[skip];]; (* if both atoms are on list, skip *)
  If[!MemberQ[currentatoms,dname1] && !MemberQ[currentatoms,dname2],
   Goto[skip];]; (* if both atoms are not the list, skip *)
  If[Or[MemberQ[currentatoms,dname1],
	MemberQ[currentatoms,dname2]],dist[[i]]=dist[[i]]+addx];
Label[skip];
),{i,1,xsum}];	
	
(* assign the current distances *)
Assignx[];
If[print,Print[currentatoms];];
If[print,Print[dist];];
 EM=EvalMono[];
EM=Drop[EM,1];
 EP=EvalPoly[];
EP=Drop[EP,1];

(*  mdroplist is not used *)
(* append to monodroplist *)
(*Do[( 
If[EM[[j]]>10^(-6),
monodroplist=Append[monodroplist,j];
];
),{j,nvariables+1,Length[EM]}]; (* don't allow the first nvariables 
monomials to get dropped *) 
*)
(* append to polydroplist *)
Do[( 
If[EP[[j]]>10^(-6),
	polydroplist=Append[polydroplist,j];
];
),{j,1,Length[EP]}];
),{nmerid,1,Length[nmeratoms]}];
(*If[print,Print["{nmono,maxmono,minmono} = ",{Length[EM],Max[EM],Min[EM]}];];  mdroplist is not used *)
If[print,Print["{npoly,maxpoly,minpoly} = ",{Length[EP],Max[EP],Min[EP]}];];

(*monodroplist=Sort[DeleteDuplicates[monodroplist]]; mdroplist is not used *)
polydroplist=Sort[DeleteDuplicates[polydroplist]];
(*lmdrop=Length[monodroplist]; mdroplist is not used *)
lpdrop=Length[polydroplist];
(*If[print,Print["{lmdrop,lpdrop} after deleting duplicates = ",{lmdrop,lpdrop}];];**)
(*If[print,Print["numbers of {monos, polys} that would be left
		after eliminating those w/o correct limiting behavior = ",{nmono-lmdrop,npoly-lpdrop}];];*)
If[diagnose,
	(*Export[DataDir<>"monodroplist ",monodroplist,"Table"]; mdroplist is not used *)
	Export[DataDir<>"polydroplist ",polydroplist,"Table"];
];

(* *************** *)
Print["\n "];
(* get rid of m(0) and p(0) because neither goes to zero when an nmer is removed to inf *)
PPCharStringmono=StringDelete[PPCharStringmono,"m(0) = 1.0D0 \n "];
PPCharStringpoly=StringDelete[PPCharStringpoly,"p(0) = m(0)\n"];


(* do polynomials first *)
(* *************** *)
Print["\n "];
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Rename polynomials "];];

(* PPCharStringpoly and PPCharStrinmono are created as a global string in GetAssignEVMonoPoly  *)

If[print,Print["       Change p to q for those on polydroplist list - see idyn countdown "]];
Do[( 
	idyn=i; (* global for dynamic *)
	PPCharStringpoly=StringReplace[PPCharStringpoly,
		"p("<>ToString[polydroplist[[i]]]<>")"-> 
		"q("<>ToString[polydroplist[[i]]]<>")"
	];			
),{i,Length[polydroplist],1,-1}];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly1d",PPCharStringpoly,"Text"];
];

(* these are for getting the q and p numbers *)
(* make a list of all the q definitions *)
qlist={};
Do[( 
If[StringContainsQ[PPCharStringpoly,"q("<>ToString[i]<>") ="],
	qlist=Append[qlist,i];
];
),{i,1,npoly}];
If[diagnose,PLHqlist=qlist;];
(* make a list of all the p definitions *)
plist={};
Do[( 
If[StringContainsQ[PPCharStringpoly,"p("<>ToString[i]<>") ="],
	plist=Append[plist,i];
];
),{i,1,npoly}];
If[diagnose,PLHplist=plist;];

(*pnumber=Length[pnewlist];*)
pnumber=Length[plist];
(*rnumber=Length[pnewlist];*)
qnumber=Length[qlist];
npolynow=pnumber; (* global *)

(* this used to put q's first, then p's thne u's.  Bad idea because in some cases the q's 
depend on p's and if the q's come first then the p's have not yet been defined.  So now we
keep the p's and q's in the original order *)
(* make a new PPCharStringpoly by putting all the p's and q's in their original order,
then the u's (which will become superpolynomials later)  *)
(*
PPCSPtab=textconvert[PPCharStringpoly,"text","table",DataDir];
If[PPCSPtab[[1,3]]==0,PPCSPtab[[1,3]]="0"];
(* leave these loops to get a count of the p's and q's *)
If[diagnose,PLHPPCSPlines=PPCSPtab;];
PPCSPtab1={};
Do[(
	If[StringTake[PPCSPtab[[i,1]],2]=="q(",
		PPCSPtab1=Append[PPCSPtab1,PPCSPtab[[i]]];
	];
),{i,1,Length[PPCSPtab]}];
If[diagnose,PLHPPCSPtabq=PPCSPtab1;];
Do[(
	If[StringTake[PPCSPtab[[i,1]],2]=="p(",
		PPCSPtab1=Append[PPCSPtab1,PPCSPtab[[i]]];
	];
),{i,1,Length[PPCSPtab]}];
If[diagnose,PLHPPCSPtabqp=PPCSPtab1;];
(*Do[(
	If[StringTake[PPCSPtab[[i,1]],2]=="u(",
		PPCSPtab1=Append[PPCSPtab1,PPCSPtab[[i]]];
	];
),{i,1,Length[PPCSPtab]}];*)
If[diagnose,PLHPPCSPtabqu=PPCSPtab1;];
PPCharStringpoly=textconvert[PPCSPtab1,"table","textstandard",DataDir];
If[diagnose,
	Export[DataDir<>"PPCharStringpoly2c",PPCharStringpoly,"Text"];
];
*)
mnumber=nmono;
unumber=Length[groupswithdups];
If[print,Print["number of m = ",mnumber];];
If[print,Print["number of p = ",pnumber];];
If[print,Print["number of q = ",qnumber];];


If[diagnose,
	Export[DataDir<>"PPCharStringmono2b ",PPCharStringmono,"Text"];
	Export[DataDir<>"PPCharStringpoly2b ",PPCharStringpoly,"Text"];
	PLHPPCharStringpoly2b=PPCharStringpoly;
];
	
{PPCharStringpoly,qnumber}=RenumberFortranExistingq[PPCharStringpoly];
If[diagnose,
	Export[DataDir<>"PPCharStringmono41 ",PPCharStringmono,"Text"];
	Export[DataDir<>"PPCharStringpoly41 ",PPCharStringpoly,"Text"];
];
{PPCharStringpoly,pnumber}=RenumberFortranExistingpPP[PPCharStringpoly];
If[diagnose,
	Export[DataDir<>"PPCharStringmono41a ",PPCharStringmono,"Text"];
	Export[DataDir<>"PPCharStringpoly41a ",PPCharStringpoly,"Text"];
];

(*pnumber=rnumber;*)
If[print,Print["          mnumber now = ",mnumber]];
If[print,Print["          qnumber now = ",qnumber]];
If[print,Print["          pnumber now = ",pnumber]];
npolynow=pnumber;
nmononow=mnumber;
nqnow=qnumber;


(* NB: At this point we have pnumber,rnumber,number,mnumber,PPCharStringmono,
and PPCharStringpoly that all apply to the uncompacted but purified basis *)
(* **************** End of Part One    *******************  *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* The second part is no longer operational. If you want to see what it is, see v10.0, which still has it *)
Goto[skipaddition];

(* *)
(* *)
(* *)
(* *)
(* *)
(* ********  Third Part - compact by getting rid of unused m's and q's  ******* *)

Label[skipaddition];
If[skipcompaction,Goto[skipcompact];];

Print[DateString[]];
Print["skipcompaction is false, so compaction is starting"];
Print[Style["Starting Part Three: compaction ",Blue,FontSize->14]];
Print[DateString[]];
Goto[startcpct];

Label[skipcompact];
If[ValueQ[skipcompaction] && skipcompaction,
Print["skipcompaction is true, so this is being skipped"];
csmonopurecpt=PPCharStringmono;
cspolypurecpt=PPCharStringpoly;
unumber=Length[groupswithdups];
{mnumbernow,qnumbernow,pnumbernow}={mnumber,qnumber,pnumber};
Goto[Skipcpct];];

Label[startcpct];
{csmonopurecpt,cspolypurecpt,{mnumbernow,qnumbernow,pnumbernow}}=
	CompactionPure[mnumber,pnumber,qnumber];
(*unumber=Length[groupswithdups];
unumbernow=unumber;*)
Print["Compaction done. {mnumbernow,qnumbernow,pnumbernow} = ",
	{mnumbernow,qnumbernow,pnumbernow}];	
pnumber=pnumbernow;
qnumbersave=qnumbernow;
If[pnumber==0,Print["No polynomials have the correct limiting behavior"];
	Print["    Aborting"];Abort[];];



(* ********  End of Third Part  ******* *)
(* *)
(* *)
(* *)
(* *)
(* *)
Label[Skipcpct];
(* ******* Part Four -  Export Fortran File  ******** *)
Print[DateString[]];
If[print,Print[Style["Starting Part 4: exporting fortran file",Blue,FontSize->14]]];
PPCharStringmono=csmonopurecpt;
PPCharStringpoly=cspolypurecpt;
If[diagnose,PLHPPCharStringmono654=PPCharStringmono;
	PLHPPCharStringpoly654=PPCharStringpoly;];
print=True;
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Create and Export new fortran file named in OutputCptFilename"]]; 
fortranname=OutputCptFilename;
rijnames[[1]]=rijnamesnow;
CommentXAssignments=GenerateFortranCommentWithXAssignments[];
CommentXAssignmentsSave=CommentXAssignments;
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

If[print,Print["Making/exporting ",fortranname," with \n{mnumbernow,qnumbernow,pnumbernow} = ",
	{mnumbernow,qnumbernow,pnumbernow}];];
If[diagnose,PLHCSM567=PPCharStringmono;PLHCSP5667=PPCharStringpoly;];
MakeExportFortranPure[CommentXAssignments,PPCharStringmono,PPCharStringpoly,
	pnumbernow,qnumbernow,mnumbernow];
fortranname=fortrannamesave;
(*
If[!skipcompaction,
	Print["\nSome helpful information"];
	fortranname = OutputCptFilename;
	newfortran=Import[OutputCptFilename,"Text"];
	nf=textconvert[newfortran,"text","textlines",DataDir];
	nf=Drop[nf,natomsparent(natomsparent-1)/2];
	newfortran=textconvert[nf,"textlines","text",DataDir];
	CommentXAssignments2=GenerateFortranCommentWithXAssignmentsPure[];
	Print[CommentXAssignments2];
	fortranname=fortrannamesave;
];
*)
(* ******* End of Part Four   ******** *)
(* *)
(* *)
(* *)
(* ******* Part Four B add groups   ******** *)
If[skipgroups,Goto[groupsskipped];];
Label[skiptosect4b];
If[!skipcompaction && !usingpurecompactedinputfile,
	Print["\nSome helpful information"];
	fortranname = OutputCptFilename;
	newfortran=Import[OutputCptFilename,"Text"];
	nf=textconvert[newfortran,"text","textlines",DataDir];
	nf=Drop[nf,natomsparent (natomsparent-1)/2];
	newfortran=textconvert[nf,"textlines","text",DataDir];
	CommentXAssignments2=GenerateFortranCommentWithXAssignmentsPure[];
	Print[CommentXAssignments2];
	fortranname=fortrannamesave;
];

If[usingpurecompactedinputfile,
	Print["\nSome helpful information"];
	fortranname=fortrannamesave;
	initializeevalmp[fortranname];
	newfortran=Import[fortranname,"Text"];
	nf=textconvert[newfortran,"text","textlines",DataDir];
	nf=Drop[nf,natomsparent (natomsparent-1)/2];
	newfortran=textconvert[nf,"textlines","text",DataDir];
	CommentXAssignments2=GenerateFortranCommentWithXAssignmentsPure[];
	Print[CommentXAssignments2];
	fortranname=fortrannamesave;
];

If[!usingpurecompactedinputfile,
(*CopyFile[OutputCptFilename,StringDrop[OutputFortranname,-4]<>"noderiv.f90"];*)
(* we want to start with the output from section 4 *)
fortranname=OutputFortranname;
,
Print["usingpurecompactedinputfile = True, so skipping purification and compaction"];
Print["Skipped to Section 4b -- adding groups"];
(*CopyFile[fortranname,StringDrop[fortranname,-4]<>"noderiv.f90"];*)
(* we want to start with the original file, since that was purified and compacted *)
fortranname=fortrannamesave;
{pnumbernow,mnumbernow,qnumbernow}=initializeevalmp[fortranname];
];
If[diagnose,PLHPPCharStringmono654h=PPCharStringmono;
	PLHPPCharStringpoly654h=PPCharStringpoly;];
{groups,groupswithdups}=
	findevaluatepermsymgroups24nogroupvalues[fortranname,natomsparent,xyzperms,
	groupsfname,groupswithdupsfname,DataDir];
(* pnumber=pnumbernow; and qnumbersave=qnumbernow; have been defined,
as have PPCharStringmono and PPCharStringpoly  *)
Print["Starting conversion to groups with superpolynomials"];
csmonopurecpt=PPCharStringmono;
cspolypurecpt=PPCharStringpoly;
unumbernow=Length[groupswithdups];
(* now change all p's to q's with numbers starting with qnumbernow+1 *)
Do[(
	idyn=i;
	cspolypurecpt=StringReplace[cspolypurecpt,
		"p("<>ToString[i]<>")" ->"q("<>ToString[qnumbernow+i]<>")"];
),{i,pnumbernow,1,-1}];
pnumber=pnumbernow;
qnumbersave=qnumbernow;
qnumbernow=qnumbernow+pnumbernow;
If[diagnose,
	Export[DataDir<>"cspolypurecpt2h",cspolypurecpt,"Text"];
];
(* now add the new superpolynomials, but as lhs u's and rhs q's
and also adding qnumbersave to the value of each q so as to maintain
correspondence *)
cspolypurecpt=cspolypurecpt<>"\n"<>generatesuperpolydefs[qnumbersave];
If[diagnose,
	Export[DataDir<>"cspolypurecpt2i",cspolypurecpt,"Text"];
	Export[DataDir<>"csmonopurecpt2i",csmonopurecpt,"Text"];
];
(* now rename the new 'superpolynomials' *)
Do[(
cspolypurecpt=StringReplace[cspolypurecpt,
		"u("<>ToString[i]<>")" ->"p("<>ToString[i]<>")"];
),{i,1,unumbernow}];
pnumbernow=unumbernow;
If[diagnose,
	Export[DataDir<>"cspolypurecpt2j",cspolypurecpt,"Text"];
];
If[print,Print["{mnumbernow,qnumbernow,pnumbernow} = ",
	{mnumbernow,qnumbernow,pnumbernow}];];
If[print,Print["The p's are now superpolynomials"]];
If[print,Print["The last unumber = ",pnumber," of the q's were p's"]];
If[print,Print["     and these q's are needed for the superpolynomials"]];	
(* Now ReDo the file export part that was in Section 4 *)
Print[DateString[]];
If[print,Print[Style["Starting re-export of  fortran file",Blue,FontSize->14]]];
PPCharStringmono=csmonopurecpt;
PPCharStringpoly=cspolypurecpt;
If[diagnose,PLHPPCharStringmono654b=PPCharStringmono;
	PLHPPCharStringpoly654b=PPCharStringpoly;];
print=True;
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Create and Export new fortran file named in OutputCptFilename"]]; 
fortranname=OutputCptFilename;
rijnames[[1]]=rijnamesnow;
CommentXAssignments=GenerateFortranCommentWithXAssignments[];
CommentXAssignmentsSave=CommentXAssignments;
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

If[print,Print["Making/exporting ",fortranname," with \n{mnumbernow,qnumbernow,unumbernow,pnumbernow} = ",
	{mnumbernow,qnumbernow,pnumbernow}];];
MakeExportFortranPure[CommentXAssignments,PPCharStringmono,
	PPCharStringpoly,pnumbernow,qnumbernow,mnumbernow];
fortranname=fortrannamesave;
Label[groupsskipped];
(* *)
(* *)
(* ******* End of Part Four B  ******** *)
(* *)
(* *)
(* *)
(* ******  Part Five - Add derivatives if deesired ********* *)
Print["\n"];
Print[DateString[]];
Print[Style["Starting Part Five: derivatives",Blue,FontSize->14]];
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Add the derivatives, if desired, to the output fortran file"]];
If[!ProvideDerivatives,Print["   No derivatives desired"];Goto[skipderivatives];];
fortranname = OutputCptFilename;
time=Timing[
MakeExportFortranDDWithChenDerivativesPure[CommentXAssignments,
		PPCharStringmono,PPCharStringpoly,pnumbernow,qnumbernow,mnumbernow];
		][[1]];	
Print["The derivatives program took ",time," sec. = ",time/60," min."];
fortranname=fortrannamesave;



If[AddFastForwardDerivatives,
Print["\nAdding Fast Forward Derivatives"];
	fortranname = OutputCptFilename;
	newfortran=Import[OutputCptFilename,"Text"];
	newfortran=StringDelete[newfortran," end module bemsa"];
	time=Timing[
	{z,CharStringF,dpcoef,CharStringdmm}=ConvertChenDerivatives[fortranname];
	][[1]];
	Print["The fast forward derivatives program took ",time," sec. = ",time/60," min."];
	newfortran=newfortran<>z;
	newfortran=newfortran<>"\n end module bemsa";
	Export[fortranname,newfortran,"Text"];
	fortranname=fortrannamesave;	
];

If[AddReverseDerivatives,
Print["\nAdding Reverse Derivatives"];
	fortranname = OutputCptFilename;
	newfortran=Import[OutputCptFilename,"Text"];
	newfortran=StringDelete[newfortran," end module bemsa"];
	time=Timing[
	tout=MakeBackwardsFortRoutine[mnumbernow,pnumbernow,qnumbernow];
	][[1]];
	Print["The reverse derivatives program took ",time," sec. = ",time/60," min."];
		newfortran=newfortran<>tout;
	newfortran=newfortran<>"\n end module bemsa";
	Export[fortranname,newfortran,"Text"];
	fortranname=fortrannamesave;
];
Label[skipderivatives];


If[!skipcompaction,
	Print["\nAdding Helpful Information"];
	fortranname = OutputCptFilename;
	newfortran=Import[OutputCptFilename,"Text"];
	If[!ValueQ[CommentXAssignments2],
	nf=textconvert[newfortran,"text","textlines",DataDir];
	nf=Drop[nf,natomsparent (natomsparent-1)/2];
	newfortran=textconvert[nf,"textlines","text",DataDir];
	CommentXAssignments2=GenerateFortranCommentWithXAssignmentsPure[];
	];
	newfortran=Import[OutputCptFilename,"Text"];
	newfortran=StringDelete[newfortran," end module bemsa"];
	newfortran=newfortran<>CommentXAssignments2;
	newfortran=newfortran<>"\n end module bemsa";
	Export[fortranname,newfortran,"Text"];
	fortranname=fortrannamesave;
];

(* ******  End of Part Five  ********* *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* ********Part Six - Tests ******* *)
Print[DateString[]];
Print[Style["Starting Part Six: tests",Blue,FontSize->14]];
Print["\n"];
If[print,Print["***Start: ",DateString[]]];
If[print,Print["Run tests, if desired"]];
fortranname = OutputFortranname;
If[ UseRunTests== True, 
	RunTestsPure[mnumbernow,pnumbernow,qnumbernow];
	(*TestPermInvPure[mnumbernow,pnumbernow,qnumbernow];*)
	,
	Print["UseRunTests is set False"];
	];
fortranname=fortrannamesave;
(* ******** End of Part Six  ******* *)
(* *)
(* *)
(* *)
(* *)
(* *)
If[print,Print[Style["***Finished: ",Blue,FontSize->14]]];
If[print,Print[DateString[]]];
(* no output *)

];






MakeExportFortranPure[CommentXAssignments_,CharStringmono_,CharStringpoly_,pnumber1_,
	qnumber1_,mnumber1_]:=Module[
{fnamea,natomsa,rijnamesa,bemsaa,monostarta,monoenda,StringAssigns,
polystarta,polyenda,newfortran,xstart1,xstart2,xstart3,xmiddle,xending,CSMlines,
AnotherRoundM,AnotherRoundP,CSPlines,CSMlinesch,CSPlinesch,jsave,outM,outP,
CSMappend,CSMremaining,CSPappend,CSPremaining,nvar,nmn,npol,z,Tijnums,
linedist,diagnose,xform,nvariables},
(*NB: rnumber is not used *)
diagnose=False;
If[diagnose,PLHinputCSM=CharStringmono; PLHinputCSP=CharStringpoly];
nvariables=natomsparent (natomsparent-1)/2;
(* enter fortran template pieces*)
xstart1= "
module bemsa
  implicit none
  integer, parameter :: wp  = kind(1.0D0)
  real(wp)::r(12z1047,12z1047)
  real(wp), parameter :: a = 2.d0
  real(wp),dimension(1:3025)::q
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
real(wp),dimension(0:578)::p
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
        real(wp) :: xyz(12z1047,3)
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
xform="";
Do[(
If[xtransform[[i]]==1,
	xform=xform<>"    x("<>ToString[i]<>") = dexp(-x("<>ToString[i]<>")/a) \n";
	,
	xform=xform<>"    x("<>ToString[i]<>") = 1.d0/x("<>ToString[i]<>") \n";
];
),{i,1,nvariables}];
xstart2=xstart2<>"
     do i=1,12z1047
       r(i,i)=0.d0
       do j=i+1,12z1047
         r(i,j)=sqrt( (xyz(i,1)-xyz(j,1))**2 + (xyz(i,2)-xyz(j,2))**2 + &
			(xyz(i,3)-xyz(j,3))**2 )
         r(j,i)=r(i,j)
       enddo
      enddo

"; (* modified this 4-Nov-2022 *)
xstart2=xstart2<>xform;
(*
xstart2=xstart2<>"     x(:) = x(:)/a \n";
xstart2=xstart2<>"     x(:) = dexp(-x(:)) \n";
*)
xstart2=xstart2<>"     end subroutine\n\n\n";

newfortran=CommentXAssignments<>xstart1<>xstart2<>xstart3;
If[diagnose,PLHnewfortran1=newfortran];
If[diagnose,PLHCSMxx=CharStringmono;];

(* convert CharStringmono to lines of text *)
Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
CSMlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[!StringContainsQ[CSMlines[[1]],"m(0)"],
	CSMlines=Prepend[CSMlines,"    m(0) = 1.d0"];
];
If[!StringContainsQ[CSMlines[[2]],"        m(1)"],
	CSMlines[[2]]=StringReplace[CSMlines[[2]],
	"        m(1)"->"    m(1)"];
];
If[diagnose,PLHCSMlinesxxx=CSMlines];
nfadd=Ampersand[CSMlines];
nfadd=StringReplace[nfadd," ** "->"**"];  (* added 11Feb2023 *)
newfortran=StringDelete[newfortran,"\t"]<>nfadd;

newfortran=newfortran<>xmiddle;
If[diagnose,PLHnewfortran2=newfortran];

(* convert CharStringpoly to lines of text *)
If[diagnose,PLHCharStringpoly1=CharStringpoly];
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
CSPlines=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[StringContainsQ[CSPlines[[2]], "p(0) = 0."],CSPlines=Drop[CSPlines,{2}];];
If[diagnose,PLHCSPlines1=CSPlines];
If[!StringContainsQ[CSPlines[[1]], "p(0) = 0."],
	CSPlines=Prepend[CSPlines,"    p(0) = 0."];
];
newfortran=newfortran<>Ampersand[CSPlines];

newfortran=newfortran<>xending;
If[diagnose,PLHnewfortran3=newfortran];

(* fix dimensions *)
nvar=StringCount[CommentXAssignments,"="];
nmn=mnumber1;
npol=pnumber1;
(*Print["{nvar,nmn,npol} = ",{nvar,nmn,npol}];*)
z=StringReplace[newfortran,"1:21"-> "1:"<>ToString[nvar]];
z=StringReplace[z,"0:239"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:619"->  "0:"<>ToString[nmn]];
z=StringReplace[z,"0:578"->  "0:"<>ToString[npol]];
z=StringReplace[z,"0:3027"->  "0:"<>ToString[npol]];
z=StringReplace[z,"1:3025"->  "1:"<>ToString[qnumber1]];
z=StringReplace[z,"natoms1047"->ToString[natomsparent]];
z=StringReplace[z,"12z1047"->ToString[natomsparent]];
If[qnumber1==0,
	z=StringReplace[z,"  real(wp),dimension(1:0)::q"->""];
	];
Export[fortranname,z,"Text"];
(*Export[StringDrop[fortranname,-4]<>"_"<>DateString["Hour"]<>"_"<>DateString["Minute"]<>
	"_"<>DateString["Second"]<>".f90",z,"Text"];*)
z
];





GetFPolyDerivativesPure[]:=Module[
{natoms1,rijnames1,ifrag1,nvariables1,monostart1,monoend1,polystart1,
polyend1,CharStringmono1,CharStringpoly1,CharStringmono,Bx,CharStringpoly,
first,second,C3,Skipj,nstar,nplus,nminus,nopen,nclose,nq,Lx,
xsum,ifraga,natomsa,rijnamesa,nvariablesa,monostarta,monoenda,
polystarta,polyenda,rijnamesnow,nvariables,nmono,npoly,diagnose,skipvvv,xterms
},
diagnose=False;
Print["Starting GetFPolyDerivativesPure"];
(* get character strings CharStringmono and Charstringpoly from Fortran output file *)
{rijnamesnow,nvariables,nmono,npoly,nq}=GetDefinitionstoMathematicaFromFortranOutputPure[];
If[diagnose,Print["-3"];];
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;  (* rijnamesa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
nvariablesa=natomsa (natomsa-1)/2;
If[ifraga==-1,nvariablesa=Length[rijnamesa];];
(* get mono and poly strings for fragment a *)
{monostarta,monoenda,polystarta,polyenda}=
	GetpolymonostartendpointsPure[ifraga];
If[diagnose,Print["-2"];];
{CharStringmono1,CharStringpoly1}=
	GetmonopolycharstringsPure[ifraga,monostarta,monoenda,polystarta,polyenda];
If[diagnose,Print["-1"];];
If[diagnose,
	Export[DataDir<>"CharStringmonoinGetFPolyDerivatives",CharStringmono1,"Text"];
	Export[DataDir<>"CharStringpolyinGetFPolyDerivatives",CharStringpoly1,"Text"];
	If[diagnose,Print["0"];];
];
(* Here is a correction that fixes a problem*)
CharStringpoly1=StringReplace[CharStringpoly1,")+"->") +"];	
If[diagnose,
	Export[DataDir<>"CharStringpolyinGetFPolyDerivatives2",CharStringpoly1,"Text"];
	Print["1"];
];

(* make Bx, tabular form of polynomials *)
(* had to add this to get it to do multiplicative factors correctly, 8/15/2021 PLH *)
CharStringpoly1=StringReplace[CharStringpoly1," * "->"*"];
CharStringpoly=CharStringpoly1;
CharStringpoly=StringReplace[CharStringpoly,"**"->"^"];
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
Bx=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
(*Bx=Drop[Bx,1];  (* get rid of polymial 0 *)*)
If[diagnose,
	Export[DataDir<>"CharStringpolyinGetFPolyDerivatives3",CharStringpoly,"Text"];
	Print["2"];
];
(* doctor up what can be done simply *)
CharStringpoly=CharStringpoly1;
CharStringpoly=StringReplace[CharStringpoly,"**"->"^"];
CharStringpoly=StringReplace[CharStringpoly,"    p("->"    dp("];
CharStringpoly=StringReplace[CharStringpoly,"    q("->"    dq("];
CharStringpoly=StringReplace[CharStringpoly,"    r("->"    dr("];
(*Now turn CharStringpoly into a text line file *)
If[diagnose,
	Export[DataDir<>"CharStringpolyinGetFPolyDerivatives4",CharStringpoly,"Text"];
	Print["3"];
];
Export[DataDir<>"DeleteMe.txt",CharStringpoly,"Text"];
Lx=Import[DataDir<>"DeleteMe.txt",{"Text","Lines"}];
DeleteFile[DataDir<>"DeleteMe.txt"];
(*Lx=Drop[Lx,1];*)  (* get rid of zero order term *)
If[diagnose,
	PLHBxPure=Bx;
	PLHLxPure=Lx;
	Print["4"];
];

	(* work on polys line by line, using Bx as criteria but changing Lx *)
Monitor[Do[(  
(* we are now working on the ith line *)
	Do[(
		(* we are now working on the jth part of Bx, starting at 2 and going to the end *)
		If[Bx[[i,j]]==0.,nstar=0;nplus=0;nminus=0;nopen=0;nclose=0;Goto[skipvvv];];
		nstar=StringCount[Bx[[i,j]],"*"];
		nplus=StringCount[Bx[[i,j]],"+"];
		nminus=StringCount[Bx[[i,j]],"-"];
		nopen=StringCount[Bx[[i,j]],"("];
		nclose=StringCount[Bx[[i,j]],")"];
		Label[skipvvv];
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
	),{j,2,Length[Bx[[i]]]}];
	Lx[[i]]=Lx[[i]]<>"\n";
),{i,1,Length[Bx]}],ProgressIndicator[i,{1,Length[Bx]}]];

CharStringpoly=StringReplace[StringJoin[Lx]," * "->"*"];
CharStringpoly=StringReplace[CharStringpoly,"^"->"**"];
If[diagnose,
	Export[DataDir<>"CharStringpoly5",CharStringpoly,"Text"];
	Print["5"];
];
CharStringpoly
];





GetpolymonostartendpointsPure[ifraga_]:=Module[
{bemsaa,monostarta,monoenda,polystarta,polyenda,isave,
diagnose
},
diagnose=False;
(* get mono and poly strings for fragment a *)
If[ifraga==-1,
	bemsaa=bemsaTabc;  (* uses the Fortran output as input *)
	,
	bemsaa=ToExpression["bemsaTabc"<>ToString[ifraga]];
];
If[diagnose,PLHbemsaa10=bemsaa;];
Do[(
	If[Length[bemsaa[[i]]]>0 && bemsaa[[i,1]]=="m(0)",isave=i;Break[];];
),{i,1,Length[bemsaa]}];
monostarta=isave;
Do[(
	If[Or[Length[bemsaa[[i]]]<2 , StringTake[bemsaa[[i,1]],2]!="m("(*<>ToString[i-monostarta]<>")"*)],
		isave=i;Break[];];
),{i,monostarta+1,Length[bemsaa]}];
monoenda=isave-1;
If[diagnose,PLHmonostarta=monostarta;PLHmonoenda=monoenda;];

Do[(
	If[Length[bemsaa[[i]]]>0 && 
		Or[bemsaa[[i,1]]=="p(1)",bemsaa[[i,1]]=="q(1)",bemsaa[[i,1]]=="r(1)"],
		isave=i;Break[];];
	),{i,1,Length[bemsaa]}];
polystarta=isave;
(*
Do[(
	If[Or[Length[bemsaa[[i]]]==0 , StringTake[bemsaa[[i,1]],2]!="p("(*<>ToString[i-polystarta]<>")"*)],
		isave=i;Break[];];
),{i,polystarta+1,Length[bemsaa]}];
polyenda=isave-1;
*)
Do[(
	If[Or[Length[bemsaa[[i]]]<2 , And[StringTake[bemsaa[[i,1]],2]!="p(",
StringTake[bemsaa[[i,1]],2]!="q(",
StringTake[bemsaa[[i,1]],2]!="r("]
],
		isave=i;Break[];];
),{i,polystarta+1,Length[bemsaa]}];
polyenda=isave-1;
If[diagnose,PLHq={monostarta,monoenda,polystarta,polyenda};];

{monostarta,monoenda,polystarta,polyenda}
];




GetmonopolycharstringsPure[ifraga_,monostarta_,monoenda_,polystarta_,polyenda_]:=Module[
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




EvalPolyNew[PPCharStringpoly_,plistpur_]:=Module[
{S1,S2},
S1=StringReplace[StringReplace[PPCharStringpoly," "->""],"\n"->"; "];
S1=StringReplace[S1,"D0"->""];
S1=StringReplace[S1,"d0"->""];
S1=StringReplace[S1,"**"->"^"];  (* added 19 Feb 2023 *)
S2=StringDelete[StringDelete[S1,")"],"("];
S2=S2<>";";
p0=1.;
ToExpression[S2];
ToExpression[plistpur]
];


(* this function is obsolete *)
EvalqNew[qCharString_,qlistpur_]:=Module[
{S1,S2},
S1=StringReplace[StringDrop[qCharString,4],"\n"->"; "];
S1=StringReplace[S1,"**"->"^"];  (* added 19 Feb 2023 *)
S2=StringDelete[StringDelete[S1,")"],"("];
S2=S2<>";";
ToExpression[S2];
ToExpression[qlistpur]
];


(* this function is obsolete *)
EvalrNew[rCharString_,rlistpur_]:=Module[
{S1,S2},
S1=StringReplace[StringDrop[rCharString,4],"\n"->"; "];
S2=StringDelete[StringDelete[S1,")"],"("];
ToExpression[S2];
ToExpression[rlistpur]
];


EvalMonoNew[PPCharStringmono_,mlistpur_]:=Module[
{S1,S2},
S1=StringReplace[StringReplace[PPCharStringmono," "->""],"\n"->"; "];
S1=StringReplace[S1,"D0"->""];
S1=StringReplace[S1,"d0"->""];
S1=StringReplace[S1,"**"->"^"];  (* added 19 Feb 2023 *)
S2=StringDelete[StringDelete[S1,")"],"("];
S2=S2<>";";
ToExpression[S2];
ToExpression[mlistpur]
];


PPRenumberFortranExistingmInMonoAndPoly::usage="RenumberFortranExistingmInMonoAndPoly[CharListmono,CharListpoly]
Takes all the lines beginning m(i)= in StringList
renumbers it and all the m(i) throughout in consecutive
numerical order
";
PPRenumberFortranExistingmInMonoAndPoly[CharListmono_,CharListpoly_]:=Module[
{Axold,Axnew,Bxold,Temppoly,diagnose},
Axold=CharListmono;
diagnose=False;
Temppoly=CharListpoly;
(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[diagnose,
	PLHAxold=Axold;
	PLHBxold=Bxold;
	PLHCLP=CharListpoly;
];
(* replace m(i) if needed throughout monomials so as to have numerical order *)
(* Bxold (table format) is used for the criterion, but Axold is what is changed *)
(* the i+1 in Bxold is due to the zero-order term there *)
(* In addition, we want the monomial numbering in the polynomials to be the same as in the
monomials, so every time we change something in the monomials, we do a change of those
monomial numbers in the polynomials *)
Axnew=Axold;
Do[(
idyn=Length[Bxold]-i;
If["m("<>ToString[i]<>")"!= Bxold[[i,1]],
Axnew=StringReplace[Axold,Bxold[[i,1]]->  
		"m("<>ToString[i]<>")"];
Temppoly=StringReplace[Temppoly,Bxold[[i,1]]->  
		"m("<>ToString[i]<>")"];
Axold=Axnew;
];
),{i,1,Length[Bxold]}];
If[diagnose,PLHAxold2=Axold;PLHTemppoly=Temppoly;];

{Axold,Temppoly}
];



RenumberFortranExistingq::usage="RenumberExistingq[StringList_]
Takes all the lines beginning q(X)= in StringList
renumbers it and all the q(X) throughout in consecutive
numerical order
";
RenumberFortranExistingq[StringList_]:=Module[
{Axold,Axnew,Bxold,Skipit,ix},
Axold=StringList;

(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[Bxold[[1]]=={},Bxold=Drop[Bxold,1];];
PLHBxold=Bxold;

(* replace q(i) if needed throughout so as to have numerical order *)
(* Bxold (table format) is used for the criterion, but Axold is what is changed *)

Axnew=Axold;
Print["Renumbering existing q"];
(*Print["Length[Bxold] = ",Length[Bxold]];*)
ix=0;
Do[(
	idyn=Length[Bxold]-i;
	If[Bxold[[i]]=={},Goto[Skipit];];
	If[!StringContainsQ[Bxold[[i,1]],"q"],Goto[Skipit];];
	(*Print[{i,Bxold[[i,1]]}];*)
	If["q("<>ToString[ix]<>")"!= Bxold[[i,1]],
		(*Print[Bxold[[i,1]]," replaced by p(",i,")"];*)
		ix=ix+1;
		Axnew=StringReplace[Axold,Bxold[[i,1]]-> 
			"q("<>ToString[ix]<>")"];
		Axold=Axnew;
	];
Label[Skipit];
),{i,1,Length[Bxold]}];
(*Print[Bxold[[Length[Bxold]]]];
Print[Bxold[[Length[Bxold],1]]," being replaced by p(",Length[Bxold],")"];*)
Axold=Axnew;
Print["qnumber now = ",ix];
Print["Exiting RenumberFortranExistingq"];
{Axold,ix}
];



RenumberFortranExistingr::usage="RenumberExistingr[StringList_]
Takes all the lines beginning r(X)= in StringList
renumbers it and all the r(X) throughout in consecutive
numerical order
";
RenumberFortranExistingr[StringList_]:=Module[
{Axold,Axnew,Bxold,Skipit},
Axold=StringList;

(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[Bxold[[1]]=={},Bxold=Drop[Bxold,1];];

(* replace r(i) if needed throughout so as to have numerical order *)
(* Bxold (table format) is used for the criterion, but Axold is what is changed *)

Axnew=Axold;
Print["Renumbering existing r"];
(*Print["Length[Bxold] = ",Length[Bxold]];*)
ix=0;
Do[(
	idyn=Length[Bxold]-i;
	If[Bxold[[i]]=={},Goto[Skipit];];
	If[!StringContainsQ[Bxold[[i,1]],"r"],Goto[Skipit];];
	(*Print[{i,Bxold[[i,1]]}];*)
	If["r("<>ToString[ix]<>")"!= Bxold[[i,1]],
		(*Print[Bxold[[i,1]]," replaced by p(",i,")"];*)
		ix=ix+1;
		Axnew=StringReplace[Axold,Bxold[[i,1]]-> 
			"r("<>ToString[ix]<>")"];
		Axold=Axnew;
	];
Label[Skipit];
),{i,1,Length[Bxold]}];
(*Print[Bxold[[Length[Bxold]]]];
Print[Bxold[[Length[Bxold],1]]," being replaced by p(",Length[Bxold],")"];*)
Print["Exiting RenumberFortranExistingr"];
Print["rnumber now = ",ix];
Axold=Axnew;
{Axold,ix}
];






RenumberFortranExistingpPP::usage="RenumberExistingr[StringList_]
Takes all the lines beginning p(X)= in StringList
renumbers it and all the p(X) throughout in consecutive
numerical order
";
RenumberFortranExistingpPP[StringList_]:=Module[
{Axold,Axnew,Bxold,Skipit},
Axold=StringList;
Print["Renumbering existing p"];
(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[Bxold[[1]]=={},Bxold=Drop[Bxold,1];];

(* replace q(i) if needed throughout so as to have numerical order *)
(* Bxold (table format) is used for the criterion, but Axold is what is changed *)

Axnew=Axold;

(*Print["Length[Bxold] = ",Length[Bxold]];*)
ix=0;
Do[(
	idyn=Length[Bxold]-i;
	If[Bxold[[i]]=={},Goto[Skipit];];
	If[!StringContainsQ[Bxold[[i,1]],"p"],Goto[Skipit];];
	(*Print[{i,Bxold[[i,1]]}];*)
	If["p("<>ToString[ix]<>")"!= Bxold[[i,1]],
		(*Print[Bxold[[i,1]]," replaced by p(",i,")"];*)
		ix=ix+1;
		Axnew=StringReplace[Axold,Bxold[[i,1]]-> 
			"p("<>ToString[ix]<>")"];
		Axold=Axnew;
	];
Label[Skipit];
),{i,1,Length[Bxold]}];
(*Print[Bxold[[Length[Bxold]]]];
Print[Bxold[[Length[Bxold],1]]," being replaced by p(",Length[Bxold],")"];*)
Axold=Axnew;
Print["pnumber now = ",ix];
Print["Exiting RenumberFortranExistingpPP"];
{Axold,ix}
];






PPEliminateCurrentp[CharString_]:=Module[
{PPCharStringpoly,Bxold,xxx,lj,Cont1,Skipit},
PPCharStringpoly=CharString;
(* convert PPCharStringpoly to table form *)
Export[DataDir<>"DeleteMe.txt",PPCharStringpoly,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
(* get rid of empty lines *)
Do[(
If [Bxold[[i]]=={},Bxold=Drop[Bxold,{i}];];
),{i,Length[Bxold],1,-1}];

Print["Eliiminating currrent p(.) ="];
(* eliminate lines that are "p(.) =" *)
Do[(
If[StringContainsQ[Bxold[[i,1]],"p"],Bxold=Drop[Bxold,{i}];];
),{i,Length[Bxold],1,-1}];
(* convert from table form back into character string text *)
Export[DataDir<>"DeleteMe.txt",Bxold,"Table"];
PPCharStringpoly=Import[DataDir<>"DeleteMe.txt","Text"];
DeleteFile[DataDir<>"DeleteMe.txt"];

PPCharStringpoly=StringDelete[PPCharStringpoly,"	"];
PPCharStringpoly= StringReplace[PPCharStringpoly,"+(0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"-(0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"(0)+"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"(0)-"->""];
PPCharStringpoly=StringReplace[PPCharStringpoly,"="->" = "];
PPCharStringpoly=StringReplace[PPCharStringpoly,"+"->" + "];
PPCharStringpoly=StringReplace[PPCharStringpoly,"-"->" - "];
PPCharStringpoly=StringReplace[PPCharStringpoly,"*"->" * "];
PPCharStringpoly="    "<>PPCharStringpoly;
PPCharStringpoly=StringReplace[PPCharStringpoly,"\n"->"\n    "];

Print["Completed eliminating currrent p(.) ="];
PPCharStringpoly

];


PPGetRidOfZeros[CharString_]:=Module[
{PPCharStringpoly,Bxold,xxx,lj,Cont1,Skipit},
PPCharStringpoly=CharString;
(* convert PPCharStringpoly to table form *)
Export[DataDir<>"DeleteMe.txt",PPCharStringpoly,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
(* get rid of empty lines *)
Do[(
If [Bxold[[i]]=={},Bxold=Drop[Bxold,{i}];];
),{i,Length[Bxold],1,-1}];
(* This deletes ones w/ zeros on rhs and replaces their appearance elsewhere with (0) *)
Do[(
lx=Length[Bxold[[i]]];
If[Bxold[[i,lx]]=="(0)",
If[Bxold[[i,lx-1]]=="=",
xxx=Bxold[[i,1]];
Bxold[[i,1]]="(0)";
Do[(
lj=Length[Bxold[[j]]];
Do[(
If[Bxold[[j,k]]==xxx,Bxold[[j,k]]= "(0)";];
),{k,1,lj}]
),{j,i,Length[Bxold]}];
];
];
),{i,Length[Bxold],1,-1}];
(* eliminate lines that are "(0) = (0)" *)
Do[(
If[Bxold[[i]]== {"(0)","=","(0)"},Bxold=Drop[Bxold,{i}];];
),{i,Length[Bxold],1,-1}];
(* This gets rid of *(0) terms and assigns (0) to other occurances *)
Do[(
If[
Length[Bxold[[i]]]==5 &&Bxold[[i,3]]=="(0)" && Bxold[[i,4]]=="*",
	Bxold[[i]]=Drop[Bxold[[i]],{4,5}];
	Goto[Cont1];
];
If[
Length[Bxold[[i]]]==5 &&Bxold[[i,4]]=="*" && Bxold[[i,5]]=="(0)",
	Bxold[[i]]=Drop[Bxold[[i]],{3,4}];
	Goto[Cont1];
];
Goto[Skipit];
Label[Cont1];
xxx=Bxold[[i,1]];
Bxold[[i,1]]="(0)";
Do[(
lj=Length[Bxold[[j]]];
Do[(
If[Bxold[[j,k]]==xxx,Bxold[[j,k]]= "(0)";];
),{k,1,lj}]
),{j,i,Length[Bxold]}];
Label[Skipit];
),{i,Length[Bxold],1,-1}];
(* convert from table form back into character string text *)Export[DataDir<>"DeleteMe.txt",Bxold,"Table"];
PPCharStringpoly=Import[DataDir<>"DeleteMe.txt","Text"];
DeleteFile[DataDir<>"DeleteMe.txt"];

PPCharStringpoly=StringDelete[PPCharStringpoly,"	"];
PPCharStringpoly= StringReplace[PPCharStringpoly,"+(0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"-(0)"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"(0)+"->""];
PPCharStringpoly= StringReplace[PPCharStringpoly,"(0)-"->""];
PPCharStringpoly=StringReplace[PPCharStringpoly,"="->" = "];
PPCharStringpoly=StringReplace[PPCharStringpoly,"+"->" + "];
PPCharStringpoly=StringReplace[PPCharStringpoly,"-"->" - "];
PPCharStringpoly=StringReplace[PPCharStringpoly,"*"->" * "];
PPCharStringpoly="    "<>PPCharStringpoly;
PPCharStringpoly=StringReplace[PPCharStringpoly,"\n"->"\n    "];
PPCharStringpoly
];


RunTestsPure[mnumbernow_,pnumbernow_,qnumbernow_]:=Module[
{ifrag1,monostart1,monoend1,polystart1,polyend1,CharStringmono1,CharStringpoly1,
npoly1,nmono1,xsum,rijnamesnow,npoly,nmono,nvariables},


Print["Running Tests"];


(* **************** Get Fortran Output Definitions   *******************  *)
(* get mono poly fortran lists from pruned fortran file *)



(*{rijnamesnow,nvariables,nmono,npoly,qnumber}=GetDefinitionstoMathematicaFromFortranOutputPure[];*)

(* **************** Test for remaining duplicates   *******************  *)
TestForRemainingDuplicatesPure[mnumbernow,pnumbernow,qnumbernow];

(* **************** Test for permutational invariance   *******************  *)
Print["{mnumber,pnumber,qnumber} = ",{mnumbernow,pnumbernow,qnumbernow}];
TestPermInvPure[mnumbernow,pnumbernow,qnumbernow];

(* **************** Test time for basis set evaluation   *******************  *)
(* check timing of basis set evaluation  *)
TestMinimumTimeForBasisEvaluation[mnumbernow,pnumbernow,qnumbernow];
(*xsum=Length[rijnamesnow];
timesum=0;
n=5;
Do[(
	Do[(
		dist[[i]]=RandomReal[{0.1,1}];
	),{i,1,xsum}];
	tim=Timing[
		Assignx[];
		EM=IntegerPart[EvalMonoNew[PPCharStringmono,mlistpur]];
		EP=IntegerPart[EvalPolyNew[PPCharStringpoly,plistpur]];
		][[1]];
	timesum=timesum+tim;
),{i,1,n}];
timeav=timesum/n;
Print["Average time in sec. for basis set evaluation = ",timeav];*)

(* no output *)
];





GetDefinitionstoMathematicaFromFortranOutputPure[]:=Module[
{ifrag1,monostart1,monoend1,polystart1,polyend1,CharStringmono1,
CharStringpoly1,npoly1,nmono1,CharStringmono,CharStringpoly,tijn,subbesma,
rijnamesnow,xsum,msum,psum,ifraga,natomsa,rijnamesa,nvariables,npoly,nmono,
CharStringAssignx,CharStringAdd,qposition,mnumber1,pnumber1,qnumber1,
xstart,xend,x444,xyz,isave,diagnose,qcont,qcont2
},
diagnose=False;
If[diagnose,Print["       Executing GetDefinitionstoMathematicaFromFortranOutputPure"];];


(* value of ifrag-1 tells these two functions to use the Fortran output file as an input *)
ifrag1=-1; 
If[diagnose,Print["       Creating bemsa files"];];
CreatebemsaFiles[ifrag1];
{monostart1,monoend1,polystart1,polyend1}=
	GetpolymonostartendpointsPure[ifrag1];
If[diagnose,Print["1"];];
If[diagnose,Print["       Geting mono/poly character strings"];];
{CharStringmono1,CharStringpoly1}=
	GetmonopolycharstringsPure[ifrag1,monostart1,monoend1,polystart1,polyend1];
If[diagnose,PLHCSM218=CharStringmono1;PLHCSP218=CharStringpoly1;];
If[diagnose,Print["2"];];	
npoly1=polyend1-polystart1;  (* this does not include the zero-order term *)
nmono1=monoend1-monostart1;  (* this does not include the zero-order term *)
(*Print["{nmono1, npoly1} = ",{nmono1, npoly1}];*)
(* Get dimensions *)
If[diagnose,Print["       Getting dimensions"];];
If[diagnose,PLHbemsaTxtlnc200=bemsaTxtLnc;];
subbesma=Flatten[Position[bemsaTxtLnc,"  subroutine bemsav(x,p)"]][[1]];
If[!StringContainsQ[bemsaTxtLnc[[subbesma+2]],"::x"],
	Print["Probelm getting dimensions in GetDefinitionstoMathematicaFromFortranOutput"];
	Abort[];
];
Do[(
If[StringContainsQ[bemsaTxtLnc[[i]],"::q"],isave=i;Goto[qcont];];
),{i,1,Length[bemsaTxtLnc]}];
qnumber1=0;
Goto[qcont2];
Label[qcont];
qposition=isave;
If[!StringContainsQ[bemsaTxtLnc[[qposition]],"::q"],
	Print["Probelm getting  q dimensions in GetDefinitionstoMathematicaFromFortranOutput"];
	Abort[];
];
qnumber1=ToExpression[StringDrop[StringDrop[bemsaTxtLnc[[qposition]],-4],23]];
Label[qcont2];
If[!StringContainsQ[bemsaTxtLnc[[subbesma+4]],"::p"],
	Print["Probelm getting  p dimensions in GetDefinitionstoMathematicaFromFortranOutput"];
	Abort[];
];
pnumber1=ToExpression[StringDrop[StringDrop[bemsaTxtLnc[[subbesma+4]],-4],25]];
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
mnumber1=nmono;
If[diagnose, PLHmpq={mnumber1,pnumber1,qnumber1};
If[diagnose,Print["3"];];];
(*npoly=ToExpression[StringDrop[StringDrop[bemsaTxtLnc[[subbesma+4]],-4],25]];*)
npoly=pnumber1; (* this does not include the zero-order term *)
(* get EvalMono and EvalPoly *)
If[diagnose,Print["       Making Changes to mono/poly character strings"];];
CharStringmono=CharStringmono1;
CharStringmono=StringReplace[CharStringmono,"D0"->""];
CharStringmono=StringReplace[CharStringmono,"**"->"^"];  (* added 17 Feb 2023 *)
CharStringpoly=CharStringpoly1;
CharStringpoly=StringReplace[CharStringpoly,"D0"->""];
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
(*psum=npoly1;*)
psum=npoly;
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
temp=PrintTemporary["       GetAssignEVMoloPoly"];
GetAssignEVMonoPolyPure[ifraga,natomsa_,rijnamesa];
temp=PrintTemporary["       Completed GetDefinitionstoMathematicaFromFortranOutputPure"];
Pause[1];
{rijnamesnow,nvariables,mnumber1,pnumber1,qnumber1}
];







GetAssignEVMonoPolyPure::usage="
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
EvalPoly[]  Evaluates the q and p polynomials for the current monomials

";
GetAssignEVMonoPolyPure[ifraga_,natomsa_,rijnamesa_]:=Module[
{bemsaa,isave,monostarta,monoenda,polystarta,polyenda,
CharStringmonoa,CharStringpolya,CharString2a,CharString11a,
npolya, nmonoa,CharStringAsgn,nvariablesa,diagnose
},
diagnose=False;
nvariablesa=natomsa (natomsa-1)/2;
If[ifraga==-1,nvariablesa=Length[rijnamesa];];

(* get mono and poly strings for fragment a *)
{monostarta,monoenda,polystarta,polyenda}=
	GetpolymonostartendpointsPure[ifraga];
{CharStringmonoa,CharStringpolya}=
	GetmonopolycharstringsPure[ifraga,monostarta,monoenda,polystarta,polyenda];
	PPCharStringmono=CharStringmonoa;  (* Global for Polynomial pruning program *)
	PPCharStringpoly=CharStringpolya;  (* Global for Polynomial pruning program *)
(* Develop EVMono and EVPoly based on fragment a *)
CharStringmonoa=StringReplace[CharStringmonoa,"D0"->""];
CharStringpolya=StringReplace[CharStringpolya,"D0"->""];
PPCharStringmono=StringReplace[PPCharStringmono,"D0"->""];
PPCharStringpoly=StringReplace[PPCharStringpoly,"D0"->""];

CharString2a=bemsaToMathematica[CharStringmonoa];  (* this changes ** to ^, among
    others *)
If[diagnose,PLHCharString2a=CharString2a;];
If[diagnose,PLHCharStringmonoa=CharStringmonoa;];
ToExpression[CreateEvalMono[CharString2a]];

CharString11a=bemsaToMathematica[CharStringpolya];
If[diagnose,PLHCharString11a=CharString11a;];
If[diagnose,PLHCharStringpolya=CharStringpolya;];
ToExpression[CreateEvalPoly[CharString11a]];

(*Print["EVMono and EVPoly have been defined"]
Print["     EVPoly has ",StringCount[CharString11a,"="]," terms"];
Print[CreateEvalPoly[CharString11a]];*)

(* get pnumber from CharStringpolya *)
(* CharStringpolya=PLHCharStringpolya;*) (* removed 18 Feb 2023 not needed *)
CSPatab=textconvert[CharStringpolya,"text","table",DataDir];
lhs=CSPatab[[All,1]];
pnums={};
Do[(
If[StringTake[lhs[[i]],1]=="p",
pnums=Append[pnums,ToExpression[StringDrop[StringDrop[lhs[[i]],2],-1]]];
];
),{i,1,Length[CSPatab]}];
pnumber=Max[pnums];

(* this is not needed because EvalPolyNew evaluates PPCharStringpoly (which has
the q's and p's in the proper order) before output of plistpur *)
(*
(* now get qnumber and pqlist in order of CharStringpolya *)
CSPalines=textconvert[CharStringpolya,"text","textlines",DataDir];
qnumber=Length[CSPalines-pnumber];
pqlist={p0};  (*  needs to be global *)
Do[(
posit=StringPosition[CSPalines[[i]],"="][[1,1]];
def=StringDelete[StringDelete[StringDelete[StringTake[CSPalines[[i]],posit-1]," "],")"],"("];
pqlist=Append[pqlist,def];
),{i,1,Length[CSPalines]}];
*)

(*npolya=polyenda-polystarta;  (* this does not include the zero-order term *)*)
npolya=pnumber;
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


{polystarta, polyenda, npolya, nmonoa}
];








TestForRemainingDuplicatesPure[mnumbernow_,pnumbernow_,qnumbernow_]:=Module[
{success,xb,positions,xsum,rijnamesnow,duplicateEM,duplicateEP,
gatheredEM,gatheredEP,runs,gatheredEMpairs,gatheredEPpairs,
nvariables,nmono,npoly,EM,EP,mnumber,pnumber,qnumber,diagnose
},
diagnose=False;
mnumber=mnumbernow;
pnumber=pnumbernow;
qnumber=qnumbernow;
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
(* Check for Duplicate values, given random assignments *)
success=False;
{rijnamesnow,nvariables,nmono,npoly,qnumber}=GetDefinitionstoMathematicaFromFortranOutputPure[];
If[diagnose,Print["{rijnamesnow,nvariables,nmono,npoly,qnumber} =",
          {rijnamesnow,nvariables,nmono,npoly,qnumber}];];
rijnames[[1]]=rijnamesnow;
(*Print["rijnamesnow = ",rijnamesnow];
Print["{nvariables,nmono,npoly} = ",{nvariables,nmono,npoly}];*)
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)

mlistpur={"m0"};
Do[(
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];
If[diagnose,Print["dist,mlistpur,plistpur assignned"];];
If[diagnose,PLHmlistpur=mlistpur;PLHplistpur=plistpur;];

duplicateEM={};
duplicateEP={};
runs=4;
Do[(  (* this is the loop over runmum runs *)
  dist=Table[0,{i,1,xsum}];  (* needs to be global *)
  Do[(
     dist[[i]]=RandomReal[{0.1,1}];
     ),{i,1,xsum}];
  Assignx[];
  EM=IntegerPart[10^10*EvalMonoNew[PPCharStringmono,mlistpur]];
  EP=IntegerPart[10^10*EvalPolyNew[PPCharStringpoly,plistpur]];
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





TestForRemainingDuplicatesPureOld[mnumbernow_,pnumbernow_,qnumbernow_]:=Module[
{success,xb,positions,Duplicates,xsum,rijnamesnow,
nvariables,nmono,npoly,EM,EP,mnumber,pnumber,qnumber,diagnose
},
diagnose=False;
mnumber=mnumbernow;
pnumber=pnumbernow;
qnumber=qnumbernow;

(* Check for Duplicate values, given random assignments *)
success=False;
{rijnamesnow,nvariables,nmono,npoly,qnumber}=GetDefinitionstoMathematicaFromFortranOutputPure[];
If[diagnose,Print["{rijnamesnow,nvariables,nmono,npoly,qnumber} =",
          {rijnamesnow,nvariables,nmono,npoly,qnumber}];];
rijnames[[1]]=rijnamesnow;
(*Print["rijnamesnow = ",rijnamesnow];
Print["{nvariables,nmono,npoly} = ",{nvariables,nmono,npoly}];*)
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)

dist=Table[0,{i,1,xsum}];  (* needs to be global *)
Do[(
dist[[i]]=RandomReal[{0.1,1}];
),{i,1,xsum}];
Assignx[];

mlistpur={"m0"};
Do[(
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];
If[diagnose,Print["dist,mlistpur,plistpur assignned"];];
If[diagnose,PLHmlistpur=mlistpur;PLHplistpur=plistpur;];


EM=IntegerPart[10^10*EvalMonoNew[PPCharStringmono,mlistpur]];
EP=IntegerPart[10^10*EvalPolyNew[PPCharStringpoly,plistpur]];
If[diagnose,PLHdupEM=EM;PLHdupEP=EP;];
xb=Assignx[];
(* xb now has the assigned xvalues  *)
(* examine EM *)
If[Length[EM]!=Length[DeleteDuplicates[EM]],
  Print["There are ",Length[EM]-Length[DeleteDuplicates[EM]]," mono duplicates."];
  Duplicates={};
  Do[(
	positions=Position[EM,EM[[i]]][[All,1]];
  (*Print[positions,Length[positions]];*)
	Do[(
		If[Length[positions]>1,Duplicates=Append[Duplicates,positions];]
	),{j,1,Length[positions]}];
  ),{i,1,Length[EM] }];  
  Duplicates=DeleteDuplicates[Duplicates];
  If[diagnose,PLHmonoduplilcates=Duplicates;];
  (*Print["Duplicates = ",Duplicates];*)
  If[Duplicates!={},
	Duplicates=Duplicates-1;  (* this is because the index in the previous loop starts at 1,
	      but the lowest polynomial is m(0) *)
	      Duplicates=DeleteDuplicates[Duplicates];
	Print["Fortran output failed uniqueness of monomials ",Length[Duplicates]," times"];
	Print["Duplicate locations in monomials are: ",Duplicates];
	(*Print[Table[{i,EM[[i]]},{i,1,Length[EM]}]];*)
	,
	success=True;
  ];
  If[success,Print["Fortran ouptut passed uniqueness of monomials"];];
  If[diagnose,PLHEP1=EP;];
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
	      but the lowest polynomial is p(0) *)
	Print["Fortran output failed uniqueness of polynomials ",Length[Duplicates]," times"];
	Print["Duplicate locations in polynomials are: ",Duplicates];
	Do[(
	Print["{Duplicates[[nd,1]],EP[[Duplicates[[nd,1]]+1]]} = ",{Duplicates[[nd,1]],EP[[Duplicates[[nd,1]]]]}];
	Print["{Duplicates[[nd,2]],EP[[Duplicates[[nd,2]]+1]]} = ",{Duplicates[[nd,2]],EP[[Duplicates[[nd,2]]]]}];
	),{nd,1,Length[Duplicates]}];
	,
	success=True
  ];
  If[success,Print["Fortran ouptut passed uniqueness of polynomials"];];
  ,
  success=True;
  Print["Fortran ouptut passed uniqueness of polynomials"];
];

success
];





TestPermInvPure[mnumbernow_,pnumbernow_,qnumbernow_]:=Module[
{permtable,ifraga,natomsa,rijnamesa,permdone,failures,rijnamesnew,
zq1,zq2,rijc,zqtemp,namelist,namea,nameb,success,xsum,
EM,EP,EMnew,EPnew,rijnamesnow,nvariables,nmono,npoly,xyzorig,
xyztemp,Perrors,DetailedDiagnosis,mnumber,pnumber,mlistpur,plistpur
},
mnumber=mnumbernow;
pnumber=pnumbernow;
(* assumes Assignx, EvalMono and EvalPoly have already been defined *)
(* get lists of permutable atoms *)
DetailedDiagnosis=False;
success=True;
(* next command also defines Assignxfromxyz[] *)
{rijnamesnow,nvariables,nmono,npoly,qnumber}=GetDefinitionstoMathematicaFromFortranOutputPure[];
rijnames[[1]]=rijnamesnow;
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)
permtable=MakePermTables[];
If[permtable=={},Print["There are no permutable atoms"];Goto[permdone];];
Print["Testing permutations of these atoms: ",permtable];
(*GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];*)
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
mlistpur={"m0"};
Do[(
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];
GetAssignEVMonoPolyPure[ifraga,natomsa,rijnamesa];

(* evaluate EM and EP *)
(* assign x from xyz *)
xyz=Table[RandomReal[{2,4}],{i,1,natomsparent},{j,1,3}];  (* global *)
xyzorig=xyz;
Assignxfromxyz[];
EM=IntegerPart[10^10*EvalMonoNew[PPCharStringmono,mlistpur]];
EP=IntegerPart[10^10*EvalPolyNew[PPCharStringpoly,plistpur]];
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
			EMnew=IntegerPart[10^10*EvalMonoNew[PPCharStringmono,mlistpur]];
			EPnew=IntegerPart[10^10*EvalPolyNew[PPCharStringpoly,plistpur]];
			Perrors={};
			Do[(
				If[N[Abs[EP[[k]]- EPnew[[k]]]/Abs[EP[[k]]]]>10^-10,
					Perrors=Append[Perrors,k-1];
					(*If[DetailedDiagnosis,*)
						Print["{{atom1,atom2},k-1,EP[[k]],EPnew[[k]]} =
						 ",{rijcombos[permtable[[i]]][[j]],k-1,EP[[k]],EPnew[[k]]}];
				(*	];*)
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
					Print["failures = ",failures];
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








RenumberFortranExistingmInMonoAndPolyPure::usage="RenumberFortranExistingmInMonoAndPoly[CharListmono,CharListpoly]
Takes all the lines beginning m(i)= in StringList
renumbers it and all the m(i) throughout in consecutive
numerical order
";
RenumberFortranExistingmInMonoAndPolyPure[CharListmono_,CharListpoly_]:=Module[
{Axold,Axnew,Bxold,Temppoly,diagnose},

(* NB:  The m(0) = 1. and p(0) = m(0) terms SHOULD NOT BE THERE ! *)
diagnose=False;

Axold=CharListmono;
Temppoly=CharListpoly;
(* Convert StringList from Text to Table format *)
Export[DataDir<>"DeleteMe.txt",Axold,"Text"];
Bxold=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[diagnose,
	PLHAxold123=Axold;
	PLHBxold123=Bxold;
	PLHCLP123=Temppoly;
];
(* replace m(i) if needed throughout monomials so as to have numerical order *)
(* Bxold (table format) is used for the criterion, but Axold is what is changed *)
(* the i+1 in Bxold is due to the zero-order term there *)
(* In addition, we want the monomial numbering in the polynomials to be the same as in the
monomials, so every time we change something in the monomials, we do a change of those
monomial numbers in the polynomials *)
Axnew=Axold;
Do[(
idyn=Length[Bxold]-i;
If["m("<>ToString[i]<>")"!= Bxold[[i,1]],
Axnew=StringReplace[Axold,Bxold[[i,1]]->  
		"m("<>ToString[i]<>")"];
Temppoly=StringReplace[Temppoly,Bxold[[i,1]]->  
		"m("<>ToString[i]<>")"];
Axold=Axnew;
];
),{i,1,Length[Bxold]}];
If[diagnose,PLHAxold2=Axold;PLHTemppoly2=Temppoly;];

{Axold,Temppoly}
];








GetFMonoDerivativesPure[]:=Module[
{natoms1,rijnames1,ifrag1,nvariables1,monostart1,monoend1,polystart1,
polyend1,CharStringmono1,CharStringpoly1,CharStringmono,Bx,
first,second,C3,Skipj,Fmonoderivatives,
xsum,ifraga,natomsa,rijnamesa,nvariablesa,monostarta,monoenda,
polystarta,polyenda,rijnamesnow,nvariables,nmono,npoly,FMDlines,
nstar,xterms,diagnose,xnum1,qnumber
},
diagnose=False;
Print["Starting GetFMonoDerivativesPure"];
(* get character strings CharStringmono and Charstringpoly from Fortran output file *)
{rijnamesnow,nvariables,nmono,npoly,qnumber}=
	GetDefinitionstoMathematicaFromFortranOutputPure[];
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
	Getmonopolycharstrings[ifraga,monostarta,monoenda,
		polystarta,polyenda];
(*	CharStringmono1=StringDelete[CharStringmono1,"        m(0) = 1.\n"];*)
(*plhcsm=CharStringmono1;	*)
	

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
CharStringmono=StringReplace[CharStringmono,"dm(0) = 1.d0"->"dm(0) = 0.d0"];
CharStringmono=StringReplace[CharStringmono,"    dm(1) = x(66)"->"dm(1) = x(66)"];
If[diagnose,PLHCharStringmono1x=CharStringmono;];


Export[DataDir<>"DeleteMe.txt",CharStringmono,"Text"];
Bx=Import[DataDir<>"DeleteMe.txt","Table"];
DeleteFile[DataDir<>"DeleteMe.txt"];
If[diagnose,PLHBx5=Bx;];


(* fix the x values. NB this expects the m(0) term to be there *)
Print["fixing the x values in dm, nvariables = ",nvariables];
Do[(
	If[StringTake[Bx[[i-1,3]],1]=="x",
	xnum1=ToExpression[StringDrop[StringDrop[Bx[[i-1,3]],2],-1]];
	(*Print["i-1 = ",i-1,"    xnum1 = ",xnum1];
	Print["xtransform[[xnum1]] =",xtransform[[xnum1]]];
	Print["Bx[[i-1]] = ",Bx[[i-1,3]]];
	Print[""];*)
		If[xtransform[[xnum1]]==1,
			(*Print["i-1 =",i-1,"  Bx[[i-1,3]] = ",Bx[[i-1,3]]];*)
			CharStringmono=StringReplace[CharStringmono,Bx[[i-1,3]]->
			 "-"<>Bx[[i-1,1]]<>"/a*drdx(flag,"<>
			 StringDrop[StringDrop[Bx[[i-1,3]],2],-1]];
			 Bx[[i-1,3]]= "-"<>Bx[[i-1,1]]<>"/a*drdx(flag,"<>
			 StringDrop[StringDrop[Bx[[i-1,3]],2],-1];
			 ,
			 CharStringmono=StringReplace[CharStringmono,Bx[[i-1,3]]->
			 "-"<>Bx[[i-1,1]]<>"**2*drdx(flag,"<>
			 StringDrop[StringDrop[Bx[[i-1,3]],2],-1]];
			 Bx[[i-1,3]]="-"<>Bx[[i-1,1]]<>"**2*drdx(flag,"<>
			 StringDrop[StringDrop[Bx[[i-1,3]],2],-1];
		];
	];
),{i,3,nvariables+2}];
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








getridofrhszeros::usage = "
tabletext input must be in table form of text with text entries and all operators (incuding eg *,=,+,-) separated by commas, note the * in particular.  If there are (0)'s on the rhs, the aprporiate math will be done and they will be eliminated. The only that (0)'s will be left is when the whole rhs evaluates to zero.
";
getridofrhszeros[tabletext_]:=Module[
{tabletextout,pairdrop,lcsp,poslist,test,lpos,
j1,j2,skipcsi},
tabletextout=tabletext;
lcsp=Length[tabletextout];
Do[(
pairdrop={};
test=tabletextout[[i]];
poslist=Sort[Flatten[Position[test,"(0)"]]];
(*If[MemberQ[poslist,3],poslist=Drop[poslist,1];];*)
lpos=Length[poslist];
If[poslist=={},Goto[skipcsi];];
pairdrop={};
Do[( (* over j *)
j1=poslist[[j]];
j2=poslist[[j]];
Do[( (* over k *)
If[k==Length[test],
	j2=k;
	Break[];
	,
	If[Or[test[[k]]=="+",test[[k]]=="-"],j2=k-1;Break[];];
];
),{k,j2,Length[test]}];
Do[( (* over k *)
If[test[[k]]=="=",
j1=k+1;
Break[];
,
If[Or[test[[k]]=="+",test[[k]]=="-"],
j1=k;Break[];];
];
),{k,j1,2,-1}];
pairdrop=Append[pairdrop,{j1,j2}];
),{j,lpos,1,-1}];
pairdrop=Sort[DeleteDuplicates[pairdrop]];
Do[(
test=Drop[test,pairdrop[[ipair]]];
),{ipair,Length[pairdrop],1,-1}];
If[test[[Length[test]]]=="=",test=Append[test,"(0)"]];
tabletextout[[i]]=test;
Label[skipcsi];
),{i,1,lcsp}];

tabletextout
];






CompactionPure[mnumber_,pnumber_,qnumber_]:=Module[
{diagnose,txtin5,txtin5tab,qkeep,print,mkeep,plistpur,
mdroplist,savemip1,newEP,qdroplist,csmonopure,cspolypure,
csmonopuretab,cspolypuretab,csmonopuretabsave,cspolypuretabsave,
tabletext,rhs,csmtxt,csptxt,csmonopurecpt,cspolypurecpt,test1,test2,
test3,qnumbernow,mnumbernow,pnumbernow,skipm,skipq,skipm2,skipq2,
csmtemp,csptemp,qkeepnew,ineededforj,usedefinitionsformkeep,
mtxtin5tab,mtxtin5,lhs,setzero
(*,natomsfrag,combos,rijnamesnow,nvariables,rijnames,rijcombos*)
},
(*NB: PPCharStringpoly and PPCharStringmonocome in as global variables *)
print=True;
diagnose=False;
usedefinitionsformkeep=True;

Print["Starting Compaction Routine"];

Print["   {mnumber,pnumber,qnumber,nvariables} = ",
	{mnumber,pnumber,qnumber,nvariables}];
If[diagnose,PLHCSM100=PPCharStringmono;PLHCSP100=PPCharStringpoly;];

(* get qkeep *)
(*turn text into table form *)
txtin5=PPCharStringpoly;
txtin5tab=textconvert[txtin5,"text","tablestar",DataDir];
If[diagnose,PLHtxtin5tab45=txtin5tab;PLHtxtin5=txtin5;];

(* run double loop to determine which q's are needed for E evaluation *)
(* get qkeep *)
If[print,Print["   determine which q's are needed for E evaluation"];];
If[FileExistsQ[qkeepfname],
qkeep=Import[qkeepfname,"List"];
If[print,Print["Getting qkeep from file"];];
qnumbernow=Length[qkeep];
If[print,Print["qnmbernow = ",qnumbernow];];
qdroplist={};
Do[(
If[!MemberQ[qkeep,i],
qdroplist=Append[qdroplist,i];
];
),{i,1,qnumber}];
,
{qkeep,qdroplist}=getqkeepviadefinitions[txtin5tab,qnumber];
];
If[diagnose,PLHqkeep33=qkeep; PLHqdroplist33=qdroplist;];


mtxtin5=PPCharStringmono;
mtxtin5tab=textconvert[mtxtin5,"text","tablestar",DataDir];
If[diagnose,PLHmtxtin5tab45=mtxtin5tab;PLHmtxtin5=mtxtin5;];

(* get mkeep *)
(*PPCharStringpoly=textconvert[cspolypuretab,"tablestar","text",DataDir];
If[diagnose, PLHPPCharStringpoly142=PPCharStringpoly;];*)
If[print,Print["   determine which m's are needed for E evaluation"];];
If[FileExistsQ[mkeepfname],
mkeep=Import[mkeepfname,"List"];
If[print,Print["Getting mkeep from file"];];
mnumbernow=Length[mkeep];
If[print,Print["mnmbernow = ",mnumbernow];];
mdroplist={};
Do[(
If[!MemberQ[mkeep,i],
mdroplist=Append[mdroplist,i];
];
),{i,nvariables+1,mnumber}];
,
If[usedefinitionsformkeep,
{mkeep,mdroplist}=getmkeepviadefinitions[qkeep,txtin5tab,mtxtin5tab,mnumber];
,
{mkeep,mdroplist}=getmkeepviamcalculation[mnumber,pnumber];
];
];
If[diagnose,PLHmkeep33=mkeep; PLHmdroplist33=mdroplist;];

(* put in format where each operation is preceded and followed by a space *)
csmonopure=StringReplace[StringReplace[StringReplace[PPCharStringmono,
"**"->"^"],"*"->" * "],"^"->"**"];
cspolypure=StringReplace[StringReplace[StringReplace[PPCharStringpoly,
"**"->"^"],"*"->" * "],"^"->"**"];
If[diagnose,PLHcsmonopure3=csmonopure; PLHcspolypure3=cspolypure;];

If[Length[mdroplist]==0,
	Print["Length[mdroplist]=0; skipping deletion of m's"];
	Export[DataDir<>"DeleteMe.txt",csmonopure,"Text"];
	csmonopuretab=Import[DataDir<>"DeleteMe.txt","Table"]; (* in fortran *)
	DeleteFile[DataDir<>"DeleteMe.txt"];
	Goto[skipm];
];

(* replace all m(i) by d(i) if m(i) is on mdroplist *)
If[print,Print["   replace all m(i) by d(i) if m(i) is on mdroplist"];];
Do[(
idyn=i;
	csmonopure=StringReplace[csmonopure,"m("<>ToString[mdroplist[[i]]]<>")"-> 
		"d("<>ToString[mdroplist[[i]]]<>")"];
	cspolypure=StringReplace[cspolypure,"m("<>ToString[mdroplist[[i]]]<>")"-> 
		"d("<>ToString[mdroplist[[i]]]<>")"];
),{i,Length[mdroplist],1,-1}];
If[diagnose,PLHcsmonopure4=csmonopure;PLHcspolypure4=cspolypure;];

(* make table version of csmonopure  *)
Export[DataDir<>"DeleteMe.txt",csmonopure,"Text"];
csmonopuretab=Import[DataDir<>"DeleteMe.txt","Table"]; (* in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
If[diagnose, PLHcsmonopuretab122=csmonopuretab;];

(* Delete lines starting with d(i) in csmonopuretab *)
If[print,Print["   delete lines starting with d(i) in csmonopuretab"];];
Do[(
If[StringTake[csmonopuretab[[i,1]],2]=="d(",
csmonopuretab=Drop[csmonopuretab,{i}];
];
),{i,Length[csmonopuretab],1,-1}]; 
If[diagnose, PLHcsmonopuretab123=csmonopuretab;];

(* replace other d's by zero in csmonopuretab *)
If[print,Print["   replace other d's by zero in csmonopuretab"];];
Do[(
idyn=Length[csmonopuretab]-i;
Do[(
If[StringQ[csmonopuretab[[i,j]]],
If [StringLength[csmonopuretab[[i,j]]]>2,
If[StringTake[csmonopuretab[[i,j]],2]=="d(",
csmonopuretab[[i,j]]="(0)";
];
];
];
),{j,2,Length[csmonopuretab[[i]]]}];
),{i,1,Length[csmonopuretab]}];
If[diagnose, PLHcsmonopuretab125=csmonopuretab;];
Label[skipm];

If[Length[qdroplist]==0,
	cspolypure=StringReplace[StringReplace[StringReplace[PPCharStringpoly,
	"**"->"^"],"*"->" * "],"^"->"**"];
	If[diagnose,PLHcspolypure3=cspolypure;];
	(* make table version of cspolypure *)
	Export[DataDir<>"DeleteMe.txt",cspolypure,"Text"];
	cspolypuretab=Import[DataDir<>"DeleteMe.txt","Table"]; (* in fortran *)
	DeleteFile[DataDir<>"DeleteMe.txt"];
	If[diagnose, PLHcspolypuretab122a=cspolypuretab;];
	Goto[skipq];
];

(* replace all q(i) by d(i) if q(i) is on qdroplist *)
If[print,Print["   replace all q(i) by d(i) if q(i) is on qdroplist"];];
Do[(
idyn=i;
cspolypure=StringReplace[cspolypure,"q("<>ToString[qdroplist[[i]]]<>")"-> 
		"d("<>ToString[qdroplist[[i]]]<>")"];
),{i,Length[qdroplist],1,-1}];
If[diagnose,PLHPPCharStringpoly578=cspolypure;];


(* make table version of cspolypure *)
Export[DataDir<>"DeleteMe.txt",cspolypure,"Text"];
cspolypuretab=Import[DataDir<>"DeleteMe.txt","Table"]; (* in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
If[diagnose, PLHcspolypuretab122=cspolypuretab;];

(* Delete lines starting with d(i) in cspolypuretab *)
If[print,Print["   delete lines starting with d(i) in cspolypuretab"];];
Do[(
If[StringTake[cspolypuretab[[i,1]],2]=="d(",
cspolypuretab=Drop[cspolypuretab,{i}];
];
),{i,Length[cspolypuretab],1,-1}];
If[diagnose, PLHcspolypuretab123=cspolypuretab;];

If[print,Print["   replace other d's by (0) in  cspolypuretab"];];
Do[(
idyn=Length[cspolypuretab]-i;
Do[(
If[StringQ[cspolypuretab[[i,j]]],
If [StringLength[cspolypuretab[[i,j]]]>2,
If[StringTake[cspolypuretab[[i,j]],2]=="d(",
cspolypuretab[[i,j]]="(0)";
];
];
];
),{j,2,Length[cspolypuretab[[i]]]}];
),{i,1,Length[cspolypuretab]}];
If[diagnose, PLHcspolypuretab125=cspolypuretab;];
Label[skipq];



If[print,Print[" perform checks:"];];
(* get rid of  "(0)" on rhs (except for position 3, if there) in cspolypuretab *)
If[print,
	Print["   get rid of  (0) on rhs (except for position 3, if there) in cspolypuretab"];];
cspolypuretabsave=cspolypuretab;
csmonopuretabsave=csmonopuretab;
tabletext=cspolypuretabsave;
cspolypuretab=getridofrhszeros[tabletext];
(* delete lines with rhs = "(0)" alone in cspolypuretab *)
If[print,Print["   delete lines with rhs = (0) alone in cspolypuretab"];];
setzero={};
Do[(
rhs=Drop[cspolypuretab[[i]],2];
If[rhs[[1]]=="(0)",
lhs=Take[cspolypuretab[[i]],1];
setzero=Append[setzero,lhs];
If[diagnose,Print[cspolypuretab[[i]]];];
cspolypuretab=Drop[cspolypuretab,{i}];
];
),{i,Length[cspolypuretab],1,-1}];
(* get rid of  "(0)" on rhs (except for position 3, if there) in csmonopuretab*)
If[print,
	Print["   get rid of  (0) on rhs (except for position 3, if there) in csmonopuretab"];];
tabletext=csmonopuretabsave;
csmonopuretab=getridofrhszeros[tabletext];
Do[(
rhs=Drop[csmonopuretab[[i]],2];
If[rhs[[1]]=="(0)",
csmonopuretab=Drop[csmonopuretab,{i}];
];
),{i,Length[csmonopuretab],1,-1}];
If[diagnose, PLHcsmonopuretab126=csmonopuretab;PLHcspolypuretab126=cspolypuretab;];
(* make text versions of csmonopuretab and cspolypuretab*)
Export[DataDir<>"DeleteMe.txt",csmonopuretab,"Table"];
csmtxt=Import[DataDir<>"DeleteMe.txt","Text"]; (* in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
Export[DataDir<>"DeleteMe.txt",cspolypuretab,"Table"];
csptxt=Import[DataDir<>"DeleteMe.txt","Text"]; (* in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
(*
(* get rid of any other things that should be zero on rhs in csptxt *)
If[Length[setzero]\[NotEqual]0,
	If[diagnose,Print["getting rid of other things that should be zero on rhs in csptxt"];];
	Print[setzero];
	Do[(
	csptxt=StringDelete[csptxt,"+\t"<>setzero[[i]]];
	csptxt=StringDelete[csptxt,"-\t"<>setzero[[i]]];
	),{i,1,Length[setzero]}];
];
*)
If[diagnose, PLHcsmtxt127=csmtxt;PLHcsptxt127=csptxt;];
(* perform checks *)
If[StringCount[csmtxt,"(0)"]>StringCount[csmtxt,"m(0)"], 
Print["aborting; there was a (0) in csmtxt for the monomials"];Abort[];
,
Print["     Good, there are no (0) in the monomial list other than m(0)"];
]; 
If[StringCount[csptxt,"(0)"]>StringCount[csptxt,"p(0)"], 
Print["aborting; there was a (0) in csptxt for the polynomials"];Abort[];
,
Print["     Good, there are no (0) in the polynomial list other than p(0)"];
];
If[print,Print[" end of checks:"];];


(* make text versions of csmonopuretab and cspolypuretab*) 
Export[DataDir<>"DeleteMe.txt",csmonopuretab,"Table"];
csmonopurecpt=Import[DataDir<>"DeleteMe.txt","Text"]; (* in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
csmonopurecpt="    "<>StringReplace[StringReplace[StringReplace[StringReplace[StringDelete[csmonopurecpt,"\t"],
"="->" = "],"\n"->"\n    "],"+"->" + "],"-"->" - "];
Export[DataDir<>"DeleteMe.txt",cspolypuretab,"Table"];
cspolypurecpt=Import[DataDir<>"DeleteMe.txt","Text"]; (* in fortran *)
DeleteFile[DataDir<>"DeleteMe.txt"];
cspolypurecpt="    "<>StringReplace[StringReplace[StringReplace[StringReplace[StringDelete[cspolypurecpt,"\t"],
"="->" = "],"\n"->"\n    "],"+"->" + "],"-"->" - "];
If[diagnose, PLHcsmonopurecpt128=csmonopurecpt;PLHcspolypurecpt128=cspolypurecpt;];

(* drop m(0) = 1.  and p(0) = m(0) terms, if they are there *)
(* NB: RenumberFortranExistingmInMonoAndPolyPure expects these terms not 
to be there !!! *)
csmtemp=textconvert[csmonopurecpt,"text","table",DataDir];
csptemp=textconvert[cspolypurecpt,"text","table",DataDir];
If[csmtemp[[1,1]]=="m(0)",csmtemp=Drop[csmtemp,1];];
If[csptemp[[1,1]]=="p(0)",csptemp=Drop[csptemp,1];];
csmonopurecpt=textconvert[csmtemp,"table","textstandard",DataDir];
cspolypurecpt=textconvert[csptemp,"table","textstandard",DataDir];
csmonopurecpt=StringReplace[StringReplace[csmonopurecpt,
"^"->"**"]," ^ "->"**"];  (* added this 10 Feb 2023 to get rid of ^ *)
If[diagnose, PLHcsmonopurecpt128a=csmonopurecpt;PLHcspolypurecpt128a=cspolypurecpt;];

(* Next we need to renumber the m's in both and the q's in cspoly *)
If[Length[mdroplist]==0,
	{test1,test2}={csmonopurecpt,cspolypurecpt};
	Goto[skipm2];
];
If[print,Print["   renumber the m's in both csmono and cspoly and the q's in cspoly"];];
{test1,test2}=RenumberFortranExistingmInMonoAndPolyPure[csmonopurecpt,
	cspolypurecpt];
	
Label[skipm2];
If[Length[qdroplist]==0,
	test3=test2;
	Goto[skipq2];
];
{test3,qnumbernow}=RenumberFortranExistingq[test2];
Label[skipq2];
(* put initial terms back *)
test3="    p(0) = 0.d0\n"<>test3;
test1="    m(0) = 1.d0\n"<>test1;


mnumbernow=Length[mkeep];
pnumbernow=pnumber;
Print["{mnumbernow,qnumbernow,pnumbernow} = ",{mnumbernow,qnumbernow,pnumbernow}];

csmonopurecpt=test1;
cspolypurecpt=test3;
If[diagnose, PLHcsmonopurecpt129=csmonopurecpt;PLHcspolypurecpt129=cspolypurecpt;];

If[print,Print["Compaction Completed"];];
{csmonopurecpt,cspolypurecpt,{mnumbernow,qnumbernow,pnumbernow}}
];












Ampersand::useage="Ampersand[qlines]
Format is everything!  The input qlines must be the
Fortran code in line text format, achieved, for example, 
by taking the Fortran text code (every statement on a single line)
and doing the following 
    Export[DataDir<>fname,q,Textq];
    qlines=Import[DataDir<>fname,{Textq,Linesq}];
    DeleteFile[DataDir<>fname];,
where fname is DeleteMe.txt in quotes, Textq is Text in quotes,
and Lineq is Lines in quotes.
qlines must be in a format where the following characters are
surrounded by one space: =, +, -. Any occurance of caret should 
have been replaced by **.  Tab characters (\t) should be deleted.
";
Ampersand[qlines_]:=Module[
{AnotherRoundM,dpcoeflinesch,outM,CSMappend,CSMremaining,
qlinest,newfortran,jsave,break},
qlinest=qlines;
break=70;
newfortran="";
Do[(
	idyn=Length[qlinest]-i; (* global *)
	Label[AnotherRoundM];
	If[Length[Characters[qlinest[[i]]]]<= break,
		newfortran=newfortran<>qlinest[[i]]<>"\n";
		,
		dpcoeflinesch=Characters[qlinest[[i]]];
		Do[(
			If[Or[dpcoeflinesch[[break+1-j]]=="+",dpcoeflinesch[[break+1-j]]=="-"],
				jsave=j;
				Goto[outM];
			];	
		),{j,1,break}];
		Print["1. Fortran output mono line longer than break char w/o + or -"];
		Print["i = ",i];
		PLHnewfortran=newfortran;
		Abort[];
		Label[outM];
		CSMappend=StringJoin[Take[dpcoeflinesch,break+1-jsave+1]]<>" &\n";
		CSMremaining="         "<>StringJoin[Drop[dpcoeflinesch,break+1-jsave+1]];
		newfortran=newfortran<>CSMappend;
		qlinest[[i]]=CSMremaining;
		Goto[AnotherRoundM];
	];
),{i,1,Length[qlinest]}];
		newfortran
		];






AmpersandPlusOnly::useage="AmpersandPlusOnly[qlines]
Format is everything!  The input qlines must be the
Fortran code in line text format, achieved, for example, 
by taking the Fortran text code (every statement on a single line)
and doing the following 
    Export[DataDir<>fname,q,Textq];
    qlines=Import[DataDir<>fname,{Textq,Linesq}];
    DeleteFile[DataDir<>fname];,
where fname is DeleteMe.txt in quotes, Textq is Text in quotes,
and Lineq is Lines in quotes.
qlines must be in a format where the following characters are
surrounded by one space: =, +, -. Any occurance of caret should 
have been replaced by **.  

Input is in text lines format
Output is in text format
";
AmpersandPlusOnly[qlines_]:=Module[
{AnotherRoundM,dpcoeflinesch,outM,CSMappend,CSMremaining,
qlinest,newfortran,jsave,break},
qlinest=qlines;
break=110;
linesmax=500;
newfortran="";
Do[(
	idyn=Length[qlinest]-i; (* global *)
    eqpos=StringPosition[qlinest[[i]],"="][[1,1]];
    lhswequal=StringTake[qlinest[[i]],eqpos];
    lhs=StringDelete[StringDrop[lhswequal,-2]," "];
    linesnow=0;
	Label[AnotherRoundM];
	If[Length[Characters[qlinest[[i]]]]<= break,
		newfortran=newfortran<>"    "<>StringDelete[qlinest[[i]]<>"\n"," "];
		,
		dpcoeflinesch=Characters[qlinest[[i]]];
		Do[(
			If[dpcoeflinesch[[break+1-j]]=="+",
				jsave=j;
				Goto[outM];
			];	
		),{j,1,break}];
		Print["1. Fortran output mono line longer than break char w/o + "];
		Abort[];
		Label[outM];
            If[linesnow==0,
	  CSMappend="    "<>
               StringDelete[StringJoin[Take[dpcoeflinesch,break+1-jsave+1]]<>" &\n"," "];
            ,
		CSMappend="     "<>
               StringDelete[StringJoin[Take[dpcoeflinesch,break+1-jsave+1]]<>" &\n"," "];
          ];
		CSMremaining=StringJoin[Drop[dpcoeflinesch,break+1-jsave+1]];
		newfortran=newfortran<>CSMappend;
		qlinest[[i]]=CSMremaining;
       linesnow=linesnow+1;
	  If[linesnow>= linesmax,
		newfortran=StringDrop[newfortran,-3];
                  qlinest[[i]]="\n    "<>lhswequal<>lhs<>"+"<>qlinest[[i]];
		linesnow=0;
        ];
		Goto[AnotherRoundM];
	];
),{i,1,Length[qlinest]}];
		newfortran
		];





GenerateFortranCommentWithXAssignmentsPure[]:=Module[
{xx,xxx,xxxdel,xxxold,xxxnew,xxxx,xxxxx,transform,mforx,xform,firstnum,
secnum,nvariables,add,diagnose
},
diagnose=False;
xx={};
nvariables=natomsparent (natomsparent-1)/2;
Do[(xx=Join[xx,rijnames[[ifrag]]]),{ifrag,1,nfragments}];
xxx=Table[{"!     x("<>ToString[i]<>")\t=\t",xx[[i]]},{i,1,Length[xx]}];
If[diagnose,PLHxxx=xxx;Print["1"];];



xxxdel={};
Do[(
Do[(
If[xxx[[j,2]]== xxx[[i,2]],xxxdel=Append[xxxdel,j];];
),{j,i+1,Length[xxx]}];
),{i,1,Length[xxx]}];
xxxdel=Sort[DeleteDuplicates[xxxdel]];
If[diagnose,PLHxxxdel=xxxdel;Print["2"];];

xxxold=xxx;
Do[(
xxxnew =Drop[xxxold,{xxxdel[[Length[xxxdel]+1-i]]}];
xxxold=xxxnew;
),{i,1,Length[xxxdel]}];
xxxx=xxxold;
If[diagnose,PLHxxxx=xxxx;Print["3"];];
{mforx,xform}=makemforxformPure[nvariables];


xxxxx="";
Do[(
firstnum=ToExpression[StringTake[xxxx[[i,2]],2]];
secnum=ToExpression[StringTake[xxxx[[i,2]],-2]];
If[xtransform[[i]]==1, transform="Morse";,transform="1/rij";];
If[mforx[[i]]==-1,
	add=" = not used\t"<>atomnames[[firstnum]]<>"-"<>atomnames[[secnum]]<>
		"\t"<>transform;
	,
	add=" = m("<>ToString[mforx[[i]]]<>")   \t"<>atomnames[[firstnum]]<>"-"<>
		atomnames[[secnum]]<>"\t"<>transform;
];
xxxxx=xxxxx<>StringJoin[xxxx[[i,1]],xxxx[[i,2]]]<>add<>"\n";
),{i,1,Length[xxxx]}];
If[diagnose,PLHxxxxx=xxxxx;Print["4"];];
(*Print[xxxxx];*)

xxxxx
];







makemforxformPure[nvar_]:=Module[
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





AppendFastForwardDerivatives[inputfname_,outputfname_]:=Module[
{newfortran,time,z,CharStringF,dpcoef,CharStringdmm},

fortranname=inputfname;
Print[DateString[]];
Print["\nAdding Fast Forward Derivatives"];
	newfortran=Import[inputfname,"Text"];
	newfortran=StringDelete[newfortran," end module bemsa"];
	time=Timing[
	{z,CharStringF,dpcoef,CharStringdmm}=ConvertChenDerivatives[inputfname];
	][[1]];
	Print["The derivatives program took ",time," sec. = ",time/60," min."];
	newfortran=newfortran<>z;
	newfortran=newfortran<>"\n end module bemsa";
	Export[outputfname,newfortran,"Text"];
(* no output *)
];






AppendReverseDerivatives[inputfname_,outputfname_]:=Module[
{newfortran,time,tout,mnumbernow,pnumbernow,qnumbernow},

CheckAppendReverseDerivativesInputs[];

fortranname=inputfname;
Print[DateString[]];
Print["\nAdding Reverse Derivatives"];
{mnumbernow,pnumbernow,qnumbernow}=Getmpq[inputfname];
Print["{mnumbernow,pnumbernow,qnumbernow} = ",
	{mnumbernow,pnumbernow,qnumbernow}];
	newfortran=Import[inputfname,"Text"];
	newfortran=StringDelete[newfortran," end module bemsa"];
	time=Timing[
	tout=MakeBackwardsFortRoutine[mnumbernow,pnumbernow,qnumbernow];
	][[1]];
	Print["The derivatives program took ",time," sec. = ",time/60," min."];
		newfortran=newfortran<>tout;
	newfortran=newfortran<>"\n end module bemsa";
	Export[outputfname,newfortran,"Text"];
(* no output *)
];







Getmpq::usage="
gets the max of m's, p's, and (possibly) q's from the file
inputfname
Assumes DataDir is a global variable
";
Getmpq[inputfname_]:=Module[
{doneq,doneq1,donem,donep,mnumber,pnumber,qnumber},
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

{mnumber,pnumber,qnumber}
];




getmkeepviadefinitions[qkeep_,txtin5tab_,mtxtin5tab_,mnumber_]:=Module[
{print,diagnose,tim, mkeep,mkeepnew,ineededforj,rhs,endit,oldlength,mdroplilst,
newlength,know,txtin5tabm1,mtxtin5tabm1},
print=True;
diagnose=False;
idyn=0;jdyn=0;

txtin5tabm1=txtin5tab;
mtxtin5tabm1=mtxtin5tab;
If[diagnose,PLHmtxtin5tabm1m=mtxtin5tabm1];
If[diagnose,PLHtxtin5tabm1m=txtin5tabm1];
If[StringContainsQ[txtin5tab[[1,1]],"p(0)"],txtin5tabm1=Drop[txtin5tab,1];];
If[StringContainsQ[mtxtin5tab[[1,1]],"m(0)"],mtxtin5tabm1=Drop[mtxtin5tab,1];];

tim=Timing[
If[print,Print["Getting mkeep via definitions method"];];
mkeep=Table[i,{i,1,natomsparent (natomsparent-1)/2}]; (* we want all the x's *)
(* add to mkeep all the m's that are needed in the definitions of the p's *)
If[print,Print["     add to mkeep all m's that are needed in defs for p's"];];
Do[(
idyn=mnumber-i; (* global *)
Do[(
(*jdyn=Length[txtin5tabm1]-j;*)
If[StringContainsQ[StringTake[txtin5tabm1[[j,1]],2],"p("],
If[MemberQ[Drop[txtin5tabm1[[j]],2],
	"m("<>ToString[i]<>")"],
mkeep=Append[mkeep,i];
];
];
),{j,1,Length[txtin5tabm1]}];
),{i,1,mnumber}];
If[diagnose,PLHmkeepfirst=mkeep];

(* add to mkeep all the m's that are needed in the definitions of those 
q's that are needed for the p's *)
If[print,Print["     add to mkeep all m's that are needed in defs \n     for those q's needed for p's"];];
(* note that we can no longer assume that all the q's are together at the beginning of textin5tabm1 *)
Do[(
idyn=mnumber-i; (* global *)
Do[(
(*jdyn=j;*)
If[StringContainsQ[StringTake[txtin5tabm1[[j,1]],2],"q("],
If[MemberQ[Drop[txtin5tabm1[[j]],2],
	"m("<>ToString[i]<>")"],
mkeep=Append[mkeep,i];
];
];
),{j,1,Length[txtin5tabm1]}]; (* looks at all q's that are needed for p's *)
),{i,1,mnumber}];
If[diagnose,PLHmkeepsecond=mkeep];

(* make ineededforj, a table that lists in position 2 all the q(j)s that 
depend on the m(i) in position 1
*)
If[print,Print["     calculating ineededforj mono table"];];
ineededforj=Table[{i,{}},{i,1,mnumber}];
Do[(
idyn=mnumber-i;
Do[(
jdyn=mnumber-j;
rhs=Drop[mtxtin5tabm1[[j]],2];
If[MemberQ[rhs,"m("<>ToString[i]<>")"],
ineededforj[[i,2]]=Append[ineededforj[[i,2]],j];
];
),{j,i+1,mnumber}];
),{i,1,mnumber}];
If[diagnose,PLHineededforjINm=ineededforj];

(* augment mkeep with any m's that are needed for those on the
original list *)
If[print,Print["     augmenting mkeep with any m's that are needed \n     for those on the orig list"];];
mkeepnew=mkeep;
oldlength=Length[mkeepnew];
Do[( (* over k times *)
Do[(
idyn=i;
Do[(
jdyn=i-1-j;
If[MemberQ[ineededforj[[j,2]],i],
mkeepnew=Append[mkeepnew,ineededforj[[j,1]]];
(*Print["appended ",ineededforj[[j,1]]];*)
];
),{j,1,i-1}];
),{i,Sort[mkeep]}];
mkeep=Sort[DeleteDuplicates[mkeepnew]];
newlength=Length[mkeep];
If[newlength==oldlength,
	know=k;
	Print["evaluated ",know," loops of ineeededforj"];
	Goto[endit];;
	,
	oldlength=newlength;
	mkeepnew=mkeep;
	];
),{k,1,10}];
Print["Warning: loops over ineededforj did not converge; aborting"];
Abort[];
Label[endit];
]; (* end of Timing command *)

If[print,Print["     Exporting mkeep, length = ",Length[mkeep]];];
If[print,Print["     Time required for mkeep in min = ",tim[[1]]/60];];
If[ValueQ[mkeepfname],Export[mkeepfname,mkeep,"List"];];
If[diagnose,PLHmkeep1=mkeep;];
idyn=0;jdyn=0;
mdroplist={};
Do[(
If[!MemberQ[mkeep,i],
mdroplist=Append[mdroplist,i];
];
),{i,1,mnumber}];
If[diagnose,PLHmkeep60=mkeep;PLHmdroplist=mdroplist];
{mkeep,mdroplist}
];





getqkeepviadefinitions[txtin5tab_,qnumber_]:=Module[
{print,diagnose,tim, qkeep,qkeepnew,ineededforj,rhs,qkeepe,qdroplist,
txtin5tabm1},
print=True;
diagnose=False;
idyn=0;jdyn=0;

txtin5tabm1=txtin5tab;
If[diagnose,PLHtxtin5tabm1=txtin5tabm1];
If[StringContainsQ[txtin5tab[[1,1]],"p(0)"],txtin5tabm1=Drop[txtin5tab,1];];


tim=Timing[
If[print,Print["Getting qkeep via definitions method"];];
(* run double loop to determine which q's are needed for E evaluation *)
(* get qkeep *)
If[print,Print["   determine which q's are needed for E evaluation"];];
qkeepe={};
Do[(
idyn=qnumber-i; (* global *)
 Do[(
  If[StringContainsQ[StringTake[txtin5tabm1[[j,1]],2],"p("],
    If[MemberQ[Drop[txtin5tabm1[[j]],2],"q("<>ToString[i]<>")"],
      qkeepe=Append[qkeepe,i];
    ];
  ];
 ),{j,1,Length[txtin5tabm1]}];
),{i,1,qnumber}];
If[diagnose,PLHqkeepfirst=qkeepe;];
qkeep=DeleteDuplicates[qkeepe];

(* now make sure that all the q's that are needed for those on qkeep are also on qkeep *)
(* make ineededforj, a table that lists in position 2 all the q(j)s that 
depend on the q(i) in position 1
*)
If[print,Print["     calculating ineededforj table for q's"];];
ineededforj=Table[{i,{}},{i,1,qnumber}];
Do[(
idyn=qnumber-i;
Do[(
(*jdyn=qnumber-j;*)
rhs=Drop[txtin5tabm1[[j]],2];
If[MemberQ[rhs,"q("<>ToString[i]<>")"],
ineededforj[[i,2]]=Append[ineededforj[[i,2]],j];
];
),{j,i+1,qnumber}];
),{i,1,qnumber}];
If[diagnose,PLHqineededforj=ineededforj;];

(* augment qkeep with any q's that are needed for those on the
original list *)
If[print,Print["     augmenting qkeep with any q's that are needed \n     for those on the orig list"];];
qkeepnew=qkeep;
oldlength=Length[qkeepnew];
Do[( (* over k times *)
Do[(
idyn=i;
Do[(
(*jdyn=i-1-j;*)
If[MemberQ[ineededforj[[j,2]],i],
qkeepnew=Append[qkeepnew,ineededforj[[j,1]]];
(*Print["appended ",ineededforj[[j,1]]];*)
];
),{j,1,i-1}];
),{i,Sort[qkeep]}];
qkeep=Sort[DeleteDuplicates[qkeepnew]];
newlength=Length[qkeep];
If[newlength==oldlength,
know=k;
	Print["evaluated ",know," loops of ineeededforj"];
	Goto[endit];;
	,
	oldlength=newlength;
	qkeepnew=qkeep;
	];
),{k,1,10}];
Print["Warning: loops over ineededforj did not converge; aborting"];
Abort[]; 
Label[endit];
]; (* end of Timing command *)

If[print,Print["     Exporting qkeep, length = ",Length[qkeep]];];
If[print,Print["     Time required for qkeep in min = ",tim[[1]]/60];];
If[ValueQ[qkeepfname],Export[qkeepfname,qkeep,"List"];];
If[diagnose,PLHqkeep1=qkeep;];
(* make qdroplist *)
qdroplist={};
Do[(
If[!MemberQ[qkeep,i],
qdroplist=Append[qdroplist,i];
];
),{i,1,qnumber}];
idyn=0;jdyn=0;
{qkeep,qdroplist}
];






getmkeepviamcalculation[mnumber_,pnumber_]:=Module[
{print,diagnose,tim, mkeep,mdroplist},
print=True;
diagnose=False;
idyn=0;jdyn=0;

tim=Timing[
If[print,Print["Getting mkeep via m calculations method"];];
Print[DateString[]];
dist=Table[0,{i,1,nvariables}];
Do[(
	dist[[i]]=RandomReal[{1,4}];
),{i,1,nvariables}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];
Assignx[];
EM=EvalMono[];
EP=EvalPolyNew[PPCharStringpoly,plistpur];
If[diagnose,
	Export[DataDir<>"PPCharStringmono20",PPCharStringmono,"Text"];
	Export[DataDir<>"PPCharStringpoly20",PPCharStringpoly,"Text"];
	PLHPPCharStringpoly20=PPCharStringpoly;
	PLHLenEP20=Length[EP];
	PLHCount20=StringCount[PPCharStringpoly,"p"];
	PLHEM=EM;
	PLHEP=EP;
	Print["zero count = ",Count[EP,0]];
];
If[print,Print["       getting list of m's to drop"];];
mdroplist={};
Do[(
	idyn=Length[EM]-1-i;
	(*savemip1=ToExpression["m"<>ToString[i]];*)
	ToExpression["m"<>ToString[i]<>"= 10 m"<>ToString[i]<>";"];
	newEP=EvalPolyNew[PPCharStringpoly,plistpur];
	If[newEP==EP,
	mdroplist=Append[mdroplist,i];
	(*Print[i];*)
	];
	EM=EvalMono[];  (* this puts back EM where it was by evaluating it from the orig x's *)
	(*EPx=EvalPolyNew[PPCharStringpoly,plistpur];
	If[EPx!=EP,Print["Problem with mdroplist"];Abort[];];*)
),{i,1,Length[EM]-1}];
If[print,Print["       Length of mdroplist = ",Length[mdroplist]]];
If[diagnose,PLHmdroplist=mdroplist;];

Print[DateString[]];
mkeep={};
Do[(
If[!MemberQ[mdroplist,i],
mkeep=Append[mkeep,i];
];
),{i,1,mnumber}];
Export[mkeepfname,mkeep,"List"];
]; (* end of Timing command *)

If[print,Print["     Exporting mkeep, length = ",Length[mkeep]];];
If[print,Print["     Time required for mkeep in min = ",tim[[1]]/60];];
If[diagnose,PLHmkeep1=mkeep;];
idyn=0;jdyn=0;
{mkeep,mdroplist}
];







makeptabrhsplist[pnumber_,PPCharStringpoly_,deletelist_,DataDir_]:=Module[
{pp1,PPCSPtab,ptab,diagnose,rhsplist,num,posits},

diagnose=False;

(* make ptab, a tablestar list of the p definitions *)
PPCSPtab=textconvert[PPCharStringpoly,"text","tablestar",DataDir];
If[diagnose,Print["Length of PPCSPtab = ",Length[PPCSPtab]];];
pp1=Position[PPCSPtab[[All,1]],"p(1)"][[1,1]];
If[diagnose,Print["Length of pp1 = ",pp1];];
If[pp1==1,ptab=Take[PPCSPtab,pnumber];,
	ptab=Take[Drop[PPCSPtab,pp1-1],pnumber];];
If[diagnose,Print["Length of ptab = ",Length[ptab]];];
(* ptab now contains the definitions of all p's *)

(* get rhsplist, a list of polynomials with p's on rhs and for which the
p definition on the lhs will not already cause the whole line 
to be deleted*)
rhsplist={};
Do[(    (* over {i,1,Length[ptab]} *)
	idyn=Length[ptab]-i; (* global *)
	If[!MemberQ[deletelist,i], (* if i is ON the delete list, then the whole
definition will be deleted so that it doesn't matter what is on the rhs *)
		(* if rhs contains "p(", add to rhsplist *)
		If[StringContainsQ[StringJoin[Drop[ptab[[i]],2]],"p("],
			rhsplist=Append[rhsplist,i];
		];
	];
),{i,1,Length[ptab]}];

{ptab,rhsplist}
];




findrhszeros::usage="Findrhszeros[ptab,rhsplist,deletelist]

NB:  the name of this function is actually a misnomer.  It's not just zeros that we
look for; it's \!\(\*
StyleBox[\"any\",\nFontSlant->\"Italic\"]\) change that might leave a polynomial without perm sym wrt waters.

Given ptab, rhsplist, (for these see makeptabrhsplist)
the deletelist, and DataDir, findrhszeros, finds those remaining polynomials
(after deletion of those on delete list) whose rhs will be changed.
NB: DeleteDuplicatesVxx.wl needs to be loaded for this to work.
";
findrhszeros[ptab_,rhsplist_,deletelist_]:=Module[
{vettab,posits,num,(*rhstab,rhsval,*)diagnose,
addtovettab},

diagnose=False;
(*
(* make ptab, a tablestar list of the p definitions *)
PPCSPtab=textconvert[PPCharStringpoly,"text","tablestar",DataDir];
If[diagnose,Print["Length of PPCSPtab = ",Length[PPCSPtab]];];
pp1=Position[PPCSPtab[[All,1]],"p(1)"][[1,1]];
If[diagnose,Print["Length of pp1 = ",pp1];];
If[pp1==1,ptab=Take[PPCSPtab,pnumber];,
	ptab=Take[Drop[PPCSPtab,pp1-1],pnumber];];
If[diagnose,Print["Length of ptab = ",Length[ptab]];];
(* ptab now contains the definitions of all p's *)

(* get rhsplist, a list of polynomials with p's on rhs and for which the
p definition on the lhs will not already cause the whole line 
to be deleted*)
rhsplist={};
Do[( (* over {i,1,Length[ptab]} *)
	idyn=Length[ptab]-i; (* global *)
	If[!MemberQ[deletelist,i], (* if i is ON the delete list, then the whole
definition will be deleted so that it doesn't matter what is on the rhs *)
(* if rhs contains "p(", investigate further: *)
	(* if rhs contains "p(", add to rhsplist *)
		If[StringContainsQ[StringJoin[Drop[ptab[[i]],2]],"p("],
		rhsplist=Append[rhsplist,i];
		];
	];
),{i,1,Length[ptab]}];
*)

(* now calculate vettab, a list of polynomials that would cause problems 
if not deleted *)
vettab={};
Do[(  (* over {i,Sort[rhsplist,Greater]} *)
idyn=i; (* global *)
(*  find the positions of "p(" *)
posits={};
Do[( (* over {j,3,Length[ptab[[i]]]} *)
If[StringContainsQ[ptab[[i,j]],"p("],
posits=Append[posits,j];
];
),{j,3,Length[ptab[[i]]]}];
If[diagnose,Print["posits = ",posits];];
(* now see if rhs will be changed by the deletions *)
(* rhstab=ptab[[i]];*)
addtovettab=False;
Do[( (* over {j,posits} *)
num=ToExpression[StringDrop[StringDrop[ptab[[i,j]],2],-1]];
(* if the num of the rhs p is on the delete list for any of the rhs p terms,
then the lhs p might be left without perm symmetry, so we'll want to put
the lhs p value on the vettab list to get rid of it
on the delete list *)
If[MemberQ[deletelist,num],addtovettab=True; Break[];];
),{j,posits}];
If[addtovettab,vettab=Append[vettab,i];];

(*
Do[(
	If[StringTake[rhstab[[j]],1]=="q",rhstab[[j]]="(1)";];
),{j,3,Length[rhstab]}];
	If[diagnose,Print["rhstab2 = ",rhstab];];
	rhstab=Drop[rhstab,2];
	If[diagnose,Print["rhstab3 = ",rhstab];];
		rhsval=ToExpression[StringJoin[rhstab]];
		If[diagnose,Print["rhsval = ",rhsval];];
		If[rhsval==0,vettab=Append[vettab,i];];
*)

),{i,Sort[rhsplist,Greater]}];


Sort[vettab]
];




groupsforpolys::usage="groupsforpolys[polynumbers_,grouplist_]
Given grouplist, a table of lists of polynomial numbers for each group,
groupsforolys provides as output the numbers corresponding to all the
groups to which the polynomials in polynumbers are members.
";
groupsforpolys[polynumbers_,groups_]:=Module[
{groupp},

groupp={};
Do[(
groupp=Append[groupp,
Position[groups,polynumbers[[i]]][[1,1]] ];
),{i,1,Length[polynumbers]}];
Sort[groupp]
];






polysingroups::usage="polysingroups[groupnumbers_,groups_]
Given groups, a table of lists of polynomial numbers for each group,
polysingroups provides as output the numbers corresponding to all the
polynomials which are members of the groups listed in groupnumbers.
";
polysingroups[groupnumbers_,groups_]:=Module[
{keeplist},
keeplist={};
Do[(
Do[(
keeplist=Append[keeplist,groups[[group,j]]];
),{j,1,Length[groups[[group]]]}];
),{group,groupnumbers}];
Sort[keeplist]
];





makegroupskeeplist::usage=
"makegroupskeeplist[coefdesired_,groupvaluesorted_]
Given groupvaluesorted, a list of the sum of the values of all
the superpolynomials when evaluated using the maximum values
of the Morse values that occur in the data set, and the number of 
coefficients desired, makegroupskeeplist provides a list of the most 
important groups (or superpolynomials) to be kept.

Note that the ncoeffdesired now refers to the number of superpolynomials
desired.
";
makegroupskeeplist[ncoeffdesired_,groupvaluesorted_]:=Module[
{groupskeep,sum},
groupskeep={};
sum=0;
Do[(
(*sum=sum+groupvaluesorted[[i,3]];*)
sum=sum+1;
groupskeep=Append[groupskeep,groupvaluesorted[[i,1]]];
If[sum>= ncoeffdesired,Break[];];
),{i,1,Length[groupvaluesorted]}];
Sort[groupskeep]
];








findevaluatepermsymgroups24::usage="
findevaluatepermsymgroups[fortranname_, natomsparent_,...]
NB: this has now been modified to work for numofnmers = 2, 3, or4
When we have duplicated the data set so as to use a PIP basis with 
lesser symmetry, we need to add or delete polynomials in groups that 
maintain the perm symmetry of the basis set needed.  This program is
for when the data set is duplicated 24 times. 
fortranname: full name of purified/compacted file that you want to
	prune from
natomsparent:  number of atoms total (e.g. 12)
xyzperms: file of numofnmer! xyz format geometries related by symmetric exchange
	of the nmers (e.g. waters) (partitioned)
pesfile: full filename of the data base
NB: if the next two input files don't exist, they will be created;
	if they do exist, then they will be read and this routine will
	simply read them and output them. 
groupsfname:  full filename for to storing or reading the groups list
groupvaluessortedfname: full filename for storing or reading the 
	groupvaluessortedfname
DataDir: data directory where results will be stored.

This program requires DeleteDuplicatesdVX.x.wl to be loaded.  
";
findevaluatepermsymgroups24[fortranname_,natomsparent_,xyzperms_,pesfile_,
	groupsfname_,groupswithdupsfname_,groupvaluessortedfname_,DataDir_]:=Module[
{ifraga,polystarta,polyenda,npolya,nmonoa,finishup,list,
pos,skipi,groups,groupswithdups,groupvalues,groupvaluessorted,sum,
pesdata,natoms,nenergies,data,mtab,gm,xsum,group,diagnose
},

diagnose=False;
(*xyz=Partition[xyz,natomsparent+2];*)
If[FileExistsQ[groupsfname] && FileExistsQ[groupvaluessortedfname] &&
	FileExistsQ[groupswithdupsfname],
Print["Reading groups, groupswithdups, and groupvaluessorted from File"];
groups=Import[groupsfname,"Table"];
groupswithdups=Import[groupswithdupsfname,"Table"];
groupvaluessorted=Import[groupvaluessortedfname,"Table"];
Goto[finishup];
,
Print["Calculating groups, groupswithdups, and groupvaluessorted"];
];

ifraga=-1; (* indicates that below should use fortranname as the source *)
(* the following takes definitions from fortranname *)
{rijnamesnow,nvariables,mnumber,pnumber,qnumber}=
	GetDefinitionstoMathematicaFromFortranOutputPure[];
	Print["{mnumber,qnumber,pnumber} = ",{mnumber,qnumber,pnumber}];
	dist=Table[0,{i,1,nvariables}];
{polystarta, polyenda, npolya, nmonoa}=
	GetAssignEVMonoPolyPure[ifraga,natomsparent,rijnamesnow];
Print["EVMonoPolyPure has been assigned"];
mlistpur={"m0"};
Do[(
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];

(* now we evaluate the polynomials for each of the 24 permutated xyz *)
EPtab=Table[{0},{i,1,numofnmers!}];
Do[(
idyn=(numofnmers!)-i+1; (*global*)
getdist[xyzperms,i];
Assignx[];
EM=EvalMonoNew[PPCharStringmono,mlistpur];
EP=EvalPolyNew[PPCharStringpoly,plistpur];
EPm1=Drop[EP,1];
EPtab[[i]]=
	Sort[Table[{i,EPm1[[i]]},{i,1,Length[EPm1]}],
	#1[[2]] > #2[[2]] &];
If[i>1 && EPtab[[i,All,2]]!= EPtab[[1,All,2]],
Print["Values are not the same for EPTab[[1]] and EPTab[[",i,"]]"];
];
),{i,1,numofnmers!}];
(* The EPtab[[i]] are 24 tables sorted by value, each with pnumber 
values. The values are the same in each table, but the polynomial 
numbers associated with the values are permuted.  The groups are obtained
by reading for each index, the polynomial from each permutation and grouping
those 24 polynomials together as a group *)
grouppvalues=EPtab[[1,All,2]];
If[diagnose,PLHgrouppvalues=grouppvalues];
Print["grouppvalues determined"];
Print["finding groups"];
groups=Table[{},{i,1,Length[grouppvalues]}];
Do[(
Do[(
groups[[j]]=Sort[DeleteDuplicates[Append[groups[[j]],EPtab[[i,j,1]]]]];
),{j,1,Length[grouppvalues]}];
),{i,1,numofnmers!}];
groups=DeleteDuplicates[groups];
If[diagnose,PLHgroups=groups];
(* groups now contains all unique groups, not all with 24 polynomial numbers *)
Print["finding groupswithdups"];
groupswithdups=Table[{},{i,1,Length[grouppvalues]}];
Do[(
Do[(
groupswithdups[[j]]=Sort[Append[groupswithdups[[j]],EPtab[[i,j,1]]]];
),{j,1,Length[grouppvalues]}];
),{i,1,numofnmers!}];
groupswithdups=DeleteDuplicates[groupswithdups];
If[diagnose,PLHgroupswithdups=groupswithdups];
(* Now get the xx table from the data base *)
Print["calculating xx table from data base"];
pesdata=Import[pesfile,"Table"];
natoms=natomsparent;
data=pesdata;
nenergies=Length[data]/(natoms+2);
(* Get the Morse distributions *)
mtab=MorseDist[natoms,data];
gm=MorseDistGM[natoms,data];
(*Get the Averages and Standard Deviations ordered by largest average *)
xx=GetMorseAvMinMax[mtab,natoms];
If[diagnose,PLHxx=xx];
xsum=Length[rijnamesnow];
If[!ValueQ[xxindex],xxindex=5;];
Print["Evaluating polynomials with xxindex = ",xxindex];
(* assign the x values based on the xx table *)
Do[(
ijnow=rijnamesnow[[k]];
inow=ToExpression[StringDrop[ijnow,-2]];
jnow=ToExpression[StringDrop[ijnow,2]];
(*Print["{inow,jnow} = ",{inow,jnow}];*)
ToExpression["x"<>ToString[k]<>"=xx[[inow,jnow,xxindex]];"];  
(* xxindex either comes in through template values or is assigned above to be 5 *)
),{k,1,xsum}];
Print["x values assigned"];
(* Now write a program to evaluate the mean of the polynomial values 
in each group of polylist. *)
(* evaluate the polynomials with these x values *)
EM=EvalMono[];
EP=EvalPolyNew[PPCharStringpoly,plistpur];
(* now evaluate groups *)
Print["Calculating groupvalues"];
groupvalues={};
Do[(
idyn=Length[groupswithdups]-i;
list=Table["p"<>ToString[groupswithdups[[group,j]]],
	{j,1,Length[groupswithdups[[group]]]}];
sum=Sum[ToExpression[list[[j]]],{j,1,Length[groupswithdups[[group]]]}];
(*groupvalues=Append[groupvalues,{group,mean,Length[groups[[group]]]}];*)
groupvalues=Append[groupvalues,{group,sum,Length[groupswithdups[[group]]]}];
),{group,1,Length[groupswithdups]}];
(* now sort this by the value; the highest valued groups are the 
most important to keep *)
groupvaluessorted=Sort[groupvalues,#1[[2]]>#2[[2]] &];
(* export the results *)
Print["Exporting results"];
Export[groupsfname,groups,"Table"];
Export[groupswithdupsfname,groupswithdups,"Table"];
Export[groupvaluessortedfname,groupvaluessorted,"Table"];
Label[finishup];
{groups,groupswithdups,groupvaluessorted}

(* here's a description of the method used above:
A potential problem with the coefficient method for determining groups is 
that it requires a fit.  Here\[CloseCurlyQuote]s another method which is actually simpler 
and can get the groups from the DD file or the msa file, with no fit needed.  
The method just requires thinking about the problem is a slightly 
different way. 
 
Start with a file of xyz24 made by choosing a particular geometry and 
then doing the 24 water permutations on it.  It doesn\[CloseCurlyQuote]t matter what 
the original geometry is.  The idea is to evaluate the polynomials 
of the 22221111_4 output for each of the 24 geometries.  This gives 
a matrix of 24 columns and pnumber=51443 rows.  Each column has the 
same values, but not in the same order.  It has the same values 
because the energy associated with the vectors made of each column 
must be the same for each permutation.  The order is not the same, 
because the permutations mix up the correlation between the values 
and the polynomial identities.  
 
The first row shows the values of p(1) for the 24 permutations and looks
like this:
 
{0.00285262, 0.00340624, 0.00285262, 0.0116641, 0.00340624, 
0.0116641, 0.00285262, 0.00340624, 0.00285262, 0.00880536, 
0.00340624, 0.00880536, 0.00285262, 0.0116641, 0.00285262, 
0.00880536, 0.0116641, 0.00880536, 0.00340624, 0.0116641, 0.00340624, 
0.00880536, 0.0116641, 0.00880536}
 
The values are mostly different.  Now lets make the entries also show 
the number of the associated polynomial. It looks similar, but 
we now have a three-dimensional matrix.  The first row looks like 
this (same values as before, each associated with p(1)):
 
{{1, 0.00285262}, {1, 0.00340624}, {1, 0.00285262}, {1, 0.0116641}, 
{1, 0.00340624}, {1, 0.0116641}, {1, 0.00285262}, {1, 0.00340624}, 
{1, 0.00285262}, {1, 0.00880536}, {1, 0.00340624}, {1,0.00880536}, 
{1, 0.00285262}, {1, 0.0116641}, {1, 0.00285262}, {1,0.00880536}, 
{1, 0.0116641}, {1, 0.00880536}, {1, 0.00340624}, {1,0.0116641}, 
{1, 0.00340624}, {1, 0.00880536}, {1, 0.0116641}, {1,0.00880536}}
 
So this is the first row; there are 51433 rows.  Now sort each 
column by the value, but keeping the correspondence of the 
polynomials. The new first row has all the same values, 
but different polynomials: 
 
{{898, 0.351996}, {908, 0.351996}, {764, 0.351996}, {664, 0.351996}, {788, 0.351996}, 
{674, 0.351996}, {1051, 0.351996}, {1245, 0.351996}, {1040, 0.351996}, {1222, 0.351996}, 
{1424, 0.351996}, {1412, 0.351996}, {888, 0.351996}, {880, 0.351996}, {776,0.351996},
{684, 0.351996}, {775, 0.351996}, {683, 0.351996}, {1233, 0.351996}, {1027, 0.351996},
{1416, 0.351996}, {1411, 0.351996}, {1039,0.351996}, {1221,0.351996}}.
 
The sorted list of polynomials is a group that all have the same 
value, so in the coefficient list they would all have the same value 
of the coefficient.  So this is a permutationally symmetric group 
with respect to exchange of the waters:
 
{664, 674, 683, 684, 764, 775, 776, 788, 880, 888, 898, 908, 1027, 
1039, 1040, 1051, 1221, 1222, 1233, 1245, 1411, 1412, 1416, 1424}
 
In fact, the polynomials in each row share the same value and, thus, 
each is another group.  There are 51433 groups of 24.  However,
It should be noted that in not all cases are all 24 polynomials 
in a group different from one another.  In some cases, because of 
how the polynomials respond to the permutations, some polynomials 
of the group may be repeated.  So the number of unique polynomials 
may be smaller than 24.  
 
To see which groups to keep, we merely evaluate the polynomials 
(or any one of them) in each group by using the maximum Morse values 
as determined by the values of the data set.  We then order the groups 
from largest value to smallest and keep the first n groups that 
provide the requested number of polynomials.  All of this can be 
done in one fairly simple function.  
 
For the 22221111_4 case, I have confirmed for many groups that a 
group determined by this method corresponds to a group determined 
by the coefficient method.  
*)
];







findevaluatepermsymgroups24nogroupvalues::usage="
findevaluatepermsymgroups[fortranname_, natomsparent_,...]
NB: this has now been modified to work for numofnmers = 2, 3, or 4
When we have duplicated the data set so as to use a PIP basis with 
lesser symmetry, we need to add or delete polynomials in groups that 
maintain the perm symmetry of the basis set needed.  This program is
for when the data set is duplicated 24 times. 
fortranname: full name of purified/compacted file that you want to
	prune from
natomsparent:  number of atoms total (e.g. 12)
xyzperms: file of numofnmers! xyz format geometries related by symmetric exchange
	of the nmers (e.g. waters)(partitioned)
pesfile: full filename of the data base
NB: if the next two input files don't exist, they will be created;
	if they do exist, then they will be read and this routine will
	simply read them and output them. 
groupsfname:  full filename for to storing or reading the groups list

DataDir: data directory where results will be stored.

This program requires DeleteDuplicatesdVX.x.wl to be loaded.  
";
findevaluatepermsymgroups24nogroupvalues[fortranname_,natomsparent_,xyzperms_,
	groupsfname_,groupswithdupsfname_,DataDir_]:=Module[
{ifraga,polystarta,polyenda,npolya,nmonoa,finishup,list,
pos,skipi,groups,groupswithdups,grouppvalues,groupvaluessorted,sum,
pesdata,natoms,nenergies,data,mtab,gm,xsum,group,diagnose,EPtab,
EPm1,ndec,skipfornmer2,kstart,kst,ept
},

diagnose=False;
If[diagnose,PLHxyzperms=xyzperms];

If[FileExistsQ[groupsfname] && 
	FileExistsQ[groupswithdupsfname],
Print["Reading groups, groupswithdups, and groupvaluessorted from File"];
groups=Import[groupsfname,"Table"];
groupswithdups=Import[groupswithdupsfname,"Table"];

Goto[finishup];
,
Print["Calculating groups and groupswithdups"];
];

ifraga=-1; (* indicates that below should use fortranname as the source *)
(* the following takes definitions from fortranname *)
{rijnamesnow,nvariables,mnumber,pnumber,qnumber}=
	GetDefinitionstoMathematicaFromFortranOutputPure[];
	Print["{mnumber,qnumber,pnumber} = ",{mnumber,qnumber,pnumber}];
	dist=Table[0,{i,1,nvariables}];
{polystarta, polyenda, npolya, nmonoa}=
	GetAssignEVMonoPolyPure[ifraga,natomsparent,rijnamesnow];
Print["EVMonoPolyPure has been assigned"];
mlistpur={"m0"};
Do[(
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];

(* now we evaluate the polynomials for each of the  permutated xyz *)
EPtab=Table[{0},{i,1,numofnmers!}];
Do[(
idyn=numofnmers!-i+1; (*global*)
getdist[xyzperms,i];

Assignx[];
EM=EvalMonoNew[PPCharStringmono,mlistpur];
EP=EvalPolyNew[PPCharStringpoly,plistpur];

EPm1=Drop[EP,1];
EPtab[[i]]=
	Sort[Table[{i,EPm1[[i]]},{i,1,Length[EPm1]}],
	#1[[2]] > #2[[2]] &];
If[i>1 && EPtab[[i,All,2]]!= EPtab[[1,All,2]],
Print["Values are not the same for EPTab[[1]] and EPTab[[",i,"]]"];
];
),{i,1,numofnmers!}];
If[diagnose,PLHEPtab=EPtab];
(* The EPtab[[i]] are numofnmers! tables sorted by value, each with pnumber 
values. The values are the same in each table, but the polynomial 
numbers associated with the values are permuted.  The groups are obtained
by reading for each index, the polynomial from each permutation and grouping
those 24 polynomials together as a group *)
grouppvalues=EPtab[[1,All,2]];
If[diagnose,PLHgrouppvalues=grouppvalues];

If[numofnmers==2,
groupswithdups=findgroupswithdupsfor2nmers[EPtab];
If[diagnose,PLHgroupswithdups=groupswithdups];
Goto[skipfornmer2];
];
(* keep ndec decimal places *)
ndec=13;
Print["Comparing groupvalues with EPtab to ",ndec," decimal places"];
Do[(
grouppvalues[[i]]=N[IntegerPart[10^ndec*grouppvalues[[i]]]/10^ndec];
),{i,1,Length[grouppvalues]}];
grouppvalues=DeleteDuplicates[grouppvalues];  (* all polys with same group value are in
the same group *)
Print["grouppvalues determined"];
Print["finding groups"];
(* this is how it used to be done; there are problems of different groups having
the same group value
groups=Table[{},{i,1,Length[grouppvalues]}];
Do[(
Do[(
groups[[j]]=Sort[DeleteDuplicates[Append[groups[[j]],EPtab[[i,j,1]]]]];
),{j,1,Length[grouppvalues]}];
),{i,1,numofnmers!}];
*)
(* groupswithdups will first show whichpolys go with which permutations *)
groupswithdups=Table[{},{j,1,Length[grouppvalues]},{k,1,numofnmers!}];
kstart=Table[1,{k,1,numofnmers!}];
kst=1;
Do[(
idyn=Length[grouppvalues]-j;
Do[(
Do[(
		ept=N[IntegerPart[10^ndec*EPtab[[i,k,2]]]/10^ndec];
(*Print[{i,j,ept,grouppvalues[[j]],EPtab[[i,k,1]]}];*)
		If[ept==grouppvalues[[j]],
		groupswithdups[[j,i]]=Append[groupswithdups[[j,i]],EPtab[[i,k,1]]];
(*Print["appended ",EPtab[[i,k,1]]];Pause[.3];*)
		kstart[[i]]=k;
		,
		If[ept< grouppvalues[[j]],
		Break[];
		];
	];
	kst=Min[kstart[[All]]];
),{k,kst,Length[grouppvalues]}];
groupswithdups[[j,i]]=Sort[groupswithdups[[j,i]]];
),{i,1,numofnmers!}];
),{j,1,Length[grouppvalues]}];
(* get rid of unused space *)
Do[(
If[groupswithdups[[i]]==Table[{},{k,1,numofnmers!}],
groupswithdups=Drop[groupswithdups,{i}];
];
),{i,Length[groupswithdups],1,-1}];
If[diagnose,PLHgroupswithdups=groupswithdups];
(* now join the permutations *)
Do[(
groupswithdups[[i]]=Sort[Flatten[groupswithdups[[i]]]];
),{i,1,Length[groupswithdups]}];
groupswithdups=DeleteDuplicates[groupswithdups];

Label[skipfornmer2];
(* get groups *)
groups=groupswithdups;
Do[(
groups[[i]]=DeleteDuplicates[groupswithdups[[i]]];
),{i,1,Length[groupswithdups]}];
If[diagnose,PLHgroups=groups];
(* groups now contains all unique groups, not all with numofnmers! polynomial numbers *)
(* continuation of old method 
Print["finding groupswithdups"];
groupswithdups=Table[{},{i,1,Length[grouppvalues]}];
Do[(
Do[(
groupswithdups[[j]]=Sort[Append[groupswithdups[[j]],EPtab[[i,j,1]]]];
),{j,1,Length[grouppvalues]}];
),{i,1,numofnmers!}];
groupswithdups=DeleteDuplicates[groupswithdups];
If[diagnose,PLHgroupswithdups=groupswithdups];
*)
Print["Exporting results"];
Export[groupsfname,groups,"Table"];
Export[groupswithdupsfname,groupswithdups,"Table"];
Print["The number of groups is ",Length[groupswithdups]];

Label[finishup];
{groups,groupswithdups}

(* here's a description of the method used above:
A potential problem with the coefficient method for determining groups is 
that it requires a fit.  Here\[CloseCurlyQuote]s another method which is actually simpler 
and can get the groups from the DD file or the msa file, with no fit needed.  
The method just requires thinking about the problem is a slightly 
different way. 
 
Start with a file of xyz24 made by choosing a particular geometry and 
then doing the 24 water permutations on it.  It doesn\[CloseCurlyQuote]t matter what 
the original geometry is.  The idea is to evaluate the polynomials 
of the 22221111_4 output for each of the 24 geometries.  This gives 
a matrix of 24 columns and pnumber=51443 rows.  Each column has the 
same values, but not in the same order.  It has the same values 
because the energy associated with the vectors made of each column 
must be the same for each permutation.  The order is not the same, 
because the permutations mix up the correlation between the values 
and the polynomial identities.  
 
The first row shows the values of p(1) for the 24 permutations and looks
like this:
 
{0.00285262, 0.00340624, 0.00285262, 0.0116641, 0.00340624, 
0.0116641, 0.00285262, 0.00340624, 0.00285262, 0.00880536, 
0.00340624, 0.00880536, 0.00285262, 0.0116641, 0.00285262, 
0.00880536, 0.0116641, 0.00880536, 0.00340624, 0.0116641, 0.00340624, 
0.00880536, 0.0116641, 0.00880536}
 
The values are mostly different.  Now lets make the entries also show 
the number of the associated polynomial. It looks similar, but 
we now have a three-dimensional matrix.  The first row looks like 
this (same values as before, each associated with p(1)):
 
{{1, 0.00285262}, {1, 0.00340624}, {1, 0.00285262}, {1, 0.0116641}, 
{1, 0.00340624}, {1, 0.0116641}, {1, 0.00285262}, {1, 0.00340624}, 
{1, 0.00285262}, {1, 0.00880536}, {1, 0.00340624}, {1,0.00880536}, 
{1, 0.00285262}, {1, 0.0116641}, {1, 0.00285262}, {1,0.00880536}, 
{1, 0.0116641}, {1, 0.00880536}, {1, 0.00340624}, {1,0.0116641}, 
{1, 0.00340624}, {1, 0.00880536}, {1, 0.0116641}, {1,0.00880536}}
 
So this is the first row; there are 51433 rows.  Now sort each 
column by the value, but keeping the correspondence of the 
polynomials. The new first row has all the same values, 
but different polynomials: 
 
{{898, 0.351996}, {908, 0.351996}, {764, 0.351996}, {664, 0.351996}, {788, 0.351996}, 
{674, 0.351996}, {1051, 0.351996}, {1245, 0.351996}, {1040, 0.351996}, {1222, 0.351996}, 
{1424, 0.351996}, {1412, 0.351996}, {888, 0.351996}, {880, 0.351996}, {776,0.351996},
{684, 0.351996}, {775, 0.351996}, {683, 0.351996}, {1233, 0.351996}, {1027, 0.351996},
{1416, 0.351996}, {1411, 0.351996}, {1039,0.351996}, {1221,0.351996}}.
 
The sorted list of polynomials is a group that all have the same 
value, so in the coefficient list they would all have the same value 
of the coefficient.  So this is a permutationally symmetric group 
with respect to exchange of the waters:
 
{664, 674, 683, 684, 764, 775, 776, 788, 880, 888, 898, 908, 1027, 
1039, 1040, 1051, 1221, 1222, 1233, 1245, 1411, 1412, 1416, 1424}
 
In fact, the polynomials in each row share the same value and, thus, 
each is another group.  There are 51433 groups of 24.  However,
It should be noted that in not all cases are all 24 polynomials 
in a group different from one another.  In some cases, because of 
how the polynomials respond to the permutations, some polynomials 
of the group may be repeated.  So the number of unique polynomials 
may be smaller than 24.  
 
To see which groups to keep, we merely evaluate the polynomials 
(or any one of them) in each group by using the maximum Morse values 
as determined by the values of the data set.  We then order the groups 
from largest value to smallest and keep the first n groups that 
provide the requested number of polynomials.  All of this can be 
done in one fairly simple function.  
 
For the 22221111_4 case, I have confirmed for many groups that a 
group determined by this method corresponds to a group determined 
by the coefficient method.  
*)
];







findgroupswithdupsfor2nmers::usage="
idea is that for each pair of numbers, say {7,19}, if 7 is in the first
position there is a set of possibilities, p7, in the second position.  But if 
19 is the match, then when 19 is in the first position there will be a set
of possibilties p19 in the second position and p19 must contain 7; there may 
be others as well, but the only possibilities are the intersection of sets
p7 and p19.  One then tests which of the possibilities are the right ones by
comparing their EPtab values.  This works easily for numofnmers=2, but may not 
be easy for other choices, where the regular method works well.  The regular 
method does not work well for numofnmers=2 because it is too sensitive to the
machine error. 
";
findgroupswithdupsfor2nmers[EPtab1_]:=Module[
{ndec,TrEPtab,bigtable,qnum,p1,p2,p1s,p2s,possible1,possible2,
list,tpossible1,tpossible2,result,groupswithdups,orphans,first,
add,skipi,diagnose,EPtab},

diagnose=False;
EPtab=EPtab1;
(* get all values to the nearest ndec decimal places *)
ndec=13;
If[diagnose,Print["rounding all values to the nearest ",ndec," decimal places"];];
Do[(
Do[(
EPtab[[j,i,2]]=N[Round[10^ndec  EPtab[[j,1,2]]]/10^ndec ];
),{j,1,2}];
),{i,1,Length[EPtab]}];
TrEPtab=Transpose[EPtab];
Do[(
Do[(
TrEPtab[[i,j,2]]=N[Round[10^ndec  TrEPtab[[i,j,2]]]/10^ndec ];
),{j,1,2}];
),{i,1,Length[TrEPtab]}];
(* now make bigtable, which gives for each polynomial i in TrEPtab the range of 
possibilities {j1,j2,j3,....} for ths second number: {i, {j1,j2,j3,...}}  *)
(* NB: the number of the polynomial in TrEPtab is not the same as in EPtab *)
If[diagnose,Print["making bigtable"];];
bigtable={};
Do[(
qnum=i;
idyn=Length[TrEPtab]-i; (* global *)
p1s=Position[TrEPtab[[All,1]],qnum][[All,1]];
p2s=Position[TrEPtab[[All,2]],qnum][[All,1]];
p1=TrEPtab[[p1s]][[1]];
p2=TrEPtab[[p2s]][[1]];
possible2={};
list=Flatten[Position[TrEPtab[[All,2,2]],p1[[1,2]]]];
Do[(
possible2=Append[  possible2,TrEPtab[[i,2,1]]  ];
),{i,list}];
possible2=Sort[possible2];
possible1={};
list=Flatten[Position[TrEPtab[[All,1,2]],p2[[2,2]]]];
Do[(
possible1=Append[  possible1,TrEPtab[[i,1,1]]  ];
),{i,list}];
possible1=Sort[possible1];
tpossible1={{qnum},possible1};
tpossible2={possible2,{qnum}};
result={qnum,Intersection[tpossible1[[2]],tpossible2[[1]]]};
bigtable=Append[bigtable,result];
),{i,1,Length[TrEPtab]}];
(* now find groupswithdups *)
If[diagnose,Print["finding groupswithdups"];];
groupswithdups={};
orphans={};
Do[(
first=bigtable[[i,1]];
If[Length[bigtable[[i,2]]]==1,
add=Sort[{first,bigtable[[i,2,1]]}];
groupswithdups=Append[groupswithdups,add];
,
Do[(
(*Print[{i,j}];
Print[EPtab[[1,first,2]]\[Equal]EPtab[[2,bigtable[[i,2,j]],2]]];*)
If[EPtab[[1,Position[EPtab[[1,All,1]],first][[1,1]]]][[2]]==
EPtab[[  2,Position[EPtab[[2,All,1]],bigtable[[i,2,j]]][[1,1]]  ]][[2]],
add=Sort[{first,bigtable[[i,2,j]]}];
groupswithdups=Append[groupswithdups,add];
Goto[skipi];
];
(* if it gets here, there was no solution *)
orphans=Append[orphans,{first,
EPtab[[1,Position[EPtab[[1,All,1]],first][[1,1]]]],
EPtab[[2,Position[EPtab[[2,All,1]],first][[1,1]]]]
}];
),{j,1,Length[bigtable[[i,2]]]}];
];
Label[skipi];
),{i,1,Length[bigtable]}];
groupswithdups=DeleteDuplicates[groupswithdups];
Print["groupswithdups has ",Length[groupswithdups]," pairs"];
If[Length[orphans]!=0,
Print["There are ",Length[orphans]," orphans"];
Print["orphan info available as PLHorphans, a global variable"];
];
PLHorphans=orphans; (* global *)
(* for the record, here is a little program to investigate pairs
i is the first number, j values run from 1 to ? *)
(*
i=2000;
j=1;
first=bigtable[[i,1]];
{first,bigtable[[i,2,j]]}
z1=EPtab[[1,Position[EPtab[[1,All,1]],first][[1,1]]]]
z2=EPtab[[  2,Position[EPtab[[2,All,1]],bigtable[[i,2,j]]][[1,1]]  ]]
(z1[[2]]-z2[[2]])/(z1[[2]]+z2[[2]])/2
*)

groupswithdups
];







makedeletelist[keeplist_,pnumber_]:=Module[
{deletelist},
deletelist=Table[i,{i,1,pnumber}];
Do[(
deletelist=Drop[deletelist,{i}];
),{i,Sort[DeleteDuplicates[keeplist],Greater]}];
deletelist
];





makekeepdeletelists24old[]:=Module[
{startingpoint,restartingpoint,cyclenum,finishup,
lowby,coefficientssought,
groups,groupswithdups,groupvaluessorted,ptab,rhsplist,groupskeep,
keeplist,deletelist,groupp,vettab,groupskeepnew,
ifrag1,monostart1,monoend1,polystart1,polyend1,
CSMforBackward,CSPforBackward,diagnose},

diagnose=False;

(* initiation steps *)
Print[Style["Initiation steps",Bold]];
(*
Print["Getting mono/poly character strings from fortranname"];
(*{polystarta, polyenda, npoly, nmono}=GetAssignEVMonoPoly[-1,natomsparent,rijnames[[1]]];*)
ifrag1=-1; 
CreatebemsaFiles[ifrag1];
{monostart1,monoend1,polystart1,polyend1}=
	GetpolymonostartendpointsPure[ifrag1];
{CSMforBackward,CSPforBackward}=
	GetmonopolycharstringsPure[ifrag1,monostart1,monoend1,polystart1,polyend1];
(*CSMforBackward=StringReplace[StringReplace[StringReplace[
	CSMforBackward,"+"->" + "],"-"->" -"],"  "->" "];	*)
CSPforBackward=StringReplace[StringReplace[StringReplace[
	CSPforBackward,"+"->" + "],"-"->" -"],"  "->" "];	
If[diagnose,PLHCSMforBackward=CSMforBackward;];
If[diagnose,PLHCSPforBackward=CSPforBackward;];
PPCharStringpoly=CSPforBackward; (* global *)
pnumber= pnumber=ToExpression[StringDrop[
	StringDrop[bemsaTabc[[polyend1,1]],2],-1]];(* global *)
Print["pnumber = ",pnumber];
*)
{groups,groupswithdups,groupvaluessorted}=
	findevaluatepermsymgroups24[fortranname,natomsparent,xyzperms,pesfile,
	groupsfname,groupswithdupsfname,groupvaluessortedfname,DataDir];


coefficientssought=ncoeffdesired;
Label[restartingpoint];	
groupskeep=makegroupskeeplist[coefficientssought,groupvaluessorted];
Print["Current number of groups to keep = ",Length[groupskeep]];

cyclenum=0;
Label[startingpoint];
cyclenum=cyclenum+1;
If[cyclenum==10, Print["No convergence; aborting"];Abort[];];
Print[Style["Starting cycle ",Bold],cyclenum];

keeplist=polysingroups[groupskeep,groups];
(*Print["Current number of polynomials to keep = ",Length[keeplist]];*)
(* forget about restarting; it's hard to repeat a calculation
lowby=(Length[keeplist]-ncoeffdesired)/ncoeffdesired ;
If[lowby < -.1,
	coefficientssought=IntegerPart[(1-lowby)coefficientssought];
	Print[Style["Restarting with larger number = ",Red,Bold],coefficientssought];
	Goto[restartingpoint];
];
*)
deletelist=makedeletelist[keeplist,pnumber];
Print["Current number of polynomials to delete = ",Length[deletelist]];	
If[Length[keeplist]+Length[deletelist]!= pnumber,
	Print[Style[" Warning: Length[keeplist]+Length[deletelist \[NotEqual] pnumber",
		Red,Bold];];
];

Print["Making ptab and rhsplist"];
{ptab,rhsplist}=makeptabrhsplist[pnumber,PPCharStringpoly,deletelist,DataDir];	
Print["Length of rhsplist = ",Length[rhsplist]];

Print["Finding rhs changes"];
vettab=findrhszeros[ptab,rhsplist,deletelist];
Print["No. of rhs zero polynomials = ",Length[vettab]];
groupp=DeleteDuplicates[groupsforpolys[vettab,groups]];
Print["    These polynomials are in ",Length[groupp]," groups"];
If[groupp=={},Goto[finishup];];

groupskeepnew=Complement[groupskeep,groupp];
Print["Current number of groups to keep = ",Length[groupskeep]];
Print["Current number of polynomials to keep = ",
	Length[polysingroups[groupskeepnew,groups]]];
groupskeep=groupskeepnew;
Goto[startingpoint];

Label[finishup];
keeplist=polysingroups[groupskeep,groups];
keeplist=Sort[keeplist];
Print["Current number of polynomials to keep = ",Length[keeplist]];
deletelist=makedeletelist[keeplist,pnumber];
Print["Current number of polynomials to delete = ",Length[deletelist]];
If[Length[keeplist]+Length[deletelist]!= pnumber,
	Print[Style[" Warning: Length[keeplist]+Length[deletelist \[NotEqual] pnumber",
		Red,Bold];];
];

Print["Exporting files"];
Print["deletelist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"]];
Print["keeplist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"]];
Export[DataDir<>"deletelist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"],deletelist,"List"];
Export[DataDir<>"keeplist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"],keeplist,"List"];

{keeplist,deletelist}
];






makekeepdeletelists24[]:=Module[
{coefficientssought,groupskeep,keeplist,deletelist},

coefficientssought=ncoeffdesired;

{groups,groupswithdups,groupvaluessorted}=
	findevaluatepermsymgroups24[fortranname,natomsparent,xyzperms,pesfile,
	groupsfname,groupswithdupsfname,groupvaluessortedfname,DataDir];
	
groupskeep=makegroupskeeplist[coefficientssought,groupvaluessorted];
Print["Current number of groups to keep = ",Length[groupskeep]];
keeplist=groupskeep;
deletelist={};
Do[(
If[!MemberQ[keeplist,i],
	deletelist=Append[deletelist,i];
];
),{i,1,Length[groupswithdups]}];
(*
Print["Exporting files"];
Print["deletelist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"]];
Print["keeplist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"]];
Export[DataDir<>"deletelist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"],deletelist,"List"];
Export[DataDir<>"keeplist_22221111_4_"<>ToString[Length[keeplist]]<>"_"<>
	DateString["ISODate"],keeplist,"List"];
*)
{keeplist,deletelist}	
];






TestMinimumTimeForBasisEvaluation::usage="
TestMinimumTimeForBasisEvaluation[mnumbernow_,pnumbernow_,qnumbernow_]

NB: This program switches fortranname to OutputFortranname
and then set it back to its original value 
";
TestMinimumTimeForBasisEvaluation[mnumbernow_,pnumbernow_,qnumbernow_]:=Module[
{ifraga,natomsa,rijnamesa,rijnamesnew,
rijc,namelist,namea,nameb,xsum,
EM,EP,EMnew,EPnew,rijnamesnow,nvariables,nmono,npoly,xyzorig,
xyztemp,mnumber,pnumber,mlistpur,plistpur,pausesecs,n,mintime,
tim,timesum,timeav
},

fortrannamesave=fortranname;
fortranname=OutputFortranname;

mnumber=mnumbernow;
pnumber=pnumbernow;
(* assumes Assignx, EvalMono and EvalPoly have already been defined *)
(* get lists of permutable atoms *)

{rijnamesnow,nvariables,nmono,npoly,qnumber}=GetDefinitionstoMathematicaFromFortranOutputPure[];
xsum=Length[rijnamesnow];
(*Print["xsum = ",xsum];*)
(*GetAssignEVMonoPolyPrev[xsum,msum,psum,rijnamesnow,CharStringmono,CharStringpoly];*)
ifraga=-1;
natomsa=10;  (* natomsa is not used in GetAssignEVMonoPoly when ifraga=-1 *)
rijnamesa=rijnamesnow;
mlistpur={"m0"};
Do[(
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];
GetAssignEVMonoPolyPure[ifraga,natomsa_,rijnamesa];

dist=Table[0,{i,1,xsum}];  (* needs to be global *)
n=5;
mintime=100000;
timesum=0;
pausesecs=5;
Do[(
	Do[(
		dist[[i]]=RandomReal[{0.1,1}];
	),{i,1,xsum}];
	Pause[pausesecs];
	tim=Timing[
		Assignx[];
		EM=IntegerPart[EvalMonoNew[PPCharStringmono,mlistpur]];
		EP=IntegerPart[EvalPolyNew[PPCharStringpoly,plistpur]];
		][[1]];
		mintime=Min[mintime,tim];
	timesum=timesum+tim;
),{i,1,n}];
timeav=timesum/n;
Print[n," basis set evaluations were performed over ",n*pausesecs," seconds"];
Print["Average time in sec. for one basis set evaluation = ",timeav];
Print["Minimum time in sec. for one basis set evaluations = ",mintime];
fortranname=fortrannamesave;
(* no output *)
]






generatesuperpolydefs[qnumbersave_]:=Module[
{ftext,slist},

ftext="";
Do[(
idyn=Length[groupswithdups]-i;
slist=polysingroups[{i},groupswithdups];
ftext=ftext<>"    u("<>ToString[i]<>") = ";
Do[(
ftext=ftext<>"+ q("<>ToString[slist[[j]]+qnumbersave]<>") ";
(* 11FEb2022, changed limit to Length[slist] from numofnmers! *)
),{j,1,Length[slist]}];
ftext=ftext<>"\n";
(*If[i\[Equal]1,Print[ftext];];*)
),{i,1,Length[groupswithdups]}];

ftext
];






choosewater4::usage="
NB: This assumes that the input order is HHOHHOHHOHHO !!!
this transforms the dataset into one with permuted values. For, example,
to get all permutations of a dataset for four waters you would calculate dsx:
dsx=Join[\[IndentingNewLine]choosewaterorder4[1,2,3,4,dataset],\[IndentingNewLine]choosewaterorder4[1,3,4,2,dataset],\[IndentingNewLine]choosewaterorder4[1,4,2,3,dataset],\[IndentingNewLine]choosewaterorder4[2,1,3,4,dataset],\[IndentingNewLine]choosewaterorder4[2,3,4,1,dataset],\[IndentingNewLine]choosewaterorder4[2,4,1,3,dataset],\[IndentingNewLine]choosewaterorder4[3,1,2,4,dataset],\[IndentingNewLine]choosewaterorder4[3,2,4,1,dataset],\[IndentingNewLine]choosewaterorder4[3,4,1,2,dataset],\[IndentingNewLine]choosewaterorder4[4,1,2,3,dataset],\[IndentingNewLine]choosewaterorder4[4,2,3,1,dataset],\[IndentingNewLine]choosewaterorder4[4,3,1,2,dataset],\[IndentingNewLine]\[IndentingNewLine]choosewaterorder4[1,2,4,3,dataset],\[IndentingNewLine]choosewaterorder4[1,3,2,4,dataset],\[IndentingNewLine]choosewaterorder4[1,4,3,2,dataset],\[IndentingNewLine]choosewaterorder4[2,1,4,3,dataset],\[IndentingNewLine]choosewaterorder4[2,3,1,4,dataset],\[IndentingNewLine]choosewaterorder4[2,4,3,1,dataset],\[IndentingNewLine]choosewaterorder4[3,1,4,2,dataset],\[IndentingNewLine]choosewaterorder4[3,2,1,4,dataset],\[IndentingNewLine]choosewaterorder4[3,4,2,1,dataset],\[IndentingNewLine]choosewaterorder4[4,1,3,2,dataset],\[IndentingNewLine]choosewaterorder4[4,2,1,3,dataset],\[IndentingNewLine]choosewaterorder4[4,3,2,1,dataset]\[IndentingNewLine]];
";
choosewaterorder4HHOHHOHHOHHO[choice1_,choice2_,choice3_,choice4_,dataset_]:=Module[
{dset,store,rec,choices,nwater,recsize},
dset=dataset;
nwater=4;
recsize=14;
store=Table[0,{k,1,3 nwater}];
rec=Table[0,{j,1,recsize}];
choices={choice1,choice2,choice3,choice4};
If[DeleteDuplicates[choices]!=choices,
Print["Aborted: two choices are the same!"];
Abort[];
];
Do[(  
(* isolate the ith record *)
Do[(
rec[[j]]=dset[[i,j]];
),{j,1,recsize}];
(* store the initial water info in store *)
Do[(
store[[k]]=rec[[2+k]]
),{k,1,3nwater}];
(* replace the first water with the choice1 *)
Do[(
rec[[2+k]]=store[[(choice1-1)3+k]];
),{k,1,3}];
(* replace the second water with the choice2 *)
Do[(
rec[[2+3+k]]=store[[(choice2-1)3+k]];
),{k,1,3}];
(* replace the third water with the choice3 *)
Do[(
rec[[2+6+k]]=store[[(choice3-1)3+k]];
),{k,1,3}];
(* replace the fourth water with the choice4 *)
Do[(
rec[[2+9+k]]=store[[(choice4-1)3+k]];
),{k,1,3}];
(* replace the ith record with the permuted one *)
Do[(
dset[[i,j]]=rec[[j]];
),{j,1,recsize}];
),{i,1,1 Length[dset]}];
dset
];







choosewaterorder3HHOHHOHHO[choice1_,choice2_,choice3_,dataset_]:=Module[
{dset,store,rec,choices,nwater,recsize},
dset=dataset;

(* NB: This assumes that the input order is HHOHHOHHO!!! *)
nwater=3;
recsize=1;
store=Table[0,{k,1,3 nwater}];
rec=Table[0,{j,1,recsize}];
choices={choice1,choice2,choice3};
If[DeleteDuplicates[choices]!=choices,
Print["Aborted: two choices are the same!"];
Abort[];
];
Do[(  
(* isolate the ith record *)
Do[(
rec[[j]]=dset[[i,j]];
),{j,1,recsize}];
(* store the initial water info in store *)
Do[(
store[[k]]=rec[[2+k]]
),{k,1,3nwater}];
(* replace the first water with the choice1 *)
Do[(
rec[[2+k]]=store[[(choice1-1)3+k]];
),{k,1,3}];
(* replace the second water with the choice2 *)
Do[(
rec[[2+3+k]]=store[[(choice2-1)3+k]];
),{k,1,3}];
(* replace the third water with the choice3 *)
Do[(
rec[[2+6+k]]=store[[(choice3-1)3+k]];
),{k,1,3}];
(* replace the fourth water with the choice4 *)
Do[(
rec[[2+9+k]]=store[[(choice4-1)3+k]];
),{k,1,3}];
(* replace the ith record with the permuted one *)
Do[(
dset[[i,j]]=rec[[j]];
),{j,1,recsize}];
),{i,1,1Length[dset]}];
dset
];







getCSMCSPfromfortranname[]:=Module[
{ifrag1,monostart1,monoend1,polystart1,polyend1,
CSMforBackward,CSPforBackward
},
ifrag1=-1; 
	CreatebemsaFiles[ifrag1];
	{monostart1,monoend1,polystart1,polyend1}=
		GetpolymonostartendpointsPure[ifrag1];
	{CSMforBackward,CSPforBackward}=
		GetmonopolycharstringsPure[ifrag1,monostart1,monoend1,polystart1,polyend1];
	CSMforBackward=StringReplace[StringReplace[StringReplace[
		CSMforBackward,"+"->" + "],"-"->" -"],"  "->" "];	
	CSPforBackward=StringReplace[StringReplace[StringReplace[
		CSPforBackward,"+"->" + "],"-"->" -"],"  "->" "];	
	If[diagnose,PLHCSMforBackward=CSMforBackward;];
	If[diagnose,PLHCSPforBackward=CSPforBackward;];
	PPCharStringpoly=CSPforBackward; (* global *)
  PPCharStringmono=CSMforBackward; (* global *)
	pnumber=ToExpression[StringDrop[
	StringDrop[bemsaTabc[[polyend1,1]],2],-1]];(* global *)
mnumber=ToExpression[StringDrop[
	StringDrop[bemsaTabc[[monoend1,1]],2],-1]];
(* Look for q dimension definition in first 200 lines *)
If[ValueQ[isave4890],
qnumber=ToExpression[StringDrop[StringDrop[bemsaTabc[[isave4890,1]],-4],21]];
,
qnumber=0;
];

{PPCharStringmono,PPCharstringpoly,mnumber,pnumber,qnumber}
];






testlimitingbehaviorandnmerpermutation[]:=Module[
{nmono,npoly,pnumber,mnumber,print,monodroplist,
polydroplist,addx,currentatoms,dname,dname1,
dname2,skip,xsum,EM,EP,lmdrop,lpdrop,nq,sums,s,sdovermean
,polystart,polyend,ifraga,nvariables,EPtab,TrEPtab,EPm1,
nonInvariantList,sdonmeantab,sdonmean,xyzperms
},

print=False;
xyzperms=Partition[Import[xyzpermsfilename,"Table"],2+natomsparent];
(*Print[xyzperms];*)

(* The first part is testing the limiting behavior *)
	(* get mono poly fortran lists from  fortran file *)
fortranname=OutputFortranname;
	{rijnamesnow,nvariables,nmono,npoly,nq}=
		GetDefinitionstoMathematicaFromFortranOutputPure[];
(*Print["{nmono,npoly,nq} = ",{nmono,npoly,nq}];*)
rijnames[[1]]=rijnamesnow; 
(*If[print,Print["Coordinates now in use:"]];
If[print,Print[rijnamesnow]];*)
xsum=Length[rijnamesnow];
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
ifraga=-1; (* indicates that below should use fortranname as the source *)
{polystart, polyend, npoly, nmono}=
	GetAssignEVMonoPolyPure[ifraga,natomsparent,rijnamesnow];
(*Print["EVMonoPolyPure has been assigned"];*)
pnumber=npoly;
mnumber=nmono;
mlistpur={"m0"};
Do[(
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"};
Do[(
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];
monodroplist={};
polydroplist={};
addx=100.;
Do[(  (* nmerid = 1 to Length[nmeratoms] *)
currentatoms=nmeratoms[[nmerid]];
(*Assign \[OpenCurlyDoubleQuote]normal\[CloseCurlyDoubleQuote] but random distances to all possibilities*)
	Do[( 
		dist[[i]]=RandomReal[{1,2}];
	),{i,1,xsum}];
(* assign data distances to dist *)
Do[( 
dname=rijnamesnow[[i]];
dname1=ToExpression[StringDrop[dname,-2]];
dname2=ToExpression[StringDrop[dname,   2]];
(* now, for the current nmer(s), pull it away from the rest *)
(* add addx to x & y data coordinates if these are in current atoms *)
If[MemberQ[currentatoms,dname1] && MemberQ[currentatoms,dname2],
Goto[skip];]; (* if both atoms are on list, skip *)
If[!MemberQ[currentatoms,dname1] && !MemberQ[currentatoms,dname2],
Goto[skip];]; (* if both atoms are of the list, skip *)
If[Or[MemberQ[currentatoms,dname1],
	MemberQ[currentatoms,dname2]],dist[[i]]=dist[[i]]+addx];
Label[skip];
	),{i,1,xsum}];	
	
(* assign the current distances *)
Assignx[];
If[print,Print[currentatoms];];
If[print,Print[dist];];
EM=EvalMonoNew[PPCharStringmono,mlistpur];
EM=Drop[EM,1];
EP=EvalPolyNew[PPCharStringpoly,plistpur];
EP=Drop[EP,1];

(* append to monodroplist *)
Do[( 
If[EM[[j]]>10^(-6),
monodroplist=Append[monodroplist,j];
];
),{j,1,Length[EM]}];
(* append to polydroplist *)
Do[( 
If[EP[[j]]>10^(-6),
	polydroplist=Append[polydroplist,j];
];
),{j,1,Length[EP]}];
),{nmerid,1,Length[nmeratoms]}];
If[print,Print["{nmono,maxmono,minmono} = ",{Length[EM],Max[EM],Min[EM]}];];
If[print,Print["{npoly,maxpoly,minpoly} = ",{Length[EP],Max[EP],Min[EP]}];];
monodroplist=Sort[DeleteDuplicates[monodroplist]];
polydroplist=Sort[DeleteDuplicates[polydroplist]];
lmdrop=Length[monodroplist];
lpdrop=Length[polydroplist];
Print[Style["\nTest Results",Blue,FontSize->15]];
Print["Testing of file ",OutputFortranname];
If[lpdrop==0,
	Print["The output file has no polynomials that do not vanish at long distances"];
	,
	Print["The optput file has ",lmdrop," monomials and ",lpdrop, " polynomials"];
	Print["that do not vanish at long distances"];
];




(* now test permutation of nmers *)
If[numofnmers==4 || numofnmers==3 || numofnmers==2,
     (* get one geometry from data set *)
     sums={};
     EPtab=Table[{0},{i,1,numofnmers!}];
     Do[(
       idyn=numofnmers!-i+1; (*global*)
       getdist[xyzperms,i];
       Assignx[];
       EM=EvalMonoNew[PPCharStringmono,mlistpur];
	   EP=EvalPolyNew[PPCharStringpoly,plistpur];
	   EPm1=Drop[EP,1]; (* drop the p(0) term *)
	   EPtab[[i]]=Table[{j,EPm1[[j]]},{j,1,Length[EPm1]}];
     ),{i,1,numofnmers!}];
     TrEPtab=Transpose[EPtab];
     (*TrEPtab gives, for each superpolynomial the value of 
     the superpolynomial for each of numofnmer! permutations. *)      
	(* Now find which groups are invariant *)
    
    nonInvariantList={};
    sdonmeantab={};
	Do[(
		sdonmean=
		StandardDeviation[TrEPtab[[i,All,2]]]/Abs[Mean[TrEPtab[[i,All,2]]]];
		(*Print[sdonmean];*)
		If[sdonmean<10^-9,
			sdonmeantab=Append[sdonmeantab,sdonmean];
			,
			nonInvariantList=Append[nonInvariantList,{i,sdonmean}];
		];
	),{i,1,Length[TrEPtab]}];
     If[Length[Sort[nonInvariantList]]==0,
        Print["The output file passes the nmer perumtation test"];
        Print["    the mean value of SD over Mean is ",Mean[sdonmeantab]];
        ,
        Print["The output file does not pass the nmer permutation test"];
        Print["    the polynomials that are not nmer permutation invariant are,
				    along with their sdonmean values:",
            nonInvariantList];
     ];
     Print["Testing of limiting behavior and nmer permutation is complete"];
     ,
     Print["Testing of nmer permutation set up only for numofnmers = 2, 3, of 4
				no test performed."];
];

fortranname=fortrannamesave;
(* no output *)
];






countuniquepolynomials[group_]:=Module[
{count,polys},
polys={};
Do[(
Do[(
	polys=Append[polys,group[[i,j]]];
	),{j,1,Length[group[[i]]]}];
),{i,1,Length[group]}];
polys=Sort[DeleteDuplicates[polys]];
count=Length[polys];
{count,polys}
];





colorgroupswithdups::usage="
colors groups with dups to show in correctlimitcolor the ones that have 
the correct limiting behavior and in incorrectlimitcolor the ones that do not.
";
colorgroupswithdups[groupswithdups_,polydroplist_,correctlimitcolor_,incorrectlimitcolor_]:=Module[
{grpstab},
grpstab=groupswithdups;
correct={};
Do[( 
Do[( 
If[MemberQ[polydroplist,groupswithdups[[i,j]]],
grpstab[[i,j]]=Style[ToString[grpstab[[i,j]]],incorrectlimitcolor];
,
grpstab[[i,j]]=Style[ToString[grpstab[[i,j]]],correctlimitcolor];
correct=Append[correct,ToString[grpstab[[i,j]]]];
];
),{j,1,numofnmers!}];	
),{i,1,Length[groupswithdups]}];
Print["The number of unique polys with the correct limit is ",Length[DeleteDuplicates[correct]]];

grpstab
];







initializeevalmp::usage="
This is an initialization program that goes with 
convertpolytofunctofrij[...]
";
initializeevalmp[fortranname_]:=Module[
{print,xsum,pnumber,npoly,mnumber,nmono,nq,nvariables},
print=False;
{rijnamesnow,nvariables,nmono,npoly,nq}=
		GetDefinitionstoMathematicaFromFortranOutputPure[];
Print["{nmono,npoly,nq} = ",{nmono,npoly,nq}];
rijnames[[1]]=rijnamesnow; 
If[print,Print["Coordinates now in use:"]];
If[print,Print[rijnamesnow]];
xsum=Length[rijnamesnow];
dist=Table[0,{i,1,xsum}];  (* needs to be global *)
pnumber=npoly;
mnumber=nmono;
mlistpur={"m0"}; (* global *)
Do[( 
 mlistpur=Append[mlistpur,"m"<>ToString[i]];
),{i,1,mnumber}];
plistpur={"p0"}; (* global *)
Do[( 
 plistpur=Append[plistpur,"p"<>ToString[i]];
),{i,1,pnumber}];
{pnumber,mnumber,nq}
];







convertpolytofunctofx::usage="convertpolytofunctofrij[fortranname,polynumber]
given a fortran file that has instructions for calculating polynomials, this
program converts the definition of the polynomial given by polynumber into 
a function of internuclear distances, where x, the transformed distance, 
is replaced by rXXYY and where XX is the number of one atom and YY is the 
number of the other.  For example, the output might look like this:
p(982) = (r0512+r0612)*(r0709+r0809)*r1011
The program caalls initialieevalmp if it hasn't been already called.
";
convertpolytofunctofx[fortranname_,polynumbers_]:=Module[
{txt,EM,EP,nq,z,mnumber},
(* NB PPCharStringmono,mlistpur,PPCharStringpoly,plistpur are all global *)
(* Do we need to initialize? *)
If[!(ValueQ[PPCharStringpoly]&&ValueQ[PPCharStringmono]&&
	ValueQ[plistpur]&&ValueQ[mlistpur]),
{pnumber,mnumber,nq}=initializeevalmp[fortranname];
];

(*
ppcsptab=textconvert[PPCharStringpoly,"text","tablestar",DataDir];
(* put parenthesis around polys *)
Do[( 
ppcspadd={ppcsptab[[1]],ppcsptab[[2]]};
Do[( 
If[Take[ppcsptab[[i,j]],1]\[NotEqual]"p",
ppcspadd=Append[ppcspadd,ppcstbap[[i,j]]];
,
ppcspadd=Append[ppcspadd,"("];
ppcspadd=Append[ppcspadd,ppcstbap[[i,j]]];
ppcspadd=Append[ppcspadd,")"];
];
),{j,3,Length[ppcsptab[[i]]]}];
ppcsptab[[i]]=ppcspadd;
),{i,1,Length[ppcsptab]}];
PPCharStringpolyparen=textconvert[ppcsptab,"tablestar","text",DataDir];
*)


zz="";
Do[( 
Do[( 
ToExpression["Clear[x"<>ToString[i]<>"];"];
),{i,1,pnumber}];
EM=EvalMonoNew[PPCharStringmono,mlistpur];
               EM=Drop[EM,1];
               EP=EvalPolyNew[PPCharStringpoly,plistpur];
               EP=Drop[EP,1];
txttab=textconvert[StringReplace[StringReplace[
ToString[Simplify[FortranForm[EP[[npoly]]]]],
"("->"( "],")"->" )"],"text","tablestar",DataDir];
txttab=Flatten[txttab];
txt=textconvert[txttab,"tablestar","text",DataDir];
StringReplace[StringReplace[StringReplace[txt,"\n"->""],"+"->" + "],"-"->" - "];

txt=StringReplace[txt,"\n"->""];
(*Print[txt];
txt=StringJoin[textconvert[txt,"tablestar","text",DataDir]];
Print[txt];*)
txt=StringReplace[StringReplace[StringReplace[txt,"+"->" + "],"-"->" - "],"**"->"^"];
txt="p"<>ToString[npoly]<>" = "<>txt;

zz=zz<>txt<>";\n";
),{npoly,polynumbers}];
zz
];







convertpolytofunctofrij::usage-"convertpolytofunctofrij[fortranname,polynumber]
given a fortran file that has instructions for calculating polynomials, this
program converts the definition of the polynomial given by polynumber into 
a function of internuclear distances, where x, the transformed distance, 
is replaced by rXXYY and where XX is the number of one atom and YY is the 
number of the other.  For example, the output might look like this:
p(982) = (r0512+r0612)*(r0709+r0809)*r1011
The program caalls initialieevalmp if it hasn't been already called.
";
convertpolytofunctofrij[fortranname_,polynumbers_]:=Module[
{txt,EM,EP,nq,zz,li},
zz=convertpolytofunctofx[fortranname,polynumbers];
zz=StringReplace[zz,"("->" ( "];
zz=StringReplace[zz,")"->" ) "];
zz=StringReplace[zz,";"->" ;"];
zz=StringReplace[zz,"^"->" ^ "];
zztab=textconvert[zz,"text","tablestar",DataDir];

Do[(
li=Length[zztab[[i]]];
(*Print[zztab[[i]]];*)
Do[(
	If[Quiet[StringTake[zztab[[i,j]],1]]=="x",
	num=ToExpression[StringDrop[zztab[[i,j]],1]];	
	zztab[[i,j]]="x"<>ToString[num]<>"(r"<>ToString[rijnames[[1,num]]];
	zztab[[i,j]]=zztab[[i,j]]<>")";   
	];	
),{j,1,li}];
),{i,1,Length[zztab]}];
zz=textconvert[zztab,"tablestar","text",DataDir];
zz=StringReplace[zz," ** "->"^"];
zz=StringReplace[zz,"( "->"("];
zz=StringReplace[zz," )"->")"];

zz
];








CheckAppendReverseDerivativesInputs[]:=Module[
{passed,yn,mnumber, pnumber,qnumber,xnumber,mnumber1,pnumber1,qnumber1},
passed=True;
{xnumber,mnumber1,pnumber1,qnumber1}=Getxmpq[inputfname];
Print["Checking Reverse Derivatives template input"];
If[!ValueQ[natomsparent],Print["Fatal: natomsparent unassigned"];passed=False;];
If[!ValueQ[inputfname],Print["Fatal: fortranname unassigned"];passed=False;];
If[!FileExistsQ[inputfname],Print["Fatal: fortranname does not exist."];passed=False;];
If[!ValueQ[DataDir],Print["Fatal: DataDir unassigned"];passed=False;];
If[!DirectoryQ[DataDir],
Print["Fatal: DataDir does not exist."]; passed=False;];
If[!ValueQ[xtransform],Print["Fatal: xtransform unassigned"];passed=False;];
If[Length[xtransform]<xnumber,
	Print["Warning: dimension of xtransform is too small for number of variables in inputfname"];(*,
	Print["{Length[xtransform],xnumber} = ",{Length[xtransform],xnumber}];*)];
(*If[ValueQ[xtransform],
	If[!Checkxtransformvsinputfname[], passed=False];
];*)
If[!ValueQ[outputfname],
	Print["Fatal: outputfname unassigned"];passed=False;];
If[!passed,
	Print["Check of input found fatal flaws -- Aborting"];
Abort[];
,
Print["Input passed Checks -- Continuing"];
];
passed
];







CheckPolynomialAddingInputs[]:=Module[
{passed,yn,mnumber, pnumber,qnumber},
Print["Checking Polynomial Adding Inputs"];  (* corrected "Pruning" to "Adding" 17 Feb 2023 *)
passed=True;
If[!ValueQ[natomsparent],Print["Fatal: natomsparent unassigned"];passed=False;];
If[!ValueQ[natomsfrag],Print["Fatal: natomsfrag unassigned"];passed=False;];
If[!ValueQ[fortranname],Print["Fatal: fortranname unassigned"];passed=False;];
If[!ValueQ[fortrannamesave],Print["Fatal: fortrannamesave unassigned"];passed=False;];
If[!ValueQ[DataDir],Print["Fatal: DataDir unassigned"];passed=False;];
If[!ValueQ[permsym],Print["Fatal: permsym unassigned"];passed=False;];
If[!ValueQ[atoms],Print["Fatal: atoms unassigned"];passed=False;];
If[!ValueQ[purified],
	Print["Warning: purified unassigned, assuming default purified=False"];purified=False;];
If[!ValueQ[performrecompaction],
	Print["Warning: performrecompaction unassigned, assuming default performrecompaction=True"];performrecompaction=True;];
If[!ValueQ[xtransform],Print["Fatal: xtransform unassigned"];passed=False;];
(*If[!ValueQ[UseSequentialBuildupMethod],
	Print["Warning: UseSequentialBuildupMethod unassigned, assuming default UseSequentialBuildupMethod=True"];UseSequentialBuildupMethod=True;];*)
If[!ValueQ[ProvideDerivatives],
	Print["Warning: ProvideDerivatives unassigned, assuming default ProvideDerivatives=True"];ProvideDerivatives=True;];
If[!ValueQ[UseBatchDerivatives] && UseBatchDerivatives==True,
	Print["Warning: UseBatchDerivatives has been assigned as True, changing this to False"];UseBatchDerivatives=False;];
If[!ValueQ[UseRunTests],Print["Warning: UseRunTests unassigned, assuming default UseRunTests=True"];UseRunTests=True;];
If[!ValueQ[pesfile],Print["Fatal: pesfile unassigned"];passed=False;];
If[!ValueQ[ncoeffdesired],Print["Fatal: ncoeffdesired unassigned"];passed=False;];
If[!ValueQ[OutputFortranname],
	Print["Fatal: OutputFortranname unassigned"];passed=False;];
If[!ValueQ[OutputCptFilename],
	Print["Fatal: OutputCptFilename unassigned"];passed=False;];
{mnumber,pnumber,qnumber}=Getmpq[fortranname];
If[ncoeffdesired<=  pnumber,Print["Fatal: ncoeffdesired \[LessEqual]  pnumber"];passed=False;];
Clear[mnumber,pnumber,qnumber];
If[!DirectoryQ[DataDir],
Print["Fatal: DataDir does not exist."]; passed=False;];
If[fortrannamesave!=fortranname,
	Print["Fatal: fortranname and fortrannamesave are not the same."];passed=False;];
If[Sum[permsym[[1,i]],{i,1,Length[permsym[[1]]]}]!= natomsparent,
	Print["Fatal: permsym[[1]] is not consistent with natomsparent."];passed=False;];
If[nfragments==1 && Length[atoms[[1]]]!= natomsparent,
    Print[Print["Fatal: permsym[[1]] is not consistent with natomsparent."];];passed=False;];
If[!FileExistsQ[pesfile],Print["Fatal: pesfile does not exist."];passed=False;];
If[!FileExistsQ[fortranname],Print["Fatal: fortranname does not exist."];passed=False;];

If[FileExistsQ[OutputFortranname],
Print["See Dialog Box"];
yn=InputString[Style["Warning: the OutputCptFilename will be overwritten. \nIs this ok?: y/n",Red,Bold,FontSize->14]];
If[yn=="n", 
	Print["this will abort to allow change"];
	Abort[];
	,
	Print["Deleting OutputCptFilename and continuing"];
	DeleteFile[OutputCptFilename];
];
];
passed
];







CheckPolynomialPruningInputs[]:=Module[
{passed,yn,mnumber, pnumber,qnumber},
Print["Checking Polynomial Pruning Inputs"];
passed=True;
If[!ValueQ[natomsparent],Print["Fatal: natomsparent unassigned"];passed=False;];
If[!ValueQ[natomsfrag],Print["Fatal: natomsfrag unassigned"];passed=False;];
If[!ValueQ[fortranname],Print["Fatal: fortranname unassigned"];passed=False;];
If[!ValueQ[fortrannamesave],Print["Fatal: fortrannamesave unassigned"];passed=False;];
If[!ValueQ[DataDir],Print["Fatal: DataDir unassigned"];passed=False;];
If[!ValueQ[permsym],Print["Fatal: permsym unassigned"];passed=False;];
If[!ValueQ[atoms],Print["Fatal: atoms unassigned"];passed=False;];
If[!ValueQ[purified],
	Print["Warning: purified unassigned, assuming default purified=False"];purified=False;];
If[!ValueQ[performrecompaction],
	Print["Warning: performrecompaction unassigned, assuming default performrecompaction=True"];performrecompaction=True;];
If[!ValueQ[xtransform],Print["Fatal: xtransform unassigned"];passed=False;];
(*If[!ValueQ[UseSequentialBuildupMethod],
	Print["Warning: UseSequentialBuildupMethod unassigned, assuming default UseSequentialBuildupMethod=True"];UseSequentialBuildupMethod=True;];*)
If[!ValueQ[ProvideDerivatives],
	Print["Warning: ProvideDerivatives unassigned, assuming default ProvideDerivatives=True"];ProvideDerivatives=True;];
If[!ValueQ[UseBatchDerivatives] && UseBatchDerivatives==True,
	Print["Warning: UseBatchDerivatives has been assigned as True, changing this to False"];UseBatchDerivatives=False;];
If[!ValueQ[UseRunTests],Print["Warning: UseRunTests unassigned, assuming default UseRunTests=True"];UseRunTests=True;];
If[!ValueQ[pesfile],Print["Fatal: pesfile unassigned"];passed=False;];
If[!ValueQ[ncoeffdesired],Print["Fatal: ncoeffdesired unassigned"];passed=False;];
If[!ValueQ[OutputFortranname],
	Print["Fatal: OutputFortranname unassigned"];passed=False;];
If[!ValueQ[OutputCptFilename],
	Print["Fatal: OutputCptFilename unassigned"];passed=False;];
{mnumber,pnumber,qnumber}=Getmpq[fortranname];
If[ncoeffdesired>= pnumber,Print["Fatal: ncoeffdesired \[GreaterEqual] pnumber"];passed=False;];
Clear[mnumber,pnumber,qnumber];
If[!DirectoryQ[DataDir],
Print["Fatal: DataDir does not exist."]; passed=False;];
If[fortrannamesave!=fortranname,
	Print["Fatal: fortranname and fortrannamesave are not the same."];passed=False;];
If[Sum[permsym[[1,i]],{i,1,Length[permsym[[1]]]}]!= natomsparent,
	Print["Fatal: permsym[[1]] is not consistent with natomsparent."];passed=False;];
If[!FileExistsQ[pesfile],Print["Fatal: pesfile does not exist."];passed=False;];
If[!FileExistsQ[fortranname],Print["Fatal: fortranname does not exist."];passed=False;];
If[FileExistsQ[OutputFortranname],
Print["See Dialog Box"];
yn=InputString[Style["Warning: the OutputCptFilename will be overwritten. 
     Is this ok?: y/n",Red,Bold,FontSize->14]];
If[yn=="n", 
	Print["this will abort to allow change"];
	Abort[];
	,
	Print["Deleting OutputCptFilename and continuing"];
	DeleteFile[OutputFortranname];
	];
];
If[FileExistsQ[shortcutfname],
  Print["See Dialog Box"];
  yn=InputString[Style["Warning: shortcutfname exist(s). \n   Do you want to use it for the next calculation (input: use) 
   or delete it and calculate it anew (input: calculate)?",Red,Bold,FontSize->14]];
  If[yn=="calculate", 
	Print["Deleting mkeepfname and qkeepfname and continuing"];
	DeleteFile[shortcutfname];
	,
	Print["Continuing with previously calculated and stored shortcutfname"];
  ];
 ];
passed
];








CheckPurificationInputs[]:=Module[
{passed,yn,mnumber, pnumber,qnumber},
Print["Checking Polynomial Purification Inputs"];
passed=True;
If[!ValueQ[natomsparent],Print["Fatal: natomsparent unassigned"];passed=False;];
If[!ValueQ[numofnmers],Print["Fatal: numofnmers unassigned"];passed=False;];
If[ValueQ[numofnmers] && !(numofnmers==4 || numofnmers==3 || numofnmers==2),
	Print["Fatal: numofnmmers must currently be 2, 3 or 4"];passed=False;];
If[!ValueQ[natomsfrag],Print["Fatal: natomsfrag unassigned"];passed=False;];
If[!ValueQ[fortranname],Print["Fatal: fortranname unassigned"];passed=False;];
If[!ValueQ[fortrannamesave],Print["Fatal: fortrannamesave unassigned"];passed=False;];
If[!ValueQ[DataDir],Print["Fatal: DataDir unassigned"];passed=False;];
If[!ValueQ[xyzpermsfilename],Print["Fatal: xyzpermsfilename unassigned"];passed=False;];
If[!ValueQ[xyzpermsfilename],
	xyzperms=Partition[Import[xyzpermsfilename,"Table"],2+natomsparent];
	If[xyzperms[[1,1,1]]!=natomsparent,
		Print["Fatal: there is a mistake in xyzpermsfilename,
				     first argument not equal to natomsparent"];
		passed=False;
	];
];
If[!skipgroups&&!FileExistsQ[xyzpermsfilename],Print["Fatal: xyzpermsfilename cannot be found"];passed=False;];
If[!ValueQ[permsym],Print["Fatal: permsym unassigned"];passed=False;];
If[!ValueQ[atoms],Print["Fatal: atoms unassigned"];passed=False;];
If[!ValueQ[mkeepfname],Print["Comment: mkeepfname unassigned; will not be saved"];];
If[!ValueQ[qkeepfname],Print["Comment: qkeepfname unassigned; will not be saved"];];
(*If[!ValueQ[shortcutfname],Print["Comment: shortcutfname unassigned; will not be saved"];];*)
If[!ValueQ[skipcompaction],Print["Warning: skipcompaction unassigned, assuming False"];
	skipcompaction=False;];
If[!ValueQ[atomnames],Print["Fatal: atomnames unassigned"];passed=False;];
If[!ValueQ[usingpurecompactedinputfile],Print["Fatal: usingpurecompactedinputfile unassigned"];
	passed=False;];
(*If[!ValueQ[performrecompaction],
	Print["Warning: performrecompaction unassigned, assuming default performrecompaction=True"];performrecompaction=True;];*)
If[!ValueQ[xtransform],Print["Fatal: xtransform unassigned"];passed=False;];
(*If[!ValueQ[UseSequentialBuildupMethod],
	Print["Warning: UseSequentialBuildupMethod unassigned, assuming default UseSequentialBuildupMethod=True"];UseSequentialBuildupMethod=True;];*)
If[!ValueQ[ProvideDerivatives],
	Print["Warning: ProvideDerivatives unassigned, assuming default ProvideDerivatives=True"];ProvideDerivatives=True;];
	If[!ValueQ[AddFastForwardDerivatives],
	Print["Warning: AddFastForwardDerivatives unassigned, assuming default AddFastForwardDerivatives=False"];AddFastForwardDerivatives=False;];
	If[!ValueQ[AddReverseDerivatives],
	Print["Warning: AddReverseDerivatives unassigned, assuming default AddReverseDerivatives=False"];AddReverseDerivatives=False;];
If[!ValueQ[UseBatchDerivatives] && UseBatchDerivatives==True,
	Print["Warning: UseBatchDerivatives has been assigned as True, changing this to False"];UseBatchDerivatives=False;];
If[!ValueQ[UseRunTests],Print["Warning: UseRunTests unassigned, assuming default UseRunTests=True"];UseRunTests=True;];
(*If[!ValueQ[pesfile],Print["Fatal: pesfile unassigned"];passed=False;];*)
(*If[!ValueQ[ncoeffdesired],Print["Fatal: ncoeffdesired unassigned"];passed=False;];*)
If[ncoeffdesired!=0,Print["Adding not now implemented, assuming ncoeffdesired=0"];ncoeffdesired=0];
If[!ValueQ[OutputFortranname],
	Print["Fatal: OutputFortranname unassigned"];passed=False;];
If[!ValueQ[OutputCptFilename],
	Print["Fatal: OutputCptFilename unassigned"];passed=False;];
{mnumber,pnumber,qnumber}=Getmpq[fortranname];
If[ncoeffdesired>= pnumber,
	Print["Fatal: ncoeffdesired \[GreaterEqual] pnumber; addition is not currently available"];passed=False;];
If[!FileExistsQ[pesfile] && ncoeffdesired>=pnumber,
	Print["Fatal: pesfile does not exist and addition is called for"];passed=False;];
Clear[mnumber,pnumber,qnumber];
If[!DirectoryQ[DataDir],
Print["Fatal: DataDir does not exist."]; passed=False;];
If[fortrannamesave!=fortranname,
	Print["Fatal: fortranname and fortrannamesave are not the same."];passed=False;];
If[Sum[permsym[[1,i]],{i,1,Length[permsym[[1]]]}]!= natomsparent,
	Print["Fatal: permsym[[1]] is not consistent with natomsparent."];passed=False;];
If[Length[atomnames]!=natomsparent,
Print["Warning: atomnames length is not consistent with natomsparent"];];
If[!FileExistsQ[fortranname],Print["Fatal: fortranname does not exist."];passed=False;];
If[FileExistsQ[OutputCptFilename],
Print["See Dialog Box"];
yn=InputString[Style["Warning: the OutputCptFilename will be deleted and re-created. 
Is this ok?: y/n",Red,Bold,FontSize->14]];
If[yn=="n", 
	Print["this will abort to allow change"];
	Abort[];
	,
	Print["Deleting OutputCptFilename and continuing"];
	DeleteFile[OutputCptFilename];
	];
];
If[Or[FileExistsQ[mkeepfname],FileExistsQ[qkeepfname]],
Print["See Dialog Box"];
  yn=InputString[Style["Warning: mkeepfname and/or qkeepfname exist(s). \n   Do you want to use them for the next calculation (input: use) \n   or to delete them and calculate them anew (input: calculate)?",Red,Bold,FontSize->14]];
  If[yn=="calculate", 
	Print["Deleting mkeepfname and qkeepfname and continuing"];
	DeleteFile[mkeepfname];
	DeleteFile[qkeepfname];
	,
	Print["Continuing with previously calculated and stored mkeepfname and qkeepfname"];
  ];    
];
If[!passed,
	Print["Check of input found fatal flaws -- Aborting"];
Abort[];
,
Print["Input passed Checks -- Continuing"];
];
passed
];





(* ::Input::Initialization:: *)
reorderwaterdata4[symin_,symout_,datasetin_]:=Module[
{dset,store,rec,choices,nwater,recsize,
HA1,HA2,HA3,HA4,HB1,HB2,HB3,HB4,O1,O2,O3,O4},
dset=datasetin;
nwater=4;
recsize=14;
store=Table[0,{k,1,3nwater}];
rec=Table[0,{j,1,recsize}];
If[symin!="HHOHHOHHOHHO", Print["Aborted - wrong symin"];Abort[];];
If[symout!= Or["4422","4242","6321","22221111"],
Print["Aborted - wrong symout"];Abort[];];
Do[(  (* over all 14 line records *)
If[symin=="HHOHHOHHOHHO",
(* isolate the ith record *)
Do[( 
rec[[j]]=dset[[i,j]];
),{j,1,recsize}];
(* store the initial water info in store *)
Do[( 
store[[k]]=rec[[2+k]]
),{k,1,3nwater}];
(* Drop the three velocity values *)
HA1=store[[1]];
HB1=store[[2]];
O1=store[[3]];
HA2=store[[4]];
HB2=store[[5]];
O2=store[[6]];
HA3=store[[7]];
HB3=store[[8]];
O3=store[[9]];
HA4=store[[10]];
HB4=store[[11]];
O4=store[[12]];
];

If[symout=="6321",
rec[[2+1]]=HA1;
rec[[2+2]]=HB1;
rec[[2+3]]=HA2;
rec[[2+4]]=HB2;
rec[[2+5]]=HA3;
rec[[2+6]]=HB3;
rec[[2+7]]=O1;
rec[[2+8]]=O2;
rec[[2+9]]=O3;
rec[[2+10]]=HB4;
rec[[2+11]]=HA4;
rec[[2+12]]=O4;
];

If[symout=="4422",
rec[[2+1]]=HA1;
rec[[2+2]]=HB1;
rec[[2+3]]=HA2;
rec[[2+4]]=HB2;
rec[[2+5]]=HA3;
rec[[2+6]]=HB3;
rec[[2+7]]=HA4;
rec[[2+8]]=HB4;
rec[[2+9]]=O1;
rec[[2+10]]=O2;
rec[[2+11]]=O3;
rec[[2+12]]=O4;
];

If[symout=="4242",
rec[[2+1]]=HA1;
rec[[2+2]]=HB1;
rec[[2+3]]=HA2;
rec[[2+4]]=HB2;
rec[[2+5]]=O1;
rec[[2+6]]=O2;
rec[[2+7]]=HA3;
rec[[2+8]]=HB3;
rec[[2+9]]=HA4;
rec[[2+10]]=HB4;
rec[[2+11]]=O3;
rec[[2+12]]=O4;
];

If[symout=="22221111",
rec[[2+1]]=HA1;
rec[[2+2]]=HB1;
rec[[2+3]]=HA2;
rec[[2+4]]=HB2;
rec[[2+5]]=HA3;
rec[[2+6]]=HB3;
rec[[2+7]]=HA4;
rec[[2+8]]=HB4;
rec[[2+9]]=O1;
rec[[2+10]]=O2;
rec[[2+11]]=O3;
rec[[2+12]]=O4;
];

(* replace the ith record with the permuted one *)
Do[( 
dset[[i,j]]=rec[[j]];
),{j,1,recsize}];


),{i,1,Length[dset]}];
dset
];



Checkxtransformvsinputfname[]:=Module[
{out,nvar,xttest,cont,infile,infilelines,ikeep},
out=True;
       infile=Import[inputfname,"Text"];
       infilelines=textconvert[infile,"text","textlines",DataDir];
ikeep=0;
nvar=(natomsparent)(natomsparent-1)/2;
Do[(
If[!StringContainsQ[infilelines[[i]],"!"] &&(
StringContainsQ[infilelines[[i]],"x(1) = "] || StringContainsQ[infilelines[[i]],"x(:) = "]),
ikeep=i;
If[StringContainsQ[infilelines[[ikeep]],"x(:) = "],
xttest=Table[1,{k,1,nvar}];
Goto[cont];
];
If[StringContainsQ[infilelines[[ikeep]],"x(1) = "],
(*Print["x(1) = was found. ikeep =",ikeep];*)
xttest=Table[0,{k,1,nvar}];
Do[(
(*Print["k = ",k];*)
If[StringContainsQ[infilelines[[k]],"/a"],
xttest[[k-ikeep+1]]= 1;
			(*Print["xttest[[k]]= 1"];*)
,
If[StringContainsQ[infilelines[[k]],"/x"],
xttest[[k-ikeep+1]]=2;
(*Print["xttest[[k]]= 2"];*)
];
];
),{k,ikeep,ikeep+nvar-1}];
Goto[cont];
];
];	
),{i,1,Length[infilelines]}];
Print["Cannot run check of xtransform"];
Print["x(1) or x(:) definition not found"];
out=False;
Label[cont];
If[xttest!=xtransform,
	out=False;
	Print["Fatal:  Problem with xtransform."]; 
	Print["        xtransform is not the same in the template"]; 
	Print["        for appending reverse derivatives and the "];
	Print["        file in inputfname. This will cause errors;  "];
	Print["        the reverse and regular derivatives will not   "];
	Print["        be the same.  Please correct this problem.  "];
];
out
];
