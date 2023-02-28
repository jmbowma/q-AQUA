(* ::Package:: *)

c=2.99792458*^10; (* cm/s *)


k=1.3806488*^-16; (* ergs / deg K *)


h=6.6260695729*^-27; (* ergs s *)


N0=6.0221413*^23;
Jpercal=4.1855;
wnperkcal=1000*Jpercal*1.*10^7/(h*c);
wnperkJ=1000*1.*10^7/(h*c);
kcalperwn=1./wnperkcal;
AperBohr= 0.5291772109217;
eVperHartree= 27.211;
wnperHartree=219474.63;
hcm=wnperHartree;
wnpereV=wnperHartree/eVperHartree;
kcalpereV=8066 h c N0/(10^7 *4.184*10^3);
hbar=h/(2 \[Pi]);
(* Put kB in units of wavenumbers per degree K *)
kB=k/(h c);
fsperau=(2.418884326505*10^(\[Minus]2));
me=9.109383701528*10^-28; (* g *)


