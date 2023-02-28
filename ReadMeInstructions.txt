Instructions for Mathematica Files

Put the following files in a Directory:

CleanStart.wl
DeleteDuplicatesv20.0.wl
PolynomialPruningAddingv10.0.wl
FastDerivativesv5.0.wl
CGS.m

Then, in each template, rename the location of the files to
point to the directory you used.

In either another or the same directory, put 
Templates.nb

and then make a sub-directory called Data.  In the sub-directory,
put 
bemsa222111_4.f90
bemsa111231_4.f90
bemsa3111111_ 3.f90
bemsa211111_ 3.f90
bemsa31111_ 3.f90
bemsa111111_ 3.f90
bemsa2221111_ 3.f90
bemsa22221111_3.f90

ds_MP2-haTZ_22221111_ 27456.xyz
Train_data _50000.xyz
PESgly_5929.dat
xyz24
3692.xyz

Then go to all templates and, where each of these is referenced,
change the directory and sub-directory to that which you assigned.

In the data sub-directory that you made, make a sub-sub-directory
called "results" and in it place these files:

DuplicatesDeletedbemsaWater3Body222111_ 4result.f90
PureCpt3BodyMixed222111_ 4result.f90
PureCpt3BodyMixed222111_ 4wRevresult.f90
DuplicatesDeletedbemsaEthano111231_ 4result.f90
DuplicatesDeletedbemsaEthano111231_ 4ncoef8895result.f90
DuplicatesDeletedbemsaEthano111231_ 4ncoef8895wFastresult.f90
DuplicatesDeletedbemsaEthano111231_ 4ncoef8895wRev.f90
DuplicatesDeletedbemsaNMA2fragRCtestresult.f90
DuplicatesDeletedbemsaNMA3fragS1result.f90
DuplicatesDeletedbemsaAcAc3frag777SChen1result.f90
DDGLY222111_ 4wRevresult.f90
DuplicatesDeletedbemsaGLY1fragChenDOrd3result.f90
DuplicatesDeletedbemsaGLY1fragChenDOrd3ncoeff2000result.f90

These "result" files are what should be produced by the templates,
except that the names produced will not have "result" at the end 
and they will be stored in the "data" directory.  