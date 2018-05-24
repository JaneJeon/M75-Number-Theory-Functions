#!/usr/bin/env wolframscript
(* ::Package:: *)

Smooth[X_,B_]:=Select[Range[2,X],(FactorInteger[#]//Last//First//LessEqualThan[B])&]//Length


Sieve[B_,n_]:=Select[Range[Sqrt[n]//Ceiling,2Sqrt[n]//Floor]^2-n,(FactorInteger[#]//Last//First//LessEqualThan[B])&]


Pollard[N_]:=TimeConstrained[For[a=2;j=2,True,j++,a=PowerMod[a,j,N];d=GCD[a-1,N];If[d>1,Return[d]]],5*60]


PrettyPollard[N_]:=Monitor[For[a=2;j=2,True,j++,a=PowerMod[a,j,N];d=GCD[a-1,N];If[d>1,Return[d]]],ProgressIndicator[j,{2,Sqrt[N]//Floor}]]//Timing


QSieve[n_,c1_,c2_]:=TimeConstrained[b=c1*Sqrt[Log[n]Log[Log[n]]]//Exp//Floor;base=b//PrimePi//Range//Prime;x={};y={};If[r=#^2-n;FactorInteger[r]//Last//First//LessEqualThan[b],AppendTo[x,#];AppendTo[y,r]]&/@Range[n//Sqrt//Ceiling,n^c2+(n//Sqrt//Ceiling)];zeroM=NullSpace[Table[Table[Mod[IntegerExponent[y[[i]],base[[j]]],2],{i,y//Length}],{j,base//Length}]];For[i=1,i<=Length[zeroM],i++,prodX=1;prodY=1;MapIndexed[If[#1!=0,prodX*=x[[#2]];prodY*=y[[#2]]]&,zeroM[[i]]];d=GCD[prodX[[1]]+Sqrt[prodY[[1]]],n];If[Mod[d,n]>1,Return[d]]],5*60]
