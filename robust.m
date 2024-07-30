(* ::Package:: *)
SetDirectory[NotebookDirectory[]];
<<PauliAlgebra`
alg[n_]:=
Join[Flatten[Table[\[Sigma]@@ReplacePart[ConstantArray[0, n],Join[{j->1},Table[i->3,{i,j+1,j+k}],{j+k+1->1}]],{j,n-1},{k,0,n-1-j}]],Flatten[Table[\[Sigma]@@ReplacePart[ConstantArray[0,n],Join[{j->2},Table[i->3,{i,j+1,j+k}],{j+k+1->2}]],{j,n-1},{k,0,n-1-j}]],Flatten[Table[\[Sigma]@@ReplacePart[ConstantArray[0,n],Join[{j->1},Table[i->3,{i,j+1,j+k}],{j+k+1->2}]],{j,n-1},{k,0,n-1-j}]],Flatten[Table[\[Sigma]@@ReplacePart[ConstantArray[0,n], Join[{j->2}, Table[i->3, {i,j+1,j+k}], {j+k+1->1}]],{j,n-1},{k,0,n-1-j}]],Table[\[Sigma]@@ReplacePart[ConstantArray[0, n],{i-> 3}],{i,n}],Table[\[Sigma]@@ReplacePart[ConstantArray[0, n], Join[{j+1->1},Table[i->3,{i,1,j}]]],{j,0,n-1}],
Table[\[Sigma]@@ReplacePart[ConstantArray[0, n], Join[{j+1->2},Table[i->3,{i,1,j}]]],{j,0,n-1}],
Table[\[Sigma]@@ReplacePart[ConstantArray[0, n], Join[{n-j->1},Table[n-i->3, {i,0,j-1}]]],{j,0,n-1}],
Table[\[Sigma]@@ReplacePart[ConstantArray[0, n], Join[{n-j->2},Table[n-i->3, {i,0,j-1}]]],{j,0,n-1}],{\[Sigma]@@ConstantArray[3,n]}]
init[n_]:=Sum[\[Sigma]@@ReplacePart[ConstantArray[0,n],{i->3}],{i,n}];
Commutator[x_,y_]:=x\[CenterDot]y-y\[CenterDot]x
n=5;
gennum=2n^2+3n+1;
list=Flatten[Table[{k,l,Commutator[init[n],alg[n][[k]]\[CenterDot]alg[n][[l]]]//Simplify//Factor},{k,gennum},{l,gennum}],1];
list2=GatherBy[list,Not[Simplify[#[[3]]]===0]&][[2]];
list3={#[[1]],#[[2]],#[[3,2]],N[Arg[#[[3,1]]]]}&/@list2;
list4=GatherBy[list3,Sort[#[[3]],-#[[3]]]&];
list5=list4[[;;,;;,{1,2,4}]];
Export[ToString[StringForm["noncom`1`.mat",n]],list5];
