(* ::Package:: *)

ds=DateString[];
datestring=StringReplace[ds,{" "->"_",":"->"_"}];
stageDomain="jwst-docs-stage.stsci.edu";
publicDomain="jwst-docs.stsci.edu";
stageOPTION=False;
stringReplacementRules={"+"->" ","/"->": ","%3a"->":","%2c"->",","%28"->"(","%29"->")","%29"->"'"};
ignoreLinksWith={"?","#title-heading","~",".action"};
addit="https://"<>If[stageOPTION,stageDomain,publicDomain];
pahdge=addit<>"/";
samO=sample=pahdge<>"display/";
notebookDir=NotebookDirectory[];
If[Length@Kernels[]==0,LaunchKernels[]];
Off[General::stop];
ParallelEvaluate[Off[General::stop]];
WebFunction1[pagetreeDat_]:=Block[{sqd0,dat1Pre,extr0,dat2First0,dat2First},
sqd0=StringQ/@pagetreeDat;dat1Pre=StringDelete[Flatten[Extract[#,Position[StringContainsQ[#,"/display/"],True]]&[StringSplit[#,{"<",">"}]]&/@Extract[#1,Position[#2,True]]],{"a href=","\""}]&[pagetreeDat,sqd0];extr0=Extract[#,Position[StringContainsQ[#,"href"],True]]&[Flatten@#]&/@Extract[#1,Position[#2,False]]&[pagetreeDat,sqd0];
dat2First0=StringDelete[DeleteCases[Flatten[(Last/@StringSplit[#,"href="])&/@#],"\" \">"],{"</a>","\""}]&[extr0];
dat2First=Table[StringTake[#2[[k]],#1[[k]]],{k,Length@#2}]&[Flatten[Map[First,StringPosition[#,">"],{2}]]-1,#]&[dat2First0];
Join[dat1Pre,dat2First]
]
AnalyzeLinks[start_,links_,sample_,ignoreLinksWith_]:=
Module[
{tfs,nlinks,dels,brokenMaybe,posHashes,hashlinks,lensHashes,newhashlinks,alldots,sls},
tfs=StringContainsQ[links,sample];
nlinks=Extract[links,Position[tfs,True]];

dels=Position[Or@@#&/@Transpose@(StringContainsQ[nlinks,#]&/@#),True]&[ignoreLinksWith];
brokenMaybe=Extract[nlinks,dels];
nlinks=Delete[nlinks,dels];
nlinks=Delete[#,Position[StringTake[#,-1],"/"]]&[nlinks];sls=StringLength@sample;
nlinks=Delete[#,Position[StringTake[#,sls],_?(#!=sample&)]]&[nlinks];

hashlinks=Extract[#,posHashes=Position[StringContainsQ[#,"#"],True]]&[nlinks];
lensHashes=(StringLength/@#-Flatten@StringPosition[#,"#"][[All,All,1]])+1&[hashlinks];
newhashlinks=Table[StringDrop[#1[[k]],-#2[[k]]],{k,Length@#1}]&[hashlinks,lensHashes];
nlinks[[Flatten@posHashes]]=newhashlinks;
nlinks=DeleteDuplicates@nlinks;

alldots=Extract[#1,Position[StringContainsQ[StringDelete[#1,#2],"."],True]]&[nlinks,sample];
nlinks=Complement[nlinks,alldots];

AppendTo[alldox,{start,alldots}];
AppendTo[allhashlinks,{start,hashlinks}];
AppendTo[allmaybebroken,{start,brokenMaybe}];
AppendTo[directions,start\[DirectedEdge]#&/@nlinks];
AppendTo[linksWorked,start];
AppendTo[alllinksAppend,nlinks];
AppendTo[allBSLinks,{start,Extract[links,Position[tfs,False]]}];

alllinksAppend=DeleteDuplicates@Flatten@alllinksAppend;
]

TestSpaceFun[alldaspaces_,string_,spass_]:=Module[{tmpthins},
tmpthins=If[StringContainsQ[string,spass],Extract[alldaspaces,Position[StringContainsQ[#,spass]\[And](StringLength@spass<StringLength@#)&/@alldaspaces,True]],{}];
If[Length@tmpthins==0,StringContainsQ[string,spass],And[StringContainsQ[string,spass],Sequence@@(StringContainsQ[string,#]&/@tmpthins)]==False]]

PartitionFun[lengthTable_,array_]:=Module[{a,b,c,d},a=Accumulate[lengthTable];
b=a-lengthTable+1;c=Partition[Riffle[b,a],2];d=Table[c[[k,1]];;c[[k,2]],{k,Length[c]}];
Table[array[[d[[k]]]],{k,Length[d]}]]

commFun[ds_,num_,str_]:={{""},{"As of "<>ds<>" there are "<>ToString@num<>str},{""}}

commFun2[num_,str_]:={{""},{"There are "<>ToString[num]<>str}}

labeler[v_,{i_,j_},{ri_,cj_}]:=Placed[Row[{Style[ToString@v,Bold,14]},","],Center]

tmpfun[var_,str_]:=Flatten[(#[[Position[#,str][[All,1]]]]&[#]&/@var),1][[All,2]]

MonitorParallelMap[foo_,expr_,levelSpec:{_?IntegerQ}:{1},OptionsPattern[{ParallelMap,"print":>False}]]:=Module[{res,exprCount,progress=0,a,opts=Options[ParallelMap]},
SetSharedVariable[progress];
exprCount=Length@Level[expr,levelSpec];
a=First@AbsoluteTiming[Monitor[res=ParallelMap[(progress++;foo[#])&,expr,levelSpec,opts],Column[{ToString@progress<>" of "<>ToString@exprCount,ProgressIndicator[progress,{0,exprCount}]},Alignment->Center]]];
If[OptionValue["print"],Print[a]];
res]

XMLReduce[variable_]:=Module[{xmls={1},var=variable,xmlposes,fixedxmls},
While[xmls!={},
xmls=Cases[var,XMLElement[_,_,_],Infinity];
xmlposes=Position[var,#]&/@xmls;
fixedxmls=Cases[xmls,XMLElement[_,_,x_]->x];
var=ReplacePart[var,Thread[xmlposes->fixedxmls]];];
Map[StringJoin[Flatten[#]]&,var,{2}]]

ExtLinkFun[extLinksDatPre_,alllinksPull_]:=Module[{poses,seets,leets,var},
poses=Position[extLinksDatPre,_?(#!={}&),1];
{seets,leets}=Thread[Unevaluated[Extract[{alllinksPull,extLinksDatPre},poses]]];var=XMLReduce[leets];
{seets,var}]

WebCrawl[sample_,ignoreLinksWith_]:=Module[{checkdis={1},linkDatPre,failedPoses,start,links},
While[Length@checkdis!=0,
linkDatPre=ParallelMap[Check[Import[#,"Hyperlinks"],AppendTo[brokenlinks,#];]&,alllinksToCheck];
failedPoses=Position[linkDatPre,Null];
linkDatPre=Delete[linkDatPre,failedPoses];
alllinksToCheck=Delete[alllinksToCheck,failedPoses];
Table[
start=alllinksToCheck[[k]];links=DeleteDuplicates[linkDatPre[[k]]];
AnalyzeLinks[start,links,sample,ignoreLinksWith]
,{k,Length@alllinksToCheck}];
checkdis=alllinksToCheck=Complement[Flatten@alllinksAppend,Join[linksWorked,brokenlinks]];
]
]

shapesFun[shapesize_,pos_,k_]:=Module[{blackWidowShape={{0,0},{1,1},{0,1},{1,0}}},
Which[
k==5,Polygon[CirclePoints[N@#]*shapesize+ConstantArray[pos,#]]&[3],
k==3,Polygon[CirclePoints[N@#]*shapesize+ConstantArray[pos,#]]&[4],
k==2,Polygon[CirclePoints[N@#]*shapesize+ConstantArray[pos,#]]&[5],
k==8,Polygon[CirclePoints[N@#]*shapesize+ConstantArray[pos,#]]&[6],
k==7,Polygon[CirclePoints[N@#]*shapesize+ConstantArray[pos,#]]&[7],
k==6,Polygon[blackWidowShape*shapesize*1.5+ConstantArray[pos,Length@blackWidowShape]],
k==1,Disk[pos,shapesize*0.8],
k==4,Polygon[Sort[CirclePoints[N@#]]*shapesize+ConstantArray[pos,#]]&[4]]]
HyperlinkCheck[name_,link_]:=If[StringContainsQ[link," "]||link=="",link,Hyperlink[name,link]]
HyperlinkCheck[link_]:=If[StringContainsQ[link," "]||link=="",link,Hyperlink[link]]
URLPretty[urls_]:=HyperlinkCheck[StringReplace[StringDelete[#,If[StringContainsQ[#,"jwst-docs-stage.stsci.edu"],"https://jwst-docs-stage.stsci.edu/display/","https://jwst-docs.stsci.edu/display/"]],stringReplacementRules],#]&/@urls
BrenStringContainsQ[testString_,contain_,exceptions_]:=Module[{test1,test2},
test1=StringContainsQ[testString,contain];
test2=If[exceptions=={},False,Or@@(StringContainsQ[testString,#]&/@exceptions)];
If[test1,test2==False,False]]

ButtonFunPre[varr_,options_]:=Module[{var=ToExpression@varr},
Column@{Row@Flatten@Outer[Button[#,#2=#]&,#2,{#}],Style[Dynamic@#,Bold,16]}&[var,options]]
ButtonFun[var_,options_]:=Last[{Clear@#,ButtonFunPre[#,options]}&[var]]
selectionFun=With[{links=#1,insals=#2},Select[links,With[{onesplit=#},With[{link=#},And@@(StringContainsQ[link,#]&/@onesplit)]&]]&/@StringSplit/@insals]&;
edgeSelectionFun=With[{links=#1,insals=#2},Select[links,With[{onesplit=#},With[{link=#},Or@@{And@@(StringContainsQ[First@link,#]&/@onesplit),And@@(StringContainsQ[Last@link,#]&/@onesplit)}]&]]&/@StringSplit/@insals]&;

stringReplaceFun=StringReplace[StringDelete[#,#2],stringReplacementRules]&;
stringReplaceFunJustPage=StringReplace[FileNameTake@#,stringReplacementRules]&;
stringCheck=With[{nods=#},Position[Table[And@@(StringContainsQ[nods[[k]],#,IgnoreCase->True]&/@#),{k,Length@nods}],True]&/@(StringSplit/@#2)]&;

LinkTypeFun[testartPre_String,{contentLinkDatRulez_,relatedLinkDatRulez_}]:=Module[{testart,contentLinks,relatedLinks},
testart=ToLowerCase@testartPre;
contentLinks=testart/.contentLinkDatRulez;
relatedLinks=testart/.relatedLinkDatRulez;{Flatten@Complement[relatedLinks,contentLinks],Flatten@Complement[contentLinks,relatedLinks],Flatten@Intersection[contentLinks,relatedLinks]}
]
ClearAll[NodeAndEdgeSelection];
complementFun=Table[Complement[#[[kkk]],Flatten[Drop[#,kkk],1]],{kkk,Length@#}]&;
NodeAndEdgeSelection[nfdirections_,ins_,insin_,alldaspacesWhile_,spacesLook_,otherKeys_,colors_,othercolors_,noncolor_,stringReplacementRules_,includeNONs_,includeOtherSpaces_,samO_,otherKeysAll_]:=Module[{sub,excludedSpaces,nodes,allposes,otherposes,insEdges,nonINSEdges,allposesOther,subIn,subsub,datdat,nodesSelect,allPosesEver,prettyStrings,datTT,colorsUsedByGraph,primaryNodesPre,primaryNodes},

sub=DeleteDuplicates@Flatten[edgeSelectionFun[nfdirections,spacesLook]];
If[includeOtherSpaces,
excludedSpaces=Complement[nfdirections,DeleteDuplicates@Flatten[edgeSelectionFun[nfdirections,Drop[alldaspacesWhile,-1]]]];
sub=Join[sub,excludedSpaces];];
nodes=DeleteDuplicates@Flatten@{sub[[All,1]],sub[[All,2]]};
insEdges=DeleteDuplicates@Flatten@edgeSelectionFun[sub,insin];

subIn=If[
includeNONs,
nonINSEdges=Complement[sub,DeleteDuplicates@Flatten@edgeSelectionFun[sub,ins]];DeleteDuplicates@Join[insEdges,nonINSEdges],
insEdges];
subsub=DeleteDuplicates@Flatten[{#[[All,1]],#[[All,2]]}&[subIn]];
nodesSelect=nodes[[Flatten[Position[nodes,#]&/@subsub]]];
primaryNodes=Flatten[selectionFun[nodesSelect,insin],1];
{nodesSelect,subIn,primaryNodes}
]
NumShapeFun[justDaStrings_,alldaspacesWhile_,shaperules_]:=Module[{nums,k,posses},
nums=Flatten@Table[
posses=Position[StringContainsQ[justDaStrings[[k]],#]&/@alldaspacesWhile,True];
If[Length@posses==1,Last@posses,Last@Drop[posses,-1]],{k,Length@justDaStrings}];
nums/.shaperules
]
MyStringSortFun[keys_]:=Module[{turd2=keys,wierdTFs,tmp,poseThings,nozeros,subsetQs,tmp3,tmp4,num},
wierdTFs=Table[tmp=turd2;StringContainsQ[tmp[[k]]="";tmp,#[[k]]],{k,Length@#}]&[turd2];
poseThings=Map[Position[#,True]&,wierdTFs];
nozeros=Position[Length/@poseThings,_?(#!=0&)];
subsetQs=Extract[turd2,nozeros];
tmp3=Extract[turd2,#]&/@Flatten[poseThings[[#]]&/@nozeros,1];
tmp4=Reverse@Sort[Sort/@Flatten/@Transpose@{Partition[subsetQs,1],tmp3}];Table[num=First@Sort@Flatten[Position[turd2,#]&/@tmp4[[k]]];turd2=Flatten@Insert[Fold[DeleteCases[#1,#2]&,turd2,tmp4[[k]]],tmp4[[k]],num],{k,Length@tmp4}];
turd2
]
stupidStarSizeVar=10;
centerDotThing=Style["*",stupidStarSizeVar,White];
ClearAll@LegendFun
LegendFun[shaperules_,alldaspaces_,otherSpacesName_,instrumentsPre_,otherpagesCalled_,colors_,othercolors_]:=Module[{key1,things,shapekey,filter2keys,filter2keysFin,filter2colors,filter2,filter2Sub,arrowNames1Pre={"One Way","Two Way","Connection of Interest"},linktypeKeys={"Only in Related Links","Only in Body","Body and Related Links"},arrowNames1,arrows1,edgesKey1,arrows2,edgesKeys2,allColorsKey,edgesKey,alldataLegendThings,legendLengths,difs,fillers,alldataLegendThingsFin},
key1={Append[alldaspaces,otherSpacesName],shaperules[[All,2]]};things=EdgeDelete[Graph[{1->1},VertexShapeFunction->#,VertexSize->1,ImageSize->10],{1->1}]&/@Last@key1;shapekey=Row/@Transpose@{things," "<>#&/@First@key1};

filter2keys=Append[instrumentsPre,otherpagesCalled];
filter2keysFin=" "<>#&/@filter2keys;
filter2colors=Append[colors,noncolor];
filter2=Row/@Transpose@{filter2colors,filter2keysFin};
filter2Sub=Row/@Transpose@{othercolors," "<>#&/@otherKeysPre};
arrowNames1=" "<>#&/@arrowNames1Pre;

arrows1={Graphics[{Gray,Arrowheads[20*arrowsize],Thickness[40*thicknessedge],Arrow[{{0,0},{1,0}}]},ImageSize->Tiny,PlotRangePadding->0,AspectRatio->1/10],
Graphics[{Darker@Red,Arrowheads[20*arrowsize],Thickness[80*thicknessedge],Line[{{0,0},{1,0}}]},ImageSize->Tiny,PlotRangePadding->0,AspectRatio->1/10],
Graphics[{Gray,dashes1,Arrowheads[20*arrowsize],Thickness[40*thicknessedge],Arrow[{{0,0},{1,0}}]},ImageSize->Tiny,PlotRangePadding->0,AspectRatio->1/10]};edgesKey1=Row/@Transpose@{arrows1,arrowNames1};


arrows2={Graphics[{Blue,dashes1,Arrowheads[20*arrowsize],Thickness[40*thicknessedge],Arrow[{{0,0},{1,0}}]},ImageSize->Tiny,PlotRangePadding->0,AspectRatio->1/10],Graphics[{Darker[Red],dashes2,Arrowheads[20*arrowsize],Thickness[40*thicknessedge],Arrow[{{0,0},{1,0}}]},ImageSize->Tiny,PlotRangePadding->0,AspectRatio->1/10]
,Graphics[{Black,Arrowheads[20*arrowsize],Thickness[80*thicknessedge],Arrow[{{0,0},{1,0}}]},ImageSize->Tiny,PlotRangePadding->0,AspectRatio->1/10]
};
edgesKeys2=Row/@Transpose@{arrows2," "<>#&/@linktypeKeys};
allColorsKey=Flatten@{filter2,"=====",filter2Sub};
edgesKey=Flatten@{Style["Simple Edges",Bold],edgesKey1,"\n",Style["Related vs. Content Links Edges",Bold],edgesKeys2};

alldataLegendThings={shapekey,allColorsKey,edgesKey};
legendLengths=Length/@alldataLegendThings;
difs=Max@legendLengths-legendLengths;
fillers=ConstantArray["",#]&/@difs;
alldataLegendThingsFin=Table[Join[alldataLegendThings[[k]],fillers[[k]]],{k,Length@fillers}];Grid[{Style[#,Bold]&/@{"Shapes","Colors","Edges"},Column/@alldataLegendThingsFin},Frame->All,Background->Lighter[LightBlue,0.7]]
]

InitPre[]:=Module[{},
printMostPages=False;cutArb=500;cutArbitrary=True;
justspacesLower=StringDelete[checkSpaces,"/"];
pagesToChange=StringInsert[justspacesLower,samO,1];
pageRulesToChange=Thread[pagesToChange->StringInsert[pagesToChange,"/",-1]];
alllinksLower=alllinksLower/.pageRulesToChange;
contentLinksPre2=contentLinksPre/.pageRulesToChange;
relatedLinkDatPre2=relatedLinkDatPre/.pageRulesToChange;
contentLinkDatRulez=linkRuleFun[contentLinksPre2];
relatedLinkDatRulez=linkRuleFun[relatedLinkDatPre2];
allRLvsCLDatPre=Flatten/@Transpose[With[{link=#},Map[link\[DirectedEdge]#&,LinkTypeFun[link,{contentLinkDatRulez,relatedLinkDatRulez}],{2}]]&/@alllinksLower];
fdirectionsGraph=Delete[#,emptyconnections=Position[#[[All,2]],{}]]&[allconnects=DeleteDuplicates[Flatten@Join[contentLinksPre2,relatedLinkDatPre2]]];
empties=DeleteDuplicates@Extract[allconnects,emptyconnections][[All,1]];
mostPre=Reverse[Sort[Reverse/@Tally[fdirectionsGraph[[All,2]]]]];mostNodes=mostPre[[All,1]];maxnode=mostPre[[1,1]];cut=If[cutArbitrary,Length@Position[mostNodes,_?(#>=cutArb&)],Length@Position[mostNodes,_?(#>=maxnode&)]];
mostest=mostPre[[1;;cut]];
If[printMostPages,Print[mostest//TableForm];
Print[mostPre[[cut+1;;cut+cut]]//TableForm];];
nfdirections=If[disregardpop,Select[fdirectionsGraph,Not@MemberQ[mostest[[All,2]],#[[2]]]&],fdirectionsGraph];

possibleNodes=DeleteDuplicates@Join[#[[All,1]],#[[All,2]]]&[nfdirections];
allnodesEverInTheUniverse=DeleteDuplicates@Join[#[[All,1]],#[[All,2]]]&[nfdirections(*fdirections*)];
articlesWithNoIncomingLinks=Complement[allnodesEverInTheUniverse,possibleNodes];

];
ClearAll@InitOncePre
InitOncePre[{otherKeysPre_,instrumentsPre_,alldaspacesPre_,spaceTransferOfInterestPre_},othercolorsPre_,colorpalette_,newshapePrefs_,alltheShapes_,fdirectionsGraph_,disregardpop_,otherSpacesName_,otherpagesCalled_,alllinksLower_,imsize_,ds_]:=Module[{lengthColors,lengthOtherColors,uno,dos,maxShapes,colors,othercolors,mostPre,mostNodes,maxnode,cut,mostest,dasta,inslinks,stoopid,spacestmpins,b2dat,orders,possibleconnectionsPre,diffs,sames,possibleconnections,check,alldaspaces,ins,otherKeysAll,spaceTransferOfInterestPre2,alldaspacesWhile,shaperules,nfdirections,b1,b2,legend,genplots=False},

lengthColors=Length@colorpalette;
lengthOtherColors=Length@othercolorsPre;

If[Length@otherKeysPre>lengthOtherColors,Print["Aborted! You have too many options for otherKeysPre - only "<>ToString@lengthOtherColors<>" options allowed"];Abort[];];
If[Length@instrumentsPre>lengthColors,Print["Aborted! You have too many options for instrumentsPre - only "<>ToString@lengthColors<>" options allowed"];Abort[];];
alldaspaces=MyStringSortFun@ToLowerCase@alldaspacesPre;
otherKeysAll=ToLowerCase@otherKeysPre;
ins=ToLowerCase@instrumentsPre;

maxShapes=Length@alldaspaces+1;
shaperules=Thread[Range@maxShapes->alltheShapes[[Take[newshapePrefs,maxShapes]]]];


colors=colorpalette[[1;;Length@ins]];
othercolors=othercolorsPre[[1;;Length@otherKeysAll]];
legend=LegendFun[shaperules,alldaspaces,otherSpacesName,instrumentsPre,otherpagesCalled,colors,othercolors];


alldaspacesWhile=Append[alldaspaces,_];

If[genplots,
dasta=Reverse@Sort@Transpose@{Count[StringContainsQ[#<>"/"&/@alllinksLower,#],True]&/@alldaspaces,StringDelete[alldaspaces,"/"]};
b1=BarChart[dasta[[All,1]],ChartLabels->dasta[[All,2]],Frame->True,FrameLabel->(Style[#,16]&/@{"Space","Number of Articles"}),PlotLabel->Style[StringDrop[ds,-9]<>" - "<>ToString@Length@alllinksLower<>" Total Articles",18],PlotTheme->"Detailed",LabelingFunction->labeler,ImageSize->imsize,FrameTicksStyle->14];

inslinks=Extract[alllinksLower,#]&/@stringCheck[alllinksLower,ins];
stoopid=Map[FileNameSplit,inslinks,{2}][[All,All,5]];
spacestmpins=DeleteDuplicates@Flatten@stoopid;
b2dat=Transpose@Table[Map[Count[#,spacestmpins[[k]]]&,stoopid],{k,Length@spacestmpins}];orders=Reverse@Ordering@First@b2dat;
uno=#[[orders]]&/@b2dat;
dos=#[[orders]]&/@ConstantArray[spacestmpins,Length@inslinks];
b2=BarChart[uno,ChartLabels->{Style[#,18]&/@StringReplace[ins," "->"\n"],Style[#,9]&/@ToUpperCase/@Flatten@dos},Frame->True,PlotTheme->"Detailed",ImageSize->imsize,FrameTicksStyle->16,LabelingFunction->labeler,FrameLabel->{"",Style["Number of Articles",18]},PlotLabel->Style[StringDrop[ds,-9],22]];];

possibleconnectionsPre=Join[Subsets[Sort@#,{2}],Transpose@{#,#}]&[alldaspaces];
diffs=Select[possibleconnectionsPre,#[[1]]!=#[[2]]&];
sames=Complement[possibleconnectionsPre,diffs];
possibleconnections=Join[diffs,sames];


spaceTransferOfInterestPre2=Map[StringTrim,spaceTransferOfInterestPre,{2}];
check=And@@{And@@(Flatten@Map[StringQ,spaceTransferOfInterestPre2,{2}]),DeleteDuplicates[Length/@spaceTransferOfInterestPre2]=={2}};
If[Not@check,PopUpFun["Something is wrong with the syntax for the edge connection terms."];Abort[];];

{{alldaspaces,ins,otherKeysAll,spaceTransferOfInterestPre2},{alldaspacesWhile,shaperules,colors,othercolors},{b1,b2,legend}}
]
ClearAll@InitOnce
InitOnce[]:=Module[{},

{{alldaspaces,ins,otherKeysAll,spaceTransferOfInterestPre2},{alldaspacesWhile,shaperules,colors,othercolors},{b1,b2,legend}}=InitOncePre[{otherKeysPre,instrumentsPre,alldaspacesPre,spaceTransferOfInterestPre},othercolorsPre,colorpalette,newshapePrefs,alltheShapes,fdirectionsGraph,disregardpop,otherSpacesName,otherpagesCalled,alllinksLower,imsize,ds];
]
ClearAll@GenGraphData
GenGraphData[insin_,spacesLook_,spaceTransferOfInterest_,includeOtherSpaces_,includeNONs_,otherKeys_]:=
Module[{},
{nodesSelect,subIn,primaryNodes}=NodeAndEdgeSelection[nfdirections,ins,insin,alldaspacesWhile,spacesLook,otherKeys,colors,othercolors,noncolor,stringReplacementRules,includeNONs,includeOtherSpaces,samO,otherKeysAll];{inputNodes,vLabelsFin}=AddShapesToNodes[nodesSelect,primaryNodes,alldaspacesWhile,shaperules,otherKeys,ins,samO,colors,noncolor,othercolors,otherKeysAll];
{toolTippedEdgesSimple,allInputEdges}=EdgeDesignFunO[subIn,spaceTransferOfInterest,{dashes,arrowsize,thicknessedge,biggerThickness}];
{toolTippedEdges,allEdges}=RLvsCLEdges[allRLvsCLDatPre,subIn,{arrowsize,thicknessedge}];
]
columnClicks=Column@{Row[{Button["Reset Sample",sample={}],"  ",Dynamic@TableForm@(HyperlinkCheck[#[[1]],#[[2]]]&/@Transpose@{stringReplaceFun[#,samO],#}&[sample])}],"=====================",
Row[{Button["Reset Edge Sample",edgies={}],"  ",Dynamic@TableForm@DeleteDuplicates@edgies}]};
QueryJDoxArticles[var_]:=Extract[#,Flatten[stringCheck[#,{var}]&[#],1]]&[alllinksLower]
ClearAll@SubPrep
SubPrep[sampleIn_,allInputEdges_,inputNodesTXT_,centerDotThing_,samO_]:=Module[{allposes,nodePoses,smaplePretty,vLabels,vLabelsMini,edgesTXT},
edgesTXT=Map[Take[Level[#,{-1}],2]&,allInputEdges];
allposes=With[{edges=#},Sort@DeleteDuplicates@Flatten[Position[edges,#]&/@sampleIn,1][[All,1]]]&[edgesTXT];
nodePoses=Sort@DeleteDuplicates@Flatten[Position[inputNodesTXT,#]&/@DeleteDuplicates@Flatten@edgesTXT[[allposes]]];
smaplePretty=Style[#,22]&/@stringReplaceFun[sample,samO];
vLabels=Table[Placed[Button[Tooltip[centerDotThing,#2],AppendTo[sample,#];sample=DeleteDuplicates@sample;,Appearance->None]&[sample[[k]],smaplePretty[[k]]],Center],{k,Length@sample}];
vLabelsMini=Thread[sample->vLabels];
{nodePoses,smaplePretty,vLabelsMini}
]
SubPrepEdges[sample_,toolTippedEdgesSimple_,allInputEdges_]:=Module[{allposeys,nodePoses,vLabelsMini,smaplePretty,vLabels,edgesTXTSPE},
edgesTXTSPE=Map[Take[Level[#,{-1}],2]&,{toolTippedEdgesSimple,allInputEdges},{2}];
allposeys=With[{edges=#},Sort@DeleteDuplicates@Flatten[Position[edges,#]&/@sample,1][[All,1]]]&[#]&/@edgesTXTSPE;
allposeys]
SubGraphData[sampleIn_]:=Module[{},
{nodePoses,smaplePretty,vLabelsMini}=SubPrep[sampleIn,allInputEdges,inputNodesTXT,centerDotThing,samO];
allposeysSimple=SubPrepEdges[sampleIn,toolTippedEdgesSimple,allInputEdges];
allposeysFancy=SubPrepEdges[sampleIn,toolTippedEdges,allEdges];
]
ClearAll@PartitionWithLeftOver
PartitionWithLeftOver[varToPartition_,partitionNum_]:=Module[{partitionOutPut,takeLastNum,leftOver,len1},
len1=Length@varToPartition;
partitionOutPut=Partition[varToPartition,partitionNum];
If[Divisible[len1,partitionNum],partitionOutPut,
takeLastNum=len1-Length@Flatten@partitionOutPut;
leftOver=Take[varToPartition,-takeLastNum];
Append[partitionOutPut,leftOver]
]
]
stupidStarSizeVar=10;
centerDotThing=Style["*",stupidStarSizeVar,White];
ClearAll[AddShapesToNodes];
AddShapesToNodes[nodesSelect_,primaryNodes_,alldaspacesWhile_,shaperules_,otherKeys_,ins_,samO_,colors_,noncolor_,othercolors_,otherKeysAll_]:=
Module[
{justDaStrings,nums,k,shapesForNodes,inputNodes,allposesOther,allposes,otherposes,allPosesEver,prettyStrings,datTT,colorsUsedByGraph,datdat,datdatPre,justDaStringsPre,priNodePoses,ordersForNodes,justDaStringsPretty,lenDif,one,two,vLabels,vLabelsFin},

colorsUsedByGraph=Join[colors,{noncolor},othercolors[[Flatten[Position[otherKeysAll,#]&/@otherKeys]]]];
allposesOther=complementFun@stringCheck[nodesSelect,otherKeys];
allposes=complementFun[With[{poseys=#,allposs=#2},Complement[#,poseys]&/@allposs]&[Flatten[allposesOther,1],stringCheck[nodesSelect,ins]]];
otherposes=Partition[Complement[Range@Length@nodesSelect,Flatten@Join[allposes,allposesOther]],1];
allPosesEver=Join[allposes,{otherposes},allposesOther];
datdatPre=Flatten@Table[Style[#,colorsUsedByGraph[[k]]]&/@Extract[nodesSelect,allPosesEver[[k]]],{k,Length@allPosesEver}];

justDaStringsPre=ToString/@datdatPre[[All,1]];
priNodePoses=Flatten[Position[justDaStringsPre,#]&/@primaryNodes];

ordersForNodes=DeleteDuplicates@Join[Complement[Range@Length@justDaStringsPre,priNodePoses],priNodePoses];
datdat=datdatPre[[ordersForNodes]];
justDaStrings=justDaStringsPre[[ordersForNodes]];
justDaStringsPretty=Style[#,22]&/@stringReplaceFun[justDaStrings,samO];

lenDif=Length@datdat-Length@primaryNodes;
shapesForNodes=NumShapeFun[justDaStrings,alldaspacesWhile,shaperules];

inputNodes=Table[Button[Tooltip[Property[#,VertexShapeFunction->#2],#4],AppendTo[sample,#3];sample=DeleteDuplicates@sample;]&[datdat[[k]],shapesForNodes[[k]],justDaStrings[[k]],justDaStringsPretty[[k]]],{k,Length@datdat}];
inputNodesTXT=First@Level[#,{-1}]&/@inputNodes;
posses=With[{thing=#},Flatten[Position[thing,#]&/@#2]]&[inputNodesTXT,primaryNodes];
primFancyNodesInt=inputNodesTXT[[posses]];
vLabels=Table[Placed[Button[Tooltip[centerDotThing,#2],AppendTo[sample,#];sample=DeleteDuplicates@sample;,Appearance->None]&[justDaStrings[[k]],justDaStringsPretty[[k]]],Center],{k,lenDif+1,Length@datdat}];
vLabelsFin=Thread[primaryNodes->vLabels];
{inputNodes,vLabelsFin}
]
ClearAll@EdgeDesignFunO
EdgeDesignFunO[subIn_,spaceTransferOfInterest_,{dashes_,arrowsize_,thicknessedge_,biggerThickness_}]:=Module[{ints,tints,newEdgesPre,oneways,otherways,memberedOtherWays,newEdgesPre2,newEdges,justedgedat,likeSpacesOfInterest,diffSpacesOfInterest,allTFs,rightones={{{False,True},{True,False}},{{True,True},{True,True}},{{True,False},{True,True}},{{False,True},{True,True}}},allExtractPoses,edgeToolTips1,edgeToolTips2,onewayNEs,twowayNEs,toolTippedEdgesSimplePre,onlyedges,oneway,twoway,dashedEdges1,dashedEdges2,(*dashedEdges,*)solidEdges,(*solidsOneWay,solidsTwoWay,*)allInputEdges,toolTippedEdgesSimple,justedgeDatNosamO},

ints=Intersection[subIn,Reverse/@subIn];
tints=DeleteDuplicates[Sort[#[[1]]<->#[[2]]]&/@ints];
newEdgesPre=Join[tints,Complement[subIn,ints]];

oneways=EdgeList[newEdgesPre,_\[DirectedEdge]_];
otherways=Reverse/@oneways;
memberedOtherWays=Intersection[otherways,nfdirections];
newEdgesPre2=Join[Thread[memberedOtherWays[[All,1]]<->memberedOtherWays[[All,2]]],Complement[newEdgesPre,Reverse/@memberedOtherWays]];newEdges=Fold[DeleteCases[#1,#2]&,newEdgesPre2,Select[newEdgesPre2,#[[1]]==#[[2]]&]];

justedgedat={newEdges[[All,1]],newEdges[[All,2]]};
likeSpacesOfInterest=Select[spaceTransferOfInterest,#[[1]]==#[[2]]&];
diffSpacesOfInterest=Select[spaceTransferOfInterest,#[[1]]!=#[[2]]&];
justedgeDatNosamO=Map[StringDelete[#,samO]&,justedgedat];
allTFs=Map[With[{spaceTrans=#},Sort/@Transpose@(With[{aList=#},Transpose[(And@@#&/@(Transpose@(StringContainsQ[aList,#]&/@StringSplit@#)))&/@spaceTrans]]&/@justedgeDatNosamO(*justedgedat*))]&,{diffSpacesOfInterest,likeSpacesOfInterest},{2}];
allExtractPoses=Partition[Flatten[Position[allTFs,#][[All,3]]&/@rightones],1];

edgeToolTips1=Thread[#[[1]]\[DirectedEdge]#[[2]]]&[Map[stringReplaceFunJustPage@#&,{#[[All,1]],#[[All,2]]},{2}]]&[onewayNEs=EdgeList[newEdges,_\[DirectedEdge]_]];
edgeToolTips2=Thread[#[[1]]<->#[[2]]]&[Map[stringReplaceFunJustPage@#&,{#[[All,1]],#[[All,2]]},{2}]]&[twowayNEs=EdgeList[newEdges,_<->_]];
toolTippedEdgesSimple=Table[Tooltip[#[[k]],Style[#2[[k]],18]],{k,Length@#}]&[Join[onewayNEs,twowayNEs],Join[edgeToolTips1,edgeToolTips2]];(*toolTippedEdgesSimple=Table[Button[#,AppendTo[edgies,HyperlinkCheck[stringReplaceFunJustPage@#,#]&/@First@#];edgies=DeleteDuplicates@edgies;]&[toolTippedEdgesSimplePre[[k]]],{k,Length@toolTippedEdgesSimplePre}];*)

onlyedges=Extract[toolTippedEdgesSimple,allExtractPoses];
oneway=EdgeList[onlyedges,_\[DirectedEdge]_];
twoway=EdgeList[onlyedges,_<->_];

dashedEdges1=Thread[oneway->Directive[dashes,Arrowheads[arrowsize],Thickness[2*thicknessedge]]];
dashedEdges2=Thread[twoway->Directive[Darker[Red],Thickness[biggerThickness],dashes,Arrowheads[arrowsize]]];dashedEdges=Join[dashedEdges1,dashedEdges2];solidEdges=Complement[toolTippedEdgesSimple,onlyedges];
solidsOneWay=Thread[EdgeList[solidEdges,_\[DirectedEdge]_]->Directive[Gray,Arrowheads[arrowsize],Thickness[2*thicknessedge]]];
solidsTwoWay=Thread[EdgeList[solidEdges,_<->_]->Directive[Darker[Red],Thickness[biggerThickness]]];
delEdges=Join[solidsOneWay,solidsTwoWay][[All,1]];
allInputEdges=Join[solidsOneWay,solidsTwoWay,dashedEdges];
{toolTippedEdgesSimple,allInputEdges}
]
ClearAll@GenAllGraphs
GenAllGraphs[imsize_,vertexSize_]:=Module[{vcoords,(*delEdges,*)(*posses,*)(*primFancyNodesInt,*)zeroDegreeNodes,nodesDel,(*delPosesFin,*)zeroPoses,(*fZeroPoses,*)vCoordsOfInterest,subInJustEdgesOfInterest,vertexKeepPoses,keptNodes,toolTippedEdgesOfInterest,allEdgeDesignOfInterest,g3,g4,one,twoWays},g1=Graph[inputNodes,toolTippedEdgesSimple,EdgeStyle->allInputEdges,ImageSize->imsize,VertexSize->vertexSize,VertexLabels->vLabelsFin];vcoords=PropertyValue[{g1,#},VertexCoordinates]&/@VertexList[g1];g2=Graph[inputNodes,toolTippedEdges,EdgeStyle->allEdges,ImageSize->imsize,VertexSize->vertexSize,VertexCoordinates->vcoords,VertexLabels->vLabelsFin];
If[dashedEdges=={},Clear[delEdges,delPosesFin,fZeroPoses];g3pre=g5=g6="No Edges of Interest To Show!";,
g3pre=EdgeDelete[Graph[inputNodes,toolTippedEdgesSimple,EdgeStyle->allInputEdges,ImageSize->imsize,VertexSize->vertexSize,VertexLabels->vLabelsFin,VertexCoordinates->vcoords],delEdges];
zeroDegreeNodes=Extract[inputNodesTXT,zeroPoses=Position[VertexDegree[g3pre],0]];
nodesDel=Complement[zeroDegreeNodes,primFancyNodesInt];
delPosesFin=Flatten[Position[inputNodesTXT,#]&/@nodesDel];
fZeroPoses=Flatten@zeroPoses;
g3=VertexDelete[g3pre,inputNodesTXT[[delPosesFin]]];(*same vertex locations as g1, shows isolated nodes with white star*)
g4=VertexDelete[g3pre,inputNodesTXT[[fZeroPoses]]];(*same graph as g3 but no isolated nodes show*)
g5=VertexDelete[EdgeDelete[g1,delEdges],inputNodesTXT[[delPosesFin]]];(*g3 graph recalculated with no predetermined vertex coordinates*)
vCoordsOfInterest=PropertyValue[{g5,#},VertexCoordinates]&/@VertexList[g5];twoWays=EdgeList[g5,_<->_];one=Thread[twoWays[[All,1]]\[DirectedEdge]twoWays[[All,2]]];
subInJustEdgesOfInterest=Join[one,Reverse/@one,EdgeList[g5,_\[DirectedEdge]_]];
vertexKeepPoses=Complement[Range@Length@inputNodes,delPosesFin];
keptNodes=inputNodes[[vertexKeepPoses]];{toolTippedEdgesOfInterest,allEdgeDesignOfInterest}=RLvsCLEdges[allRLvsCLDatPre,subInJustEdgesOfInterest,{arrowsize,thicknessedge}];g6=Graph[keptNodes,toolTippedEdgesOfInterest,EdgeStyle->allEdgeDesignOfInterest,ImageSize->imsize,VertexSize->vertexSize,VertexCoordinates->vCoordsOfInterest,VertexLabels->vLabelsFin];];
]
ClearAll@StringParser
StringParser[stringText_]:=Module[{phrasesPre,phrases,indWords,allSearches,paddedStringText},
phrasesPre=StringCases[#,Shortest["\""~~__~~"\""]]&[stringText];
phrases=StringTrim/@StringDelete[phrasesPre,"\""];
indWords=StringSplit[StringDelete[#,AppendTo[phrasesPre,","]]]&[stringText];
allSearches=Join[phrases,indWords];
paddedStringText=StringPadLeft[StringPadRight[stringText,StringLength@stringText+1],StringLength@stringText+2];
allSearches[[With[{string=#},Ordering[Flatten[StringPosition[string," "<>#<>" "]&/@allSearches,1][[All,1]]]]]]&[StringDelete[paddedStringText,{"\"",","}]]]
ClearAll@InitOnce
InitOnce[]:=Module[{},

alldaspacesPre=StringParser[alldaspacesPreText];
instrumentsPre=StringParser[instrumentsPreText];
otherKeysPre=StringParser[otherKeysPreText];
spaceTransferOfInterestPre=DeleteCases[StringTrim/@StringSplit[DeleteCases[StringTrim[StringSplit[StringDelete[#,"\""],{"{","}","(",")","[","]"}]],""],","],{}]&[spaceTransferOfInterestPreText];


{{alldaspaces,ins,otherKeysAll,spaceTransferOfInterestPre2},{alldaspacesWhile,shaperules,colors,othercolors},{b1,b2,legend}}=InitOncePre[{otherKeysPre,instrumentsPre,alldaspacesPre,spaceTransferOfInterestPre},othercolorsPre,colorpalette,newshapePrefs,alltheShapes,fdirectionsGraph,disregardpop,otherSpacesName,otherpagesCalled,alllinksLower,imsize,ds];
]
ClearAll@ShowSubGraph
ShowSubGraph[sample_,imsize_,vertexSize_]:=Module[{innodes,subedges1,inputSubEdges1,vcoords,subPre,g3Sub,verticesOne,edgesOne,posesInNodes,posesInEdges,posesInInputEdges,subsubNodes,twoWaysSub,oneSub,subRLvsBLlinks,toolTippedEdgesOfInterestSub,allEdgeDesignOfInterestSub,edges1TMP,edges2TMP,ints,vLabelFin2,one,two,vcoordsSubSub},
If[sample=={},Abort[];];
SubGraphData[sample];
innodes=inputNodes[[nodePoses]];
subedges1=toolTippedEdgesSimple[[First@allposeysSimple]];inputSubEdges1=allInputEdges[[Last@allposeysSimple]];
g1sub=Graph[innodes,subedges1,EdgeStyle->inputSubEdges1,ImageSize->imsize,VertexSize->vertexSize,VertexLabels->vLabelsMini];
vcoords=With[{gsub=#},PropertyValue[{gsub,#},VertexCoordinates]&/@VertexList[gsub]]&[g1sub];
edges1TMP=toolTippedEdges[[First@allposeysFancy]];edges2TMP=allEdges[[Last@allposeysFancy]];
g2sub=Graph[innodes,edges1TMP,EdgeStyle->edges2TMP,ImageSize->imsize,VertexSize->vertexSize,VertexLabels->vLabelsMini,VertexCoordinates->vcoords];
If[Not@ValueQ@delEdges,Clear[g3subFin,g4subFin];,
subPre=VertexDelete[EdgeDelete[#,Intersection[EdgeList[#],delEdges]],Intersection[VertexList[#],inputNodesTXT[[delPosesFin]]]]&[g1sub];
g3subFin=VertexDelete[subPre,Complement[Extract[VertexList[subPre],Position[VertexDegree[subPre],0]],vLabelsMini[[All,1]]]];verticesOne=VertexList[g3subFin];
posesInNodes=DeleteDuplicates@Flatten[(Position[innodes,#]&/@verticesOne)[[All,All,1]]];
ints=Intersection[vLabelsMini[[All,1]],inputNodesTXT[[nodePoses]][[posesInNodes]]];
vLabelFin2=vLabelsMini[[Flatten[Position[vLabelsMini[[All,1]],#]&/@ints]]];

one=VertexList[g2sub];
subsubNodes=innodes[[Flatten[Position[one,#]&/@verticesOne]]];

twoWaysSub=EdgeList[g3subFin,_<->_];
oneSub=Thread[twoWaysSub[[All,1]]\[DirectedEdge]twoWaysSub[[All,2]]];
subRLvsBLlinks=Join[oneSub,Reverse/@oneSub,EdgeList[g3subFin,_\[DirectedEdge]_]];

{toolTippedEdgesOfInterestSub,allEdgeDesignOfInterestSub}=RLvsCLEdges[allRLvsCLDatPre,subRLvsBLlinks,{arrowsize,thicknessedge}];

two=VertexList[g3subFin];
vcoordsSubSub=PropertyValue[{g3subFin,#},VertexCoordinates]&/@two;g4subFin=Graph[subsubNodes,toolTippedEdgesOfInterestSub,EdgeStyle->allEdgeDesignOfInterestSub,ImageSize->imsize,VertexSize->vertexSize,VertexCoordinates->vcoordsSubSub,VertexLabels->vLabelFin2];
];
defaultGraphSub=If[Not@ValueQ@defaultGraphSub,graphNamesSub[[1]],defaultGraphSub];
]
ClearAll@GenerateGraph
GenerateGraph[imsize_,vertexSize_]:=Module[{},
If[spacesLook=={},Abort[];];
If[insin=={},Abort[];];
If[oldRun=={insin,spacesLook,spaceTransferOfInterest,includeOtherSpaces,includeNONs,otherKeys},
GenAllGraphs[imsize,vertexSize],
GenGraphData[insin,spacesLook,spaceTransferOfInterest,includeOtherSpaces,includeNONs,otherKeys];
oldRun={insin,spacesLook,spaceTransferOfInterest,includeOtherSpaces,includeNONs,otherKeys};
GenAllGraphs[imsize,vertexSize]]
]
ClearAll@PopUpFun
PopUpFun[message_]:=DialogInput[DialogNotebook[{TextCell[Style[message,Bold,18]],DefaultButton[]}]]
ClearAll@CheckMainSearchTerms
ClearAll@CheckMainSearchTerms
CheckMainSearchTerms[]:=Module[{zeroPosesOne,zeroPosesTwo,zeroPosesThree,fEOIs,zeroPosesTwoP5},
zeroPosesOne=Position[Length/@selectionFun[possibleNodes,alldaspaces],0];
zeroPosesTwo=Position[Length/@selectionFun[possibleNodes,ins],0];
zeroPosesTwoP5=Position[Length/@selectionFun[possibleNodes,otherKeysAll],0];
zeroPosesThree=Position[Length/@selectionFun[possibleNodes,fEOIs=Flatten@spaceTransferOfInterestPre2],0];
If[zeroPosesOne!={},PopUpFun["shape filter search terms "<>ToString[Extract[alldaspaces,zeroPosesOne]]<>" bring back null results, try another term"];Abort[];];
If[zeroPosesTwo!={},PopUpFun["color filter search terms "<>ToString[Extract[ins,zeroPosesTwo]]<>" bring back null results, try another term"];Abort[];];
If[zeroPosesTwoP5!={},PopUpFun["color no filter search terms "<>ToString[Extract[otherKeysAll,zeroPosesTwoP5]]<>" bring back null results, try another term"];Abort[];];
If[zeroPosesThree!={},PopUpFun["edge connection terms "<>ToString[Extract[fEOIs,zeroPosesThree]]<>" bring back null results, try another term"];Abort[];];
]
ClearAll[DynamicButtons]
Options[DynamicButtons]={"ButtonStyleFunction":>(Style[#,Bold,Black,18]&)};
DynamicButtons[variables_,outputName_String,tfTable_List,OptionsPattern[]]:=DynamicModule[{nums,output,tfTableFun=tfTable,f,partionNum=5},
Clear[outputName];output=ToExpression[outputName];
nums=Boole[tfTable];f=OptionValue["ButtonStyleFunction"];
Row@{Dynamic[Grid[PartitionWithLeftOver[Table[With[{x=x},
Button[f@variables[[x]],nums[[x]]+=1;tfTableFun[[x]]=OddQ[nums[[x]]],Appearance->If[Dynamic@tfTableFun[[x]],"Pressed",Automatic]]],{x,Length@variables}],partionNum],Spacings->({#,#}&[0.025])]],Style[Dynamic[(#=ToLowerCase@Extract[variables,Position[tfTableFun,True]])]&[output],0.01,Opacity[0.0]]}]
DynamicButtons[variable_,outputName_String,opts:OptionsPattern[DynamicButtons]]:=DynamicButtons[variable,outputName,ConstantArray[False,Length@variable],opts]
ClearAll@UserButtonFun
UserButtonFun[output_]:=DynamicModule[{alldaspaces=output[[1]],ins=output[[2]],otherKeysAll=output[[3]],spaceTransferOfInterestPre2=output[[4]],frac,frac2,sizeTXTSub,buttonTXTsize
},

frac=0.8;frac2=0.65;
sizeTXTSub=frac*sizeTXT;
buttonTXTsize=frac2*sizeTXT;
includeOtherSpaces=False;
includeNONs=False;

Deploy@Column[{Style["Select Graph Options",Bold,sizeTXT],Row[{Style["Shape Filter: ",Bold,sizeTXTSub],DynamicButtons[alldaspaces,"spacesLook",ConstantArray[True,Length@alldaspaces],"ButtonStyleFunction"->(Style[#,Bold,Black,buttonTXTsize]&)]}],
Row[{Style["Color Filter: ",Bold,sizeTXTSub],DynamicButtons[ins,"insin","ButtonStyleFunction"->(Style[#,Bold,Black,buttonTXTsize]&)]}],
Row[{Style["Second Color: ",Bold,sizeTXTSub],DynamicButtons[otherKeysAll,"otherKeys","ButtonStyleFunction"->(Style[#,Bold,Black,buttonTXTsize]&)]}],
Row[{Style["Dashed Edges: ",Bold,sizeTXTSub],DynamicButtons[spaceTransferOfInterestPre2,"spaceTransferOfInterest","ButtonStyleFunction"->(Style[StringRiffle[#," : "],Bold,Black,buttonTXTsize]&)]}],
Row@{Style["Include Other Shapes:",Bold,sizeTXTSub],Checkbox[Dynamic[includeOtherSpaces]],"   ",Style["Include Other Colors:",Bold,sizeTXTSub],Checkbox[Dynamic[includeNONs]]}},Background->Lighter[Gray, 0.9],Frame->All,Spacings->0.5]

]
ClearAll[columnClicks];
columnClicks=Row[{Button["Reset Sample",sample={}],"  ",Dynamic@TableForm@(HyperlinkCheck[#[[1]],#[[2]]]&/@Transpose@{stringReplaceFun[#,samO],#}&[sample])}];
ClearAll@CellFun;
CellFun:=CellPrint[ExpressionCell[Deploy[Button[#2,NotebookDelete[Cells[CellTags->#3]];CellPrint[ExpressionCell[#1,"Output",CellTags->#3]],Method->"Queued"]],"Text"]]&
ClearAll[GenerateLinkReports];
GenerateLinkReports[var_]:=Module[{},
If[ToString@var=="{}","Click on some nodes first!",
If[Length@var==0,If[MemberQ[alllinksLower,ToLowerCase@var],LinkReport[ToLowerCase@var,{contentLinkDatRulez,relatedLinkDatRulez},{RLNC,CNRL,IB,IL}],"Your input is not a URL to a live JDox article!"],
Column[LinkReport[#,{contentLinkDatRulez,relatedLinkDatRulez},{RLNC,CNRL,IB,IL}]&/@#]&[var]]]
]
styleFunChecks:=Style[#1,Bold,14]&
ClearAll@runOnce
runOnce[]:=Column@{
Row@{styleFunChecks@"Show Related Links not in Content: ",Checkbox[Dynamic[RLNC]]},
Row@{styleFunChecks@"Show Content Links not in Related: ",Checkbox[Dynamic[CNRL]]},
Row@{styleFunChecks@"Show Links in Both:                ",Checkbox[Dynamic[IB]]},Row@{styleFunChecks@"Show All Incoming Links:           ",Checkbox[Dynamic[IL]]}}
ClearAll@LinkReport
LinkReport[testartPre_String,{contentLinkDatRulez_,relatedLinkDatRulez_},{RLNC_,CNRL_,IB_,IL_}]:=Module[
{contentLinks,relatedLinks,linkCompareDat,showThisDat,lens,acclens,testDat,griDatPre,griDat,texts={"Related Links not in article Content","Content Links not in Related Links","Links in Both","All Incoming Links to this Article"},
linkFunTitle=Style[HyperlinkCheck[ToUpperCase@StringReplace[FileNameTake@#,stringReplacementRules],#],15,Blue,Bold]&,
linkFunTMP=If[ToString@#=="{}",{},HyperlinkCheck[StringReplace[FileNameTake@#,stringReplacementRules],#]]&,
nullPoses,testart},

testart=ToLowerCase@testartPre;
contentLinks=testart/.contentLinkDatRulez;
relatedLinks=testart/.relatedLinkDatRulez;linkCompareDat={"RLNC"-> Flatten@Complement[relatedLinks,contentLinks],"CNRL"->Flatten@Complement[contentLinks,relatedLinks],"IB"->Flatten@Intersection[contentLinks,relatedLinks]};showThisDat={If[RLNC,"RLNC"/.linkCompareDat],If[CNRL,"CNRL"/.linkCompareDat],If[IB,"IB"/.linkCompareDat],If[IL,Extract[nfdirections[[All,1]],Position[nfdirections[[All,2]],testart]]]};lens=Length/@showThisDat;
acclens=Accumulate@lens;testDat=Prepend[Map[linkFunTMP,DeleteCases[showThisDat,Null],{2}],Column[{linkFunTitle@testart,ToString@Length@DeleteDuplicates@Flatten@Join[contentLinks,relatedLinks]<>" Unique Links On Page",ToString@Length@Flatten@contentLinks<>" Content Links",ToString@Length@Flatten@relatedLinks<>" Related Links"}]];
griDatPre=Insert[Insert[Insert[Insert[Partition[#&/@Flatten[testDat,1],1],{Style[texts[[1]],Bold]},2],{Style[texts[[2]],Bold]},acclens[[1]]+3],{Style[texts[[3]],Bold]},acclens[[2]]+4],{Style[texts[[4]],Bold]},acclens[[3]]+5];
griDat=Fold[DeleteCases[#1,#2]&,griDatPre,#]&[Partition[Style[#,Bold]&/@Extract[texts,nullPoses=Position[showThisDat,Null]],1]];
Grid[griDat,Frame->All,Background->{None,{Flatten@{LightRed,Delete[{{None,ConstantArray[Yellow,lens[[1]]]},{None,ConstantArray[LightGreen,lens[[2]]]},{None,ConstantArray[LightBlue,lens[[3]]]},{None,ConstantArray[LightBrown,lens[[4]]]}},nullPoses]}}},Alignment->Left]
]

ClearAll@RLvsCLEdges;
RLvsCLEdges[allRLvsCLDatPre_,subIn_,{arrowsize_,thicknessedge_}]:=Module[{edgePositionsPre,RelatedLinkOnlyEdges,ContentOnlyEdges,BothEdges,dashes1=Dashing[{Large,Small}],dashes2=Dashing[{Tiny,Tiny}],rlEdges,cEdges,bEdges,allEdges,edgeToolTips,toolTippedEdges,toolTippedEdgesPre,stings,subInStings},

stings=Map[ToString,allRLvsCLDatPre,{2}];subInStings=Map[ToString,subIn];
edgePositionsPre=Map[Position[stings,#,{2},1]&,subInStings];

{RelatedLinkOnlyEdges,ContentOnlyEdges,BothEdges}=Extract[subIn,Position[Flatten[edgePositionsPre[[All,All,1]]],#]]&/@{1,2,3};rlEdges=Thread[RelatedLinkOnlyEdges->Directive[dashes1,Blue,Thickness[2*thicknessedge],Arrowheads[arrowsize]]];
cEdges=Thread[ContentOnlyEdges->Directive[dashes2,Darker[Red],Thickness[2*thicknessedge],Arrowheads[arrowsize]]];bEdges=Thread[BothEdges->Directive[Black,Thickness[4*thicknessedge],Arrowheads[arrowsize]]];
allEdges=Join[rlEdges,cEdges,bEdges];
edgeToolTips=Thread[#[[1]]\[DirectedEdge]#[[2]]]&[Map[stringReplaceFunJustPage@#&,{#[[All,1]],#[[All,2]]},{2}]]&[subIn];

toolTippedEdges=Table[Tooltip[#[[k]],Style[#2[[k]],18]],{k,Length@#}]&[subIn,edgeToolTips];

{toolTippedEdges,allEdges}
]
xmlMissing=Import[pahdge,"XMLObject"];
pageID=First@Cases[xmlMissing,XMLElement["meta",{"name"->"ajs-latest-page-id","content"->x_},_]->x,Infinity,1];
rootPageIds=Cases[xmlMissing,XMLElement["input",{"type"->"hidden","name"->"rootPageId","value"->x_},{}]->x,Infinity];
ancestorIds=DeleteDuplicates[Cases[xmlMissing,XMLElement["input",{"type"->"hidden","name"->"ancestorId","value"->x_},{}]->x,Infinity]];
rootPageId=rootPageIds[[Position[MemberQ[ancestorIds,#]&/@rootPageIds,True][[1,1]]]];
pagetreeDat=Import[pahdge<>"plugins/pagetree/naturalchildren.action?hasRoot=true&expandAll=true&pageId="<>#<>StringJoin["&ancestors="<>#&/@ancestorIds]<>"&expandAll=true"<>"&treePageId="<>pageID]&/@ancestorIds;
joindat=WebFunction1[pagetreeDat];
beg=StringDrop[pahdge,-1];
datPre=beg<>#&/@joindat;
xmlPre=ParallelMap[Import[#,"XMLObject"]&,datPre];
idsToSearchMaybe=Flatten[Cases[#,XMLElement["meta",{"name"->"ajs-latest-page-id","content"->x_},_]->x,Infinity,1]&/@xmlPre];
homelinks=pahdge<>"plugins/pagetree/naturalchildren.action?decorator=none&excerpt=false&sort=position&reverse=false&disableLinks=false&expandCurrent=false&hasRoot=true&pageId="<>#<>"&treeId=1&startDepth=999&mobile=false&treePageId="<>pageID&/@idsToSearchMaybe;
datHOME=ParallelMap[Import,homelinks];
joindat2=WebFunction1[datHOME];
alldat=beg<>#&/@joindat2;
alllinksToCheck=DeleteDuplicates@Join[datPre,alldat];
brokenlinks=directions=linksWorked=allmaybebroken=allBSLinks=alldox=allhashlinks=alllinksAppend={};
SetSharedVariable[brokenlinks];

WebCrawl[sample,ignoreLinksWith];

xmldatPre=ParallelMap[Import[#,"XMLObject"]&,linksWorked];
Quiet[falled={};
idDatPre=Table[
Check[xmlMissing=xmldatPre[[kk]];
pageID=First@Cases[xmlMissing,XMLElement["meta",{"name"->"ajs-latest-page-id","content"->x_},_]->x,Infinity,1];
rootPageIds=Cases[xmlMissing,XMLElement["input",{"type"->"hidden","name"->"rootPageId","value"->x_},{}]->x,Infinity];
ancestorIds=DeleteDuplicates[Cases[xmlMissing,XMLElement["input",{"type"->"hidden","name"->"ancestorId","value"->x_},{}]->x,Infinity]];
rootPageId=rootPageIds[[Position[MemberQ[ancestorIds,#]&/@rootPageIds,True][[1,1]]]];
{rootPageId,ancestorIds,pageID},AppendTo[falled,{linksWorked[[kk]],{kk}}]],{kk,Length@xmldatPre}];]
falled2={};
SetSharedVariable[falled2];
Quiet[
idDat=Delete[idDatPre,delposeIm=falled[[All,2]]];
linksWorkedDel=Delete[linksWorked,delposeIm];
pagetreeDat=ParallelTable[
{rootPageId,ancestorIds,pageID}=idDat[[kk]];Quiet@Check[Import[pahdge<>"plugins/pagetree/naturalchildren.action?hasRoot=true&pageId="<>rootPageId<>StringJoin["&ancestors="<>#&/@ancestorIds]<>"&treePageId="<>pageID],AppendTo[falled2,{linksWorkedDel[[kk]],{kk}}]],
{kk,Length@idDat}];
datPrePre=WebFunction1[pagetreeDat];
datPre=StringDrop[pahdge,-1]<>#&/@DeleteDuplicates@datPrePre;
lostpages=Complement[DeleteDuplicates@datPre,linksWorked];
tFotmOut=(HyperlinkCheck[stringReplaceFun[#,samO],#]&/@(failedOnes=Join[Extract[linksWorked,delposeIm],Extract[linksWorkedDel,falled2[[All,2]]]]));
(*Print[TableForm@tFotmOut];*)
];
lostLINKS=checkdis=alllinksToCheck=Complement[lostpages,brokenlinks];
linksWorkedO=linksWorked;
(*First@AbsoluteTiming@*)WebCrawl[sample,ignoreLinksWith];
(*{Length@linksWorked,Length@DeleteDuplicates@linksWorked}*)
importLinksLeft=Drop[linksWorked,Length@linksWorkedO];
(*importLinksLeft//Length*)
xmldatSupp=ParallelMap[Import[#,"XMLObject"]&,importLinksLeft];
xmldat=Join[xmldatPre,xmldatSupp];
alllinks=linksWorked;
fdirectionsPre=Flatten@directions;
fdirectionsPre2=Delete[#,DeleteDuplicates[Join[Position[StringQ/@#[[All,1]],False],Position[StringQ/@#[[All,2]],False]]]]&[fdirectionsPre];
fdirections=Map[ToLowerCase,Complement[fdirectionsPre2,Select[fdirectionsPre2,#[[1]]==#[[2]]&]],{2}];
alllinksLower=ToLowerCase@alllinks;

dasta=Reverse@Sort[Reverse/@Tally[(FileNameSplit/@alllinksLower)[[All,5]]]];
checkSpaces="/"<>#<>"/"&/@Reverse[Sort@dasta[[All,2]]];
brokeContentLinks={};
brokeRelatedLinks={};
dumbcontentLinks={};
brokeSummaryLinks={};
emptynums={};
emptynumsIter={};
relatedLinkDatPre=Table[
xmlwoo=xmldat[[poss]];
linker=alllinksLower[[poss]];
thing=Cases[xmlwoo,XMLElement["div",{__,"data-name"->"Related links",__},x_]->x,Infinity];
If[thing=={},AppendTo[emptynums,poss];
thingPre=Join[Cases[xmlwoo,XMLElement["div",{"class"->"brikit-content-block original designable-element",__},x_]->x,Infinity],Cases[xmlwoo,XMLElement["div",{"class"->"brikit-content-block original designable-element pdf-hide",__},x_]->x,Infinity]];
thing=Extract[thingPre,Position[StringContainsQ[ToString/@thingPre,"{Related Links}",IgnoreCase->True],True,1,1]];
If[thing=={},AppendTo[emptynumsIter,poss];];
];
links=DeleteDuplicates[First/@StringSplit[DeleteCases[ToLowerCase@Cases[#,XMLElement["a",{"shape"->"rect","href"->x_},__]->x,Infinity],_?(StringTake[#,1]=="#"&)],"#"]]&[thing];
tf=StringContainsQ[links,addit];
If[MemberQ[tf,True],AppendTo[brokeRelatedLinks,{linker,Extract[links,trueposes=Position[tf,True]]}];
links[[Flatten@trueposes]]=StringDelete[links[[Flatten@trueposes]],addit];
links=DeleteDuplicates[links];
];
If[links=={},{linker\[DirectedEdge]{}},linker\[DirectedEdge]addit<>#&/@links],{poss,Length@xmldat}];
emptynumsContent={};
contentLinksPrePre=Table[
xmlwoo=xmldat[[poss]];
linker=alllinksLower[[poss]];
thing=Cases[xmlwoo,XMLElement["div",{__,"data-name"->"Content",__},x_]->x,Infinity];
If[thing=={},AppendTo[emptynumsContent,poss]];
links=DeleteDuplicates[First/@StringSplit[DeleteCases[ToLowerCase@Cases[#,XMLElement["a",{"shape"->"rect","href"->x_},__]->x,Infinity],_?(StringTake[#,1]=="#"&)],"#"]]&[thing];
tf=StringContainsQ[links,addit];
If[MemberQ[tf,True],AppendTo[brokeContentLinks,{linker,Extract[links,trueposes=Position[tf,True]]}];
links[[Flatten@trueposes]]=StringDelete[links[[Flatten@trueposes]],addit];
links=DeleteDuplicates[links];];
tf2=Not/@StringContainsQ[links,"/display/"];
If[MemberQ[tf2,True],AppendTo[dumbcontentLinks,{linker,Extract[links,trueposes2=Position[tf2,True]]}];
links=Fold[DeleteCases[#1,#2]&,links,links[[Flatten[trueposes2]]]];];
If[links=={},{linker\[DirectedEdge]{}},linker\[DirectedEdge]addit<>#&/@links],{poss,Length@xmldat}];
summaryLinksPre=Table[
xmlwoo=xmldat[[poss]];
linker=alllinksLower[[poss]];
thing=Cases[xmlwoo,XMLElement["div",{__,"data-name"->"Summary",__},x_]->x,Infinity];
links=DeleteDuplicates[First/@StringSplit[DeleteCases[ToLowerCase@Cases[#,XMLElement["a",{"shape"->"rect","href"->x_},__]->x,Infinity],_?(StringTake[#,1]=="#"&)],"#"]]&[thing];
tf=StringContainsQ[links,addit];
If[MemberQ[tf,True],AppendTo[brokeSummaryLinks,{linker,Extract[links,trueposes=Position[tf,True]]}];
links[[Flatten@trueposes]]=StringDelete[links[[Flatten@trueposes]],addit];
links=DeleteDuplicates[links];];
tf2=Not/@StringContainsQ[links,"/display/"];
If[MemberQ[tf2,True],AppendTo[dumbcontentLinks,{linker,Extract[links,trueposes2=Position[tf2,True]]}];
links=Fold[DeleteCases[#1,#2]&,links,links[[Flatten[trueposes2]]]];];
If[links=={},{linker\[DirectedEdge]{}},linker\[DirectedEdge]addit<>#&/@links],{poss,Length@xmldat}];
contentLinksPre=(DeleteDuplicates/@Flatten/@Transpose@{contentLinksPrePre,summaryLinksPre});
thermomColors1=Table[ColorData["ThermometerColors"][k],{k,0.0,1.0,0.25}];
thermomColors2=Reverse@Table[ColorData["ThermometerColors"][k],{k,0.125,1.0,0.25}];
colorpalette=Join[{Orange,Red,Green,Pink,Lighter[Purple]},thermomColors1];
othercolorsPre=Join[{Cyan,Magenta},thermomColors2];
Graph Vars
sizeTXT=20.0;
noncolor=Gray;
imsize=1200;vertexSize=Automatic;
graphNames={"Simple Edges","Content Links vs. Related Links","Edges of Interest on Whole Graph","Edges of Interest: Simple Edges","Edges of Interest: Content Links vs. Related Links"};
graphNamesSub={"Simple Edges","Content Links vs. Related Links","Edges of Interest: Simple Edges","Edges of Interest: Content Links vs. Related Links"};
arrowsize=0.006;shapesize=0.06;thicknessedge=0.0003;dashes=Dashing[{Large,Small}];biggerThickness=4*thicknessedge;
alltheShapes=GraphElementData["Vertex"];
newshapePrefs=Join[#,Complement[Range@Length@alltheShapes,#]]&[{4,2,13,14,28,29,27,26}];
disregardpop=False;
sample={};
edgies={};
styleFun=Style[#,Bold,22]&;
includeNONs=False;
includeOtherSpaces=False;
linkRuleFun=Thread[Flatten[DeleteDuplicates/@#[[All,All,1]]]->#[[All,All,2]]]&[DeleteCases[#,{}]]&;
RLNC=True;CNRL=True;IB=True;IL=True;
otherpagesCalled="Other";
otherSpacesName="Other";
oldRun={};
InitPre[];
dashes1=Dashing[{Large,Small}];dashes2=Dashing[{Tiny,Tiny}];
Share[];
alldaspacesPreText="JTI JSP JPPOM JPP JDAT";
instrumentsPreText="NIRCam NIRSpec NIRISS MIRI \"recommended strategies\" coronagraphy";
otherKeysPreText="etc apt";
spaceTransferOfInterestPreText="{etc, apt} {miri, apt} {miri, recommended strat} {coronagraphy,jwst}";
styleForInputFields:=Style[#,Bold,0.8*sizeTXT]&
inputFields=Column[{
Row@{styleForInputFields@"Shape Filter Buttons: ",InputField[Dynamic[alldaspacesPreText],String]},
Row@{styleForInputFields@"Color Filter Buttons: ",InputField[Dynamic[instrumentsPreText],String]},
Row@{styleForInputFields@"Second Color Buttons: ",InputField[Dynamic[otherKeysPreText],String]},
Row@{styleForInputFields@"Dashed Edges Buttons: ",InputField[Dynamic[spaceTransferOfInterestPreText],String]}
},Background->Lighter[Gray, 0.7],Frame->True];

ClearAll@graphManip
graphManip:=Dynamic[If[Not@Or[insin=={},spacesLook=={}],sample={};edgies={};Clear[g1,g2,g3pre,g5,g6,g1sub,g2sub,g3subFin,g4subFin];GenerateGraph[imsize,vertexSize];TabView[{graphNames[[1]]->g1,graphNames[[2]]->g2,graphNames[[3]]->g3pre,graphNames[[4]]->g5,graphNames[[5]]->g6},ImageSize->Automatic],"Click on Buttons for Shape/Color Filters!"],SynchronousUpdating->False];


ClearAll@subgraphManip
subgraphManip:=Dynamic[If[Not@Or[insin=={},spacesLook=={},sample=={}],Clear[g1sub,g2sub,g3subFin,g4subFin];ShowSubGraph[sample,imsize,vertexSize];TabView[{graphNamesSub[[1]]->g1sub,graphNamesSub[[2]]->g2sub,graphNamesSub[[3]]->g3subFin,graphNamesSub[[4]]->g4subFin},ImageSize->Automatic],"Click on Buttons for Shape/Color Filters and make sure you clicked on nodes put in your sample!"],SynchronousUpdating->False];


genLinkReportsManip:=Dynamic@GenerateLinkReports[sample];
inputLinkReport:=Deploy@Manipulate[GenerateLinkReports[copypaste],{{copypaste,"",Style["Paste JDox URL Here:",14]},InputField[#,String]&},SynchronousUpdating->False];

clearLine[]:=Clear[insin,spacesLook,spaceTransferOfInterest,otherKeys,g1,g2,g3pre,g5,g6];
userButtons:=Deploy@UserButtonFun[{alldaspaces,ins,otherKeysAll,spaceTransferOfInterestPre2,includeOtherSpaces,includeNONs}]
checkDefault[]:=Module[{},defaultGraph=If[Not@ValueQ@defaultGraph,graphNames[[1]],defaultGraph]]

directiveManipStyle=Directive[Darker@Blue,16];
DeployJDoxGUI[]:=Column@{inputFields,CellPrint@ExpressionCell[Deploy@Button[Style["Update Buttons",Bold,24],NotebookDelete[Cells[CellTags->"calculationbutton"]];
CellPrint@ExpressionCell[
InitOnce[];
Column[{
CheckMainSearchTerms[];sample={};edgies={};clearLine[];
Deploy@legend,
Deploy@columnClicks,
checkDefault[];
Deploy@TabView[{"Graph Options"->userButtons,"Graph"->graphManip,"Subgraph"->subgraphManip,"Link Report for Sample"->genLinkReportsManip,"Input Link Report"->inputLinkReport},ImageSize->Automatic]
}],"Output",CellTags->"calculationbutton"],Method->"Queued"],"Text"]}
