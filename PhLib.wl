(* ::Package:: *)

Begin["PhLib`"];

labels = {
	"date","Bmicro",
	"A138","AA138","B138","AB138","C138","AC138",
	"A230","AA230","B230","AB230","C230","AC230",
	"IA1381","AIA1381","IB1381","AIB1381","IC1381","AIC1381",
	"IA2301","AIA2301","IB2301","AIB2301","IC2301","AIC2301",
	"IA1382","AIA1382","IB1382","AIB1382","IC1382","AIC1382",
	"IA2302","AIA2302","IB2302","AIB2302","IC2302","AIC2302"
};

KC = <|
 "FA1381" -> 0.36, "FB1381" -> 0.40, "FC1381" -> 0.40, 
 "FA2301" -> 0.38, "FB2301" -> 0.33, "FC2301" -> 0.30,
 "FA1382" -> 0.46, "FB1382" -> 0.37, "FC1382" -> 0.36, 
 "FA2302" -> 0.35, "FB2302" -> 0.39, "FC2302" -> 0.36,
 "CA1381" -> 230, "CB1381" -> 240, "CC1381" -> 240, 
 "CA2301" -> 287, "CB2301" -> 283, "CC2301" -> 290,
 "CA1382" -> 231, "CB1382" -> 230, "CC1382" -> 230, 
 "CA2302" -> 277, "CB2302" -> 296, "CC2302" -> 290
 |>;


KV = <| "X138" -> 10, "X230" -> 10 |>;
KI = <| "X138" -> 1 10^-6, "X230" -> 1 10^-6 |>;

(* Calibrate constants *)
calibrate[ dataset_, target_, save_:Null] := Module[{h, x, iref, i, xc, ilabel},
	h = Cases[labels, s_ /; StringMatchQ[ s, "A"~~_~~"230"]];
	x = Cases[labels, s_ /; StringMatchQ[ s, "A"~~_~~"138"]];
	
	KV = Association[ Union [ 
		Table[hi->230000/Mean@dataset[[All,hi]], {hi,h}], 
		Table[xi->138000/Mean@dataset[[All,xi]], {xi,x}] ] ];
	
	KI = <||>;	
	Do[ 
		xc = 10^12/(120 Pi KC[ "C" <> StringDrop[hi,1] <> t ]);
		iref =  230000 / xc;
		ilabel = "AI" <> StringDrop[hi,1] <> t;
		i = Median@dataset[[All, ilabel]];
		AppendTo[KI, ilabel->iref/i];	
	,{hi,h}, {t,{"1","2"}}];
		
	Do[ xc = 10^12/(120 Pi KC["C" <> StringDrop[xi,1] <> t]);
		iref =  138000 / xc;
		ilabel = "AI" <> StringDrop[xi,1] <> t;
		i = Median@dataset[[All, ilabel]];
		AppendTo[KI, ilabel->iref/i];	
	,{xi,x}, {t, {"1","2"}}];

	If[ StringQ[save], Print["Updating ...", save]; Put[<|"KV" -> PhLib`KV, "KI" -> PhLib`KI|>, save] ];
];

(* Adjust ADC measurements according to the KV and KI constants *)
scale[r_] := (
	If[FileExistsQ["calibration.txt"], (KV= #["KV"]; KI= #["KI"];) &[ Get["calibration.txt"] ] 	];
	<|Table[
    l -> r[l] Which[ 
    	StringMatchQ[l, "A"~~_~~"138"], Lookup[KV, l, KV["X138"]],
    	StringMatchQ[l, "A"~~_~~"230"], Lookup[KV, l, KV["X230"]],
    	StringMatchQ[l, "AI" ~~ _~~"138"~~ _], Lookup[KI, l, KI["X138"]],
		StringMatchQ[l, "AI" ~~ _~~"230"~~ _], Lookup[KI, l, KI["X230"]],
		True, 1]
    , {l, Keys[r]}]|> 
);

(* Normalize angle to the range -180 to +180 degrees *)
toang[a_] := Module[{aa},
	aa = a; While[aa >= 360, aa -= 360]; 
	While[aa <= -360, aa += 360]; If[aa > 180, aa -= 360]; 
	If[aa < -180, aa += 360]; 
	Return[aa];
	];

(* Define the mapping between the RMS values of currents and tensions *)
toVrefI[labels_] := Module[{v,i},
	v = Cases[labels, s_ /; StringMatchQ[ s, "A"~~_~~"138"|"230"]];
	i = Cases[labels, s_ /; StringMatchQ[ s, "AI"~~_~~"138"|"230"~~_]];
	Flatten[Table[ Thread[{t, Cases[i, s_ /; StringMatchQ[ s, "AI"~~StringDrop[t,1]~~_]]}] ,{t,v}],1]
	];
	
(* Define the mapping between the phase values of currents and tensions *)
toVrefIF[labels_] := Module[{v,i},
	v = Cases[labels, s_ /; StringMatchQ[ s, _~~"138"|"230"]];
	i = Cases[labels, s_ /; StringMatchQ[ s, "I"~~_~~"138"|"230"~~_]];
	Flatten[Table[ Thread[{t, Cases[i, s_ /; StringMatchQ[ s, "I"~~t~~_]]}] ,{t,v}],1]
	];
(* Computes power factor phases *)	
ToPF[onda_] := Module[{vrefi},
   vrefi = toVrefI[labels];
   vrefi = StringDrop[#,1] & /@ # &/@ vrefi;
   
   Return[<| StringReplace[Last[#], "I" -> "F"] -> 
        toang[onda[First[#]] - onda[Last[#]]] & /@ vrefi |> ]
	];
(* Computes capacitances *)
ToCap[fasor_] := Module[{rms, vrefi, c, vi},
   vrefi = toVrefI[labels];
   rms = scale[fasor];
  (* zc = z Sqrt[1+Tan[ArcCos[PF]]^2]/Tan[ArcCos[PF]] *)
 
   Return[<|
     Table[ 
     	c = (1/((120 Pi) rms[First[vi]]/rms[Last[vi]]));
     	StringReplace[Last[vi],"AI"->"C"]->(c * 10^12)
	, {vi, vrefi}]|>]
   ];

(* Reads fasor file  *)
ReadPhasor[filename_] := Module[{data, i, date, d},
	If[ FailureQ[ 
		f = OpenRead[filename] (* // Quiet *) ], 
			Print[ "The attempt to open the file failed!!!"]; 
			Return[Null] 
	];

	data = Reap[
	While[ True,
		If[ (i =ReadLine[f]) == EndOfFile, Break[]];
		i = StringSplit[i,";"];
		d = ToExpression[ StringSplit[i[[1]], {"/", " ", ":"}]][[{3, 2, 1, 4, 5, 6}]];
			
		If[ FailureQ[ date = DateObject[d]] // Quiet, 
			Print["Date Problem ", filename]; Pause[0.1];
			Continue[]
		];
		i = Join[{date}, ToExpression[i[[2;;]]]];
		Sow[ Association@MapThread[Rule,{labels,i}] ];
	]][[2,1]];
	Close[f];

	Return[data]
];

(* Read all phasors in a period - used by calibration *)

ReadHeadPhasors[ filepath_, time_, unit_]  := Module[{i0, file, file2, dir, ds, idate, date, filename},
	file = FileNameTake[filepath];
	dir = DirectoryName[filepath];
   
	i0 = i = ToExpression[StringCases[file, NumberString]][[1]];
   
	filename = filepath;

	ds = {};
	While [  True,
		If[ !FileExistsQ[filename], Print[filename]; Break[]];
		data = ReadPhasor[filename];
		If[data == Null, Break[]];
		If[ i == i0, idate = data[[1]]["date"] ];
		date = data[[-1]]["date"];
		ds = Join[ds,data];
		If [ date - idate > Quantity[time, unit], Break[]];
		i += 1;
		file2 = StringReplace[file, ToString[i0] -> ToString[i]];
		
		filename = If[StringLength[dir] > 0, FileNameJoin[{dir, file2}], file2, Print["nao sei"]];
	];
	Return[ds];
];

(* Reads fasor file and computes phase and capacitance *)
ToPhase[filename_] := Module[{f, onda,  r, i, res, date, data},
	
	data = ReadPhasor[filename];
		
	(* Save a new dataset with bushing data - capacitance and dissipation factor *)
	res = Reap[	Do[
		r = Union[ ToPF[onda] , ToCap[onda]];
		r = Union[ <|"file" -> FileNameTake[filename], "data" -> onda["date"] |> , r ];
		Sow[r];
	, {onda, data}]][[2,1]];
 
 	Return[res]; 
];



(* Computes the fasorial resultant of voltages or currents 
   Label is 138 or 230 for tensions or 1381, 1382, 2301, 2302 for currents *)

PolarData[phasors_, label_] := Module[{f, x, pl, al, p, a, data, r}, 
	f[x_] := Which[ x <= -Pi, x + 2 Pi, x > Pi, x - 2 Pi, True, x];
	pl = # <> label & /@ {"A", "B", "C"};
	If[ StringLength[label] > 3 , pl = "I" <> # & /@ pl];
	al = "A" <> # & /@ pl;
	p = Values@KeyTake[phasors Degree,  pl] - phasors[pl[[1]]] Degree;
	p = f /@ p;
	a = Values@KeyTake[phasors,  al]/phasors[al[[1]]];
	data = Partition[ Riffle[p, a], 2];
	r = Reverse@ToPolarCoordinates@Total[FromPolarCoordinates /@ (Reverse /@ data)]; 
 	AppendTo[data, r];
	Return[{ phasors["date"],data} ];
]

(* Returns a PNG with fasorial resultant of voltages or currents 
   Label is 138 or 230 for tensions or 1381, 1382, 2301, 2302 for currents *)

PolarDiagram[phasors_, label_] := 
	Module[{v, a , p, i, al, pl, data, r, f, pdata},
	f[x_] := Which[ x <= -Pi, x + 2 Pi, x > Pi, x - 2 Pi, True, x];
	pl = # <> label & /@ {"A", "B", "C"};
	If[ StringLength[label] > 3 , pl = "I" <> # & /@ pl];
	al = "A" <> # & /@ pl;
	p = Values@KeyTake[phasors Degree,  pl] - phasors[pl[[1]]] Degree;
	p = f /@ p;
	a = Values@KeyTake[phasors,  al]/phasors[al[[1]]];
	data = Partition[ Riffle[p, a], 2];
	r = Reverse@ToPolarCoordinates@Total[FromPolarCoordinates /@ (Reverse /@ data)]; 
 	AppendTo[data, r];
	AppendTo[pl, "Residual"];

	ang = Mod[#/Degree + 360, 360] & /@ data[[All,1]];
	pdata = StringForm["``\[Angle]``", NumberForm[#[[1]],2],NumberForm[#[[2]],5] ] 
           &/@ Partition[Riffle[data[[All,2]], ang],2];
	data = Partition[ Riffle[data, {{0, 0}}, {2, -1, 2}], 2];
	ListPolarPlot[data, Joined -> True, 
     PlotTheme -> {"Scientific", "Grid"}, 
     PlotStyle -> {Directive[Dashed,Black], Directive[Dashed,Green], Directive[Dashed,Blue], Red},  
     PlotRange -> {{-1.1, 1.1}, {-1.1, 1.1}}, 
	 PlotLegends -> pdata, 
     PlotLabels -> (Callout[#, Left, Background -> LightBlue] & /@ {"BUSH 1","BUSH 2","BUSH 3","RES"}),
	 PlotLabel->StringJoin[ If[StringLength[label]>3,"Leakage currents T=","Voltage T="], label, " measured at ", DateString[phasors["date"]]]
	]
];

PNGPolarDiagram[phasors_, labels_:{}, dir_:"DASHBOARD"] := Module[{plots, file},
	If[ labels == {},
		plots = {"138","230","1381","1382","2301","2302"},
		plots = labels;
	];
 
	If[ !DirectoryQ[dir], CreateDirectory[dir]];

	Do[ file = FileNameJoin[ {dir, "tri" <> p <> ".png"} ];
		Export[file, PolarDiagram[phasors, p], "PNG"]
	, {p, plots}];
];

(*  Transform phasor files (CSV) into wolfram cisei files
	filepath: first phasor (CSV) file to be processed
	datasetdir: dataset to store the wolfram cisei files (keep existent files)
	numfiles: number of (CSV) to be grouped into a cisei file
	limit: Default: Process all files available in the source folder
	index: filename of the index of processed files
*)
Options[CreateDataset] = {"numfiles"->1, "limit"->0, "index"->"index.txt"};
CreateDataset[filepath_, datasetdir_, OptionsPattern[]] := 
	Module[{filename, file, dir, i, i0, res, dsfile, file2, data, date, file3, fp},

	filename = filepath;
	file = FileNameTake[filename];
	dir = DirectoryName[filename];
	count = If[ OptionValue["limit"] > 0, limit, Infinity];

	i0 = i = ToExpression[StringCases[file, NumberString]] [[1]]; 
	
	If[ !DirectoryQ[datasetdir], CreateDirectory[datasetdir]; ];

	While[ True,
		If[ count == 0 || !FileExistsQ[filename], Return[filename]];
		count -= 1;

		Do[	
			res =  ToPhase[filename];
			If [ j == 1,
				date = StringRiffle[ DateList[res[[1]]["data"]], ","];
				data = StringReplace[DateString[res[[1]]["data"]],":" -> "_"];
				dsfile  = FileNameJoin[{datasetdir, "cisei_" <> data <> ".txt"}];
				PutAppend[ res[[1]]["data"]->dsfile, OptionValue["index"]] ;
			];

			fp = OpenAppend[dsfile];
			Do[ Write[ fp, r], {r,res} ];
			Close[fp];

			i += 1;
			file2 = StringReplace[file, ToString[i0] -> ToString[i]];
			filename = If[StringLength[dir] > 0, FileNameJoin[{dir, file2}], file2 ];

		,{j,1,OptionValue["numfiles"]}]  (* Do *);

	]; (* While *);
];


End[];
