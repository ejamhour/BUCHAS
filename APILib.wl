(* ::Package:: *)
(* DSLIbScript.wl must be loaded in the kernet to used this library *)

Begin["APILib`"]

KC = Null;

(* Adjust measures according to the calibration  *)
Calibrate[r_] := (
	If[KC == Null,
       If[ FileExistsQ["DATASETS/calibration.txt"], KC = Get["DATASETS/calibration.txt"] ],
       Return[r]
    ];

	<|  Table[ l -> Which[ 
                StringStartsQ[l, "F"] , Tan[(90 - r[l] + KC[l])*Pi/180]*100,  
                StringStartsQ[l, "C"], r[l]/ KC[l] , 
                True, r[l] ]  , {l, Keys[r]}]
    |> 
);


(* Plot a graphical representation of the last acquisition 
   datasetindex: filename of the index file
   transformer: string with the transformer number
   trafo: string with the voltage in KV: 138 ou 230
*)

Options[ShowLastMeasure] = {"Period"->24, "Unit"->"Hours"};
ShowLastMeasure[datasetindex_, transformer_, trafo_, OptionsPattern[]] := 
Module[{vars, res, r, label, cal, F},

   	vars = Table[ "F" <> f <> trafo <> transformer, {f, {"A", "B", "C"}}];
   	vars = Join[vars, Table[ "C" <> f <> trafo <> transformer, {f, {"A", "B", "C"}}]];
   	
    label[l_] :=  StringReplace[ l, 
        {"F" -> "DF", "A138" -> " X1", "A230" -> " H1", 
         "B138" -> " X2", "B230" -> " H2", 
         "C138" -> " X3", "C230" -> " H3", "1" -> "", "2" -> ""}];

    r = DSLib`TailDataLog[OptionValue["Period"], OptionValue["Unit"]];
    r = Calibrate /@ r;
    res = {};
    Do[
        d = r [[All, F]][[All, 2]];
        scales = DSLib`calibration[F] * If[StringStartsQ[F,"F"], 
            {  0, 0.75, 1, 1.25, 1.4, 1.75, 2} , 
            {  0.95,0.97, 1, 1.03, 1.05, 1.1, 1.2 } ] ; 
        AppendTo[res, 
            VerticalGauge[Quantile[d, {0.5}], scales[[{1, -1}]], ImageSize -> 100, 
                ScaleRanges -> Partition[scales, 2, 1], 
                ScaleRangeStyle -> {White, Gray, Green, Yellow, Red, Black}, 
                GaugeLabels -> label[F]]];
    , {F, vars} ];
   
    Panel[
        Row[ Riffle[res, Spacer[10]]],
            {"BUSHING " <> trafo <> "KV from " <> DateString[r[[1]]["date"]] <> 
            " to " <> DateString[r[[-1]]["date"]],
            "DF = Dissipation Factor (%) and C = Capacitance (pF)"},
        {Top, Bottom}]
   ];


APIShowLastMeasure = 
  APIFunction[{"datasetindex" -> String, "transformer" -> String, 
    "trafo" -> "String"}, ShowLastMeasure[#datasetindex, #transformer, #trafo] &, "PNG"];

PNGShowLastMeasure[datasetindex_, labels_:{}, dir_:"DASHBOARD"] := Module[{plots, file},
	If[ labels == {},
		plots = {"1381","1382","2301","2302"},
		plots = labels;
	];
 
    If[ !DirectoryQ[dir], CreateDirectory[dir]];

	Do[ file = FileNameJoin[ {dir, "lastmeasurebush" <> p <> ".png"} ];
		Export[file, ShowLastMeasure[datasetindex, StringTake[p,-1], StringTake[p,3]], "PNG"]
	, {p, plots}];
];

(* Show system information 
   datasetindex: filename of the index file
*)

ShowSystemStatus[datasetindex_] := Module[{t1, t2, r, rr, res},
    Instant = "Instant";
    Gregorian = "Gregorian";
    
    t1 = If [ $OperatingSystem  == "Windows", 
        FileDate[datasetindex, "Modification"],
     	FileDate[datasetindex, "Change"]
    ]; 
    t1 = TimeZoneConvert[t1, -3];
   	t2 =  LocalTime[] - t1;

   	r = ReadList[datasetindex];
   	rr = ReadList[r[[-1, 2]]];
   	res = "Local time is " <> DateString[ LocalTime[]];
   
   	If [ t2 > Quantity[120, "Minutes"],                    
        res = res <> "<H3> SYSTEM ALERT - ACTION IS REQUIRED</H3>";
        res = res <> "<P>CHECK THE SYSTEM HEALTH AT SUBSTATION",
    	res = res <> "<P><H3>SYSTEM IS FULLY OPERATIONAL</H3>" 
    ];
   
   	res = res <> "<P>Last update received at: " <>  DateString[t1];
   	res = res <> "<P>Elapsed time from last update time: " <>  ToString[t2];
   	res = res <> "<P>Most recent data acquired from " 
              <> DateString[rr[[1]]["data"]] <> " to " <> DateString[rr[[-1]]["data"]];
   	res = res <> "<P>Most recent file processed: " <> ToString[rr[[-1]]["file"]];
   	res = res <> "<P>Historical data available from " <> DateString[ r[[1]][[1]]] 
              <> " to " <> DateString[rr[[-1]]["data"]];

    Return[res];
   ];

APIShowSystemStatus = 
  APIFunction[{"datasetindex" -> String}, 
   ShowSystemStatus[#datasetindex] &, "HTML"];

(* Plot a variable of a CISEI file within a (backward) period 
   datasetindex: filename of the index file
   trafo: string identifying the trafo (138KV or 230KV) and transformer
   period unit: backward period used in the plot
*)

Options[PlotRawData] = {"quantile"->{}, "func"->Median};
PlotRawData[datasetindex_, trafo_, period_, unit_, OptionsPattern[]] := 
    Module[{files, ds, myds, de, label, bdata, i, ang},
    
    ds = ReadList[datasetindex ];
    i = Interval[{ DatePlus[ds[[-1, 1]] , -Quantity[period, unit]], ds[[-1, 1]] } ];
   
    files = Select[  ds, Between[ #[[1]], i] &];
    myds = Reap [ Do[ Sow@ReadList[ f[[2]] ] , {f, files}] ][[2,1]];
    
    label = "F" <> # <> trafo & /@ {"A", "B", "C"};

    bdata = {};
    Do[ AppendTo[ bdata,  
        Table[ 
            ang = d[[All,l]];
            If[ OptionValue["quantile"] != {},
                ang = DeleteCases[ ang, x_ /; x < #[[1]] || x > #[[2]] &[Quantile [ang, OptionValue["quantile"]] ]];
            ];
            { d[[1]]["data"],  OptionValue["func"]@ang } 
        , {d, myds}] ]
    , {l,label}
    ];

    Return[
        DateListPlot[bdata,
        PlotTheme -> {"Scientific", "Grid"}, 
        PlotLegends -> {"BUSH 1","BUSH 2","BUSH 3"},
        PlotLabel->StringJoin[ "Power factor angle T=", trafo, " until ", DateString[files[[-1,1]]] ],
        ImageSize->Large,
        PlotRange->Full
        ]
    ];
];

PNGPlotRawData[indexfile_, period_:24, unit_:"Hours", labels_:{}, dir_:"DASHBOARD"] := Module[{plots, file},
	If[ labels == {},
		plots = {"1381","1382","2301","2302"},
		plots = labels;
	];
 
	If[ !DirectoryQ[dir], CreateDirectory[dir]];

	Do[ file = FileNameJoin[ {dir, "pfangle" <> p <> ".png"} ];
		Export[file, PlotRawData[indexfile, p, period, unit], "PNG"]
	, {p, plots}];
];


(* Plot a variable of a CISEI file within a (backward) period 
   info: string (F|C trafo transformer)
   period unit: backward period used in the plot
*)
Options[PlotDataLog] = {"MovingAverage" -> 3 };
PlotDataLog[info_, period_, unit_, OptionsPattern[]] := Module[{res, dhour, dunit, i, t, label, data},


    dhour = DSLib`TailDataLog[period + OptionValue["MovingAverage"], unit];
    dhour = Calibrate[#] & /@ KeyTake[ dhour, {"date", info}];

    t = Partition[ NestWhileList[ DSLib`NextIndex[dhour, #, 1, unit] &, 1, # < Length[dhour] & ], 2, 1];  

    dunit = {};
    Do[ AppendTo[dunit, 
        Thread[{dhour[[i[[1]], 1]], 
        Median@Part[ Part[ dhour, i[[1]] ;; i[[2]] ] , All, 2] }]];
    , {i, t}];

    If[ OptionValue["MovingAverage"] > 0,
        res = {};
        Do[
            data = dunit[[All, i]];  
            madata = MovingAverage[ data[[All, 2]], OptionValue["MovingAverage"]] ; 
            AppendTo[res, Thread[ {data[[All, 1]][[OptionValue["MovingAverage"];;]], madata }] ]; 
        , {i, 1, 3}];
        ,
        res = Transpose@res;
    ];

    label = If[ StringStartsQ[info, "F"], "Dissipation Factor (%)", "Capacitance (pF)"];
    
    Return[
        DateListStepPlot[res, 
        Filling -> {1 -> {{3}, Directive[LightBlue]}}, 
        GridLines->Automatic, 
        PlotStyle -> {Dotted, Black, Dotted}, 
        FrameLabel -> label , AspectRatio->1/3, ImageSize -> Large]
    ];
]

FAPIPlotDataLog := Module[{par, f},

    par = { "Transformer" -> {"1", "2"},
            "Bushing" -> {"H1" -> "A230", "H2" -> "B230", "H3" -> "C230", 
            "X1" -> "A138", "X2" -> "B138", "X3" -> "C138"}, 
            "Parameter" -> <|"Interpreter" -> {"Dissipation Factor" -> "F", "Capacitance" -> "C"}|>,
            "Period" -> <|"Interpreter" -> Number, 
            "Control" -> Function[InputField[##, FieldSize -> 5]]|>, 
            "Unit" -> {"Hours", "Days", "Weeks", "Months"}};

    f = FormObject[par, AppearanceRules -> <|"ItemLayout" -> "Vertical"|>,
        FormLayoutFunction -> 
        Function[
        Row[{#["Transformer"], Spacer[2], #["Bushing"], 
        Spacer[2], #["Parameter"], Spacer[2], #["Period"], 
        Spacer[2], #["Unit"]}] ]   ];

    Return[FormPage[f, PlotDataLog[#Parameter <> #Bushing <> #Transformer, #Period, #Unit] &]
        [<| "Transformer" -> "1", "Bushing" -> "H1", 
        "Parameter" -> "F", "Period" -> 10, "Unit" -> "Days"|>]
    ];
]

PlotBushDataLog[ bush_, period_, unit_] := Module[{label},
    label[l_] :=  "BUSH " <> StringReplace[ l, 
        {"A138" -> "X1", "A230" -> "H1", 
         "B138" -> "X2", "B230" -> "H2", 
         "C138" -> "X3", "C230" -> "H3", "1" -> " TRANSFORMER ATF-C", "2" -> " TRANSFORMER ATF-D"}];

    Return@Labeled[ Framed[ 
        Column[{
            Style[ StringForm["Calibration: Dissipation Factor=`` Capacitance=`` pF",  
            DecimalForm@DSLib`calibration["F"<> bush], DSLib`calibration["C"<> bush] ], Bold],
            PlotDataLog["F" <> bush, period, unit], 
            PlotDataLog["C" <> bush, period, unit]}]], label[bush], Top]
];


PNGPlotBushDataLog[ period_, unit_, labels_:{}, dir_:"DASHBOARD"] := Module[{plots, file},
	plots = If[ labels == {},
		Flatten[Table[ # <> t & /@ {"A", "B", "C"}, {t, {"1381","1382","2301","2302"}}]], 
		labels
	];
 
	If[ !DirectoryQ[dir], CreateDirectory[dir]];

	Do[ file = FileNameJoin[ {dir, "bushdatalog" <> p <> ".png"} ];
		Export[file, PlotBushDataLog[p, period, unit], "PNG"]
	, {p, plots}];
];

(* Plot evolution of the capacity with respect to a calibration period
   info: string (C trafo transformer)
   cperiod cunit: calibration period
   unit: unit used in the plot
*)

PlotCapTrend[info_, cperiod_, cunit_, unit_] := 
  Module[{res, res2, res3, i, j, t, cal, ical, xcal},
   res = DSLib`TailDataLog[10, "Years"];
   xcal = Get["DATASETS/calibration.txt"];
   ical = DSLib`NextIndex[res, 1, cperiod, cunit];
   cal = Mean[res[[1 ;; ical]][[All, info, 2]]];
   res2 = Table[ {r["date"], r[info]}, {r, res}];
   res3 = {};
   i = 1;
   t = Partition[ 
     NestWhileList[ DSLib`NextIndex[res, #, 1, unit] &, 1, # < Length[res] & ], 2, 1];
   Do[ AppendTo[res3, 
      Thread[{res2[[j[[1]], 1]], 
        Mean[Take[res2, j][[All, 2]] ]/cal   }]  ];, {j, t}];
   Return[
    DateListStepPlot[Transpose@res3, 
     Filling -> {1 -> {{3}, Directive[LightBlue]}}, 
     PlotStyle -> {Dotted, Black, Dotted}, ImageSize -> "Large" , 
     FrameLabel -> "Relative Variation", 
     PlotLabel -> 
      "Reference is C=" <> ToString[cal/xcal[info]] <> " pF"]]
    ];

FAPIPlotCapTrend := Module[{f, par},
    par = { "Transformer" -> {"1", "2"},
    "Bushing" -> {"H1" -> "A230", "H2" -> "B230", "H3" -> "C230", 
    "X1" -> "A138", "X2" -> "B138", "X3" -> "C138"}, 
    "Reference" -> <|"Interpreter" -> "Number", 
    "Control" -> Function[InputField[##, FieldSize -> 5]]|>, 
    "Unit" -> {"Days", "Weeks", "Months"},
    "Step" -> {"Days", "Weeks", "Months"}
    };

    f = FormObject[par, 
        AppearanceRules -> <|"ItemLayout" -> "Vertical"|>, 
        FormLayoutFunction -> Function[
            Row[{#["Transformer"], Spacer[5], #["Bushing"], 
            Spacer[5], #["Reference"], Spacer[2], #["Unit"], 
            Spacer[5], #["Step"]}]
        ] 
    ];

    Return@FormPage[f, PlotCapTrend[
        "C" <> #Bushing <> #Transformer, #Reference, #Unit, #Step] &][<| 
        "Transformer" -> "1", "Bushing" -> "H1", "Reference" -> 1, 
        "Unit" -> "Weeks", "Step" -> "Weeks"|>]
];

(* Plot evolution of the dissipation factor with respect to a calibration period
   info: string (C trafo transformer)
   cperiod cunit: calibration period
   unit: unit used in the plot
*)

PlotDFTrend[info_, cperiod_, cunit_, unit_] := 
    Module[{info2, cal, data, iref, ref, f, fcal, t, res, fref},
    info2 = StringDrop[info, -1]  <> If[StringEndsQ [info, "1"], "2", "1"];
    cal = Get["DATASETS/calibration.txt"];
    
    fcal[r_] := <|  info -> Tan[(90 - r[info] + cal[info])*Pi/180]*100, 
                    info2 -> Tan[(90 - r[info2] + cal[info2])*Pi/180]*100   |>;

    fref[r_] := (r[info] /ref[info])/(r[info2]/ref[info2]) ;

    (* f[a_,b_] := fcal/@(data[[a;;b,info,2]] - (#-Mean[#]) &[data[[a;;
    b,info2,2]] ]); *)
   
    f[a_, b_] :=  fref[fcal[#]] & /@  data[[a ;; b, {info, info2}, 2]];
   
    data = DSLib`TailDataLog[10, "Years"];
    iref = DSLib`NextIndex[data, 1, cperiod, cunit];
    ref =  Mean[fcal /@ data[[1 ;; iref, {info, info2}, 2]]];    

    t = Partition[ 
        NestWhileList[ DSLib`NextIndex[data, #, 1, unit] &, 
        1, # < Length[data] & ], 2, 1];
   
    res = {};
    Do[ 
        AppendTo[res, Thread[{data[[First[j], 1]], 
        Quantile[(f @@ j), {0.1, 0.5, 0.9}]  }]  ];
    , {j, t}];

    Return@DateListStepPlot[Transpose@res, PlotRange -> Full, 
        Filling -> {1 -> {{3}, Directive[LightBlue]}}, 
        PlotStyle -> {Dotted, Black, Dotted}, 
        PlotLabel -> "Reference Bushing 1 = " <> ToString[ref[info]] <> 
                     " and Bushing 2 = " <> ToString[ref[info2]], 
        ImageSize -> "Large" , FrameLabel -> "Relative variation"];
];

FAPIPlotDFTrend := Module[{par,f},
    par = {
        "Transformer" -> {"1", "2"},
        "Bushing" -> {"H1" -> "A230", "H2" -> "B230", "H3" -> "C230", 
        "X1" -> "A138", "X2" -> "B138", "X3" -> "C138"}, 
        "Reference" -> <|"Interpreter" -> "Number", 
        "Control" -> Function[InputField[##, FieldSize -> 5]]|>, 
        "Unit" -> {"Days", "Weeks", "Months"},
        "Step" -> {"Days", "Weeks", "Months"}};

    f = FormObject[par, 
        AppearanceRules -> <|"ItemLayout" -> "Vertical"|>, 
        FormLayoutFunction -> Function[
            Row[{#["Transformer"], Spacer[5], #["Bushing"], 
            Spacer[5], #["Reference"], Spacer[2], #["Unit"], 
            Spacer[5], #["Step"]}]
        ]  
    ];

    Return[FormPage[f, 
        PlotDFTrend[
        "F" <> #Bushing <> #Transformer, #Reference, #Unit, #Step] &][<| 
        "Transformer" -> "1", "Bushing" -> "H1", "Reference" -> 1, 
    "Unit" -> "Weeks", "Step" -> "Weeks"|>]];
];

End[]

