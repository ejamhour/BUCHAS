(* ::Package:: *)

Begin["DSLib`"]; 

calibration = <|
 "FA1381" -> 0.36, "FB1381" -> 0.40, "FC1381" -> 0.40, 
 "FA2301" -> 0.38, "FB2301" -> 0.33, "FC2301" -> 0.30,
 "FA1382" -> 0.46, "FB1382" -> 0.37, "FC1382" -> 0.36, 
 "FA2302" -> 0.35, "FB2302" -> 0.39, "FC2302" -> 0.36,
 "CA1381" -> 230, "CB1381" -> 240, "CC1381" -> 240, 
 "CA2301" -> 287, "CB2301" -> 283, "CC2301" -> 290,
 "CA1382" -> 231, "CB1382" -> 230, "CC1382" -> 230, 
 "CA2302" -> 277, "CB2302" -> 296, "CC2302" -> 290
 |>;

(* Sort the cisei_files by the aquisition date 
   files = list of file names correspoding to cise_files
   RETURN = list of date->file ordered by Date
-----------------------------------------------------*)
SortFiles[files_] := Module[ {x, dates={}},
	Do[
		AppendTo[dates, 
		DateObject[StringReplace[ FileNameTake[f], {"cisei_"|".txt"->"", "_"->":"}]]->f];
	,{f,files}];

	dates = DeleteCases[ dates, x_ /; StringMatchQ[ x[[2]], __~~".lock"]];
	Return[ SortBy[ dates, First]];
];

(* Build an index of date->filename for all files in a Directory
   dataindex = filename of the index
   dataset = directory with the cise_files
   new = create (TRUE) or append (FALSE:default) the dataset to the index
   RETURN = last entry of the new index file
-----------------------------------------------------*)
BuildDatasetIndex[dataindex_, dataset_, new_:False] := Module[  {files, df}, 
	If[ !DirectoryQ[dataset],  Return["Can't open dataset directory" ]];
	files = SortFiles[FileNames[All,dataset]];
	df = If[new,OpenWrite[dataindex], OpenAppend[dataindex]];
	Do[ Write[df, f], {f, files}];
	Close[df];
	Return[files[[-1]]];
];

(* Add an entry corresponding to a new cise_file to an index file
   dataindex = filename of the index
   date = date of the entry (string format)
   file = filename of the cisei_file   
-----------------------------------------------------*)
UpdateDatasetIndex[dataindex_, date_, file_] := Module[{df, do},
	If[ !FileExistsQ[dataindex],  Return["Can't open index file" ]];
	df = OpenAppend[dataindex];
	do = ToExpression[StringSplit[date,","]];
	Write[df, DateObject[do]->file];
	Close[df]
];

APIUpdateDatasetIndex = APIFunction[{"datasetindex" -> String, "date" -> String, 
    "file" -> "String"}, 
   UpdateDatasetIndex[#datasetindex, #date, #file] &];

(* Returns the date interval corresponding to the dataset
	dataindex = filename of the index
	RETURN = string with the interval information
------------------------------------------------------*)
DatasetInfo[dataindex_] := Module[{index},
    If[ !FileExistsQ[dataindex],  Return["Can't open index file" ]];
    index = ReadList[dataindex];
    Return[ToString[StringForm["From: `` to: `` ", DateString[index[[1,1]]],DateString[index[[-1,1]] ] ] ]];
];

(*  Next dataset index to complete a timespan 
	ds = list of date->cisefile entries (e.g. bateias.txt)
	i = initial position in ds
	quantity and period = time span to the next index
	RETURN = next index
-----------------------------------------------------*)
NextIndex[ ds_, i_, quantity_, period_] := Module[{index, t1, t2, max},
	max = Length[ds];
	t1 = ds[[i,1]];
	t2 =   t1 + Quantity[quantity,period];
	index = i ;
	While[  t2 > ds[[index,1]] && index < max, index +=1 ];
	Return[index];
];

(* Process dataset entries into a single register 
	ds = list of date->cisefile entries corresponding to a timespan of 1 hour
	RETURN = an association with phasor and capacitance data
------------------------------------------------------*)
HourRegister[ds_] := Module[{myds, keys, reg, d, de, bdata},
	myds = {};
	Do[  AppendTo[myds, ReadList[ f[[2]]] ], {f,  ds}];
	
	keys = Keys@ReadList[ds[[1,2]]][[1]][[3;;]];
	reg = { "date"->ds[[1,1]] };

	Do[
		bdata =Flatten@ Table[  Table[de[k], {de,d}],{d, myds} ];
		AppendTo[reg, k->Quantile[ bdata,{0.25,0.5,0.75}]];
	, {k,keys}];
	
	If[Length[reg] == 1, Abort[]];
	Return[Association[reg]];
];

(* Creates an hourly dataset from raw dataset entries for a single month
	ds = list of date->cisefile entries
	index = points to the first entry to be processed. Stops automatically at the end of the month.
	CREATE = a file YEAR-MONTH in the DATASETS directory
	RETURN = an association with finame->{indexes of the last register}
    OBS. DATASETS folder must be created manually befor calling this function
----------------------------------------------------------*)

BuildMonthDataset[ds_, index_] := Module[{t1,file, df, i, j, max,a, b},
	t1 = ds[[index,1]];
	file = FileNameJoin[{"DATASETS",DateString[t1,{"Year","-","Month"}]}];
	df = OpenAppend[file];
	a = b = i= index;
	max = Length[ds];
	While[ True, 
		j  = NextIndex[ds, i, 1, "Hours"];
		If[ i < j < max, 
			a = i;
			b= j -1;
			Write[df, HourRegister[ds[[a;;b]]] ]; 
			i = j;
			,
			Break[];
		];
		If [ DateObject[ds[[j,1]],"Month"] !=  DateObject[t1,"Month"], Break[]];
	]; 
	Close[df];
	Return[<| "file"->file, "last"->{a, b}|>];
];

(* Creates an hourly dataset from raw dataset entries 
    dataset = filename with the date->cisefile entries
    update = TRUE (default): update the dataset with missing entries, FALSE : rebuild all datasets
    CREATE = files in the DATASETS directory
	PROBLEM: If dataset has less than two full hours
-----------------------------------------------------*)
BuildDatasetHourly[dataset_, update_:True]  := Module[{count, ds,max,i,if,res, date, f},
	ds = ReadList[dataset];
	max = Length[ds]; 
	
	If[update && FileExistsQ["DATASETS/index.txt"],
		if = ReadList["DATASETS/index.txt"];
		res = Last[if]["last"];
		i = res[[2]] + 1;
		, 
		DeleteFile /@ FileNames[All,"DATASETS"];
		if ={ <|"date"->DateObject[ds[[1,1]],"Month"] |>};
		i = 1;
	];
	count = 0;

	While[ True,
		res = BuildMonthDataset[ds, i];
		Print[res];
		count +=1; If[count > 12, Print["LOOP DETECTED"]; Return[{"IVFP",res}] ];

		If[res["last"][[2]] == max || res["last"][[1]] == res[["last"]][[2]], Break[] ];
		AssociateTo[if[[-1]] , res];
		i = res["last"][[2]] + 1;
		date =DateObject[ds[[i,1]],"Month"];
		If[ date != if[[-1]]["date"], AppendTo[if,  <|"date"->date |> ] ]; 
	];

	f = OpenWrite[FileNameJoin[{"DATASETS","index.txt"}]];
	Do[ Write[f, rif], {rif,if}];
	Close[f];
];

(* Retrieve a set of HourlyDataset entries backwards 
	period and unit = time span to the next index
	RETURN = set of registers
*)
TailDataLog[period_, unit_] := Module[{data, last = Null, res={}, files, i},
	files = Reverse@ReadList["DATASETS/index.txt"] ;
	Do[
		data = ReadList[f["file"]];
		If[ last == Null, last = data[[-1]]["date"]];
		i = Length[data];
		While[ Quantity[period,unit] >= (last - data[[i]]["date"]) && i > 1, i -= 1];
		res = Join[ data[[i;;]],res];
		If[i != 1, Break[]];
	, {f,files}];
	Return[res];
];

(* Retrieve a set of entries forward 
	period and unit = time span to the next index
	RETURN = set of registers
*)
HeadDataLog[period_, unit_] := Module[{data, first=Null, res={}, files, i},
	files = ReadList["DATASETS/index.txt"] ;
	Do[
		data=ReadList[f["file"]];
		i = 1;
		If[ first == Null, first = data[[1]]["date"]];

		While[ Quantity[period,unit] >= ( data[[i]]["date"]-first) && i < Length[data], i += 1];
		res = Join[ data[[1;;i]],res];
		If[i !=Length[data],  Break[]];
		, {f,files}];
		Return[res];
];

(* Calculate the calibration constants of DF and Capacitance
   period, unit: calibration period when dataset is not supplied
   target: values of Capacitance and Dissipation Factor
   ds: Hourly dataset from DATASETS files (Optional)
   save: creates calibration.txt file in the DATASETS folder
*)

RunCalibration[period_, unit_, target_, ds_:{}, save_:False] := Module[{cal, res, ical},
   
	res = If[Length[ds] > 0,  ds, HeadDataLog[period, unit]];
	(* cal = Association[ Table[k->Mean[res[[All,k,2]]] , {k, Keys[target]}]]; *)

	cal = Association[ Table[k -> Median[res[[All, k, 2]]] , {k, Keys[target]}]];
	Do[ 
		If[StringStartsQ[k, "F"] ,
			AssociateTo[cal, k -> (cal[k] - ArcCos[target[k]/100]*180/Pi)],
			AssociateTo[cal, k -> (cal[k]/target[k])] ]
    , {k, Keys[target]}];

	If[ save, Put[cal, FileNameJoin[{"DATASETS", "calibration.txt"}]] ]; 

	Return[cal];
];

(* Runs as an Schedule Task
   dataindex: filename of the list of date->cisei-files
   time: maximum allowed time in minutes to receive an update from substation
   notify: send alert emails
   Updates HourlyDataset
   Check system health and notify status by email
*)

SystemStatus[dataindex_, time_:120, notify_:False] := Module[{t1, t2, ok, nok, msg, alert},
   	BuildDatasetHourly[dataindex];

   	t1 = If [ $OperatingSystem  == "Windows", 
        FileDate[dataindex, "Modification"],
     	FileDate[dataindex, "Change"]
    ]; 

   	t2 =  TimeZoneConvert[Now, -3] - t1 ;

    alert =  If [ t2 > Quantity[time, "Minutes"], True, False ]; 

    nok = { "ejamhour@gmail.com",
            "Bushing Monitoring System ALERT", 
      	    "SYSTEM IS DOWN FOR MORE THAN " <> ToString[t2] <> "." <> 
            "\n\nPlease, check the system health using RDP." };
            
    ok = {  {"ejamhour@gmail.com, i.chuiri@pucpr.br"}, 
            "Bushing Monitoring System STATUS",
            "SYSTEM FULLY OPERATIONAL.\n\n" <>
            "Last update received at " <> DateString[t1] <>
        	"\n\nYou may check the system data through the link:" <>  
            "https://www.ppgia.pucpr.br/~jamhour/teste.html" };
           
   	If [ notify && alert, SendMail[ nok[[1]], nok[[2;;]]] ];
    If [ notify && !alert && StringMatchQ[ DateString[TimeZoneConvert[Now, -3], {"Hour"}] , "06"], 
        SendMail[ ok[[1]], ok[[2;;]]] ];
    
    Return[If[alert, nok[[3]], ok[[3]]]];
    
   ];


End[];


