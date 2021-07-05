#!/usr/bin/env wolframscript
(* ::Package:: *)

(* $ScriptCommandLine[[1]] is the script name *)

confFile = If[ Length[$ScriptCommandLine] < 2, "phasors.conf", $ScriptCommandLine[[2]] ];

If[ !FileExistsQ[confFile], Print["Configuration file " <> confFile <> " not found"]; Pause[0.1]; Abort[]];

conf = Get[confFile];

If[ conf["cloud"], Print[" Data will be saved to the cloud"]; Pause[0.1]];

Print["First file to be processed: ", conf["file"]];
Print["Phasor files per cloudfile: ", conf["numfiles"]]; Pause[0.1];	
If[ conf["limit"] > 0, 
    Print["Maximum files processed by round: ", conf["limit"]]; Pause[0.1]; ]; 

Get["PhLib.wl"];

conf["file"] = PhLib`CreateDataset[conf["file"], conf["dataset"], "numfiles"->conf["numfiles"], "limit"->conf["limit"]];

Put[conf, "phasors.conf"];

Print["Next file: ", conf["file"]];


if[ conf["dashboard"],
    Print["Updating DASHBOARD ..."];
    file = FileNameTake[conf["file"]];
    i0 = ToExpression[StringCases[file, NumberString]][[1]];
    file = FileNameJoin[ {
        DirectoryName[conf["file"]], 
        StringReplace[file, ToString[i0] -> ToString[i0 - 1]]  }];
    PhLib`PNGPolarDiagram[ PhLib`scale[Last[PhLib`ReadPhasor[file]]] ];
    Print["DASHBOARD was updated"];
];

(*
If[ IvanCloud, 
    file3 = StringReplace[dsfile, {"\\" -> "//"}];
    CopyFile[ dsfile, CloudObject[file3]];
    URLExecute[CloudObject["https://www.wolframcloud.com/obj/i.chueiri/update"],
    {"datasetindex"->"bateias.txt", "date"->date, "file"->file3}] 
];
*)

