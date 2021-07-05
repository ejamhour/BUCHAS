#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["PhLib.wl"];
Get["DSLib.wl"];
Get["APILib.wl"];

(* DSLib`BuildDatasetIndex["index.txt", "BATEIAS", True]; *)
Print[ DSLib`DatasetInfo["index.txt"] ];

If[ 
    DirectoryQ["DATASETS"] && FileExistsQ["DATASETS\\calibration.txt"],
    Print["Updating dataset ... "];
    DSLib`BuildDatasetHourly["index.txt"],
    Print["Building dataset"];
    If[ !DirectoryQ["DATASETS"], CreateDirectory["DATASETS"]];
    DSLib`BuildDatasetHourly["index.txt", False];
    DSLib`RunCalibration[1,"Day", DSLib`calibration, {}, True];
];

Print["Updating DASHBOARD..."];
APILib`PNGShowLastMeasure["index.txt"];
APILib`PNGPlotRawData["index.txt"]; 
APILib`PNGPlotBushDataLog[ 7*24, "Hours"];

Print["DASHBOARD was updated"];



