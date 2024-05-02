(* ::Package:: *)

(* ::Section:: *)
(*Funciones para volver el NoteBook => texto plano*)


(* ::Subsection:: *)
(*Functions to enforce cell ID generation*)


(* ::Text:: *)
(*If this is not done before cells are added to the notebook some of the cell reference methods can fail.*)
(*This could be strictly enforced, checking all cells for ID at runtime, and replacing those without. I have not implemented this.*)


(* ::Input:: *)
(*checkCellIDSetting:=TrueQ[CurrentValue[targetNotebook,CreateCellID]]*)
(*checkCellIDSetting*)


(* ::Subsection:: *)
(*Specify notebook function, here the current evaluation book*)


(* ::Input:: *)
(*targetNotebook:=EvaluationNotebook[]*)


(* ::Subsection:: *)
(*Specify the cells targeted, here the contents of each 'title' style super cell*)


(* ::Input:: *)
(*BodyCellFilter=Table[#[[1,p[[1]]+1;;p[[2]]-1]],{p,#[[2]]}]&@{#,Transpose[{Drop[#,-1],Drop[#,1]}]&@Append[Position[(NotebookRead/@#)[[;;,2]],"Title",1][[;;,1]],0]}&;*)


(* ::Subsection:: *)
(*Cell selection functionality*)


(* ::Input:: *)
(*getBodyInputCells:=Join@@BodyCellFilter@Cells[CellStyle->{"Input"|"Title"}];*)
(*getBodyOutputCells:=Join@@BodyCellFilter@Cells[CellStyle->{"Output"|"Title"}];*)
(*getBodyContentCells:=Join@@BodyCellFilter@Cells[CellStyle->{"Text"|"Output"|"Title"}];*)


(* ::Input:: *)
(*cellContentLog[val_]:={};*)
(*cellResultLog[val_]:={};*)
(*getOutOfDateCells:=Pick[#,Not[TrueQ[cellContentLog[#[[1]]]==#[[2]]]]&@{CurrentValue[#,CellID],NotebookRead[#][[1]]}&/@#]&@getBodyInputCells;*)
(**)
(*addTagToCells[cells_,tag_]:=SetOptions[#,CellTags->Union[If[Depth[#]==1,{#},#]&@CurrentValue[#,CellTags],{tag}]]&/@cells*)
(*removeTagFromCells[cells_,tag_]:=SetOptions[#,CellTags->Complement[If[Depth[#]==1,{#},#]&@CurrentValue[#,CellTags],{tag}]]&/@cells*)
(*applyFunctionToCellList[cellList_,uniqueTag_,function_]:=( *)
(*addTagToCells[cellList,uniqueTag];*)
(*NotebookLocate[uniqueTag];*)
(*removeTagFromCells[cellList,uniqueTag];*)
(*function[targetNotebook];)*)
(*goToSpecificInput[targetString_]:=( *)
(*addTagToCells[Select[#,NotebookRead[#][[1]]==BoxData[targetString]&][[;;1]]&@Cells[CellStyle->{"Input"}],targetString];NotebookLocate[targetString];*)
(*removeTagFromCells[Select[#,NotebookRead[#][[1]]==BoxData[targetString]&][[;;1]]&@Cells[CellStyle->{"Input"}],targetString];*)
(*)*)
(*deploySubsequentCell[targetString_,functionString_]:=( *)
(*goToSpecificInput[targetString];*)
(*SelectionMove[EvaluationNotebook[],After,Cell];*)
(*NotebookWrite[EvaluationNotebook[],BoxData[functionString],After];*)
(*goToSpecificInput[functionString];*)
(*SelectionEvaluateCreateCell[targetNotebook];)*)


(* ::Subsection:: *)
(*Plaintext conversion functions*)


(* ::Input:: *)
(*updateBook:=( *)
(*cellsToUpdate=getOutOfDateCells;*)
(*applyFunctionToCellList[cellsToUpdate,"tagged input",SelectionEvaluateCreateCell];*)
(*deploySubsequentCell["updateBook","loadBook"];*)
(*deploySubsequentCell["loadBook","compileBook"];*)
(*deploySubsequentCell["compileBook","exportBook"];*)
(*deploySubsequentCell["exportBook","cleanBook"];*)
(*)*)
(**)
(*blankFunction=#&;*)
(*loadBook:=Module[{updateResultPairs},*)
(*updateResultPairs=Transpose[{cellsToUpdate,getBodyOutputCells}];*)
(*(cellContentLog[CurrentValue[#[[1]],CellID]]=NotebookRead[#[[1]]][[1]])&/@updateResultPairs;*)
(*(cellResultLog[CurrentValue[#[[1]],CellID]]=NotebookRead[#[[2]]])&/@updateResultPairs;*)
(*applyFunctionToCellList[updateResultPairs[[;;,2]],"tagged output",NotebookDelete];*)
(*(applyFunctionToCellList[{#},"tagged input",blankFunction];*)
(*SelectionMove[targetNotebook,After,Cell];*)
(*NotebookWrite[targetNotebook,cellResultLog[CurrentValue[#,CellID]],All];*)
(*)&/@getBodyInputCells;]*)
(*(*--- Despejo el titulo ---*)*)
(*titleSplit:={#[[1]]/.Cell[t_,"Title",___]:>t,#[[2;;]]/.{Cell[t_,"Text",___]:>t,Cell[BoxData[t_],"Output",___]:>ToExpression[t]}}&/@(Table[#[[1,p[[1]];;p[[2]]]],{p,#[[2]]}]&@{#,Transpose[{Drop[#,-1],Drop[#,1]-1}]&@Append[Position[#,Cell[_,"Title",___],1][[;;,1]],0]})&@Cases[NotebookGet[EvaluationNotebook[]],Cell[_,"Text",___]|Cell[_,"Output",___]|Cell[_,"Title",___],\[Infinity]];*)
(*compileBook:=( *)
(*(titles=#[[1]]&/@#;plainTexts=StringJoin[#[[2]]]&/@#;)&@titleSplit;*)
(*applyFunctionToCellList[getBodyOutputCells,"tagged output",NotebookDelete];*)
(*)*)
(*cleanBook:=( *)
(*applyFunctionToCellList[getBodyOutputCells,"tagged output",NotebookDelete];*)
(*goToSpecificInput["loadBook"];NotebookDelete[EvaluationNotebook[]];*)
(*goToSpecificInput["compileBook"];NotebookDelete[EvaluationNotebook[]];*)
(*goToSpecificInput["exportBook"];NotebookDelete[EvaluationNotebook[]];*)
(*goToSpecificInput["cleanBook"];NotebookDelete[EvaluationNotebook[]];*)
(*goToSpecificInput["updateBook"];*)
(*)*)
