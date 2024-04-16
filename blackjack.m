(* ::Package:: *)

BeginPackage["Blackjack`"]

playBlackjack::usage = "playBlackjack[] avvia il gioco del Blackjack."

Begin["`Public`"]

(* Mazzo di carte *)
initDeck[] := 
	deck = Flatten[Table[{11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10}, {4}], 1];
    Return[RandomSample[deck]];

(* Inizializzazione della mano del giocatore  *)
initPlayerHand[deck_] := Return[RandomSample[deck, 2]];
	
(* Inizializzazione della mano del dealer *)
initDealerHand[deck_, playerHand_] := RandomSample[Join[deck, playerHand], 2];

(* Calcolo punteggio della mano data in input *)
calculateScore[hand_List] :=
    Return[
    If[MemberQ[hand, 11] && Total[hand] > 21,
        Total[hand]-10,    (* then *)
        Total[hand]]];     (* else *)
        
(* Funzione per mostrare le carte *)
showCards[hand_List] := StringJoin[Riffle[ToString /@ hand, ", "]];

(* Turno del giocatore *)
playerTurn[deck_, playerHand_, dealerHand_] := 
	Module[{decision},
    decision = DialogInput[
      DialogNotebook[{TextCell[
         "Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[calculateScore[playerHand]], "Text"], 
         TextCell["La carta del dealer \[EGrave]: " <> ToString[dealerHand[[1]]] <> ". Totale: " <> ToString[dealerHand[[1]]], "Text"], 
         TextCell["'hit' o 'stand'?", "Text"], 
         Grid[{{Button[Hit, DialogReturn["Hit"], 
            Background -> {Darker[LightBlue, 0.2], Lighter[LightBlue]}, 
            BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}], 
            Button[Stand, DialogReturn["Stand"], 
            Background -> {Darker[LightGreen, 0.2], Lighter[LightGreen]}, 
            BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}]}}]}]];
    If[decision === "Hit",
     AppendTo[playerHand, RandomChoice[Complement[deck, playerHand, dealerHand]]];
     If[calculateScore[playerHand] > 21, DialogReturn["bust"], playerTurn[deck, playerHand, dealerHand]];   (* else: richiama la funzione: nuovo turno *)
     If[decision === "Stand", Return[playerHand]]]];

(* Turno del dealer *)
dealerTurn[deck_, playerHand_, dealerHand_] :=
      dealerHand = Join[dealerHand, RandomChoice[Complement[deck, dealerHand, playerHand]]];
      While[calculateScore[dealerHand] < 17,
          AppendTo[dealerHand, RandomChoice[Complement[deck, dealerHand, playerHand]]];
          If[calculateScore[dealerHand] > 21, Break[]]];
      Return[dealerHand];

(* Determina vincitore *)
determineWinner[playerScore_, dealerScore_] :=
    If[playerScore > 21, Return["Il dealer vince!"]];
    If[playerScore == dealerScore, Return["Pareggio!"]];
    If[dealerScore > 21 || playerScore > dealerScore, Return["Hai vinto!"], Return["Il dealer vince!"]];

End[]

EndPackage[]

