BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];


playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";

Begin["`Private`"];

playBlackjack[seed_: Automatic] :=
 Module[{deck, playerHand, dealerHand, playerScore, dealerScore, 
   playerDecision, dealerDecision, winner},

  (* Impostazione del seed se specificato dall'utente *)
  If[seed =!= Automatic, SeedRandom[seed]];

  (* Inizializzazione del mazzo di carte *)
  deck = Flatten[
    Table[{11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10}, {4}], 1];

  (* Mescolamento del mazzo *)
  deck = RandomSample[deck];

  (* Inizializzazione delle mani del giocatore e del dealer *)
  playerHand = RandomSample[deck, 2];
  dealerHand = RandomSample[Complement[deck, playerHand], 2];

  (* Inizializzazione del punteggio del giocatore *)
  playerScore = Total[playerHand];

  (* Funzione per calcolare il punteggio *)
  calculateScore[hand_List] :=
   Module[{score},
    score = Total[hand];
    If[MemberQ[hand, 11] && score > 21, score -= 10];
    score];

  (* Funzione per mostrare le carte *)
  showCards[hand_List] := StringJoin[Riffle[ToString /@ hand, ", "]];

  (* Funzione per il turno del giocatore *)
  playerTurn[] :=
   Module[{decision},
    decision = DialogInput[
      DialogNotebook[{TextCell[
          "Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[playerScore], "Text"], 
         TextCell["La carta del dealer è: " <> ToString[dealerHand[[1]]] <> ". Totale: " <> ToString[dealerHand[[1]]], "Text"], 
         TextCell["HIT o STAND?", "Text"], 
         Grid[{{Button["Hit", DialogReturn["Hit"], 
            Background -> {Darker[LightBlue, 0.2], Lighter[LightBlue]}, 
            BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}], 
           Button["Stand", DialogReturn["Stand"], 
            Background -> {Darker[LightGreen, 0.2], Lighter[LightGreen]}, 
            BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}]}}]}]];
    If[decision === "Hit",
     AppendTo[playerHand, RandomChoice[Complement[deck, playerHand]]];
     playerScore = calculateScore[playerHand];
     If[playerScore > 21, DialogReturn["bust"], playerTurn[]];

     If[decision === "Stand",
      dealerHand = Join[dealerHand, RandomChoice[Complement[deck, dealerHand]]];
      dealerScore = calculateScore[dealerHand];
      Null,
      DialogReturn["cancel"]]]];

  (* Funzione per il turno del dealer *)
  dealerTurn[] :=
   Module[{},
    dealerScore = calculateScore[dealerHand];
    While[dealerScore < 17,
     AppendTo[dealerHand, RandomChoice[Complement[deck, dealerHand]]];
     dealerScore = calculateScore[dealerHand];
     If[dealerScore > 21, Break[]]];
    dealerScore];

  (* Funzione per determinare il vincitore *)
  determineWinner[] :=
   Module[{},
    If[playerScore > 21, Return["Il dealer vince!"]];
    If[playerScore == dealerScore, Return["Pareggio!"]];
    If[dealerScore > 21 || playerScore > dealerScore,
     Return["Hai vinto!"],
     Return["Il dealer vince!"]]];

  (* Esecuzione del turno del giocatore *)
  playerTurn[];

  (* Esecuzione del turno del dealer *)
  dealerScore = dealerTurn[];

  (* Determina il vincitore *)
  winner = determineWinner[];

  (* Mostra il risultato finale *)(*(DialogInput[
   DialogNotebook[{TextCell[
       "Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[playerScore], "Text"], 
      TextCell["Le carte del dealer sono: " <> showCards[dealerHand] <> ". Totale: " <> ToString[dealerScore], "Text"], 
      TextCell[winner, Background -> LightBlue], 
      Button["Nuova Partita", playBlackjack[]], Button["Quit", DialogReturn[]]}]]*) 
      
  Module[{decision},
    decision = DialogInput[
      DialogNotebook[{TextCell[
          "Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[playerScore], "Text"], 
         TextCell["La carta del dealer è: " <> ToString[dealerHand] <> ". Totale: " <> ToString[dealerScore], "Text"], 
         TextCell[winner, Background -> LightBlue],
         Grid[{{Button["Nuova Partita", DialogReturn["NG"], 
            Background -> {Darker[LightBlue, 0.2], Lighter[LightBlue]}, 
            BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}], 
           Button["Quit", DialogReturn["Quit"], 
            Background -> {Darker[LightGreen, 0.2], Lighter[LightGreen]}, 
            BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}]}}]}]];
    If[decision === "NG", playBlackjack[]];
    If[decision === "Quit", DialogReturn[]]];      
      
  ]
  

End[];

EndPackage[];

