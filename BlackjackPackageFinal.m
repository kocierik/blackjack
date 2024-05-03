(* ::Package:: *)

BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];


playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";

Begin["`Private`"];


playBlackjack[] := 
DialogInput[
 DynamicModule[{playerName = "", phase = "chooseName", inputValue = "", actualSeed=0, dealerScore=0, playerScore=0, winner=""},
 
  (* Pulizia della console: rimuovendo questo comando il codice si bugga *)
  ClearSystemConsole[];
  playingcardgraphic = ResourceFunction["PlayingCardGraphic"]; (* Funzione per convertire numeri in carte da gioco *)
  (* Inizializzazione del punteggio del giocatore *)
  valutaCarta[x_/;(x>0||x!=0)] := Print[x];
  valutaCarta[x_/; x<=0] := Print[x+1];
  (* Inizializzazione del punteggio del giocatore *)
  (*playerScore = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[playerHand,13]];  (* asso=11 *)*)
  playerScore = Total[
  Map[valutaCarta, Mod[playerHand, 13]]
  
  ];
  
  (* Funzione per calcolare il punteggio *)
  calculateScore[hand_List] :=
   Module[{score},
    score = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[hand,13]];
    If[MemberQ[Mod[hand,13],1]&&score>21,score-=10];
    score];
  (* Inizializzazione del mazzo di carte *)
  deck = playingcardgraphic[Range@52];
  quitgame = False;
(* Inizializzazione delle mani del giocatore e del dealer *)
  playerHand = RandomSample[Range@52,2];
  dealerHand = RandomSample[Complement[Range@52,playerHand],2];
  dealerHand1 = dealerHand[[1]];

  (* Funzione per determinare il vincitore *)
  determineWinner[] :=
    Module[{},
        Switch[True,
            playerScore > 21, "dealer",                 
            playerScore == dealerScore, "pareggio",    
            dealerScore > 21 || playerScore > dealerScore, "player",   
            True, "dealer"                                         
        ]
    ];
  Dynamic[
   Switch[phase,
    "chooseName",
    Column[{
      TextCell["Inserisci il nome del giocatore:", FontSize -> 18],
      InputField[Dynamic[playerName], String, ImageSize -> {200, 50}, BaseStyle -> {FontSize -> 18}],
      
      Button[Style["Procedi", 18, Bold], (If[playerName === "", playerName = "Giocatore ignoto"]; phase = "chooseSeed"), ImageSize -> {100, 50}]
      }],
    "chooseSeed",
    Column[{
      Row[{TextCell["Inserisci un numero intero da utilizzare come seed:", FontSize -> 12],
      InputField[Dynamic[inputValue], Number, ImageSize -> {100, 30}, BaseStyle -> {FontSize -> 14}],
      }],Button[Style["Procedi", 12, Bold], 
      (
      phase = "playerTurn"; 
      If[inputValue != "", 
    actualSeed = inputValue;
    SeedRandom[inputValue],
    actualSeed = RandomInteger[1000000];
    SeedRandom[actualSeed]
      ]
     ), 
  ImageSize -> {100, 30}]
      }],
    "playerTurn", 
     Column[{
     Row[{TextCell["Seed attuale: " <> ToString[actualSeed], "Text"]}],
            Grid[{
            {TextCell["La mano di " <> ToString[playerName] <> ": ", "Text"], 
              TextCell["La mano del dealer: ", "Text"]},
            {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
              playingcardgraphic[{0, dealerHand1}, "CardSpreadAngle" -> 0.1]},
              dealerHandValue = Mod[dealerHand[[1]], 13];
            {TextCell["Il tuo punteggio totale \[EGrave]: " <> ToString[calculateScore[playerHand]], "Text"], 
              TextCell["Il punteggio totale del dealer \[EGrave]: " <> ToString[If[(dealerHandValue>10)||(dealerHandValue===0),10,If[dealerHandValue==1,11,dealerHandValue]]], "Text"]}
            },
            Spacings -> {20, 2}
          ], 
            Row[{
            Button[Style["Chiedi Carta", 18, Bold],
            
             AppendTo[playerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
             playerScore = calculateScore[playerHand];
             If[playerScore > 21,
             winner="dealer";phase="determineWinner", 
             phase="playerTurn"],
        
            ImageSize -> {200, 50}], (* Aumenta la dimensione del pulsante *)
          Button[Style["Stai", 18, Bold],
          phase="dealerTurn"
          
          , ImageSize -> {200, 50}], 
          (* Pulsante per cominciare una partita da capo con lo stesso seed*)
          Button[Style["Ricomincia Partita", 18, Bold], 
          playBlackjack[];
          
          , ImageSize -> {200, 50}]
          }]
     }],
   "dealerTurn",
Dynamic@Column[{
   dealerScore = calculateScore[dealerHand];
   While[calculateScore[dealerHand] < 17,
     AppendTo[dealerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
     If[calculateScore[dealerHand] > 21, winner = "player"; phase = "determineWinner"]];
   
   Row[{TextCell["Seed attuale: " <> ToString[actualSeed], "Text"]}],
   Grid[{
          (* Intestation row *)
          {TextCell["LE CARTE DI " <> ToString[playerName] <> ": ", "Text"],
            TextCell["LE CARTE DEL DEALER: ", "Text"]},
          (* Cards row *)
          {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
            playingcardgraphic[dealerHand, "CardSpreadAngle" -> 0.1]},
          (* Scores row *)
          {TextCell["Il tuo punteggio totale \[EGrave]: " <> ToString[calculateScore[playerHand]], "Text"], 
          TextCell["Il suo punteggio totale \[EGrave]: " <> ToString[calculateScore[dealerHand]], "Text"]}
        }, Spacings -> {10, 1}],
       phase="determineWinner";
}],


   "determineWinner",
   
   Column[{
   Grid[{
          (* Intestation row *)
          {TextCell["LE CARTE DI " <> ToString[playerName] <> ": ", "Text"],
            TextCell["LE CARTE DEL DEALER: ", "Text"]},
          (* Cards row *)
          {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
            playingcardgraphic[dealerHand, "CardSpreadAngle" -> 0.1]},
          (* Scores row *)
          {TextCell["Il tuo punteggio totale \[EGrave]: " <> ToString[calculateScore[playerHand]], "Text"], 
          TextCell["Il suo punteggio totale \[EGrave]: " <> ToString[calculateScore[dealerHand]], "Text"]}
        }, Spacings -> {10, 1}],
        (* Winner message *)
        Grid[{
          {If[(winner === "dealer") || (dealerScore > playerScore),
            TextCell["HAI PERSO\nIL DEALER VINCE", "Text", FontColor -> Red, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20], 
            If[(winner === "player") || (playerScore > dealerScore), 
              TextCell["HAI VINTO!", "Text", FontColor -> Green, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20],
              TextCell["PAREGGIO!", "Text", FontColor -> Orange, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20]]]}
        }],
        (* Buttons grid *)
        Grid[{
          {
            Button[Style["Nuova Partita", 18, Bold], DialogReturn["NewGame"], ImageSize -> {200, 50}],
            Button[Style["Ricomincia partita", 18, Bold], DialogReturn["Restart"], ImageSize -> {200, 50}], 
            Button[Style["Cambia giocatore", 18, Bold], DialogReturn["chooseCharacter"], ImageSize -> {200, 50}], 
            Button[Style["Esci", 18, Bold], DialogReturn["Quit"], ImageSize -> {200, 50}]
          }
        }]
        
        }]
    ]


    
    
    ,
   DefaultButton[]
   ]
  ],
 TextAlignment -> {Center, Center},
 WindowSize -> {Scaled[1], Scaled[1]}
 ]

End[];




(* main code starting *)
Off[Lookup::invrl];
(*chooseSeed[];*)
playBlackjack[];

EndPackage[];

