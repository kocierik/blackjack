BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];


playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";

Begin["`Private`"];

playBlackjack[playerName_, seed_: Automatic] :=
 Module[{deck, actualSeed, playerHand, dealerHand, dealerHand1, playerScore, dealerScore, 
   playerDecision, dealerDecision, winner, playingcardgraphic},

  playingcardgraphic = ResourceFunction["PlayingCardGraphic"];
  (* Impostazione del seed se specificato dall'utente. In alternativa viene generato casualmente *)
  If[seed =!= Automatic,
    actualSeed = seed;
    SeedRandom[seed],
    actualSeed = RandomInteger[1000000];
    SeedRandom[actualSeed],
  ]
  
  (* Pulizia della console: rimuovendo questo comando il codice si bugga *)
  ClearSystemConsole[];

  (* Inizializzazione del mazzo di carte *)
  deck = playingcardgraphic[Range@52];

  (* Inizializzazione delle mani del giocatore e del dealer *)
  playerHand = RandomSample[Range@52,2];
  dealerHand = RandomSample[Complement[Range@52,playerHand],2];
  dealerHand1 = dealerHand[[1]];

  (* Inizializzazione del punteggio del giocatore *)
  playerScore = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[playerHand,13]];  (* asso=11 *)

  (* Funzione per calcolare il punteggio *)
  calculateScore[hand_List] :=
   Module[{score},
    score = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[hand,13]];
    If[MemberQ[Mod[hand,13],1]&&score>21,score-=10];
    score];

  (* Funzione per mostrare le carte *)
  showCards[hand_List] := StringJoin[Riffle[ToString /@ hand, ", "]];

  (* Funzione per il turno del giocatore *)
  playerTurn[] :=
   Module[{decision},
    decision = DialogInput[
        DialogNotebook[{
            Grid[{{TextCell["Seed attuale: " <> ToString[actualSeed], "Text"]}}],
            Grid[{
            {TextCell["LE CARTE DI " <> ToString[playerName] <> ": ", "Text"], 
              TextCell["LA CARTA DEL DEALER: ", "Text"]},
            {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
              playingcardgraphic[{0, dealerHand1}, "CardSpreadAngle" -> 0.1]},
              dealerHand1 = Mod[dealerHand[[1]],13];
            {TextCell["Il tuo punteggio totale \[EGrave]: " <> ToString[calculateScore[playerHand]], "Text"], 
              TextCell["Il suo punteggio totale \[EGrave]: " <> ToString[If[(dealerHand1>10)||(dealerHand1===0),10,If[dealerHand1==1,11,dealerHand1]]], "Text"]}
            },
            Spacings -> {10, 1}
          ], 
          Grid[{{
            Button["Chiedi Carta", DialogReturn["Hit"], 
              Background -> {Darker[LightBlue, 0.2], Lighter[LightBlue]}, 
              BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}], 
            Button["Stai", DialogReturn["Stand"], 
              Background -> {Darker[LightGreen, 0.2], Lighter[LightGreen]}, 
              BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}],
            
            (* Pulsante per cominciare una partita da capo con lo stesso seed*)
            Button["Ricomincia partita", DialogReturn["Restart"], 
              Background -> {Darker[LightRed, 0.2], Lighter[LightRed]}, 
              BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}]
          }}]
        }]
      ];
    Switch[decision,
      "Hit",
        AppendTo[playerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
        playerScore = calculateScore[playerHand];
        If[playerScore > 21, DialogReturn["bust"], playerTurn[]],

      "Stand",
        AppendTo[dealerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
        dealerScore = calculateScore[dealerHand],

      "Restart",
        DialogReturn["cancel"];
        playBlackjack[playerName, actualSeed],

      _, (* default case *)
        DialogReturn["cancel"]
    ]

  ];

  (* Funzione per il turno del dealer *)
  dealerTurn[] :=
   Module[{},
    dealerScore = calculateScore[dealerHand];
    While[dealerScore < 17,
     AppendTo[dealerHand, RandomChoice[Complement[Range@52,playerHand, dealerHand]]];
     dealerScore = calculateScore[dealerHand];
     If[dealerScore > 21, Break[]]];
    dealerScore];

  (* Funzione per determinare il vincitore *)
  determineWinner[] :=
    Module[{},
        Switch[True,
            playerScore > 21, "Il dealer vince!",
            playerScore == dealerScore, "Pareggio!",
            dealerScore > 21 || playerScore > dealerScore, ToString[playerName] <> " hai vinto!",
            True, "Il dealer vince!"
        ]
    ];


  (* Esecuzione del turno del giocatore *)
  playerTurn[];

  (* Esecuzione del turno del dealer *)
  dealerScore = dealerTurn[];

  (* Determina il vincitore *)
  winner = determineWinner[];

  (* Mostra il risultato finale *)
  (*(DialogInput[
   DialogNotebook[{TextCell[
       "Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[playerScore], "Text"], 
      TextCell["Le carte del dealer sono: " <> showCards[dealerHand] <> ". Totale: " <> ToString[dealerScore], "Text"], 
      TextCell[winner, Background -> LightBlue], 
      Button["Nuova Partita", playBlackjack[]], Button["Quit", DialogReturn[]]}]]
  *) 
        
  Module[{decision},
    decision = DialogInput[
      DialogNotebook[{
        Grid[{{TextCell["Seed attuale: " <> ToString[actualSeed], "Text"]}}],
        Grid[{
          {TextCell["LE CARTE DI " <> ToString[playerName] <> ": ", "Text"], TextCell["LA CARTA DEL DEALER: ", "Text"]},
          {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1], playingcardgraphic[dealerHand, "CardSpreadAngle" -> 0.1]},
          {TextCell["Il tuo punteggio totale \[EGrave]: " <> ToString[calculateScore[playerHand]], "Text"], 
          TextCell["Il suo punteggio totale \[EGrave]: " <> ToString[calculateScore[dealerHand]], "Text"]}
        }, Spacings -> {10, 1}],
        TextCell[winner, Background -> LightBlue, FontSize -> 14],
        Grid[{{Button["Nuova Partita", DialogReturn["NewGame"], 
                Background -> {Darker[LightBlue, 0.2], Lighter[LightBlue]}, 
                BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}], 
              Button["Ricomincia partita", DialogReturn["Restart"], 
                Background -> {Darker[LightRed, 0.2], Lighter[LightRed]}, 
                BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}],
              Button["Cambia giocatore", DialogReturn["chooseCharacter"], 
                Background -> {Darker[LightRed, 0.2], Lighter[LightRed]}, 
                BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}], 
              Button["Esci", DialogReturn["Quit"], 
                Background -> {Darker[LightGreen, 0.2], Lighter[LightGreen]}, 
                BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}]}
            }]
      }]
    ];
    Switch[decision,
      "NewGame", 
        DialogReturn["cancel"];
        chooseSeed[playerName],
      "Quit", 
        DialogReturn[],
      "Restart", 
        DialogReturn["cancel"];
        playBlackjack[playerName, actualSeed],
      "chooseCharacter", 
        DialogReturn["cancel"];
        chooseName[];
    ]
  ];      
]
End[];

chooseSeed[playerName_] := DynamicModule[{inputValue, input}, 
  inputValue = "";
  input = DialogInput[{TextCell["Inserisci un numero intero da utilizzare come seed:", FontSize -> 12], 
    InputField[Dynamic[inputValue], Number, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button["Procedi", DialogReturn[inputValue], ImageSize -> {100, 30}]}]}];
  If[inputValue =!= "", playBlackjack [playerName, inputValue], playBlackjack[playerName]]
]



chooseName[] := DynamicModule[{input, playerName}, 
  playerName = "";
  input = DialogInput[{TextCell["Inserisci il nome del giocatore:", FontSize -> 12], 
    InputField[Dynamic[playerName], String, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button["Procedi", DialogReturn[playerName], ImageSize -> {100, 30}]}]}];
  If[playerName === "", playerName = "Giocatore ignoto"]
  chooseSeed[playerName];
 
]


(* main code starting *)
Off[Lookup::invrl];
(*chooseSeed[];*)
chooseName[];

EndPackage[];

