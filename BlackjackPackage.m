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
    SeedRandom[actualSeed]
  ]
  
  (* Pulizia della console: rimuovendo questo comando il codice si bugga *)
  ClearSystemConsole[];

  (* Inizializzazione del mazzo di carte *)
  deck = playingcardgraphic[Range@52];

  (* Inizializzazione delle mani del giocatore e del dealer *)
  playerHand = RandomSample[Range@52,2];
  dealerHand = RandomSample[Complement[Range@52,playerHand],2];
  dealerHand1 = dealerHand[[1]];

  (* Funzione per calcolare il punteggio *)
  calculateScore[hand_List] :=
   Module[{score},
    score = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[hand,13]];
    aces = Count[Mod[hand,13], 1];    (* conto il numero di assi prensenti nella mano *)
    While[aces > 0,               (* ogni asso vale 1 se il punteggio corrente è > 21 *)
      If[score>21, score-=10];
      aces -= 1;
    ];
    score];

  (* Inizializzazione del punteggio del giocatore *)
  playerScore = calculateScore[playerHand];

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
            dealerHandValue = Mod[dealerHand[[1]], 13];
          {TextCell["Il tuo punteggio totale è: " <> ToString[calculateScore[playerHand]], "Text"], 
            TextCell["Il suo punteggio totale è: " <> ToString[If[(dealerHandValue>10)||(dealerHandValue===0),10,If[dealerHandValue==1,11,dealerHandValue]]], "Text"]}
          },
          Spacings -> {20, 2}
        ], 
        Grid[{{
          Button[Style["Chiedi Carta", 18, Bold], DialogReturn["Hit"], ImageSize -> {200, 50}], (* Aumenta la dimensione del pulsante *)
          Button[Style["Stai", 18, Bold], DialogReturn["Stand"], ImageSize -> {200, 50}], 
          (* Pulsante per cominciare una partita da capo con lo stesso seed*)
          Button[Style["Ricomincia Partita", 18, Bold], DialogReturn["Restart"], ImageSize -> {200, 50}]}
        }]
      }], WindowSize->{Scaled[1], Scaled[1]}, TextAlignment->Center
	  ];
    Switch[decision,
      "Hit",
        AppendTo[playerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
        playerScore = calculateScore[playerHand];
        If[playerScore > 21, DialogReturn[], playerTurn[]],

      "Stand",
        AppendTo[dealerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
        playerScore = calculateScore[playerHand];
        dealerScore = calculateScore[dealerHand],

      "Restart",
        DialogReturn["cancel"];
        playBlackjack[playerName, actualSeed],

      _, (* default case *)
        Quit[];
    ]
  ];

  (* Funzione per il turno del dealer *)
  dealerTurn[] :=
    Module[{},
      If[playerScore <= 21,
        While[calculateScore[dealerHand] < 17,
          AppendTo[dealerHand, RandomChoice[Complement[Range@52,playerHand, dealerHand]]];
          If[calculateScore[dealerHand] > 21, Break[]]
        ]
      ];
      calculateScore[dealerHand]
    ];

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


  (* Esecuzione del turno del giocatore *)
  playerTurn[];

  (* Esecuzione del turno del dealer *)
  dealerScore = dealerTurn[];

  (* Verifica del vincitore *)
  winner = determineWinner[];

  Module[{decision},
    decision = DialogInput[
      DialogNotebook[{
        Grid[{
          {TextCell["Seed attuale: " <> ToString[actualSeed], "Text"]}},
           ColumnWidths -> {40,40,40}, Alignment -> {Center, Center}],
        Grid[{
          {TextCell["LE CARTE DI " <> ToString[playerName] <> ": ", "Text"], TextCell["LA CARTA DEL DEALER: ", "Text"]},
          {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1], playingcardgraphic[dealerHand, "CardSpreadAngle" -> 0.1]},
          {TextCell["Il tuo punteggio totale è: " <> ToString[calculateScore[playerHand]], "Text"], 
          TextCell["Il suo punteggio totale è: " <> ToString[calculateScore[dealerHand]], "Text"]}
        }, Spacings -> {10, 1}],
        Grid[{
          {If[winner === "dealer",
            TextCell["HAI PERSO\nIL DEALER VINCE", "Text", FontColor -> Red, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20], 
            If[winner === "player", 
              TextCell["HAI VINTO!", "Text", FontColor -> Green, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20],
              TextCell["PAREGGIO!", "Text", FontColor -> Orange, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20]]]}
        }, ColumnWidths -> {40,40,40}, Alignment -> {Center, Center}],
        Grid[{
          {
            Button[Style["Nuova Partita", 18, Bold], DialogReturn["NewGame"], ImageSize -> {200, 50}],
            Button[Style["Ricomincia partita", 18, Bold], DialogReturn["Restart"], ImageSize -> {200, 50}], 
            Button[Style["Cambia giocatore", 18, Bold], DialogReturn["chooseCharacter"], ImageSize -> {200, 50}], 
            Button[Style["Esci", 18, Bold], DialogReturn["Quit"], ImageSize -> {200, 50}]
          }
        }]
      }], WindowSize->{Scaled[1], Scaled[1]}, TextAlignment->Center
    ];
    Switch[decision,
      "NewGame", 
        DialogReturn[];
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
  ];];
 
End[];

(* richiama la funzione playBlackjack con o senza seed in base all'input dell'utente *)
evaluateInput[playerName_, inputSeed_/;(inputSeed != "")]:= playBlackjack[playerName, inputSeed];
evaluateInput[playerName_, inputSeed_/;(inputSeed == "")]:= playBlackjack[playerName];


chooseSeed[playerName_] := DynamicModule[{inputSeed, input, closeWindow}, 
  inputSeed = "";
  closeWindow = True;
  input = DialogInput[{TextCell["Inserisci un numero intero da utilizzare come seed:", FontSize -> 12], 
    InputField[Dynamic[inputSeed], Number, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button[Style["Procedi", 12, Bold], (closeWindow = False; DialogReturn[inputSeed]), ImageSize -> {100, 30}]}],
    EventHandler[{confirmedSeed},{"MenuCommand", "CancelButton"} :> (confirmedSeed = False; DialogReturn[])];
  }];
  
  If[!closeWindow, evaluateInput[playerName, inputSeed]]
]


chooseName[] := DynamicModule[{input, playerName, closeWindow},
  playerName = "";
  closeWindow = True; 
  input = DialogInput[{TextCell["Inserisci il nome del giocatore:", FontSize -> 12], 
    InputField[Dynamic[playerName], String, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button[Style["Procedi", 12, Bold], (closeWindow = False; DialogReturn[playerName]), ImageSize -> {100, 30}]}],
  }];  

  If[playerName === "", playerName = "Giocatore ignoto"];
  If[!closeWindow, chooseSeed[playerName]];
]


(* main code starting *)
Off[Lookup::invrl];
(*chooseSeed[];*)
chooseName[];

EndPackage[];

