BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];


playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";

Begin["`Private`"];

playBlackjack[playerName_, seed_: Automatic] :=
 Module[{deck, actualSeed, playerHand, dealerHand, dealerHand1, playerScore, dealerScore, 
   playerDecision, dealerDecision, winner, playingcardgraphic, quitgame},

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
  quitgame = False;
  (* Inizializzazione delle mani del giocatore e del dealer *)
  playerHand = RandomSample[Range@52,2];
  dealerHand = RandomSample[Complement[Range@52,playerHand],2];
  dealerHand1 = dealerHand[[1]];

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
              dealerHandValue = Mod[dealerHand[[1]], 13];
            {TextCell["Il tuo punteggio totale \[EGrave]: " <> ToString[calculateScore[playerHand]], "Text"], 
              TextCell["Il suo punteggio totale \[EGrave]: " <> ToString[If[(dealerHandValue>10)||(dealerHandValue===0),10,If[dealerHandValue==1,11,dealerHandValue]]], "Text"]}
            },
            Spacings -> {10, 1}
          ], 
          Grid[{{
            Button[Style["Chiedi Carta", 16, Bold], DialogReturn["Hit"]], 
            Button[Style["Stai", 16, Bold], DialogReturn["Stand"]], 
            (* Pulsante per cominciare una partita da capo con lo stesso seed*)
            Button[Style["Ricomincia Partita", 16, Bold], DialogReturn["Restart"]], 
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
        quitgame= True;
        DialogReturn[]
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
            playerScore > 21, "dealer",                 
            playerScore == dealerScore, "pareggio",    
            dealerScore > 21 || playerScore > dealerScore, "player",   
            True, "dealer"                                         
        ]
    ];


  (* Esecuzione del turno del giocatore *)
  playerTurn[];
   
  (* Esecuzione del turno del dealer *)
   If[quitgame == True, Print["quitto"];Quit[]]
   If[quitgame == False, Print["non quitto"];dealerScore = dealerTurn[];  winner = determineWinner[];
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
            Button[Style["Nuova Partita", 16, Bold], DialogReturn["NewGame"]],
            Button[Style["Ricomincia partita", 16, Bold], DialogReturn["Restart"]], 
            Button[Style["Cambia giocatore", 16, Bold], DialogReturn["chooseCharacter"]], 
            Button[Style["Esci", 16, Bold], DialogReturn["Quit"]]
          }
        }]



      }]
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
  ];  ];
 
 

  (* Mostra il risultato finale *)
  (*(DialogInput[
   DialogNotebook[{TextCell[
       "Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[playerScore], "Text"], 
      TextCell["Le carte del dealer sono: " <> showCards[dealerHand] <> ". Totale: " <> ToString[dealerScore], "Text"], 
      TextCell[winner, Background -> LightBlue], 
      Button["Nuova Partita", playBlackjack[]], Button["Quit", DialogReturn[]]}]]
  *) 
        
      
]
End[];

evaluateInput[playerName_, inputToEvaluate_/;(inputToEvaluate != "")]:= playBlackjack [playerName, inputToEvaluate];
evaluateInput[playerName_, inputToEvaluate_/;(inputToEvaluate == "")]:= playBlackjack [playerName];
chooseSeed[playerName_] := DynamicModule[{inputValue, input, confirmedSeed}, 
  inputValue = "";
  confirmedSeed = False;
  input = DialogInput[{TextCell["Inserisci un numero intero da utilizzare come seed:", FontSize -> 12], 
    InputField[Dynamic[inputValue], Number, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button[Style["Procedi", 12, Bold], (confirmedSeed = True; DialogReturn[inputValue]), ImageSize -> {100, 30}]}],
    EventHandler[{confirmedSeed},{"MenuCommand", "CancelButton"} :> (confirmedSeed = False; DialogReturn[])];}];
  
  If[confirmedSeed, Map[(evaluateInput[playerName, #]) &, {inputValue}]]
]



chooseName[] := DynamicModule[{input, playerName, confirmedName},
  playerName = "";
  confirmedName = False; (* Inizializza la variabile confirmed a False *)
  input = DialogInput[{TextCell["Inserisci il nome del giocatore:", FontSize -> 12], 
    InputField[Dynamic[playerName], String, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button[Style["Procedi", 12, Bold], (confirmedName = True; DialogReturn[playerName]), ImageSize -> {100, 30}]}],
    (* Aggiungi un EventHandler per catturare l'evento di chiusura della finestra di dialogo *)
    EventHandler[{confirmedName},{"MenuCommand", "CancelButton"} :> (confirmedName = False; DialogReturn[])];
  }];
  (* Se l'utente ha confermato l'input, esegui l'istruzione If *)
  If[confirmedName && playerName === "", playerName = "Giocatore ignoto"];
  If[confirmedName, chooseSeed[playerName]]; (* Chiamata a chooseSeed solo se l'utente ha confermato l'input *)
]


(* main code starting *)
Off[Lookup::invrl];
(*chooseSeed[];*)
chooseName[];

EndPackage[];

