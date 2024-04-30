BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];


playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";
chooseSeed::usage = "Lets the user choose the seed he wants to play with. If not specified, an automatic one will be set";
chooseName::usage = "Lets the user pick his own player name";

Begin["`Private`"];

(* PARAMETRI *)
const 

playBlackjack[playerName_, seed_: Automatic] :=
 Module[{actualSeed, playerHand, dealerHand, dealerHand1, playerScore, dealerScore, 
   playerDecision, dealerDecision, winner, playingcardgraphic},

  (*Import Graphic library for cards *)
  playingcardgraphic = ResourceFunction["PlayingCardGraphic"];

  (* Setting the seed the user specified. Otherwise it will be created automatically *)
  If[seed =!= Automatic,
    actualSeed = seed;
    SeedRandom[seed],
    actualSeed = RandomInteger[1000000];
    SeedRandom[actualSeed]
  ]
  
  (* Cleaning the console *)
  ClearSystemConsole[];

  (* Player and Dealer hands initialized *)
  playerHand = RandomSample[Range@52,2];    (* two random cards from deck [1,52] *)
  dealerHand = RandomSample[Complement[Range@52,playerHand],2];   (* two random cards from deck excluding player cards *)
  dealerHand1 = dealerHand[[1]];    (* first dealer card, used when showing dealer card during playerTurn *)

  (* Calculate score *)
  calculateScore[hand_List] :=
   Module[{score},
    score = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[hand,13]];    (* for each card [1,52] calculates card%13, if > 10 then real value = 10, if = 11 then real value = 11 *)
    aces = Count[Mod[hand,13], 1];    (* count the number of aces in given hand *)
    While[aces > 0,               (* each ace values 1 if the current score is > 21 *)
      If[score>21, score-=10];
      aces -= 1;
    ];
    score]; (* return score *)

  (* Initializing the player score *)
  playerScore = calculateScore[playerHand];

  (* Player turn *)
  playerTurn[] :=
   Module[{decision},
    decision = DialogInput[
      DialogNotebook[{
        Grid[{{TextCell["Seed attuale: " <> ToString[actualSeed], "Text"]}}],   (* insert in a Grid element in order to center TextCell horizontally *)
        Grid[{
        (* Intestation row *)
          {TextCell["LE CARTE DI " <> ToString[playerName] <> ": ", "Text"], 
            TextCell["LA CARTA DEL DEALER: ", "Text"]},
        (* Cards row *)
          {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
            playingcardgraphic[{0, dealerHand1}, "CardSpreadAngle" -> 0.1]},
          dealerHandValue = Mod[dealerHand[[1]], 13];
        (* Scores row *)
          {TextCell["Il tuo punteggio totale è: " <> ToString[calculateScore[playerHand]], "Text"], 
            TextCell["Il suo punteggio parziale è: " <> ToString[If[(dealerHandValue>10)||(dealerHandValue===0),10,If[dealerHandValue==1,11,dealerHandValue]]], "Text"]} (* Print the value of the dealer's visible card *)
          },
          Spacings -> {20, 2}
        ], 
        (* Buttons grid *)
        Grid[{{
          Button[Style["Chiedi Carta", 18, Bold], DialogReturn["Hit"], ImageSize -> {200, 50}],   (* hit: ask for a new card *)
          Button[Style["Stai", 18, Bold], DialogReturn["Stand"], ImageSize -> {200, 50}],         (* stand: no more cards *)
          Button[Style["Ricomincia Partita", 18, Bold], DialogReturn["Restart"], ImageSize -> {200, 50}]}   (* restart: restart game with same seed *)
        }]
      }], WindowSize->{Scaled[1], Scaled[1]}, TextAlignment->Center   (* full screen *)
	  ];
    
    Switch[decision,  (* based on the pressed button in DialogInput *)
      "Hit",
        (* If hit, adds a new card to the player's hand and sarts a new player turn *)
        AppendTo[playerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
        playerScore = calculateScore[playerHand];
        If[playerScore > 21, DialogReturn[], playerTurn[]], (* If player score > 21, exit *)

      "Stand",
        (* If stands, both player and dealr score are calculated*)
        AppendTo[dealerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
        playerScore = calculateScore[playerHand];
        dealerScore = calculateScore[dealerHand],

      "Restart",
        (* Restart the game with the same seed *)
        DialogReturn["cancel"];
        playBlackjack[playerName, actualSeed],

      _, (* default case *)
        (* Needed when user presses X on the right top side of the window *)
        Quit[];
    ]
  ];

  (* Funzione per il turno del dealer *)
  dealerTurn[] :=
    Module[{},
      (* If player score > 21, dealer stands because the player has already lost *)
      If[playerScore <= 21,
        (* Until the dealer reaches 17, he needs to ask other cards *)
        While[calculateScore[dealerHand] < 17,
          AppendTo[dealerHand, RandomChoice[Complement[Range@52,playerHand, dealerHand]]];    (* append a card at dealer hand *)
          If[calculateScore[dealerHand] > 21, Break[]]
        ]
      ];
      calculateScore[dealerHand]    (* return dealer score *)
    ];

  (* Determine the winner *)
  determineWinner[] :=
    Module[{},
      Switch[True,
        playerScore > 21, "dealer",                 
        playerScore == dealerScore, "pareggio",    
        dealerScore > 21 || playerScore > dealerScore, "player",   
        True, "dealer"                                         
      ]
    ];


  (* Executes the player turn *)
  playerTurn[];

  (* Executes the dealer turn *)
  dealerScore = dealerTurn[];

  (* Execute function to determine winner *)
  winner = determineWinner[];
  
  (* Last window which prints the final status of the game *)
  Module[{decision},
    decision = DialogInput[
      DialogNotebook[{
        Grid[{
          {TextCell["Seed attuale: " <> ToString[actualSeed], "Text"]}},
           ColumnWidths -> {40,40,40}, Alignment -> {Center, Center}],
        Grid[{
          (* Intestation row *)
          {TextCell["LE CARTE DI " <> ToString[playerName] <> ": ", "Text"],
            TextCell["LE CARTE DEL DEALER: ", "Text"]},
          (* Cards row *)
          {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
            playingcardgraphic[dealerHand, "CardSpreadAngle" -> 0.1]},
          (* Scores row *)
          {TextCell["Il tuo punteggio totale è: " <> ToString[calculateScore[playerHand]], "Text"], 
          TextCell["Il suo punteggio totale è: " <> ToString[calculateScore[dealerHand]], "Text"]}
        }, Spacings -> {10, 1}],
        (* Winner message *)
        Grid[{
          {If[winner === "dealer",
            TextCell["HAI PERSO\nIL DEALER VINCE", "Text", FontColor -> Red, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20], 
            If[winner === "player", 
              TextCell["HAI VINTO!", "Text", FontColor -> Green, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20],
              TextCell["PAREGGIO!", "Text", FontColor -> Orange, FontWeight -> Bold, TextAlignment -> Center, FontSize -> 20]]]}
        }, ColumnWidths -> {40,40,40}, Alignment -> {Center, Center}],
        (* Buttons grid *)
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

    Switch[decision,    (* based on button pressed *)
      "NewGame",
        (* Stat a new game with the same player name*) 
        DialogReturn[];
        chooseSeed[playerName],
      "Quit", 
        (* Closes windows *)
        DialogReturn[],
      "Restart", 
        (* Restarts the game with the same seed *)
        DialogReturn["cancel"];
        playBlackjack[playerName, actualSeed],
      "chooseCharacter", 
        (*Let the user change his player name*)
        DialogReturn["cancel"];
        chooseName[];
    ]
  ];];
 
End[];

(* Calls playBlackjack function with or without seed, based on user input *)
evaluateInput[playerName_, inputSeed_/;(inputSeed != "")]:= playBlackjack[playerName, inputSeed];
evaluateInput[playerName_, inputSeed_/;(inputSeed == "")]:= playBlackjack[playerName]; (*If user doesn't specify the seed an automatic one will be generated *)

(* Called in chooseName function: asks user for a seed *)
chooseSeed[playerName_] := DynamicModule[{inputSeed, input, closeWindow}, 
  inputSeed = "";
  closeWindow = True;

  input = DialogInput[{TextCell["Inserisci un numero intero da utilizzare come seed:", FontSize -> 12], 
    InputField[Dynamic[inputSeed], Number, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button[Style["Procedi", 12, Bold], (closeWindow = False; DialogReturn[inputSeed]), ImageSize -> {100, 30}]}],
    EventHandler[{confirmedSeed},{"MenuCommand", "CancelButton"} :> (confirmedSeed = False; DialogReturn[])]; (* Needed if the user chooses to close window with X and not to continue *)
  }];
  
  (* If user didn't close window then call playBlackJack *)
  If[!closeWindow, evaluateInput[playerName, inputSeed]]
]

(* Lets the user pick his name *)
chooseName[] := DynamicModule[{input, playerName, closeWindow},
  playerName = "";
  closeWindow = True; 
  input = DialogInput[{TextCell["Inserisci il nome del giocatore:", FontSize -> 12], 
    InputField[Dynamic[playerName], String, ImageSize -> {100,30}, BaseStyle -> {FontSize -> 14}], 
    Row[{Button[Style["Procedi", 12, Bold], (closeWindow = False; DialogReturn[playerName]), ImageSize -> {100, 30}]}],
  }];  

  (* If user decides not to put the name "Giocatore ignoto" will be set as player name *)
  If[playerName === "", playerName = "Giocatore ignoto"];
  
  (* If user didn't close window with X, then call chooseSeed*)
  If[!closeWindow, chooseSeed[playerName]];
]


(* MAIN CODE STARTING *)
Off[Lookup::invrl];

chooseName[];

EndPackage[];

