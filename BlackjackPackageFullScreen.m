(* ::Package:: *)

BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];

playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";

Begin["`Private`"];

(* Parameters *)
fontSize = 20;
fontSizePlus = 30;
inputFieldSizeX = 200;
inputFieldSizeY = 50;
buttonSizeX = 100;
buttonSizeY = 50;

playBlackjack[insertedName_:"", insertedSeed_:""] := 
Module[{decision, playedSeed, playingGamer},
  (* Cleaning the console *)
  ClearSystemConsole[];
  
  decision = DialogInput[
    DynamicModule[{playerName = insertedName, phase = "chooseName", inputValue = ""},

      actualSeed = insertedSeed;
    
      (* Import Graphic library for cards *)
      playingcardgraphic = ResourceFunction["PlayingCardGraphic"];

      (* Calculate score function *)
      calculateScore[hand_List] :=
      Module[{score},
        score = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[hand,13]];    (* for each card [1,52] calculates card%13, if > 10 then real value = 10, if = 11 then real value = 11 *)
        aces = Count[Mod[hand,13], 1];    (* count the number of aces in given hand *)
        While[aces > 0,               (* each ace values 1 if the current score is > 21 *)
          If[score>21, score-=10];
          aces -= 1;
        ];
        score]; (* return score *)


      (* dynamic function that manage the dynamic game state *)
      Dynamic[
      Switch[phase,    (* phase variable is set with the current game state *)

      (* CASE chooseName *)
        "chooseName", 
          If[playerName =!= "" && actualSeed === "",    (* when "Nuova partira" button is pressed *)
            phase = "chooseSeed";];   
          If[playerName =!= "" && actualSeed =!= "",    (* when "Ricomincia partita" button is pressed *)
            phase = "skipSeed";];

          Column[{
            TextCell["Inserisci il nome del giocatore", FontSize -> fontSize],
            InputField[Dynamic[playerName], String, ImageSize -> {inputFieldSizeX, inputFieldSizeY}, BaseStyle -> {FontSize -> fontSize}],

            Button[
              Style["Procedi", fontSize, Bold],
              (* button pressed action *)
              (If[playerName === "", playerName = "checkvuoto";];   (* if no name is inserted *)
              phase = "chooseSeed";),    (* when "Procedi" button is pressed, continue with chooseSeed section *)
              ImageSize -> {buttonSizeX, buttonSizeY},
              Appearance -> {"DialogBox"}
            ]
          }, Center],

      (* CASE skipSeed *)
        "skipSeed",     (* when play restarted: when the playBlackjack function is called with seed parameter  *)
          SeedRandom[actualSeed];

          (* Player and Dealer hands initialized (here beacause it has to be done after seed is set) *)
          playerHand = RandomSample[Range@52,2];                          (* two random cards from deck [1,52] *)
          dealerHand = RandomSample[Complement[Range@52,playerHand],2];   (* two random cards from deck excluding player cards *)
          dealerHand1 = dealerHand[[1]];                                  (* first dealer card, used when showing dealer card during playerTurn *)
          (* Initializing player and dealer score *)
          playerScore = calculateScore[playerHand];
          dealerScore = calculateScore[dealerHand];

          (* Pause[5]; *)
          phase = "playerTurn";
          
          Column[{TextCell["CARICAMENTO...", "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]}, Center],

          (* Column[{TextCell["IL VINCITORE È...", "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]}, Center], *)
          (* Pause[2];, *)

      (* CASE chooseSeed *)
        "chooseSeed",        
          Column[{
            TextCell["Inserisci un numero intero da utilizzare come seed", FontSize -> fontSize],
            InputField[Dynamic[inputValue], Number, ImageSize -> {inputFieldSizeX, inputFieldSizeY}, BaseStyle -> {FontSize -> fontSize}],

            Button[
              Style["Procedi", fontSize, Bold], 
              (* button pressed action *)
              (If[inputValue != "", 
                  actualSeed = inputValue; SeedRandom[inputValue];,                (* seed entered *)
                  actualSeed = RandomInteger[1000000]; SeedRandom[actualSeed];    (* seed not entered -> random *)
                ];

                (* Player and Dealer hands initialized (here beacause it has to be done after seed is set) *)
                playerHand = RandomSample[Range@52,2];                          (* two random cards from deck [1,52] *)
                dealerHand = RandomSample[Complement[Range@52,playerHand],2];   (* two random cards from deck excluding player cards *)
                dealerHand1 = dealerHand[[1]];     (* first dealer card, used when showing dealer card during playerTurn *)

                (* Initializing player and dealer score *)
                playerScore = calculateScore[playerHand];
                dealerScore = calculateScore[dealerHand];

                phase = "playerTurn";   (* when "Procedi" button is pressed, continue with playerTurn section *)
              ), 
              ImageSize -> {buttonSizeX, buttonSizeY},
              Appearance -> {"DialogBox"}]
          }, Center],

      (* CASE playerTurn *)
        "playerTurn", 
          Column[{
            Grid[{{TextCell["Seed attuale: " <> ToString[actualSeed], "Text", FontSize -> fontSize]}}],   (* insert in a Grid element in order to center TextCell horizontally *)
            Grid[{
            (* Intestation row *)
              {If[playerName === "checkvuoto",
                TextCell["LE TUE CARTE", "Text", FontSize -> fontSize],
                TextCell["LE CARTE DI " <> ToString[playerName], "Text", FontSize -> fontSize]], 
                TextCell["LA CARTA DEL DEALER", "Text", FontSize -> fontSize]},
            (* Cards row *)
              {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
                playingcardgraphic[{0, dealerHand1}, "CardSpreadAngle" -> 0.1]},
                dealerHandValue = Mod[dealerHand[[1]], 13];
            (* Scores row *)
              {TextCell["Il tuo punteggio totale è: " <> ToString[calculateScore[playerHand]], "Text", FontSize -> fontSize], 
                TextCell["Il suo punteggio parziale è: " <> ToString[If[(dealerHandValue>10)||(dealerHandValue===0),10,If[dealerHandValue==1,11,dealerHandValue]]], "Text", FontSize -> fontSize]} (* Print the value of the dealer's visible card *)
              },
              Spacings -> {20, 5}
            ], 
            Row[{
              (* "chiedi carta" button *)
              Button[
                Style["Chiedi Carta", fontSize, Bold],   (* ask for a new card button *)
                (* If hit, adds a new card to the player's hand and sarts a new player turn *)
                (AppendTo[playerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
                playerScore = calculateScore[playerHand];  (* update playerScore *)
                If[playerScore > 21,
                  phase = "determineWinner";,    (* if player goes out of bound, game over *)
                  phase = "playerTurn";];),       (* otherwise, player goes on with his turn *)
                ImageSize -> {inputFieldSizeX, inputFieldSizeY},
                Appearance -> {"DialogBox"}
              ],

              (* "stai" button *)
              Button[
                Style["Stai", fontSize, Bold],   (* end player turn *)
                phase = "dealerTurn";,
                ImageSize -> {inputFieldSizeX, inputFieldSizeY},
                Appearance -> {"DialogBox"}
              ], 

              (* "Ricomincia partita" button *)
              Button[
                (* Restart the game with the same seed *)
                Style["Ricomincia Partita", fontSize, Bold],
                playedSeed = actualSeed;
                playingGamer = playerName;
                DialogReturn["Restart"];,          
                ImageSize -> {inputFieldSizeX, inputFieldSizeY},
                Appearance -> {"DialogBox"}
              ]
            }, "   "]
          }, Center],   (* Center parameter align column content *)

        (* CASE dealerTurn *)
        "dealerTurn",
          While[dealerScore < 17,   (* heuristic value, above the which dealer keep hitting cards *)
            AppendTo[dealerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
            dealerScore = calculateScore[dealerHand];     (* update dealerScore *)
          ];

          phase = "determineWinner";
          
          Column[{TextCell["IL VINCITORE È...", "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]}, Center],

        (* CASE determineWinner *)
        "determineWinner",
          Pause[1];
          Column[{
            Grid[{{TextCell["Seed attuale: " <> ToString[actualSeed], "Text", FontSize -> fontSize]}}],   (* insert in a Grid element in order to center TextCell horizontally *)
            Grid[{
            (* Intestation row *)
              {If[playerName === "checkvuoto",
                TextCell["LE TUE CARTE", "Text", FontSize -> fontSize],
                TextCell["LE CARTE DI " <> ToString[playerName], "Text", FontSize -> fontSize]], 
                TextCell["LE CARTE DEL DEALER", "Text", FontSize -> fontSize]},
            (* Cards row *)
              {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
                playingcardgraphic[dealerHand, "CardSpreadAngle" -> 0.1]},
            (* Scores row *)
              {TextCell["Il tuo punteggio totale è: " <> ToString[calculateScore[playerHand]], "Text", FontSize -> fontSize], 
                TextCell["Il suo punteggio totale è: " <> ToString[calculateScore[dealerHand]], "Text", FontSize -> fontSize]}
              },
              Spacings -> {20, 5}
            ],
            (* Winner message *)
            Grid[{
              {(* determine winner *)
                playerScore = calculateScore[playerHand];
                dealerScore = calculateScore[dealerHand];

                If[playerScore > 21,    (* player goes out of bound *)
                  winner = "dealer";,
                  If[playerScore === dealerScore,  (* same score *)
                    winner = "pareggio";,
                    If[dealerScore > 21 || playerScore > dealerScore,   (* dealer goes out of bound or player'score is higher *)
                      winner = "player";,
                        winner = "dealer";    (* dealer'score is higher *)
                ];];];

                If[winner === "dealer",
                  TextCell["HAI PERSO\nIL DEALER VINCE", "Text", FontColor -> Red, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus], 
                If[winner === "player", 
                  TextCell["HAI VINTO!", "Text", FontColor -> Green, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus],
                  TextCell["PAREGGIO!", "Text", FontColor -> Orange, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]]
                ]}
              }, ColumnWidths -> {40,40,40}, Alignment -> {Center, Center}
            ],
            Row[{
              (* "Nuova Partita" button *)
              Button[
                (* Stat a new game with the same player name*) 
                Style["Nuova Partita", fontSize, Bold], 
                playingGamer = playerName;
                DialogReturn["NewGame"];,         
                ImageSize -> {inputFieldSizeX, inputFieldSizeY}
              ],
              (* "Ricomincia partita" button *)
              Button[
                (* Restart the game with the same seed and same player name *)
                Style["Ricomincia Partita", fontSize, Bold], 
                playedSeed = actualSeed;
                playingGamer = playerName;
                DialogReturn["Restart"];,                  
                ImageSize -> {inputFieldSizeX, inputFieldSizeY}
              ],
              (* "Cambia giocatore" button *)
              Button[
                (* Let the user change his player name *)
                Style["Cambia giocatore", fontSize, Bold],
                DialogReturn["chooseCharacter"];,                  
                ImageSize -> {inputFieldSizeX, inputFieldSizeY}
              ],
              (* "Esci" button *)
              Button[
                (* Closes window *)
                Style["Esci", fontSize, Bold], 
                DialogReturn["Quit"];,          
                ImageSize -> {inputFieldSizeX, inputFieldSizeY}
              ]
            }, "   "]
          }, Center],   (* Center parameter align column content *)

          (* CASE default *)
          _, 
            DefaultButton[Style["Default button switch"]];
      ]
      ]
    ],  (* end DynamicModule *)
      TextAlignment -> {Center, Center},
      WindowSize -> {Scaled[1], Scaled[1]}
  ];   (* end DialogInput *)

  (* Evaluate decision *)
  Switch[decision,    
    "NewGame",
      playBlackjack[playingGamer];,
    "Quit", 
      Quit[];,
    "Restart", 
      playBlackjack[playingGamer, playedSeed];, 
    "chooseCharacter", 
      playBlackjack[];,      
    _, 
      Quit[];
  ];
];

End[];


(* main code starting *)
Off[Lookup::invrl];
playBlackjack[];


EndPackage[];