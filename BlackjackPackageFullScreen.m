(* ::Package:: *)

BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];

playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";

Begin["`Private`"];

(* Parameters *)
fontSize = 20;
fontSizeDisclaimer = 15;
fontSizePlus = 28;
fontSizeTitle = 35;
inputFieldSizes = {200, 50};
buttonSizes = {100, 50};
spacingValues = {20, 5};
columnWidthValues = {40,40,40};
buttonProcediSpacing = {3, 2, 2};
buttonProcediSpacingDisclaimer = {3, 2, 2, 4};
columnSpacing = {3, 3, 3};

(* parameters used when starting new game, same game or changing player name *)
playedSeed = "";
playingGamer = "";

(* Parameters used to keep history *)
win = 0;
lose = 0;
draw = 0;
(* historyFlag is used to avoid that the history counter keep increasing more the it should *)
historyFlag = False;

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

playBlackjack[insertedName_:"", insertedSeed_:""] := 
  Module[{decision},
    (* Cleaning the console *)
    ClearSystemConsole[];

    (*Import Graphic library for cards *)
    playingcardgraphic = ResourceFunction["PlayingCardGraphic"];

    decision = singlePlay[insertedName, insertedSeed];

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
  

singlePlay[insertedName_, insertedSeed_] :=
  DialogInput[DynamicModule[{playerName = insertedName, phase = "chooseName", inputValue = ""},

    actualSeed = insertedSeed;

    (* dynamic function that manage the dynamic game state *)
    Dynamic[
      Switch[phase,    (* phase variable is set with the current game state *)

      (* CASE chooseName *)
        "chooseName", 
          If[playerName =!= "" && actualSeed === "",    (* when "Nuova partita" button is pressed *)
            phase = "chooseSeed";];   
          If[playerName =!= "" && actualSeed =!= "",    (* when "Ricomincia partita" button is pressed *)
            phase = "skipSeed";];

          Column[{
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],

            TextCell["Inserisci il " Style["nome del giocatore", Bold], "Text", FontFamily -> "Helvetica", FontSize -> fontSize],
            InputField[Dynamic[playerName], String, ImageSize -> inputFieldSizes, BaseStyle -> {FontSize -> fontSize}],

            Button[
              Style["Procedi", fontSize, Bold],
              (* button pressed action *)
              (If[playerName === "", playerName = "Anonimo";];   (* if no name is inserted *)
              phase = "chooseSeed";),    (* when "Procedi" button is pressed, continue with chooseSeed section *)
              ImageSize -> buttonSizes,
              Appearance -> {"DialogBox"}
            ]
          }, Center, Spacings -> buttonProcediSpacing],

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

          phase = "playerTurn";
          
          Column[{
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],
            TextCell["CARICAMENTO...", "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
          }, Center, Spacings -> {15}],

      (* CASE chooseSeed *)
        "chooseSeed",        
          Column[{
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],
            TextCell["Ciao, " Style[playerName, Bold], "Text", FontFamily -> "Helvetica", FontSize -> fontSize],
            
            TextCell["Inserisci un numero intero da utilizzare come " Style["seed", Bold], "Text", FontFamily -> "Helvetica", FontSize -> fontSize],
            InputField[Dynamic[inputValue], Number, ImageSize -> inputFieldSizes, BaseStyle -> {FontSize -> fontSize}],

            Button[
              Style["Indietro", fontSize, Bold],
              (* button pressed action *)
              playerName = "";
              phase = "chooseName";,    (* when "Procedi" button is pressed, continue with chooseSeed section *)
              ImageSize -> buttonSizes,
              Appearance -> {"DialogBox"}
            ]
            Button[
              Style["Procedi", fontSize, Bold], 
              (* button pressed action *)
              ( Pause[0.8];     (* used to wait the inputValue dynamic variable update *)
                If[inputValue != "", 
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
              ImageSize -> buttonSizes,
              Appearance -> {"DialogBox"}
            ],
            
            TextCell["Attenzione! È possibile inserire solo numeri!\nNon inserendo un seed questo verrà generato casualmente.",  FontColor -> Red, FontSize -> fontSizeDisclaimer]
          }, Center, Spacings -> buttonProcediSpacingDisclaimer],

      (* CASE playerTurn *)
        "playerTurn", 
          Column[{
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],

            Grid[{{TextCell["Seed: " <> ToString[actualSeed], "Text", FontSize -> fontSize]}}],   (* insert in a Grid element in order to center TextCell horizontally *)
            Grid[{
            (* Intestation row *)
              {If[playerName === "Anonimo",
                TextCell["LE TUE CARTE", "Text", FontWeight -> Bold, FontSize -> fontSize],
                TextCell["LE CARTE DI " <> ToString[playerName], "Text", FontWeight -> Bold, FontSize -> fontSize]], 
                TextCell["LA CARTA DEL DEALER", "Text", FontWeight -> Bold, FontSize -> fontSize]},
            (* Cards row *)
              {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
                playingcardgraphic[{0, dealerHand1}, "CardSpreadAngle" -> 0.1]},
                dealerHandValue = Mod[dealerHand[[1]], 13];
            (* Scores row *)
              {TextCell["Punteggio totale: " <> ToString[calculateScore[playerHand]], "Text", FontSize -> fontSize], 
                TextCell["Punteggio parziale: " <> ToString[If[(dealerHandValue>10)||(dealerHandValue===0),10,If[dealerHandValue==1,11,dealerHandValue]]], "Text", FontSize -> fontSize]} (* Print the value of the dealer's visible card *)
              },
              Spacings -> spacingValues
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
                ImageSize -> inputFieldSizes,
                Appearance -> {"DialogBox"}
              ],

              (* "stai" button *)
              Button[
                Style["Stai", fontSize, Bold],   (* end player turn *)
                phase = "dealerTurn";,
                ImageSize -> inputFieldSizes,
                Appearance -> {"DialogBox"}
              ], 

              (* "Ricomincia partita" button *)
              Button[
                (* Restart the game with the same seed *)
                Style["Ricomincia Partita", fontSize, Bold],
                playedSeed = actualSeed;
                playingGamer = playerName;
                DialogReturn["Restart"];,          
                ImageSize -> inputFieldSizes,
                Appearance -> {"DialogBox"}
              ]
            }, "   "]
          }, Center, Spacings -> columnSpacing],   (* Center parameter align column content *)

        (* CASE dealerTurn *)
        "dealerTurn",
          While[dealerScore < 17,   (* heuristic value, above the which dealer keep hitting cards *)
            AppendTo[dealerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
            dealerScore = calculateScore[dealerHand];     (* update dealerScore *)
          ];

          phase = "determineWinner";
          
          Column[{
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],
            TextCell["IL VINCITORE È...", "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
          }, Center, Spacings -> {15}],

        (* CASE determineWinner *)
        "determineWinner",
          Pause[1];   (* to show on the screen the "IL VINCITORE È..." message for one second *)
          Column[{
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],

            Grid[{{TextCell["Seed: " <> ToString[actualSeed], "Text", FontSize -> fontSize]}}],   (* insert in a Grid element in order to center TextCell horizontally *)
            Grid[{
            (* Intestation row *)
              {If[playerName === "Anonimo",
                TextCell["LE TUE CARTE", "Text", FontWeight -> Bold, FontSize -> fontSize],
                TextCell["LE CARTE DI " <> ToString[playerName], "Text", FontWeight -> Bold, FontSize -> fontSize]], 
                TextCell["LE CARTE DEL DEALER", "Text", FontWeight -> Bold, FontSize -> fontSize]},
            (* Cards row *)
              {playingcardgraphic[playerHand, "CardSpreadAngle" -> 0.1],
                playingcardgraphic[dealerHand, "CardSpreadAngle" -> 0.1]},
            (* Scores row *)
              {TextCell["Punteggio totale: " <> ToString[calculateScore[playerHand]], "Text", FontSize -> fontSize],
                TextCell["Punteggio totale: " <> ToString[calculateScore[dealerHand]], "Text", FontSize -> fontSize]}
              },
              Spacings -> spacingValues
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
                    (
                      If[!historyFlag,
                        lose = lose + 1;
                        historyFlag = True;
                      ];
                      TextCell["HAI PERSO, IL DEALER VINCE!", "Text", FontColor -> Red, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
                    ),
                If[winner === "player",
                    (
                      If[!historyFlag,
                        win = win + 1;
                        historyFlag = True;
                      ];

                      TextCell["HAI VINTO!", "Text", FontColor -> Green, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
                    ),
                    (
                      If[!historyFlag,
                        draw = draw + 1;
                        historyFlag = True;
                      ];
                      TextCell["PAREGGIO!", "Text", FontColor -> Orange, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
                    ) 
                  ]
                ]
                }
                  TextCell["Vittorie: " <> ToString[win] <> " - Sconfitte: " <> ToString[lose] <> " - Pareggi: " <> ToString[draw], "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSize]
              }, ColumnWidths -> columnWidthValues, Alignment -> {Center, Center}
            ],
            Row[{
              (* "Nuova Partita" button *)
              Button[
                (* Stat a new game with the same player name*) 
                Style["Nuova Partita", fontSize, Bold], 
                historyFlag = False;
                playingGamer = playerName;
                DialogReturn["NewGame"];,         
                ImageSize -> inputFieldSizes
              ],
              (* "Ricomincia partita" button *)
              Button[
                (* Restart the game with the same seed and same player name *)
                Style["Ricomincia Partita", fontSize, Bold], 
                historyFlag = False;
                playedSeed = actualSeed;
                playingGamer = playerName;
                DialogReturn["Restart"];,                  
                ImageSize -> inputFieldSizes
              ],
              (* "Cambia giocatore" button *)
              Button[
                (* Let the user change his player name *)
                Style["Cambia giocatore", fontSize, Bold],
                historyFlag = False;
                DialogReturn["chooseCharacter"];,                  
                ImageSize -> inputFieldSizes
              ],
              (* "Esci" button *)
              Button[
                (* Closes window *)
                Style["Esci", fontSize, Bold], 
                DialogReturn["Quit"];,          
                ImageSize -> inputFieldSizes
              ]
            }, "   "]
          }, Center, Spacings -> columnSpacing],   (* Center parameter align column content *)

          (* CASE default *)
          _, 
            DefaultButton[Style["Default button switch"]];
      ]
      ]
    ],  (* end DynamicModule *)
      TextAlignment -> {Center, Center},
      WindowSize -> {Scaled[1], Scaled[1]}
  ];   (* end DialogInput *)


End[];


(* main code starting *)
playBlackjack[];


EndPackage[];