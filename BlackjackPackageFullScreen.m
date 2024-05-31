(* ::Package:: *)

(* :Title: BlackJack *)
(* :Context: BlackJack *)
(* :Author: Francesco Apollonio, Gaia Clerici, Federica Grisendi, Erik Koci, Giacomo Sagliano*)
(* :Summary: An interactive black jack game *)
(* :Copyright: FA GC FG EK GS 2024 *)
(* :Package Version: 3.0 *)
(* :Mathematica Version: 14.0.0.0 *)
(* :History: last modified 31/05/2024 *)
(* :Keywords: cards, black jack, seed *)
(* :Limitations: this is for educational purposes only *)
(* :Requirements: *)

BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];

playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";

Begin["`Private`"];

(* PARAMETERS *)
fontSize = 20; (*Font size*)
fontSizeDisclaimer = 15; (* Font size for red disclaimer text *)
fontSizePlus = 28;  (* Font size for important information such as the winner *)
fontSizeTitle = 35; (* Font size for titles *)
inputFieldSizes = {200, 50}; (* Dimensions for input fields *)
buttonSizes = {100, 50}; (* Dimensions for the buttons *)
spacingValues = {20, 5};  (* Line spacing parameters *)
spacingValuesWinnerMassage = {20, 2};  (* Line spacing parameters for the winner message *)
buttonProcediSpacing = {3, 2, 2}; (* Margins for the buttons *)
buttonProcediSpacingDisclaimer = {3, 2, 2, 4}; (* Margins for the back and proceed buttons in the ChooseSeed window *)
columnSpacing = {3, 3, 3}; (* Parameters for column spacing *)

(* Parameters used when starting a new game, the same game, or changing player name *)
playedSeed = ""; (*Played seed*)
playingGamer = ""; (*Player name*)

(* Parameters used to keep history *)
win = 0; (*Counter for the number of wins*)
lose = 0; (*Counter for the number of losses*)
draw = 0; (*Counter for the number of draws*)
(* historyFlag is used to avoid that the history counter keeps increasing more then it should *)
historyFlag = False;

(* Calculate score function. This function takes a hand of cards as input, either the player's or the dealer's *)
calculateScore[hand_List] :=
  Module[{score, aces},
    (* The graphic library, used for displaying the cards, assigns an incremental value, starting from 1 and going up to 52, to each card *)
    (* In order to assign the real value to the cards, we take the value (between 1 and 52) and mod it by 13 (the maximum value cards can assume) *)
    (* If the value obtained is greater than 10, it means the card is either a Jack or a Queen, so its value for the game is 10 *)
    (* If the value obtained is 0, it means the card is a King, so its value for the game is 10 *)
    (* If the value obtained is 1, it means the card is an Ace, so its value for the game is 11 *)
    score = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[hand,13]];
    (* We need to keep track of the number of aces we have in our hand, in order to know how many times we need to iterate next *)
    aces = Count[Mod[hand,13], 1];
    (* If the total score is greater than 21 and there is at least one ace in the hand, the value of the ace becomes 1 instead of 11 (this applies to every ace in the hand) *)
    While[aces > 0,               
      If[score>21, score-=10];
      aces -= 1;
    ];
  score]; (* Return score *)

(* This function checks if the seed inserted is valid: this condition is verified if the number is an integer *)
checkInputSeed[input_String] :=
  Module[{valid},
    If[StringContainsQ[input, "."] || input == "", valid = False, valid = True]; (* This "If" checks if the input contains a "." or if the value is empty *)
  valid]; (* Return true if the inserted number is valid: it doesn't contain dots and isn't empty *)

(* Actual game function *)
playBlackjack[insertedName_:"", insertedSeed_:""] := 
  Module[{decision},
    (* Clear the console *)
    ClearSystemConsole[];

    (* Import Graphic library for the cards *)
    playingcardgraphic = ResourceFunction["PlayingCardGraphic"];

    (* the decision variable stores the choice of the button pressed during the game *)
    decision = singlePlay[insertedName, insertedSeed];

    (* All the possible decision are cases of the switch *)
    Switch[decision,    
      "NewGame",  (* Start a new game *)
        playBlackjack[playingGamer];,
      "Quit", (* Quit the game and exits it *)
        Quit[];,
      "Restart", (* Restart the same game *)
        playBlackjack[playingGamer, playedSeed];, 
      "chooseCharacter", (* Change the name of the player *)
        playBlackjack[];,      
      _, 
        Quit[];
    ];
  ];  
  
(*This function is responsible for the execution of a single game*)
singlePlay[insertedName_, insertedSeed_] :=
  DialogInput[DynamicModule[
    {playerName = insertedName,
     phase = "chooseName", 
     inputValue = "", 
     actualSeed = insertedSeed, 
        (* actualSeed stores the inserted seed value. This value could be either an integer or an empty string *)
        (* In the first case, the game is played with that seed, meaning the player wants to replay their latest game *)
        (* In the second case, the game is started without a set seed, which means it's the first game or a new game started with a new seed *)
     playerHand, 
     dealerHand, 
     dealerHand1, 
     playerScore,
     dealerScore,
     dealerHandValue},
    
    (* dynamic function that manages the game state dynamically *)
    Dynamic[
      Switch[phase,    (* phase variable stores the game state *)

      (* CASE chooseName. The player needs to choose their character name *)
        "chooseName", 
          If[playerName =!= "" && actualSeed === "",     (* If these two conditions are met, it means the player pressed "Nuova Partita". So it doesn't need to ask for the player name again, but changes the phase to chooseSeed *)
            phase = "chooseSeed";];   
          If[playerName =!= "" && actualSeed =!= "",    (* If these two conditions are met, it means the player pressed "Ricomincia Partita". So the phase is changed to skipSeed, because we are playing with the same seed *)
            phase = "skipSeed";];

          Column[{
            (* Display the screen which asks the player to enter their name *)
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],

            TextCell["Inserisci il " Style["nome del giocatore", Bold], "Text", FontFamily -> "Helvetica", FontSize -> fontSize],
            InputField[Dynamic[playerName], String, ImageSize -> inputFieldSizes, BaseStyle -> {FontSize -> fontSize}],

            Button[
              Style["Procedi", fontSize, Bold],
              (* button pressed action *)
              If[playerName === "", playerName = "anonimo";];  (* if no name is entered, an automatic one is assigned to the player *)
              phase = "chooseSeed";,    (* when "Procedi" button is pressed, phase is changed to chooseSeed *)
              ImageSize -> buttonSizes,
              Appearance -> {"DialogBox"}
            ]
          }, Center, Spacings -> buttonProcediSpacing],

      (* CASE skipSeed. This phase means the player wants to replay the game with the same seed *)
        "skipSeed",    (* when play is restarted: when the playBlackjack function is called with a seed parameter *)
          SeedRandom[actualSeed];

          (* Player and Dealer hands are initialized (here because it has to be done after the seed is set) *)
          playerHand = RandomSample[Range@52,2];                          (* two random cards are drawn from the deck *)
          dealerHand = RandomSample[Complement[Range@52,playerHand],2];   (* two random cards are drawn from the deck, excluding the cards that have already been drawn *)
          dealerHand1 = dealerHand[[1]];                                  (* this value stores the first card the dealer has drawn. This is necessary because only one of the two the dealer's cards can be shown from the beginning of the game*)
          (* Initializing player and dealer scores *)
          playerScore = calculateScore[playerHand]; (* The value of the player's hand is calculated *)
          dealerScore = calculateScore[dealerHand]; (* The value of the dealer's hand is calculated *)

          phase = "playerTurn"; (*Phase is changed to playerTurn*)
          
          Column[{
            (* Displaying the "Caricamento" screen *)
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],
            TextCell["CARICAMENTO...", "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
          }, Center, Spacings -> {15}],

      (* CASE chooseSeed. This phase asks the player to choose a seed; if they don't, an automatic one is assigned to the game *)
        "chooseSeed",        
          Column[{
            (* Display the screen which asks the player to enter the seed *)
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],
            TextCell["Ciao, " Style[playerName, Bold], "Text", FontFamily -> "Helvetica", FontSize -> fontSize],
            
            TextCell["Inserisci un numero intero da utilizzare come " Style["seed", Bold], "Text", FontFamily -> "Helvetica", FontSize -> fontSize],
            InputField[Dynamic[inputValue], Number, ImageSize -> inputFieldSizes, BaseStyle -> {FontSize -> fontSize}],

            Row[{
              (* "Indietro" button is useful because if the player entered their name incorrectly, they can press the button and change the name *)
              Button[
                Style["Indietro", fontSize, Bold],
                (* button pressed action *)
                playerName = "";
                phase = "chooseName";,    (* when "Indietro" button is pressed, phase changes to chooseName *)
                ImageSize -> buttonSizes,
                Appearance -> {"DialogBox"}
              ],

              Button[
                (* "Procedi" button is used to proceed with the actual game *)
                Style["Procedi", fontSize, Bold], 
                (* button pressed action *)
                Pause[0.8];     (* used to wait the inputValue dynamic variable updates *)
                  If[checkInputSeed[ToString[inputValue]], (* if the entered value is valid, then the seed is set with the entered value, otherwise a random seed is assigned to the game *)
                    actualSeed = inputValue; SeedRandom[inputValue];,                 (* the seed entered is valid *)
                    actualSeed = RandomInteger[1000000]; SeedRandom[actualSeed];      (* If the seed wasn't entered or has an invalid value, a random one is generated*)
                  ];

                  (* Player and Dealer hands are initialized (it is done here beacause it has to be done after seed is set) *)      
                  playerHand = RandomSample[Range@52,2];                          (* two random cards are drawn from the deck, excluding the cards that have already been drawn *)
                  dealerHand = RandomSample[Complement[Range@52,playerHand],2];   (* this value stores the first card the dealer has drawn. This is necessary because only one out of the two cards of the dealer can be shown from the beginning of the game*)
                  dealerHand1 = dealerHand[[1]];     (* this value stores the first card the dealer has drawn. This is necessary because only one out of the two dealer's cards can be shown from the beginning of the game*)

                  (* Initializing player and dealer scores *)
                  playerScore = calculateScore[playerHand]; (* The value of the player's hand is calculated *)
                  dealerScore = calculateScore[dealerHand]; (* The value of the dealer's hand is calculated *)

                  phase = "playerTurn";,   (*Now the phase is changed to playerTurn*)
                ImageSize -> buttonSizes,
                Appearance -> {"DialogBox"}
              ]
            }, "   "],
            (*This is the disclaimer that points out the properties the seed must have*)
            TextCell["Attenzione! È possibile inserire solo numeri INTERI!\nNon inserendo un seed questo verrà generato casualmente.\nInserendo un numero non valido, il seed verrà generato casualmente.",  FontColor -> Red, FontSize -> fontSizeDisclaimer]
          }, Center, Spacings -> buttonProcediSpacingDisclaimer],

       (* CASE playerTurn. This phase is the actual player's turn, where they can decide whether to take another card or to stand *)
        "playerTurn", 
          Column[{
            (*Displaying the game dynamic*)
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],

            Grid[{{TextCell["Seed: " <> ToString[actualSeed], "Text", FontSize -> fontSize]}}],   (* insert in a Grid element in order to center TextCell horizontally *)
            Grid[{
            (* Header row *)
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
              (* "Chiedi Carta" button. It adds another card to the player's hand *)
              Button[
                Style["Chiedi Carta", fontSize, Bold],   (* ask for a new card button *)
                (* If hit, adds a new card to the player's hand and starts a new player turn *)
                AppendTo[playerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]];
                playerScore = calculateScore[playerHand];  (* update playerScore *)
                If[playerScore > 21,  (*if playerScore is greater than 21, the player can't ask for another card nor stand, because they lose. So the phase is switched to determinWinner*)
                  phase = "determineWinner";,    (* if player goes out of bound, game over *)
                  phase = "playerTurn";];,       (* otherwise, playercontinues their turn *)
                ImageSize -> inputFieldSizes,
                Appearance -> {"DialogBox"}
              ],

              (* "Stai" button. The player chooses to stand and not to ask for any more cards *)
              Button[
                Style["Stai", fontSize, Bold],   (* End player turn *)
                phase = "dealerTurn";,  (*phase is changed to dealerTurn*)
                ImageSize -> inputFieldSizes,
                Appearance -> {"DialogBox"}
              ], 

              (* "Ricomincia partita" button. When pressed the player asks to replay the game with the same seed *)
              Button[
                (* The game is restarted with the same seed *)
                Style["Ricomincia Partita", fontSize, Bold],
                playedSeed = actualSeed;
                playingGamer = playerName;
                DialogReturn["Restart"];,          
                ImageSize -> inputFieldSizes,
                Appearance -> {"DialogBox"}
              ]
            }, "   "]
          }, Center, Spacings -> columnSpacing],   (* Center parameter align column content *)

        (* CASE dealerTurn. This phase is the one where the dealer plays *)
        "dealerTurn",
          While[dealerScore < 17,   (* Blackjack rules state that the dealer has to ask for cards until their hand value reaches at least 17*)
            AppendTo[dealerHand, RandomChoice[Complement[Range@52, playerHand, dealerHand]]]; (*Adds another card to dealer's hand*)
            dealerScore = calculateScore[dealerHand];     (* update dealerScore *)
          ];

          phase = "determineWinner"; (* Phase is changed to determineWinner *)
          
          Column[{
            (*Displaying the screen which appears before showing the actual winner and in a second moment the actual winner*)
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],
            TextCell["IL VINCITORE È...", "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
          }, Center, Spacings -> {15}],

        (* CASE determineWinner. This phase shows who actually won the game, the player, the dealer, or if it's a draw *)
        "determineWinner",
          Pause[1];   (* to show on the screen the "IL VINCITORE È..." message for one second *)
          Column[{
            (*Printing the winner screen*)
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],

            Grid[{{TextCell["Seed: " <> ToString[actualSeed], "Text", FontSize -> fontSize]}}],   (* insert in a Grid element in order to center TextCell horizontally *)
            Grid[{
            (* Header row *)
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
                playerScore = calculateScore[playerHand]; (*Player's score is calculated*)
                dealerScore = calculateScore[dealerHand]; (*Dealer's score is calculated*)

                If[playerScore > 21,    (* If the player goes out of bound, reaching a score greater than 21 *)
                  winner = "dealer";, (*The dealer wins*)
                  If[playerScore === dealerScore,  (* If the player's score and the dealer's one are equal *)
                    winner = "pareggio";, (*The game ends with a draw*)
                    If[dealerScore > 21 || playerScore > dealerScore,   (* If the dealer goes out of bound or the player's score is higher than the dealer's*)
                      winner = "player";, (*The player wins*)
                        winner = "dealer";    (* dealer'score is higher, so the dealer wins *)
                ];];];
                If[winner === "dealer",
                      (*if the dealer has won, display that the player has lost*)
                      If[!historyFlag, lose = lose + 1; historyFlag = True;];
                      TextCell["HAI PERSO, IL DEALER VINCE!", "Text", FontColor -> Red, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus],
                If[winner === "player",
                      (*if the player has won, display that the player has won*)
                      If[!historyFlag, win = win + 1; historyFlag = True;];
                      TextCell["HAI VINTO!", "Text", FontColor -> Green, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus],
                    
                      (* Otherwise, it means there was a draw, and the following message is printed *)
                      If[!historyFlag, draw = draw + 1; historyFlag = True;];
                      TextCell["PAREGGIO!", "Text", FontColor -> Orange, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
                  ]
                ]
              },
              {TextCell["Vittorie: " <> ToString[win] <> " - Sconfitte: " <> ToString[lose] <> " - Pareggi: " <> ToString[draw], "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSize]}
            }, Spacings -> spacingValuesWinnerMassage, Alignment -> {Center, Center}
            ],
            (* At the end of the game, the following buttons can be pressed *)
            Row[{
              (* "Nuova Partita" button. Starts a new game, the player is asked to insert a new seed or a randomic one will be assigned *)
              Button[
                (* Stat a new game with the same player name*) 
                Style["Nuova Partita", fontSize, Bold], 
                historyFlag = False;
                playingGamer = playerName;
                DialogReturn["NewGame"];,         
                ImageSize -> inputFieldSizes
              ],
              (* "Ricomincia Partita" button restarts the latest game with the same seed *)
              Button[
                (* Restart the game with the same seed and same player name *)
                Style["Ricomincia Partita", fontSize, Bold], 
                historyFlag = False;
                playedSeed = actualSeed;
                playingGamer = playerName;
                DialogReturn["Restart"];,                  
                ImageSize -> inputFieldSizes
              ],
              (* "Cambia giocatore" button. Lets the player decide for a new character name *)
              Button[
                (* Let the user change his player name *)
                Style["Cambia giocatore", fontSize, Bold],
                historyFlag = False;
                DialogReturn["chooseCharacter"];,                  
                ImageSize -> inputFieldSizes
              ],
              (* "Esci" button. Closes the window and ends the game *)
              Button[
                (* Closes window *)
                Style["Esci", fontSize, Bold], 
                DialogReturn["Quit"];,          
                ImageSize -> inputFieldSizes
              ]
            }, "   "]
          }, Center, Spacings -> columnSpacing],   (* Center parameter aligns column content *)

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
