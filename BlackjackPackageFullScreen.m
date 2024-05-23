(* ::Package:: *)

BeginPackage["BlackjackPackage`"];
ClearAll["BlackjackPackage`*"];

playBlackjack::usage = "playBlackjack[] avvia il gioco di Blackjack.";
calculateScore::usage = "calculateScore[hand_] calcola il punteggio della mano di gioco data in input.";
checkInputSeed::usage = "checkInputSeed[input_] viene utilizzata nel momento in cui l'utente preme il tasto 'Procedi' nella fase di inserimento del seed. Verifica se il valore dato in input è valido come seed (ritorna True) oppure no (ritorna False).";
(*This fdunction is responsible for the execution of a single game*)
singlePlay::usage = "singlePlay[insertedName_, insertedSeed_] esegue una singola partita. I due parametri opzionali servono per l'inserimento del nome del giocatore "

Begin["`Private`"];

(* Parameters *)
fontSize = 20; (*Font size*)
fontSizeDisclaimer = 15; (*Red written text font size*)
fontSizePlus = 28;  (*Font size of important informatzions suche as Winner*)
fontSizeTitle = 35; (*Titles font size*)
inputFieldSizes = {200, 50}; (*Parameters for the input fields*)
buttonSizes = {100, 50}; (*Parameters for the button size*)
spacingValues = {20, 5};  (*Parameters for the the line spacing*)
spacingValuesWinnerMassage = {20, 2};  (*Parameters for the the line spacing of the winner message*)
buttonProcediSpacing = {3, 2, 2}; (*Parameter defining the buttons margin*)
buttonProcediSpacingDisclaimer = {3, 2, 2, 4}; (*Spacing for the button Indietro e Procedi in the ChooseSeed window*)
columnSpacing = {3, 3, 3}; (*Parameters for columns spacing*)

(* parameters used when starting new game, same game or changing player name *)
playedSeed = ""; (*Played seed*)
playingGamer = ""; (*Player name*)

(* Parameters used to keep history *)
win = 0; (*Counter for the number of vicotries*)
lose = 0; (*Counter for the number of lost games*)
draw = 0; (*Counter for the number of draws*)
(* historyFlag is used to avoid that the history counter keeps increasing more then it should *)
historyFlag = False;

(* Calculate score function. This function takes a hand of cards as input, either the player's or the dealr's *)
calculateScore[hand_List] :=
  Module[{score, aces},
    (*The graphic library, used for displaying the cards, assigns an incremental value, starting 1 and reaching 52, to each card*)
    (*In order to assign the real value to the cards, we are taking the value (between 1 and 52) and moduling it by 13 (the maximum value cards can assume)*)
    (*If the value obtained is grater than 10, it means that the card is either a Jack or a Queen, so the value it has to assume for the game is 10*)
    (*If the value obtained is equal to 0, it means the card is a King, so the value it has to assume for the game is 10*)
    (*If the value obtained is eqaul to 1, it means the card ia an Ace, so the value it has to assume for the game is 11*)
    score = Total[If[#>10||#==0,10,If[#==1,11,#]]&/@Mod[hand,13]];
    (* We need keep track of the number of aces we have in our hand, in order to know how many times we need to iterate next*)
    aces = Count[Mod[hand,13], 1];
    (*If the total score is greater than 21 and there is at least an ace in the hand, the value of the ace becomes 1 instead of 11 (this applies to every ace in the hand) *)
    While[aces > 0,               
      If[score>21, score-=10];
      aces -= 1;
    ];
  score]; (* return score *)

(* This function checks if the seed inserted is valid: this condition is verified if the number is an integer *)
checkInputSeed[input_String] :=
  Module[{valid},
    If[StringContainsQ[input, "."] || input == "", valid = False, valid = True]; (*This if checks if the input contains the "." of if the value is empty*)
  valid]; (* return true if the inserted number is valid: doesn't contain dots *)

(* Actual game function *)
playBlackjack[insertedName_:"", insertedSeed_:""] := 
  Module[{decision},
    (* Cleaning the console *)
    ClearSystemConsole[];

    (*Import Graphic library for the cards *)
    playingcardgraphic = ResourceFunction["PlayingCardGraphic"];

    (*the decision variable stores the choice of the button pressed*)
    decision = singlePlay[insertedName, insertedSeed];

    (* All the possible decision are the cases of the switch *)
    Switch[decision,    
      "NewGame",  (*Starts a new game*)
        playBlackjack[playingGamer];,
      "Quit", (*Quit the game and exits it*)
        Quit[];,
      "Restart", (*Restarts the same game*)
        playBlackjack[playingGamer, playedSeed];, 
      "chooseCharacter", (*Change the name of the player*)
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
      (*actualSeed stores the inserted seed value. This value could be either integer or an empty string*)
      (*In the first case, the game is played with that seed and it means that the player wants to replay his latest game*)
      (*In the second case, the game is started without a setted seed, which means it's the first game or a new game started with new seed*)
     playerHand, 
     dealerHand, 
     dealerHand1, 
     playerScore,
     dealerScore,
     dealerHandValue},
    
    (* dynamic function that manages the game state dynamic *)
    Dynamic[
      Switch[phase,    (* phase variable stores the game state *)

      (* CASE chooseName. The player needs to choose his character name *)
        "chooseName", 
          If[playerName =!= "" && actualSeed === "",    (* if this two conditions verify, it means the player pressed "Nuova Partita". So it doesn't need to ask for the player name again, but we change phase to chooseSeed *)
            phase = "chooseSeed";];   
          If[playerName =!= "" && actualSeed =!= "",    (* if this two conditions verify, it means the player pressed "Ricomincia Partita".  So the phase is changed to skipSeed, because we are playing with the same seed *)
            phase = "skipSeed";];

          Column[{
            (*Printing the displayed cards and text*)
            TextCell["BLACKJACK", "Text", FontFamily -> "Times", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizeTitle],

            TextCell["Inserisci il " Style["nome del giocatore", Bold], "Text", FontFamily -> "Helvetica", FontSize -> fontSize],
            InputField[Dynamic[playerName], String, ImageSize -> inputFieldSizes, BaseStyle -> {FontSize -> fontSize}],

            Button[
              Style["Procedi", fontSize, Bold],
              (* button pressed action *)
              (If[playerName === "", playerName = "Anonimo";];   (* if no name is inserted, an automatic one is assigned to the player *)
              phase = "chooseSeed";),    (* when "Procedi" button is pressed, continue with phase is changed to chooseSeed *)
              ImageSize -> buttonSizes,
              Appearance -> {"DialogBox"}
            ]
          }, Center, Spacings -> buttonProcediSpacing],

      (* CASE skipSeed. This phase means the player wants to replay the game with th*)
        "skipSeed",     (* when play restarted: when the playBlackjack function is called with seed parameter  *)
          SeedRandom[actualSeed];

          (* Player and Dealer hands initialized (here beacause it has to be done after seed is set) *)
          playerHand = RandomSample[Range@52,2];                          (* two random cards are drawn from the deck *)
          dealerHand = RandomSample[Complement[Range@52,playerHand],2];   (* two random cards are drawn from the deck, excluding the cards that have already been drawn *)
          dealerHand1 = dealerHand[[1]];                                  (* this value stores the first card the dealer has drawn. This is necessary because only one out of the two cards of the dealer can be shown from the beginning of the game*)
          (* Initializing player and dealer score *)
          playerScore = calculateScore[playerHand]; (* The value of the player hand is calculated *)
          dealerScore = calculateScore[dealerHand]; (* The value of the dealer hand is calculated *)

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

            Row[{
              (* "chiedi carta" button *)
              Button[
                Style["Indietro", fontSize, Bold],
                (* button pressed action *)
                playerName = "";
                phase = "chooseName";,    (* when "Procedi" button is pressed, continue with chooseName section *)
                ImageSize -> buttonSizes,
                Appearance -> {"DialogBox"}
              ],

              Button[
                Style["Procedi", fontSize, Bold], 
                (* button pressed action *)
                ( Pause[0.8];     (* used to wait the inputValue dynamic variable update *)
                  If[checkInputSeed[ToString[inputValue]], (* if the inserted value is valid, then the seed is set with entered value, else a random seed is set *)
                    actualSeed = inputValue; SeedRandom[inputValue];,                 (* seed entered and valid *)
                    actualSeed = RandomInteger[1000000]; SeedRandom[actualSeed];      (* seed not entered or invalid -> random *)
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
              ]
            }, "   "],
            
            TextCell["Attenzione! È possibile inserire solo numeri!\nNon inserendo un seed questo verrà generato casualmente.\nInserendo un numero non valido, il seed verrà generato casualmente.",  FontColor -> Red, FontSize -> fontSizeDisclaimer]
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
                      If[!historyFlag, lose = lose + 1; historyFlag = True;];
                      TextCell["HAI PERSO, IL DEALER VINCE!", "Text", FontColor -> Red, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
                    ),
                If[winner === "player",
                    (
                      If[!historyFlag, win = win + 1; historyFlag = True;];

                      TextCell["HAI VINTO!", "Text", FontColor -> Green, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
                    ),
                    (
                      If[!historyFlag, draw = draw + 1; historyFlag = True;];
                      TextCell["PAREGGIO!", "Text", FontColor -> Orange, FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSizePlus]
                    ) 
                  ]
                ]
              },
              {TextCell["Vittorie: " <> ToString[win] <> " - Sconfitte: " <> ToString[lose] <> " - Pareggi: " <> ToString[draw], "Text", FontWeight -> Bold, TextAlignment -> Center, FontSize -> fontSize]}
            }, Spacings -> spacingValuesWinnerMassage, Alignment -> {Center, Center}
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