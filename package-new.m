BeginPackage["BlackjackPackage`"];

playBlackjack::usage = "playBlackjack[seed_: Automatic] avvia una partita di Blackjack. Il seed è opzionale.";

Begin["`Private`"];

(* Funzione per inizializzare il mazzo di carte *)
initializeDeck[] := Flatten[Table[{11, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10}, {4}], 1];

(* Funzione per mescolare il mazzo *)
shuffleDeck[deck_List] := RandomSample[deck];

(* Funzione per inizializzare le mani di giocatore e dealer *)
initializeHands[deck_List] := Module[{playerHand, dealerHand},
    playerHand = RandomSample[deck, 2];
    dealerHand = RandomSample[Complement[deck, playerHand], 2];
    {playerHand, dealerHand}
];

(* Funzione per calcolare il punteggio di una mano *)
calculateScore[hand_List] := Module[{score},
    score = Total[hand];
    If[MemberQ[hand, 11] && score > 21, score -= 10];
    score
];

(* Funzione per mostrare le carte *)
showCards[hand_List] := StringJoin[Riffle[ToString /@ hand, ", "]];

(* Funzione per il turno del giocatore *)
playerTurn[deck_List, playerHand_List, dealerHand_List, playerScore_] := Module[{decision},
    decision = DialogInput[
        DialogNotebook[{TextCell[
            "Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[playerScore], "Text"], 
            TextCell["La carta del dealer è: " <> ToString[dealerHand[[1]]] <> ". Totale: " <> ToString[dealerHand[[1]]], "Text"], 
            TextCell["'hit' o 'stand'?", "Text"], 
            Grid[{{Button[Hit, DialogReturn["Hit"], 
                Background -> {Darker[LightBlue, 0.2], Lighter[LightBlue]}, 
                BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}], 
                Button[Stand, DialogReturn["Stand"], 
                Background -> {Darker[LightGreen, 0.2], Lighter[LightGreen]}, 
                BaseStyle -> {FontSize -> 14, FontWeight -> "Bold", FontFamily -> "Comic Sans MS", Black}]}}]}]];
    If[decision === "Hit",
        AppendTo[playerHand, RandomChoice[Complement[deck, playerHand]]];
        playerScore = calculateScore[playerHand];
        If[playerScore > 21, DialogReturn["bust"]];
        playerTurn[deck, playerHand, dealerHand, playerScore], 
        If[decision === "Stand",
            dealerHand = Join[dealerHand, RandomChoice[Complement[deck, dealerHand]]];
            dealerScore = calculateScore[dealerHand];
            Null,
            DialogReturn["cancel"]
        ]
    ]
];

(* Funzione per il turno del dealer *)
dealerTurn[deck_List, dealerHand_List] := Module[{dealerScore},
    dealerScore = calculateScore[dealerHand];
    While[dealerScore < 17,
        AppendTo[dealerHand, RandomChoice[Complement[deck, dealerHand]]];
        dealerScore = calculateScore[dealerHand];
        If[dealerScore > 21, Break[]]
    ];
    dealerScore
];

(* Funzione per determinare il vincitore *)
determineWinner[playerScore_, dealerScore_] := Module[{},
    If[playerScore > 21, Return["Il dealer vince!"]];
    If[playerScore == dealerScore, Return["Pareggio!"]];
    If[dealerScore > 21 || playerScore > dealerScore,
        Return["Hai vinto!"],
        Return["Il dealer vince!"]
    ]
];

(* Funzione per avviare una nuova partita reinizializzando le variabili *)
newGame[] := (
    Clear[playerHand, dealerHand, playerScore, dealerScore];
    Print["Inizia una nuova partita? (Sì/No)"];
    choice = ToLowerCase[InputString[]];
    If[choice === "sì" || choice === "si",
        playBlackjack[],
        Print["Partita terminata."]
    ]
);

newGameButton[] := Button["Nuova Partita", newGame[]];

(* Funzione principale per avviare il gioco *)
playBlackjack[seed_: Automatic] := Module[{deck, winner},
    newGame[]; (* reinizializza le variabili *)
    If[seed =!= Automatic, SeedRandom[seed]];
    deck = initializeDeck[];
    deck = shuffleDeck[deck];
    {playerHand, dealerHand} = initializeHands[deck];
    playerScore = calculateScore[playerHand];
    playerTurn[deck, playerHand, dealerHand, playerScore];
    dealerScore = dealerTurn[deck, dealerHand];
    winner = determineWinner[playerScore, dealerScore];
   DynamicModule[{playerHand, dealerHand, playerScore, dealerScore, winner, showNewGameButton = True},
    Column[{
        Dynamic[
            If[showNewGameButton,
                newGameButton[],
                Nothing
            ]
        ],
        Dynamic[
            If[Not@showNewGameButton,
                Column[{
                    TextCell["Le tue carte sono: " <> showCards[playerHand] <> ". Totale: " <> ToString[playerScore], "Text"], 
                    TextCell["Le carte del dealer sono: " <> showCards[dealerHand] <> ". Totale: " <> ToString[dealerScore], "Text"], 
                    TextCell[winner, Background -> LightBlue],
                    Button["Quit", showNewGameButton = True]
                }],
                Nothing
            ]
        ]
    }]
]
];

End[];

EndPackage[];
