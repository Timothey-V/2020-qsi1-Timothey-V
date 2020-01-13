type player =
  | PlayerOne
  | PlayerTwo;

type point =
  | Love
  | Fifteen
  | Thirty

type pointsData = {
  playerOne: point,
  playerTwo: point
};

type fortyData = {
  player: player, /* The player who have forty points */
  otherPlayerPoint: point
};

type score =
| Points(pointsData)
| Forty(fortyData)
| Deuce
| Advantage(player)
| Game(player);

let scoreWhenDeuce: player => score = _ => Advantage(PlayerOne);
let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) =>
    advantagedPlayer == winner ? Game(winner) : Deuce;

/* This time we infer that the function type is (player) => player */
let other = player =>
  switch player {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };
/* We add a tool function to increment point */
let incrementPoint: point => option(point) =
  point =>
    switch point {
    | Love => Some(Fifteen)
    | Fifteen => Some(Thirty)
    | Thirty => None
    };

let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p})
      | None => Deuce
      }
    );

let pointTo = (player, point, current) =>
  switch player {
  | PlayerOne => {...current, playerOne: point}
  | PlayerTwo => {...current, playerTwo: point}
  };

let pointFor = (player, current) =>
  switch player {
  | PlayerOne => current.playerOne
  | PlayerTwo => current.playerTwo
  };

let scoreWhenPoints = (current, winner) =>
  switch (current |> pointFor(winner) |> incrementPoint) {
  | Some(np) => Points(pointTo(winner, np, current))
  | None =>
    Forty({
      player: winner,
      otherPlayerPoint: current |> pointFor(other(winner))
    })
  };

let scoreWhenGame = winner => Game(winner);

let score = (current, winner) =>
  switch current {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };

let newGame = Points({playerOne: Love, playerTwo: Love});

let string_of_player = (player) =>
  switch player {
  | PlayerOne => "Player One"
  | PlayerTwo => "Player Two"
  | _ => "Not a player"
  };

let string_of_point = (points) => 
switch points {
| Love => "Zero or 0"
| Fifteen => "Fifteen or 15"
| Thirty => "Thirty or 30"
| _ => "Not a points"
};

let string_of_score = (score) => 
switch score {
| Points(pointsData) => string_of_point(pointsData.playerOne) ++" / "++ string_of_point(pointsData.playerTwo)
| Forty(fortyData) => string_of_player(fortyData.player) ++ "has 40 points, the other player has "++ string_of_point(fortyData.otherPlayerPoint)
| Deuce => "This is a Deuce"
| Advantage(player) => "This is Advantage for the player :" ++ string_of_player(player)
| Game(player) => "The Game is won by" ++ string_of_player(player)
| _ => "Not a Score"
};