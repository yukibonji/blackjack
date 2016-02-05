open System

printfn "Blackjack Sim 0.1"

let report = false

let shuffle n =
  let shuffle' (cards : int array) =
    let swap i j =
      let t = cards.[i]
      cards.[i] <- cards.[j]
      cards.[j] <- t
    let rand = new Random()
    let len = Array.length cards
    for i in 0 .. (len - 2) do
      swap i (rand.Next(i, len))
    cards
  let quarter = [|11; 2; 3; 4; 5; 6; 7; 8; 9; 10; 10; 10; 10|]
  [| for i in 1 .. n * 52 -> quarter.[i % 13] |] |> shuffle' |> List.ofArray

type Hand =
  | Hard of int
  | Soft of int

type Phase =
  | Initial
  | Player
  | Dealer

type IDealer =
  abstract member Hit: Hand -> bool
  abstract member Shuffle: int list -> bool * int list

type IPlayer<'count> =
  // abstract member Seen: int -> 'count -> 'count
  abstract member Seen: int -> (int * int) -> (int * int)
  abstract member Shuffled: 'count -> 'count
  abstract member Bet: 'count -> float
  abstract member Surrender: Hand -> Hand -> 'count -> bool
  abstract member Split: int -> Hand -> 'count -> bool
  abstract member Double: Hand -> Hand -> 'count -> bool
  abstract member Hit: Hand -> Hand -> 'count -> bool

let rec play bet phand (player: IPlayer<int * int>) pcount (dealer: IDealer) dhand phase shoe winnings iter max =
  if iter >= max then winnings else
    let next win =
      if bet = 0. then iter else
        let iter' = iter + 1I
        if iter' % 10000I = 0I then printf "."
        if iter' % 1000000I = 0I then
          let winnings' = winnings + win
          printfn "%A (won $%f [%f%%])" iter' winnings' (winnings' / float iter' * 100.)
        iter'
    let draw hand shoe pcount =
      let rec card shoe pcount =
        let fresh, shoe = dealer.Shuffle shoe
        if fresh then
          if report then printfn "Shuffling..."
          let pcount' = player.Shuffled pcount
          card shoe pcount'
        else
          match shoe with
          | c :: shoe' ->
            let pcount' = player.Seen c pcount
            c, shoe', pcount'
          | _ -> failwith "Invalid shoe"
      let c, shoe', pcount' = card shoe pcount
      (match hand with
       | Hard h ->
         if c = 11 then
           if h <= 10 then Soft (h + c) else Hard (h + 1)
         else Hard (h + c)
       | Soft s ->
         if s + c <= 21 then Soft (s + c)
         else Hard (s + c - 10)), c, shoe', pcount'
    match phase with
    | Initial ->
      // TODO: dealer 21
      let d, d0, shoe, pcount = draw (Hard 0) shoe pcount
      let d', d1, shoe, pcount = draw d shoe pcount
      let p, p0, shoe, pcount = draw (Hard 0) shoe pcount
      let p, p1, shoe, pcount = draw p shoe pcount
      if report then printfn "Deal: %A - %A (%i, %i)" d p p0 p1
      if d' = Soft 21 then
        if report then printfn "Dealer Blackjack"
        if p = Soft 21 then
          if report then printfn "So does player (push)"
          play (player.Bet pcount) (Hard 0) player pcount dealer (Hard 0, Hard 0) Initial shoe winnings (next 0.) max
        else
          if report then printfn "Player loses"
          play (player.Bet pcount) (Hard 0) player pcount dealer (Hard 0, Hard 0) Initial shoe (winnings - bet) (next -bet) max
      elif p = Soft 21 then
        if report then printfn "Player Blackjack!"
        let win = bet * 1.5
        play (player.Bet pcount) (Hard 0) player pcount dealer (Hard 0, Hard 0) Initial shoe (winnings + win) (next win) max
      else
        if player.Surrender phand (fst dhand) pcount then
          if report then printfn "Player surrenders"
          let lose = -(bet / 2.)
          play (player.Bet pcount) (Hard 0) player pcount dealer (Hard 0, Hard 0) Initial shoe (winnings + lose) (next lose) max
        elif false (* TODO *) && p0 = p1 && player.Split p0 (fst dhand) pcount then
          if report then printfn "Player splits: %A %A" p0 p1
          // TODO
          play bet p player pcount dealer (d, d') Player shoe winnings iter max
        elif player.Double phand (fst dhand) pcount then
          if report then printfn "Player doubles"
          let p, _, shoe, pcount = draw p shoe pcount
          play (bet * 2.) p player pcount dealer (d, d') Dealer shoe winnings iter max
        else play bet p player pcount dealer (d, d') Player shoe winnings iter max
    | Player ->
      if player.Hit phand (fst dhand) pcount then
        let phand', _, shoe', pcount' = draw phand shoe pcount
        if report then printfn "Player hit: %A" phand'
        play bet phand' player pcount' dealer dhand Player shoe' winnings iter max
      else play bet phand player pcount dealer dhand Dealer shoe winnings iter max
    | Dealer ->
      let d, d' = dhand
      if dealer.Hit d' then
        let d', _, shoe, pcount = draw d' shoe pcount
        if report then printfn "Dealer hit: %A" d'
        play bet phand player pcount dealer (d, d') phase shoe winnings iter max
      else
        let value = function Hard h | Soft h -> h
        let pv, dv = value phand, dhand |> snd |> value
        let win = if pv <= 21 && (pv > dv || dv > 21) then bet elif pv = dv then 0. else -bet
        if report then printfn "Settle: %A %A (win %f)" d' phand win
        play (player.Bet pcount) (Hard 0) player pcount dealer (Hard 0, Hard 0) Initial shoe (winnings + win) (next win) max

type Dealer() =
  interface IDealer with
    member x.Hit hand =
      match hand with
      | Hard h -> h < 17
      | Soft s -> s <= 17
    member x.Shuffle shoe =
      if List.length shoe < 104 then true, shuffle 6 else false, shoe

let dealer = new Dealer()

type PlayerDoNothing() =
  interface IPlayer<int * int> with
    member x.Seen card count = count
    member x.Shuffled count = count
    member x.Bet count = 1.
    member x.Surrender player dealer count = false
    member x.Split pair dealer count = false
    member x.Double player dealer count = false
    member x.Hit player dealer count = false

type PlayerSuperSafe() =
  interface IPlayer<int * int> with
    member x.Seen card count = count
    member x.Shuffled count = count
    member x.Bet count = 1.
    member x.Surrender player dealer count = false
    member x.Split pair dealer count = false
    member x.Double player dealer count = false
    member x.Hit player dealer count =
      match player with
      | Hard h -> h <= 11
      | Soft s -> s <= 17

type PlayerLikeDealer() =
  interface IPlayer<int * int> with
    member x.Seen card count = count
    member x.Shuffled count = count
    member x.Bet count = 1.
    member x.Surrender player dealer count = false
    member x.Split pair dealer count = false
    member x.Double player dealer count = false
    member x.Hit player dealer count =
      match player with
      | Hard h -> h < 17
      | Soft s -> s <= 17

type PlayerBasicStrategy() =
  interface IPlayer<int * int> with
    member x.Seen card count =
      let c, n = count
      (match card with
       | 2 | 3 | 4 | 5 | 6 -> c + 1
       | 10 | 11 -> c - 1
       | _ -> c), n + 1
    member x.Shuffled count = 0, 0
    member x.Bet count = 1.
//      let c, n = count
//      if n > 3 * 52 then 1. else
//         let b = c |> max 1 |> float
//         if b > 0. then b else 1.
//         if c > 10 then 10.
//         elif c > 5 then 3.
//         elif c > 3 then 2.
//         elif c < -10 then 0.
//         else 1.
    member x.Surrender player dealer count =
      match player with
      | Hard 15 -> match dealer with Hard 10 -> true | _ -> false
      | Hard 16 -> match dealer with Hard 9 | Hard 10 | Hard 11 -> true | _ -> false
      | _ -> false
    member x.Split pair dealer count =
      match pair with
      | 2 | 3 -> match dealer with Hard 8 | Hard 9 | Hard 10 | Hard 11 -> false | _ -> true
      | 4 -> match dealer with Hard 5 | Hard 6 -> true | _ -> false
      | 5 -> false
      | 6 -> match dealer with Hard h when h <= 6 -> true | _ -> false
      | 7 -> match dealer with Hard h when h <= 7 -> true | _ -> false
      | 9 -> match dealer with Hard 7 -> false | Hard h when h <= 9 -> true | _ -> false
      | 10 -> false
      | 8 | 11 -> true
      | _ -> failwith "Invalid pair"
    member x.Double player dealer count =
      match player with
      | Hard 9 -> match dealer with Hard 3 | Hard 4 | Hard 5 | Hard 6 -> true | _ -> false
      | Hard 10 -> match dealer with Hard 10 | Hard 11 -> false | _ -> true
      | Hard 11 -> match dealer with Hard 11 -> false | _ -> true
      | Soft 13 | Soft 14 -> match dealer with Hard 5 | Hard 6 -> true | _ -> false
      | Soft 15 | Soft 16 -> match dealer with Hard 4 | Hard 5 | Hard 6 -> true | _ -> false
      | Soft 17 | Soft 18 -> match dealer with Hard 3 | Hard 4 | Hard 5 | Hard 6 -> true | _ -> false
      | _ -> false
    member x.Hit player dealer count =
      match player with
      | Hard 4 | Hard 5 | Hard 6 | Hard 7 | Hard 8 | Hard 9 | Hard 10 | Hard 11 -> true
      | Hard 12 ->
        match dealer with Hard 4 | Hard 5 | Hard 6 -> false | _ -> true
      | Hard 13 | Hard 14 | Hard 15 | Hard 16 ->
        match dealer with Hard 2 | Hard 3 | Hard 4 | Hard 5 | Hard 6 -> false | _ -> true
      | Hard h when h >= 17 -> false
      | Soft 13 | Soft 14 | Soft 15 | Soft 16 | Soft 17 -> true
      | Soft 18 ->
        match dealer with Hard 9 | Hard 10 | Hard 11 -> true | _ -> false
      | Soft 19 | Soft 20 | Soft 21 -> false
      | _ -> sprintf "Invalid player hand: %A" player |> failwith

let player =
  // new PlayerDoNothing() // ~17.3%
  // new PlayerSuperSafe() // ~4.6%
  // new PlayerLikeDealer() // ~4.1%
  new PlayerBasicStrategy()

let count = (0, 0)
let bet = 1. // player.Bet count

play bet (Hard 0) player count dealer (Hard 0, Hard 0) Initial [] 0. 0I 1000000000I
|> printfn "Total winning: %f"

// TODO
// - Player only care about dealer *value*
