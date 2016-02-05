printfn "Blackjack Solver 0.1"

let cards deck =
  [11I, (1I, 13I)
   2I,  (1I, 13I)
   3I,  (1I, 13I)
   4I,  (1I, 13I)
   5I,  (1I, 13I)
   6I,  (1I, 13I)
   7I,  (1I, 13I)
   8I,  (1I, 13I)
   9I,  (1I, 13I)
   10I, (4I, 13I)]

type Hand =
  | Hard of bigint
  | Soft of bigint

let count cards =
  let count' (aces, total) card =
    let t = total + card
    let a = aces + if card = 11I then 1 else 0
    if t > 21I && a > 0 then a - 1, t - 10I else a, t
  match Seq.fold count' (0, 0I) cards with
  | 0, t -> Hard t
  | _, t -> Soft t

let categorize = function
  | Soft v | Hard v -> if v > 21I then 0I else v

let mutable iters = 0

let rec play showing dealer player deck (n, m) = seq {
  iters <- iters + 1
  if List.length dealer < 1 then // dealer's up card
    for (c, (n', m')) in cards deck do
      yield! play c (c :: dealer) player deck (1I, 1I) // odds per up card
  elif List.length dealer < 2 then
    for (c, (n', m')) in cards deck do
      let odds = (n * n', m * m')
      yield! play showing (c :: dealer) player deck odds
  else
    let hit =
      match count dealer with
      | Soft s -> s <= 17I
      | Hard h -> h < 17I
    if hit then
      for (c, (n', m')) in cards deck do
        let odds = (n * n', m * m')
        yield! play showing (c :: dealer) player deck odds
    else yield (showing, dealer, (n, m)) }

// count [11I] |> printfn "A = %A"
// count [11I;10I] |> printfn "A 10 = %A"
// count [11I;10I;3I] |> printfn "A 10 3 = %A"
// count [11I;11I;3I] |> printfn "A A 3 = %A"
// count [10I;3I] |> printfn "10 3 = %A"

let rec gcd x y = if y = 0I then abs x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let add (n, m) (n', m') =
  let c = lcm m m'
  let nn = n * (c / m)
  let nn' = n' * (c / m')
  nn + nn', c

let name card =
  if card = 11I then "A"
  elif card = 10I then "T"
  else sprintf "%A" card

printfn "Dealer shows:"
play 123I [] [] [] (1I, 1I)
|> Seq.map (fun (showing, cards, odds) -> showing, count cards |> categorize, odds)
|> Seq.groupBy (fun (showing, _, _) -> showing)
|> Seq.map (fun (showing, ways) -> showing, Seq.map (fun (_, total, way) -> total, way) ways)
|> Seq.map (fun (showing, ways) -> showing, Seq.groupBy fst ways) // by totals
|> Seq.map (fun (showing, totals) -> showing, totals |> Seq.map (fun (total, ways) -> total, ways |> Seq.map snd |> Seq.reduce add))
|> Seq.iter (fun (showing, results) ->
  showing |> name |> printfn "%s"
  Seq.iter (fun (t, (n, m)) ->
    let odds =
      // let g = gcd n m
      // sprintf "%A:%A" (n / g) (m / g)
      System.Numerics.BigInteger.Divide(n * 10000I, m) |> float |> (fun x -> x / 100.)
    if t = 0I then printfn "  Bust %A" odds else printfn "  %A %A" t odds) results
  )

printfn "Iterations: %i" iters
