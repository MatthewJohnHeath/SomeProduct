namespace FactorGraph
module Maths=
    let rebracketR mapPairToSomething =
        mapPairToSomething 
            |> Map.toSeq 
            |> Seq.map (fun ((a,b),c) -> a,(b,c) )
            |> Map.ofSeq

    let rebracketL mapSomethingToPair =
        mapSomethingToPair
            |> Map.toSeq 
            |> Seq.map (fun (a,(b,c)) -> (a,b) ,c ) 
            |> Map.ofSeq

    let mult (dic1: Map<int,float>) (dic2: Map<int,float>) =
        dic1 |> Seq.zip dic2
        |> Seq.map (fun ( KeyValue(key1,val1),KeyValue(_,val2) ) -> key1, val1*val2 )
        |> Map.ofSeq

    let plus (dic1: Map<int,float>) (dic2: Map<int,float>) =
        dic1 |> Seq.zip dic2
        |> Seq.map (fun ( KeyValue(key1,val1),KeyValue(_,val2) ) -> key1, val1+val2 )
        |> Map.ofSeq