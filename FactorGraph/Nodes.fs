namespace FactorGraph
open Maths
type Variable = {id: int; values: int}
type Factor =  Map<Map<Variable, int>, float>

type VariableNode(variable : Variable, neighbours : FactorNode seq )=   
    let mutable _neighbours = neighbours
    let mutable messages = Map.empty : Map<Factor, Map<int, float> >
    let message neighbour = 
        messages 
        |> Map.toSeq
        |> Seq.filter (fst >> (=) neighbour >> not)
        |> Seq.map snd
        |> Seq.reduce mult
    
    member this.Variable = variable
    member this.ReceiveMessage (neighbour: FactorNode) distribution = 
        messages <- Map.add neighbour.Factor distribution messages
    member this.Neighbours = _neighbours
    member this.SendMessage (neighbour: FactorNode) = 
        neighbour.ReceiveMessage this (message neighbour.Factor)
        
and FactorNode(factor : Factor, neighbours : VariableNode seq ) =   
    let mutable messages =  Map.empty : Map<Variable, Map<int,float> >
    let product (neighbour: Variable) (config: Map<Variable, int>) =
        config 
            |> Map.toSeq
            |> Seq.filter (fst >> (=) neighbour >> not)
            |> Seq.map (fun (var,value) ->  messages |> Map.find var |> Map.find value)
            |> Seq.reduce (*)
            
    let messageAtValue neighbour value = 
        factor
            |> Map.toSeq
            |> Seq.filter (fst >> Map.find neighbour  >> (=) value)
            |> Seq.map (fun (config, score ) -> score * product neighbour config)
            |> Seq.sum

    let message (neighbour: Variable) =
        Seq.init neighbour.values (fun i -> i, messageAtValue neighbour i)
        |> Map.ofSeq

    member this.Factor = factor
    member this.ReceiveMessage neighbour distribution = 
        messages <- Map.add  neighbour.Variable distribution messages
    member this.SendMessage (neighbour: VariableNode) = 
        neighbour.ReceiveMessage this (message neighbour.Variable)
    