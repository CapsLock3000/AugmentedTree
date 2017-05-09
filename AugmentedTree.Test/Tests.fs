module Tests

open NUnit.Framework
open FsUnit
open AugmentedTree

let exampleData = [ 5; 6; 7; 8; 1; 2; 3; 4 ]
let ``const`` _ _ _ = ()

let extractNodeValue = 
    function
    | AugmentedTree.Node(_, _, v, _, _) -> Some v
    | AugmentedTree.Empty -> None

[<Test>]
let ``trivial augmented tree test``() = 
    let tree = exampleData |> AugmentedTree.ofSeq ``const`` AugmentedTree.defaultComparer
    tree
    |> AugmentedTree.toList
    |> should equal [ 1; 2; 3; 4; 5; 6; 7; 8 ]

[<Test>]
let ``tracking size of augmented tree``() = 
    let extractSize t = 
        match t with
        | AugmentedTree.Node(_, _, _, _, size) -> size
        | AugmentedTree.Empty -> 0
    
    let sizeCalc l _ r = extractSize l + extractSize r + 1
    let tree = exampleData |> AugmentedTree.ofSeq sizeCalc AugmentedTree.defaultComparer
    let foundNode = tree |> AugmentedTree.tryFindNode AugmentedTree.defaultComparer 4
    extractSize tree |> should equal 8
    foundNode
    |> extractNodeValue
    |> should equal (Some 4)

[<Test>]
let ``find returns greatest key, but LE to input``() = 
    let tree = [ 1; 2; 4; 5 ] |> AugmentedTree.ofSeq ``const`` AugmentedTree.defaultComparer
    let foundNode = tree |> AugmentedTree.tryFindNodeOrLess AugmentedTree.defaultComparer 3
    foundNode
    |> extractNodeValue
    |> should equal (Some 2)

[<Test>]
let ``trivial nonunique tree test``() = 
    let tree = [ 1; 1; 1 ] |> AugmentedTree.ofSeq ``const`` AugmentedTree.defaultComparer
    tree
    |> AugmentedTree.toList
    |> should equal [ 1; 1; 1 ]

[<Test>]
let ``nonunique tree test``() = 
    let tree = [ 2; 1; 3; 2; 1; 1 ] |> AugmentedTree.ofSeq ``const`` AugmentedTree.defaultComparer
    tree
    |> AugmentedTree.toList
    |> should equal [ 1; 1; 1; 2; 2; 3 ]
