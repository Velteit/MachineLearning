type BinaryTree<'T> =
    | Node of Left: BinaryTree<'T> * Value : 'T * Right : BinaryTree<'T>
    | Leaf of 'T
    | Null

let rgen = System.Random()
let data = 
    [for _ in 1..rgen.Next(500) -> rgen.Next(0, 100)]

let createTree (data : 'T list) = 
    let data' = data |> List.sort |> List.distinct |> List.toArray
    let rec iter (data: 'T []) l r = 
        if l = r 
        then
            if l < data.Length then Leaf(data.[l]) else Null
        else
            let mean = (l + r) / 2
            let leftLeaf = iter data l mean
            let rightLeaf = iter data (mean + 1) r
            Node(leftLeaf, data.[mean], rightLeaf)
    iter data' 0 (data'.Length)

createTree data
data |> List.sort 