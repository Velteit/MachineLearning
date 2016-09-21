module Trees

type BTree<'V when 'V : comparison and 'V : equality> = 
    | Node of Left: BTree<'V> * Value: 'V * Right: BTree<'V>
    | Leaf of 'V
    | Null
module BTree = 
    let getHeight tree = 
        let rec inner = function
            | Node(l, _, r) -> 
                let leftHeight = inner l 
                let rightHeight = inner r
                (max leftHeight rightHeight) + 1
            | Leaf(_) -> 1
            | Null -> 0
        inner tree

type BinaryTree<'V when 'V : comparison and 'V : equality> =
    {Tree: BTree<'V>; Comparator: 'V -> 'V  -> int}
    
type KDTree<'P,'V when 'P : comparison and 'V : comparison and 'P : equality and 'V : equality> = 
    {Tree: BTree<Item<'P, 'V>>; Distance: 'P -> 'P -> 'P}
and Item<'P, 'V> = 
    {Position: 'P; Value: 'V; Axis: int}

module BinaryTree = 
    
    let inline compare a b comparator fg fe fl = 
        match comparator a b with
        | 1     -> fg ()
        | 0     -> fe ()
        | -1    -> fl ()
        | _     ->  failwith "Wrong comapre function!"

    let inline add value tree = 
        let rec innerAdd = function
            | Node(left, nodeValue, right) as node  -> 
                compare value nodeValue tree.Comparator (fun () -> Node(left, nodeValue, innerAdd right)) (fun () -> node) (fun () -> Node(innerAdd left, nodeValue, right))
            | Leaf(leafValue) as leaf ->
                compare value leafValue tree.Comparator (fun () -> Node(Null, leafValue, Leaf(value))) (fun () -> leaf) (fun () -> Node(Leaf(value), leafValue, Null))
            | Null          -> Leaf(value)
        {tree with Tree = innerAdd (tree.Tree)}

    let inline create values comparator = 
        Seq.fold (fun tree value -> add value tree) {Tree = Null; Comparator = comparator} values

    let inline getHeight tree = BTree.getHeight tree.Tree

    