module AugmentedTree.AugmentedTree

/// <summary>
/// Color used for tree balancing.
/// </summary>
type Color = 
    | Red
    | Black

/// <summary>
/// Describes relation between pair of elements.
/// </summary>
type Relation = 
    | Equal
    | Less
    | Greater

/// <summary>
/// Returns relation between pair of elements.
/// </summary>
/// <param name="x">The first element.</param>
/// <param name="y">The second element.</param>
/// <returns>Relation between first and second element.</returns>
let defaultComparer x y = 
    if x < y then Less
    else if x > y then Greater
    else Equal


/// <summary>
/// Augmented tree with keys of type 'T and attributes of type 'A.
/// </summary>
type ATree<'T, 'A> = 
    | Empty
    | Node of Color * ATree<'T, 'A> * 'T * ATree<'T, 'A> * 'A

let inline balance attributeGetter =
    function
    | B, Node(Red, Node(Red, a, x, b, _), y, c, _), z, d            
    | B, Node(Red, a, x, Node(Red, b, y, c, _), _), z, d            
    | B, a, x, Node(Red, Node(Red, b, y, c, _), z, d, _)            
    | B, a, x, Node(Red, b, y, Node(Red, c, z, d, _), _)            
        -> 
            let xn = Node(B, a, x, b, attributeGetter a x b)
            let zn = Node(B, c, z, d, attributeGetter c z d)
            Node(Red, xn, y, zn, attributeGetter xn y zn)
    | c, l, x, r -> Node(c, l, x, r, attributeGetter l x r)

/// <summary>
/// Returns a new tree containing the new key and elements
/// from input tree.
/// </summary>
/// <param name="attributeGetter">The function calculating node attribute.</param>
/// <param name="comparer">The function comparing keys.</param>
/// <param name="item">New key.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting tree.</returns>
let inline insert attributeGetter comparer item tree = 
    let rec ins = 
        function 
        | Empty -> Node(Red, Empty, item, Empty, attributeGetter Empty item Empty)
        | Node(c, a, y, b, _) -> 
            match comparer item y with
            | Equal | Less -> balance attributeGetter (c, ins a, y, b)
            | Greater -> balance attributeGetter (c, a, y, ins b)
    match ins tree with
    | Empty -> failwith "impossible"
    | Node(_, l, x, r, q) -> Node(Black, l, x, r, q)

/// <summary>
/// Finds node in tree.
/// </summary>
/// <param name="comparer">The function comparing keys.</param>
/// <param name="item">Searched key.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The node.</returns>
let inline tryFindNode comparer item tree = 
    let rec f = 
        function 
        | Empty -> Empty
        | Node(_, l, v, r, _) as node -> 
            match comparer item v with
            | Equal -> node
            | Less -> f l
            | Greater -> f r
    f tree

/// <summary>
/// Finds node in tree with greatest key, but not greater than given key.
/// </summary>
/// <param name="comparer">The function comparing keys.</param>
/// <param name="item">Searched key.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The node.</returns>
let inline tryFindNodeOrLess comparer item tree = 
    (* s is last node smaller than item *)
    let rec f s = 
        function 
        | Empty -> 
            match s with
            | Some(n) -> n
            | None -> Empty
        | Node(_, l, v, r, _) as node -> 
            match comparer item v with
            | Equal -> node
            | Less -> f s l
            | Greater -> f (Some node) r
    f None tree

/// <summary>
/// Creates a new tree from the given enumerable object.
/// </summary>
/// <param name="attributeGetter">The function calculating node attribute.</param>
/// <param name="comparer">The function comparing keys.</param>
/// <param name="source">The input sequence.</param>
/// <returns>The list of elements from the sequence.</returns>
let inline ofSeq attributeGetter comparer source =
    Seq.fold (fun x y -> insert attributeGetter comparer y x) Empty source

/// <summary>
/// Applies a function to each element of the collection, threading an accumulator 
/// argument through the computation. If the input function is f and the elements are
/// i0...iN, then this function computes f i0 (...(f iN s)).
/// The tree is processed in infix order.
/// </summary>
/// <param name="folder">The function to update the state given the input elements.</param>
/// <param name="tree">The input tree.</param>
/// <param name="state">The initial state.</param>
/// <returns>The final state value.</returns>
let rec foldBack folder tree state = 
    match tree with
    | Empty -> state
    | Node(_, l, v, r, _) -> folder (foldBack folder l state) v (foldBack folder r state)
    
/// <summary>
/// Applies the given function to each element of the collection in infix order.
/// </summary>
/// <param name="action">The function to apply to elements from the input list.</param>
/// <param name="tree">The input tree.</param>
let iter action tree = foldBack (fun () x () -> action x) tree ()

/// <summary>
/// Converts the supplied tree to a list in infix order.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The array containing the elements of the tree.</returns>
let toList tree = foldBack (fun l x r -> List.append l (x :: r)) tree List.Empty