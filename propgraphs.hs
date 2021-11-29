------------------------------------------------------------------
--                              INFO                            --
------------------------------------------------------------------
{- 
    CLASS: Llenguatges de Programació
    STUDENT: Carla Campàs Gené
    GROUP: 11
    QUARTER: 2021 - 2022 Q1 [AUTUMN TERM]
-}

------------------------------------------------------------------
--                             CAUTION                          --
------------------------------------------------------------------
{-
Program ontains an iterative main program for user input, it when 
starting the program after initial compilation it may take a little
long to run this. This is NOT an infinity loop, just be patient! :)

There are some outputs that come out in a different order than expected
    e.g. defPropEdge/defPropNode -> property data is requested before
        property
-}

------------------------------------------------------------------
--                        OBSERVATIONS                         --
------------------------------------------------------------------
{- 
1. We assume that nodes and edges are identified by their IDs, 
    for most of the functions below we will use the node/edge id rather than 
    passing the node/edge in order to simplify the functions 

2. All functions are documented with headers explaining what each paramater
    is supposed to be.

3. For those functions that require a node/edge to be passed as a parameter, the user
    can chose to use the findNode/findEdge function to extract a Node from the existing list

4. Nodes/Edges that have already been instanciated will not be changed if different 
    parameters are passed through if the function isn't explicitly created for the 
    modification of the node/edge.

5. We will assume at all points that the data being inputed is correct. The functions
    do not check repetition, rather ignore those inputs that have repeated values unless
    specified otherwise.

6. To execute the file:
    > ghc propgraphs.hs
    > ./propgraphs

7. To execute functions within the file follow the instructions when executing. 

8. To test out individual functions that can't be accessed through the main execution.
    > ghci propgraphs.hs
    A command line for haskell will open, use the name and paramaters of each function 
    to test them out

9. To end the execution of the main program, use '*'

10. While executing the program, pay close attention to the information required so as 
    to not produce erronuous ouput.

11. When executing the main program you might notice that input is interrupted by the
    program to allow for printing to take place, that is due to Haskells "Lazy Evalutaion"
    policy.

12. The assumption has been made that nodes and edges cannot share the same name. 
    [information ]

13. The assumption has been made that all relevant nodes in the graph have at least one connection,
    therefore reading the corresponding rho file will obtain all relevant nodes.

14. Function utility is explained in function summary, any implementation comments are placed directly
    above the function.

15. Nodes with capital letters/ lower case letters WILL be differentiated i.e. n1 ≠ N1    

16. For the function kHops: some instances of what the value it passed by parameter has been instanciated
    and the user has a choice amongst these, these might not be all the options available for the 
    function type, but just some basic ones.
-}

------------------------------------------------------------------
--                      DATA TYPE DECISIONS                     --
------------------------------------------------------------------
{-
data Property = Property String Val
data Node = EmptyNode | Node String Label [Property]

data PG = PG [Node] [Edge] [PropertyType]

1. Label is instanciated as a type String to make the code more visual, any string sufices to instanciate this type.

2. Data val is used to reference property types, as mentioned in the documentation.

3. Date is instanciated as three ints, this should be passed by as dd-mm-yyyy, in the case it doesn't follow this format the
    execution will fail thus, providing extra error checking.

4. Property type follows the following format {Property, DataType}, in which case DataType is one of the following tyoes:
    {String, Double, Int, Date}. Unkown types will only be obtained when finding a certain property, but a property is never 
    instanciated as undefined.

5. An Edge follows the following format {EdgeName, NodeStart, NodeEnd, Label, Properties}. Since the Property Graph is a directed
    graph, our edge instance will have a start and end node, and these will be saved in an array. NodeStart and NodeEnd are both of 
    String type and therefore contain an id of the node. This will facilitate node mainpulation wihtin the program as the node only
    has to be changed within the node container, not in every edge. Edges can also be empty and will then be instanciated as an 
    emptyEdge.

6. The nodes follow a similar format to the edges, {NodeName, Label, Properties} and will be updated following the program, 
    when required. All conenctions to this node have 

7. A property is a property name (also exists in property type) and a value extracted from its corresponding property type.
-}
------------------------------------------------------------------
--                        FUNCTION SUMMARY                      --
------------------------------------------------------------------
{- 
Extract First, Second, Last Elements:
    fE
        -> extract first element in string (words separated by spaces)
    sE
        -> extract second element in string (words separated by spaces)
    lE
        -> extract last element in string (words separated by spaces)

Helper Functions For Property Graphs in Haskell
    ls
        -> put string to lower case, used to 
    findNode
        -> Find a node in a vector of nodes
    findEdge
        -> Find an edge in a vector of edges
    makePG
        -> Initialize property graph with a vector of edges
    addLabelNode
        -> Iterate through a list of nodes to add a label to a given node
    addLabelEdge
        -> Iterate through a list of nodes to add a label to a given node
    defVLabel'
        -> Given that defVLabel might return an error, we use this as a buffer function
            to extract the error if such exists. 
        -> Since we assume that the initial data input is correct this class is used to factor
            out the Either.
    defELabel'
        -> Given that defELabel might return an error, we use this as a buffer function
            to extract the error if such exists. 
        -> Since we assume that the initial data input is correct this class is used to factor
            out the Either.
    addLabels
        -> add labels from an array of strings in format "id label" (separated by a space), uses auxiliary functions 
            defVProp', defELabel' to do this.
    addPropertyNotRepeated
        -> Make sure the properties that are being added to the hasn't been added before, if it's been added previously
            it eliminates the previous instance and updates the graph.
    addPropertiesNode
        -> Auxiliary function for defVProp, this adds the property to the corresponding node and leaves the rest intact
            if the property is already in the node, it will make the property contain the new value.
    addPropertiesEdge
        -> Auxiliary function for defEProp, this adds the property to the corresponding edge and leaves the rest intact
            if the property is already in the node, it will make the property contain the new value.
    wordsWhen
        -> Breaks the corresponding string by the given charachter (similar to words function with any characther).
    toDate
        -> Breaks date into int values to promote error checking.
    toVal
        -> Makes a string into the data type Val to include it in the property.
    toProperty
        -> Make a string into a property data type (property type, val)
    addProperties 
        -> Auxiliary function for the populate function, calls defVProp and defEProp
    addPropTypes
        -> Add property types from property file.
    fR
        -> Read file from file name
    uIO
        -> unsafePerformIO, this is a function to simplify the lines using this 

Property Graphs in Haskell
    populate
        -> Specified in documentation
        -> Populate the graph (initiate the graph from a set of files)
        -> Files required:
            rho:    File containing the edges (connections between nodes) in the format "EdgeID NodeFromID NodeToID"
            lambda: File containing the labels for each graph in the format "NodeID Label"
            sigma:  File containing the properties for each graph in the format "NodeID Property Data"
            prop:   File containing the properties from a graph in the format "Property Val"
    addNode
        -> Adds a node without repetitions into the graph, this node will only be changed in the array of nodes that the graph
            contains, all other conections are instanciated using strings for the node id.
    addEdge
        -> Specified in documentation: addEdge : PG × E × V × V → PG
        -> Adds edge to property graph, if edge already exists, doesn't add anything to the graph, otherwise a new edge is added.
        -> If a node passed by reference is not already in the graph, the node is added.
        -> This is the only way (aside from populating the graph) to add a node
    defVProp
        -> Specified in documentation defVProp : PG × V × P(Prop × Val) → PG
        -> Add a group of properties to a given node.
    defVProp'
        -> Add a property to a node     
        -> Auxiliary function used by defVProp when defining properties for a node.
    defEProp
        -> Specified in documentation defEProp : PG × E × P(Prop × Val) → PG
        -> Add a group of properties to a given edge.
    defEProp'
        -> Add a property to an edge.
        -> Auxiliary function used by defEProp when defining properties for an edge.
    labelAlreadyExists
        -> Check if a label has already been implemented for a node, if this is true the functions defVLabel and defELabel 
    labelAlreadyExistsNode
        -> Maps node to label already exists
    labelAlreadyExistsEdge
        -> Maps edge to label already exists
    defVLabel
        -> Specified in documentation defVLabel : PG × V × Lab → (PG ∪ Error)
        -> Add label to a node, if the node doesn't exist or already contains a label an error is returned. Otherwise,
            the label is added to the node.
    defELabel
        -> Specified in documentation defELabel : PG × E × Lab → (PG ∪ Error)
        -> Add label to a edge, if the edge doesn't exist or already contains a label an error is returned. Otherwise,
            the label is added to the edge.
    showGraph
        -> Specified in documentation showGraph : PG → (V, E, Lab, Prop, ρ, λ, σ)
        -> Printing has been instanciated by instanciating Show per each data type, this will insure the proper printing, thus
            we don't need to run any more comands than a simple print.
        -> Vertex printing format: v[l]{(p1, val1),(p2, val2), · · · ,(pk, valk)}
        -> Edge printing format: (n1) − e[l]−> (n2){(p1, val1),(p2, val2), · · · ,(pk, valk)}

Querying Against Property Graphs     
    containsProperty
        -> Check if a node/edge contains a certain property
    findNodesWithProperty
        -> find a property in a give node by iterating through its instanciated references, returns property undefined otherwise
    findEdgesWithProperty 
        -> find a property in a give edge by iterating through its instanciated references, returns property undefined otherwise
    getAllProperties
        -> Get all properties from a given node/edge, including those that haven't been instanciated.
    getPropertiesEdge
        -> Get all properties from a given edge, including those that haven't been instanciated.
    getPropertiesNode 
        -> Get all properties from a given node, including those that haven't been instanciated.
    sigma'
        -> Specified in documentation σ' : PG × (V ∪ E) → P(Prop × Val)
        -> Returns all properties from a given node/edge even those that haven't been already instanciated.
    findProperty 
        -> Finds a property in an array of properties that can be from a node or an edge
    propV 
        -> Specified in documentation propV : PG × Nat × Prop → P(V × Val)
        -> Returns all values that a node contins [EVEN UNDEFINED PROPERTIES].
    propE
        -> Specified in documentation propE : PG × Nat × Prop → P(E × Val)
        -> Returns all values that an edge contins [EVEN UNDEFINED PROPERTIES].
    kHops
        -> Specified in documentation kHops : PG × Nat × V × Prop × (Val × Val → Bool) × Val → P(V × Lab × Val)
        -> Given a property, k amount of steps and a function to determine what the user wants to check for the given property 
            return all values at k that have the correct properties.
        -> For this function we asume that we can repeat nodes that have already been visited.
    cA
        -> check that function applies to the correct value for the kHops algorithm
    reachable 
        -> Specified in documentation reachable : PG × V × V × Lab → Bool
        -> If there is a path from the initial vertex to the final vertex, then return true. Otherwise return false

I/O
    showLine
        -> Shows a line through standard output, used to make code cleaner to section it off in blocks
    showInitOptions
        -> Show inital options to user
    showOptionsProperty
        -> Show options for section Property Graphs in Haskell
    showOptionsQuery
        -> Show options for Querying Against Property Graphs
    getUserCommand
        -> Output and get user preferences for each command required to execute the code
    handlePopulate
        -> Show options and populate the grpah from user input
    handleAddEdge
        -> Show options and add an edge to the graph from user input
    handleDefEProp 
        -> Show options and define a property for an edge from user input
    handleDefVProp
        -> Show options and define a property for a node from user input
    handleDefVLabel
        -> Show options and define a label for a node from user input
    handleDefELabel
        -> Show options and define a label for an edge from user input
    controlPropertyInput
        -> Check which command the user wishes to run for Property Graphs in Haskell
    handleReachable 
        -> Show options and call reachable given user input
    handlePropE
        -> Show options and call propE given user input
    handlePropV
        -> Show options and call propV given user input
    showKHopsOptions
        -> show function options for kHops algorithm
    checkUndefined
        -> modify function to check if the user wants to see undefined properties or not
    kHops'
        -> handle kHops options and return function
    handleKHops
        -> Show options and call kHops given user input
    handleSigma'
        -> Show options and call sigma' given user input
    controlQueryInput
        -> Check which command the user wishes to run for Querying Against Property Graphs
    iterateForM
        -> iterate through the user input passing through a given property graph so the user doesn't lose their current position
    printError
        -> check if execution has caused any errors
    main
        -> main program
-}

{-# LANGUAGE BangPatterns #-}
import System.IO.Unsafe (unsafePerformIO)
import Data.Either
import Data.Char
------------------------------------------------------------------
--                       Data Structure                         --
------------------------------------------------------------------

type Label = String
data Val = I Int | S String | D Date | B Bool | N Double 
    deriving (Eq, Ord)
data Date = Date Int Int Int

data PropertyType = PropertyType String String deriving (Show)

data Edge = EmptyEdge | Edge String String String Label [Property]
data Property = Property String Val
data Node = EmptyNode | Node String Label [Property]

data PG = PG [Node] [Edge] [PropertyType]

------------------------------------------------------------------
--                       Show Instances                         --
------------------------------------------------------------------

instance Show Val where
    show (I x) = show x
    show (S x) = x
    show (D x) = show x
    show (B x) = show x
    show (N x) = show x

instance Show Date where
    show (Date d m y) = show d ++ "-" ++ show m ++ "-" ++ show y

instance Show Node where
    show EmptyNode          = ""
    show (Node "" _ [])     = "}\n"
    show (Node n l [])      = n ++ "[" ++ l ++ "]{}\n"
    show (Node "" _ (p:pt)) = "," ++ show p ++ show (Node "" "" pt)
    show (Node n l (p:pt))  = n ++ "[" ++ l ++ "]{" ++ show p ++ show (Node "" "" pt)

instance Show Property where
    show (Property p p1) = "(" ++ p ++ "," ++ show p1 ++ ")"

instance Show Edge where
    show EmptyEdge
            = ""
    show (Edge "" _ _ _ [])
            = "}\n"
    show (Edge e n1 n2 l [])
            = "(" ++ n1 ++ ")-" ++ e ++ "[" ++ l ++ "]->(" ++ n2 ++ "){}\n"
    show (Edge "" _ _ _ (p:pt))
            = "," ++ show p ++ show (Edge "" "" "" "" pt)
    show (Edge e n1 n2 l (p:pt))
            = "(" ++ n1 ++ ")-" ++ e ++ "[" ++ l ++ "]->(" ++ n2 ++ "){" ++ show p ++ show (Edge "" "" "" "" pt)

instance Show PG where
    show (PG [] [] _)       = ""
    show (PG [] (x:v) pt)   = show x ++ show (PG [] v pt)
    show (PG (x:v) e pt)    = show x ++ show (PG v e pt)

------------------------------------------------------------------
--                       Equal Instances                        --
------------------------------------------------------------------

instance Eq Property where
    (Property p1 _) == (Property p2 _) = p1 == p2

instance Eq Node where
    EmptyNode == EmptyNode          = True
    EmptyNode == _                  = False
    _ == EmptyNode                  = False
    (Node n1 _ _) == (Node n2 _ _)  = n1 == n2

instance Eq Edge where
    EmptyEdge == EmptyEdge                  = True
    EmptyEdge == _                          = False
    _ == EmptyEdge                          = False
    (Edge e1 _ _ _ _) == (Edge e2 _ _ _ _)  = e1 == e2

instance Eq PropertyType where
    (PropertyType p1 _) == (PropertyType p2 _) = p1 == p2

instance Eq Date where
    (Date d1 m1 y1) == (Date d2 m2 y2) = (d1 == d2) && (m1 == m2) && (y1 == y2)

instance Ord Date where
    (Date d1 m1 y1) < (Date d2 m2 y2) = (y1 < y2) || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 < d2)
    (Date d1 m1 y1) <= (Date d2 m2 y2) = (y1 < y2) || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 <= d2)
    (Date d1 m1 y1) > (Date d2 m2 y2) = (y1 > y2) || (y1 == y2 && m1 > m2) || (y1 == y2 && m1 == m2 && d1 > d2)
    (Date d1 m1 y1) >= (Date d2 m2 y2) = (y1 > y2) || (y1 == y2 && m1 > m2) || (y1 == y2 && m1 == m2 && d1 >= d2)
    max d1 d2
        | d1 < d2 = d2
        | otherwise = d1
    min d1 d2
        | d1 > d2 = d2
        | otherwise = d1
------------------------------------------------------------------
--              Extract First, Second, Last Elements            --
------------------------------------------------------------------

fE :: String -> String
fE s = head $ words s

sE :: String -> String
sE s = head $ tail $ words s

lE :: String -> String
lE s = last $ words s

------------------------------------------------------------------
--        Helper Functions For Property Graphs in Haskell       --
------------------------------------------------------------------
ls :: String -> String
ls str = [toLower s | s <- str]

findNode :: [Node] -> String -> Node
findNode x s
    | null fN   = EmptyNode
    | otherwise = head fN
    where
        fN = filter (== Node s "" []) x

findEdge :: [Edge] -> String -> Edge
findEdge x s
    | null fN   = EmptyEdge
    | otherwise = head fN
    where
        fN = filter (== Edge s "" "" "" []) x

makePG :: PG -> [String] -> PG
makePG = foldl (\ pg xs -> addEdge pg (fE xs) (sE xs) (lE xs))

addLabelNode :: [Node] -> String -> String -> [Node]
addLabelNode n nv l = map addLabelNode' n
    where
        addLabelNode' :: Node -> Node
        addLabelNode' EmptyNode = EmptyNode
        addLabelNode' (Node nx s pt)
            | nx == nv = Node nx l pt
            | otherwise = Node nx s pt

addLabelEdge :: [Edge] -> String -> String -> [Edge]
addLabelEdge n nv l = map addLabelEdge' n
    where
        addLabelEdge' :: Edge -> Edge
        addLabelEdge' EmptyEdge = EmptyEdge
        addLabelEdge' (Edge e v1 v2 s pt)
            | e == nv = Edge e v1 v2 l pt
            | otherwise = Edge e v1 v2 s pt

defVLabel' :: PG -> String -> Label -> PG
defVLabel' pg s l = fromLeft (PG [] [] []) (defVLabel pg s l)

defELabel' :: PG -> String -> Label -> PG
defELabel' pg s l = fromLeft(PG [] [] []) (defELabel pg s l)

addLabels :: PG -> [String] -> PG
addLabels = foldl (\ pg xs -> defELabel' (defVLabel' pg (fE xs) (lE xs)) (fE xs) (lE xs))

addPropertyNotRepeated :: [Property] -> Property -> [Property]
addPropertyNotRepeated prop (Property pts p) 
    | null pt = Property (ls pts) p : prop
    | otherwise = prop
    where
        pt = filter (== Property (ls pts) p) prop

addPropertiesNode :: [Node] -> String -> Property -> [Node]
addPropertiesNode n v p = map addPropertiesNode' n
    where
        addPropertiesNode' :: Node -> Node
        addPropertiesNode' EmptyNode = EmptyNode
        addPropertiesNode' (Node nv l pts)
            | nv == v = Node nv l (addPropertyNotRepeated pts p)
            | otherwise =  Node nv l pts

addPropertiesEdge :: [Edge] -> String -> Property -> [Edge]
addPropertiesEdge n v p = map addPropertiesEdge' n
    where
        addPropertiesEdge' :: Edge -> Edge
        addPropertiesEdge' EmptyEdge = EmptyEdge
        addPropertiesEdge' (Edge e v1 v2 l pts)
            | e == v = Edge e v1 v2 l (addPropertyNotRepeated pts p)
            | otherwise =  Edge e v1 v2 l pts

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    sp -> w : wordsWhen p spp
        where (w, spp) = break p sp

toDate :: String -> Date
toDate dt = Date d m y
    where
        dateBreak = wordsWhen (== '-') dt
        d = read $ head dateBreak ::Int
        m = read $ head $ tail dateBreak ::Int
        y = read $ last dateBreak ::Int

toVal :: String -> String -> Val
toVal t tt
    | tt == "Bool"      = B (read t :: Bool)
    | tt == "Date"      = D $ toDate t
    | tt == "Int"       = I (read t)
    | tt == "Double"    = N (read t :: Double)
    | otherwise         = S t

toProperty :: String -> String -> [PropertyType] -> Property
toProperty p t [] = Property p $ toVal t ""
toProperty p t ((PropertyType pt tt):ptx)
    | pt == p   = Property (ls p) $ toVal t tt
    | otherwise = toProperty p t ptx

iterateThroughAndFind :: [String] -> String -> ([String], [String])
iterateThroughAndFind p v = (filter (\s -> head (words s) == v) p, filter (\s -> head (words s) /= v) p)

addProperties :: PG -> [String] -> PG
addProperties pg [] = pg
addProperties (PG v e pt) props
    | findNode v h /= EmptyNode = addProperties (defVProp (PG v e pt) h p') r
    | otherwise = addProperties (defEProp (PG v e pt) h p') r
    where
        (p, r) = iterateThroughAndFind props h
        p' = [toProperty (ls $ sE x) (lE x) pt | x <- p]
        h = head $ words $ head props -- vertex 

addPropTypes :: PG -> [String] -> PG
addPropTypes pg [] = pg
addPropTypes (PG v e pt) (x:s) = addPropTypes (PG v e (pt ++ [PropertyType (ls $ fE x) (lE x)])) s

fR :: FilePath -> IO String
fR r = do
    readFile r

uIO :: IO a -> a
uIO = unsafePerformIO

------------------------------------------------------------------
--                  Property Graphs in Haskell                  --
------------------------------------------------------------------

populate :: String -> String -> String -> String -> PG
populate r l s p = pg
    where
        pg_rho      = makePG  (PG [] [] []) (lines $ uIO $ fR r)
        pg_lambda   = addLabels pg_rho $ lines $ uIO $ fR l
        pg_prop     = addPropTypes pg_lambda $ lines $ uIO $ fR p
        pg          = addProperties pg_prop $ lines $ uIO $ fR s

addNode :: [Node] -> String -> [Node]
addNode n s
    | nd == EmptyNode = n ++ [Node s "" []]
    | otherwise = n
    where
        nd = findNode n s

addEdge :: PG -> String -> String -> String -> PG
addEdge (PG vpg epg pt) e v1 v2 = PG vpg_new epg_new pt
    where
        vpg_new = addNode (addNode vpg v1) v2
        epg_new = addEdge'
        fe = findEdge epg e

        addEdge' :: [Edge]
        addEdge'
            | fe == EmptyEdge = epg ++ [Edge e v1 v2 "" []]
            | otherwise = epg

defVProp :: PG -> String -> [Property] -> PG
defVProp pg v = foldl (`defVProp'` v) pg

defVProp' :: PG -> String -> Property -> PG
defVProp' (PG v e pt) vput p = PG (addPropertiesNode v vput p) e pt

defEProp :: PG -> String -> [Property] -> PG
defEProp pg v = foldl (`defEProp'` v) pg

defEProp' :: PG -> String -> Property -> PG
defEProp' (PG v e pt) eput p = PG v (addPropertiesEdge e eput p) pt

labelAlreadyExists :: String -> Bool
labelAlreadyExists "" = False
labelAlreadyExists _ = True

labelAlreadyExistsNode :: Node -> Bool
labelAlreadyExistsNode EmptyNode = False
labelAlreadyExistsNode (Node _ l _) = labelAlreadyExists l

labelAlreadyExistsEdge :: Edge -> Bool
labelAlreadyExistsEdge EmptyEdge = False
labelAlreadyExistsEdge (Edge _ _ _ l _) = labelAlreadyExists l

defVLabel :: PG -> String -> Label -> Either PG String
defVLabel (PG vt e pt) v l
    | labelAlreadyExistsNode (findNode vt v) = Right "Error label already exists"
    | otherwise = Left (PG (addLabelNode vt v l)  e pt)

defELabel :: PG -> String -> Label -> Either PG String
defELabel (PG v et pt) e l
    | labelAlreadyExistsEdge (findEdge et e) = Right "Error label already exists"
    | otherwise = Left (PG v (addLabelEdge et e l) pt)

showGraph :: PG -> IO()
showGraph !pg = do
    print pg

------------------------------------------------------------------
--                Querying Against Property Graphs              --
------------------------------------------------------------------

containsProperty :: [Property] -> Property -> Bool
containsProperty pts p = elem p pts

findNodesWithProperty :: [Node] -> Property -> [Node]
findNodesWithProperty [] _ = []
findNodesWithProperty (EmptyNode:x) p = findNodesWithProperty x p
findNodesWithProperty ((Node v l pt):x) p
    | containsProperty pt p = Node v l pt : findNodesWithProperty x p
    | otherwise             = findNodesWithProperty x p

findEdgesWithProperty :: [Edge] -> Property -> [Edge]
findEdgesWithProperty [] _ = []
findEdgesWithProperty (EmptyEdge:x) p = findEdgesWithProperty x p
findEdgesWithProperty ((Edge e v1 v2 l pt):x) p
    | containsProperty pt p = Edge e v1 v2 l pt : findEdgesWithProperty x p
    | otherwise             = findEdgesWithProperty x p

getAllProperties :: [Property] -> [PropertyType] -> [Property]
getAllProperties _ [] = []
getAllProperties p ((PropertyType pts _):pt) = Property pts propFin:getAllProperties p pt
    where
        propFin = findProperty p pts

getPropertiesEdge :: Edge -> [PropertyType] -> [Property]
getPropertiesEdge EmptyEdge _ = []
getPropertiesEdge (Edge _ _ _ _ ps) pt = getAllProperties ps pt

getPropertiesNode :: Node -> [PropertyType] -> [Property]
getPropertiesNode EmptyNode _ = []
getPropertiesNode (Node _ _ ps) pt = getAllProperties ps pt

sigma' :: PG -> String -> [Property]
sigma' (PG v e pt) s
    | null props = getPropertiesEdge (findEdge e s) pt
    | otherwise = props
    where
        props = getPropertiesNode (findNode v s) pt

findProperty :: [Property] -> String -> Val
findProperty [] _ = S "⊥"
findProperty ((Property p pd):v) p1
    | p == p1 = pd
    | otherwise = findProperty v p1

propV :: PG -> Int -> String -> [(Label, Val)]
propV (PG v _ pt) k p = plV
    where
        n = findNodesWithProperty v (toProperty p "" pt)
        nodes = take k n
        plV = [(l, findProperty pt1 p) | (Node _ l pt1) <- nodes]

propE :: PG -> Int -> String -> [(Label, Val)]
propE (PG _ e pt) k p = plV
    where
        eds = findEdgesWithProperty e (toProperty p "" pt)
        edges = take k eds
        plV = [(l, findProperty pt1 p) | (Edge _ _ _ l pt1) <- edges]

remDups :: (Eq a) => [a] -> [a]
remDups [] = []
remDups (x:xs) = x : remDups (filter (/= x) xs)

kHops :: PG -> Integer -> Node -> String -> (Val -> Val -> Bool) -> Val -> [(String, Label, Val)]
kHops _ _ EmptyNode _ _ _ = []
kHops (PG _ [] _) _ _ _ _ _ = []
kHops _ 0 (Node v l pts) p f x
    | cA (Node v l pts) p f x = [(v, l, findProperty pts p)]
    | otherwise = []
kHops (PG vpg epg ptpg) k (Node v _ _) p f x = iterateKHops vv'
    where
        vv' = [findNode vpg v2 | (Edge _ v1 v2 _ _) <- epg, v1 == v]

        iterateKHops :: [Node] -> [(String, Label, Val)]
        iterateKHops [] = []
        iterateKHops (vhops : vaux) = 
            remDups (kHops (PG vpg epg ptpg) (k-1) vhops p f x ++ iterateKHops vaux)

cA :: Node -> String -> (Val -> Val -> Bool) -> Val -> Bool
cA EmptyNode _ _ _= False
cA (Node _ _ pts) p f x = f (findProperty pts p) x

reachable :: PG -> Node -> Node -> Label -> Bool
reachable _ EmptyNode EmptyNode _ = False
reachable _ EmptyNode _ _         = False
reachable _ _ EmptyNode _         = False
reachable (PG vpg epg ppg) (Node s _ _) (Node e n d) l
    | s == e    = True
    | null vpg || null epg || null v = False
    | otherwise = any (\x -> reachable (PG vpg epg' ppg) x (Node e n d) l) v
    where
        v = [findNode vpg v2 | (Edge _ v1 v2 le _) <- epg, v1 == s, le == l]
        epg' = [Edge e1 v1 v2 le pt | (Edge e1 v1 v2 le pt) <- epg, v1 /= s, v2 /= s]

------------------------------------------------------------------
--                             I/O                              --
------------------------------------------------------------------
showLine :: IO ()
showLine = do putStrLn "---------------------------------------------------------------"

showInitOptions :: IO ()
showInitOptions = do
    showLine
    putStrLn "property : to add or modify settings in the property graph"
    putStrLn "query : to query the existing property graph"
    putStrLn "* : stop execution"
    showLine

showOptionsProperty :: IO()
showOptionsProperty = do
    putStrLn "1. populate : new property graph from files"
    putStrLn "2. addEdge : add an edge to the existing graph"
    putStrLn "3. defPropEdge : add an property to an existing edge"
    putStrLn "4. defPropNode : add an property to an existing node"
    putStrLn "5. defLabelEdge : add label to an existing edge"
    putStrLn "6. defLabelNode : add label to an existing node"
    putStrLn "7. show : show current graph"
    showLine

showOptionsQuery :: IO()
showOptionsQuery = do
    putStrLn "1. sigma' : show all properties for a given node/edge"
    putStrLn "2. propV : add an edge to the existing graph"
    putStrLn "4. propE : add an property to an existing edge"
    putStrLn "5. kHops : add an property to an existing node"
    putStrLn "6. reachable : add label to an existing edge"
    showLine

getUserCommand :: String -> IO String
getUserCommand !op = do
    print op
    getLine

handlePopulate :: PG
handlePopulate = ($!) populate rho lambda sigma prop
    where
        b = "Escriu nom arxiu "
        !rho = uIO $ getUserCommand $ b ++ "rho"
        !lambda = uIO $ getUserCommand $ b ++ "lambda"
        !sigma = uIO $ getUserCommand $ b ++ "sigma"
        !prop = uIO $ getUserCommand $ b ++ "prop"

handleAddEdge :: PG -> PG
handleAddEdge pg = addEdge pg edg n1 n2
    where
        !edg = uIO $ getUserCommand "Input edge name: "
        !n1 = uIO $ getUserCommand "Input start node name: "
        !n2 = uIO $ getUserCommand "Input end node name: "

handleDefEProp :: PG -> PG
handleDefEProp (PG vpg epg ptpg) = defEProp' (PG vpg epg ptpg) e (toProperty p pd ptpg)
    where
        !e = uIO $ getUserCommand "Input edge name: "
        !p = uIO $ getUserCommand "Input property name: "
        !pd = uIO $ getUserCommand "Input property data: "

handleDefVProp :: PG -> PG
handleDefVProp (PG vpg epg ptpg) = defVProp' (PG vpg epg ptpg) v (toProperty p pd ptpg)
    where
        !v = uIO $ getUserCommand "Input node name: "
        !p = uIO $ getUserCommand "Input property name: "
        !pd = uIO $ getUserCommand "Input property data: "

handleDefVLabel :: PG -> Either PG String
handleDefVLabel pg = defVLabel pg v l
    where
        !v = uIO $ getUserCommand "Input node name: "
        !l = uIO $ getUserCommand "Input label: "

handleDefELabel :: PG -> Either PG String
handleDefELabel pg = defELabel pg e l
    where
        !e = uIO $ getUserCommand "Input edge name: "
        !l = uIO $ getUserCommand "Input label: "

controlPropertyInput :: String -> PG -> Either PG String
controlPropertyInput op pg
    | op == "populate"      = Left handlePopulate
    | op == "addEdge"       = Left $ handleAddEdge pg
    | op == "defPropEdge"   = Left $ handleDefEProp pg
    | op == "defPropNode"   = Left $ handleDefVProp pg
    | op == "defLabelNode"  = handleDefVLabel pg
    | op == "defLabelEdge"  = handleDefELabel pg
    | op == "show"          = Left pg
    | otherwise             = Right "ERROR: incorrect option property graph"

handleReachable :: PG -> IO ()
handleReachable (PG vpg epg pt) = do
    let s = uIO $ getUserCommand "Input start node: "
    let e = uIO $ getUserCommand "Input end node: "
    let l = uIO $ getUserCommand "Input label node: "

    let sv = findNode vpg s
    let ev = findNode vpg e
    print (reachable (PG vpg epg pt) sv ev l)

handlePropE :: PG -> IO ()
handlePropE pg = do
    let !k =  read (uIO $ getUserCommand "Input number of pairs wanted: ") :: Int
    let !prop = uIO $ getUserCommand "Input property: "
    print (propE pg k prop)

handlePropV :: PG -> IO ()
handlePropV pg = do
    let k =  read (uIO $ getUserCommand "Input number of pairs wanted: ") :: Int
    let prop = uIO $ getUserCommand "Input property: "
    print (propV pg k prop)

showKHopsOptions :: IO ()
showKHopsOptions = do
    showLine
    putStrLn "Possible functions"
    putStrLn "      - not equal"
    putStrLn "      - equals"
    putStrLn "      - greater than"
    putStrLn "      - less than"
    putStrLn "      - greater than equal to"
    putStrLn "      - less than equal to"
    putStrLn "      - always true"
    putStrLn "      - always false"
    putStrLn "      To check for undefined/defined enter ⊥"
    showLine

kHops' :: String -> (Val -> Val -> Bool)
kHops' s
    | s == "not equal" = (/=)
    | s == "equals" = (==)
    | s == "greater than" = (>)
    | s == "less than" = (<)
    | s == "greater than equal to" = (>=)
    | s == "less than equal to" = (<=)
    | s == "always true" = \_ _ -> True
    | otherwise = \_ _ -> False

checkUndefined :: (Val -> Val -> Bool) -> String -> (Val -> Val -> Bool)
checkUndefined f s
    | s == "no" = \x y -> f x y && x /= S "⊥"
    | otherwise = f

handleKHops :: PG -> IO ()
handleKHops (PG v e pt) = do
    let !k = read (uIO $ getUserCommand "Input number of hops (k): ") :: Integer
    let !s = uIO $ getUserCommand "Input start node: "
    let !prop = uIO $ getUserCommand "Input property: "
    let !vl = uIO $ getUserCommand "Input comparison value: "
    showKHopsOptions
    let !f = uIO $ getUserCommand "Pick function: "
    let !x = uIO $ getUserCommand "Do you want to show undefined properties (yes/no): "
    let !func = kHops' f
    let !fFinal = checkUndefined func x

    let Property _ pd = toProperty prop vl pt
    print (kHops (PG v e pt) k (findNode v s) prop fFinal pd)

handleSigma' :: PG -> IO ()
handleSigma' pg = do
    let v = uIO $ getUserCommand "Input node/edge name: "
    print (sigma' pg v)

controlQueryInput :: String -> PG -> IO ()
controlQueryInput op pg
    | op == "sigma'"     = handleSigma' pg
    | op == "propV"      = handlePropV pg
    | op == "propE"      = handlePropE pg
    | op == "kHops"      = handleKHops pg
    | op == "reachable" = handleReachable pg
    | otherwise          = do print "ERROR: incorrect option query graph"

iterateForM :: PG -> IO String
iterateForM pg = do
    showInitOptions
    io <- getLine
    if io /= "*" then do
        if io == "property" then do
            showOptionsProperty
            inp <- getLine

            let r = controlPropertyInput inp pg
            let rl = fromLeft (PG [] [] []) r
            let rr = fromRight "" r

            if rr /= "" then do return rr
            else do showGraph rl ; iterateForM rl

        else do
            if io == "query" then do
                showOptionsQuery
                inp <- getLine

                controlQueryInput inp pg
                iterateForM pg
            else
                return "ERROR: incorrect initial option"
    else
        return ""

printError :: String -> IO ()
printError s
    | s /= "" = do putStrLn s ; return ()
    | otherwise = return ()

main :: IO ()
main = do
    let it = iterateForM (PG [] [] [])
    printError $ uIO it
    putStrLn "Llenguatges de Programació 2021-2022 Q1 © Carla Campàs Gené"
    return ()