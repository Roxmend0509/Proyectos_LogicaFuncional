--Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

--Node 5 (Node 3 (Node 1 (Node 0 EmptyTree EmptyTree) EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 4(Node 6 (Node 7 EmptyTree EmptyTree) EmptyTree) (Node 9 EmptyTree EmptyTree))

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)


singleton :: a -> Tree a 
singleton x=Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a(treeInsert x left) right
	| x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree=False
treeElem x (Node a left right)
	| x == a = True
	| x < a = treeElem x left
	| x > a = treeElem x right

preorden :: Tree a -> [a]
preorden EmptyTree = []
preorden (Node x i d)= x: (preorden i ++ preorden d)

postorden :: Tree a -> [a]
postorden EmptyTree = []
postorden (Node x i d) = postorden i ++ postorden d ++ [x]

profundidad :: Tree a -> Int
profundidad EmptyTree = 0
profundidad (Node x i d)= 1 +max (profundidad i) (profundidad d)

inorden :: Tree a -> [a]
inorden EmptyTree = []
inorden (Node x i d)= inorden i ++(x : inorden d)

nNodos :: Tree a -> Int
nNodos (EmptyTree)=0
nNodos (Node x i d)=1+nNodos i+nNodos d


