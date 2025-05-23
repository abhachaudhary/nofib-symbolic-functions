>{-# LANGUAGE FlexibleContexts #-}

    Inferred type for 'getMin' has a constraint (MArray a (MyMaybe t) m)
    An alternative fix (better, but less faithful to backward perf
    comparison) would be MonoLocalBinds

Date:    Tue, 04 Jul 1995 13:10:58 -0400
From:    Chris_Okasaki@LOCH.MESS.CS.CMU.EDU
To:      simonpj@dcs.gla.ac.uk
Subject: Fibonacci Heaps

As I promised at the Haskell Workshop, here is a sample program
using encapsulated state.  I've translated this from SML, but
in doing so, I noticed that in fact accumArray is all the
encapsulated state you really need for this application.  In SML,
we are forced to use mutable arrays because we don't have such
fancy monolithic array "primitives" as accumArray.

I've written and tested this as a literate Gofer script because I've
never been able to get GHC to run under Mach. :-(

Let me know if you have any problems...

Chris


- ----------------------------------------------------------------------

FIBONACCI HEAPS

Fibonacci heaps are a priority queue data structure supporting the
following operations:
        O(1) Insert
        O(1) FindMin
        O(1) Meld
    O(log n) DeleteMin

(In an imperative settting, Fibonacci heaps also support
        O(1) DecreaseKey (of an indicated element)
    O(log n) Delete (an indicated element)
but these operations are problematic in a functional setting.)

There is one catch: for the DeleteMin operation, the bounds are
amortized instead of worst-case.  This means that the bounds are
only guaranteed if you use the data structure in a single-threaded manner.
Otherwise, you can take longer than expected by repeatedly going back
and operating on an "expensive" version of the data structure.

(Note: I am currently working on a paper with another student describing
a functional priority queue achieving the above bounds in the worst-case
instead of amortized.  This data structure may be freely used in a
non-single-threaded manner with no ill effects.)

To understand the implementation of Fibonacci heaps, it is helpful to
first understand binomial queues.  See, for example, David King's
"Functional Binomial Queues" from the last Glasgow workshop.

> -- partain
>module Main (main) where
>import GHC.Arr
>import System.Environment

>import Control.Monad (forM_)
>import Control.Monad.ST
>import Data.Array.ST
>import G2.Symbolic

                         --------------------

Like binomial queues, Fibonacci heaps are based on heap-ordered
binomial trees.

>data Tree a = Node a [Tree a]

The degree of a binomial tree is equal to its number of children.
Every binomial tree of degree k has binomial trees of degrees
k-1...0 as children, in that order.  It is easy to show that
a binomial tree of degree k has size 2^k.

The fundamental operation on binomial trees is linking, which compares
the roots of two binomial trees and makes the larger a child of the
smaller (thus bumping its degree by one).  It is essential that this
only be called on binomial trees of equal degree.

>link (a@(Node x as)) (b@(Node y bs)) =
>  if x <= y then Node x (b:as) else Node y (a:bs)

It will also be useful to extract the minimum element from a tree.

>root (Node x _) = x

We will frequently need to tag trees with their degrees.

>type TaggedTree a = (Int,Tree a)
>
>degree (k, t) = k
>tree (k, t) = t

Given a tagged tree, extract and tag its children.

>getChildren (n, Node x ts) = zipWith (,) [n-1,n-2 .. ] ts

Extract the minimum element from a tagged tree.

>root' = root . tree

                         --------------------

We also need a type for bags supporting constant time union.  The simple
representation given here is sufficient since we will always process bags
as a whole.  Note that for this application it is not necessary to
filter out occurences of EmptyBag.  Also, for this application order
is irrelevant.

>data Bag a = EmptyBag | ConsBag a (Bag a) | UnionBags (Bag a) (Bag a)
>
>bagToList b = flatten b []
>  where flatten EmptyBag xs = xs
>        flatten (ConsBag x b) xs = flatten b (x:xs)
>        flatten (UnionBags b1 b2) xs = flatten b1 (flatten b2 xs)
>
>applyToAll :: (a -> ST s ()) -> Bag a -> ST s ()
>applyToAll f EmptyBag = return ()
>applyToAll f (ConsBag x b) = f x >> applyToAll f b
>applyToAll f (UnionBags b1 b2) = applyToAll f b1 >> applyToAll f b2

                         --------------------

Miscellaneous stuff.

>log2 1 = 0
>log2 n = 1 + log2 (n `div` 2)

>data MyMaybe a = Zero | One a

                         --------------------

Since binomial trees only come in certain, fixed sizes, we need some
way to represent priority queues of other sizes.  We will do this
with a forest of trees summing to the correct size.

>type Forest a = Bag (TaggedTree a)

In binomial queues, this forest must be maintained in strictly increasing
order of degree.  For Fibonacci heaps, we adopt a more relaxed attitude:
degrees may be repeated and order does not matter.

To be able to find the minimum element quickly, we keep the tree with the
minimum root outside of the bag.  In addition, at the top level of each heap,
we store the total size of the heap.

>data FibHeap a = EmptyFH | FH Int (TaggedTree a) (Forest a)

                         --------------------

Now, the following operations are trivial.

>emptyFH = EmptyFH
>
>isEmptyFH EmptyFH = True
>isEmptyFH (FH _ _ _) = False
>
>singleFH x = FH 1 (0, Node x []) EmptyBag
>
>insertFH x xs = meldFH (singleFH x) xs
>
>minFH EmptyFH = error "minFH EmptyFH"
>minFH (FH n tt f) = root' tt

                         --------------------

Meld achieves its efficiency by simply unioning the two forests.

>meldFH EmptyFH xs = xs
>meldFH xs EmptyFH = xs
>meldFH (FH n1 tt1 f1) (FH n2 tt2 f2) =
>  if root' tt1 <= root' tt2 then
>      FH (n1+n2) tt1 (ConsBag tt2 (UnionBags f1 f2))
>  else
>      FH (n1+n2) tt2 (ConsBag tt1 (UnionBags f1 f2))

                         --------------------

Finally, the only hard operation is deleteMin.  After throwing away the
minimum element, it repeatedly links trees of equal degree until
no such pairs are left.  The most efficient way to do this is with
an array.  I give two implementations, one using monadic arrays,
the other using accumArray.

In the first implementation, there are three steps.
  1. Allocate an array indexed by degrees.
  2. Insert every tree into this array.  If, when inserting a tree of
     degree k, there already exists a tree of degree k, link the
     two trees and reinsert the new larger tree.
  3. Transfer the trees into a bag, keeping track of the minimum tree.

>deleteMinFH EmptyFH = error "deleteMinFH EmptyFH"
>deleteMinFH (FH 1 tt f) = EmptyFH
>deleteMinFH (FH n tt f) =
>  let
>    d = log2 (n-1) -- maximum possible degree
>
>    ins :: Ord a => STArray s Int (MyMaybe (Tree a)) -> (Int,Tree a) -> ST s ()
>    ins a (i, t) =
>        readArray a i >>= \e ->
>        case e of
>          Zero   -> writeArray a i (One t)
>          One t2 -> writeArray a i Zero >>
>                    ins a (i+1, link t t2)

Note that after inserting all the trees, the array contains trees
in the same pattern as the bits of n-1.  Since we know that the
highest order bit of n-1 is one, we know that there is a tree in
the highest slot of the array.

>    getMin a =
>        readArray a d >>= \e ->
>        case e of
>          Zero  -> error "must be One" -- since array is filled as bits of n-1
>          One t -> getMin' a d t EmptyBag 0
>    getMin' a mini mint b i =
>        if i >= d then
>          return ((mini, mint),b)
>        else
>          readArray a i >>= \e ->
>          case e of
>            Zero  -> getMin' a mini mint b (i+1)
>            One t -> if root mint <= root t then
>                       getMin' a mini mint (ConsBag (i, t) b) (i+1)
>                     else
>                       getMin' a i t (ConsBag (mini, mint) b) (i+1)
>
>  in
>    runST (newArray (0,d) Zero >>= \a ->
>           applyToAll (ins a) f >>
>           sequence (map (ins a) (getChildren tt)) >>
>           getMin a >>= \ (tt,f) ->
>           return (FH (n-1) tt f))

                         --------------------

The second version of deleteMin uses accumArray to group trees of like
size.  It then performs the linking and all remaining steps purely
functionally.

>deleteMinFH' EmptyFH = error "deleteMinFH EmptyFH"
>deleteMinFH' (FH 1 tt f) = EmptyFH
>deleteMinFH' (FH n tt f) =
>  let
>    d = log2 (n-1) -- maximum possible degree
>
>    a = accumArray (flip (:)) [] (0,d) (getChildren tt ++ bagToList f)
>
>    doLinks (ts:rest) = startup 0 ts rest
>      where startup i [] [] = []
>            startup i [] (ts:rest) = startup (i+1) ts rest
>            startup i ts [] = combine i ts [] []
>            startup i ts (next:rest) = combine i ts next rest
>
>            combine i [] next rest = startup (i+1) next rest
>            combine i [t] next rest = (i, t) : startup (i+1) next rest
>            combine i (t1:t2:ts) next rest =
>                combine i ts (link t1 t2 : next) rest
>
>    getMin (tt:rest) = foldl chooseMin (tt,EmptyBag) rest
>      where chooseMin (tt1,b) tt2 =
>                if root' tt1 <= root' tt2 then
>                    (tt1,ConsBag tt2 b)
>                else
>                    (tt2,ConsBag tt1 b)
>
>    (new_tt,new_f) = getMin (doLinks (elems a))
>  in
>    FH (n-1) new_tt new_f

                         --------------------

Testing...

f :: ((List -> List)-> List) -> List

>fibToList :: (Ord a) => (FibHeap a -> FibHeap a) -> FibHeap a -> [a]
>fibToList f xs = if isEmptyFH xs then []
>               else minFH xs : fibToList f (deleteMinFH (f xs))
>
>fibToList' :: (Ord a) => (FibHeap a -> FibHeap a) -> FibHeap a -> [a]
>fibToList' f xs = if isEmptyFH xs then []
>                else minFH xs : fibToList' f (deleteMinFH' (f xs))
>
>makeFH :: (Ord a) => [a] -> FibHeap a
>makeFH xs = foldr insertFH emptyFH xs
>
>fibSort :: (Ord a) => (FibHeap a -> FibHeap a) -> [a] -> [a]
>fibSort f = fibToList f . makeFH
>
>fibSort' :: (Ord a) => (FibHeap a -> FibHeap a) -> [a] -> [a]
>fibSort' f = fibToList' f . makeFH
>
>randoms :: Int -> [Int]
>randoms n = take n (iterate (\seed-> (77*seed+1) `rem` 1024) 1967)
>
>test f g n = fibSort f (randoms n) == fibSort' f (randoms n)

>--partain
>main = do
>   n <- mkSymbolic
>   test n `seq` return ()

>main2 symFun1 symFun2 n = test symFun1 symFun2 n
