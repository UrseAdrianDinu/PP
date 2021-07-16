{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node
  {
    state :: s,
    lastMove :: Maybe a,
    parent :: Maybe (Node s a),
    depth :: Int,
    cost :: Float,
    children :: [Node s a]
  }

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    (Node s1 _ _ _ _ _) == (Node s2 _ _ _ _ _) = s1 == s2

instance Ord s => Ord (Node s a) where
    (Node s1 _ _ _ _ _) <= (Node s2 _ _ _ _ _) = s1 <= s2



{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node s _ _ _ _ _) = s

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ p _ _ _) = p

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ d _ _) = d

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ _ c) = c

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Node _ _ _ _ c _) = c

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ l _ _ _ _) = l

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}
createStateSpaceHelper :: (ProblemState s a, Eq s) => s -> Maybe a -> Maybe (Node s a) -> Int -> Float -> Node s a
createStateSpaceHelper st lmove p d c =
   let currentNode = Node st lmove p d c copiii
       suc = successors st
       copiii = if isGoal st then []
         else map (\x -> createStateSpaceHelper (snd x)
         (Just (fst x)) (Just currentNode) (d + 1) (h (snd x))) suc in
       currentNode

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createStateSpaceHelper initialState Nothing Nothing 0 (h initialState) -- initialNode

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited =
  let suc = successors (nodeState node) in
    filter (\(Node st _ _ _ _ _) -> if elem st (map (snd) $ filter (\(_, s) -> S.notMember s visited) suc) then True else False) (nodeChildren node)



{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.

    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}
compar :: Ord p => p -> p -> p
compar c d = if (c > d) then d else c
insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith (compar) node ((nodeHeuristic node ) + fromIntegral (nodeDepth node)) frontier -- newFrontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl (insertSucc) frontier (suitableSuccs node visited) --newFrontier

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier =
  let currentNode =  fst (deleteFindMin frontier)
      newVisited = (S.insert (nodeState currentNode) visited)
      newFrontier = snd (deleteFindMin frontier) in
      if isGoal (nodeState currentNode) then currentNode else
        astar' newVisited ( insertSuccs currentNode newFrontier newVisited)

{-
    *** TODO ***

    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty $ PQ.singleton (initialNode) (nodeHeuristic initialNode)
{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

getParent :: Node s a  -> Node s a
getParent node = if isNothing $ nodeParent node then node
  else fromJust $ nodeParent node

extractPath :: Node s a -> [(a, s)]
extractPath goalNode =
    let parents = takeWhile (\node -> nodeDepth node > 0) $ iterate getParent goalNode in
    reverse $ map (\node -> (fromJust (nodeAction node), nodeState node)) parents
