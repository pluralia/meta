{-# LANGUAGE PatternGuards #-}

module Super (
  main
) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Function  (on)

-- Data ---------------------------------------------------------------------------------------------
type Name = String

data Expr = Var Name
       | Ctor Name [Expr]
       | FCall Name [Expr]
       | GCall Name [Expr]
       | Let Name Expr Expr
  deriving (Eq, Show)

data Pat = Pat Name [Name]
  deriving (Eq, Show)

data Def = FDef Name [Name] Expr
         | GDef Name Pat [Name] Expr
  deriving (Eq, Show)

type Prog = [Def]

data Task = Task [Def] Expr
  deriving (Eq, Show)

type Renaming = [(Name, Name)]
type Subst = [(Name, Expr)]
type Env = [(Name, Expr)]

-- aux ----------------------------------------------------------------------------------------------
nameSupply :: [Name]
nameSupply = ["v" ++ (show i) | i <- [0..]]

subst :: Subst -> Expr -> Expr
subst sub (Var x)                = maybe (Var x) id (lookup x sub)
subst sub (Ctor name args)       = Ctor name $ (subst sub) <$> args
subst sub (FCall name args)      = FCall name $ (subst sub) <$> args
subst sub (GCall name args)      = GCall name $ (subst sub) <$> args
subst sub (Let name expr1 expr2) = Let name (subst sub expr1) (subst sub expr2)

getFDef :: Prog -> Name -> Def
getFDef prog name = go prog
  where
    go :: Prog -> Def
    go []                            = error "No fdef"
    go (def@(FDef currName _ _) : _) | currName == name = def
    go (_ : xs)                      = go xs

getGDefs :: Prog -> Name -> [Def]
getGDefs prog name = go prog
  where
    go :: Prog -> [Def]
    go []                               = []
    go (def@(GDef currName _ _ _) : xs) | currName == name = def : go xs
    go (_ : xs)                         = go xs

getGDef :: Prog -> Name -> Name -> Def
getGDef prog name patName = go (getGDefs prog name)
  where
    go :: [Def] -> Def
    go (def@(GDef _ (Pat currPatName _) _ _) : _) | currPatName == patName = def
    go (_ : xs) = go xs

unusedNames :: [Name] -> [Name] -> [Name]
unusedNames = (\\)

usedNames :: Expr -> [Name]
usedNames = nub . go
  where
    go :: Expr -> [Name]
    go (Var x)        = [x]
    go (Ctor _ args)  = go `concatMap` args
    go (FCall _ args) = go `concatMap` args
    go (GCall _ args) = go `concatMap` args
    go (Let _ e1 e2)  = go e1 ++ go e2

freeVars :: Expr -> [Name]
freeVars = nub . go
  where
    go :: Expr -> [Name]
    go (Var x)        = [x]
    go (Ctor _ args)  = go `concatMap` args
    go (FCall _ args) = go `concatMap` args
    go (GCall _ args) = go `concatMap` args
    go (Let x e1 e2)  = go e1 ++ delete x (go e2)

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _       = False

isCall :: Expr -> Bool
isCall (FCall _ _) = True
isCall (GCall _ _) = True
isCall _           = False

renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = sequence (go e1 e2) >>= \ps -> final (gs fst ps) (gs snd ps)
  where
    go :: Expr -> Expr -> [Maybe (Name, Name)]
    go (Var x)             (Var y)             = [Just (x, y)]
    go (Ctor name1 args1)  (Ctor name2 args2)
      | name1 == name2 = concat . map (uncurry go) $ zip args1 args2
    go (FCall name1 args1) (FCall name2 args2)
      | name1 == name2 = concat . map (uncurry go) $ zip args1 args2
    go (GCall name1 args1) (GCall name2 args2)
      | name1 == name2 = concat . map (uncurry go) $ zip args1 args2
    go (Let x e1 e2)       (Let x' e1' e2')    = go e1 e1' ++ go e2 ([(x, Var x')] `subst` e2')
    go _                   _                   = [Nothing]
    
    gs :: ((Name, Name) -> Name) -> [(Name, Name)] -> [[(Name, Name)]]
    gs f = groupBy (\x y -> f x == f y) . sortBy (\x y -> compare (fst x) (fst y)) . nub
    
    final xs ys
      | all ((== 1) . length) xs && all ((== 1) . length) ys = Just $ concat xs
      | otherwise = Nothing

-- interpreter -------------------------------------------------------------------------------------
interpret :: Task -> Expr
interpret (Task prog expr) = go expr
  where
    go :: Expr -> Expr
    go (Var x)           = Var x
    go (Ctor name args)  = Ctor name $ go <$> args
    go (FCall name args)
      | (FDef _ head body) <- getFDef prog name =
        go $ zip head args `subst` body
    go (GCall name (Ctor ctorName ctorArgs : args))
      | (GDef _ (Pat _ patArgs) head body) <- getGDef prog name ctorName =
        go $ zip (patArgs ++ head) (ctorArgs ++ args) `subst` body
    go (GCall name (x : xs)) = go (GCall name $ go x : xs)
    go (Let name e1 e2)  = go $ [(name, e1)] `subst` e2
    go _                 = error "Fail interpretation"

-- configuration graph -----------------------------------------------------------------------------
data Step a = Transient a
            | Stop
            | Decompose [a]
            | Variants [((Name, Pat), a)]
            | Fold Renaming a
  deriving (Eq, Show)

instance Functor Step where
    fmap f (Transient a)     = Transient $ f a
    fmap _ Stop          = Stop
    fmap f (Decompose l) = Decompose $ fmap f l
    fmap f (Variants l)  = Variants $ fmap (fmap f) l
    fmap f (Fold r a)    = Fold r $ f a

data Graph a = Node a (Step (Graph a))

-- driving -----------------------------------------------------------------------------------------
driveMachine :: Task -> [Name] -> Step Expr
driveMachine (Task prog expr) = go expr
  where
    go :: Expr -> [Name] -> Step Expr
    go (Var _)           _ = Stop
    go (Ctor _ [])       _ = Stop
    go (Ctor _ args)     _ = Decompose args
    go (Let _ e1 e2)     _ = Decompose [e1, e2]
    go (FCall name args) _
      | (FDef _ head body) <- getFDef prog name =
        Transient $ zip head args `subst` body
    go (GCall name (Ctor ctorName ctorArgs : args)) _
      | (GDef _ (Pat _ patArgs) head body) <- getGDef prog name ctorName =
        Transient $ zip (patArgs ++ head) (ctorArgs ++ args) `subst` body
    
    go (GCall name (Var x : args)) ns = Variants $ getVariant ns x args <$> getGDefs prog name
    go (GCall name (arg : args))   ns = GCall name . (: args) <$> go arg ns
    go _                           _  = error "fail driving!"

    getVariant :: [Name] -> Name -> [Expr] -> Def -> ((Name, Pat), Expr)
    getVariant ns x args (GDef _ (Pat patName patArgs) head body) =
      let fresh = take (length patArgs) ns
       in inject x (Pat patName fresh) (zip (patArgs ++ head) (fmap Var fresh ++ args) `subst` body)

    inject :: Name -> Pat -> Expr -> ((Name, Pat), Expr)
    inject x pat@(Pat patName patArgs) expr =
      ((x, pat), [(x, Ctor patName (Var <$> patArgs))] `subst` expr)

buildTree :: Task -> Graph Expr
buildTree (Task prog expr) = go expr nameSupply
  where
    go :: Expr -> [Name] -> Graph Expr
    go expr ns = Node expr $
      case driveMachine (Task prog expr) ns of
        Stop         -> Stop
        Decompose es -> Decompose $ fmap (flip go ns) es
        Transient e  -> Transient $ go e ns
        Variants vs -> Variants $ fmap (\(p@(n, Pat _ ns'), x) -> (p, go x $ ns `unusedNames` ns')) vs

-- folding ------------------------------------------------------------------------------------------
foldTree :: Graph Expr -> Graph Expr
foldTree = fixT (tieKnot [])
  where
    fixT :: (Graph Expr -> Graph Expr -> Graph Expr) -> Graph Expr -> Graph Expr
    fixT f (Node expr next)
      | Stop         <- next = Node expr Stop
      | Decompose es <- next = let next' = Node expr $ Decompose $ fmap (f next') es in next'
      | Variants vs  <- next = let next' = Node expr $ Variants $ fmap (fmap $ f next') vs in next'
      | Transient e  <- next = let next' = Node expr $ Transient $ f next' e in next'
      | Fold r n     <- next = Node expr $ Fold r n

    tieKnot :: [Graph Expr] -> Graph Expr -> Graph Expr -> Graph Expr
    tieKnot acc prev curr@(Node expr _)
      | isCall expr, Just (node, rename) <- getRenaming expr full = Node expr (Fold rename node)
      | otherwise                                                 = fixT (tieKnot full) curr
      where
        full = prev : acc
        getRenaming :: Expr -> [Graph Expr] -> Maybe (Graph Expr, Renaming)
        getRenaming expr =
          msum . fmap (\x@(Node e _) -> sequence (x, renaming e expr))

-- generator ----------------------------------------------------------------------------------------
generate :: Graph Expr -> Task
generate = fst . go nameSupply []
  where
    go :: [Name] -> [(Expr, Expr)] -> Graph Expr -> (Task, [Name])
    go ns _ (Node e Stop)                  = (Task [] e, ns)
    go ns acc (Node (Ctor n _) (Decompose l)) = (Task defs $ Ctor n es, ns')
      where
        (defs, es, ns') = evalArgs ns acc l
    go ns acc (Node (Let n _ _) (Decompose l)) = (Task defs $ subst [(n, e)] b, ns')
      where
        (defs, [e, b], ns') = evalArgs ns acc l
    go ((_ : num) : ns) acc (Node e (Transient n)) = (Task (fDef : defs) fCall, ns')
      where
        free  = freeVars e
        fName = 'f' : num

        fCall = FCall fName $ fmap Var free

        (defs, [e'], ns') = evalArgs ns ((e, fCall) : acc) [n]
        fDef              = FDef fName free e'
    go ((_ : num) : ns) acc (Node e (Variants l)) = (Task (gDefs ++ defs) gCall, ns')
      where
        free@(v : vs) = freeVars e
        gName         = 'g' : num

        gCall = GCall gName $ fmap Var (v : free)

        (defs, es, ns') = evalArgs ns ((e, gCall) : acc) $ fmap snd l
        gDefs           = zipWith (\p e' -> GDef gName p free e') (fmap (snd . fst) l) es
    go ns acc (Node e (Fold r (Node e' _))) = (Task [] thisCall, ns)
      where
        Just recCall = e' `lookup` acc
        thisCall     = subst (fmap (fmap Var) r) recCall
    go _ _ _ = error "Can't generate code for node."


    evalArgs :: [Name] -> [(Expr, Expr)] -> [Graph Expr] -> ([Def], [Expr], [Name])
    evalArgs ns acc = foldl' accum ([], [], ns)
      where
        accum :: ([Def], [Expr], [Name]) -> Graph Expr -> ([Def], [Expr], [Name])
        accum (defs, es, ns') node = (defs ++ defs', es ++ [e], ns'')
          where
            (Task defs' e, ns'') = go ns' acc node


-- Tests --------------------------------------------------------------------------------------------
addZ :: Def
addZ = GDef "add" (Pat "Z" []) ["y"] $ Var "y"

addS :: Def
addS = GDef "add" (Pat "S" ["x"]) ["y"] $ Ctor "S" [GCall "add" [Var "x", Var "y"]]

multZ :: Def
multZ = GDef "mult" (Pat "Z" []) ["y"] $ Ctor "Z" []

multS :: Def
multS = GDef "mult" (Pat "S" ["x"]) ["y"] $ GCall "add" [Var "y", GCall "mult" [Var "x", Var "y"]]

sqr :: Def
sqr = FDef "sqr" ["x"] $ GCall "mult" [Var "x", Var "x"]

evenZ :: Def
evenZ = GDef "even" (Pat "Z" []) [] $ Ctor "True" []

evenS :: Def
evenS = GDef "even" (Pat "S" ["x"]) [] $ GCall "odd" [Var "x"]

oddZ :: Def
oddZ = GDef "odd" (Pat "Z" []) [] $ Ctor "False" []

oddS :: Def
oddS = GDef "odd" (Pat "S" ["x"]) [] $ GCall "even" [Var "x"]

prog :: [Def]
prog = [addZ, addS, multZ, multS, sqr, evenZ, evenS, oddZ, oddS]

main :: IO ()
main = print . generate . foldTree . buildTree $ Task prog (GCall "add" [Var "Z"])
