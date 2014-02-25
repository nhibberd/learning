import Data.List


data ListZipper a =
  ListZipper [a] a [a]
  deriving Eq

data MaybeListZipper a =
  IsListZipper (ListZipper a)
  | IsNotListZipper
  deriving Eq

instance Functor ListZipper where
  fmap f (ListZipper l x r) = ListZipper (fmap f l) (f x) (fmap f r)

instance Functor MaybeListZipper where
  fmap f (IsListZipper z) =
    IsListZipper (fmap f z)
  fmap _ IsNotListZipper =
    IsNotListZipper

fromList ::
  [a]
  -> MaybeListZipper a
fromList [] = 
  IsNotListZipper
fromList (h:t) = 
  IsListZipper (ListZipper [] h t)

toMaybe ::
  MaybeListZipper a
  -> Maybe (ListZipper a)
toMaybe IsNotListZipper =
  Nothing
toMaybe (IsListZipper z) =
  Just z

class Functor f => ListZipper' f where
  toMaybeListZipper ::
    f a
    -> MaybeListZipper a
  fromListZipper ::
    ListZipper a
    -> f a

instance ListZipper' ListZipper where
  toMaybeListZipper =
    IsListZipper
  fromListZipper =
    id

instance ListZipper' MaybeListZipper where
  toMaybeListZipper =
    id
  fromListZipper =
    IsListZipper
    
toList ::
  ListZipper' f =>
  f a
  -> [a]
toList z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper l x r) -> reverse l ++ x:r
    IsNotListZipper -> []

withFocus ::
  ListZipper' f =>
  (a -> a)
  -> f a
  -> f a
withFocus f z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper l x r) -> fromListZipper (ListZipper l (f x) r)
    IsNotListZipper -> z
 
setFocus ::
  ListZipper' f =>
  a
  -> f a
  -> f a
setFocus =
  withFocus . const

(.=) ::
  ListZipper' f =>
  f a
  -> a
  -> f a
(.=) =
  flip setFocus

-- !! non-total
len ::
  ListZipper a
  -> Int
len (ListZipper l _ r) =
  length l + 1 + length r

hasLeft ::
  ListZipper' f =>
  f a
  -> Bool
hasLeft z = 
  case toMaybeListZipper z of
    IsListZipper (ListZipper l _ _) -> not (null l)
    IsNotListZipper -> False

hasRight ::
  ListZipper' f =>
  f a
  -> Bool
hasRight z = 
  case toMaybeListZipper z of
    IsListZipper (ListZipper _ _ r) -> not (null r)
    IsNotListZipper -> False

findLeft ::
  ListZipper' f =>
  (a -> Bool)
  -> f a
  -> MaybeListZipper a
findLeft p z = 
  case moveLeft z of
    z'@(IsListZipper (ListZipper _ x _)) -> if p x then z' else findLeft p z'
    IsNotListZipper -> IsNotListZipper

findRight ::
  ListZipper' f =>
  (a -> Bool)
  -> f a
  -> MaybeListZipper a
findRight p z = 
  case moveRight z of
    z'@(IsListZipper (ListZipper _ x _)) -> if p x then z' else findRight p z'
    IsNotListZipper -> IsNotListZipper

moveRightLoop ::
  ListZipper' f =>
  f a
  -> f a
moveRightLoop z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper l x []) -> let (x':l') = reverse (x:l) 
                                        in fromListZipper (ListZipper [] x' l')
    IsListZipper (ListZipper l x (h:t)) -> fromListZipper (ListZipper (x:l) h t)
    IsNotListZipper -> z

-- !! non-total
moveLeftLoop ::
  ListZipper' f =>
  f a
  -> f a
moveLeftLoop z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper [] x r) -> let (x':r') = (reverse (x:r)) 
                                        in fromListZipper (ListZipper r' x' [])
    IsListZipper (ListZipper (h:t) x r) -> fromListZipper (ListZipper t h (x:r))
    IsNotListZipper -> z

moveRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
moveRight z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper _ _ []) -> IsNotListZipper
    IsListZipper (ListZipper l x (h:t)) -> IsListZipper (ListZipper (x:l) h t)
    IsNotListZipper -> IsNotListZipper

moveLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
moveLeft z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper [] _ _) -> IsNotListZipper
    IsListZipper (ListZipper (h:t) x r) -> IsListZipper (ListZipper t h (x:r))
    IsNotListZipper -> IsNotListZipper

-- nth

nth ::
  ListZipper' f =>
  Int
  -> f a
  -> MaybeListZipper a
nth i z =      
  case toMaybeListZipper z of 
    g@(IsListZipper z') -> case moveLeftN' i z' of
                             Left a -> moveRightN (i-a) z
                             Right (ListZipper l _ _) -> moveLeftN (length l) g
    z'@IsNotListZipper -> z'

moveLeftN' ::
  ListZipper' f =>
  Int
  -> f a
  -> Either Int (f a)
moveLeftN' n z =
  let moveLeftN'' n' z' q =
        if n' == 0
          then 
            Right z'
          else
            if n' < 0
              then
                moveRightN' (negate n') z
              else
                case moveLeft z' of
                  IsListZipper zz -> moveLeftN'' (n' - 1) (fromListZipper zz) (q + 1)
                  IsNotListZipper -> Left q
  in moveLeftN'' n z 0

moveRightN' ::
  ListZipper' f =>
  Int
  -> f a
  -> Either Int (f a)
moveRightN' n z =
  let moveRightN'' n' z' q =
        if n' == 0
          then 
            Right z'
          else
            if n' < 0
              then
                moveLeftN' (negate n') z
              else
                case moveRight z' of
                  IsListZipper zz -> moveRightN'' (n' - 1) (fromListZipper zz) (q + 1)
                  IsNotListZipper -> Left q
  in moveRightN'' n z 0
   
instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) = 
    (show . reverse $ l) ++ ('?':show x ++ "?") ++ show r

instance Show a => Show (MaybeListZipper a) where
  show (IsListZipper z) = show z
  show IsNotListZipper = "Ø"

swapRight :: 
  ListZipper' f =>
  f a
  -> MaybeListZipper a
swapRight z =
  case toMaybeListZipper z of 
    IsListZipper (ListZipper _ _ []) -> IsNotListZipper
    IsListZipper (ListZipper l x (h:t)) -> IsListZipper (ListZipper l h (x:t))
    IsNotListZipper -> IsNotListZipper

swapLeft :: 
  ListZipper' f =>
  f a
  -> MaybeListZipper a
swapLeft z =
  case toMaybeListZipper z of 
    IsListZipper (ListZipper [] _ _) -> IsNotListZipper
    IsListZipper (ListZipper (h:t) x r) -> IsListZipper (ListZipper (x:t) h r)
    IsNotListZipper -> IsNotListZipper

dropRights ::
  ListZipper' f =>
  f a
  -> f a
dropRights z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper l x _) -> fromListZipper (ListZipper l x [])
    IsNotListZipper -> z

dropLefts ::
  ListZipper' f =>
  f a
  -> f a
dropLefts z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper _ x r) -> fromListZipper (ListZipper [] x r)
    IsNotListZipper -> z

moveLeftN ::
  ListZipper' f =>
  Int
  -> f a 
  -> MaybeListZipper a
moveLeftN p z =
  if p == 0
    then
      toMaybeListZipper z
    else
      if p < 0
        then
          moveRightN (negate p) z
        else
          moveLeftN (p-1) (moveLeft z)

moveRightN ::
  ListZipper' f =>
  Int
  -> f a 
  -> MaybeListZipper a
moveRightN p z =
  if p == 0
    then
      toMaybeListZipper z
    else
      if p < 0
        then
          moveRightN (negate p) z
        else
          moveRightN (p-1) (moveRight z)

mfocus ::
  ListZipper' f =>
  f a
  -> Maybe a
mfocus z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper _ x _) -> Just x
    IsNotListZipper -> Nothing

index ::
  ListZipper' f =>
  f a
  -> Maybe Int
index z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper l _ _) -> Just (length l)
    IsNotListZipper -> Nothing

-- non-total
end ::
  ListZipper' f =>
  f a
  -> f a
end z =
  case toMaybeListZipper z of 
    IsListZipper (ListZipper l x r) -> let (x':r') = reverse (x:r)
                                       in fromListZipper (ListZipper (r' ++ l) x' [])
    IsNotListZipper -> z   

start ::
  ListZipper' f =>
  f a
  -> f a
start z =
  case toMaybeListZipper z of 
    IsListZipper (ListZipper l x r) -> let (x':r') = reverse (x:l)
                                       in fromListZipper (ListZipper [] x' (r' ++ r))
    IsNotListZipper -> z                                       

deletePullRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
deletePullRight z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper _ _ []) -> IsNotListZipper
    IsListZipper (ListZipper l _ (h:t)) -> IsListZipper (ListZipper l h t)
    IsNotListZipper -> IsNotListZipper

deletePullLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
deletePullLeft z =
  case toMaybeListZipper z of
    IsListZipper (ListZipper [] _ _) -> IsNotListZipper
    IsListZipper (ListZipper (h:t) _ r) -> IsListZipper (ListZipper t h r)
    IsNotListZipper -> IsNotListZipper

insertPushLeft :: 
  ListZipper' f =>
  a
  -> f a
  -> f a
insertPushLeft a z =
  case toMaybeListZipper z of    
    IsListZipper (ListZipper l x r) -> fromListZipper (ListZipper (x:l) a r)
    IsNotListZipper -> z

insertPushRight :: 
  ListZipper' f =>
  a
  -> f a
  -> f a
insertPushRight a z =
  case toMaybeListZipper z of    
    IsListZipper (ListZipper l x r) -> fromListZipper (ListZipper l a (x:r))
    IsNotListZipper -> z

class Functor f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

class Apply f => Applicative f where
  unit ::
    a -> f a

class Functor f => Extend f where
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b
  f <<= x =
    fmap f (duplicate x)
  duplicate ::
    f a
    -> f (f a)
 
class Extend f => Comonad f where
  counit ::
    f a
    -> a
    
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable Maybe where
  traverse _ Nothing =
    unit Nothing
  traverse f (Just a) =
    fmap Just (f a)

instance Traversable [] where
  traverse f =
    foldr (\a b -> fmap (:) (f a) <*> b) (unit [])

instance Apply ListZipper where
  ListZipper fl fx fr <*> ListZipper al ax ar = ListZipper (zipWith ($) fl al) (fx ax) (zipWith ($) fr ar)

instance Apply MaybeListZipper where
  IsNotListZipper <*> _ = IsNotListZipper
  _ <*> IsNotListZipper = IsNotListZipper
  IsListZipper f <*> IsListZipper a = IsListZipper (f <*> a)

instance Applicative ListZipper where
  unit a =
    ListZipper (repeat a) a (repeat a)

instance Applicative MaybeListZipper where
  unit = IsListZipper . unit

instance Extend ListZipper where
  duplicate z =
    ListZipper (unfoldr (fmap (\z' -> (z', z')) . toMaybe . moveLeft) z) z (unfoldr (fmap (\z' -> (z', z')) . toMaybe . moveRight) z)

instance Comonad ListZipper where
  counit (ListZipper _ x _) = x

instance Traversable ListZipper where
  traverse f (ListZipper l x r) =
    fmap (ListZipper . reverse) (traverse f $ reverse l) <*> f x <*> traverse f r

instance Traversable MaybeListZipper where
  traverse _ IsNotListZipper =
    unit IsNotListZipper
  traverse f (IsListZipper z) =
    fmap IsListZipper (traverse f z)