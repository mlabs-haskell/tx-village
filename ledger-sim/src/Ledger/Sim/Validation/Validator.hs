module Ledger.Sim.Validation.Validator (
    Validity (Valid, Invalid),
    Validator (Validator),
    runValidator,
    validatePass,
    validateFail,
    validateIf,
    validateBool,
    validateWith,
    validateFoldable,
    validateListAndAnnotateErrWithIdx,
    validateRoundtrip,
    validateOptional,
    mapErr,
    mapErrWithSubject,
    contramapAndMapErr,
    contramapAndMapErrWithSubject,
    itemsInContext,
    InContext (InContext, getContext, getSubject),
    traverseFirst,
    sequenceFirst,
) where

import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Function (on)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (
    Decidable (choose, lose),
    Divisible (conquer, divide),
 )
import Data.List qualified as L
import Data.Void (absurd)

data Validity err = Valid | Invalid [err]
    deriving stock (Eq, Functor)

instance Semigroup (Validity err) where
    Valid <> Valid = Valid
    (Invalid l) <> Valid = Invalid l
    Valid <> (Invalid r) = Invalid r
    (Invalid l) <> (Invalid r) = Invalid $ l <> r

instance Monoid (Validity err) where
    mempty = Valid

newtype Validator err a = Validator (a -> Validity err)

instance Contravariant (Validator err) where
    contramap f (Validator vf) = Validator $ vf . f

instance Divisible (Validator err) where
    divide f l r = Validator $ \x ->
        let (l', r') = f x
         in runValidator l l' <> runValidator r r'
    conquer = validatePass

instance Decidable (Validator err) where
    lose f = Validator $ absurd . f
    choose f l r = Validator $ \x -> case f x of
        Left l' -> runValidator l l'
        Right r' -> runValidator r r'

instance Semigroup (Validator err a) where
    l <> r = Validator $ (liftA2 (<>) `on` runValidator) l r

instance Monoid (Validator err a) where
    mempty = validatePass

runValidator :: Validator err a -> a -> Validity err
runValidator (Validator f) = f

validatePass :: Validator err a
validatePass = Validator $ const Valid

validateFail :: err -> Validator err a
validateFail = Validator . const . Invalid . L.singleton

validateIf :: (a -> Bool) -> (a -> err) -> Validator err a
validateIf predicate mkErr = Validator $ \x ->
    if predicate x
        then Valid
        else Invalid [mkErr x]

validateBool :: err -> Validator err Bool
validateBool = validateIf id . const

validateWith :: (a -> Validator err a) -> Validator err a
validateWith f = Validator $ \x -> runValidator (f x) x

validateFoldable :: (Foldable f) => Validator err a -> Validator err (f a)
validateFoldable = Validator . foldMap . runValidator

validateListAndAnnotateErrWithIdx ::
    (Int -> err1 -> err2) ->
    Validator err1 a ->
    Validator err2 [a]
validateListAndAnnotateErrWithIdx mkErr =
    contramap (zip [0 ..])
        . validateFoldable
        . mapErrWithSubject (mkErr . fst)
        . contramap snd

validateRoundtrip ::
    (Eq a) =>
    (a -> a) ->
    (a -> a -> err) ->
    Validator err a
validateRoundtrip roundtrip mkErr = validateWith $ \x ->
    let x' = roundtrip x
     in validateIf (== x') (mkErr x')

validateOptional :: Validator err a -> Validator err (Maybe a)
validateOptional =
    choose
        ( \case
            Nothing -> Left ()
            Just x -> Right x
        )
        validatePass

mapErr ::
    (err1 -> err2) ->
    Validator err1 a ->
    Validator err2 a
mapErr = mapErrWithSubject . const

mapErrWithSubject ::
    (a -> err1 -> err2) ->
    Validator err1 a ->
    Validator err2 a
mapErrWithSubject f v = validateWith $ \x -> mapErr (f x) v

contramapAndMapErr ::
    (b -> a) ->
    (errA -> errB) ->
    Validator errA a ->
    Validator errB b
contramapAndMapErr f = contramapAndMapErrWithSubject f . const

contramapAndMapErrWithSubject ::
    (b -> a) ->
    (b -> errA -> errB) ->
    Validator errA a ->
    Validator errB b
contramapAndMapErrWithSubject f g = mapErrWithSubject g . contramap f

data InContext a ctx = InContext
    { getSubject :: a
    , getContext :: ctx
    }
    deriving stock (Functor)

instance Bifunctor InContext where
    first f c = InContext (f $ getSubject c) (getContext c)
    second f c = InContext (getSubject c) (f $ getContext c)

instance Bifoldable InContext where
    bifoldMap f g =
        liftA2 (<>) (f . getSubject) (g . getContext)

instance Bitraversable InContext where
    bitraverse f g =
        liftA2 (liftA2 InContext) (f . getSubject) (g . getContext)

traverseFirst ::
    (Applicative f, Bitraversable t) =>
    (l -> f l') ->
    t l c ->
    f (t l' c)
traverseFirst f = bitraverse f pure

sequenceFirst ::
    (Applicative f, Bitraversable t) =>
    t (f l) r ->
    f (t l r)
sequenceFirst = bitraverse id pure

itemsInContext :: InContext [a] ctx -> [InContext a ctx]
itemsInContext = sequenceFirst
