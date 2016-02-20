-- "Data/Lens/Minimal.hs"  An minimalistic lens library.
-- 
-- Copyright (C) 2008-2016  Ramin Honary.
--
-- This library is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.

-- | The 'Lens' type aims to provide a very simple improvement on Haskell's record syntax: the idea of
-- /composable/ record accessors called 'Lens'es.
--
-- 'Lens'es also bring one of the strengths (or weaknesses, depending on your point of view) of
-- Object-Oriented programming to Haskell: the idea of making setter and getter functions to control
-- access to elements in data structures. A 'Lens' combines both getters and setters into a single
-- composable data type which can be invoked using simple functions like 'fetch' or 'alter', or
-- using infix operators such as @'Data.Lens.Minimal.~>'@, @'Data.Lens.Minimal.<~'@, and
-- @'Data.Lens.Minimal.$='@.
--
-- Note that it is necessary to always include the following import statements in order to make use
-- of Minimal Lenses:
--
-- @
-- import Prelude hiding ((.), id)
-- import Control.Category
-- @
--
-- With Haskell record syntax you can 'fetch' and 'alter' a data type like so:
-- 
-- @
-- let previousValue = record1 dat in
--     dat{ record1 = newValue1, record2 = newValue2 }
-- @
--
-- With Minimal 'Lens'es, it is possible to achieve the same thing with the following expression:
--
-- @
-- let previousValue = dat 'Data.Lens.Minimal.Lens.~>' recordName in
--     'with' dat [ record1 'Data.Lens.Minimal.Lens.<~' newValue1, record2 'Data.Lens.Minimal.Lens.<~' newValue2 ]
-- @
--
-- Minimal 'Lens'es can be composed with the 'Control.Category.Category' operators @(.)@,
-- @('Control.Category.<<<')@ and @('Control.Category.>>>')@ so something like this:
--
-- @
-- return $ dat{ record = (\\dat' -> dat'{ subRecord = newValue }) (record dat) }
-- @
--
-- can be simplified to this:
--
-- @
-- return $ 'with' dat [ record 'Control.Category.>>>' subRecord 'Data.Lens.Minimal.Lens.<~' newValue ]
-- @
--
-- or equivalently with the dot operator, which is identical to @('Control.Category.>>>')@ with the
-- arguments flipped:
--
-- @
-- return $ 'with' dat [ subRecord . record 'Data.Lens.Minimal.Lens.<~' newValue ]
-- @
--
-- Fetching values is done with @('Data.Lens.Minimal.Lens.~>')@, which is a left-handed infix operator of
-- precedence 9 so that you can compose 'Lens'es for fetching. The above example with @record@ and
-- @subRecord@ could be fetched like so:
--
-- @
-- return (dat~>record~>subRecord)  -- uses tilde-greater-than, not hyphen-greater-than.
-- @
--
-- This is reminiscient of popular languages like C/C++ in which you could write the above with a
-- similar expression:
--
-- > return (dat->record->subRecord);
module Data.Lens.Minimal
  ( -- * The Lens operators
    (~>), (<~), ($=), ($$=), (<$~>),
    fetch', fetch, alter', alter, lensGet, lensModify, lensPut,
    -- * Defining a sequence of updates
    with, with', by, by',
    -- * The Lens data type
    Lens(..), PureLens,
    newLens, newLens', isoLens, liftLens,
    -- * Lens combinators
    new, new', defaul', defaul, ifJust, orElse', orElse, just, notEmpty, notMEmpty, exists,
    leftLens, rightLens,
    -- * Lenses for @containers@
    mapLens, intMapLens, intMapListLens, mapListLens, dictionaryLens,
    arrayLens, maybeArrayLens, ioArrayLens, ioMaybeArrayLens,
    ioRefLens, mvarLens,
    -- * Lenses for tuples
    -- $TupleLenses
    TupleLens0(..), TupleLens1(..), TupleLens2(..), TupleLens3(..), TupleLens4(..),
    TupleLens5(..), TupleLens6(..), TupleLens7(..), TupleLens8(..), TupleLens9(..),
    -- * Isomorphisms
    Iso(..), PureIso, newIso', newIso, inverse, un, make, un', make', unK, makeK, isoMonad,
    isoMonadTrans, (*~*), (+~+), isoFirst, isoSecond, isoMap, isoFMap, twoWayFunctor,
    -- * Two-Way functions
    TwoWay(..), PureTwoWay, new2way, new2way', isoTo2way,
    -- * Operations shared by 'Iso's and 'TwoWay's
    TwoWayClass(..),
    -- * 'Iso's and 'TwoWay' combinators
    -- $IsoCombinators
    swapped, dyslexic, zipped, negated, charred, parsed, rounded, floored, ceilinged, added,
    multiplied, worded, lined,
    LazyText, StrictText, text, lazyText, strictifyText, lazyLined, lazyWorded,
    strictLined, strictWorded, LazyByteString, StrictByteString, byteString, lazyByteString,
    utf8, utf16BE, utf16LE, utf32BE, utf32LE,
    lazyUtf8, lazyUtf16BE, lazyUtf16LE, lazyUtf32BE, lazyUtf32LE,
    -- * Isomorphisms over 'Data.Monoid.Monoid', 'Data.Semigroup.Semigroup' and 'Control.Applicative.Applicative' newtypes
    identity, kleisli, monoidEndo, endoIdentity, monoidDual, monoidAll, monoidAny, monoidSum,
    monoidProduct, monoidFirst, monoidLast, semiFirst, semiLast, semiOption, semiMin, semiMax,
    apConst, apWrapMonad, apWrapArrow, apZipList,
  )
  where

import           Prelude hiding ((.), id)

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State.Lazy

import           Data.Array.IArray
import           Data.Array.IO
import           Data.Char (ord, chr)
import qualified Data.IntMap as I
import           Data.IORef
import qualified Data.Map    as M
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup

import qualified Data.Text.Lazy           as Lazy
import qualified Data.Text.Lazy.Encoding  as Lazy
import qualified Data.Text                as Strict
import qualified Data.Text.Encoding       as Strict
import qualified Data.ByteString.Lazy     as Bz
import qualified Data.ByteString          as B
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | A 'Lens' is a 'Control.Monad.State.StateT' monadic function that 'Control.Monad.Trans.lift's a
-- monad @m@, and can perform an 'alter' on an element @e@ stored within a container @c@.
--
-- The 'Lens' @newtype@ contains a pair of functions. The first function is the fetch function
-- (which is evaluated when you call 'fetch' on a 'Lens'). The second function is the alter
-- function (which is evaluated when you call 'alter' on a 'Lens').
--
-- Although you will usually construct lenses using the 'newLens' or 'newLens'' functions, lets take
-- a look at how 'Lens'es work by constructing a 'Lens' with the 'Lens' constructur, taking a simple
-- container data type as an example. The Container will be a pair elements, each element being of
-- the same type: @e@. So here is how we would define our simplified version of the 'containerFirst'
-- lens:
--
-- @
-- data Container e = Container e e
--
-- containerFirst :: 'Control.Monad.Monad' m => 'Lens' m (Container e) e
-- containerFirst = Lens (         \ (Container e0    e1) -> return e0
--                       , \updateE0 (Container oldE0 e1) -> do
--                            newE0 <- updateE0 e0
--                            return (Container newE0 c1)
--                       )
-- @
--
-- But as stated above, it is usually easier to use 'newLens' instead of the 'Lens' constructor. The
-- 'newLens' function takes a new element, rather than an updating function.
--
-- @
-- containerFirst :: 'Control.Monad.Monad' m => 'Lens' m (Container e) e
-- containerFirst = newLens (      \ (Container e0 e1) -> e0
--                          , \newE0 (Container _  e1) -> Container newE0 e1
--                          )
-- @
--
-- There is one law that the fetching and altering functions must follow. These laws cannot be
-- checked by the compiler, so it is the duty of you, the programmer using this module, to make sure
-- your code obeys these laws:
--
-- The element must be fetched from the exact same place that the alter function alters, and the
-- altering function must alter the element retrieved by the fetching function. Lets look at the
-- above 'containerFirst' example again, and change it to make it _illegal_:
-- 
-- @
-- containerFirst :: 'Control.Monad.Monad' m => 'Lens' m (Container e) e
-- containerFirst = Lens (         \\ (Container e0 e1) -> e0
--                       , \\updateE0 (Container e0 e1) -> do
--                           newE0 <- updateE0 e0
--                           return (Container e0 newE0) -- NO! This is illegal.
--                       )
-- @
-- 
-- Notice that the fetch function retrieved the first element @e0@, but the altering function
-- altered the second element @e1@ (it returned a @Container@ with the updated e0 in the spot where
-- e1 used to be), and did it not alter the old @e0@. This would cause some extraordinarily
-- confusing problems, so never do this.
newtype Lens m c e = Lens (c -> m e, (e -> m e) -> c -> m c)

-- | This is a 'Lens' where the monad must be 'Control.Monad.Identity.Identity'. The advantage of
-- using this over 'Lens' (apart from the fact that it is easier to write it's type in your code) is
-- that it guarantees access to your container must be pure, with no IO.
type PureLens c e = Lens Identity c e

instance Monad m => Category (Lens m) where
  id = Lens (return, \updateE e -> updateE e)
  (Lens (getCfromB, changeCinB)) . (Lens (getBfromA, changeBinA)) = Lens
    ( getBfromA >=> getCfromB
    , \updateC a -> do
        b <- getBfromA a
        b <- changeCinB updateC b
        changeBinA (const $ return b) a
    )

-- | Construct a 'Lens' (polymorphic over the monadic type) from a pair of functions: 'Prelude.fst'
-- being a function that fetches an element @e@ from a container @c@, and 'Prelude.snd' being a
-- function that places an element @e@ into the a container @c@.
newLens :: Monad m => (c -> e, e -> c -> c) -> Lens m c e
newLens (fetch, upd) = Lens
  ( return . fetch
  , \updateE c -> updateE (fetch c) >>= return . flip upd c
  )

-- | Like 'newLens', but take monadic fetch and alter functions.
newLens' :: Monad m => (c -> m e, e -> c -> m c) -> Lens m c e
newLens' (fetch, upd) = Lens (fetch, \updateE c -> fetch c >>= updateE >>= flip upd c)

-- | Like 'newLens' but uses an 'Iso' to construct the 'Lens'. Often times a 'Lens' simply operates
-- on an element within some container. Converting between the contained and the contained element
-- is an 'Iso'morphic operation, so a lens operating on the contained element can easily be
-- constructed from an 'Iso'morphism.
--
-- It is sometimes covenient, when designing your API, to export an 'Iso'morphism for your
-- @newtype@, rather than a constructor. This allows programmers using you API to use 'make' and
-- 'un' to wrap and unwrap the @newtype@, and to use this 'isoLens' function to access elements
-- within the @newtype@ using the lens operators.
isoLens :: Monad m => Iso m c e -> Lens m c e
isoLens iso = newLens' (un' iso, \e _ -> make' iso e)

-- | Use 'Control.Monad.Trans.Class.lift' to lift the operating monad of the 'Lens'. This is useful
-- when you want to modify the behavior of an existing 'Lens' by changing the monad in which 'alter'
-- and 'fetch' operate. One example of how this could be used is if you want 'alter' or 'fetch' to
-- be able to throw an error message, you could lift the monad into an
-- 'Control.Monad.Except.ExceptT' monad.
--
-- @
-- data Container = Container ('Prelude.Maybe' 'Prelude.Int') ('Prelude.Maybe' 'Prelude.String')
-- 
-- maybeInt :: 'Prelude.Monad' m => 'Lens' m Container ('Prelude.Maybe' 'Prelude.Int')
  -- maybeInt = 'newLens' (\\ (Container i _) -> i, \\i (Container _ s) -> Container i s)
-- 
-- maybeString :: 'Prelude.Monad' m => 'Lens' m Container ('Prelude.Maybe' 'Prelude.String')
-- maybeString = 'newLens' (\\ (Container _ s) -> s, \\s (Container i _) -> Container i s)
-- 
-- required :: 'Prelude.Monad' m => 'Prelude.String' -> 'Lens' m Container (Maybe element) -> 'Lens' ('Control.Monad.Except.ExceptT' 'Prelude.String' m) Container element
-- required fieldName lens = 'liftLens' lens 'Control.Category.>>>'
--     'defaul'' 'Prelude.True' ('Control.Monad.Except.throwError' $ fieldName++" is undefined") 'Prelude.return'
-- 
-- requireInt :: 'Prelude.Monad' m => 'Lens' ('Control.Monad.Except.ExceptT' String m) Container 'Prelude.Int'
-- requireInt = required "int" maybeInt
-- 
-- requireString :: 'Prelude.Monad' m => 'Lens' ('Control.Monad.Except.ExceptT' String m) Container 'Prelude.String'
-- requireString = required "string" maybeString
-- @
liftLens :: (Monad m, MonadTrans t, Monad (t m)) => Lens m c e -> Lens (t m) c e
liftLens (Lens (fetch, alter)) = Lens
  ( lift . fetch
  , \upd c -> lift (fetch c) >>= upd >>= \e -> lift (alter (const $ return e) c)
  )

----------------------------------------------------------------------------------------------------

-- | This is a simple isomorphism data type, it contains a function and it's inverse, and
-- instantiates 'Control.Category.Category'.  An 'Iso'-morphism is a relationship between a @whole@
-- and it's @parts@: it 'make's the @whole@ from @parts@, and can 'un'-make the @whole@ back into
-- it's @parts@, without ever losing anything in the process.
--
-- When building your own 'Iso'-morphisms, it is a law -- a law which cannot be enforced by the
-- compiler, so it is you the programmer's duty to obey this law -- that the second function _must_
-- be an inverse of the first function, i.e. the following funtion must always
-- 'Control.Monad.return' 'Prelude.True':
--
-- @
-- example :: forall m whole parts . 'Control.Monad.Monad' m => 'Iso' m whole parts -> whole -> m Bool
-- example iso whole0 = do
--     parts  <- 'un''   iso whole0
--     whole1 <- 'make'' iso parts
--     return (whole0 == whole1) -- *must* be True, by law
-- @
-- 
-- When creating an 'Iso'-morphism for @newtype@s, the convetion established by this module is that
-- the 'Prelude.fst' function in the pair is the function that unwraps/deconstructs the @newtype@, and the
-- 'Prelude.snd' function is the function that wraps/constructs the @newtype@.
--
-- This is decided by how the 'un' and 'make' functions work. For example, when using the
-- 'identity' 'Iso'-morphism, @'make' 'identity'@ is the same as using the 'Identity' constructor
-- because 'make' uses the 'Prelude.snd' function to call the 'Identity' constructor. Likewise
-- calling @'un' 'identity'@ is the same 'Control.Monad.Identity.runIdentity' because 'un' uses the
-- 'Prelude.fst' function to call 'Control.Monad.Identity.runIdentity'.
--
-- It can be difficult to remember this type definition of 'Iso', and that the first polymophic
-- parameter is @whole@, and the second polymorphic paramter is @parts@. So as a mnemonic keep in
-- mind the phrase, "the whole is greater than the sum of the parts," the order the words appear in
-- that phrase is the same as the order of polymorphic type parameters.
newtype Iso m whole parts = Iso (Kleisli m whole parts, Kleisli m parts whole)

-- | For some related data types, such as 'Prelude.Double' versus 'Prelude.Int', or 'Data.Text.Text'
-- versus 'Data.ByteString.ByteString', there may seem to be an 'Iso'-morphic relationship between
-- data types as you can convert from one to the other in both directions, however there is
-- information lost in the conversion process.
--
-- If information is lost during conversion, then the types are not 'Iso'-morphic. However it may
-- still be useful to define an two-way function that can make use of the
-- 'Control.Category.Category' API, namely function composition of two-way functions.
--
-- To prevent you from mixing up truly 'Iso'-morphic functions and functions that just want to take
-- advantage of the convenience of using two-way API functions that can convert back and forth
-- between types, this 'TwoWay' data type is provided. 'TwoWay' instantiates
-- 'Control.Category.Category', and can be mixed with truly 'Iso'-morphic functions, while making
-- the type explicitly 'TwoWay'.
--
-- This is the best way to make explicit which API functions will lose information during
-- conversion, that way people using your API will not use your function expecting that they can
-- (for example) convert from a 'Double' to an 'Int' and then back from an 'Int' to a 'Double'
-- without loosing the information after the decimal point.
--
-- You can think of a 'TwoWay' function as IKEA furniture: you can 'make' a @whole@ from it's
-- @parts@ but you are likely missing some of the @parts@, or you will end up with extra @parts@
-- that don't go into the @whole@, and if you 'un'-make the @whole@ you probably won't end up with
-- the same pile of @parts@ you started with.
newtype TwoWay m whole parts = TwoWay (Kleisli m whole parts, Kleisli m parts whole)

instance Monad m => Category (TwoWay m) where
  id = TwoWay (returnA, returnA)
  (TwoWay (fwdB, revB)) . (TwoWay (fwdA, revA)) = TwoWay (fwdA >>> fwdB, revA <<< revB)

instance Monad m => Category (Iso m) where
  id = Iso (returnA, returnA)
  (Iso (fwdB, revB)) . (Iso (fwdA, revA)) = Iso (fwdA >>> fwdB, revA <<< revB)

-- | 'Iso' with the 'Data.Functor.Identity.Identity' monad.
type PureIso whole parts = Iso Identity whole parts

-- | 'TwoWay' with the 'Data.Functor.Identity.Identity' monad
type PureTwoWay whole parts = TwoWay Identity whole parts

-- | Both 'TwoWay'-morphisms and 'Iso'-morphisms are two-way functions, and so share similar APIs
-- for working with each of these types of fuctions. This type class allows the similar API
-- functions to be used for both 'Iso'-morhpisms and ordinary 'TwoWay' functions.
--
-- The 'isoKleisli' function is an 'Iso'-morphism between the @twoWay@ function (the type
-- instantiating this class) and a pair of 'Control.Arrow.Kleisli' functions, where the first
-- element of the pair converts a value of type @whole@ to a value of type @parts@, and the second
-- element of the pair converts a value of type @parts@ to a value of type @whole@.
--
-- Note that the pair of 'Kleisli' functions contained within the @twoWay@ type need not be
-- isomorphic, but the 'isoKleisli' function converting between the 'Kleisli' function pair
-- and the @twoWay@ function type must be a genuine 'Iso'-morphism.
class TwoWayClass twoWay where
  isoKleisli :: (Monad m, Monad mm) => Iso m (twoWay mm whole parts) (Kleisli mm whole parts, Kleisli mm parts whole)

instance TwoWayClass Iso    where { isoKleisli = Iso (Kleisli $ \ (Iso    o) -> return o, Kleisli $ return . Iso   ); }
instance TwoWayClass TwoWay where { isoKleisli = Iso (Kleisli $ \ (TwoWay o) -> return o, Kleisli $ return . TwoWay); }

-- | Construct a 'TwoWay' function or 'Iso'-morphism, or any 'TwoWayClass' of function, from a pair
-- of monadic functions. It is a good idea to only use this when declaring a top-level function,
-- where you provide an explicit type signature for the 'TwoWayClass'.
new2way' :: (TwoWayClass iso, Monad m) => (whole -> m parts, parts -> m whole) -> iso m whole parts
new2way' (fwd, rev) = (\ (Iso (_, make)) -> runIdentity $ runKleisli make (Kleisli fwd, Kleisli rev)) isoKleisli

-- | Construct a 'TwoWay' function or 'Iso'-morphism, or any 'TwoWayClass' of function, from a pair
-- of pure functions. It is a good idea to only use this when declaring a top-level function, where
-- you provide an explicit type signature for the 'TwoWayClass'.
new2way :: (TwoWayClass iso, Monad m) => (whole -> parts, parts -> whole) -> iso m whole parts
new2way (fwd, rev) = new2way' (return . fwd, return . rev)

-- | Like 'new2way\'' but make it explicitly an 'Iso'-morphic function. This function is just a
-- convenience so you do not have to provide an explicit type signature.
newIso' :: Monad m => (whole -> m parts, parts -> m whole) -> Iso m whole parts
newIso' = new2way'

-- | Like 'new2way' but make it explicitly an 'Iso'-morphic function. This function is just a
-- convenience so you do not have to provide an explicit type signature.
newIso :: Monad m => (whole -> parts, parts -> whole) -> Iso m whole parts
newIso = new2way

-- | You may construct a 'TwoWay' from an 'Iso'-morphism, but must not make an 'Iso'-morphism from a
-- 'TwoWay' function. 'Iso'-morphisms should be subset of 'TwoWay' functions.
isoTo2way :: Iso m whole parts -> TwoWay m whole parts
isoTo2way (Iso o) = TwoWay o

-- | Extract the 'un' 'Control.Arrow.Kleisli' 'Control.Arrow.Arrow' from the 'Iso'-morphism.
unK :: (TwoWayClass iso, Monad m) => iso m whole parts -> Kleisli m whole parts
unK iso = (\ (Iso (un, _)) -> fst $ runIdentity $ runKleisli un iso) isoKleisli

-- | Extract the 'make' 'Control.Arrow.Kleisli' 'Control.Arrow.Arrow' from the 'Iso'-morphism.
makeK :: (TwoWayClass iso, Monad m) => iso m whole parts -> Kleisli m parts whole
makeK iso = (\ (Iso (un, _)) -> snd $ runIdentity $ runKleisli un iso) isoKleisli

-- | An 'Iso'-morphism that takes a type @whole@ and @un@-makes it, turning the @whole@ into the
-- @parts@.
un' :: (TwoWayClass iso, Monad m) => iso m whole parts -> whole -> m parts
un' = runKleisli . unK

-- | An 'Iso'-morphism that takes a type @parts@ and makes it into a type @whole@.
make' :: (TwoWayClass iso, Monad m) => iso m whole parts -> parts -> m whole
make' = runKleisli . makeK

-- | Like 'un'' but uses a 'PureIso' so you do not need to evaluate a 'Control.Monad.Monad', this
-- function automatically evaluates 'Data.Functor.Identity.runIdentity'.
un :: TwoWayClass iso => iso Identity whole parts -> whole -> parts
un = fmap runIdentity . un'

-- | Like 'make'' but uses a 'PureIso' so you do not need to evaluate a 'Control.Monad.Monad', this
-- function automatically evaluates 'Data.Functor.Identity.runIdentity'.
make :: TwoWayClass iso => iso Identity whole parts -> parts -> whole
make = fmap runIdentity . make'

-- | Transform the monadic type of the 'Iso' or 'TwoWay'.
isoMonadTrans
  :: (TwoWayClass iso, Monad oldM, Monad newM)
  => (forall c . oldM c -> newM c)
  -> iso oldM whole parts -> iso newM whole parts
isoMonadTrans mtrans iso = let (Kleisli fwd, Kleisli rev) = un isoKleisli iso in
  make isoKleisli (Kleisli $ fmap mtrans fwd, Kleisli $ fmap mtrans rev)

-- | Similar to the @('Control.Arrow.***')@ operator in the "Control.Arrow" module, except only
-- works on 'Iso'-morphisms. In short, this operator takes a left and right parameter and creates a
-- new 'Iso'-morphism that applies the left 'Iso' to the first element of a pair and the right 'Iso'
-- to the second element of the pair.
(*~*)
  :: (TwoWayClass iso, Monad m)
  => iso m whole0 parts0 -> iso m whole1 parts1 -> iso m (whole0, whole1) (parts0, parts1)
(*~*) left right = make isoKleisli (unK left *** unK right, makeK left *** makeK right)
infixr 3 *~*

-- | Similar to the @('Control.Arrow.+++')@ operator in the "Control.Arrow" module, except only
-- works with 'Iso'-morphisms.
(+~+)
  :: (TwoWayClass iso, Monad m)
  => iso m wholeLeft partsLeft
  -> iso m wholeRight partsRight
  -> iso m (Either wholeLeft wholeRight) (Either partsLeft partsRight)
(+~+) left right = make isoKleisli (unK left +++ unK right, makeK left +++ makeK right)
infixr 2 +~+

-- | An 'Iso' for turning any another 'Iso' into a pair of 'Control.Monad.Monad'-ic functions,
-- similar to 'isoKleisli' but exposes the monadic function within the 'Control.Arrow.Kleisli'
-- function.
isoMonad :: (TwoWayClass inner, Monad m, Monad mm) => Iso m (inner mm whole parts) (whole -> mm parts, parts -> mm whole)
isoMonad = isoKleisli >>> kleisli *~* kleisli

-- | Invert an isomorphism so 'make' becomes 'un' and 'un' becomes 'make'.
inverse :: (TwoWayClass iso, Monad m) => iso m whole parts -> iso m parts whole
inverse = make isoKleisli . uncurry (flip (,)) . un isoKleisli

-- | Similar to the 'Control.Arrow.first' function in the "Control.Arrow" module, except only works
-- with 'Iso'-morphisms. Take an 'Iso'-morphism and turn it into a new 'Iso'-morphism that operates
-- only on the first argument of a pair, ignoring the second argument. Also note that:
-- @'isoFirst' 'Control.Category.>>>' 'isoSecond'@ is the same as
-- @'isoSecond' 'Control.Category.>>>' 'isoFirst'@ is the same as @('*~*')@.
isoFirst :: (TwoWayClass iso, Monad m) => iso m whole parts -> iso m (whole, ignored) (parts, ignored)
isoFirst iso = make isoKleisli (first $ unK iso, first $ makeK iso)

-- | Similar to the 'Control.Arrow.second' function in the "Control.Arrow" module, except only works
-- with 'Iso'-morphisms. Take an 'Iso'-morphism and turn it into a new 'Iso'-morphism that operates
-- only on the second argument of a pair, ignoring the first argument. Also note that:
-- @'isoFirst' a 'Control.Category.>>>' 'isoSecond' b@ is the same thing as
-- @'isoSecond' b 'Control.Category.>>>' 'isoFirst' a@ is the same thing as @a '*~*' b@.
isoSecond :: (TwoWayClass iso, Monad m) => iso m whole parts -> iso m (ignored, whole) (ignored, parts)
isoSecond iso = make isoKleisli (second $ unK iso, second $ makeK iso)

-- | Create an 'Iso'-morphism that operates on the contents of a @box@ data type by taking the
-- element of type @e@ out of @box@, applying another 'Iso'-morphic transformation on the element
-- @e@, then placing the element @e@ back into the @box@, thus returning a new @box@.
isoMap :: (TwoWayClass iso, Monad m) => iso m box e -> iso m e e -> iso m box box
isoMap box f = make isoKleisli (makeK box . unK f . unK box, makeK box . makeK f . unK box)

-- | If you have an 'Iso'-morphism that can wrap and unwrap a value @v@ from any data type @F v@,
-- then you can turn that data type @F@ into a functor using the 'Iso'-morphism. Let's call @F@ a
-- pseudo-functor, because it does not necessarily need to be functor in the sense that it
-- instantiates 'Data.Functor.Functor', but becomes a functor when we have an 'Iso'-morphism that
-- could treat it as a functor.
--
-- This function Operate on a pseudo-functor by providing an 'Iso'-morphism to unbox and box the
-- contents of the functor, then apply an 'Iso'-morphic transformation on the contents of the
-- functor.
isoFMap
  :: (TwoWayClass mapIso, Monad m)
  => (forall e . Iso m (box e) e) -> mapIso m a b -> mapIso m (box a) (box b)
isoFMap box f = make isoKleisli (makeK box . unK f . unK box, makeK box . makeK f . unK box)

-- | Create a new 'Iso'-morphism by applying a given pure 'Iso'-morphism to a 'Data.Functor.Functor'.
twoWayFunctor
  :: (TwoWayClass iso, Monad m, Functor f)
  => (forall whole parts . iso Identity whole parts) -> iso m (f whole) (f parts)
twoWayFunctor iso = new2way (fmap $ un iso, fmap $ make iso)

----------------------------------------------------------------------------------------------------

-- $IsoCombinators
-- Sometimes you have an 'Iso' or 'TwoWay' function that nearly operates on the type you need, but
-- not quite. For example, you have an 'Iso'morphism that produces a pair, but you need the 'Iso' to
-- swap the elements of the pair, then you can use 'swapped'. 
--
-- This section provides several simple 'Iso's and 'TwoWay's that can be composed with other 'Iso's
-- and 'TwoWay's using the "Control.Category" operators @('Control.Category.>>>')@ and
-- @('Control.Category.<<<')@.
--
-- One simple example of how this could be useful: converting temperature values between Farenheight
-- and Celcius, you can compose the functions 'added' and 'multiplied' like so:
--
-- @
-- c2f :: 'Prelude.Monad' m => 'Iso' m Rational Rational
-- c2f = 'added' ('Prelude.negate' 32) 'Control.Category.>>>' 'multiplied' (5/9)
-- @
--
-- Then, to convert from Farenheight to Celcius you use @'un' c2f@, and to convert from Celcius to
-- Farenheight, use @'make' c2f@. The inverse function is constructed automatically by virtue of the
-- fact that addition and multiplication over 'Prelude.Rational' numbers are isomorphic functions.

swapped :: Monad m => Iso m (a, b) (b, a)
swapped = newIso (uncurry (flip (,)), uncurry (flip (,)))

dyslexic :: Monad m => Iso m (Either a b) (Either b a)
dyslexic = newIso (Right ||| Left, Right ||| Left)

zipped :: Monad m => TwoWay m [(a, b)] ([a], [b])
zipped = new2way (unzip, uncurry zip)

negated :: (Num a, Monad m) => Iso m a a
negated = newIso (negate, negate)

charred :: Monad m => TwoWay m Char Int
charred = new2way (ord, chr)

parsed :: (Read a, Show a, Monad m) => TwoWay m a String
parsed = new2way (show, read)

rounded :: (Real a, Integral a, RealFrac b, Fractional b, Monad m) => TwoWay m a b
rounded = new2way (realToFrac, round)

floored :: (Real a, Integral a, RealFrac b, Fractional b, Monad m) => TwoWay m a b
floored = new2way (realToFrac, floor)

ceilinged :: (Real a, Integral a, RealFrac b, Fractional b, Monad m) => TwoWay m a b
ceilinged = new2way (realToFrac, ceiling)

added :: (Num a, Monad m) => a -> Iso m a a
added a = newIso (subtract a, (+ a))

multiplied :: (Num a, Fractional a, Monad m) => a -> Iso m a a
multiplied a = new2way ((/ a), (* a))

worded :: Monad m => TwoWay m [String] String
worded = new2way (unwords, words)

lined :: Monad m => TwoWay m [String] String
lined = new2way (unlines, lines)

-- | A 'Data.Text.Lazy.Text' data type from the "Data.Text.Lazy" module.
type LazyText = Lazy.Text

-- | A 'Data.Text.Text' data type from the "Data.Text" module.
type StrictText = Strict.Text

text :: Monad m => Iso m StrictText String
text = newIso (Strict.unpack, Strict.pack)

lazyText :: Monad m => Iso m LazyText String
lazyText = newIso (Lazy.unpack, Lazy.pack)

-- | 'make' 'StrictText' from 'LazyText'.
strictifyText :: Monad m => Iso m StrictText LazyText
strictifyText = newIso (Lazy.fromStrict, Lazy.toStrict)

lazyLined :: Monad m => TwoWay m [LazyText] LazyText
lazyLined = new2way (Lazy.unlines, Lazy.lines)

lazyWorded :: Monad m => TwoWay m [LazyText] LazyText
lazyWorded = new2way (Lazy.unwords, Lazy.words)

strictLined :: Monad m => TwoWay m [StrictText] StrictText
strictLined = new2way (Strict.unlines, Strict.lines)

strictWorded :: Monad m => TwoWay m [StrictText] StrictText
strictWorded = new2way (Strict.unwords, Strict.words)

-- | A 'Data.ByteString.Lazy.ByteString' data type from the "Data.Text.Lazy" module.
type LazyByteString = Bz.ByteString
--
-- | A 'Data.ByteString.ByteString' data type from the "Data.ByteString" module.
type StrictByteString = B.ByteString

byteString :: Monad m => Iso m StrictByteString [Word8]
byteString = newIso (B.unpack, B.pack)

lazyByteString :: Monad m => Iso m LazyByteString [Word8]
lazyByteString = newIso (Bz.unpack, Bz.pack)

utf8 :: Monad m => Iso m StrictByteString StrictText
utf8 = newIso (Strict.decodeUtf8, Strict.encodeUtf8)

utf16BE :: Monad m => Iso m StrictByteString StrictText
utf16BE = newIso (Strict.decodeUtf16BE, Strict.encodeUtf16BE)

utf16LE :: Monad m => Iso m StrictByteString StrictText
utf16LE = newIso (Strict.decodeUtf16LE, Strict.encodeUtf16LE)

utf32BE :: Monad m => Iso m StrictByteString StrictText
utf32BE = newIso (Strict.decodeUtf16BE, Strict.encodeUtf16BE)

utf32LE :: Monad m => Iso m StrictByteString StrictText
utf32LE = newIso (Strict.decodeUtf32LE, Strict.encodeUtf32LE)

lazyUtf8 :: Monad m => Iso m LazyByteString LazyText
lazyUtf8 = newIso (Lazy.decodeUtf8, Lazy.encodeUtf8)

lazyUtf16BE :: Monad m => Iso m LazyByteString LazyText
lazyUtf16BE = newIso (Lazy.decodeUtf16BE, Lazy.encodeUtf16BE)

lazyUtf16LE :: Monad m => Iso m LazyByteString LazyText
lazyUtf16LE = newIso (Lazy.decodeUtf16LE, Lazy.encodeUtf16LE)

lazyUtf32BE :: Monad m => Iso m LazyByteString LazyText
lazyUtf32BE = newIso (Lazy.decodeUtf16BE, Lazy.encodeUtf16BE)

lazyUtf32LE :: Monad m => Iso m LazyByteString LazyText
lazyUtf32LE = newIso (Lazy.decodeUtf32LE, Lazy.encodeUtf32LE)

----------------------------------------------------------------------------------------------------

-- $ImportantDataType
-- The Haskell platform provides a variety of data @newtype@s for 'Data.Monoid.Monoid' and
-- 'Control.Applicative.Applicative' functions. This section provides convenient 'Iso'morphism
-- combinators for converting between these @newtype@s and the wrapped values.

-- | 'Iso'-morphism for the 'Control.Monad.Identity.Identity' value in the "Data.Monoid" module.
identity :: Monad m => Iso m (Identity a) a
identity = newIso (runIdentity, Identity)

-- | 'Iso'-morphism that can 'make' and 'un'-make a 'Control.Arrow.Kleisli'
-- 'Control.Arrow.Arrow'.
kleisli :: (Monad m, Monad mm) => Iso m (Kleisli mm a b) (a -> mm b)
kleisli = newIso (runKleisli, Kleisli)

-- | 'Iso'-morphism for the 'Data.Monoid.Endo' value in the "Data.Monoid" module.
monoidEndo :: Monad m => Iso m (Endo a) (a -> a)
monoidEndo = newIso (appEndo, Endo)

-- | 'Iso'-morphism that changes an ordinary 'Data.Monoid.Endo'-functor into it's equivalent
-- 'Control.Monad.Identity.Identity' monad.
endoIdentity :: Monad m => Iso m (a -> a) (a -> Identity a)
endoIdentity = newIso ((return .), (runIdentity .))

-- | 'Iso'-morphism for the 'Data.Monoid.Dual' value in the "Data.Monoid" module.
monoidDual :: Monad m => Iso m (Dual a) a
monoidDual = newIso (getDual, Dual)

monoidAll :: Monad m => Iso m All Bool
monoidAll = newIso (getAll, All)

monoidAny :: Monad m => Iso m Any Bool
monoidAny = newIso (getAny, Any)

monoidSum :: Monad m => Iso m (Sum a) a
monoidSum = newIso (getSum, Sum)

monoidProduct :: Monad m => Iso m (Product a) a
monoidProduct = newIso (getProduct, Product)

monoidFirst :: Monad m => Iso m (Data.Monoid.First a) (Maybe a)
monoidFirst = newIso (Data.Monoid.getFirst, Data.Monoid.First)

monoidLast :: Monad m => Iso m (Data.Monoid.Last a) (Maybe a)
monoidLast = newIso (Data.Monoid.getLast, Data.Monoid.Last)

semiFirst :: Monad m => Iso m (Data.Semigroup.First a) a
semiFirst = newIso (Data.Semigroup.getFirst, Data.Semigroup.First)

semiLast :: Monad m => Iso m (Data.Semigroup.Last a) a
semiLast = newIso (Data.Semigroup.getLast, Data.Semigroup.Last)

semiOption :: Monad m => Iso m (Data.Semigroup.Option a) (Maybe a)
semiOption = newIso (getOption, Option)

semiMin :: Monad m => Iso m (Min a) a
semiMin = newIso (getMin, Min)

semiMax :: Monad m => Iso m (Max a) a
semiMax = newIso (getMax, Max)

apConst :: Monad m => Iso m (Const a b) a
apConst = newIso (getConst, Const)

apWrapMonad :: (Monad m, Monad wm) => Iso m (WrappedMonad wm a) (wm a)
apWrapMonad = newIso (unwrapMonad, WrapMonad)

apWrapArrow :: (Monad m, Arrow wa) => Iso m (WrappedArrow wa a b) (wa a b)
apWrapArrow = newIso (unwrapArrow, WrapArrow)

apZipList :: Monad m => Iso m (ZipList a) [a]
apZipList = newIso (getZipList, ZipList)

----------------------------------------------------------------------------------------------------

-- | The expression:
--
-- @
-- someContainer 'Data.Lens.Minilens.~>' getElem
-- @
--
-- will use a lens called @getElem@ to fetch an element from the data structure called
-- @someContainer@.
--
-- It looks and behaves similar to the C/C++ programming language operator @->@. It is left
-- associative (precedence 9) so the expression @a~>b~>c~>d@ is the same as @((a ~> b) ~> c) ~> d@,
-- which means you can use this operator to compose 'Lens'es to retrieve elements at arbitrary
-- depth.
--
-- This function requires a 'PureLens', but of course any 'Lens' polymorphic over the monadic type
-- @m@ can be used, automatic type inference will choose to make use of
-- 'Data.Monad.Identity.Identity' if the type is polymorphic.
(~>) :: c -> PureLens c e -> e
(~>) = flip fetch
infixl 9 ~>

-- | The 'Data.Lens.Minilens.~>' operator uses a 'Lens' to fetch an element from a container. Often
-- times, it is useful to use the 'Lens' alone using Haskell's infix operator section syntax:
--
-- @
-- getter :: Container -> Element
-- getter container = container'Data.Lens.Minilens.~>'lensToGetElem
-- -- This can be abreviated to:
--
-- getter :: Container -> Element
-- getter = (~> lensToGetElem)
-- @
--
-- It is often useful to use a @getter@ that we see above with 'Data.Functor.Functor's '
-- 'Data.Functor.fmap' infix operator: @('Data.Functor.<$>')@. For example:
--
-- @
-- fmapper :: [Container] -> [Element]
-- fmapper list = (~> lensToGetElem) 'Data.Functor.<$>' list
-- @
--
-- This pattern is used so often that it warrants it's own operator. The 'Data.Lens.Minimal.<$~>'
-- operator uses @('Data.Lens.Minimal.~>)'@ applies a lens to the element of a
-- 'Data.Functor.Functor' using 'Data.Functor.<$>'. So the above @fmapper@ could be written as:
--
-- @
-- fmapper :: [Container] -> [Element]
-- fmapper list = listToGetElem 'Data.Lens.Minimal.<$~>' list
-- @
--
-- which can be further abbreviated to:
--
-- @
-- fmapper :: [Container] -> [Element]
-- fmapper = (listToGetElem 'Data.Lens.Minimal.<$~>')
-- @
--
-- Used as an infix operator, @('Data.Lens.Minimal.Lens.<$~>') is right-associative with a
-- precedence of 4, making it the same fixity and associativity as the 'Control.Applicative.<$>' and
-- 'Control.Applicative.<*>' functions.
(<$~>) :: Functor f => PureLens c e -> f c -> f e
(<$~>) lens f = (~> lens) <$> f
infixr 4 <$~>

-- | The 'with' function is defined to replace the Haskell programming language's own record alter
-- syntax:
--
-- @
-- record{ field1=val1, field2=val2, ... fieldN=valN }
-- @
--
-- The 'with' function simulates this record updating semantics with a visually similar syntax:
--
-- @
-- with record [ field1<~val1, field2<~val2, ... , field2<~val3 ]
-- @
--
-- But unlike the Haskell's record syntax, record fields are 'Lens'es, which are composable. So it
-- allows you write a record updating function like so:
--
-- @
-- moveSoutheast :: Gameboard -> Gameboard
-- moveSoutheast gameboard = 'with' gameboard
--     [ player 'Control.Category.>>>' position 'Control.Category.>>>' northCoord 'Data.Lens.Minimal.$=' 'Prelude.subtract' 1
--     , player 'Control.Category.>>>' position 'Control.Category.>>>' eastCoord 'Data.Lens.Minimal.$=' (+ 1)
--     , player 'Control.Category.>>>' isMyTurn 'Data.Lens.Minimal.<~' False
--     ]
-- @
--
-- The elements of the list passed to 'with' can be any updating function to change elements in a
-- container @c@. The list elements are of type @(c -> 'Control.Monad.Identity.Identity' c)@, which
-- are usually constructed using 'Lens' operators like @('Data.Lens.Minimal.<~')@ or
-- @('Data.Lens.Minimal.$=')@. Therefore any updating function may be used, not only functions
-- constructed from 'Lens' operators.
--
-- In category theory jargon, 'with' applies a sequence of 'Data.Monoid.Endo'functors (updating
-- functions) to a container @c@. The 'Data.Monoid.Endo'functors are applied in the order they
-- appear in the list from left-to-right, therefore the 'Data.Monoid.mconcat'enation of the
-- 'Data.Monoid.Dual' of each 'Data.Monoid.Endo'functor in the list @[c -> c]@ is applied to the
-- container @c@.
with :: c -> [c -> Identity c] -> c
with c = runIdentity . with' c

-- | Identical to the 'with' function, but performs each alter within a 'Control.Monad.Monad'. If
-- the 'Control.Monad.Monad' is @IO@, then each alter may result in side-effects.
with' :: Monad m => c -> [c -> m c] -> m c
with' c fx = foldl (>=>) return fx $ c

-- | This is the 'with' function with the parameters 'Prelude.flip'ped. It is convenient when used
-- with 'Control.Monad.State.Class.modify' when you want to alter the state of a
-- 'Control.Monad.State.Lazy.StateT' monad using a 'Lens'. So take the example given in the 'with'
-- section above, the function could be written as:
--
-- @
-- moveSoutheast :: Gameboard -> Gameboard
-- moveSoutheast = 'by'
--     [ player 'Control.Category.>>>' position 'Control.Category.>>>' northCoord 'Data.Lens.Minimal.$=' 'Prelude.subtract' 1
--     , player 'Control.Category.>>>' position 'Control.Category.>>>' eastCoord 'Data.Lens.Minimal.$=' (+ 1)
--     , player 'Control.Category.>>>' isMyTurn 'Data.Lens.Minimal.<~' False
--     ]
-- @
by :: [c -> Identity c] -> c -> c
by = flip with

-- | Monadic version of 'by'.
by' :: Monad m => [c -> m c] -> c -> m c
by' = flip with'

-- | Like 'with' but passes 'Data.Monoid.mempty' as the first parameter, so instead of writing
-- something like:
--
-- @
-- 'with' 'Data.Monoid.mempty' [foo 'Data.Lens.Minimal.Lens.<~' 0, bar 'Data.Lens.Minimal.Lens.<~' 1]
-- @
-- 
-- All you have to write is:
--
-- @
-- new [foo 'Data.Lens.Minimal.Lens.<~' 0, bar 'Data.Lens.Minimal.Lens.<~' 1]
-- @
new :: Monoid c => [c -> Identity c] -> c
new = runIdentity . new'

-- | Monadic version of 'new'
new' :: (Monad m, Monoid c) => [c -> m c] -> m c
new' = with' mempty

-- | This is a function intended to be used with the 'with', 'withM', 'by', or 'byM' function. It is
-- used for constructing a simple updating 'Data.Monoid.Endo'functor (updating function) that simply
-- stores the element @e@ into a container @c@ using 'pureUpdate'. You would use this operator when
-- building a list of updates to pass to the 'with' function.
--
-- This operator is visually similar to the bind operator used in Haskell's "do" notation @(<-)@.
-- Visually, it looks like you are writing a value into a lens, like in a procedural programming
-- language where the field you want to modify is on the left of the assignment operator, and the
-- value you want to write is on the right.
--
-- @
-- 'with' myData [fieldInData 'Data.Lens.Minimal.Lens.<~' 0]
-- @
(<~) :: Monad m => Lens m c e -> e -> c -> m c
(<~) lens e = alter' lens (const $ return e)
infixr 0 <~

-- | This is a function intended to be used with the 'with', 'by', or 'new' functions. It is used
-- for constructing a simple updating 'Data.Monoid.Endo'functor (updating function) that updates
-- element @e@ inside of a container @c@ using 'pureAlter'. You would use this operator when
-- building a list of updates to pass to the 'with' function.
--
-- This operator is superficially similar to updating operators in popular C/C++ family of
-- programming languages. In this languages, to do an in-place alter on a variable "x", for example
-- to increment an integer "x" by 5, you would write: @myData->x += 5;@
--
-- Likewise this operator does an "in-place alter." However you must provide a _non_monadic_
-- function on the right-hand side of this operator that will perform the alter:
--
-- @
-- 'with' myData [x 'Data.Lens.Minimal.Lens.$=' (+ 5)]
-- @
($=) :: Monad m => Lens m c e -> (e -> e) -> c -> m c
($=) lens f = lens $$= return . f
infixr 0 $=

-- | Like @('Data.Lens.Minimal.Lens.$=')@, but the updater function is monadic.
($$=) :: Monad m => Lens m c e -> (e -> m e) -> c -> m c
($$=) = alter'
infixr 0 $$=

----------------------------------------------------------------------------------------------------

-- | Use a 'Lens' to read an element @e@ within a container @c@. This function is usually used in
-- @do@ notation:
--
-- @
-- func container1 container2 = do
--     elem <- container1 `fetch` a 'Control.Category.>>>' b 'Control.Category.>>>' c 'Control.Category.>>>' d;
--     foo  <- container2 `fetch` bar 'Control.Category.>>>' baz
--     combineAndReturn elem foo
-- @
fetch' :: Monad m => Lens m c e -> c -> m e
fetch' (Lens (fetch, _)) = fetch

-- | Similar to 'fetch'', but performs the lookup purely, without Monadic side-effects. It is usually
-- easier to use the infix operator version of this function: @('Data.Lens.Minimal.~>')@.
fetch :: PureLens c e -> c -> e
fetch lens = runIdentity . fetch' lens

-- | Defined as @(\\lens -> 'Control.Monad.State.Class.get' >>= 'fetch' lens)@. Although instead of
-- this function, it is usually easier to simply write:
--
-- @
-- 'Control.Monad.State.Class.get' >>= 'fetch' lens
-- @
--
-- or if your lens can be used as a 'PureLens':
--
-- @
-- 'Control.Monad.State.Class.gets' ('Data.Lens.Minimal.~>' lens)
-- @
lensGet :: (Monad m, MonadState c m) => Lens m c e -> m e
lensGet lens = get >>= fetch' lens

lensModify :: (Monad m, MonadState c m) => Lens m c e -> (e -> m e) -> m e
lensModify (Lens (fetch, alter)) upd = get >>= \c ->
  fetch c >>= upd >>= \e -> alter (const $ return e) c >>= put >> return e

-- | Use a 'Lens' that can access the 'Control.Monad.State.state' within a
-- 'Control.Monad.State.Class.MonadState' function to alter the 'Control.Monad.State.state'. The
-- 'lensPut' function is defined as:
--
-- @
-- (\\lens elem -> 'Control.Monad.State.Class.get' >>= 'alter' lens elem >>= \\e -> 'Control.Monad.State.Class.put' e >> return e)
-- @
lensPut :: (Monad m, MonadState c m) => Lens m c e -> e -> m e
lensPut lens e = lensModify lens (const $ return e)

-- | Uses a 'Lens' to extract an element @e@ from a container @c@, then applies the element to the
-- given monadic updating function @(e -> m e)@, then places the element @e@ back into the container
-- @c@.
alter' :: Monad m => Lens m c e -> (e -> m e) -> c -> m c
alter' (Lens (_, alter)) f = alter f

-- | Similar to 'alter' but requires a 'PureLens' and performs an alter with a pure function. It
-- is usually easier to use the infix operator version of this function: @('Data.Lens.Minimal.$=')@.
alter :: PureLens c e -> (e -> e) -> c -> c
alter lens f = runIdentity . alter' lens (return . f)

----------------------------------------------------------------------------------------------------

-- | The 'defaul' 'Lens' operates on the element of a 'Data.Maybe.Maybe' data type given a default
-- value if the structure contains 'Data.Maybe.Nothing'. The structure will not be updated if it is
-- 'Data.Maybe.Nothing', so it will not 'Data.Map.alter' a 'Data.Map.Map' data structure unless the
-- key being updated already exists. This function is not called @default@ because @default@ is a
-- reserved word in Haskell.
--
-- Try this in GHCi:
--
-- @
-- 'with' ('Data.Maybe.Just' 10) ['defaul'' ('Control.Monad.return' 0) 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 15)
-- 'with' 'Data.Maybe.Nothing' ['defaul'' ('Control.Monad.return' 0) 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to 'Data.Maybe.Nothing'
-- @
defaul' :: Monad m => m e -> Lens m (Maybe e) e
defaul' e = newLens' (maybe e return, \e -> maybe (return Nothing) $ const $ return $ Just e)

-- | This 'Lens' is shorthand for @('defaul'' 'Control.Category..' 'Control.Monad.return')@
--
-- Try this in GHCi:
--
-- @
-- 'with' ('Data.Maybe.Just' 10) ['defaul' 0 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 15)
-- 'with' 'Data.Maybe.Nothing' ['defaul' 0 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to 'Data.Maybe.Nothing'
-- @
defaul :: Monad m => e -> Lens m (Maybe e) e
defaul = defaul' . return

-- | The 'ifJust' lens only works if the element in the container is not 'Data.Maybe.Nothing'.
-- This 'Lens' is shorthand for @('defaul' 'Data.Monoid.mempty')@. This 'Lens' accesss the element
-- or returns 'Data.Monoid.mempty'. If a non-existent element is updated, nothing is changed --
-- changes only occur if the element is not 'Data.Maybe.Nothing'.
--
-- Try this in GHCi:
--
-- @
-- import "Data.Monoid"
-- import "Data.Functor"
-- 'Data.Monoid.getSum' 'Data.Functor.<$>' 'with' ('Data.Maybe.Just' 10) ['ifJust' 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 15)
-- 'Data.Monoid.getSum' 'Data.Functor.<$>' 'with' 'Data.Maybe.Nothing' ['ifJust' 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to 'Data.Maybe.Nothing'
-- @
ifJust :: (Monad m, Monoid e) => Lens m (Maybe e) e
ifJust = defaul mempty

-- | The 'orElse'' 'Lens' operates on the element of a 'Data.Maybe.Maybe' data type given a default
-- value if the structure contains 'Data.Maybe.Nothing'. The structure will be changed to the
-- updated default value if it is 'Data.Maybe.Nothing'. So if this function is used to
-- 'Data.Map.alter' a 'Data.Map.Map' data structure where no element is associated with a given key,
-- this function will force the value to exist and be associated with the given key.
--
-- Try this in GHCi:
--
-- @
-- 'with' ('Data.Maybe.Just' 10) ['orElse'' ('Control.Monad.return' 0) 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 15)
-- 'with' 'Data.Maybe.Nothing' ['orElse'' ('Control.Monad.return' 0) 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 5)
-- @
orElse' :: Monad m => m e -> Lens m (Maybe e) e
orElse' e = Lens (maybe e return, \upd -> liftM Just . maybe (e >>= upd) upd)

-- | This 'Lens' is shorthand for @('orElse'' 'Control.Category..' 'Control.Monad.return')@.
--
-- Try this in GHCi:
--
-- @
-- 'with' ('Data.Maybe.Just' 10) ['orElse' 0 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 15)
-- 'with' 'Data.Maybe.Nothing' ['orElse' 0 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 5)
-- @
orElse :: Monad m => e -> Lens m (Maybe e) e
orElse = orElse' . return

-- | This function is shorthand for @('orElse' 'Data.Monoid.mempty')@. This 'Lens' accesses the
-- element, it forces the element to 'Data.Monoid.mempty' if it does not exist.
--
-- Try this in GHCi:
--
-- @
-- import "Data.Monoid"
-- import "Data.Functor"
-- 'Data.Monoid.getSum' 'Data.Functor.<$>' 'with' ('Data.Maybe.Just' 10) ['just' 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 15)
-- 'Data.Monoid.getSum' 'Data.Functor.<$>' 'with' 'Data.Maybe.Nothing' ['just' 'Data.Lens.Minimal.$=' (+ 5)] 
-- -- evaluates to ('Data.Maybe.Just' 5)
-- @
just :: (Monad m, Monoid e) => Lens m (Maybe e) e
just = orElse mempty

-- | This lens looks at a sub-container type @sub@ within a 'Data.Maybe.Maybe' container. Pass a
-- predicate that evaluates to 'Prelude.True' if the @sub@-container is empty, and an empty @sub@
-- container. When 'fetch'-ing using this lens, if the container is 'Data.Maybe.Nothing' this lens
-- evaluates to the given empty @sub@-container. When 'alter'-ing, if the updated @sub@-container
-- is empty according to the given predicate, the container becomes 'Data.Maybe.Nothing'. For
-- example, in GHCi:
--
-- @
-- with ('Data.Maybe.Just' [1, 2]) ['notEmpty' 'Data.List.null' [] 'Data.Lens.Minimal.$=' 'Data.Functor.fmap' (+ 10)]
-- -- evaluates to [11, 12]
--
-- with ('Data.Maybe.Just' [1, 2]) ['notEmpty' 'Data.List.null' [] 'Data.Lens.Minimal.$=' tail]
-- -- evaluates to [2]
--
-- with ('Data.Maybe.Just' [1, 2])
--     [ 'notEmpty' 'Data.List.null' [] 'Data.Lens.Minimal.$=' tail -- removes the first element
--     , 'notEmpty' 'Data.List.null' [] 'Data.Lens.Minimal.$=' tail -- removes the second (and final) element
--        -- The list we started with is now empty.
--     ]
-- -- evaluates to 'Data.Maybe.Nothing'
--
-- with ('Data.Maybe.Just' [0..]) ['notEmpty' 'Data.List.null' [] <~ []] -- store a null list
-- -- evaluates to 'Data.Maybe.Nothing'
--
-- with 'Data.Maybe.Nothing' ['notEmpty' 'Data.List.null' [] 'Data.Lens.Minimal.$=' (0 :)]
-- -- evaluates to [0]
-- @
notEmpty :: Monad m => (sub -> Bool) -> sub -> Lens m (Maybe sub) sub
notEmpty null empty = newLens (fromMaybe empty, \sub _ -> if null sub then Nothing else Just sub)

-- | Like 'notEmpty' but uses a 'Data.Monoid.Monoid' that satisfies 'Prelude.Eq'-uality. This
-- function is shorthand for
--
-- @
-- 'notEmpty' (== 'Data.Monoid.mempty') 'Data.Monoid.mempty'
-- @
notMEmpty :: (Monad m, Eq sub, Monoid sub) => Lens m (Maybe sub) sub
notMEmpty = notEmpty (== mempty) mempty

-- | This is a non-pure 'Lens' that requires the mondic type instantiate 'Control.Monad.MonadPlus'
-- so it can function as a kind of 'Control.Monad.guard' function.  This 'Lens' operates on a
-- 'Data.Maybe.Maybe' data structure. The 'Lens' will 'fetch' or 'alter' a value if and only if the
-- data structure is 'Data.Maybe.Just'. If it is 'Data.Maybe.Nothing' the 'Lens' will evaluate to
-- 'Control.Monad.mzero'.
exists :: MonadPlus m => Lens m (Maybe e) e
exists = Lens (maybe mzero return, \upd -> liftM Just . maybe mzero upd)

-- | This is a non-pure 'Lens' that requires the monadic type instantiate 'Control.Monad.MonadPlus'.
-- This 'Lens' operates on a 'Data.Either.EIther' data structure. The 'Lens' will 'fetch' or
-- 'alter' a value if and only if the data structure is 'Data.Either.Left', otherwise the 'Lens'
-- evaluates to 'Control.Monad.mzero'.
leftLens :: MonadPlus m => Lens m (Either e anything) e
leftLens = Lens (return ||| const mzero, \upd -> liftM Left . upd ||| const mzero)

-- | This is a non-pure 'Lens' that requires the monadic type instantiate 'Control.Monad.MonadPlus'.
-- This 'Lens' operates on a 'Data.Either.Either' data structure. The 'Lens' will 'fetch' or
-- 'alter' a value if and only if the data structure is 'Data.Either.Left', otherwise the 'Lens'
-- evaluates to 'Control.Monad.mzero'.
rightLens :: MonadPlus m => Lens m (Either anything e) e
rightLens = Lens (const mzero ||| return, \upd -> const mzero ||| liftM Right . upd)

----------------------------------------------------------------------------------------------------

-- | To create a 'Lens' that focuses on an element of a dictionary, provide a lookup function (e.g.
-- 'Data.Map.lookup') and an alter function (e.g. 'Data.Map.alter'). Or just use the 'mapLens' or
-- 'intMapLens' functions predefined for the 'Data.Map.Map' and 'Data.IntMap.IntMap' data types,
-- respectively.
dictionaryLens
  :: (Monad m, Eq key, Ord key)
  => (key -> map o -> Maybe o)
  -> ((Maybe o -> Maybe o) -> key -> map o -> map o)
  -> key -> Lens m (map o) (Maybe o)
dictionaryLens lookup alter key = newLens (lookup key, \o -> alter (const o) key)

-- | A 'Lens' that focuses on an element of an 'Data.Map.Map' with the key to that element.
mapLens :: (Monad m, Eq key, Ord key) => key -> Lens m (M.Map key o) (Maybe o)
mapLens = dictionaryLens M.lookup M.alter

-- | A 'Lens' that focuses on an element of an 'Data.IntMap.IntMap' with the key to that element.
intMapLens :: Monad m => Int -> Lens m (I.IntMap o) (Maybe o)
intMapLens = dictionaryLens I.lookup I.alter

intMapListLens :: Monad m => Lens m (I.IntMap o) [(I.Key, o)]
intMapListLens = newLens (I.assocs, \o _ -> I.fromList o)

mapListLens :: (Monad m, Ord key) => Lens m (M.Map key o) [(key, o)]
mapListLens = newLens (M.assocs, \o _ -> M.fromList o)

----------------------------------------------------------------------------------------------------

-- | Create a lens that accesses an element at the given index in an array -- without bounds
-- checking, so this lens evaluates to 'Prelude.undefined' if the index is out of bounds.
arrayLens :: (Monad m, Ix i, IArray arr o) => i -> Lens m (arr i o) o
arrayLens i = newLens ((! i) , \o -> (// [(i, o)]))

-- | Create a lens that accesses an element at the given index in an array with bounds checking.
-- Evaluates 
maybeArrayLens :: (Monad m, Ix i, IArray arr o) => i -> Lens m (arr i o) (Maybe o)
maybeArrayLens i = let check a = inRange (bounds a) i in newLens
  ( \  a -> guard (check a) >> Just (a!i)
  , \e a -> maybe a (\e -> if check a then a // [(i, e)] else a) e
  )

ioArrayLens :: (Monad m, MonadIO m, Ix i, MArray arr o IO) => i -> Lens m (arr i o) o
ioArrayLens i = newLens'
  ( liftIO . flip readArray i
  , \o arr -> liftIO $ writeArray arr i o >> return arr
  )

-- | Checks if the index is within the bounds of the array, does no lookup or alter if the index is
-- out of bounds.
ioMaybeArrayLens :: (Monad m, MonadIO m, Ix i, MArray arr o IO) => i -> Lens m (arr i o) (Maybe o)
ioMaybeArrayLens i = let check a = liftIO $ flip inRange i <$> getBounds a in newLens'
  ( \  a -> check a >>= \ok -> if ok then liftIO $ Just <$> readArray a i else return Nothing
  , \e a -> flip (maybe $ return a) e $ \e -> check a >>= \ok ->
      if ok then liftIO (writeArray a i e) >> return a else return a
  )

----------------------------------------------------------------------------------------------------

ioRefLens :: (Monad m, MonadIO m) => Lens m (IORef o) o
ioRefLens = newLens' (liftIO . readIORef, \o ref -> liftIO $ writeIORef ref o >> return ref)

mvarLens :: (Monad m, MonadIO m) => Lens m (MVar o) o
mvarLens = newLens'
  ( liftIO . readMVar
  , \o mvar -> liftIO $ modifyMVar_ mvar (const $ return o) >> return mvar
  )

----------------------------------------------------------------------------------------------------

-- $TupleLenses
-- This section provides 10 lenses: @tuple0@, @tuple1@, @tuple2@, @tuple3@, @tuple4@, @tuple5@,
-- @tuple6@, @tuple7@, @tuple8@, and @tuple9@. The left-most element is the zero'th element. These
-- functions are all type class methods, because every different size of tuple is a different type.
--
-- If you end up using tuples with more than 10 elements, consider this a "code smell," (a sign that 
-- you are working with a program of bad design), and try to break-up your tuples into smaller
-- units, perhaps units wrapped in @newtype@s.

class TupleLens0 a o where { tuple0 :: Monad m => Lens m a o }
class TupleLens1 a o where { tuple1 :: Monad m => Lens m a o }
class TupleLens2 a o where { tuple2 :: Monad m => Lens m a o }
class TupleLens3 a o where { tuple3 :: Monad m => Lens m a o }
class TupleLens4 a o where { tuple4 :: Monad m => Lens m a o }
class TupleLens5 a o where { tuple5 :: Monad m => Lens m a o }
class TupleLens6 a o where { tuple6 :: Monad m => Lens m a o }
class TupleLens7 a o where { tuple7 :: Monad m => Lens m a o }
class TupleLens8 a o where { tuple8 :: Monad m => Lens m a o }
class TupleLens9 a o where { tuple9 :: Monad m => Lens m a o }

instance TupleLens0 (o, b, c, d, e, f, g, h, i, j) o where
  tuple0 = newLens (\ (o, _, _, _, _, _, _, _, _, _) -> o, \o (_, b, c, d, e, f, g, h, i, j) -> (o, b, c, d, e, f, g, h, i, j))

instance TupleLens0 (o, b, c, d, e, f, g, h, i) o where
  tuple0 = newLens (\ (o, _, _, _, _, _, _, _, _) -> o, \o (_, b, c, d, e, f, g, h, i) -> (o, b, c, d, e, f, g, h, i))

instance TupleLens0 (o, b, c, d, e, f, g, h) o where
  tuple0 = newLens (\ (o, _, _, _, _, _, _, _) -> o, \o (_, b, c, d, e, f, g, h) -> (o, b, c, d, e, f, g, h))

instance TupleLens0 (o, b, c, d, e, f, g) o where
  tuple0 = newLens (\ (o, _, _, _, _, _, _) -> o, \o (_, b, c, d, e, f, g) -> (o, b, c, d, e, f, g))

instance TupleLens0 (o, b, c, d, e, f) o where
  tuple0 = newLens (\ (o, _, _, _, _, _) -> o, \o (_, b, c, d, e, f) -> (o, b, c, d, e, f))

instance TupleLens0 (o, b, c, d, e) o where
  tuple0 = newLens (\ (o, _, _, _, _) -> o, \o (_, b, c, d, e) -> (o, b, c, d, e))

instance TupleLens0 (o, b, c, d) o where
  tuple0 = newLens (\ (o, _, _, _) -> o, \o (_, b, c, d) -> (o, b, c, d))

instance TupleLens0 (o, b, c) o where
  tuple0 = newLens (\ (o, _, _) -> o, \o (_, b, c) -> (o, b, c))

instance TupleLens0 (o, b) o where
  tuple0 = newLens (\ (o, _) -> o, \o (_, b) -> (o, b))


instance TupleLens1 (a, o, c, d, e, f, g, h, i, j) o where
  tuple1 = newLens (\ (_, o, _, _, _, _, _, _, _, _) -> o, \o (a, _, c, d, e, f, g, h, i, j) -> (a, o, c, d, e, f, g, h, i, j))

instance TupleLens1 (a, o, c, d, e, f, g, h, i) o where
  tuple1 = newLens (\ (_, o, _, _, _, _, _, _, _) -> o, \o (a, _, c, d, e, f, g, h, i) -> (a, o, c, d, e, f, g, h, i))

instance TupleLens1 (a, o, c, d, e, f, g, h) o where
  tuple1 = newLens (\ (_, o, _, _, _, _, _, _) -> o, \o (a, _, c, d, e, f, g, h) -> (a, o, c, d, e, f, g, h))

instance TupleLens1 (a, o, c, d, e, f, g) o where
  tuple1 = newLens (\ (_, o, _, _, _, _, _) -> o, \o (a, _, c, d, e, f, g) -> (a, o, c, d, e, f, g))

instance TupleLens1 (a, o, c, d, e, f) o where
  tuple1 = newLens (\ (_, o, _, _, _, _) -> o, \o (a, _, c, d, e, f) -> (a, o, c, d, e, f))

instance TupleLens1 (a, o, c, d, e) o where
  tuple1 = newLens (\ (_, o, _, _, _) -> o, \o (a, _, c, d, e) -> (a, o, c, d, e))

instance TupleLens1 (a, o, c, d) o where
  tuple1 = newLens (\ (_, o, _, _) -> o, \o (a, _, c, d) -> (a, o, c, d))

instance TupleLens1 (a, o, c) o where
  tuple1 = newLens (\ (_, o, _) -> o, \o (a, _, c) -> (a, o, c))

instance TupleLens1 (a, o) o where
  tuple1 = newLens (\ (_, o) -> o, \o (a, _) -> (a, o))


instance TupleLens2 (a, b, o, d, e, f, g, h, i, j) o where
  tuple2 = newLens (\ (_, _, o, _, _, _, _, _, _, _) -> o, \o (a, b, _, d, e, f, g, h, i, j) -> (a, b, o, d, e, f, g, h, i, j))

instance TupleLens2 (a, b, o, d, e, f, g, h, i) o where
  tuple2 = newLens (\ (_, _, o, _, _, _, _, _, _) -> o, \o (a, b, _, d, e, f, g, h, i) -> (a, b, o, d, e, f, g, h, i))

instance TupleLens2 (a, b, o, d, e, f, g, h) o where
  tuple2 = newLens (\ (_, _, o, _, _, _, _, _) -> o, \o (a, b, _, d, e, f, g, h) -> (a, b, o, d, e, f, g, h))

instance TupleLens2 (a, b, o, d, e, f, g) o where
  tuple2 = newLens (\ (_, _, o, _, _, _, _) -> o, \o (a, b, _, d, e, f, g) -> (a, b, o, d, e, f, g))

instance TupleLens2 (a, b, o, d, e, f) o where
  tuple2 = newLens (\ (_, _, o, _, _, _) -> o, \o (a, b, _, d, e, f) -> (a, b, o, d, e, f))

instance TupleLens2 (a, b, o, d, e) o where
  tuple2 = newLens (\ (_, _, o, _, _) -> o, \o (a, b, _, d, e) -> (a, b, o, d, e))

instance TupleLens2 (a, b, o, d) o where
  tuple2 = newLens (\ (_, _, o, _) -> o, \o (a, b, _, d) -> (a, b, o, d))

instance TupleLens2 (a, b, o) o where
  tuple2 = newLens (\ (_, _, o) -> o, \o (a, b, _) -> (a, b, o))


instance TupleLens3 (a, b, c, o, e, f, g, h, i, j) o where
  tuple3 = newLens (\ (_, _, _, o, _, _, _, _, _, _) -> o, \o (a, b, c, _, e, f, g, h, i, j) -> (a, b, c, o, e, f, g, h, i, j))

instance TupleLens3 (a, b, c, o, e, f, g, h, i) o where
  tuple3 = newLens (\ (_, _, _, o, _, _, _, _, _) -> o, \o (a, b, c, _, e, f, g, h, i) -> (a, b, c, o, e, f, g, h, i))

instance TupleLens3 (a, b, c, o, e, f, g, h) o where
  tuple3 = newLens (\ (_, _, _, o, _, _, _, _) -> o, \o (a, b, c, _, e, f, g, h) -> (a, b, c, o, e, f, g, h))

instance TupleLens3 (a, b, c, o, e, f, g) o where
  tuple3 = newLens (\ (_, _, _, o, _, _, _) -> o, \o (a, b, c, _, e, f, g) -> (a, b, c, o, e, f, g))

instance TupleLens3 (a, b, c, o, e, f) o where
  tuple3 = newLens (\ (_, _, _, o, _, _) -> o, \o (a, b, c, _, e, f) -> (a, b, c, o, e, f))

instance TupleLens3 (a, b, c, o, e) o where
  tuple3 = newLens (\ (_, _, _, o, _) -> o, \o (a, b, c, _, e) -> (a, b, c, o, e))

instance TupleLens3 (a, b, c, o) o where
  tuple3 = newLens (\ (_, _, _, o) -> o, \o (a, b, c, _) -> (a, b, c, o))


instance TupleLens4 (a, b, c, d, o, f, g, h, i, j) o where
  tuple4 = newLens (\ (_, _, _, _, o, _, _, _, _, _) -> o, \o (a, b, c, d, _, f, g, h, i, j) -> (a, b, c, d, o, f, g, h, i, j))

instance TupleLens4 (a, b, c, d, o, f, g, h, i) o where
  tuple4 = newLens (\ (_, _, _, _, o, _, _, _, _) -> o, \o (a, b, c, d, _, f, g, h, i) -> (a, b, c, d, o, f, g, h, i))

instance TupleLens4 (a, b, c, d, o, f, g, h) o where
  tuple4 = newLens (\ (_, _, _, _, o, _, _, _) -> o, \o (a, b, c, d, _, f, g, h) -> (a, b, c, d, o, f, g, h))

instance TupleLens4 (a, b, c, d, o, f, g) o where
  tuple4 = newLens (\ (_, _, _, _, o, _, _) -> o, \o (a, b, c, d, _, f, g) -> (a, b, c, d, o, f, g))

instance TupleLens4 (a, b, c, d, o, f) o where
  tuple4 = newLens (\ (_, _, _, _, o, _) -> o, \o (a, b, c, d, _, f) -> (a, b, c, d, o, f))

instance TupleLens4 (a, b, c, d, o) o where
  tuple4 = newLens (\ (_, _, _, _, o) -> o, \o (a, b, c, d, _) -> (a, b, c, d, o))


instance TupleLens5 (a, b, c, d, e, o, g, h, i, j) o where
  tuple5 = newLens (\ (_, _, _, _, _, o, _, _, _, _) -> o, \o (a, b, c, d, e, _, g, h, i, j) -> (a, b, c, d, e, o, g, h, i, j))

instance TupleLens5 (a, b, c, d, e, o, g, h, i) o where
  tuple5 = newLens (\ (_, _, _, _, _, o, _, _, _) -> o, \o (a, b, c, d, e, _, g, h, i) -> (a, b, c, d, e, o, g, h, i))

instance TupleLens5 (a, b, c, d, e, o, g, h) o where
  tuple5 = newLens (\ (_, _, _, _, _, o, _, _) -> o, \o (a, b, c, d, e, _, g, h) -> (a, b, c, d, e, o, g, h))

instance TupleLens5 (a, b, c, d, e, o, g) o where
  tuple5 = newLens (\ (_, _, _, _, _, o, _) -> o, \o (a, b, c, d, e, _, g) -> (a, b, c, d, e, o, g))

instance TupleLens5 (a, b, c, d, e, o) o where
  tuple5 = newLens (\ (_, _, _, _, _, o) -> o, \o (a, b, c, d, e, _) -> (a, b, c, d, e, o))


instance TupleLens6 (a, b, c, d, e, f, o, h, i, j) o where
  tuple6 = newLens (\ (_, _, _, _, _, _, o, _, _, _) -> o, \o (a, b, c, d, e, f, _, h, i, j) -> (a, b, c, d, e, f, o, h, i, j))

instance TupleLens6 (a, b, c, d, e, f, o, h, i) o where
  tuple6 = newLens (\ (_, _, _, _, _, _, o, _, _) -> o, \o (a, b, c, d, e, f, _, h, i) -> (a, b, c, d, e, f, o, h, i))

instance TupleLens6 (a, b, c, d, e, f, o, h) o where
  tuple6 = newLens (\ (_, _, _, _, _, _, o, _) -> o, \o (a, b, c, d, e, f, _, h) -> (a, b, c, d, e, f, o, h))

instance TupleLens6 (a, b, c, d, e, f, o) o where
  tuple6 = newLens (\ (_, _, _, _, _, _, o) -> o, \o (a, b, c, d, e, f, _) -> (a, b, c, d, e, f, o))


instance TupleLens7 (a, b, c, d, e, f, g, o, i, j) o where
  tuple7 = newLens (\ (_, _, _, _, _, _, _, o, _, _) -> o, \o (a, b, c, d, e, f, g, _, i, j) -> (a, b, c, d, e, f, g, o, i, j))

instance TupleLens7 (a, b, c, d, e, f, g, o, i) o where
  tuple7 = newLens (\ (_, _, _, _, _, _, _, o, _) -> o, \o (a, b, c, d, e, f, g, _, i) -> (a, b, c, d, e, f, g, o, i))

instance TupleLens7 (a, b, c, d, e, f, g, o) o where
  tuple7 = newLens (\ (_, _, _, _, _, _, _, o) -> o, \o (a, b, c, d, e, f, g, _) -> (a, b, c, d, e, f, g, o))


instance TupleLens8 (a, b, c, d, e, f, g, h, o, j) o where
  tuple8 = newLens (\ (_, _, _, _, _, _, _, _, o, _) -> o, \o (a, b, c, d, e, f, g, h, _, j) -> (a, b, c, d, e, f, g, h, o, j))

instance TupleLens8 (a, b, c, d, e, f, g, h, o) o where
  tuple8 = newLens (\ (_, _, _, _, _, _, _, _, o) -> o, \o (a, b, c, d, e, f, g, h, _) -> (a, b, c, d, e, f, g, h, o))


instance TupleLens9 (a, b, c, d, e, f, g, h, i, o) o where
  tuple9 = newLens (\ (_, _, _, _, _, _, _, _, _, o) -> o, \o (a, b, c, d, e, f, g, h, i, _) -> (a, b, c, d, e, f, g, h, i, o))

