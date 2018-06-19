{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Interface where

import Control.Monad
import Control.Monad.State
import GhcPlugins hiding (unitExpr)
import TcRnTypes hiding (Hole)
import HsExpr
import HsBinds
import HsExtension
import ConLike
import TcRnMonad hiding (Hole)
import TcEnv
import Control.Arrow
import TcEvidence
import TcSMonad hiding (tcLookupId)
import TcSimplify
import Bag
import IfaceEnv
import HsUtils
import TcExpr
import TcHsSyn
import TcType
import RnExpr
import Convert
import RnEnv


-- Library Interface

-- | A syntactic marker which shows where to run the plugin
runElab :: UserElab a -> a
runElab = error "You must run the plugin"

data Raw = Raw
data BinderTT = BinderTT
data NameType = NameType


-- | A data type of actions the interface supports
data ElabA a where
  GenSym :: OccName -> ElabA Name
  Focus :: Name -> ElabA ()
  Unfocus :: Name -> ElabA ()
  Fill :: Raw -> ElabA ()
  Solve :: ElabA ()
  Claim :: Name -> Raw -> ElabA ()
  Apply :: Raw -> [Bool] -> ElabA [Name]
  Compute :: ElabA ()
  RewriteWith :: Raw -> ElabA ()
  Attack :: ElabA ()
  Intro :: Name -> ElabA ()
  Forall :: Name -> Raw -> ElabA ()
  Patbind :: Name -> ElabA ()
  Letbind :: Name -> Raw -> Raw -> ElabA ()

  GetEnv :: ElabA [(Name, BinderTT)]
  LookupTy :: Name -> ElabA [(Name, NameType, CoreExpr)]

data Program instr a where
  Return :: a -> Program instr a
  (:>>=) :: Program instr a -> (a -> Program instr b) -> Program instr b
  Instr :: instr a -> Program instr a

instance Functor (Program instr) where
  fmap = liftM

instance Applicative (Program instr) where
  pure = Return
  (<*>) = ap


instance Monad (Program instr) where
  return = Return
  (>>=) = (:>>=)

interpretWithMonad :: forall instr m b.
    Monad m => (forall a. instr a -> m a) -> (Program instr b -> m b)
interpretWithMonad f = eval
    where
--    eval :: Program instr a -> m a
    eval (Return a) = return a
    eval (Instr i)  = f i
    eval (m :>>= k) = interpretWithMonad f m >>= interpretWithMonad f . k

type UserElab a = Program ElabA a


data Hole = Hole

data Constraint = Constraint

data ElabState = ElabState { goalType :: Type
                           , proofTerm :: CoreExpr
                           , holeQueue :: [Hole]
                           , constraints :: [Constraint]
                           }


newtype ElabM a = ElabM { runElabM :: StateT ElabState TcM a }
                  deriving (Functor, Applicative, Monad)

elaborate :: ElabM a -> TcM a
elaborate = flip evalStateT initialElabState . runElabM

initialElabState = undefined

interpret :: UserElab a -> ElabM a
interpret ue =
  interpretWithMonad interpretInstr ue

liftTcM :: TcM a -> ElabM a
liftTcM = ElabM . lift

interpretInstr :: ElabA a -> ElabM a
interpretInstr c =
  case c of
    GenSym s -> liftTcM (newName s)
    {-
    Focus :: Name -> Elab ()
    Unfocus :: Name -> Elab ()
    Fill :: Raw -> Elab ()
    Solve :: Elab ()
    Claim :: Name -> Raw -> Elab ()
    Apply :: Raw -> [Bool] -> Elab [Name]
    Compute :: Elab ()
    RewriteWith :: Raw -> Elab ()
    Attack :: Elab ()
    Intro :: Name -> Elab ()
    Forall :: Name -> Raw -> Elab ()
    Patbind :: Name -> Elab ()
    Letbind :: Name -> Raw -> Raw -> Elab ()

    GetEnv :: Elab [(Name, Binder TT)]
    LookupTy :: Name -> Elab [(Name, NameType, TT)]
-}





