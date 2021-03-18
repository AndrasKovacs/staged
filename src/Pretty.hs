
module Pretty where

-- import Common
-- import qualified Presyntax as P

-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
-- import FlatParse.Stateful

-- data Bind = DontBind | Bind ByteString
--   deriving Show

-- data Tm
--   = Var ByteString
--   | Let ByteString (Maybe Tm) Tm Tm
--   | Pi Bind Icit Tm Tm
--   | Lam Bind ArgInfo (Maybe Tm) Tm
--   | App Tm Tm ArgInfo
--   | Ty ByteString U
--   | Lift Tm
--   | Up   Tm
--   | Down Tm
--   | Rec    [(ByteString, Tm)]
--   | RecCon [(ByteString, Tm)]
--   | EmptyRec
--   | Tuple [Tm]
--   | Field Tm ByteString
--   | Fix ByteString ByteString Tm
--   | Case Tm [(ByteString, [ByteString], Tm)]
--   | Hole
--   deriving Show

-- showPTm :: ByteString -> P.Tm -> String
-- showPTm bs = show . go where

--   span :: Span -> ByteString
--   span sp = case runParser (unsafeSpanToByteString sp) () 0 bs of
--     OK s _ _ -> s
--     _        -> undefined

--   bind :: P.Bind -> Bind
--   bind P.DontBind = DontBind
--   bind (P.Bind x) = Bind (span x)

--   go :: P.Tm -> Tm
--   go = \case
--     P.Var x         -> Var (span x)
--     P.Let _ x a t u -> Let (span x) (go <$> a) (go t) (go u)
--     P.Pi _ x i a b  -> Pi (bind x) i (go a) (go b)
--     P.Lam _ x i a t -> Lam (bind x) i (go <$> a) (go t)
--     P.App t u
