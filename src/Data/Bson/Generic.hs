{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

------------------------------------------------------------------------------
-- | Examples
--
-- > data Test0 = A | B | C deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test0
-- > instance FromBSON Test0
-- >
-- > (fromBSON $ toBSON A) :: Maybe Test0
--
--
-- > data Test1 = Test1 String String deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test1
-- > instance FromBSON Test1
-- >
-- > (fromBSON $ toBSON $ Test1 "aa" "bb") :: Maybe Test1
--
--
-- > data Test2 = Test2 { test20 :: String, test21 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test2
-- > instance FromBSON Test2
-- >
-- > (fromBSON $ toBSON $ Test2 "aa" "bb") :: Maybe Test2
--
--
-- > data Test3 = Test3 { test30 :: Test2, test31 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test3
-- > instance FromBSON Test3
-- >
-- > (fromBSON $ toBSON $ Test3 (Test2 "aa" "bb") "cc") :: Maybe Test3
--
--
-- > data Test4 = Test4 { test4Key :: ObjectKey, test4 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test4
-- > instance FromBSON Test4
-- >
-- > (fromBSON $ toBSON $ Test4 (ObjectKey . Just $ unsafePerformIO genObjectId) "something") :: Maybe Test4
-- > (fromBSON $ toBSON $ Test4 (ObjectKey Nothing) "something") :: Maybe Test4
--
--
-- > data Comment = Comment { author :: String, comments :: [Comment] } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Comment
-- > instance FromBSON Comment
-- >
-- > (fromBSON $ toBSON $ Comment "Joe1" [Comment "Joe2" [], Comment "Joe3" [Comment "Joe4" [], Comment "Joe5" []]]) :: Maybe Comment
--
--
-- Representation
--
-- > toBSON $ Test2 "aa" "bb"
-- >
-- > [ test20: "aa", test21: "bb" ]
--
-- > toBSON $ Test3 (Test2 "aa" "bb") "cc"
-- >
-- > [ test30: [ test20: "aa", test21: "bb"], test31: "cc" ]
--
-- > toBSON $ Test4 (ObjectKey . Just $ unsafePerformIO genObjectId) "something"
-- >
-- > [ _id: 4f226c27900faa06ab000001, test4: "something" ]
--
-- > toBSON $ Test4 (ObjectKey Nothing) "something"
-- >
-- > [ test4: "something" ]
--
-- > toBSON $ Comment "Joe1" [ Comment "Joe2" []
-- >                         , Comment "Joe3" [ Comment "Joe4" []
-- >                                          , Comment "Joe5" []
-- >                                          ]
-- >                         ]
-- >
-- > [ author: "Joe1", comments: [ [ author: "Joe2", comments: []]
-- >                             , [ author: "Joe3", comments: [ [ author: "Joe4", comments: []]
-- >                                                           , [ author: "Joe5", comments: []]
-- >                                                           ]]
-- >                             ]]


module Data.Bson.Generic
( ToBSON(..)
, FromBSON(..)
, ObjectKey(..)
, Options(..)
, defaultOptions
, defaultLensOptions
) where

------------------------------------------------------------------------------
import           GHC.Generics
------------------------------------------------------------------------------
import           Control.Applicative  ()
import           Control.Monad
import           Control.Monad.Except (Except, throwError)
------------------------------------------------------------------------------
import           Data.Bson
import qualified Data.Bson            as BSON (lookup)
import qualified Data.Text            as TS (Text, concat, pack, unpack)
import           Data.Typeable

------------------------------------------------------------------------------

newtype ObjectKey = ObjectKey { unObjectKey :: Maybe ObjectId } deriving (Generic, Typeable, Show, Eq)
instance FromBSON ObjectKey
instance ToBSON ObjectKey

------------------------------------------------------------------------------

data Options = Options
    { fieldLabelModifier :: String -> String
    , keyLabel           :: Label
    , constructorLabel   :: Label
    , contentLabel       :: Label
    }

defaultOptions :: Options
defaultOptions = Options
    { fieldLabelModifier = id
    , keyLabel = TS.pack "_id"
    , constructorLabel = TS.pack "_co"
    , contentLabel = TS.pack "_data"
    }

defaultLensOptions :: Options
defaultLensOptions = defaultOptions { fieldLabelModifier = drop 1 }

------------------------------------------------------------------------------

instance Val a => ToBSON a where
    toBSONOpt _ _ = []
    toValue _ = val

instance ToBSON a => ToBSON [a] where
    toBSONOpt _ _ = []
    toValue opts x = Array $ map (toValue opts) x

instance ToBSON String where
    toBSONOpt _ _ = []
    toValue _ = val

instance Val a => FromBSON a where
    fromBSONOpt _ _ = throwError [""]
    fromValue _ x = case cast' x of
                        Just y  -> return y
                        Nothing -> throwError ["Mismatched type"]

instance FromBSON a => FromBSON [a] where
    fromBSONOpt _ _ = throwError [""]
    fromValue opts (Array x) = mapM (fromValue opts) x
    fromValue _ _            = throwError ["Not an array"]

instance FromBSON String where
    fromBSONOpt _ _ = throwError [""]
    fromValue _ (String x) = return $ TS.unpack x
    fromValue _ _          = throwError ["Not a string"]

------------------------------------------------------------------------------
-- TO BSON

class ToBSON a where
    toBSON :: a -> Document
    toBSON = toBSONOpt defaultOptions

    toBSONWithLens :: a -> Document
    toBSONWithLens = toBSONOpt defaultLensOptions

    toBSONOpt :: Options -> a -> Document

    default toBSONOpt :: (Generic a, GConstructorCount (Rep a), GToBSON (Rep a)) => Options -> a -> Document
    toBSONOpt opts doc = genericToBSON opts (constructorCount doc) (from doc)

    toValue :: Options -> a -> Value
    toValue opts = Doc . toBSONOpt opts

class GToBSON f where
    genericToBSON :: Options -> Int -> f a -> Document

-- | Unit type -> Empty document
instance GToBSON U1 where
    genericToBSON _ _ U1 = []

-- | Sum of types
instance (GToBSON a, GToBSON b) => GToBSON (a :*: b) where
    genericToBSON opts n (x :*: y) = genericToBSON opts n x ++ genericToBSON opts n y

-- | Product of types
instance (GToBSON a, GToBSON b) => GToBSON (a :+: b) where
    genericToBSON opts n (L1 x) = genericToBSON opts n x
    genericToBSON opts n (R1 x) = genericToBSON opts n x

-- | Datatype information tag
instance (GToBSON a) => GToBSON (D1 c a) where
    genericToBSON opts n (M1 x) = genericToBSON opts n x

-- | Constructor tag
gHandleConstructorFieldsToBSON :: (GToBSON a, GToArray a, Constructor c) => Options -> Int -> (C1 c a) b -> Document
gHandleConstructorFieldsToBSON opts n c@(M1 x) = if conIsRecord c
                                           then genericToBSON opts n x
                                           else
                                                let contentArray = genericToArray opts x
                                                    len = length contentArray
                                                in if len > 0 then [ contentLabel opts =: contentArray ] else []

instance (GToBSON a, GToArray a, Constructor c) => GToBSON (C1 c a) where
    genericToBSON opts 0 x = gHandleConstructorFieldsToBSON opts 0 x
    genericToBSON opts 1 x = gHandleConstructorFieldsToBSON opts 1 x
    genericToBSON opts n c@x = gHandleConstructorFieldsToBSON opts n x ++ [ constructorLabel opts =: conName c ]

-- | Selector tag
instance (ToBSON a, Selector s) => GToBSON (S1 s (K1 i a)) where
    genericToBSON opts _ s@(M1 (K1 x)) = [(TS.pack . fieldLabelModifier opts . selName) s =: toValue opts x]

class GToArray f where
    genericToArray :: Options -> f a -> [Value]

instance GToArray U1 where
    genericToArray _ U1 = []

instance (GToArray a, GToArray b) => GToArray (a :*: b) where
    genericToArray opts (x :*: y) = genericToArray opts x ++ genericToArray opts y

instance (ToBSON a, Selector s) => GToArray (S1 s (K1 i a)) where
    genericToArray opts (M1 (K1 x)) = [toValue opts x]

-- | ObjectKey special treatment
instance (Selector s) => GToBSON (S1 s (K1 i ObjectKey)) where
    genericToBSON opts _ (M1 (K1 (ObjectKey (Just key)))) = [ keyLabel opts =: key ]
    genericToBSON                            _ _ _ = []

-- | Constants
instance (ToBSON a) => GToBSON (K1 i a) where
    genericToBSON _ _ (K1 x) = toBSON x

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- FROM BSON
type DeserializeResult a = Except [TS.Text] a

class FromBSON a where
    fromBSON :: Document -> DeserializeResult a
    fromBSON = fromBSONOpt defaultOptions

    fromBSONWithLens :: Document -> DeserializeResult a
    fromBSONWithLens = fromBSONOpt defaultLensOptions

    fromBSONOpt :: Options -> Document -> DeserializeResult a

    default fromBSONOpt :: (Generic a, GConstructorCount (Rep a), GFromBSON (Rep a)) => Options -> Document -> DeserializeResult a
    fromBSONOpt opts doc = to <$> genericFromBSON opts (constructorCount (undefined :: a)) doc

    fromValue :: Options -> Value -> DeserializeResult a
    fromValue opts (Doc x) = fromBSONOpt opts x
    fromValue _ _          = throwError ["Not a value"]

class GFromBSON f where
    genericFromBSON :: Options -> Int -> Document -> DeserializeResult (f a)

instance GFromBSON U1 where
    genericFromBSON _ _ [] = return U1
    genericFromBSON _ _ _  = throwError ["Expect empty object"]

instance (GFromBSON a, GFromBSON b) => GFromBSON (a :*: b) where
    genericFromBSON opts n doc = do
        x <- genericFromBSON opts n doc
        y <- genericFromBSON opts n doc
        return $ x :*: y

instance (GFromBSON a, GFromBSON b) => GFromBSON (a :+: b) where
    genericFromBSON opts n doc = left `mplus` right
        where left  = L1 <$> genericFromBSON opts n doc
              right = R1 <$> genericFromBSON opts n doc

instance (GFromBSON a) => GFromBSON (M1 D c a) where
    genericFromBSON opts n doc = M1 <$> genericFromBSON opts n doc

gIsConstructorMatch :: Constructor c => Options -> Document -> C1 c a r -> DeserializeResult (C1 c a r)
gIsConstructorMatch opts doc x = do
    cname <- BSON.lookup (constructorLabel opts) doc
    let expectCname = conName x
    if cname == expectCname
    then return x
    else throwError [TS.concat ["Mismatch constructor: Expect '", TS.pack expectCname, "', but was '", TS.pack cname, "'"]]

gHandleConstructorFieldsFromBSON :: (GFromArray a, GFromBSON a, Constructor c) => Options -> Int -> Document -> C1 c a r -> DeserializeResult (C1 c a r)
gHandleConstructorFieldsFromBSON opts n doc x = if conIsRecord x
    then M1 <$> genericFromBSON opts n doc
    else do
        valArray <- BSON.lookup (contentLabel opts) doc :: DeserializeResult Value
        case valArray of
            Array arr -> M1 <$> genericFromArray opts arr
            _ -> throwError [TS.concat ["Expect an array under '", contentLabel opts, "' in ", TS.pack $ show doc]]


instance Constructor c => GFromBSON (C1 c U1) where
    genericFromBSON opts 0 doc = M1 <$> genericFromBSON opts 0 doc
    genericFromBSON opts 1 doc = M1 <$> genericFromBSON opts 1 doc
    genericFromBSON opts _ doc = gIsConstructorMatch opts doc (undefined :: M1 C c a r) >> (return $ M1 U1)

instance (GFromArray a, GFromBSON a, Constructor c) => GFromBSON (C1 c a) where
    genericFromBSON opts 0 doc = gHandleConstructorFieldsFromBSON opts 0 doc (undefined :: M1 C c a r)
    genericFromBSON opts 1 doc = gHandleConstructorFieldsFromBSON opts 1 doc (undefined :: M1 C c a r)
    genericFromBSON opts n doc = gIsConstructorMatch opts doc (undefined :: M1 C c a r) >>= gHandleConstructorFieldsFromBSON opts n doc


instance (FromBSON a, Selector s) => GFromBSON (S1 s (K1 i a)) where
    genericFromBSON opts _ doc = M1 . K1 <$> (BSON.lookup sname doc >>= fromValue opts)
        where
          sname = TS.pack . fieldLabelModifier opts . selName $ (undefined :: S1 s (K1 i a) r)

class GFromArray f where
    genericFromArray :: Options -> [Value] -> DeserializeResult (f a)

instance (FromBSON a, Selector s) => GFromArray (S1 s (K1 i a)) where
    genericFromArray opts  [x] = M1 . K1 <$> fromValue opts x
    genericFromArray _ _       = throwError ["Invalid type, expect Array"]

instance (GFromArray a, GFromArray b) => GFromArray (a :*: b) where
    genericFromArray _ [] = throwError ["Expect at least 2 elements in the array, got 0"]
    genericFromArray _ [_] = throwError ["Expect at least 2 elements in the array, got 1"]
    genericFromArray opts (x : xs) = do
        p <- genericFromArray opts [x]
        q <- genericFromArray opts xs
        return $ p :*: q


-- | ObjectKey special treatment
instance (Selector s) => GFromBSON (S1 s (K1 i ObjectKey)) where
    genericFromBSON opts _ doc = return . M1 . K1 $ ObjectKey (BSON.lookup (keyLabel opts) doc)

------------------------------------------------------------------------------
-- CONVENIENCE

class GConstructorCount f where
    gconstructorCount :: f a -> Int

instance GConstructorCount V1 where
    gconstructorCount _ = 0

instance (GConstructorCount a) => GConstructorCount (D1 d a) where
    gconstructorCount (M1 x) = gconstructorCount x

instance (Constructor c) => GConstructorCount (C1 c a) where
    gconstructorCount _ = 1

instance (GConstructorCount a, GConstructorCount b) => GConstructorCount (a :+: b) where
    gconstructorCount (_ :: (a :+: b) r) = gconstructorCount (undefined :: a r) +
                                           gconstructorCount (undefined :: b r)

constructorCount :: (Generic a, GConstructorCount (Rep a)) => a -> Int
constructorCount x = gconstructorCount $ from x
