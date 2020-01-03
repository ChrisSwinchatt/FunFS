{- |
 - Module      : Data.FileSystem.FunFS.Ext2.LowLevel.Struct
 - Description : Serialisable records for representing low-level data structures.
 - Copyright   : (c) 2017-2019 Chris Swinchatt
 - License     : MIT
 - Maintainer  : Chris Swinchatt <chris@swinchatt.dev>
 - Stability   : experimental
 - Portability : portable
 -}
module Data.FileSystem.FunFS.Ext2.LowLevel.Struct ( Field
                                                  , field
                                                  , array
                                                  , struct
                                                  , module Data.FileSystem.FunFS.Util.SizeOf
                                                  , module Data.FileSystem.FunFS.Util.Serialisable
                                                  , module Data.FileSystem.FunFS.Util.Strings
                                                  ) where

import qualified Language.Haskell.TH as TH
import Data.FileSystem.FunFS.Util.SizeOf
import Data.FileSystem.FunFS.Util.Serialisable
import Data.FileSystem.FunFS.Util.Strings

-- | Structure field. This can be a scalar field, which represents a single value, or a vector field, which represents
-- an array.
data Field = Field TH.Name TH.TypeQ
           | Array TH.Name TH.TypeQ Int

-- | Create a field which is a single value.
field :: String -> TH.TypeQ -> Field
field = Field . TH.mkName

-- | Create a field which is an array of values.
array :: String -> TH.TypeQ -> Int -> Field
array name typeq size
    | size < 1  = error eListTooSmall
    | otherwise = Array (TH.mkName name) typeq size

-- | Generate a serialisable record.
struct :: String -> [Field] -> [String] -> TH.DecsQ
struct name fields instances = sequence [record,serialisable,sizeOf]
    where   name'   = TH.mkName name
            -- Generate a record.
            record  = do
                ctors' <- ctors
                return $ TH.DataD cxt name' tv kind ctors' insts
                where   cxt     = []
                        tv      = []
                        kind    = Nothing
                        ctors   = do
                            fields' <- mapM genField fields
                            return [TH.RecC name' fields']
                        insts   = map (TH.ConT . TH.mkName) instances
                        bang    = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
                        -- Generate scalar field.
                        genField (Field fname ftypeq) = do
                            ftype <- ftypeq
                            return (fname,bang,ftype)
                        -- Generate vector field.
                        genField (Array fname ftypeq _) = do
                            ftype <- ftypeq
                            return (fname,bang,TH.AppT TH.ListT ftype)
            -- Generate an instance of Serialisable.
            serialisable = do
                put'' <- put'
                return $ TH.InstanceD overlap cxt type' [get',put'']
                where   overlap = Nothing
                        cxt     = []
                        type'   = TH.AppT (TH.ConT $ TH.mkName "Serialisable") $ TH.ConT name'
                        -- Generate 'get' function.
                        get' = myFunD "get" [] (TH.DoE $ binds ++ ret) []
                            where   binds = map doGet fields
                                    ret   = [TH.NoBindS $ myAppE "return" $ TH.RecConE name' $ map fieldExp fields]
                                    -- Generate a field expression from a scalar field.
                                    fieldExp (Field fname _)   = (fname,TH.VarE fname)
                                    -- Generate a field expression from a vector field.
                                    fieldExp (Array fname _ _) = (fname,TH.VarE fname)
                                    -- Generate a 'get' call for a single value field.
                                    doGet (Field fname _)      = TH.BindS (TH.VarP fname) $ myVarE "get"
                                    -- Generate a 'get' call for a vector field.
                                    doGet (Array fname _ fcount) = TH.BindS (TH.VarP fname) $ myAppE "getList" $ intLE fcount
                        -- Generate 'put' function.
                        put' = do
                            arg <- TH.newName "x"
                            return $ myFunD "put" [TH.VarP arg] (TH.DoE $ map (doPut arg) fields) []
                            where   -- Generate a 'put' call for a scalar field.
                                    doPut arg (Field fname _)   = TH.NoBindS $ myAppE "put" $ TH.AppE (TH.VarE fname) $ TH.VarE arg
                                    -- Generate a 'put' call for a vector field.
                                    doPut arg (Array fname _ _) = TH.NoBindS $ myAppE "putList" $ TH.AppE (TH.VarE fname) $ TH.VarE arg
            -- Generate an instance of SizeOf
            sizeOf = do
                sizeof'' <- sizeof'
                return $ TH.InstanceD overlap cxt type' [sizeof'']
                where   overlap = Nothing
                        cxt     = []
                        type'   = TH.AppT (TH.ConT $ TH.mkName "SizeOf") $ TH.ConT name'
                        sizeof' = do
                            arg <- TH.newName "x"
                            return $ myFunD "sizeof" [TH.VarP arg] (body arg) []
                            where   body arg = myAppE "sum" $ TH.ListE $ exps arg
                                    exps arg = map (f arg) fields
                                    -- For scalar fields, compute the size of the field.
                                    f arg (Field fname _) = myAppE "sizeof" $ TH.AppE (TH.VarE fname) $ TH.VarE arg
                                    -- For vector fields, compute the product of the number of elements and the size of
                                    -- the 0th element.
                                    f arg (Array fname _ fcount) = prod
                                        where   prod  = TH.InfixE count mul size
                                                count = Just $ intLE fcount
                                                mul   = myVarE "*"
                                                size  = Just $ myAppE "sizeof" item
                                                item  = TH.InfixE list sub index
                                                list  = Just $ TH.AppE (TH.VarE fname) $ TH.VarE arg
                                                sub   = myVarE "!!"
                                                index = Just $ intLE 0
            -- Generate a simple function declaration.
            myFunD fn args body decs = TH.FunD (TH.mkName fn) [TH.Clause args (TH.NormalB body) decs]
            -- Generate a variable expression.
            myVarE = TH.VarE . TH.mkName
            -- Generate an application expression.
            myAppE = TH.AppE . myVarE
            -- Generate an integer literal expression.
            intLE  = TH.LitE . TH.IntegerL . fromIntegral
