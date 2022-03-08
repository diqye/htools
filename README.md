# htools
tools in haskell

## Log
Source: Module/Log.hs
```haskell
{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
module Module.Test where 
import Module.Log(hInfo) -- required for Template syntax
import qualified Module.Log as G
testLog' = do
  $(G.err') "I am an $(err')"
  $(G.info') "I am an $(info')"
  $(G.trace') "I am a $(trace')"
  G.err "I am an err"
  G.info "I am an info"
  G.trace "I am a trace'"
  putStrLn "Done"
```
You will get output
```bash
err Module.Test(6,5)2022-03-09 03:16:07> I am an $(err')
info Module.Test(7,5)2022-03-09 03:16:07> I am an $(info')
trace Module.Test(8,5)2022-03-09 03:16:07> I am a $(trace')
err 2022-03-09 03:16:07> I am an err
info 2022-03-09 03:16:07> I am an info
trace 2022-03-09 03:16:07> I am a trace'
Done
```
Or use simple syntax
```haskell
{-#LANGUAGE TemplateHaskell#-}
import Module.Log(hInfo,err,err') 
testLog' = do
  $err' "I am an $(err')"
  err "I am an err"
  putStrLn "Done"
```
## Getter Setter Over
Source: Module/TemplateGetter.hs
```Haskell
data Foo t = Foo {a::Int,b::String,c::t} | Foo2 {d::Int} deriving(Show)

mkSetAndOver ''Foo

-- You will get
set_a,set_b,set_c,set_d
over_a,over_b,over_c,over_d
set_a :: Int -> Foo t -> Foo t
over_a :: (Int -> Int) -> Foo t -> Foo t

-- Tuple 
_1,_2,_3,_4
set_1,set_2,set_3,set_4
over_1,over_2,over_3,over_4
_2 (1,2,3) --  2
set_2 100 (1,2,3,4) -- (1,100,3,4)
over_2 (+1) (1,2,3,4) -- (1,3,3,4)
```
## JSON tools
Source: Module/FreeJSON.hs

```haskell
getField :: A.FromJSON a => Text -> A.Value -> Either (Int,String) a
getValue :: A.FromJSON a => A.Value -> Either (Int,String) a
```
## HTTP Request
```haskell
let param = A.object 
      [ "channel" A..= ("XAU" :: String)
      , "content" A..= ("nihaoya" :: String)
      ]
let req = bhUtf8json param 
      $ hAuthorization "1qaz2wsx"
      $ mpost "http://hk.diqye.com/speaker/c"
r <- httpActionBody req
Char8.putStrLn r
-- hUtf8json = header (Header.hContentType,"application/json; charset=utf-8")
```
## Run a script
```bash
stack scripts/test.hs 
```