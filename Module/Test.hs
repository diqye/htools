{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
module Module.Test where 
import Module.Log(hInfo,err',err) -- required for Template syntax
testLog' = do
  $err' "I am an $err'"
  err "I am an err"
  putStrLn "Done"