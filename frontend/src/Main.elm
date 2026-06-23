module Main exposing (main)

{-| Entry point for the Memory Diary 
-}

import Browser
import Browser.Navigation as Nav
import Init exposing (init)
import Types exposing (..)
import Update exposing (update)
import Url exposing (Url)
import View exposing (view)

-- | Handle URL changes by updating the route in the model
main : Program { year : Int, month : Int, day : Int } Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
