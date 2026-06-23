module Main exposing (main)

import Browser
import Init exposing (init)
import Types exposing (Model, Msg)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = Types.UrlChanged
        , onUrlRequest = Types.LinkClicked
        }
