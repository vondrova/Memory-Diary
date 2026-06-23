module UpdateWorkflowTest exposing (suite)

{-| Tests for the HTTP mutation workflow helpers in `Update.Workflow`

Covers the two functions that convert `Http.Error` values into user-visible
messages stored in `model.infoDialog`:

  - `mutationFailure`     — maps each `Http.Error` variant to a fixed Czech
                            message (400 → "server rejected", network → "no
                            connection", etc.)
  - `withMutationFailure` — convenience wrapper that applies `mutationFailure`
                            inside a `Result` and stores it on the model
-}

import Expect
import Http
import Test exposing (Test, describe, test)
import Utils.Helpers exposing (mutationFailure, withMutationFailure)


suite : Test
suite =
    describe "HTTP mutation workflow"
        [ test "turns a validation response into a visible stable message" <|
            \_ ->
                mutationFailure (Http.BadStatus 400)
                    |> Expect.equal "The server rejected the request."
        , test "reports timeouts without exposing implementation details" <|
            \_ ->
                mutationFailure Http.Timeout
                    |> Expect.equal "The request timed out. Please try again."
        , test "reports network failures with a recovery hint" <|
            \_ ->
                mutationFailure Http.NetworkError
                    |> Expect.equal "The server is unreachable. Check your connection and try again."
        , test "does not expose decoder internals" <|
            \_ ->
                mutationFailure (Http.BadBody "secret decoder details")
                    |> Expect.equal "The server returned an invalid response."
        , test "opens an error dialog without changing unrelated state" <|
            \_ ->
                { infoDialog = Nothing, savedValue = 42 }
                    |> withMutationFailure (Http.BadStatus 400)
                    |> Expect.equal
                        { infoDialog = Just "The server rejected the request."
                        , savedValue = 42
                        }
        ]
