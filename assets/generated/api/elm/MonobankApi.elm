module MonobankApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


getBankCurrency : Http.Request (List (CurrencyPair))
getBankCurrency =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "bank"
                , "currency"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeCurrencyPair)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getPersonalClientinfo : Maybe (String) -> Http.Request (User)
getPersonalClientinfo header_X_Token =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "X-Token") header_X_Token
                ]
        , url =
            String.join "/"
                [ ""
                , "personal"
                , "client-info"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getPersonalStatementByAccountByFromByTo : Maybe (String) -> String -> String -> String -> Http.Request (Statement)
getPersonalStatementByAccountByFromByTo header_X_Token capture_account capture_from capture_to =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "X-Token") header_X_Token
                ]
        , url =
            String.join "/"
                [ ""
                , "personal"
                , "statement"
                , capture_account |> Http.encodeUri
                , capture_from |> Http.encodeUri
                , capture_to |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeStatement
        , timeout =
            Nothing
        , withCredentials =
            False
        }