module MonobankApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { uName : String
    , uAccounts : List (Account)
    }

type alias Currency =
    { isoCode : String
    , isoNumericCode : Int
    , decimalDigits : Int
    , symbol : String
    , name : String
    }

type alias Account =
    { acId : String
    , acBalance : Int
    , acCreditLimit : Int
    , acCurrencyCode : Int
    , acCashbackType : String
    }

type alias Statement =
    { stId : String
    , stTime : String
    , stDescription : String
    , stMCC : String
    , stHold : Bool
    , stAmount : Int
    , stOperationAmount : Int
    , stCurrency : Int
    , stComissionRate : Int
    , stCashbackAmount : Int
    , balance : Int
    }

type alias CurrencyPair =
    { cpCurrencyCodeA : Currency
    , cpCurrencyCodeB : Currency
    , cpDate : Int
    , cpRateSell : Maybe (Float)
    , cpRateBuy : Maybe (Float)
    , cpRateCross : Maybe (Float)
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "uName" string
        |> required "uAccounts" (list decodeAccount)

decodeCurrency : Decoder Currency
decodeCurrency =
    decode Currency
        |> required "isoCode" string
        |> required "isoNumericCode" int
        |> required "decimalDigits" int
        |> required "symbol" string
        |> required "name" string

decodeAccount : Decoder Account
decodeAccount =
    decode Account
        |> required "acId" string
        |> required "acBalance" int
        |> required "acCreditLimit" int
        |> required "acCurrencyCode" int
        |> required "acCashbackType" string

decodeStatement : Decoder Statement
decodeStatement =
    decode Statement
        |> required "stId" string
        |> required "stTime" string
        |> required "stDescription" string
        |> required "stMCC" string
        |> required "stHold" bool
        |> required "stAmount" int
        |> required "stOperationAmount" int
        |> required "stCurrency" int
        |> required "stComissionRate" int
        |> required "stCashbackAmount" int
        |> required "balance" int

decodeCurrencyPair : Decoder CurrencyPair
decodeCurrencyPair =
    decode CurrencyPair
        |> required "cpCurrencyCodeA" decodeCurrency
        |> required "cpCurrencyCodeB" decodeCurrency
        |> required "cpDate" int
        |> required "cpRateSell" (maybe float)
        |> required "cpRateBuy" (maybe float)
        |> required "cpRateCross" (maybe float)

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