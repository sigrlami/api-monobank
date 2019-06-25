# Monobank Api Client

Haskell client for MonoBank API services (https://api.monobank.ua/docs/)

![Monobank Api](assets/logo.png "Monobank logo")

1. [Introduction](#introduction)
2. [Public API](#public)
3. [Private API](#private)
4. [Building library](#building)

## Introduction

API for extracting information and personal account status. To grant access, you must pass the authorization in your personal office at https://api.monobank.ua/ and get a token for personal use.

If you have questions about the operation of the API, we invite to the community in the [Telegram channel](https://t.me/joinchat/FiAEWhDf-QzTqM4wzEtffw).

If you have a service or application and you want to centrally join the API for customer service, you need to connect to a [corporate](https://api.monobank.ua/docs/corporate.html) API that has more features.

This will allow monobank clients to log in to your service (for example, in a financial manager) to provide information about the status of an account or statements.

In the event of the exploitation of this API as corporate, the bank reserves the right to impose sanctions on the company.

## Public
General information provided without authorization.

| # |Path           | Type | Params |  Description         | Response | Notes|
|---|---------------|------|--------|----------------------|----------|------|
| 1 | /bank/currency| GET  |       | Get a basic list of monobank currency rates. Information is cached and updated no more than once every 5 minutes. | JSON | Response will be a json array with objects of 2 types


```json
   {
    "currencyCodeA": 978,
    "currencyCodeB": 840,
    "date": 1561426807,
    "rateBuy": 1.1249,
    "rateSell": 1.1368
  },
  {
    "currencyCodeA": 826,
    "currencyCodeB": 980,
    "date": 1561461569,
    "rateCross": 33.2857
  },
```
note the absence of `rateBuy`, `rateSell` in a second example.Inside client system automatically convert `int` base date into normalized `UTCTime` format.


## Private
Information provided with authorization.

| # |Path                  | Type | Params                        |  Description         | Response | Notes|
|---|----------------------|------|-------------------------------| ---------------------|----------|------|
| 1 | /personal/client-info| GET  |                               | Obtaining information about the client and the list of his accounts. Limit on the use of the function no more than 1 time in 60 seconds.| JSON |
|   |                      |      | `X-Token`: string ; in header | Token for personal access to the API | |
| 2 | /personal/statement  | GET  |  /{account}/{from}/{to}       | Receive an extract for the time from {to} to {to} time in seconds Unix time format. The maximum time for which it is possible to extract an extract is 31 days (2678400 seconds) Limit on the use of the function no more than 1 time in 60 seconds. | JSON |
|   |                      |      | `X-Token` string; in `header` | Token for personal access to the API | |
|   |                      |      | `account` string; in `path`   | Account ID from the Statement list list or 0 is a default account.
|   |                      |      | `from` string; in `path`      | Start of the excerpt time
|   |                      |      | `to` string; in `path`        | End time of the excerpt (if not, the current time will be used)

## Building

- `src` - contains actual API that can be used in 3rd part apps
- `app` - executable for CLI app that can be used to view current value or run continuosly

Just build repository with `stack build` and use following

```
$ stack exec -- mnb-app
```

or simply `mnb-app` if you used `stack install` command

```
$ mnb-app
```
that will bring following output
