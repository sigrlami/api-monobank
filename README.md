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

## Private
Information provided with authorization.

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
