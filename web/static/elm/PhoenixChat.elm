module PhoenixChat where

import StartApp
import Task exposing (Task, andThen)
import Effects exposing (Effects, Never)
import Signal exposing (Signal, Mailbox)

import Html exposing (..)
import Html.Attributes exposing (class, id, href, type', value, attribute)
import Html.Events exposing (on)

import Json.Encode
import Json.Decode exposing ((:=))

import Phoenix.Socket exposing (Socket)
import Phoenix.Channel exposing (Channel)


(=>): a -> b -> (a, b)
(=>) = (,)


-- MODEL

type alias Message =
  { user: String
  , body: String
  }


type alias Model =
  { channel: Maybe Channel
  , messages: List Message
  , username: String
  , field: String
  }


init : (Model, Effects Action)
init =
  ( Model Nothing [] "" ""
  , connect "/socket" `andThen` join "rooms:lobby"
    |> Task.map Channel
    |> Effects.task
  )


-- UPDATE

type Action
  = NoOp
  | Channel Channel
  | NewMessage Message
  | Username String
  | UpdateField String
  | SubmitMessage


mailbox : Mailbox Action
mailbox =
  Signal.mailbox NoOp


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      ( model
      , Effects.none
      )

    Channel channel ->
      ( { model
          | channel = Just channel
        }
      , Effects.none
      )

    NewMessage message ->
      ( { model
          | messages = message :: model.messages
        }
      , Effects.none
      )

    Username string ->
      ( { model
          | username = string
        }
      , Effects.none
      )

    UpdateField string ->
      ( { model
          | field = string
        }
      , Effects.none
      )

    SubmitMessage ->
      case model.channel of
        Nothing ->
          (model, Effects.none)
        Just channel ->
          ( { model
              | field = ""
            }
          , Phoenix.Channel.push "new:msg" (encodeMessage model.username model.field) channel
            |> Task.map (always NoOp)
            |> Effects.task
          )


connect : String -> Task x Socket
connect endPoint =
  Phoenix.Socket.socket endPoint
  |> Phoenix.Socket.connect


join : String -> Socket -> Task x Channel
join topic socket =
  Phoenix.Channel.channel topic socket
  |> Phoenix.Channel.on "new:msg" newMessage
  |> Phoenix.Channel.join


newMessage : Json.Decode.Value -> Phoenix.Channel.Response x a
newMessage value =
  case Json.Decode.decodeValue messageDecoder value of
    Ok message ->
      Phoenix.Channel.SendMessage <| Signal.message mailbox.address <| NewMessage message
    Err reason ->
      Phoenix.Channel.Leave


messageDecoder : Json.Decode.Decoder Message
messageDecoder =
  Json.Decode.object2 Message
    ("user" := Json.Decode.string)
    ("body" := Json.Decode.string)


encodeMessage : String -> String -> Json.Decode.Value
encodeMessage user body =
  Json.Encode.object
    [ "user" => Json.Encode.string user
    , "body" => Json.Encode.string body
    ]

-- VIEW

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  on "keydown"
    (Json.Decode.customDecoder Html.Events.keyCode is13)
    (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


messageTemplate : Message -> Html
messageTemplate message =
  p []
    [ a [ href "#" ] [ text( "[" ++ message.user ++ "]")]
    , text(" " ++ message.body)
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [ id "messages", class "container" ] <| List.map messageTemplate <| List.reverse model.messages
    , div [ id "footer" ]
      [ div [ class "container" ]
        [ div [ class "row" ]
          [ div [ class "col-sm-2" ]
            [ div [ class "input-group" ]
              [ span [ class "input-group-addon" ] [ text "@" ]
              , input
                [ id "username"
                , type' "text"
                , class "form-control"
                , (attribute "placeholder" "username")
                , on "input" Html.Events.targetValue (Signal.message address << Username)
                ]
                []
              ]
            ]
          , div [ class "col-sm-10" ]
            [ input
              [ id "message-input"
              , class "form-control"
              , value model.field
              , on "input" Html.Events.targetValue (Signal.message address << UpdateField)
              , onEnter address SubmitMessage
              ]
              []
            ]
          ]
        ]
      ]
    ]

-- MAIN

app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [ mailbox.signal ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
