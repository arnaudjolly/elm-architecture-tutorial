import Html exposing (Html, div, button)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
    { time : Time
    , on : Bool
    }


init : (Model, Cmd Msg)
init =
  (Model 0 True, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | ToggleSubs


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({model | time = newTime }, Cmd.none)

    ToggleSubs ->
      ({model | on = not model.on}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.on then
        Time.every second Tick
    else
        Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  let
    (sHandX, sHandY) =
        handPosition 40 Time.inMinutes model.time

    (mHandX, mHandY) =
        handPosition 30 Time.inHours model.time

    (hHandX, hHandY) =
        handPosition 20 inHalfDays model.time

  in
    div []
     [ button [ onClick ToggleSubs ] [ text "Pause Time" ]
     , svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , line [ x1 "50", y1 "50", x2 sHandX, y2 sHandY, stroke "yellow" ] []
        , line [ x1 "50", y1 "50", x2 mHandX, y2 mHandY, stroke "red" ] []
        , line [ x1 "50", y1 "50", x2 hHandX, y2 hHandY, stroke "pink" ] []
        ]
     ]

{- number of turns for the hour hand -}
inHalfDays : Time -> Float
inHalfDays t =
    {- add 2 hours since i'm on a GMT+2 place -}
    (t + 2 * Time.hour) / (12 * Time.hour)

handPosition : Float -> (Time -> Float) -> Time ->  (String, String)
handPosition handLength timeConverter time =
    let
        {- minus 1/4 turn to make 0h be at top -}
        angle =
            turns (timeConverter time) - turns 0.25

        handX =
            toString (50 + handLength * cos angle)

        handY =
            toString (50 + handLength * sin angle)
    in
        (handX, handY)