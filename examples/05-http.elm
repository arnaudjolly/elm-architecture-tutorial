import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode



main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  , error : String
  , availableTopics : List String
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" "" topics
  , getRandomGif topic
  )


topics : List String
topics = [ "cats", "cars", "dogs", "christmas", "easter"]


-- UPDATE


type Msg
  = MorePlease
  | Topic String
  | NewGif (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)

    Topic topic ->
      ({model | topic = topic}, getRandomGif topic)

    NewGif (Ok newUrl) ->
      ({model | gifUrl = newUrl, error = "" }, Cmd.none)

    NewGif (Err err) ->
        let
            errmsg =
                case err of
                    BadUrl m ->
                        m

                    Timeout ->
                        "timeout !"

                    NetworkError ->
                        "network error!"

                    BadStatus response ->
                        response.status.message

                    BadPayload s response ->
                        s ++ " with body: " ++ response.body
        in
            ({ model | error = errmsg } , Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [
      select [ onInput Topic, value model.topic ] (renderOptions model)
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , span [ style [ ("color", "red") ] ] [ text model.error ]
    , br [] []
    , img [src model.gifUrl] []
    ]

renderOptions : Model -> List (Html Msg)
renderOptions model =
    let
        renderOption opt =
            option [ selected (opt == model.topic), value opt ] [ text opt ]
    in
        List.map renderOption model.availableTopics

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
