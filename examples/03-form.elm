import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validation: Bool
  }


model : Model
model =
  Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }

    Submit ->
      { model | validation = True }

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type_ "number", placeholder "Age", onInput Age ] []
    , button [ onClick Submit ] [ text "submit" ]
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  if not model.validation then
    div [] [ text "" ]
  else
      let
        (color, messages) =
            validate model
      in
        div [ style [("color", color)] ] [ viewMessages messages ]


viewMessages : List String -> Html msg
viewMessages messages =
    ul []
        (List.map
            (\m -> li [] [ text m ])
            messages
        )

activeValidations : List (Model -> Maybe String)
activeValidations =
    [ identicalPasswords
    , checkLength
    , checkLower
    , checkUpper
    , checkAgeIsNumber
    ]


validate : Model -> (String, List String)
validate model =
    let
        errors =
            List.filterMap
                (\v -> v model)
                activeValidations
    in
        if List.isEmpty errors then
            ("green", ["OK"])
        else
            ("red", errors)

identicalPasswords : Model -> Maybe String
identicalPasswords model =
    if model.password == model.passwordAgain then
        Nothing
    else
        Just "passwords should be identical"


checkAgeIsNumber : Model -> Maybe String
checkAgeIsNumber model =
    case String.toInt model.age of
        Err msg ->
            Just "age should be a number"

        _ ->
            Nothing



checkLength : Model -> Maybe String
checkLength model =
    if String.length model.password < 8 then
        Just "passwords should at least be with length 8"
    else
        Nothing

checkDigit : Model -> Maybe String
checkDigit model =
    checkString Char.isDigit model.password "passwords should contains at least one digit"


checkLower : Model -> Maybe String
checkLower model =
    checkString Char.isLower model.password "passwords should contains a LowerCase letter"


checkUpper : Model -> Maybe String
checkUpper model =
    checkString Char.isUpper model.password "passwords should contains an UpperCase letter"


checkString : (Char -> Bool) -> String -> String -> Maybe String
checkString check s error =
    if String.any check s then
        Nothing
    else
        Just error