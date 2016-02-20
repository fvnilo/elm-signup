-- Module Declaration
module SignupForm where

-- Import everything from the Html and Html.Event
-- module into the current namespace
import Html exposing (..)
import Html.Events exposing (..)

-- Import specific functions from the Html.Attributes module.
import Html.Attributes exposing (id, type', for, value, class)

-- StartApp is a Module that wires eveything
-- neatly according to the elm architecture
import StartApp

-- Effects manages the side-effects by returning tasks.
import Effects

-- Http request module
import Http

-- The Task module that is like promises and callbacks.
-- Like promises: they can be chained.
-- Like callbacks: when you declare them, they do not do anything
-- until it is handed to something that can execute them.
import Task exposing (Task)

-- The Decoder used to always return the same value on sucxess.
import Json.Decode exposing (succeed)


-- MODEL
type alias Errors =
    { username: String, password: String, usernameTaken: Bool }

-- The initial errors (none).
initialErrors : Errors
initialErrors =
    { username = "", password = "", usernameTaken = False }



type alias Model =
    { username: String, password: String, errors: Errors }

-- This is the initial model when starting the app.
initialModel : Model
initialModel =
    { username = "", password = "", errors = initialErrors }


-- UPDATE
-- The Action Types
type Action
  = Validate
  | SetUserName String
  | SetPassword String
  | UserNameTaken
  | UserNameAvailable

-- The update function receives an action that describes what
-- has to be done. It knows the logic to execute
-- the right update.
update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
      Validate ->
        -- A let expression is similar to the one in Clojure.
        -- It creates local variables that cannot be seen outside.
        let
            url =
                "https://api.github.com/users/" ++ model.username

            -- Http.get receives two arguments:
            -- 1. A Decoder
            -- 2. An url
            -- The succeed Decoder returns usernameTakenAction no matter what.
            request =
                Http.get (succeed UserNameTaken) url

            -- When the task fails, we convert it in a task success since that means
            -- that the username is available.
            neverFailingRequest =
                Task.onError request (\err -> Task.succeed UserNameAvailable)
        in
            ( { model | errors = getErrors model }, Effects.task neverFailingRequest )

      SetUserName userName ->
        ( { model | username = userName }, Effects.none )

      SetPassword password ->
        ( { model | password = password }, Effects.none )

      UserNameTaken ->
        ( withUsernameTaken True model, Effects.none )

      UserNameAvailable ->
        ( withUsernameTaken False model, Effects.none )


withUsernameTaken : Bool -> Model -> Model
withUsernameTaken isTaken model =
    let
        currentErrors =
            model.errors

        newErrors =
            { currentErrors | usernameTaken = isTaken }
    in
        { model | errors = newErrors }

-- getErrors is the function that validates the model and
-- returns an object that describes the errors.
getErrors : Model -> Errors
getErrors model =
    { username =
        if model.username == "" then
            "Please enter a username!"
        else
            ""

    , password =
        if model.password == "" then
            "Please enter a password!"
        else
            ""
    , usernameTaken = model.errors.usernameTaken
    }


viewUsernameErrors : Model -> String
viewUsernameErrors model =
   if model.errors.usernameTaken then
       "That username is taken!"
   else
       model.errors.username


-- VIEW
-- The view function takes an action dispatcher and a model and renders a Form.
-- The action dispatcher is used to fire up actions.
-- As we type in the inputs, the action dispatcher fires actions that updates our model.
view : Signal.Address Action -> Model -> Html
view actionDispatcher model =
    form
        [ id "signup-form" ]
        [ h1 [] [ text "Sensational Signup Form" ]
        , label [ for "username-field" ] [ text "username: " ]
        , input
            [ id "username-field"
            , type' "text"
            , value model.username
            , on "input" targetValue (\value -> Signal.message actionDispatcher (SetUserName value))
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.username ]
        , label [ for "password" ] [ text "password: " ]

        , input
            [ id "password-field"
            , type' "password"
            , value model.password
            , on "input" targetValue (\value -> Signal.message actionDispatcher (SetPassword value))
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.password ]
        , div [ class "signup-button", onClick actionDispatcher Validate ] [ text "Sign Up!" ]
        , div [ class "validation-error" ] [ text (viewUsernameErrors model) ]
        ]


-- Wire the our app with the elm architecture.
app =
    StartApp.start
        { init = ( initialModel, Effects.none )
        , update = update
        , view = view
        , inputs = []
        }

main =
    app.html


-- StartApp packs the Tasks that come from the update
-- function but to run them, this must be added to the
-- end of the file.
port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
