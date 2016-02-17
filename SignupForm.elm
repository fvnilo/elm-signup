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

-- The update function receives an action that describes what
-- has to be done. It knows the logic to execute
-- the right update.
update action model =
    if action.actionType == "VALIDATE" then
        -- A let expression is similar to the one in Clojure.
        -- It creates local variables that cannot be seen outside.
        let
            url =
                "https://api.github.com/users/" ++ model.username

            usernameTakenAction =
                { actionType = "USERNAME_TAKEN", payload = "" }

            usernameAvailableAction =
                { actionType = "USERNAME_AVAILABLE", payload = "" }

            -- Http.get receives two arguments:
            -- 1. A Decoder
            -- 2. An url
            -- The succeed Decoder returns usernameTakenAction no matter what.
            request =
                Http.get (succeed usernameTakenAction) url

            -- When the task fails, we convert it in a task success since that means
            -- that the username is available.
            neverFailingRequest =
                Task.onError request (\err -> Task.succeed usernameAvailableAction)
        in
            ({ model | errors = getErrors model }, Effects.task neverFailingRequest)
    else if action.actionType == "SET_USERNAME" then
        ( { model | username = action.payload }, Effects.none )
    else if action.actionType == "SET_PASSWORD" then
        ( { model | password = action.payload }, Effects.none )
    else if action.actionType == "USERNAME_TAKEN" then
        ( withUsernameTaken True model, Effects.none )
    else if action.actionType == "USERNAME_AVAILABLE" then
        ( withUsernameTaken False model, Effects.none )
    else
        ( model, Effects.none )

withUsernameTaken isTaken model =
    let
        currentErrors =
            model.errors

        newErrors =
            { currentErrors | usernameTaken = isTaken }
    in
        { model | errors = newErrors }

-- The view function takes an action dispatcher and a model and renders a Form.
-- The action dispatcher is used to fire up actions.
-- As we type in the inputs, the action dispatcher fires actions that updates our model.
view actionDispatcher model =
    form
        [ id "signup-form" ]
        [ h1 [] [ text "Sensational Signup Form" ]
        , label [ for "username-field" ] [ text "username: " ]
        , input
            [ id "username-field"
            , type' "text"
            , value model.username
            , on "input" targetValue (\value -> Signal.message actionDispatcher { actionType = "SET_USERNAME", payload = value })
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.username ]
        , label [ for "password" ] [ text "password: " ]

        , input
            [ id "password-field"
            , type' "password"
            , value model.password
            , on "input" targetValue (\value -> Signal.message actionDispatcher { actionType = "SET_PASSWORD", payload = value })
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.password ]
        , div [ class "signup-button", onClick actionDispatcher { actionType = "VALIDATE", payload = "" } ] [ text "Sign Up!" ]
        , div [ class "validation-error" ] [ text (viewUsernameErrors model) ]
        ]
-- getErrors is the function that validates the model and
-- returns an object that describes the errors.
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

viewUsernameErrors model =
   if model.errors.usernameTaken then
       "That username is taken!"
   else
       model.errors.username

-- The initial errors (none).
initialErrors =
    { username = "", password = "", usernameTaken = False }

-- This is the initial model when starting the app.
initialModel =
     { username = "", password = "", errors = initialErrors }

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
