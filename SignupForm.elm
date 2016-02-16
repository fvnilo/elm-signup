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

update action model =
    if action.actionType == "VALIDATE" then
        ( { model | errors = getErrors model }, Effects.none )
    else
        ( model, Effects.none )

-- The view function takes an action dispatcher and a model and renders a Form.
-- The action dispatcher is used to fire up actions.
view actionDispatcher model =
    form
        [ id "signup-form" ]
        [ h1 [] [ text "Sensational Signup Form" ]
        , label [ for "username-field" ] [ text "username: " ]
        , input [ id "username-field", type' "text", value model.username ] []

        , div [ class "validation-error" ] [ text model.errors.username ]
        , label [ for "password" ] [ text "password: " ]
        , input [ id "password-field", type' "password", value model.password ] []

        , div [ class "validation-error" ] [ text model.errors.password ]
        , div [ class "signup-button", onClick actionDispatcher { actionType = "VALIDATE", payload = "" } ] [ text "Sign Up!" ]
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
    }

-- The initial errors (none).
initialErrors =
    { username = "", password = "" }

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
