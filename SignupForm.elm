-- Module Declaration
module SignupForm where

-- Import everything from the Html and Html.Event
-- module into the current namespace
import Html exposing (..)

import Html.Events exposing (..)

-- Import specific functions from the Html.Attributes module.
import Html.Attributes exposing (id, type', for, value, class)

-- The view function takes a model and renders a Form.
view model =
    form
        [ id "signup-form" ]
        [ h1 [] [ text "Sensational Signup Form" ]
        , label [ for "username-field" ] [ text "username: " ]
        , input [ id "username-field", type' "text", value model.username ] []
        , label [ for "password" ] [ text "password: " ]
        , input [ id "password-field", type' "password", value model.password ] []
        , div [ class "signup-button" ] [ text "Sign Up!" ]
        ]


-- The Main function consists of calling the view function with an empty module.
main =
    view { username = "", password = "" }
