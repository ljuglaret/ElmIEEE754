module  Main exposing (..)

import IEEE755 exposing (..)

import Markdown exposing(..)


import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { x : Maybe Float
    }

init : Model
init =
  Model Nothing


type Msg
  = I1 (Result String Float)
    
update : Msg -> Model -> Model
update msg model =
        case msg of
            I1 (Ok int0)  -> {model|x =  Just int0 }
            I1 (Err err ) -> {model|x = Nothing }
     
resFloat : String -> Result String Float 
resFloat s = Result.fromMaybe "nope" (String.toFloat s)
    



view : Model -> Html Msg
view model =
     
    div [ ][
        input [ type_ "String", placeholder " Saisie d un nombre ", onInput ( I1<<resFloat) ] []
        ,br[][]
        , calcul model.x        
        ]        

contenu1 : Html msg
contenu1 =
   Markdown.toHtml [class "contenu1"] """
### conversion en binaire
"""

contenu2 =
   Markdown.toHtml [class "contenu3"] """
### Exposant en base 10 puis 2
"""

contenu3 =
   Markdown.toHtml [class "contenu3"] """
### On obtient une liste dans laquelle le premier element
### est 0(si nombre de depart positif) ou 1 sinon , puis l exposant en binaire, puis la chaine de depart en   
### binaire sans le premier element, puis on complete par des 0.   
### Ce qui donne
"""

contenu4 =
   Markdown.toHtml [class "contenu3"] """

### Pour une meilleure lisibilite et une meilleure conversion   
### il faut separer la chaine obtenue en bloc de 4 elements
"""


calcul y = 
    case y of 
                Nothing -> text(" ")
                Just xm -> 
                            div[][
                                contenu1
                                ,text(Debug.toString (final xm)) -- OK
                                ,br[][]
                                ,contenu2
                                ,text( String.fromInt(exposant xm)++ " => " ++ Debug.toString(exposantEnBinaire xm) )
                                ,contenu3   -- exposant = OK , exposant en binaire = OK 
                                ,text (Debug.toString (List.concat(parBlocs (format xm))))
                                ,contenu4 -- parBlocs = Ok , format = OK
                                ,text (Debug.toString (parBlocs (format xm)))
                                ,br[][]
                                ,text(blocsHexa xm) -- BlocsHexa = Ok
                                ]
