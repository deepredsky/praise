port module Main exposing (Model, Msg(..), emptyModel, infoFooter, init, main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Random exposing (Generator)


get : Int -> List a -> Maybe a
get index list =
    if index < 0 then
        Nothing

    else
        case List.drop index list of
            [] ->
                Nothing

            x :: xs ->
                Just x


selectWithDefault : a -> List a -> Generator a
selectWithDefault defaultValue list =
    Random.map (Maybe.withDefault defaultValue) (select list)


select : List a -> Generator (Maybe a)
select list =
    Random.map (\index -> get index list)
        (Random.int 0 (List.length list - 1))


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Random Praise Phrases", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL
-- The full application state of our todo app.


type alias Model =
    String


emptyModel : Model
emptyModel =
    "opp"


data : List Model
data =
    [ "Great!"
    , "Phenomenal!"
    , "Superb!"
    , "Cool!"
    , "Out Of Sight!"
    , "Excellent!"
    , "Unbelievable Work!"
    , "Two Thumbs Up!"
    , "You've Got It!"
    , "Way To Go!"
    , "Terrific!"
    , "Outstanding Performance!"
    , "You've Outdone Yourself!"
    , "Marvelous!"
    , "Your Help Counts!"
    , "Amazing Effort!"
    , "Bravo!"
    , "Exceptional!"
    , "Breathtaking!"
    , "Wonderful!"
    , "You're Special!"
    , "Keep Up The Good Work!"
    , "First Rate Work!"
    , "Fantastic Work!"
    , "You Should Be Proud!"
    , "I Knew You Had It In You!"
    , "Very Good!"
    , "Stupendous!"
    , "Sensational!"
    , "A+ Work!"
    , "What An Imagination!"
    , "Awesome!"
    , "You're A Great Example For Others!"
    , "You Made It Happen!"
    , "You're A Real Trooper!"
    , "It Couldn't Be Better!"
    , "Good For You!"
    , "You're A Good Sport!"
    , "You Made The Difference!"
    , "Take A Bow!"
    , "Super Job!"
    , "You're Unique!"
    , "It's Everything I Hoped For!"
    , "How Thoughtful Of You!"
    , "Nice Going!"
    , "You're A Class Act!"
    , "Well Done!"
    , "You're Inspiring!"
    , "How Artistic!"
    , "You Go The Extra Mile!"
    , "Hooray For You!"
    , "Great Answer!"
    , "You Deserve A Hug!"
    , "High Five!"
    , "Extra Special Work!"
    , "Wow!"
    , "You're Getting Better!"
    , "You're Tops!"
    , "You're Amazing!"
    , "What A Great Idea!"
    , "You Figured It Out"
    , "You've Got What It Takes!"
    , "You're Neat!"
    , "You're A Joy!"
    , "You're A Shining Star!"
    , "Spectacular Work!"
    , "You're #1!"
    , "You Tried Hard!"
    , "The Time You Put In Really Shows!"
    , "Remarkable!"
    , "Far Out!"
    , "How Extraordinary!"
    , "You're A Winner!"
    , "You Came Through!"
    , "That's Incredible!"
    , "5 Star Work!"
    , "You're Super!"
    , "You Can Do It!"
    , "You're The Greatest!"
    , "Sweet!"
    , "Great Effort!"
    , "How Original!"
    , "What A Genius!"
    , "You're A Natural!"
    , "Very Brave!"
    , "You're A Pleasure To Know!"
    , "Way To Go!"
    , "You're Sharp!"
    , "Congratulations!"
    , "I'm Proud Of You!"
    , "Thank You For Caring!"
    , "I'm Impressed!"
    , "You're Very Talented!"
    , "Great Discovery!"
    , "You're A Champ!"
    , "Right On!"
    , "You're So Kind!"
    , "Thanks For Helping!"
    , "You're A-OK!"
    , "Magnificent!"
    , "You've Earned My Respect!"
    , "You've Made Progress!"
    , "Outstanding Effort!"
    , "Neat Work!"
    , "I Love It!"
    , "Beautiful!"
    , "Clever!"
    , "Brilliant!"
    , "That's Perfect!"
    , "Right On!"
    , "Your Best Work!"
    , "Expressive!"
    , "You've Improved!"
    , "Keep It Up!"
    , "Nice One!"
    , "Wicked!"
    , "Incomparable!"
    , "Incredible!"
    , "I Appreciate Your Help!"
    , "Good Leadership!"
    , "Great Job!"
    , "Stunning!"
    , "You Rule!"
    , "That's Very Kind!"
    , "Keep On Trying!"
    , "You Make Me Smile!"
    , "You Rock!"
    , "You're An Angel!"
    , "That's The Way!"
    , "Good For You!"
    , "A Job Well Done!"
    , "You're A Good Friend!"
    , "Way To Use Your Head!"
    , "Radical!"
    , "Alright!"
    , "You're Very Patient!"
    , "Wonderful!"
    , "Way To Be Responsible!"
    , "Getting Better All The Time!"
    , "Worthy Of An Oscar!"
    , "Better Than Ever!"
    , "Super Duper!"
    , "Great Dedication!"
    , "Top Notch!"
    , "Hats Off To You!"
    , "Spectacular!"
    , "Good Try!"
    , "Very Courageous!"
    , "I Like It!"
    , "Great Enthusiasm!"
    , "Lovely!"
    , "Very Resourceful!"
    , "It's A Masterpiece!"
    ]


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = GetRandomPraise
    | NewPraise String


randomPraise : Random.Generator String
randomPraise =
    selectWithDefault "ok" data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPraise praise ->
            ( praise, Cmd.none )

        GetRandomPraise ->
            ( model, Random.generate NewPraise randomPraise )


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            []
            [ text model ]
        , div
            []
            [ button [ onClick GetRandomPraise ] [ text "Click Me!" ] ]
        , infoFooter
        ]



-- VIEW ALL ENTRIES


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
