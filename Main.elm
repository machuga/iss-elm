module Main exposing (..)

import Html exposing (Html, div, text, h1, pre)


--import Date exposing (Date)

import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)


type Msg
    = NoOp
    | FetchInfo
    | LoadInfo (Result Http.Error SatelliteInfo)


webtaskUrl : String
webtaskUrl =
    "https://wt-8f9f6a577da77a9add9cadbb90e66b75-0.run.webtask.io/iss-tracker"


type alias SightingOpportunity =
    { title : String
    , description : String
    , date : String
    , time : String
    , duration : String
    , maximumElevation : String
    , approach : String
    , departure : String
    }


type alias SatelliteInfo =
    { url : String
    , description : String
    , items : List SightingOpportunity
    , expiresAt : Int
    }


type alias Model =
    { state : String
    , satelliteInfo : Maybe SatelliteInfo
    }


initialModel : Model
initialModel =
    { state = ""
    , satelliteInfo = Nothing
    }


satelliteInfoDecoder : Decoder SatelliteInfo
satelliteInfoDecoder =
    Decode.map4 SatelliteInfo
        (field "url" Decode.string)
        (field "title" Decode.string)
        (field "items" (Decode.list opportunityDecoder))
        (field "expiresAt" Decode.int)


opportunityDecoder : Decoder SightingOpportunity
opportunityDecoder =
    Decode.map8 SightingOpportunity
        (field "title" Decode.string)
        (field "description" Decode.string)
        (field "date" Decode.string)
        (field "time" Decode.string)
        (field "duration" Decode.string)
        (field "maximum_elevation" Decode.string)
        (field "approach" Decode.string)
        (field "departure" Decode.string)


getSatelliteInfo : String -> Cmd Msg
getSatelliteInfo url =
    let
        var =
            Debug.log ("HELLO")
    in
        Http.send LoadInfo (Http.get url satelliteInfoDecoder)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( initialModel, Cmd.none )

        FetchInfo ->
            ( initialModel, getSatelliteInfo webtaskUrl )

        LoadInfo (Ok satelliteInfo) ->
            let
                var =
                    Debug.log ("foo " ++ toString satelliteInfo)
            in
                ( { model | satelliteInfo = Just satelliteInfo }, Cmd.none )

        LoadInfo (Err a) ->
            Debug.crash "oh noes"


mainView : Model -> Html Msg
mainView model =
    div []
        [ h1 [] [ text "Erie ISS Sightings" ]
        , pre [] [ text (toString model) ]
        , div [] [ satelliteInfoView model.satelliteInfo ]
        ]


satelliteInfoView : Maybe SatelliteInfo -> Html msg
satelliteInfoView satelliteInfo =
    case satelliteInfo of
        Just satelliteInfo ->
            div [] (List.map opportunityView satelliteInfo.items)

        Nothing ->
            text ""


opportunityView : SightingOpportunity -> Html msg
opportunityView opportunity =
    div []
        [ h1 [] [ text opportunity.date ]
        , div [] [ text opportunity.time ]
        , div [] [ text opportunity.duration ]
        , div [] [ text opportunity.maximumElevation ]
        , div [] [ text opportunity.approach ]
        , div [] [ text opportunity.departure ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getSatelliteInfo webtaskUrl )
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = mainView
        }
