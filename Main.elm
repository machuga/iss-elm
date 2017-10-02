module Main exposing (..)

import Html exposing (Html, div, text, h1, pre, dl, dt, dd)
import Html.Attributes exposing (class)


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


type alias SightingDayInfo =
    ( String, List SightingOpportunity )


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


sortOpportunitiesByDay : List SightingOpportunity -> List SightingDayInfo
sortOpportunitiesByDay opportunities =
    let
        forDate date =
            List.filter (\o -> o.date == date) opportunities

        appendUnique : String -> List String -> List String
        appendUnique date list =
            if List.member date list then
                list
            else
                date :: list
    in
        opportunities
            |> List.map .date
            |> List.foldr appendUnique []
            |> List.map (\date -> ( date, forDate date ))


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
    div [] [ satelliteInfoView model.satelliteInfo ]


satelliteInfoView : Maybe SatelliteInfo -> Html msg
satelliteInfoView satelliteInfo =
    case satelliteInfo of
        Just satelliteInfo ->
            let
                groupedOpps =
                    sortOpportunitiesByDay satelliteInfo.items
            in
                div [] (List.map sightingDayView groupedOpps)

        Nothing ->
            text ""


sightingDayView : SightingDayInfo -> Html msg
sightingDayView ( date, opportunities ) =
    div [ class "sighting-day" ]
        [ h1 [] [ text date ]
        , div
            [ class "opportunities" ]
            (List.map opportunityView opportunities)
        ]


opportunityView : SightingOpportunity -> Html msg
opportunityView opportunity =
    div [ class "opportunity" ]
        [ dl []
            [ dt [] [ text "Start Time" ]
            , dd [] [ text opportunity.time ]
            , dt [] [ text "Duration" ]
            , dd [] [ text opportunity.duration ]
            , dt [] [ text "Maximum Elevation" ]
            , dd [] [ text opportunity.maximumElevation ]
            , dt [] [ text "Approach" ]
            , dd [] [ text opportunity.approach ]
            , dt [] [ text "Departure" ]
            , dd [] [ text opportunity.departure ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getSatelliteInfo webtaskUrl )
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = mainView
        }
