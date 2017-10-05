port module Main exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, pre, p)
import Html.Attributes exposing (class)
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


port store : SatelliteInfo -> Cmd msg


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
                ( { model | satelliteInfo = Just satelliteInfo }, store satelliteInfo )

        LoadInfo (Err a) ->
            Debug.crash (toString a)


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
        [ h2 [] [ text opportunity.time ]
        , h3 [] [ text ("For " ++ opportunity.duration ++ " peaking at " ++ opportunity.maximumElevation) ]
        , p [] [ text ("From " ++ opportunity.approach ++ " till " ++ opportunity.departure) ]
        ]


init : Maybe SatelliteInfo -> ( Model, Cmd Msg )
init flags =
    case flags of
        Nothing ->
            ( initialModel, getSatelliteInfo webtaskUrl )

        Just info ->
            ( { initialModel | satelliteInfo = flags }, Cmd.none )


main : Program (Maybe SatelliteInfo) Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = mainView
        }
