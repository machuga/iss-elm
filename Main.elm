port module Main exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, pre, p, span, select, option)
import Html.Attributes exposing (class)
import Http
import Debug
import Regex exposing (Regex, HowMany)
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


type alias TimeTuple =
    ( Int, TimeOfDay )


type TimeOfDay
    = Morning
    | Night


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


groupOpportunitiesByDay : List SightingOpportunity -> List SightingDayInfo
groupOpportunitiesByDay opportunities =
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


stringFromJust : Maybe String -> String
stringFromJust value =
    case value of
        Just a ->
            a

        Nothing ->
            ""


hourTo24 : String -> TimeOfDay -> Int
hourTo24 hourStr timeOfDay =
    let
        hour =
            Result.withDefault 0 (String.toInt hourStr)
    in
        case timeOfDay of
            Morning ->
                if hour == 12 then
                    0
                else
                    hour

            Night ->
                if hour == 12 then
                    12
                else
                    hour + 12


timeStringToTuple : String -> ( String, String, String )
timeStringToTuple time =
    let
        timeRegex =
            Regex.regex "(\\d?\\d):(\\d\\d)\\s(AM|PM)"

        timeChunks =
            (Regex.find (Regex.AtMost 1) timeRegex time)
                |> List.concatMap .submatches
                |> List.map stringFromJust
    in
        case timeChunks of
            [] ->
                ( "", "", "" )

            hourStr :: [] ->
                ( hourStr, "", "" )

            hourStr :: minuteStr :: [] ->
                ( hourStr, minuteStr, "" )

            hourStr :: minuteStr :: meridiem :: _ ->
                ( hourStr, minuteStr, meridiem )


meridiemToTimeOfDay : String -> TimeOfDay
meridiemToTimeOfDay meridiem =
    if meridiem == "PM" then
        Night
    else
        Morning


splitTime : String -> TimeTuple
splitTime time =
    let
        ( hourStr, minuteStr, meridiem ) =
            timeStringToTuple time

        timeOfDay =
            meridiemToTimeOfDay meridiem

        hour =
            (hourTo24 hourStr timeOfDay) * 100

        minute =
            Result.withDefault 0 (String.toInt minuteStr)
    in
        ( hour + minute, timeOfDay )



-- Update


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
    div []
        [ --filterView model
          satelliteInfoView model.satelliteInfo
        ]


filterView : Model -> Html Msg
filterView model =
    div []
        [ p []
            [ text "Filter"
            , select []
                [ option [] [ text "Mornings" ]
                , option [] [ text "Nights" ]
                , option [] [ text "Family Friendly" ]
                ]
            ]
        ]


satelliteInfoView : Maybe SatelliteInfo -> Html msg
satelliteInfoView satelliteInfo =
    case satelliteInfo of
        Just satelliteInfo ->
            let
                groupedOpps =
                    groupOpportunitiesByDay satelliteInfo.items
            in
                div [] (List.map sightingDayView groupedOpps)

        Nothing ->
            text ""


sightingDayView : SightingDayInfo -> Html msg
sightingDayView ( date, opportunities ) =
    div [ class "sighting-day" ]
        [ h1 [ class "sighting-date" ] [ text date ]
        , div
            [ class "opportunities" ]
            (List.map opportunityView opportunities)
        ]


opportunityView : SightingOpportunity -> Html msg
opportunityView opportunity =
    let
        ( time, timeOfDay ) =
            splitTime opportunity.time
    in
        div [ class ("opportunity " ++ (toString timeOfDay |> String.toLower)) ]
            [ h2 [] [ text (opportunity.time ++ (highElevationText opportunity.maximumElevation) ++ (idealTimeText time)) ]
            , h3 [] [ text ("For " ++ opportunity.duration ++ " peaking at " ++ opportunity.maximumElevation) ]
            , p [] [ text ("From " ++ opportunity.approach ++ " till " ++ opportunity.departure) ]
            ]


highElevationText : String -> String
highElevationText elevationStr =
    let
        elevation =
            elevationStr |> String.dropRight 1 |> String.toInt |> Result.withDefault 0
    in
        if elevation > 44 then
            "★"
        else
            ""


idealTimeText : Int -> String
idealTimeText time =
    let
        x =
            Debug.log "time" time
    in
        if time > 700 && time <= 1940 then
            "👨\x200D👩\x200D👧\x200D👧"
        else
            ""


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
