module GoogleCalendar exposing (Duration(..), eventEditUrl, EventDetails)

{-| Build URLs for editing prefilled Google Calendar events


# Rule

@docs Duration, eventEditUrl, EventDetails

-}

import Time
import Url exposing (Url)
import Url.Builder


{-| Create a URL for a Google Calendar Event.

    import Time
    import Url
    import GoogleCalendar exposing (Duration(..))

    Url.toString <| eventEditUrl Time.utc { title = "Some event", duration = NoDurationLetUserChoose, details = "Details about the event.\n\nMight contain newlines.", guests = [] }
    --> "https://calendar.google.com/calendar/u/0/r/eventedit?text=Some%20event&details=Details%20about%20the%20event.%0A%0AMight%20contain%20newlines."

    Url.toString <| eventEditUrl Time.utc { title = "Some event", duration = TimeSpan { from = Time.millisToPosix 1612508680856, to = Time.millisToPosix 1612508680856 }, details = "Details about the event.\n\nMight contain newlines.", guests = [] }
    --> "https://calendar.google.com/calendar/u/0/r/eventedit?text=Some%20event&details=Details%20about%20the%20event.%0A%0AMight%20contain%20newlines.&dates=20210205T070440%2F20210205T070440"

    Url.toString <| eventEditUrl Time.utc { title = "Some all-day event", duration = CustomDates "20210405/20210406", details = "Details about the event.\n\nMight contain newlines.", guests = [] }
    --> "https://calendar.google.com/calendar/u/0/r/eventedit?text=Some%20all-day%20event&details=Details%20about%20the%20event.%0A%0AMight%20contain%20newlines.&dates=20210405%2F20210406"

    Url.toString <| eventEditUrl Time.utc { title = "Some event with guests", duration = NoDurationLetUserChoose, details = "Details about the event.\n\nMight contain newlines.", guests = ["hello@example.com", "hi@example.com"] }
    --> "https://calendar.google.com/calendar/u/0/r/eventedit?text=Some%20event%20with%20guests&details=Details%20about%20the%20event.%0A%0AMight%20contain%20newlines.&add=hello%40example.com,hi%40example.com"

-}
eventEditUrl : Time.Zone -> EventDetails -> Url
eventEditUrl zone { title, duration, details, guests } =
    let
        formatTime time =
            let
                extractAndZeroPad extractFunction =
                    String.padLeft 2 '0' <| String.fromInt <| extractFunction zone time
            in
            -- YYYYMMDDTHHMMSS
            String.join ""
                [ String.fromInt <| Time.toYear zone time
                , extractAndZeroPad (\z t -> Time.toMonth z t |> monthNumber)
                , extractAndZeroPad Time.toDay
                , "T"
                , extractAndZeroPad Time.toHour
                , extractAndZeroPad Time.toMinute
                , extractAndZeroPad Time.toSecond
                ]

        queryValue =
            Url.Builder.string

        dates =
            case duration of
                NoDurationLetUserChoose ->
                    Nothing

                TimeSpan { from, to } ->
                    Just <| formatTime from ++ "/" ++ formatTime to

                CustomDates string ->
                    Just string

        guestsIfAny =
            case guests of
                [] ->
                    Nothing

                _ ->
                    Just <| String.join "," guests
    in
    { host = "calendar.google.com"
    , path = "/calendar/u/0/r/eventedit"
    , port_ = Nothing
    , protocol = Url.Https
    , query =
        Just <|
            String.dropLeft 1 <|
                Url.Builder.toQuery <|
                    List.filterMap identity
                        [ Just <| queryValue "text" title
                        , Just <| queryValue "details" details
                        , Maybe.map (queryValue "dates") dates
                        , Maybe.map (queryValue "add") guestsIfAny
                        ]
    , fragment = Nothing
    }


{-| Values needed for creating an event
Note that the details text might contain HTML, if you want e.g. images or anchor tags.
-}
type alias EventDetails =
    { title : String
    , duration : Duration
    , details : String
    , guests : List String
    }


{-| How long should the event last for?
Note: If you want an all-day event, use `CustomDates` with the requested date, a `/` and then the date of the following day.
Example: An all-day event for `2024-01-01` can be achieved like this:

    CustomDates "20240101/20240102"

-}
type Duration
    = NoDurationLetUserChoose
    | TimeSpan { from : Time.Posix, to : Time.Posix }
    | CustomDates String


{-| Yes, this exists in other libraries, but it's duplicated here to minimize the number of dependencies
-}
monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
