# google-calendar-url

Build URLs for creating events in Google Calendar with prefilled contents.

## Example usage

```elm
eventEditUrl Time.utc
    { title = "Some all-day event"
    , duration = CustomDates "20210405/20210406"
    , details = "Details about the event.\n\nMight contain newlines."
    , guests = ["hello@example.com"]
    }
```

will create this link:
https://calendar.google.com/calendar/u/0/r/eventedit?text=Some%20all-day%20event&details=Details%20about%20the%20event.%0A%0AMight%20contain%20newlines.&dates=20210405%2F20210406&add=hello%40example.com

Try it yourself (if you're logged in with a Google account).

Follow the link to find a new Google Calendar event prefilled with title, duration and details like this:

![Screenshot of example event](example_screenshot.png)
