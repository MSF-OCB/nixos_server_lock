module Content exposing (..)

import Element exposing (Color, rgb255)


confirmationTriggerText : String
confirmationTriggerText =
    "YES"


explanationText : String
explanationText =
    """
# What is this page

This page allows you to securely lock **all** the servers in your project with a single click.
This allows you to quickly secure all data contained on these servers in case of any serious emergency.

Please note that all services hosted by these servers (Unifield, EMR, DHIS, etc.) will **stop working**
and an intervention from HQ IT will be required to re-enable them.

No data will be lost, but the data will not be readable anymore.

## Test mode

You can try out this procedure by selecting "Test mode".
In test mode, we will proceed as if we are locking the servers during a real emergency situation,
but no servers will actually be disabled.
This allows you to try out the panic button without actually
"""


progressText : String
progressText =
    """
"""


lockingFailedText : String
lockingFailedText =
    "This server could not be locked, please contact your IT staff."


black : Color
black =
    rgb255 0 0 0


grey : Color
grey =
    rgb255 234 237 243


darkGrey : Color
darkGrey =
    rgb255 224 227 233


white : Color
white =
    rgb255 255 255 255


red : Color
red =
    rgb255 238 0 0


backgroundColor : Color
backgroundColor =
    white


buttonBackgroundColor : Color
buttonBackgroundColor =
    white


fontColor : Color
fontColor =
    black


msfLogoPath : List String
msfLogoPath =
    [ "static", "assets", "azg-logo.svg" ]
