module Content exposing (..)

import Element exposing (Color, rgb255)


confirmationTriggerText : String
confirmationTriggerText =
    "YES"


explanationText : String
explanationText =
    """
# Instructions
**Make sure that you know what you are doing before using this page,
  read the explanations below if needed.**

To securely lock the servers in your project/mission in case of an emergency
  or to prepare them for transport, follow the following steps:
1. **Uncheck "Test mode"**
1. Type "YES" in the text box to confirm
1. Press enter or click the "Go!" button

# Explanation

This page allows you to securely lock **all** the servers in your project
with a single click.
This allows you to quickly secure all data contained on these servers in case
of any serious emergency.

Please note that all services hosted by these servers (Unifield, EMR, DHIS, etc.)
will **stop working** and an intervention from HQ IT will be required to re-enable them.

No data will be lost, but the data will not be readable anymore.

# Try it out

You can safely try out this procedure by selecting "Test mode".
In test mode, everything will look as if we are locking the servers during
a real emergency situation, but we will not actually lock any servers.
This allows you to try out the nixos server lock button and become familiar with
its functioning, without disrupting any services.
A bit like an evacuation exercise.
"""


progressText : String
progressText =
    """
This page shows you the progress of the locking procedure.

Locking happens in two steps:
1. we send the command to every server to perform the actual locking; and
1. we verify that the server has effectively been securely locked.

The first step should be very fast, a matter of seconds.
The second step can take several minutes because the server will be rebooted
first. If you are in a hurry, **you do not need to wait for the verification**,
if all the servers have been locked (you can see this by checking the first counter),
then you should be good to go.
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
