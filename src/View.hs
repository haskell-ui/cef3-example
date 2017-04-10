{-# OPTIONS_GHC -Wno-missing-signatures #-}
module View where

import Graphics.UI.Threepenny.Core hiding (value)
import qualified Graphics.UI.Threepenny as UI

--------------------------------------------------------------------------------

button cl label = UI.button #. cl # set text label
buttonStart     = button "ui left  attached      small button" "Start"
buttonReset     = button "ui right attached grey small button" "Reset"
buttonGroup bts = UI.div #. "ui right floated right aligned column"
                # set children bts

displayText txt = UI.h1 # set text txt
display dt = UI.div
    #. "ui left floated left aligned column"
    #+ [ element dt ]

content st cn = UI.div
    #. "ui two column grid"
    # set style st
    # set children cn
segment c = UI.div
    #. "ui middle aligned segment"
    # set children [ c ]


centerGrid :: [Element] -> UI Element
centerGrid ls = do
    gridContent <- UI.div #. "ui column"
        # set children ls
        # set style [("width", "60%")]
    UI.div #. "ui middle aligned center aligned grid"
        # set children [ gridContent ]
        # set style [("height", "100%")]

--------------------------------------------------------------------------------

