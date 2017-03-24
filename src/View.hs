{-# OPTIONS_GHC -Wno-missing-signatures #-}
module View where

import Graphics.UI.Threepenny.Core hiding (value)
import qualified Graphics.UI.Threepenny as UI

--------------------------------------------------------------------------------

button cl label = UI.button #. cl # set text label
buttonStop      = button "ui left  attached      small button" "Start"
buttonReset     = button "ui right attached grey small button" "Reset"
buttonGroup bts = UI.div #. "ui right floated right aligned column"
                # set children bts

display = UI.div #. "ui left floated left aligned column"
content = UI.div #. "ui two column grid"
segment = UI.div #. "ui middle aligned segment"

centerGrid :: [Element] -> UI Element
centerGrid ls = do
    gridContent <- UI.div #. "ui column"
        # set children ls
        # set style [("width", "60%")]
    UI.div #. "ui middle aligned center aligned grid"
        # set children [ gridContent ]
        # set style [("height", "100%")]

--------------------------------------------------------------------------------

