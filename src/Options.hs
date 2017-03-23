module Options where

import Options.Applicative

data Options = Options
   { port           :: Int
   }

optParser :: Parser Options
optParser = Options
    <$> option auto optPort
    where
    optPort = mconcat $
        [ short 'p'
        , long "port"
        , showDefault
        , value 8099
        , metavar "INT"
        , help "Server port"
        ]

getOptions :: IO Options
getOptions = execParser opts
    where
    opts = info (optParser <**> helper) $ mconcat
         [ fullDesc
         , progDesc "Starts the timer server"
         , header "CountDown - timer program"
         ]
