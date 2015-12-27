module Config where
    data WindowConfig = WindowConfig {
                      initialPosition :: (Int, Int)
                    , initialSize :: (Int, Int)
                    , header :: String
                    }
