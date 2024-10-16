main :: IO ()
main = do
    let names = ["Debdutta", "Padma", "Summer", "Kaustav", "Ole"]
    mapM_ (\name -> putStrLn ("Hello " ++ name ++ "!")) names
