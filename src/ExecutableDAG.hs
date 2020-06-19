module ExecutableDAG 
    (main, diverge)
where

import Control.Concurrent (forkIO)
import Control.Monad ((>=>))

diverge :: [a -> IO ()] -> a -> IO ()
diverge [] _ = pure ()
diverge (f: fs) a = forkIO (f a) >> diverge fs a


resize, convert :: String -> IO String
resize  path = path <> "-resized"   <$ putStrLn ("Resizing an image: " <> path)
convert path = path <> "-converted" <$ putStrLn ("Converting an image to png: " <> path)

save :: String -> IO ()
save path = putStrLn $ "Saving image " <> " to file " <> path <> "-saved"

resizeAndConvert = diverge [resize >=> save, convert >=> save]
app = diverge [resizeAndConvert, save] -- save original and proceed with resizing and converting in parallel

main = app $ "hello.jpg"

