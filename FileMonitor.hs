import System.Directory
import System.IO
import Filesystem
import System.FSNotify

main :: IO ()
main = do
  let dir = "/home" -- ^ to set the path you want to monitor
  setCurrentDirectory dir
  wd <- getWorkingDirectory
  print wd
  man <- startManager
  watchTree man wd (const True) $ \event ->
    case event of
      Modified  dir' _ -> putStrLn $ "Modified: " ++ show dir'
      Added     dir' _ -> putStrLn $ "Added: " ++ show dir'
      Removed   dir' _ -> putStrLn $ "Removed: " ++ show dir'

  print "press retrun to stop"
  getLine
  print "watching stopped, press retrun to exit"
  stopManager man
  getLine
  return ()
