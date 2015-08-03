import Game.Spacewar.Controls
import Game.Spacewar.Backend.GL

main :: IO ()
main = do
    runBackend [player1, player2]
