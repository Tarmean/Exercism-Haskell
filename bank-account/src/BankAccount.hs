module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where
import Control.Monad.STM
import Control.Concurrent.STM.TVar

type BankAccount = TVar (Maybe Integer)

closeAccount :: BankAccount -> IO ()
closeAccount a = atomically $ writeTVar a Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readTVarIO

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance a i = atomically $ modifyTVar a (fmap (+ i)) >> readTVar a
        

openAccount :: IO BankAccount
openAccount = atomically $ newTVar (Just 0)
