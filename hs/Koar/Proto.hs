-- extensions {{{
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
-- }}}

-- exports {{{
module Koar.Proto

    -- messages.
    ( Message(..)
    , Head(..)

    -- the direct monad.
    , DirectT()
    , runDirectT
    , runDirectT'
    , noReply
    , withReply
    , pushReply
    , pushReply'
    , pullMsg
    , flushMsgs

    ) where
-- }}}

-- imports {{{
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy as LB
import           Data.ByteString.Lazy (ByteString)
import           Data.Int
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Data.Monoid
import           Data.Word
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString.Lazy
-- }}}

-- messages {{{

data Message = Message
    { msgHead :: !Head
    , msgBody :: ByteString
    }

data Head
    = Msg !Word32
    | Reply !Word32

-- }}}

-- (un)marshalling {{{

type Marshall = Builder
type Unmarshall a = ByteString -> Maybe a

marshall :: Marshall -> ByteString
marshall = toLazyByteString

unmarshall :: Unmarshall a -> ByteString -> Maybe a
unmarshall = ($)

mWord32 :: Word32 -> Marshall
mWord32 = word32BE

umWord32 :: Unmarshall Word32
umWord32 b
    | LB.length b /= 4  =  Nothing
    | otherwise = Just $ LB.foldl' f (0 :: Word32) b
  where
    f z n = (z `shiftL` 8) .|. (toEnum . fromEnum $ n)

mHead :: Head -> Marshall
mHead h = mWord32 $ case h of
    Msg n -> n `shiftL` 1
    Reply n -> (n `shiftL` 1) .|. 1

umHead :: Unmarshall Head
umHead b = fmap f $ umWord32 b
  where
    f w = case w .&. 1 of
        0 -> Msg $ w `shiftR` 1
        1 -> Reply $ w `shiftR` 1

mPfx :: Head -> Int64 -> Marshall
mPfx h len = mHead h <> mWord32 (fromIntegral len)

umPfx :: Unmarshall (Head, Int64)
umPfx b
    | LB.length b /= 8 = Nothing
    | otherwise = do
        h <- unmarshall umHead $ LB.take 4 b
        len <- unmarshall umWord32 $ LB.drop 4 b
        return (h, fromIntegral len)

-- }}}

-- IO with sockets {{{

sendMsgs :: Socket -> [Message] -> IO ()
sendMsgs sock = sendAll sock . LB.concat . concatMap (\m ->
    [ marshall $ mPfx (msgHead m) (LB.length $ msgBody m)
    , msgBody m
    ])

recvMsg :: Socket -> IO Message
recvMsg sock = do
    Just (hd, len) <- unmarshall umPfx `liftM` recv sock 8
    body <- recv sock $ fromIntegral len
    if LB.length body /= len
        then fail "truncated message"
        else return $ Message hd body

-- }}}

-- direct monad {{{

newtype DirectT m a = DirectT
    { unDirectT :: ReaderT Socket
                    (StateT (PeerState m) m) a
    }
  deriving (Functor, Monad)

data PeerState m = PeerState
    { psNextSerial :: Word32
    , psHooks :: IntMap (ByteString -> DirectT m ())
    , psBuffer :: [Message]
    }

runDirectT' :: (MonadIO m) => Socket -> DirectT m a -> m a
runDirectT' sock x = evalStateT (runReaderT (unDirectT x) sock) $ PeerState 1 IM.empty []

runDirectT :: (MonadIO m) => HostName -> PortNumber -> DirectT m a -> m a
runDirectT host port action = do
    let hints = defaultHints { addrFlags = [AI_CANONNAME], addrSocketType = Stream }
    addr:_ <- liftIO $ getAddrInfo (Just hints) (Just host) Nothing
    sock <- liftIO $ socket (addrFamily addr) Stream (addrProtocol addr)
    liftIO $ connect sock (addrAddress addr)
    runDirectT' sock action

bufferedMsgs = 10

pushMsg :: (Monad m) => ByteString -> DirectT m Word32
pushMsg body = DirectT $ do
    k <- gets psNextSerial
    let m = Message (Msg k) body
    modify $ \ps -> ps
        { psNextSerial = succ k
        , psBuffer = m : psBuffer ps
        }
    return k

pushReply' :: (Monad m) => Head -> ByteString -> DirectT m ()
pushReply' (Msg k) body = DirectT $ do
    let m = Message (Reply k) body
    modify $ \ps -> ps { psBuffer = m : psBuffer ps }
pushReply' (Reply _) _ = error "cannot reply to a reply"

pushReply :: (Monad m) => Message -> ByteString -> DirectT m ()
pushReply = pushReply' . msgHead

pullMsg :: (MonadIO m) => DirectT m Message
pullMsg = DirectT $ do
    sock <- ask
    hooks <- gets psHooks
    m <- liftIO $ recvMsg sock
    case msgHead m of
        Msg _ -> return m
        Reply k -> case IM.lookup (fromEnum k) hooks of
            Just hook -> do
                modify $ \ps -> ps { psHooks = IM.delete (fromEnum k) hooks }
                unDirectT $ do
                    hook $ msgBody m
                    pullMsg
            Nothing -> unDirectT pullMsg

flushMsgs :: (MonadIO m) => DirectT m ()
flushMsgs = DirectT $ do
    ms <- reverse `liftM` gets psBuffer
    sock <- ask
    liftIO $ sendMsgs sock ms
    modify $ \ps -> ps { psBuffer = [] }

noReply :: (MonadIO m) => ByteString -> DirectT m ()
noReply body = pushMsg body >>= \k -> DirectT $ do
    len <- gets $ length . psBuffer
    when (len >= bufferedMsgs) $ unDirectT flushMsgs

withReply :: (MonadIO m) => ByteString -> (ByteString -> DirectT m ()) -> DirectT m ()
withReply body hook = pushMsg body >>= \k -> DirectT $ do
    hooks <- gets psHooks
    modify $ \ps -> ps { psHooks = IM.insert (fromEnum k) hook hooks }
    unDirectT flushMsgs

-- }}}

-- vim:fdm=marker:
