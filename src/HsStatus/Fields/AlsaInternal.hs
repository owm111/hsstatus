module HsStatus.Fields.AlsaInternal
  ( MixerElement
  , openMixerElement
  , closeMixerElement
  , awaitNewStatus
  ) where

{-
The goal of this module is to recreate this C code in Haskell:

    #include <alsa/asoundlib.h>
    #include <poll.h>
    #include <stdbool.h>
    #include <unistd.h>

    // Initialize
    long int min, max, vol;
    int sw, perc;
    bool mute;
    nfds_t pfds_count;
    snd_mixer_t * handle;
    snd_mixer_elem_t * elem;
    snd_mixer_selem_id_t * s_elem;
    snd_mixer_open(&handle, 0);
    snd_mixer_attach(handle, "default");
    snd_mixer_selem_register(handle, NULL, NULL);
    snd_mixer_load(handle);
    snd_mixer_selem_id_malloc(&s_elem);
    snd_mixer_selem_id_set_name(s_elem, "Master");
    elem = snd_mixer_find_selem(handle, s_elem);
    pfds_count = snd_mixer_poll_descriptors_count(handle);
    struct pollfd pfds[pfds_count];
    pfds_count = snd_mixer_poll_descriptors(handle, pfds, pfds_count);

    // Run
    while (true) {
      poll(pfds, pfds_count, -1);
      snd_mixer_handle_events(handle);
      snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
      snd_mixer_selem_get_playback_volume(elem, 0, &vol);
      perc = max ? vol * 100 / max : 0;
      snd_mixer_selem_get_playback_switch(elem, 0, &sw);
      mute = sw;
      // ...
    }

    // Free
    snd_mixer_selem_id_free(s_elem);
    snd_mixer_close(handle);

A basic translation of this in Haskell:

  import Control.Concurrent
  import Control.Monad

  -- Initialize
  handle <- openMixer
  attachMixer handle "default"
  registerMixer handle
  loadMixer handle
  s_elem <- openSimpleId
  setSimpleIdName s_elem "Master"
  elem <- findSimpleId handle s_elem
  (PollFd pfd _ _ : _) <- getMixerPollDescriptors handle

  -- Run
  forever $ do
    threadWaitRead pfd
    handleMixerEvents handle
    (min, max) <- getPlaybackVolumeRange elem
    vol <- getPlaybackVolume elem
    let perc | max == 0  = 0 :: Int
             | otherwise = fromIntegral (vol * 100 `div` max)
    mute <- getPlaybackSwitch elem
    -- ...

    -- Free
    finalizeForeignPtr s_elem
    closeMixer handle

To make it a bit more specific to my needs:

  import Control.Monad

  -- Initialize
  mixerelem <- openMixerElement "default" "Master"

  -- Run
  forever $ do
    (mute, perc) <- awaitNewStatus mixerelem
    -- ...

  -- Free
  closeMixerElement mixerelem

Improvements that can still be made:

- Add or re-export handler for Sound.ALSA.Exception.T
- Use ForeignPtr for Mixer (so then it is freed automatically)
- Use mallocForeignPtr (the docs say it is more performant)

-}

import Control.Concurrent
import Foreign
import Foreign.C
import Sound.ALSA.Exception hiding (show)
import System.Posix.Types

{-- * For testing
import Control.Exception hiding (throw)
import Control.Monad

main = bracket (openMixerElement "default" "Master") closeMixerElement
     $ \mixerelem -> forever (awaitNewStatus mixerelem >>= print)
-}

-- * Basic translation

-- Type defintions. This strange-looking recursive pattern adds safety (over
-- type ... = Ptr ()).

newtype Mixer    = Mixer    (Ptr Mixer)    -- ^ snd_mixer_t
newtype Element  = Element  (Ptr Element)  -- ^ snd_mixer_elem_t
newtype SimpleId = SimpleId (Ptr SimpleId) -- ^ snd_mixer_selem_id_t

-- | struct pollfd
data PollFd = PollFd { pfdFd :: Fd, pfdEvents :: CShort, pfdRevents :: CShort }

instance Storable PollFd where
  sizeOf    _ = 8
  alignment _ = 4
  peek p = do
    let fp = castPtr p :: Ptr Fd
    f <- peek fp
    let ep = fp `plusPtr` sizeOf f :: Ptr CShort
    e <- peek ep
    let rp = ep `plusPtr` sizeOf e :: Ptr CShort
    r <- peek rp
    return (PollFd f e r)
  poke p (PollFd f e r) = do
    let fp = castPtr p :: Ptr Fd
    poke fp f
    let ep = fp `plusPtr` sizeOf f :: Ptr CShort
    poke ep e
    let rp = ep `plusPtr` sizeOf e :: Ptr CShort
    poke rp r

-- ** Initializing and freeing mixers

-- | Allocates a pointer (to a pointer to a mixer), calls snd_mixer_open (with
-- mode 0), peeks at the pointer pointer (storing it as a Mixer), frees it, and
-- returns the Mixer. It needs to be closed at some point.
--
-- TODO: that can be automated with ForeignPtr
openMixer :: IO Mixer
openMixer = do
  mixerPtrPtr <- malloc :: IO (Ptr (Ptr Mixer))
  c_snd_mixer_open mixerPtrPtr 0 >>= checkResult_ "snd_mixer_open"
  mixer <- Mixer <$> peek mixerPtrPtr
  free mixerPtrPtr
  return mixer

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_open"
  c_snd_mixer_open :: Ptr (Ptr Mixer) -> CInt -> IO CInt

-- | Simple wrapper for snd_mixer_close.
closeMixer :: Mixer -> IO ()
closeMixer (Mixer ptr) = c_snd_mixer_close ptr

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_close"
  c_snd_mixer_close :: Ptr Mixer -> IO ()

-- | Calls snd_mixer_attach with the given string.
attachMixer :: Mixer -> String -> IO ()
attachMixer (Mixer ptr) namestr = do
  withCString namestr (c_snd_mixer_attach ptr)
    >>= checkResult_ "snd_mixer_attach"

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_attach"
  c_snd_mixer_attach :: Ptr Mixer -> CString -> IO CInt

-- | Calls snd_mixer_register with null pointers for arguments and throws any
-- exceptions.
registerMixer :: Mixer -> IO ()
registerMixer (Mixer ptr) = do
  c_snd_mixer_selem_register ptr nullPtr nullPtr
    >>= checkResult_ "snd_mixer_selem_register"

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_selem_register"
  c_snd_mixer_selem_register :: Ptr Mixer -> Ptr () -> Ptr (Ptr ()) -> IO CInt

-- | Simple wrapper for snd_mixer_load that throws exceptions.
loadMixer :: Mixer -> IO ()
loadMixer (Mixer ptr) = do
  c_snd_mixer_load ptr >>= checkResult_ "snd_mixer_load"

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_load"
  c_snd_mixer_load :: Ptr Mixer -> IO CInt

-- | Runs all of the above in order, taking the name of the mixer (argument for
-- snd_mixer_attach/attachMixer) and returning the Mixer.
initMixer :: String -> IO Mixer
initMixer name = do
  handle <- openMixer
  attachMixer handle name
  registerMixer handle
  loadMixer handle
  return handle

-- ** Initializing simple element ids

-- (No freeing should be necessary since they are stored in ForeignPtrs.)

-- Used as the finalizer.
foreign import ccall unsafe "alsa/asoundlib.h &snd_mixer_selem_id_free"
  funptr_snd_mixer_selem_id_free :: FunPtr (Ptr SimpleId -> IO ())

-- | Locally allocates a pointer (to a pointer to an id), calls
-- snd_mixer_selem_id_malloc, peeks at the pointer pointer (storing it as a Ptr
-- SimpleId), creates a foreign pointer the Ptr SimpleId (finalized with
-- snd_mixer_selem_id_free), and returns it. Freeing should not be explicitly
-- necessary but can be done with finalizeForeignPtr.
openSimpleId :: IO (ForeignPtr SimpleId)
openSimpleId = do
  alloca $ \selemPtrPtr -> do
    c_snd_mixer_selem_id_malloc selemPtrPtr >>= checkResult_ "snd_mixer_selem_id_malloc"
    selemPtr <- peek selemPtrPtr
    fptr <- newForeignPtr funptr_snd_mixer_selem_id_free selemPtr
    return fptr
    
foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_selem_id_malloc"
  c_snd_mixer_selem_id_malloc :: Ptr (Ptr SimpleId) -> IO CInt

-- | Simple wrapper for snd_mixer_selem_id_set_name that throws exceptions.
setSimpleIdName :: ForeignPtr SimpleId -> String -> IO ()
setSimpleIdName fptr name = do
  withCString name $ \idname -> do
    withForeignPtr fptr $ \selemPtr -> do
      c_snd_mixer_selem_id_set_name selemPtr idname

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_selem_id_set_name"
  c_snd_mixer_selem_id_set_name :: Ptr SimpleId -> CString -> IO ()

-- | Combines the above two functions. Use this instead.
simpleIdWithName :: String -> IO (ForeignPtr SimpleId)
simpleIdWithName name = do
  withCString name $ \idname -> do
    alloca $ \selemPtrPtr -> do
      c_snd_mixer_selem_id_malloc selemPtrPtr >>= checkResult_ "snd_mixer_selem_id_malloc"
      selemPtr <- peek selemPtrPtr
      c_snd_mixer_selem_id_set_name selemPtr idname
      fptr <- newForeignPtr funptr_snd_mixer_selem_id_free selemPtr
      return fptr

-- ** Finding mixer elements

-- (These aren't freed in the C code so I'm not freeing them here.)

-- | Calls snd_mixer_find_selem on the given mixer and (foreign pointer to a)
-- simple element id, throwing an exception if the element could not be found
-- (i.e. is a null pointer).
findSimpleId :: Mixer -> ForeignPtr SimpleId -> IO Element
findSimpleId (Mixer mixerPtr) selemFptr = do
  withForeignPtr selemFptr $ \selemPtr -> do
    elemPtr <- c_snd_mixer_find_selem mixerPtr selemPtr
    if elemPtr == nullPtr
      then throw "snd_mixer_find_selem" eNOENT
      else return (Element elemPtr)

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_find_selem"    
  c_snd_mixer_find_selem :: Ptr Mixer -> Ptr SimpleId -> IO (Ptr Element)

-- ** Getting element status

-- | Calls snd_mixer_selem_get_playback_volume on a locally-allocated CLong
-- pointer, throws any exceptions, peeks at the pointer, and returns the result
-- as an Integer.
--
-- Uses front left channel.
getPlaybackVolume :: Element -> IO Integer
getPlaybackVolume (Element elemPtr) = do
  alloca $ \volPtr -> do
    c_snd_mixer_selem_get_playback_volume elemPtr 0 volPtr >>= checkResult_ "snd_mixer_selem_get_playback_volume"
    volLong <- peek volPtr
    return (fromIntegral volLong)

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_selem_get_playback_volume"
  c_snd_mixer_selem_get_playback_volume :: Ptr Element -> CInt -> Ptr CLong -> IO CInt

-- | Calls snd_mixer_selem_get_playback_volume_range on two locally-allocated
-- CLong pointers, throws any exceptions, peeks at the pointers, and returns the
-- result as a pair of Integers.
getPlaybackVolumeRange :: Element -> IO (Integer, Integer)
getPlaybackVolumeRange (Element elemPtr) = do
  alloca $ \minPtr -> do
    alloca $ \maxPtr -> do
      c_snd_mixer_selem_get_playback_volume_range elemPtr minPtr maxPtr >>= checkResult_ "snd_mixer_selem_get_playback_volume_range"
      minLong <- peek minPtr
      maxLong <- peek maxPtr
      return (fromIntegral minLong, fromIntegral maxLong)

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_selem_get_playback_volume_range"
  c_snd_mixer_selem_get_playback_volume_range :: Ptr Element -> Ptr CLong -> Ptr CLong -> IO CInt

-- | Calls snd_mixer_selem_get_playback_switch on a locally-allocated CInt
-- pointer, throws any exceptions, peeks at the pointer, and returns the result
-- as a boolean.
--
-- Uses front left channel.
getPlaybackSwitch :: Element -> IO Bool
getPlaybackSwitch (Element elemPtr) = do
  alloca $ \swPtr -> do
    c_snd_mixer_selem_get_playback_switch elemPtr 0 swPtr >>= checkResult_ "snd_mixer_selem_get_playback_switch"
    swInt <- peek swPtr
    return (toBool swInt)

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_selem_get_playback_switch"
  c_snd_mixer_selem_get_playback_switch :: Ptr Element -> CInt -> Ptr CInt -> IO CInt

-- | Combines the above 3 functions, returning a pair of the switch as a boolean
-- and the volume percent as an Int.
getPlaybackStatus :: Element -> IO (Bool, Int)
getPlaybackStatus elem = do
  vol        <- getPlaybackVolume      elem
  (min, max) <- getPlaybackVolumeRange elem
  sw         <- getPlaybackSwitch      elem
  let perc | max == 0   = 0
           | min == 0   = fromIntegral (vol * 100 `div` max)
           | max == min = 0
           | otherwise  = fromIntegral ((vol - min) * 100 `div` (max - min))
  return (sw, perc)

-- ** Mixer events and poll descriptors

-- | Simple wrapper for snd_mixer_handle_events that throws exceptions.
handleMixerEvents :: Mixer -> IO ()
handleMixerEvents (Mixer mixerPtr) =
  c_snd_mixer_handle_events mixerPtr >>= checkResult_ "snd_mixer_handle_events"

foreign import ccall unsafe "alsa/asoundlib.h snd_mixer_handle_events"
  c_snd_mixer_handle_events :: Ptr Mixer -> IO CInt

foreign import ccall "alsa/asoundlib.h snd_mixer_poll_descriptors_count"
  c_snd_mixer_poll_descriptors_count :: Ptr Mixer -> IO CInt
foreign import ccall "alsa/asoundlib.h snd_mixer_poll_descriptors"
  c_snd_mixer_poll_descriptors :: Ptr Mixer -> Ptr PollFd -> CUInt -> IO CInt

-- | Calls snd_mixer_poll_descriptors_count, locally allocates an array of poll
-- descriptors with that size, calls snd_mixer_poll_descriptors, peeks the array
-- into a list using the new count, and returns it.
getMixerPollDescriptors :: Mixer -> IO [PollFd]
getMixerPollDescriptors (Mixer ptr) = do
  pfdsCount <- c_snd_mixer_poll_descriptors_count ptr >>= checkResult "snd_mixer_poll_descriptors_count"
  allocaArray (fromIntegral pfdsCount) $ \arr -> do
    filledCount <- c_snd_mixer_poll_descriptors ptr arr (fromIntegral pfdsCount) >>= checkResult "snd_mixer_poll_descriptors"
    peekArray (fromIntegral filledCount) arr

-- * Specialized

-- | Combines the Mixer, (foreign pointer to the) SimpleId, Element, and list of
-- poll descriptors into a single type.
data MixerElement = MixerElement Mixer (ForeignPtr SimpleId) Element [PollFd]

-- | Completes the initialization process described at the top: open a mixer,
-- attach it to a name, register it, load it, open a simple id, set its name,
-- find the element, get the poll descriptors, and return everything.
openMixerElement :: String -> String -> IO MixerElement
openMixerElement mixerName elemName = do
  mixer <- initMixer mixerName
  sid <- simpleIdWithName elemName
  elem <- findSimpleId mixer sid
  pfds <- getMixerPollDescriptors mixer
  return (MixerElement mixer sid elem pfds)

-- | Frees a MixerElement.
closeMixerElement :: MixerElement -> IO ()
closeMixerElement (MixerElement mixer sidFptr _ _) = do
  finalizeForeignPtr sidFptr
  closeMixer mixer

-- | Takes the first descriptor from a list of poll descriptors.
fstFd :: [PollFd] -> Fd
fstFd (PollFd fd _ _ : _) = fd

-- | Waits for an event on the mixer, handles it, and returns the playback
-- status of the mixer (described in getPlaybackStatus).
awaitNewStatus :: MixerElement -> IO (Bool, Int)
awaitNewStatus (MixerElement mixer _ elem pfds) = do
  threadWaitRead (fstFd pfds)
  handleMixerEvents mixer
  getPlaybackStatus elem
