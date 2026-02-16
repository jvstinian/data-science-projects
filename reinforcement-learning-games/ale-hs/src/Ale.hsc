{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Ale (
    hello,
    welcomeMessage,
    Reward,
    Action(..),
    ALEInterface,
    newAleInterface,
    getString,
    getInt,
    getBool,
    getFloat,
    setString,
    setInt,
    setBool,
    setFloat,
    loadRom,
    act,
    gameOver,
    gameTruncated,
    resetGame,
    lives,
    getAvailableModes,
    getMode,
    setMode,
    getAvailableDifficulties,
    getDifficulty,
    setDifficulty,
    getLegalActionSet,
    getMinimalActionSet,
    getFrameNumber,
    getEpisodeFrameNumber,
    getMaxNumFrames,
    getScreenGrayscale,
    getScreenRGB,
    saveScreenPNG 
) where

import qualified Language.C.Inline as C
-- import qualified Language.C.Types as C
-- import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C
-- import qualified Language.C.Inline.Cpp.Unsafe as CPPU
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM
import AleContext ( aleCtx, C'ALEInterface, C'Reward, C'GameMode, C'GameDifficulty, Action(..), C'Pixel )
import           Data.Word(Word8, Word32)
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.C.String
import           Foreign.Marshal.Array


newtype ALEInterface = ALEInterface {unALEInterface:: ForeignPtr C'ALEInterface}

type Reward = Int
type GameMode = Word32
type GameDifficulty = Word32
type Pixel = Word8

-- C.context C.cppCtx
C.context aleCtx
C.include "<ale_interface.hpp>" 
C.using "namespace ale"

{-
#include <ale_interface.hpp>
c_cos :: CDouble -> IO CDouble
c_cos x = [C.exp| double { cos($(double x)) } |]

faster_c_cos :: CDouble -> IO CDouble
faster_c_cos x = [CU.exp| double { cos($(double x)) } |]
-}

welcomeMessage :: IO String
-- welcomeMessage = [CU.exp| const char* { ALEInterface::welcomeMessage().c_str() } |]
-- welcomeMessage = [CU.block| const char* { ALEInterface::welcomeMessage().c_str() } |]

copyWelcomeMessage :: Int -> Ptr CChar -> IO String
copyWelcomeMessage size ptr = do
    let maxsize = fromIntegral size :: CSize
    _ <- [C.block| size_t {
        size_t destsize = $(size_t maxsize);
        std::string message(ALEInterface::welcomeMessage());
        strncpy($(char * ptr), message.c_str(), destsize);
        $(char* ptr)[destsize - 1] = '\0';
        return message.length();
    } |]
    peekCString ptr

welcomeMessage = allocaArray 100 (copyWelcomeMessage 100)

{-
newEmptyMat = liftIO ( unsafeCoerceMat <$> fromPtr [CU.exp|Mat * { new Mat() }|] )
-}
{-
aleInterface = [CU.exp|ALEInterface * { new ALEInterface() }|] :: IO (Ptr C'ALEInterface)
-}

newAleInterface :: IO ALEInterface
newAleInterface = do
    alePtr <- [CU.exp|ALEInterface * { new ALEInterface() }|] :: IO (Ptr C'ALEInterface)
    let deleteAlePtr = [C.funPtr| void deleteAleInterface(ALEInterface * p) { delete p; }|]
    ALEInterface <$> newForeignPtr deleteAlePtr alePtr

c'getString :: Ptr C'ALEInterface -> CString -> IO String
c'getString aleptr ckey = do
    vallen <- [C.block| size_t {
        std::string keystr( $(char * ckey) );
        return $(ALEInterface * aleptr)->getString(keystr).size();
    } |]
    let vallenp1 = vallen + 1
    allocaArray (fromIntegral vallenp1) (copyCStringValue ckey (vallenp1))
    
  where copyCStringValue key lenp1 valptr = do
          _ <- [C.block| size_t {
              std::string keystr( $(char * key) );
              std::string valstr = $(ALEInterface * aleptr)->getString(keystr);
              size_t destsize = $(size_t lenp1);
              strncpy($(char * valptr), valstr.c_str(), destsize);
              $(char* valptr)[destsize - 1] = '\0';
              return strlen($(char * valptr));
          } |]
          peekCString valptr

getString :: ALEInterface -> String -> IO String
getString ale key = withForeignPtr (unALEInterface ale) $ \aleptr -> withCString key (c'getString aleptr)

c'getInt :: Ptr C'ALEInterface -> CString -> IO CInt
c'getInt aleptr key = 
    [C.block| int {
        std::string keystr( $(char * key) );
        return $(ALEInterface * aleptr)->getInt(keystr);
    } |]

getInt :: ALEInterface -> String -> IO Int
getInt ale key = withForeignPtr (unALEInterface ale) $ \aleptr -> fromIntegral <$> withCString key (c'getInt aleptr)

c'getBool :: Ptr C'ALEInterface -> CString -> IO CBool
c'getBool aleptr key = 
    [C.block| bool {
        std::string keystr( $(char * key) );
        return $(ALEInterface * aleptr)->getBool(keystr);
    } |]

getBool :: ALEInterface -> String -> IO Bool
getBool ale key = withForeignPtr (unALEInterface ale) $ \aleptr -> cBoolToHBool <$> withCString key (c'getBool aleptr)
    where cBoolToHBool = toEnum . fromEnum

c'getFloat :: Ptr C'ALEInterface -> CString -> IO CFloat
c'getFloat aleptr key = 
    [C.block| float {
        std::string keystr( $(char * key) );
        return $(ALEInterface * aleptr)->getFloat(keystr);
    } |]

fromCFloat :: CFloat -> Float
fromCFloat (CFloat f) = f

getFloat :: ALEInterface -> String -> IO Float
getFloat ale key = withForeignPtr (unALEInterface ale) $ \aleptr -> fromCFloat <$> withCString key (c'getFloat aleptr)
 
-- TODO: getString

c'setString :: Ptr C'ALEInterface -> CString -> CString -> IO ()
c'setString aleptr key value = 
    [C.block| void {
        std::string keystr( $(char * key) );
        std::string valstr( $(char * value) );
        $(ALEInterface * aleptr)->setString(keystr, valstr);
    } |]

setString :: ALEInterface -> String -> String -> IO ()
setString ale key value = withForeignPtr (unALEInterface ale) $ \aleptr ->
    withCString key (\ckey -> withCString value (c'setString aleptr ckey))

c'setInt :: Ptr C'ALEInterface -> CString -> CInt -> IO ()
c'setInt aleptr key value = 
    [C.block| void {
        std::string keystr( $(char * key) );
        $(ALEInterface * aleptr)->setInt(keystr, $(int value));
    } |]

setInt :: ALEInterface -> String -> Int -> IO ()
setInt ale key value = withForeignPtr (unALEInterface ale) $ \aleptr ->
    withCString key (\ckey -> c'setInt aleptr ckey (fromIntegral value))

c'setBool :: Ptr C'ALEInterface -> CString -> CBool -> IO ()
c'setBool aleptr key value = 
    [C.block| void {
        std::string keystr( $(char * key) );
        $(ALEInterface * aleptr)->setBool(keystr, $(bool value));
    } |]

setBool :: ALEInterface -> String -> Bool -> IO ()
setBool ale key value = withForeignPtr (unALEInterface ale) $ \aleptr ->
    withCString key (\ckey -> c'setBool aleptr ckey (toEnum . fromEnum $ value))

c'setFloat :: Ptr C'ALEInterface -> CString -> CFloat -> IO ()
c'setFloat aleptr key value = 
    [C.block| void {
        std::string keystr( $(char * key) );
        $(ALEInterface * aleptr)->setFloat(keystr, $(float value));
    } |]

setFloat :: ALEInterface -> String -> Float -> IO ()
setFloat ale key value = withForeignPtr (unALEInterface ale) $ \aleptr ->
    withCString key (\ckey -> c'setFloat aleptr ckey (CFloat value))

c'loadRom :: Ptr C'ALEInterface -> CString -> IO ()
c'loadRom aleptr path = 
    [C.block| void {
        fs::path rom_file($(char * path));
        $(ALEInterface * aleptr)->loadROM(rom_file);
    } |]

loadRom :: ALEInterface -> String -> IO ()
loadRom ale path = withForeignPtr (unALEInterface ale) $ \aleptr ->
    withCString path (c'loadRom aleptr)

c'act :: Ptr C'ALEInterface -> CInt -> CFloat -> IO C'Reward
c'act aleptr caction cpaddlestrength =
    [C.exp| reward_t {
        $(ALEInterface * aleptr)->act($(Action caction), $(float cpaddlestrength))
    } |]

act :: ALEInterface -> Action -> Float -> IO Reward
act ale action paddlestrength =
    withForeignPtr (unALEInterface ale) $ \ptr -> fromIntegral <$> c'act ptr (enumToCInt action) (CFloat paddlestrength)
    where enumToCInt = fromIntegral . fromEnum

c'gameOver :: Ptr C'ALEInterface -> CBool -> IO CBool
c'gameOver aleptr with_trunc =
    [C.exp| bool {
        $(ALEInterface * aleptr)->game_over($(bool with_trunc))
    } |]

gameOver :: ALEInterface -> Bool -> IO Bool
gameOver ale with_trunc = withForeignPtr (unALEInterface ale) $ \aleptr ->
        cBoolToHBool <$> c'gameOver aleptr (hBoolToCBool with_trunc)
    where cBoolToHBool :: CBool -> Bool
          cBoolToHBool = toEnum . fromEnum
          hBoolToCBool :: Bool -> CBool
          hBoolToCBool = toEnum . fromEnum
          

c'gameTruncated :: Ptr C'ALEInterface -> IO CBool
c'gameTruncated aleptr =
    [C.exp| bool {
        $(ALEInterface * aleptr)->game_truncated()
    } |]

gameTruncated :: ALEInterface -> IO Bool
gameTruncated ale = withForeignPtr (unALEInterface ale) $ fmap (toEnum . fromEnum) . c'gameTruncated

c'resetGame :: Ptr C'ALEInterface -> IO ()
c'resetGame aleptr =
    [C.exp| void {
        $(ALEInterface * aleptr)->reset_game()
    } |]

resetGame :: ALEInterface -> IO ()
resetGame ale = withForeignPtr (unALEInterface ale) c'resetGame

c'getAvailableModesLength :: Ptr C'ALEInterface -> IO CUInt
c'getAvailableModesLength aleptr =
    [C.exp| unsigned int {
        $(ALEInterface * aleptr)->getAvailableModes().size()
    } |]

c'getAvailableModes :: Ptr C'ALEInterface -> IO (VM.IOVector C'GameMode)
c'getAvailableModes aleptr = do
    len <- c'getAvailableModesLength aleptr :: IO CUInt
    iovec <- VM.new (fromIntegral len) :: IO (VM.IOVector C'GameMode)
    _ <- [C.block| void {
            std::vector<game_mode_t> game_modes($(ALEInterface * aleptr)->getAvailableModes());
            const game_mode_t* game_modes_array = game_modes.data();
            std::copy(game_modes_array, game_modes_array + $(unsigned int len), $vec-ptr:(game_mode_t * iovec));
         } |]
    return iovec

getAvailableModes :: ALEInterface -> IO (VS.Vector GameMode)
getAvailableModes ale = withForeignPtr (unALEInterface ale) c'getAvailableModes  >>= VS.freeze >>= (return . VS.map fromIntegral)

c'setMode :: Ptr C'ALEInterface -> C'GameMode -> IO ()
c'setMode aleptr gamemode = 
    [C.exp| void {
        $(ALEInterface * aleptr)->setMode($(game_mode_t gamemode));
    } |]

setMode :: ALEInterface -> GameMode -> IO ()
setMode ale gamemode = withForeignPtr (unALEInterface ale) $ \aleptr -> c'setMode aleptr (fromIntegral gamemode)

c'getMode :: Ptr C'ALEInterface -> IO C'GameMode
c'getMode aleptr = [C.exp| game_mode_t { $(ALEInterface * aleptr)->getMode() } |]

getMode :: ALEInterface -> IO GameMode
getMode ale = withForeignPtr (unALEInterface ale) (fmap fromIntegral . c'getMode)

c'getAvailableDifficultiesLength :: Ptr C'ALEInterface -> IO CUInt
c'getAvailableDifficultiesLength aleptr =
    [C.exp| unsigned int {
        $(ALEInterface * aleptr)->getAvailableDifficulties().size()
    } |]

c'getAvailableDifficulties :: Ptr C'ALEInterface -> IO (VM.IOVector C'GameDifficulty)
c'getAvailableDifficulties aleptr = do
    len <- c'getAvailableDifficultiesLength aleptr :: IO CUInt
    iovec <- VM.new (fromIntegral len) :: IO (VM.IOVector C'GameDifficulty)
    _ <- [C.block| void {
            std::vector<difficulty_t> game_difficulties($(ALEInterface * aleptr)->getAvailableDifficulties());
            const difficulty_t* game_difficulties_array = game_difficulties.data();
            std::copy(game_difficulties_array, game_difficulties_array + $(unsigned int len), $vec-ptr:(difficulty_t * iovec));
         } |]
    return iovec

getAvailableDifficulties :: ALEInterface -> IO (VS.Vector GameDifficulty)
getAvailableDifficulties ale = withForeignPtr (unALEInterface ale) c'getAvailableDifficulties >>= VS.freeze >>= (return . VS.map fromIntegral)

c'setDifficulty :: Ptr C'ALEInterface -> C'GameDifficulty -> IO ()
c'setDifficulty aleptr difficulty = 
    [C.exp| void {
        $(ALEInterface * aleptr)->setDifficulty($(difficulty_t difficulty));
    } |]

setDifficulty :: ALEInterface -> GameDifficulty -> IO ()
setDifficulty ale difficulty = withForeignPtr (unALEInterface ale) $ \aleptr -> c'setDifficulty aleptr (fromIntegral difficulty)

c'getDifficulty :: Ptr C'ALEInterface -> IO C'GameDifficulty
c'getDifficulty aleptr = [C.exp| difficulty_t { $(ALEInterface * aleptr)->getDifficulty() } |]

getDifficulty :: ALEInterface -> IO GameDifficulty
getDifficulty ale = withForeignPtr (unALEInterface ale) (fmap fromIntegral . c'getDifficulty)

c'getLegalActionSetLength :: Ptr C'ALEInterface -> IO CUInt
c'getLegalActionSetLength aleptr =
    [C.exp| unsigned int {
        $(ALEInterface * aleptr)->getLegalActionSet().size()
    } |]

c'getLegalActionSet :: Ptr C'ALEInterface -> IO (VM.IOVector CInt)
c'getLegalActionSet aleptr = do
    len <- c'getLegalActionSetLength aleptr :: IO CUInt
    iovec <- VM.new (fromIntegral len) :: IO (VM.IOVector CInt)
    _ <- [C.block| void {
            std::vector<Action> actions($(ALEInterface * aleptr)->getLegalActionSet());
            const Action* actions_array = actions.data();
            std::copy(actions_array, actions_array + $(unsigned int len), $vec-ptr:(Action * iovec));
         } |]
    return iovec

getLegalActionSet:: ALEInterface -> IO (V.Vector Action)
getLegalActionSet ale = withForeignPtr (unALEInterface ale) c'getLegalActionSet >>= VS.freeze >>= (return . VS.convert . VS.map fromIntegral) >>= (return . V.map toEnum)

c'getMinimalActionSetLength :: Ptr C'ALEInterface -> IO CUInt
c'getMinimalActionSetLength aleptr =
    [C.exp| unsigned int {
        $(ALEInterface * aleptr)->getMinimalActionSet().size()
    } |]

c'getMinimalActionSet :: Ptr C'ALEInterface -> IO (VM.IOVector CInt)
c'getMinimalActionSet aleptr = do
    len <- c'getMinimalActionSetLength aleptr :: IO CUInt
    iovec <- VM.new (fromIntegral len) :: IO (VM.IOVector CInt)
    _ <- [C.block| void {
            std::vector<Action> actions($(ALEInterface * aleptr)->getMinimalActionSet());
            const Action* actions_array = actions.data();
            std::copy(actions_array, actions_array + $(unsigned int len), $vec-ptr:(Action * iovec));
         } |]
    return iovec

getMinimalActionSet:: ALEInterface -> IO (V.Vector Action)
getMinimalActionSet ale = withForeignPtr (unALEInterface ale) c'getMinimalActionSet >>= VS.freeze >>= (return . VS.convert . VS.map fromIntegral) >>= (return . V.map toEnum)

c'getFrameNumber :: Ptr C'ALEInterface -> IO CInt
c'getFrameNumber aleptr = [C.exp| int { $(ALEInterface * aleptr)->getFrameNumber() } |]

getFrameNumber :: ALEInterface -> IO Int
getFrameNumber ale = withForeignPtr (unALEInterface ale) (fmap fromIntegral . c'getFrameNumber)

c'getEpisodeFrameNumber :: Ptr C'ALEInterface -> IO CInt
c'getEpisodeFrameNumber aleptr = [CU.exp| int { $(ALEInterface * aleptr)->getEpisodeFrameNumber() }|]

getEpisodeFrameNumber :: ALEInterface -> IO Int
getEpisodeFrameNumber ale = withForeignPtr (unALEInterface ale) (fmap fromIntegral . c'getEpisodeFrameNumber)

c'getMaxNumFrames :: Ptr C'ALEInterface -> IO CInt
c'getMaxNumFrames aleptr = [CU.exp| int { $(ALEInterface * aleptr)->max_num_frames }|]

getMaxNumFrames :: ALEInterface -> IO Int
getMaxNumFrames ale = withForeignPtr (unALEInterface ale) $ fmap fromIntegral . c'getMaxNumFrames

c'lives :: Ptr C'ALEInterface -> IO CInt
c'lives aleptr = [C.exp| int { $(ALEInterface * aleptr)->lives() } |]

lives :: ALEInterface -> IO Int
lives ale = withForeignPtr (unALEInterface ale) $ \ptr -> fromIntegral <$> c'lives ptr

c'getScreenSize :: Ptr C'ALEInterface -> IO (CSize, CSize)
c'getScreenSize aleptr = do
    rows <- [C.exp| size_t { 
                $(ALEInterface * aleptr)->getScreen().height()
            } |]
    cols <- [C.exp| size_t { 
                $(ALEInterface * aleptr)->getScreen().width()
            } |]
    return (rows, cols)

c'getScreenGrayscale :: Ptr C'ALEInterface -> IO (CSize, CSize, VM.IOVector C'Pixel)
c'getScreenGrayscale aleptr = do
    (rows, cols) <- c'getScreenSize aleptr
    let size = rows * cols
    gsvec <- VM.new (fromIntegral size) :: IO (VM.IOVector C'Pixel)
    _ <- [C.block| void {
            std::vector<unsigned char> grayscale_buffer($(size_t size), 0u);
            $(ALEInterface * aleptr)->getScreenGrayscale(grayscale_buffer);
            const unsigned char* gs_array = grayscale_buffer.data();
            std::copy(gs_array, gs_array + $(size_t size), $vec-ptr:(pixel_t * gsvec));
         } |]
    return (rows, cols, gsvec)

getScreenGrayscale :: ALEInterface -> IO (Int, Int, V.Vector Pixel)
getScreenGrayscale ale = withForeignPtr (unALEInterface ale) c'getScreenGrayscale >>= outputConversion
    where pixelVectorConvert = return . VS.convert . (VS.map fromIntegral)
          pixelVectorFreezeConvert = (>>= pixelVectorConvert) . VS.freeze
          outputConversion (r, c, screenvec) = do
                screenout <- pixelVectorFreezeConvert screenvec
                return (fromIntegral r, fromIntegral c, screenout)

    
c'getScreenRGB :: Ptr C'ALEInterface -> IO (CSize, CSize, VM.IOVector C'Pixel)
c'getScreenRGB aleptr = do
    (rows, cols) <- c'getScreenSize aleptr
    let size = 3 * rows * cols
    rgbvec <- VM.new (fromIntegral size) :: IO (VM.IOVector C'Pixel)
    _ <- [C.block| void {
            std::vector<unsigned char> rgb_buffer($(size_t size), 0u);
            $(ALEInterface * aleptr)->getScreenRGB(rgb_buffer);
            const unsigned char* rgb_array = rgb_buffer.data();
            std::copy(rgb_array, rgb_array + $(size_t size), $vec-ptr:(pixel_t * rgbvec));
         } |]
    return (rows, cols, rgbvec)

getScreenRGB :: ALEInterface -> IO (Int, Int, V.Vector Pixel)
getScreenRGB ale = withForeignPtr (unALEInterface ale) c'getScreenRGB >>= outputConversion
    where pixelVectorConvert = return . VS.convert . (VS.map fromIntegral)
          pixelVectorFreezeConvert = (>>= pixelVectorConvert) . VS.freeze
          outputConversion (r, c, screenvec) = do
                screenout <- pixelVectorFreezeConvert screenvec
                return (fromIntegral r, fromIntegral c, screenout)

c'saveScreenPNG :: Ptr C'ALEInterface -> CString -> IO ()
c'saveScreenPNG aleptr filename = 
    [C.block| void {
        std::string file($(char * filename));
        $(ALEInterface * aleptr)->saveScreenPNG(file);
    } |]
 
saveScreenPNG :: ALEInterface -> String -> IO ()
saveScreenPNG ale filename = withForeignPtr (unALEInterface ale) $ \aleptr -> 
    withCString filename (\cfilename -> c'saveScreenPNG aleptr cfilename)

{-
getCStringSafe :: IO String
getCStringSafe =
  bracket
    [C.block| char* {
      char* buffer = (char*)malloc(sizeof(char) * 50);
      strncpy(buffer, "Hello from C safely!", 49);
      buffer[49] = '\0';
      return buffer;
    } |]
    free -- Function to run on exit (free the pointer)
    peekCString -- Function to run with the resource (convert CString to Haskell String)
-}

hello :: IO ()
hello = putStrLn "someFunc"
