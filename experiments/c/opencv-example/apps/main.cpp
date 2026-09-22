#include <iostream>
#include <unistd.h>
#include <opencv2/core.hpp>
#include <opencv2/videoio.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgproc.hpp>
/* #include <opencv2/core/hal/interface.h> CV_CN_MAX CV_CN_SHIFT */

bool contourAreaLessThan250(const std::vector<cv::Point>& ipts) {
    std::vector<cv::Point2f> fpts(ipts.size());
    for(size_t i = 0; i < ipts.size(); i++) {
        fpts[i] = cv::Point2f(ipts[i]);
    }
    // The Haskell literal ContourAreaAbsoluteValue corresponds to not oriented
    // (i.e., false in the following)
    return cv::contourArea(fpts, false) < 250.0;
}

bool print_video_properties(const std::string& file) {
    std::cout << "Working with file " << file << std::endl;
    cv::VideoCapture* vcptr = new cv::VideoCapture();
    vcptr->open(cv::String(file), 0);
    
    // Video capture properties
    std::cout << "Width: " << (int) vcptr->get(cv::CAP_PROP_FRAME_WIDTH) << std::endl;
    std::cout << "Height: " << (int) vcptr->get(cv::CAP_PROP_FRAME_HEIGHT) << std::endl;
    std::cout << "Pos Frames: " << (int) vcptr->get(cv::CAP_PROP_POS_FRAMES) << std::endl;
    std::cout << "fourcc: " << (int) vcptr->get(cv::CAP_PROP_FOURCC) << std::endl;
    std::cout << "Is open: " << (bool) vcptr->isOpened() << std::endl;
    std::cout << "Grab successful: " << (bool) vcptr->grab() << std::endl;

    cv::Mat* mptr = new cv::Mat();
    bool ok = vcptr->retrieve(*mptr, 0);
    if (ok) {
        std::cout << "Able to retrieve image!" << std::endl;
    } else {
        std::cout << "Unable to retrieve image" << std::endl;
    }
    delete mptr;
    std::cout << "PropMode: " << (int) vcptr->get(cv::CAP_PROP_MODE) << std::endl;
    vcptr->release();
    return ok;
}

int main() {
    std::string file("./videos/1744035803-video.mp4");
    bool verbose(true);

    if (verbose && !print_video_properties(file)) {
        std::cerr << "Error working with file " << file << std::endl;
        exit(1);
    }

    cv::VideoCapture* vcptr = new cv::VideoCapture();

    /* See https://hackage-content.haskell.org/package/opencv-0.0.2.1/src/src/OpenCV/HighGui.hsc
     * It seems that the window name is made by first obtaining a unique string,
     * then hashing so that an integer is obtained, and
     * then converting the integer to a string.
     * We instead just use "opencv_contour_example_0" for now.
    */
    /* Also, in Haskell makeWindow specifies that
       mouseCallback and trackbars are empty.
       We omit the latter here.
    */
    cv::String winname("opencv_contour_example_0");
    cv::String wintitle("opencv contour example");
    cv::namedWindow(winname, cv::WINDOW_NORMAL | cv::WINDOW_KEEPRATIO);
    cv::setWindowTitle(winname, wintitle);
    cv::resizeWindow(winname, 1920 / 4, 1080 / 4);
    
    /* Open video file */
    vcptr->open(cv::String(file), 0);
    if (!vcptr->grab()) {
        std::cerr << "Grab not successful" << std::endl;
        exit(2);
    }

    int ct = 0;
    cv::Mat* rawimagep = new cv::Mat();
    cv::Mat* cumdiffframe0 = new cv::Mat();
    cv::Mat* framep = new cv::Mat();
    cv::Mat* grayframep = new cv::Mat();
    cv::Mat* grayblurp = new cv::Mat();
    cv::Mat* lastframep = new cv::Mat();
    cv::Mat* framedelta = new cv::Mat();
    cv::Mat* cumdiffframeDouble = new cv::Mat();
    double decayrate = 0.2; /* 0.4 works pretty well with threshold val 5.  0.8 works very well with threshold val 5 for unnormalized series sum. */
    double fdmin, fdmax, cdmin, cdmax;
       
    /* Initial frames */ 
    if (!vcptr->retrieve(*rawimagep, 0)) {
        std::cerr << "Unable to retrieve image" << std::endl;
        exit(1);
    }
    // Should the height be 270 in the following? 
    cv::resize(*rawimagep, *framep, cv::Size2i(480, 262), 0.0, 0.0, cv::INTER_AREA);
    cv::cvtColor(*framep, *grayframep, cv::COLOR_BGR2GRAY, 0);
    cv::GaussianBlur(*grayframep, *grayblurp, cv::Size(21, 21), 0.0, 0.0);
    *cumdiffframe0 = cv::Mat::zeros(grayblurp->size(), grayblurp->type());
    grayblurp->copyTo(*lastframep);

    while (1) {
        /* Grab is necessary to move to the next image */
        if (!vcptr->grab()) {
            std::cerr << "Grab not successful" << std::endl;
            exit(2);
        }
        if (!vcptr->retrieve(*rawimagep, 0)) {
            std::cerr << "Unable to retrieve image" << std::endl;
            break;
        }
        if (verbose && (ct == 0)) {
            /* We print frame information following the approach used in the Haskell code
             * for the methods matInfo and unmarshalFlags. */
            std::cout << "Flags: " << rawimagep->flags << std::endl;
            std::cout << "Depth: " << (rawimagep->flags & cv::Mat::DEPTH_MASK) << std::endl;
            std::cout << "Channels: " << 1 + ((rawimagep->flags >> CV_CN_SHIFT) & (CV_CN_MAX - 1)) << std::endl;
            std::cout << "Dims: " << rawimagep->dims << std::endl;
            std::cout << "Dims from size: " << rawimagep->size.dims() << std::endl;
            std::cout << "Shape: ";
            for (int i = 0; i < rawimagep->size.dims(); i++) {
                std::cout << rawimagep->size[i] << ", ";
            }
            std::cout << std::endl;
        }

        cv::resize(*rawimagep, *framep, cv::Size2i(480, 262 /*Should this be 270?*/), 0.0, 0.0, cv::INTER_AREA);

        if (verbose && (ct == 0)) {
            /* NOTE: Height and width are reversed in the dimensions array, e.g. the following yields
             *       "Frame Shape: 262, 480" */
            std::cout << "Resized Frame Depth: " << (framep->flags & cv::Mat::DEPTH_MASK) << std::endl;
            std::cout << "Resized Frame Channels: " << 1 + ((framep->flags >> CV_CN_SHIFT) & (CV_CN_MAX - 1)) << std::endl;
            std::cout << "Resized Frame Shape: "; /* NOTE: This appears to specify the expected height and width */
            for (int i = 0; i < framep->size.dims(); i++) {
                std::cout << framep->size[i] << ", ";
            }
            std::cout << std::endl;
        }

        cv::cvtColor(*framep, *grayframep, cv::COLOR_BGR2GRAY, 0);
        /* TODO: Remove.  Just gives 0 for depth and 1 for channels 
        std::cout << "Gray Depth: " << (grayframep->flags & cv::Mat::DEPTH_MASK) << std::endl;
        std::cout << "Gray Channels: " << 1 + ((grayframep->flags >> CV_CN_SHIFT) & (CV_CN_MAX - 1)) << std::endl;
        */

        cv::GaussianBlur(*grayframep, *grayblurp, cv::Size(21, 21), 0.0, 0.0);
        /* TODO: Remove.  Just gives 0 for depth and 1 for channels 
        std::cout << "Blur Depth: " << (grayblurp->flags & cv::Mat::DEPTH_MASK) << std::endl;
        std::cout << "Blur Channels: " << 1 + ((grayblurp->flags >> CV_CN_SHIFT) & (CV_CN_MAX - 1)) << std::endl;
        */

        cv::absdiff(*grayblurp, *lastframep, *framedelta);
        cv::addWeighted(*cumdiffframe0, decayrate, *framedelta, (1.0 - decayrate), 0.0, *cumdiffframeDouble, cumdiffframe0->depth());

        cv::minMaxLoc(*framedelta, &fdmin, &fdmax, NULL, NULL);
        cv::minMaxLoc(*cumdiffframeDouble, &cdmin, &cdmax, NULL, NULL);
       
        /* Perhaps copyTo would be preferable */
        cumdiffframeDouble->convertTo(*cumdiffframe0, cumdiffframe0->depth(), 1.0, 0.0);

        /* NOTE: The tutorial uses threshold value 25 */
        cv::Mat* threshp = new cv::Mat();
        enum cv::ThresholdTypes threshValMode = (enum cv::ThresholdTypes) 0;  /* NOTE: cv::THRESH_BINARY == 0 */
        double threshVal = 5.0;
        enum cv::ThresholdTypes threshType = cv::THRESH_BINARY;
        double threshMaxVal = 255.0;
        enum cv::ThresholdTypes finalThreshType = static_cast<enum cv::ThresholdTypes>(threshType | threshValMode);
        /* Discard the output of the following the input threshVal is returned when
         * the threshold type is THRESH_BINARY (or so it appears). */
        cv::threshold(*cumdiffframe0, *threshp, threshVal, threshMaxVal, finalThreshType);
        /* std::cout << "Threshold return value: " << calcThresh << std::endl; TODO */

        /* See https://docs.opencv.org/4.11.0/d4/d86/group__imgproc__filter.html#ga4ff0f3318642c4f469d0e11f242f3b6c
         * for the dilate method.
         * While we could have omitted the arguments starting with anchor since the C++ method has default values,
         * we include them here as we did end up defining them for the Haskell code.
         * The value borderValue is intended to be identical to the return of morphologyDefaultBorderValue
         * for the dilate method.
         * See https://docs.opencv.org/4.11.0/d4/d86/group__imgproc__filter.html#ga94756fad83d9d24d29c9bf478558c40a
         * for more information.
         */
        cv::Mat* thresh2p = new cv::Mat();
        cv::Point2i anchor(-1, -1);
        cv::BorderTypes borderType = cv::BORDER_CONSTANT;
        cv::Scalar borderValue(-DBL_MAX, -DBL_MAX, -DBL_MAX, -DBL_MAX);
        cv::Mat kernel;
        cv::dilate(*threshp, *thresh2p, kernel, anchor, 30,  borderType, borderValue);
        delete threshp;

        if (verbose) {
            std::cout << "wa double mat min: " << cdmin << std::endl
                      << "wa double mat max: " << cdmax << std::endl
                      << "framedelta mat min: " << fdmin << std::endl
                      << "framedelta mat max: " << fdmax << std::endl;
        }

        enum cv::RetrievalModes contour_mode = cv::RETR_EXTERNAL;
        enum cv::ContourApproximationModes contour_method = cv::CHAIN_APPROX_SIMPLE;

        std::vector<std::vector<cv::Point> > contours;
        std::vector<cv::Vec4i> hierarchy;
        cv::findContours(*thresh2p, contours, hierarchy, contour_mode, contour_method);
              
        if (verbose) {
            std::cout << "contour areas: " << std::endl;
            for (std::vector<std::vector<cv::Point> >::const_iterator cit = contours.begin(); cit != contours.end(); cit++) {
                std::vector<cv::Point2f> fpts(cit->size());
                for(size_t i = 0; i < cit->size(); i++) {
                    fpts[i] = cv::Point2f((*cit)[i]);
                }
                double area = cv::contourArea(fpts, false); // ContourAreaAbsoluteValue corresponds to not oriented
                std::cout << "    contour " << cit - contours.begin() << ": " << area << std::endl;
            }
            std::cout << "contour count: " << contours.size() << std::endl;
        }

        delete thresh2p;
        
        std::vector<std::vector<cv::Point> > largecontours(contours);
        std::vector<std::vector<cv::Point> >::iterator rm_it = std::remove_if(
            largecontours.begin(), largecontours.end(), contourAreaLessThan250
        );
        largecontours.erase(rm_it, largecontours.end());
        if (verbose) {
            std::cout << "large contour count: " << largecontours.size() << std::endl;
        }

        /* The following logic can probably be condensed */
        std::vector<cv::RotatedRect> rotrects(largecontours.size());
        for (size_t i = 0; i < largecontours.size(); i++) {
            rotrects[i] = cv::minAreaRect(largecontours[i]);
        }
        std::vector<cv::Rect2i> bddrects(largecontours.size());
        for (size_t i = 0; i < largecontours.size(); i++) {
            bddrects[i] = rotrects[i].boundingRect();
        }
              
        cv::Scalar blue(255.0, 0.0, 0.0, 0.0);
        for (size_t i = 0; i < bddrects.size(); i++) {
            cv::rectangle(*framep, bddrects[i], blue, 2, cv::LINE_8, 0);
        }

        cv::imshow(winname, *framep);
        cv::waitKey(1000 / 30);

        /* Increment */
        ct++;
        grayblurp->copyTo(*lastframep);
        if (ct >= 10) break;
    }
    delete lastframep;
    delete cumdiffframe0;
    delete framedelta;
    delete cumdiffframeDouble;
    vcptr->release();
    cv::destroyWindow(winname);
    
    delete vcptr;
    return 0;
}

/*
Haskell Method Implementations (method name in HTML anchor):
* https://hackage.haskell.org/package/opencv-0.0.2.1/docs/src/OpenCV-Core-Types.html#rotatedRectBoundingRect
* https://hackage.haskell.org/package/opencv-0.0.2.1/docs/src/OpenCV-Core-Types-Mat.html#emptyMat
* https://hackage.haskell.org/package/opencv-0.0.2.1/docs/src/OpenCV-Core-Types-Mat.html#matConvertTo
* https://hackage.haskell.org/package/opencv-0.0.2.1/docs/src/OpenCV-ImgProc-ImgFiltering.html#dilate
* https://hackage.haskell.org/package/opencv-0.0.2.1/docs/src/OpenCV-ImgProc-MiscImgTransform.html#threshold
* https://hackage.haskell.org/package/opencv-0.0.2.1/docs/src/OpenCV-ImgProc-StructuralAnalysis.html#findContours

OpenCV ThresholdTypes:
https://docs.opencv.org/4.11.0/d7/d1b/group__imgproc__misc.html#gaa9e58d2860d4afa658ef70a9b1115576

marshalThreshType :: ThreshType -> (Int32, CDouble)
marshalThreshType = \case
    Thresh_Binary    maxVal -> (c'THRESH_BINARY    , realToFrac maxVal)
    Thresh_BinaryInv maxVal -> (c'THRESH_BINARY_INV, realToFrac maxVal)
    Thresh_Truncate         -> (c'THRESH_TRUNC     , 0)
    Thresh_ToZero           -> (c'THRESH_TOZERO    , 0)
    Thresh_ToZeroInv        -> (c'THRESH_TOZERO_INV, 0)

data ThreshValue
   = ThreshVal_Abs !Double
   | ThreshVal_Otsu
   | ThreshVal_Triangle
     deriving (Show, Eq)

marshalThreshValue :: ThreshValue -> (Int32, CDouble)
marshalThreshValue = \case
    ThreshVal_Abs val  -> (0                , realToFrac val)
    ThreshVal_Otsu     -> (c'THRESH_OTSU    , 0)
    ThreshVal_Triangle -> (c'THRESH_TRIANGLE, 0)
*/

/*
#define 	CV_8U   0
#define 	CV_8S   1
#define 	CV_16U   2
#define 	CV_16S   3
#define 	CV_32S   4
#define 	CV_32F   5
#define 	CV_64F   6
#define 	CV_16F   7
*/
/* Note the last item isn't listed for the depth() method in
 * https://docs.opencv.org/4.11.0/d3/d63/classcv_1_1Mat.html#a8da9f853b6f3a29d738572fd1ffc44c0
 * It is listed on
 * https://docs.opencv.org/4.11.0/d1/d1b/group__core__hal__interface.html
 * though.
*/
/*
https://hackage-content.haskell.org/package/opencv-0.0.2.1/src/src/OpenCV/Internal/Core/Types/Mat/Marshal.hsc

unmarshalFlags :: Int32 -> (Depth, Int32)
unmarshalFlags n =
    ( unmarshalDepth $ n .&. c'CV_MAT_DEPTH_MASK
    , 1 + ((n `unsafeShiftR` c'CV_CN_SHIFT) .&. (c'CV_CN_MAX - 1))
    )
*/

/* resize 
https://hackage-content.haskell.org/package/opencv-0.0.2.1/src/src/OpenCV/ImgProc/GeometricImgTransform.hsc
    let frame      = exceptError $ resize (ResizeAbs (toSize (V2 480 262))) InterArea image

resize
    :: ResizeAbsRel
    -> InterpolationMethod
    -> Mat ('S [height, width]) channels depth
    -> CvExcept (Mat ('S ['D, 'D]) channels depth)
resize factor interpolationMethod src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src   $ \srcPtr   ->
      withPtr dst   $ \dstPtr   ->
      withPtr dsize $ \dsizePtr ->
        [cvExcept|
          cv::resize
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , *$(Size2i * dsizePtr)
          , $(double fx)
          , $(double fy)
          , $(int32_t c'interpolation)
          );
        |]
  where
    (dsize, fx, fy) = marshalResizeAbsRel factor
    c'interpolation = marshalInterpolationMethod interpolationMethod


marshalResizeAbsRel
    :: ResizeAbsRel
    -> (Size2i, CDouble, CDouble)
marshalResizeAbsRel (ResizeAbs s) = (s, 0   , 0   )
marshalResizeAbsRel (ResizeRel f) = (s, c'fx, c'fy)
  where
    s :: Size2i
    s = toSize (zero :: V2 Int32)

    (V2 c'fx c'fy) = realToFrac <$> f
*/

/*
https://hackage-content.haskell.org/package/opencv-0.0.2.1/src/src/OpenCV/Internal/ImgProc/Types.hsc

marshalInterpolationMethod :: InterpolationMethod -> Int32
marshalInterpolationMethod = \case
   InterNearest  -> c'INTER_NEAREST
   InterLinear   -> c'INTER_LINEAR
   InterCubic    -> c'INTER_CUBIC
   InterArea     -> c'INTER_AREA
   InterLanczos4 -> c'INTER_LANCZOS4
*/

/*
import OpenCV.Core.Types (ToScalar(toScalar), FreezeThaw(thaw, freeze), rotatedRectBoundingRect)
import OpenCV.Core.Types.Point (Point2i, IsPoint(toPoint, fromPoint), Point2f)
import OpenCV.Core.Types.Mat (coerceMat, matInfo, Mat, cloneMat, matConvertTo)
import OpenCV.Core.Types.Rect (Rect2i)
import OpenCV.TypeLevel (DS(S))
import OpenCV.VideoIO.Types ({-VideoCaptureAPI(..), -}VideoCaptureProperties(..), FourCC(..))
import OpenCV.HighGui
import Control.Concurrent (threadDelay)
import OpenCV.Core.ArrayOps (matAbsDiff, matAdd, matScalarMult, matAddWeighted, minMaxLoc, matScalarMult)
import OpenCV.ImgProc.Types (InterpolationMethod(InterArea), BorderMode(BorderConstant))
import OpenCV.ImgProc.ImgFiltering (gaussianBlur, dilate)
import OpenCV.ImgProc.MiscImgTransform (cvtColor, threshold, ThreshValue(ThreshVal_Abs), ThreshType(Thresh_Binary))
import OpenCV.ImgProc.MiscImgTransform.ColorCodes (bgr, gray)
import OpenCV.ImgProc.GeometricImgTransform (resize, ResizeAbsRel(ResizeAbs))
import OpenCV.Core.Types.Size (IsSize(toSize))
import OpenCV.ImgProc.Drawing (LineType(LineType_8), rectangle)
import Linear.V2 () -- instances, mainly for fmap
import Linear.V2 (V2(..))
import Linear.V4 (V4(V4))
import OpenCV.VideoIO.VideoCapture 
  ( VideoCaptureSource(VideoFileSource)
  , newVideoCapture
  , videoCaptureOpen
  , videoCaptureIsOpened
  , videoCaptureGrab
  , videoCaptureRetrieve
  , videoCaptureGetD
  , videoCaptureGetI
  {-, videoCaptureSetD
  , videoCaptureSetI -}
  , videoCaptureRelease )
import OpenCV.ImgProc.StructuralAnalysis 
  ( ContourRetrievalMode(ContourRetrievalExternal)
  , ContourApproximationMethod(ContourApproximationSimple)
  , ContourAreaOriented(ContourAreaAbsoluteValue)
  , Contour(contourPoints)
  , findContours
  , contourArea
  , minAreaRect )
import OpenCV.Exception (exceptErrorIO, exceptError)

main :: IO ()
main = do
    let 
        file = "./videos/1710073869-video.mp4"
        source = VideoFileSource file Nothing
    putStrLn $ "Working with file " ++ file
    _ <- bracket (newVideoCapture >>= (open_vc source))
            (exceptErrorIO . videoCaptureRelease)
            -- (\vc -> videoCaptureGrab vc >>= (putStrLn . show))
            video_processor
    
    lvc <- newVideoCapture
    emptyvalE <- runExceptT . (flip videoCaptureOpen source) $ lvc
    case emptyvalE of
      Left _ -> putStrLn "Encountered error opening video file"
      Right _ -> putStrLn "Successfully opened file"
    emptyvalE2 <- runExceptT . videoCaptureRelease $ lvc
    case emptyvalE2 of
      Left _ -> putStrLn "Encountered error releasing video file"
      Right _ -> putStrLn "Successfully released file"

    window <- makeWindow "wookie"
    resizeWindow window (1920 `div` 4) (1080 `div` 4) -- 1920 1080
    _ <- bracket (newVideoCapture >>= (open_vc source))
                 (exceptErrorIO . videoCaptureRelease)
                 (write_to_window Nothing window)
    threadDelay $ 1 * 1000000 -- 5 seconds
    destroyWindow window
    
  where open_vc vsource vc = do
          exceptErrorIO . (flip videoCaptureOpen vsource) $ vc
          return vc
        video_processor vc = do
          -- setwidthCheck <- videoCaptureSetI vc VideoCapPropFrameWidth 1920
          -- putStrLn $ "Set width check: " ++ show setwidthCheck
          widthD <- videoCaptureGetD vc VideoCapPropFrameWidth
          widthI <- videoCaptureGetI vc VideoCapPropFrameWidth
          putStrLn $ "Width (D): " ++ show widthD
          putStrLn $ "Width (I): " ++ show widthI
          heightD <- videoCaptureGetD vc VideoCapPropFrameHeight
          heightI <- videoCaptureGetI vc VideoCapPropFrameHeight
          putStrLn $ "Height (D): " ++ show heightD
          putStrLn $ "Height (I): " ++ show heightI
          posFramesD <- videoCaptureGetD vc VideoCapPropPosFrames
          posFramesI <- videoCaptureGetI vc VideoCapPropPosFrames
          putStrLn $ "posFrames (D): " ++ show posFramesD
          putStrLn $ "posFrames (I): " ++ show posFramesI
          fourccI <- videoCaptureGetI vc VideoCapPropFourCc
          putStrLn $ "fourcc (I): " ++ show (FourCC fourccI)
          videoCaptureIsOpened vc >>= (putStrLn . ("Is open: "++) .  show)
          videoCaptureGrab vc >>= (putStrLn . ("Grab successful: "++) . show)
          imageM <- videoCaptureRetrieve vc
          case imageM of
            Nothing -> putStrLn "Unable to retrieve image"
            _       -> putStrLn "Able to retrieve image!"
          propModeD <- videoCaptureGetD vc VideoCapPropMode
          propModeI <- videoCaptureGetI vc VideoCapPropMode
          putStrLn $ "PropMode (D): " ++ show propModeD
          putStrLn $ "PropMode (I): " ++ show propModeI
          return "Finished" --  :: String -- specify type to suppress compiler warnings 

        write_to_window firstframeM window vc = do
          videoCaptureGrab vc >>= (putStrLn . ("Grab successful: "++) . show)
          imageM <- videoCaptureRetrieve vc
          case imageM of
            Nothing    -> do
              putStrLn "Unable to retrieve image"
              return "Finished" -- :: String
            Just image -> do
              -- putStrLn "Showing image"
              putStrLn $ show $ matInfo image
              let frame      = exceptError $ resize (ResizeAbs (toSize (V2 480 262))) InterArea image
                  -- Use ShapeT in the following
                  cframe     = (exceptError $ coerceMat frame) :: Mat ('S ['S 262, 'S 480]) ('S 3) ('S Word8)
                  grayframe  = exceptError $ cvtColor bgr gray cframe
                  grayblur   = exceptError $ gaussianBlur (toSize (V2 21 21)) 0.0 0.0 grayframe 
                  zeromat    = matAbsDiff grayblur grayblur -- TODO: Maybe try another way if this works
                  (ct, cumdiffframe0, baseframe) = fromMaybe (0 :: Int32, zeromat, grayblur) firstframeM
                  framedelta = matAbsDiff grayblur baseframe
                  
                  decayrate = 0.2 -- 0.4 works pretty well with threshold val 5.  0.8 works very well with threshold val 5 for unnormalized series sum.
                  totalweight = (1.0 - decayrate) / (1.0 - (decayrate ^^ (ct + 2) )) -- TODO: Note the +2 rather than +1 to fix the weighted average below
                  prevweight = (1.0 - decayrate) / (1.0 - (decayrate ^^ (ct + 1) )) -- TODO: Note the +2 rather than +1 to fix the weighted average below
                  {- matConvertTo seems to be for conversion, not entirely clear on coerceMat
                  cumdiffframeDouble = exceptError $ matConvertTo Nothing Nothing cumdiffframe0 :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Double)
                  -- cumdiffframe = exceptError $ coerceMat cumdiffframeDouble :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Word8)
                  cumdiffframe = exceptError $ matConvertTo Nothing Nothing cumdiffframe0 :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Word8)
                  -}
                  cumdiffframeDouble = exceptError $ matAddWeighted (exceptError $ matConvertTo Nothing Nothing cumdiffframe0 :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Double))
                                                                    decayrate -- (decayrate*totalweight/prevweight)  -- (decayrate*totalweight) -- TODO: What do we use here?
                                                                    (exceptError $ matConvertTo Nothing Nothing framedelta :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Double))
                                                                    (1.0 - decayrate) -- totalweight -- TODO: What do we use here?
                                                                    (0.0 :: Double) :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Double)
                  -- cumdiffframe = exceptError $ coerceMat cumdiffframeDouble :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Word8)
                  (fdmin, fdmax, _, _) = exceptError $ minMaxLoc framedelta
                  (cdmin, cdmax, _, _) = exceptError $ minMaxLoc cumdiffframeDouble
                  -- TODO: When we use a scalar multiple multipliers, it appears to modify cumdiffframeDouble as though it were mutable
                  -- cumdiffframe = exceptError $ matConvertTo Nothing {-(Just (255.0/cdmax))-} Nothing (matScalarMult cumdiffframeDouble (255.0/cdmax)) :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Word8)
                  cumdiffframe = exceptError $ matConvertTo Nothing {-(Just (255.0/cdmax))-} Nothing cumdiffframeDouble :: Mat ('S ['S 262, 'S 480]) ('S 1) ('S Word8)
                  -- (cdmin2, cdmax2, _, _) = exceptError $ minMaxLoc cumdiffframe
                  
                  (thresh, _ {-threshret-}) = exceptError $ threshold (ThreshVal_Abs 5) (Thresh_Binary 255) cumdiffframe -- framedelta -- TODO: The tutorial uses threshold value 25
                  -- TODO: Arguments in the following follow the example and use defaults based on https://docs.opencv.org/3.4.20/d4/d86/group__imgproc__filter.html
                  -- thresh2 = dilate thresh Nothing (Nothing :: Maybe Point2i) 2 BorderConstant 
                  thresh2 = exceptError $ dilate thresh Nothing (Just (toPoint (V2 (-1) (-1))):: Maybe Point2i) 30 (BorderConstant morphologyDefaultBorderValue)
                  adjbaseframe = if (ct <= 0) 
                    then 
                      grayblur
                    else
                      -- matAdd (matScalarMult baseframe (decayrate * totalweight)) (matScalarMult grayblur totalweight)
                      -- matScalarMult (matAdd baseframe (matScalarMult grayblur (decayrate ^^ ct))) totalweight
                      -- exceptError $ matAddWeighted baseframe totalweight grayblur (totalweight * (decayrate ^^ ct)) (0.0 :: Double)
                      -- matScalarMult (matAdd baseframe (matScalarMult grayblur (decayrate ^^ ct))) totalweight
                      -- exceptError $ matAddWeighted baseframe 0.5 grayblur 0.5 (0.0 :: Double)
                      grayblur
                      -- baseframe
                  -- adjbaseframe = matAdd (matScalarMult baseframe decayrate) grayblur 
                  -- adjbaseframe = matAdd (matScalarMult baseframe (decayrate * (1.0 - decayrate))) (matScalarMult grayblur decayrate)
              -- putStrLn $ ("threshold return value: " ++) . show $ threshret
              putStrLn $ ("wa double mat min: " ++) . show $ cdmin
              putStrLn $ ("wa double mat max: " ++) . show $ cdmax
              -- putStrLn $ ("cumdiff mat min: " ++) . show $ cdmin2
              -- putStrLn $ ("cumdiff mat max: " ++) . show $ cdmax2
              putStrLn $ ("framedelta mat min: " ++) . show $ fdmin
              putStrLn $ ("framedelta mat max: " ++) . show $ fdmax
              putStrLn $ ("totalweight: " ++) . show $ totalweight
              putStrLn $ ("decay to power: " ++) . show $ decayrate ^^ (ct + 1)
              contours <- (thaw thresh2 >>= findContours ContourRetrievalExternal ContourApproximationSimple)
              putStrLn $ ("contour areas: " ++) . show $ fmap getContourArea contours
              putStrLn $ ("contour count: " ++) . show $ length contours
              let largecontours = V.filter ((>= 250) . getContourArea) contours -- TODO: Previously we used 400
              putStrLn $ ("large contour count: " ++) . show $ length largecontours
              let rotrects = fmap (minAreaRect . contourPoints) largecontours
                  bddrects = fmap rotatedRectBoundingRect rotrects
              -- lcontours = filter  contours
              output <- imageWithRects cframe bddrects
              -- imshow window thresh2 -- grayframe -- grayblur -- TODO: Decide what to do here
              -- imshow window baseframe -- grayframe -- grayblur -- TODO: Decide what to do here
              -- remove the following when ready
              {-
              mimg <- thaw $ cloneMat cframe
              V.forM_ bddrects (\rect -> rectangle mimg rect blue 2 LineType_8 0)
              output <- freeze mimg
              -}
              -- imshow window cumdiffframeDouble 
              imshow window output 
              -- end of remove
              _ <- waitKey (1000 `div` 30)
              write_to_window (Just (ct+1, cumdiffframe, adjbaseframe)) window vc 
        morphologyDefaultBorderValue = toScalar (V4 val val val val) 
            where val = -(maxValue :: Double)
        pointConversion :: Point2i -> Point2f
        pointConversion ipoint = toPoint $ fmap fromIntegral (fromPoint ipoint :: V2 Int32) 
        getContourArea contour = exceptError $ contourArea (fmap pointConversion (contourPoints contour)) ContourAreaAbsoluteValue

        imageWithRects :: Mat ('S [height, width]) channels depth -> V.Vector Rect2i -> IO (Mat ('S [height, width]) channels depth)
        imageWithRects img rects = do
          let blue = V4 255.0 0.0 0.0 0.0 :: V4 Double
              cimg = cloneMat img
          mimg <- thaw $ cimg -- cloneMat img
          V.forM_ rects (\rect -> rectangle mimg rect blue 2 LineType_8 0)
          freeze mimg
*/
