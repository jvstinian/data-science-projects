#include <iostream>
#include <unistd.h>
#include <opencv2/core.hpp>
#include <opencv2/videoio.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgproc.hpp>

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

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <video_file>" << std::endl;
        return 1;
    }
    std::string file(argv[1]);
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
    int resizeWidth = 1920 / 4;
    int resizeHeight = 1080 / 4;
    cv::String winname("opencv_contour_example_0");
    cv::String wintitle("opencv contour example");
    cv::namedWindow(winname, cv::WINDOW_NORMAL | cv::WINDOW_KEEPRATIO);
    cv::setWindowTitle(winname, wintitle);
    cv::resizeWindow(winname, resizeWidth, resizeHeight);
    
    /* Open video file */
    vcptr->open(cv::String(file), 0);
    if (!vcptr->grab()) {
        std::cerr << "Grab not successful" << std::endl;
        exit(2);
    }

    int ct = 0;
    cv::Mat rawimagem, cumdiffframe; /* cv::Mat() */
    cv::Mat framem, grayframem, grayblurm, lastframem, framedeltam;
    cv::Mat tempcumdiff;
    /* Mat for thresholds */
    cv::Mat threshm, thresh2m; /* cv::Mat(); */
    double decayrate = 0.2; /* 0.4 works pretty well with threshold val 5.  0.8 works very well with threshold val 5 for unnormalized series sum. */
    double fdmin, fdmax, cdmin, cdmax;
       
    /* Initial frames */ 
    if (!vcptr->retrieve(rawimagem, 0)) {
        std::cerr << "Unable to retrieve image" << std::endl;
        exit(1);
    }
    /* Frames are processed by resizing to 480x270, converting to grayscale,
     * and applying Gaussian blur. */
    cv::resize(rawimagem, framem, cv::Size2i(resizeWidth, resizeHeight), 0.0, 0.0, cv::INTER_AREA);
    cv::cvtColor(framem, grayframem, cv::COLOR_BGR2GRAY, 0);
    cv::GaussianBlur(grayframem, grayblurm, cv::Size(21, 21), 0.0, 0.0);
    cumdiffframe = cv::Mat::zeros(grayblurm.size(), grayblurm.type());
    grayblurm.copyTo(lastframem);

    while (1) {
        /* Grab is necessary to move to the next image */
        if (!vcptr->grab()) {
            /* Finished processing frames. */
            break;
        }
        if (!vcptr->retrieve(rawimagem, 0)) {
            std::cerr << "Unable to retrieve image" << std::endl;
            break;
        }
        if (verbose && (ct == 0)) {
            /* We print frame information following the approach used in the Haskell code
             * for the methods matInfo and unmarshalFlags. */
            std::cout << "Flags: " << rawimagem.flags << std::endl;
            std::cout << "Depth: " << (rawimagem.flags & cv::Mat::DEPTH_MASK) << std::endl;
            std::cout << "Channels: " << 1 + ((rawimagem.flags >> CV_CN_SHIFT) & (CV_CN_MAX - 1)) << std::endl;
            std::cout << "Dims: " << rawimagem.dims << std::endl;
            std::cout << "Dims from size: " << rawimagem.size.dims() << std::endl;
            std::cout << "Shape: ";
            for (int i = 0; i < rawimagem.size.dims(); i++) {
                std::cout << rawimagem.size[i] << ", ";
            }
            std::cout << std::endl;
        }

        cv::resize(rawimagem, framem, cv::Size2i(resizeWidth, resizeHeight), 0.0, 0.0, cv::INTER_AREA);

        if (verbose && (ct == 0)) {
            /* NOTE: Height and width are reversed in the dimensions array, e.g. the following yields
             *       "Frame Shape: 270, 480," */
            std::cout << "Resized Frame Depth: " << (framem.flags & cv::Mat::DEPTH_MASK) << std::endl;
            std::cout << "Resized Frame Channels: " << 1 + ((framem.flags >> CV_CN_SHIFT) & (CV_CN_MAX - 1)) << std::endl;
            std::cout << "Resized Frame Shape: ";
            for (int i = 0; i < framem.size.dims(); i++) {
                std::cout << framem.size[i] << ", ";
            }
            std::cout << std::endl;
        }

        cv::cvtColor(framem, grayframem, cv::COLOR_BGR2GRAY, 0);
        cv::GaussianBlur(grayframem, grayblurm, cv::Size(21, 21), 0.0, 0.0);

        /* Compare current blurred frame to the last and updated the
         * weighted cumulative difference frame.
         * We initially store to a temporary frame and copy to the
         * target frame below. */
        cv::absdiff(grayblurm, lastframem, framedeltam);
        cv::addWeighted(cumdiffframe, decayrate, framedeltam, (1.0 - decayrate), 0.0, tempcumdiff, cumdiffframe.depth());

        cv::minMaxLoc(framedeltam, &fdmin, &fdmax, NULL, NULL);
        cv::minMaxLoc(tempcumdiff, &cdmin, &cdmax, NULL, NULL);
       
        /* Perhaps copyTo would be preferable */
        tempcumdiff.convertTo(cumdiffframe, cumdiffframe.depth(), 1.0, 0.0);

        /* NOTE: The tutorial uses threshold value 25 */
        /* NOTE: We use threshValMode to follow the Haskell implementation.
         *       This could be skipped. */
        enum cv::ThresholdTypes threshValMode = (enum cv::ThresholdTypes) 0;  /* NOTE: cv::THRESH_BINARY == 0 */
        double threshVal = 5.0;
        enum cv::ThresholdTypes threshType = cv::THRESH_BINARY;
        double threshMaxVal = 255.0;
        enum cv::ThresholdTypes finalThreshType = static_cast<enum cv::ThresholdTypes>(threshType | threshValMode);
        /* We ignore the output of the following method as the input threshVal is returned when
         * the threshold type is THRESH_BINARY (or so it appears). */
        cv::threshold(cumdiffframe, threshm, threshVal, threshMaxVal, finalThreshType);

        /* See https://docs.opencv.org/4.11.0/d4/d86/group__imgproc__filter.html#ga4ff0f3318642c4f469d0e11f242f3b6c
         * for the dilate method.
         * While we could have omitted the arguments starting with anchor since the C++ method has default values,
         * we include them here as we did end up defining them for the Haskell code.
         * The value borderValue is intended to be identical to the return of morphologyDefaultBorderValue
         * for the dilate method.
         * See https://docs.opencv.org/4.11.0/d4/d86/group__imgproc__filter.html#ga94756fad83d9d24d29c9bf478558c40a
         * for more information.
         */
        cv::Point2i anchor(-1, -1);
        cv::BorderTypes borderType = cv::BORDER_CONSTANT;
        cv::Scalar borderValue(-DBL_MAX, -DBL_MAX, -DBL_MAX, -DBL_MAX);
        cv::Mat kernel;
        cv::dilate(threshm, thresh2m, kernel, anchor, 30,  borderType, borderValue);

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
        cv::findContours(thresh2m, contours, hierarchy, contour_mode, contour_method);

        if (verbose) {
            std::cout << "contour areas: " << std::endl;
            for (std::vector<std::vector<cv::Point> >::const_iterator cit = contours.begin(); cit != contours.end(); cit++) {
                std::vector<cv::Point2f> fpts(cit->size());
                for(size_t i = 0; i < cit->size(); i++) {
                    fpts[i] = cv::Point2f((*cit)[i]);
                }
                /* ContourAreaAbsoluteValue corresponds to not oriented */
                double area = cv::contourArea(fpts, false);
                std::cout << "    contour " << cit - contours.begin() << ": " << area << std::endl;
            }
            std::cout << "contour count: " << contours.size() << std::endl;
        }

        /* We remove the contours with area less than 250.0 */
        std::vector<std::vector<cv::Point> > largecontours(contours);
        std::vector<std::vector<cv::Point> >::iterator rm_it = std::remove_if(
            largecontours.begin(), largecontours.end(), contourAreaLessThan250
        );
        largecontours.erase(rm_it, largecontours.end());
        if (verbose) {
            std::cout << "large contour count: " << largecontours.size() << std::endl;
        }

        /* NOTE: We don't need to persist the lists rectangles.
         *       We could just write the rectangles to the target frame
         *       directly in the following loop. */
        std::vector<cv::RotatedRect> rotrects(largecontours.size());
        std::vector<cv::Rect2i> bddrects(largecontours.size());
        for (size_t i = 0; i < largecontours.size(); i++) {
            rotrects[i] = cv::minAreaRect(largecontours[i]);
            bddrects[i] = rotrects[i].boundingRect();
        }
              
        cv::Scalar blue(255.0, 0.0, 0.0, 0.0);
        for (size_t i = 0; i < bddrects.size(); i++) {
            cv::rectangle(framem, bddrects[i], blue, 2, cv::LINE_8, 0);
        }

        cv::imshow(winname, framem);
        cv::waitKey(1000 / 30);

        /* Increment */
        ct++;
        grayblurm.copyTo(lastframem);
    }
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

*/

/*
Depth values can be found in
https://docs.opencv.org/4.11.0/d1/d1b/group__core__hal__interface.html#ga32b18d904ee2b1731a9416a8eef67d06

#define 	CV_8U   0
#define 	CV_8S   1
#define 	CV_16U   2
#define 	CV_16S   3
#define 	CV_32S   4
#define 	CV_32F   5
#define 	CV_64F   6
#define 	CV_16F   7
*/
