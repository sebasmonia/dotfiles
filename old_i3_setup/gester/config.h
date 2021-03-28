#ifndef CONFIG_H_INCLUDED
#define CONFIG_H_INCLUDED
        /*path to the input event, need to be part of input group in order to read*/
        char devname[] = "/dev/input/by-id/usb-Wacom_Co._Ltd._Pen_and_multitouch_sensor-event-if00";
        /*path to accelerometer data*/
        std::string accelpath = "/sys/bus/iio/devices/"; //in there the program looks for a folder named iio:device0 or 1
        std::string xrawdata = "in_accel_x_raw"; // in iio:device* there should be these files with the respective data
        std::string yrawdata = "in_accel_y_raw";
        /*maximum values x and y coordinate can have, usually bottom right of screen and can be found using evtest on the input device*/
        int xmax = 11747;
        int ymax = 6607;
        /*minimum length required for something to be a swipe*/
        double swipetolerance = 0.15;
        /*value to scale the angle in degrees for e.g. changing volume*/
        int anglescaling = 2;
        /*another value to set the stepsize for rotations*/
        int anglestepping = 5;
        /*how much the center of the imagined circle may move for a rotation*/
        int comdisttolerance = 1500;
        /*offset from the edge where a swipe is still an edge swipe*/
        int offsetbottom = 10;
        int offsettop = 10;
        int offsetleft = 10;
        int offsetright = 10;
        /*an array of commands to use within the program*/
        char* commands[] = {
            "", /*1 finger from bottom*/
            "", /*1 finger from top*/
            "", /*1 finger from right*/
            "", /*1 finger from left*/
            "", /*2 finger swipe from bottom*/
            "", /*2 finger swipe from top*/
            "", /*2 finger swipe from right*/
            "", /*2 finger swipe from left*/
            "", /*2 finger swipe down*/
            "", /*2 finger swipe up*/
            "", /*2 finger swipe right*/
            "", /*2 finger swipe left*/
            "", /*2 finger rotation left*/
            "", /*2 finger rotation right*/
            "", /*3 finger swipe from bottom*/
            "", /*3 finger swipe from top*/
            "", /*3 finger swipe from right*/
            "", /*3 finger swipe from left*/
            "", /*3 finger swipe down*/
            "/home/hoagie/.config/i3/toggle_xfce4-panel.py", /*3 finger swipe up*/
            "/home/hoagie/.config/i3/go_to_workspace.py prev", /*3 finger swipe right*/
            "/home/hoagie/.config/i3/go_to_workspace.py next", /*3 finger swipe left*/
            "", /*3 finger rotation left*/
            "", /*3 finger rotation right*/
            "/home/hoagie/.config/i3/rotate.py flip ", /*4 finger swipe from bottom*/
            "/home/hoagie/.config/i3/rotate.py flip ", /*4 finger swipe from top*/
            "", /*4 finger swipe from right*/
            "", /*4 finger swipe from left*/
            "", /*4 finger swipe down*/
            "", /*4 finger swipe up*/
            "", /*4 finger swipe right*/
            "", /*4 finger swipe left*/
            "/home/hoagie/.config/i3/rotate.py left ", /*4 finger rotation left*/
            "/home/hoagie/.config/i3/rotate.py right ", /*4 finger rotation right*/
            "", /*5 finger swipe from bottom*/
            "", /*5 finger swipe from top*/
            "", /*5 finger swipe from right*/
            "", /*5 finger swipe from left*/
            "", /*5 finger swipe down*/
            "", /*5 finger swipe up*/
            "", /*5 finger swipe right*/
            "", /*5 finger swipe left*/
            "", /*5 finger rotation left*/
            "", /*5 finger rotation right*/
        };

#endif // CONFIG_H_INCLUDED
