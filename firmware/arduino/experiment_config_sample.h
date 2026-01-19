// MQTT topics for session, trial, and event data, based on device ID
#define TOPIC_SESSIONS "gng/" DEVICE_ID "/sessions"
#define TOPIC_TRIALS "gng/" DEVICE_ID "/trials"
#define TOPIC_EVENTS "gng/" DEVICE_ID "/events"

// Servo control degrees for bottle dispense positions
#define BOTTLE_LOWER_DEG 55 // angle to lower the bottle
#define BOTTLE_RAISE_DEG 90 // angle to raise the bottle

// Step size and delay for servo movement (smooth transitions)
#define SERVO_STEP_SIZE 8 // degrees per step
#define SERVO_STEP_LATENCY 20 // milliseconds delay per step

// Protocol settings
#define CUE_BLINK 1 // enable blinking cue (set to 0 to disable)

// Experiment timing variables (in milliseconds)
#define START_DELAY_BOTTLE_DOWN 10*1000 // delay before bottle lowers (10 seconds)
//#define START_DELAY_BOTTLE_UP 20*60*1000 // (commented out) longer delay, possibly for another protocol
#define START_DELAY_BOTTLE_UP 5*1000 // delay before bottle raises (5 seconds, currently used)

// Number of experimental sessions to run in one period
#define N_SESSIONS 24

//#define SESSION_DELAY 10*60*1000 // (commented out) delay between sessions, for alternate protocols
#define SESSION_DELAY 0 // delay between sessions currently set to zero

// Cue and event timing (milliseconds)
#define PRECUE_TIME 10*1000 // precue duration
#define CUE_TIME 10*1000 // duration of main cue signal
#define REWARD_TIME 500 // duration the reward is available
#define ITI_TIME_MIN 5*1000 // minimum intertrial interval
#define ITI_TIME_MAX 5*1000 // maximum intertrial interval (fixed here, but could be randomized)

// Experiment schedule controls
#define DAY 18 // experiment day of the month
#define HOUR_START 2 // session start hour (e.g., 2 = 2 AM; 18 would be 6 PM)

#define DO_PRECUE 1 // whether to show a precue (0/1)

// LED color settings for cues and apparatus states (using FastLED CRGB defines)
#define CUE_COLOR_PRECUE CRGB::Red
#define CUE_COLOR_PRECUE_NAME "Red"

#define CUE_COLOR_GO CRGB::Yellow
//#define CUE_COLOR_GO (127,127,0) // alternate manual specification (commented out)
#define CUE_COLOR_GO_NAME "Yellow"

#define CUE_COLOR_NOGO CRGB::Blue
//#define CUE_COLOR_NOGO (0,0,255) // alternate manual specification (commented out)
#define CUE_COLOR_NOGO_NAME "Blue"

// LED colors for states during experiment (inter-trial, reward, idle)
#define ITI_COLOR CRGB::Black
//#define ITI_COLOR (0,0,0) // alternate manual specification (commented out)
#define ITI_COLOR_NAME "Black"

#define REWARD_COLOR CRGB::Black
//#define REWARD_COLOR (0,0,0) // alternate manual specification (commented out)
#define REWARD_COLOR_NAME "Black"

#define IDLE_COLOR CRGB::Black
//#define IDLE_COLOR (0,0,0) // alternate manual specification (commented out)
#define IDLE_COLOR_NAME "Black"

// LED brightness settings (out of 255)
#define LED_BRIGHTNESS 10 // active state brightness
#define LED_IDLE_BRIGHTNESS 7 // idle state brightness

// Blinking parameters for cues (timing in milliseconds)
#define BLINK_TON 100 // cue ON duration per blink
#define BLINK_TOFF 100 // cue OFF duration per blink
