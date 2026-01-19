#ifndef GNG_H
#define GNG_H

#include "Arduino.h"
#include <ArduinoJson.h>
#include <Preferences.h>

// Maximum length of trial sequence array, plus 1 for null terminator
#define MAX_TRIAL_N 128+1

// State character definitions for behavioral task phases
#define PRECUE 'A'      // Precue presentation state
#define PC_DELAY 'B'    // Delay between precue and main cue
#define CUE 'G'         // Main cue presentation (Go signal)
#define ITI 'I'         // Inter-trial interval
#define REWARD 'R'      // Reward dispensing state
#define ISD 'S'         // Inter-session delay
#define IDLE 'X'        // Idle/waiting state
#define HALT 'H'        // Halted/stopped state

/**
 * Structure containing human-readable string names for each behavioral state
 * Used for logging and MQTT communication
 */
struct states_t {
    const char* precue;                 // "POINT:PRECUE" - animal must respond
    const char* interval_precue_cue;    // "INTVL:PRECUE-CUE" - delay period
    const char* cue;                    // "POINT:CUE" - main cue presentation
    const char* interval_intertrial;    // "INTVL:INTERTRIAL" - between trials
    const char* reward;                 // "POINT:REWARD" - reward delivery
    const char* interval_intersession;  // "INTVL:INTERSESSION" - between sessions
    const char* idle;                   // "INTVL:IDLE" - system idle
    const char* halted;                 // "INTVL:HALTED" - system stopped
};

/**
 * Structure containing single character codes for each behavioral state
 * Used for efficient state tracking and communication
 */
struct char_states_t {
    char precue;                 // 'A'
    char interval_precue_cue;    // 'B'  
    char cue;                    // 'G'
    char interval_intertrial;    // 'I'
    char reward;                 // 'R'
    char interval_intersession;  // 'S'
    char idle;                   // 'X'
    char halted;                 // 'H'
};

/**
 * Structure for MQTT control messages
 * Contains buffer and size for incoming command messages
 */
struct controlmsg_t {
    char* buffer;        // Message content buffer
    size_t buffer_size;  // Size of the message buffer
};

/**
 * Main protocol structure defining all experimental parameters
 * This structure controls the entire behavioral paradigm
 */
struct protocol_t {
    // Protocol identification and testing
    uint8_t phase;           // Training phase (1-5, or 255 for custom)
    bool test_phase;         // Whether this is a test session vs training

    // Startup delays for servo positioning
    uint32_t start_delay_bup;    // Delay after raising bottle (ms)
    uint32_t start_delay_bdown;  // Delay after lowering bottle (ms)

    // Session timing control - start time
    bool expctrl_start_time_of_day;  // Whether to use scheduled start time
    uint32_t hour_start;             // Start hour (24-hour format)
    uint32_t minute_start;           // Start minute

    // Session timing control - end time  
    bool expctrl_end_time_of_day;    // Whether to use scheduled end time
    uint32_t hour_end;               // End hour (24-hour format)
    uint32_t minute_end;             // End minute

    // Session quantity control
    bool expctrl_n_sessions;     // Whether to limit number of sessions
    uint32_t total_session_n;    // Maximum number of sessions

    // Trial quantity control
    bool expctrl_n_trials;       // Whether to limit trials per session
    uint32_t total_trial_n;      // Maximum trials per session

    // Precue parameters
    bool do_precue;                     // Whether to include precue phase
    uint32_t t_precue;                  // Precue duration (ms, 0=infinite)
    uint32_t delay_precue_cue_min;      // Minimum delay before cue (ms)
    uint32_t delay_precue_cue_max;      // Maximum delay before cue (ms)
    uint32_t color_precue;              // Precue LED color

    // Cue blinking parameters
    bool cue_blink;          // Whether cue should blink
    int32_t blink_t_on;      // Blink on duration (ms)
    int32_t blink_t_off;     // Blink off duration (ms)

    // Main cue parameters
    uint32_t t_cue;          // Cue presentation duration (ms, 0=infinite)
    uint32_t color_cue;      // Cue LED color

    // Reward parameters
    uint32_t t_reward;       // Reward availability duration (ms)
    uint32_t color_reward;   // Reward LED color

    // Inter-trial interval parameters
    uint32_t t_iti_min;      // Minimum ITI duration (ms)
    uint32_t t_iti_max;      // Maximum ITI duration (ms)  
    uint32_t color_iti;      // ITI LED color

    // Session management
    uint32_t intersession_delay;  // Delay between sessions (ms)
    uint32_t color_idle;          // Idle state LED color

    // LED control
    uint8_t led_brightness;       // Active LED brightness (0-255)
    uint8_t led_idle_brightness;  // Idle LED brightness (0-255)

    // Timing randomization
    bool use_entire_range;        // True: random in range, False: min or max only

    // Runtime status
    bool is_currently_running;    // Whether protocol is currently active
};

// Macro helpers for string conversion
#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

/**
 * Current state tracking structure
 * Maintains the current behavioral state and trial number
 */
struct current_state_t {
    char state_name;     // Current state character code
    uint32_t trial_n;    // Current trial number (1-based)
};

/**
 * Session data structure
 * Contains all data collected during a single behavioral session
 */
struct session_t {
    time_t start_s;      // Session start time (seconds since epoch)
    int32_t start_us;    // Session start time microseconds

    uint32_t duration;           // Total session duration (ms)
    uint32_t experiment_number;  // Experiment sequence number
    uint32_t session_number;     // Session sequence number

    // Performance counters
    uint32_t count_licks;                 // Total licks detected
    uint32_t count_impulsive_nosepokes;   // Nosepokes during delay periods
    uint32_t count_lever_total;           // Total nosepoke responses
    uint32_t count_lever_iti;            // Nosepokes during ITI
    uint32_t count_correct;              // Correct responses
    uint32_t count_fail;                 // Incorrect responses
    uint32_t count_precue_correct;       // Correct precue responses
    uint32_t count_precue_fail;          // Incorrect precue responses

    // Trial categorization counters
    uint32_t trial_count_total;      // Total trials attempted
    uint32_t trial_count_good;       // Successfully completed trials
    uint32_t trial_count_impulsive;  // Trials with impulsive responses
    uint32_t trial_count_abandoned;  // Trials terminated early
};

/**
 * Individual trial data structure
 * Contains detailed data for a single behavioral trial
 * Note: -2 is used as placeholder for "not applicable" values
 *       -1 is used for timeout conditions
 */
struct trial_t {
    time_t start_s;      // Trial start time (seconds since epoch)
    int32_t start_us;    // Trial start time microseconds
    uint32_t trial_n;    // Trial number within session

    // Timing measurements
    int32_t iti;                    // Actual ITI duration (ms)
    uint32_t iti_extension;         // ITI extensions due to premature responses
    uint32_t delay_precue_cue;      // Actual delay between precue and cue (ms)

    // Precue performance
    int8_t precue_correct;          // Precue response correctness (-1=N/A, 0=wrong, 1=correct)
    int32_t precue_reaction_time;   // Precue response time (ms, -1=timeout, -2=N/A)
    uint32_t precue_nosepoke_wait;  // Time spent waiting during precue-cue delay

    // Response measurements
    uint32_t impulsive_nosepokes;   // Count of premature responses
    int8_t correct;                 // Main cue response correctness (-1=N/A, 0=wrong, 1=correct)
    int32_t reaction_time;          // Main cue response time (ms, -1=timeout, -2=N/A)

    // Reward consumption
    int8_t licked;                  // Whether animal licked reward (0/1, -2=N/A)
    int32_t time_to_lick;          // Time to first lick (ms, -1=no lick, -2=N/A)
    int32_t total_lick_t;          // Total time spent licking (ms, -2=N/A)

    // Sensor readings for each phase (for monitoring sensor function)
    uint32_t iti_max_touchread;        // Maximum touch sensor reading during ITI
    uint64_t iti_mean_touchread;       // Mean touch sensor reading during ITI
    uint32_t precue_max_touchread;     // Maximum touch sensor reading during precue
    uint64_t precue_mean_touchread;    // Mean touch sensor reading during precue
    uint32_t cue_max_touchread;        // Maximum touch sensor reading during cue
    uint64_t cue_mean_touchread;       // Mean touch sensor reading during cue
    uint32_t reward_max_touchread;     // Maximum touch sensor reading during reward
    uint64_t reward_mean_touchread;    // Mean touch sensor reading during reward

    char trial_type;                // Trial type identifier ('G' for Go trials)
};

/**
 * Event logging structure
 * Records discrete behavioral events with precise timestamps
 */
struct event_t {
    time_t start_s;      // Event timestamp (seconds since epoch)
    int32_t start_us;    // Event timestamp microseconds
    char state;          // Behavioral state when event occurred
    char event_name;     // Event type ('P'=nosepoke, 'L'=lick)
};

/**
 * LED blinking control structure
 * Manages parameters for blinking LED cues
 */
struct led_blink_params_t {
    bool task_running;       // Whether blink task is currently active
    uint32_t color_on;       // LED color when on
    int32_t time_on;         // Duration of on phase (ms)
    uint32_t color_off;      // LED color when off  
    int32_t time_off;        // Duration of off phase (ms)
    bool stop_flag;          // Signal to stop blinking
    uint8_t led_state;       // Current LED state (0=off, 1=on)
};

// Predefined state string mappings
// POINT: indicates animal needs to make a response
// INTVL: indicates a passive waiting interval
constexpr states_t strings_states = {
    "POINT:PRECUE",        // Precue - animal must nosepoke
    "INTVL:PRECUE-CUE",    // Delay between precue and cue
    "POINT:CUE",           // Main cue - animal must respond appropriately
    "INTVL:INTERTRIAL",    // Inter-trial interval - no response required
    "POINT:REWARD",        // Reward delivery - animal should consume
    "INTVL:INTERSESSION",  // Between sessions
    "INTVL:IDLE",          // System idle
    "INTVL:HALTED"         // System halted
};

// Character codes corresponding to each behavioral state
constexpr char_states_t char_states = {
    'A', // precue
    'B', // interval_precue_cue  
    'G', // cue (Go signal)
    'I', // interval_intertrial
    'R', // reward
    'S', // interval_intersession
    'X', // idle
    'H'  // halted
};

// Function prototypes for protocol and data management

/**
 * Non-volatile storage functions for protocol persistence
 */
bool nvs_get_useNVS();                                                    // Check if NVS is enabled
void nvs_set_useNVS(bool useNVS);                                        // Enable/disable NVS usage
void nvs_load_protocol(protocol_t& protocol, const protocol_t& default_protocol);  // Load protocol from NVS
void nvs_store_protocol(protocol_t& protocol);                           // Save protocol to NVS

/**
 * Utility functions for state and time management
 */
const char* state_char_to_string(char state);                           // Convert state char to descriptive string
int epoch_to_timestamp(time_t seconds, int32_t useconds, char* ts);     // Convert epoch time to ISO timestamp

/**
 * FreeRTOS event group and protocol validation functions
 */
bool test_evgroup(EventGroupHandle_t evgroup_handle, EventBits_t test_bits);  // Test event group conditions
bool check_n_sessions(const protocol_t& current_protocol, uint32_t current_session);  // Validate session limits
bool check_n_trials(const protocol_t& current_protocol, uint32_t current_trial);      // Validate trial limits

/**
 * JSON serialization functions for data logging
 * These convert data structures to JSON strings for MQTT transmission
 */
char* serialize_protocol_t(const protocol_t& protocol_struct, bool prettify, size_t& buffsize);
char* serialize_session_t(const session_t& session_struct, bool prettify, size_t& buffsize);
char* serialize_trial_t(const trial_t& trial_struct, bool prettify, size_t& buffsize);
char* serialize_event_t(const event_t& event_struct, bool prettify, size_t& buffsize);

#endif