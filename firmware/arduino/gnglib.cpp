/**
 * GNG Library Implementation
 * 
 * This file implements the core functionality for a Go/No-Go operant conditioning
 * apparatus, including protocol management, data serialization, and non-volatile
 * storage operations.
 * 
 * The system supports behavioral training protocols with multiple phases,
 * precise timing control, and comprehensive data logging.
 */

#include <time.h>
#include <Preferences.h>
#include "gnglib.h"

// Global preferences object for non-volatile storage
Preferences nvs;

/**
 * Check if Non-Volatile Storage (NVS) is enabled for protocol persistence
 * @return true if NVS should be used, false otherwise
 */
bool nvs_get_useNVS() {
    nvs.begin("nvs_params", true);  // Open in read-only mode
    bool useNVS = nvs.getBool("useNVS", false);  // Default to false if not set
    nvs.end();
    return useNVS;
}

/**
 * Enable or disable Non-Volatile Storage usage
 * @param useNVS true to enable NVS, false to disable
 */
void nvs_set_useNVS(bool useNVS) {
    nvs.begin("nvs_params", false);  // Open in read-write mode
    nvs.putBool("useNVS", useNVS);
    nvs.end();
}

/**
 * Load protocol parameters from Non-Volatile Storage
 * Falls back to default values if NVS data is not available
 * @param protocol Reference to protocol structure to populate
 * @param default_protocol Default values to use if NVS data is missing
 */
void nvs_load_protocol(protocol_t& protocol, const protocol_t& default_protocol) {
    nvs.begin("protocol", true);  // Open protocol namespace in read-only mode
    
    // Load all protocol parameters with fallback to defaults
    protocol.phase = nvs.getUChar("phase", default_protocol.phase);
    protocol.test_phase = nvs.getBool("test_phase", default_protocol.test_phase);
    
    // Servo startup delays
    protocol.start_delay_bup = nvs.getULong("start_delay_bup", default_protocol.start_delay_bup);
    protocol.start_delay_bdown = nvs.getULong("start_delay_bdown", default_protocol.start_delay_bdown);
    
    // Session timing control
    protocol.expctrl_start_time_of_day = nvs.getBool("expctrl_start_time_of_day", default_protocol.expctrl_start_time_of_day);
    protocol.hour_start = nvs.getULong("hour_start", default_protocol.hour_start);
    protocol.minute_start = nvs.getULong("minute_start", default_protocol.minute_start);
    protocol.expctrl_end_time_of_day = nvs.getBool("expctrl_end_time_of_day", default_protocol.expctrl_end_time_of_day);
    protocol.hour_end = nvs.getULong("hour_end", default_protocol.hour_end);
    protocol.minute_end = nvs.getULong("minute_end", default_protocol.minute_end);
    
    // Session and trial quantity limits
    protocol.expctrl_n_sessions = nvs.getBool("expctrl_n_sessions", default_protocol.expctrl_n_sessions);
    protocol.total_session_n = nvs.getULong("total_session_n", default_protocol.total_session_n);
    protocol.expctrl_n_trials = nvs.getBool("expctrl_n_trials", default_protocol.expctrl_n_trials);
    protocol.total_trial_n = nvs.getULong("total_trial_n", default_protocol.total_trial_n);
    
    // Precue parameters
    protocol.do_precue = nvs.getBool("do_precue", default_protocol.do_precue);
    protocol.t_precue = nvs.getULong("t_precue", default_protocol.t_precue);
    protocol.delay_precue_cue_min = nvs.getULong("delay_precue_cue_min", default_protocol.delay_precue_cue_min);
    protocol.delay_precue_cue_max = nvs.getULong("delay_precue_cue_max", default_protocol.delay_precue_cue_max);
    protocol.color_precue = nvs.getULong("color_precue", default_protocol.color_precue);
    
    // LED blinking parameters
    protocol.cue_blink = nvs.getBool("cue_blink", default_protocol.cue_blink);
    protocol.blink_t_on = nvs.getLong("blink_t_on", default_protocol.blink_t_on);
    protocol.blink_t_off = nvs.getLong("blink_t_off", default_protocol.blink_t_off);
    
    // Main cue parameters
    protocol.t_cue = nvs.getULong("t_cue", default_protocol.t_cue);
    protocol.color_cue = nvs.getULong("color_cue", default_protocol.color_cue);
    
    // Reward parameters
    protocol.t_reward = nvs.getULong("t_reward", default_protocol.t_reward);
    protocol.color_reward = nvs.getULong("color_reward", default_protocol.color_reward);
    
    // Inter-trial interval parameters
    protocol.t_iti_min = nvs.getULong("t_iti_min", default_protocol.t_iti_min);
    protocol.t_iti_max = nvs.getULong("t_iti_max", default_protocol.t_iti_max);
    protocol.color_iti = nvs.getULong("color_iti", default_protocol.color_iti);
    
    // Session management
    protocol.intersession_delay = nvs.getULong("intersession_delay", default_protocol.intersession_delay);
    protocol.color_idle = nvs.getULong("color_idle", default_protocol.color_idle);
    
    // LED brightness control
    protocol.led_brightness = nvs.getUChar("led_brightness", default_protocol.led_brightness);
    protocol.led_idle_brightness = nvs.getUChar("led_idle_brightness", default_protocol.led_idle_brightness);
    
    // Timing randomization
    protocol.use_entire_range = nvs.getBool("use_entire_range", default_protocol.use_entire_range);
    
    nvs.end();
}

/**
 * Save current protocol parameters to Non-Volatile Storage
 * @param protocol Reference to protocol structure to save
 */
void nvs_store_protocol(protocol_t& protocol) {
    nvs.begin("protocol", false);  // Open protocol namespace in read-write mode
    
    // Store all protocol parameters
    nvs.putUChar("phase", protocol.phase);
    nvs.putBool("test_phase", protocol.test_phase);
    
    // Servo startup delays
    nvs.putULong("start_delay_bup", protocol.start_delay_bup);
    nvs.putULong("start_delay_bdown", protocol.start_delay_bdown);
    
    // Session timing control
    nvs.putBool("expctrl_start_time_of_day", protocol.expctrl_start_time_of_day);
    nvs.putULong("hour_start", protocol.hour_start);
    nvs.putULong("minute_start", protocol.minute_start);
    nvs.putBool("expctrl_end_time_of_day", protocol.expctrl_end_time_of_day);
    nvs.putULong("hour_end", protocol.hour_end);
    nvs.putULong("minute_end", protocol.minute_end);
    
    // Session and trial quantity limits
    nvs.putBool("expctrl_n_sessions", protocol.expctrl_n_sessions);
    nvs.putULong("total_session_n", protocol.total_session_n);
    nvs.putBool("expctrl_n_trials", protocol.expctrl_n_trials);
    nvs.putULong("total_trial_n", protocol.total_trial_n);
    
    // Precue parameters
    nvs.putBool("do_precue", protocol.do_precue);
    nvs.putULong("t_precue", protocol.t_precue);
    nvs.putULong("delay_precue_cue_min", protocol.delay_precue_cue_min);
    nvs.putULong("delay_precue_cue_max", protocol.delay_precue_cue_max);
    nvs.putULong("color_precue", protocol.color_precue);
    
    // LED blinking parameters
    nvs.putBool("cue_blink", protocol.cue_blink);
    nvs.putLong("blink_t_on", protocol.blink_t_on);
    nvs.putLong("blink_t_off", protocol.blink_t_off);
    
    // Main cue parameters
    nvs.putULong("t_cue", protocol.t_cue);
    nvs.putULong("color_cue", protocol.color_cue);
    
    // Reward parameters
    nvs.putULong("t_reward", protocol.t_reward);
    nvs.putULong("color_reward", protocol.color_reward);
    
    // Inter-trial interval parameters
    nvs.putULong("t_iti_min", protocol.t_iti_min);
    nvs.putULong("t_iti_max", protocol.t_iti_max);
    nvs.putULong("color_iti", protocol.color_iti);
    
    // Session management
    nvs.putULong("intersession_delay", protocol.intersession_delay);
    nvs.putULong("color_idle", protocol.color_idle);
    
    // LED brightness control
    nvs.putUChar("led_brightness", protocol.led_brightness);
    nvs.putUChar("led_idle_brightness", protocol.led_idle_brightness);
    
    // Timing randomization
    nvs.putBool("use_entire_range", protocol.use_entire_range);
    
    nvs.end();
}

/**
 * Convert state character code to human-readable string
 * @param state Character representing the behavioral state
 * @return Pointer to descriptive string, or NULL if state not found
 */
const char* state_char_to_string(char state) {
    switch (state) {
        case char_states.precue:
            return strings_states.precue;
        case char_states.interval_precue_cue:
            return strings_states.interval_precue_cue;
        case char_states.cue:
            return strings_states.cue;
        case char_states.interval_intertrial:
            return strings_states.interval_intertrial;
        case char_states.reward:
            return strings_states.reward;
        case char_states.interval_intersession:
            return strings_states.interval_intersession;
        case char_states.idle:
            return strings_states.idle;
        case char_states.halted:
            return strings_states.halted;
        default:
            return NULL;  // Unknown state
    }
}

/**
 * Convert epoch time to ISO 8601 timestamp string
 * Format: YYYY-MM-DDTHH:MM:SS.ssssss
 * @param seconds Seconds since epoch
 * @param useconds Microseconds component
 * @param ts Buffer to store timestamp string (must be at least 27 chars)
 * @param length Size of the timestamp buffer
 * @return 0 on success, error code otherwise
 */
int epoch_to_timestamp(time_t seconds, int32_t useconds, char* ts, size_t length) {
    struct tm tinfo;
    
    // Check buffer size
    if (length < 27) {
        Serial.println("ERROR: Timestamp buffer too small!");
        return 2;
    }
    
    // Convert to local time structure
    localtime_r(&seconds, &tinfo);
    
    // Format date and time portion
    strftime(ts, 20, "%Y-%m-%dT%H:%M:%S", &tinfo);
    
    // Add microseconds
    snprintf(ts + 19, 8, ".%06d", useconds);
    
    return 0;
}

/**
 * Test if all specified bits are set in a FreeRTOS event group
 * Used to check if all conditions for experiment execution are met
 * @param evgroup_handle Handle to the event group
 * @param test_bits Bit mask of conditions to test
 * @return true if all specified bits are set, false otherwise
 */
bool test_evgroup(EventGroupHandle_t evgroup_handle, const EventBits_t test_bits) {
    const EventBits_t evgroup_state = xEventGroupGetBits(evgroup_handle);
    
    // Check if all test bits are set in the event group
    return (evgroup_state & test_bits) == test_bits;
}

/**
 * Check if experiment should continue based on session count limits
 * @param current_protocol Protocol containing session limits
 * @param current_session Current session number (1-based)
 * @return true if more sessions should be run, false if limit reached
 */
bool check_n_sessions(const protocol_t& current_protocol, uint32_t current_session) {
    // If session limiting is not enabled, always continue
    if (!current_protocol.expctrl_n_sessions) return true;
    
    // Continue if we haven't reached the session limit
    return current_session < current_protocol.total_session_n;
}

/**
 * Check if session should continue based on trial count limits
 * @param current_protocol Protocol containing trial limits
 * @param current_trial Current trial number (1-based)
 * @return true if more trials should be run, false if limit reached
 */
bool check_n_trials(const protocol_t& current_protocol, uint32_t current_trial) {
    // If trial limiting is not enabled, always continue
    if (!current_protocol.expctrl_n_trials) return true;
    
    // Continue if we haven't reached the trial limit
    return current_trial < current_protocol.total_trial_n;
}

/**
 * Serialize protocol structure to JSON string
 * @param protocol_struct Protocol data to serialize
 * @param prettify Whether to format JSON with indentation and newlines
 * @param buffsize Reference to store the buffer size
 * @return Pointer to dynamically allocated JSON string (caller must delete[])
 */
char* serialize_protocol_t(const protocol_t& protocol_struct, bool prettify, size_t& buffsize) {
    StaticJsonDocument<768> doc;  // Adjust size based on protocol complexity
    JsonObject json_protocol = doc.createNestedObject("protocol");
    
    // Serialize all protocol parameters
    json_protocol["phase"] = protocol_struct.phase;
    json_protocol["test_phase"] = protocol_struct.test_phase;
    json_protocol["start_delay_bup"] = protocol_struct.start_delay_bup;
    json_protocol["start_delay_bdown"] = protocol_struct.start_delay_bdown;
    json_protocol["expctrl_start_time_of_day"] = protocol_struct.expctrl_start_time_of_day;
    json_protocol["hour_start"] = protocol_struct.hour_start;
    json_protocol["minute_start"] = protocol_struct.minute_start;
    json_protocol["expctrl_end_time_of_day"] = protocol_struct.expctrl_end_time_of_day;
    json_protocol["hour_end"] = protocol_struct.hour_end;
    json_protocol["minute_end"] = protocol_struct.minute_end;
    json_protocol["expctrl_n_sessions"] = protocol_struct.expctrl_n_sessions;
    json_protocol["total_session_n"] = protocol_struct.total_session_n;
    json_protocol["expctrl_n_trials"] = protocol_struct.expctrl_n_trials;
    json_protocol["total_trial_n"] = protocol_struct.total_trial_n;
    json_protocol["do_precue"] = protocol_struct.do_precue;
    json_protocol["t_precue"] = protocol_struct.t_precue;
    json_protocol["delay_precue_cue_min"] = protocol_struct.delay_precue_cue_min;
    json_protocol["delay_precue_cue_max"] = protocol_struct.delay_precue_cue_max;
    json_protocol["color_precue"] = protocol_struct.color_precue;
    json_protocol["cue_blink"] = protocol_struct.cue_blink;
    json_protocol["blink_t_on"] = protocol_struct.blink_t_on;
    json_protocol["blink_t_off"] = protocol_struct.blink_t_off;
    json_protocol["t_cue"] = protocol_struct.t_cue;
    json_protocol["color_cue"] = protocol_struct.color_cue;
    json_protocol["t_reward"] = protocol_struct.t_reward;
    json_protocol["color_reward"] = protocol_struct.color_reward;
    json_protocol["t_iti_min"] = protocol_struct.t_iti_min;
    json_protocol["t_iti_max"] = protocol_struct.t_iti_max;
    json_protocol["color_iti"] = protocol_struct.color_iti;
    json_protocol["intersession_delay"] = protocol_struct.intersession_delay;
    json_protocol["color_idle"] = protocol_struct.color_idle;
    json_protocol["led_brightness"] = protocol_struct.led_brightness;
    json_protocol["led_idle_brightness"] = protocol_struct.led_idle_brightness;
    json_protocol["use_entire_range"] = protocol_struct.use_entire_range;
    json_protocol["is_currently_running"] = protocol_struct.is_currently_running;
    
    // Allocate buffer and serialize
    if (prettify) {
        buffsize = measureJsonPretty(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJsonPretty(doc, buffer, buffsize);
        return buffer;
    } else {
        buffsize = measureJson(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJson(doc, buffer, buffsize);
        return buffer;
    }
}

/**
 * Serialize session structure to JSON string
 * @param session_struct Session data to serialize
 * @param prettify Whether to format JSON with indentation and newlines
 * @param buffsize Reference to store the buffer size
 * @return Pointer to dynamically allocated JSON string (caller must delete[])
 */
char* serialize_session_t(const session_t& session_struct, bool prettify, size_t& buffsize) {
    StaticJsonDocument<256> doc;
    JsonObject json_session = doc.createNestedObject("session");
    
    // Convert timestamp to ISO format
    char isotime[27];
    epoch_to_timestamp(session_struct.start_s, session_struct.start_us, isotime, 27);
    json_session["t_start"] = isotime;
    
    // Session metadata
    json_session["duration"] = session_struct.duration;
    json_session["experiment_number"] = session_struct.experiment_number;
    json_session["session_number"] = session_struct.session_number;
    
    // Performance counters
    json_session["count_licks"] = session_struct.count_licks;
    json_session["count_impulsive_nosepokes"] = session_struct.count_impulsive_nosepokes;
    json_session["count_lever_total"] = session_struct.count_lever_total;
    json_session["count_lever_iti"] = session_struct.count_lever_iti;
    json_session["count_correct"] = session_struct.count_correct;
    json_session["count_fail"] = session_struct.count_fail;
    json_session["count_precue_correct"] = session_struct.count_precue_correct;
    json_session["count_precue_fail"] = session_struct.count_precue_fail;
    
    // Trial categorization
    json_session["trial_count_total"] = session_struct.trial_count_total;
    json_session["trial_count_good"] = session_struct.trial_count_good;
    json_session["trial_count_impulsive"] = session_struct.trial_count_impulsive;
    json_session["trial_count_abandoned"] = session_struct.trial_count_abandoned;
    
    // Allocate buffer and serialize
    if (prettify) {
        buffsize = measureJsonPretty(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJsonPretty(doc, buffer, buffsize);
        return buffer;
    } else {
        buffsize = measureJson(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJson(doc, buffer, buffsize);
        return buffer;
    }
}

/**
 * Serialize trial structure to JSON string
 * @param trial_struct Trial data to serialize
 * @param prettify Whether to format JSON with indentation and newlines
 * @param buffsize Reference to store the buffer size
 * @return Pointer to dynamically allocated JSON string (caller must delete[])
 */
char* serialize_trial_t(const trial_t& trial_struct, bool prettify, size_t& buffsize) {
    StaticJsonDocument<512> doc;
    JsonObject json_trial = doc.createNestedObject("trial");
    
    // Convert timestamp to ISO format
    char isotime[27];
    epoch_to_timestamp(trial_struct.start_s, trial_struct.start_us, isotime, 27);
    json_trial["t_start"] = isotime;
    
    // Trial identification
    json_trial["trial_n"] = trial_struct.trial_n;
    
    // Timing measurements
    json_trial["iti"] = trial_struct.iti;
    json_trial["iti_extension"] = trial_struct.iti_extension;
    json_trial["delay_precue_cue"] = trial_struct.delay_precue_cue;
    
    // Performance measurements
    json_trial["precue_correct"] = trial_struct.precue_correct;
    json_trial["precue_reaction_time"] = trial_struct.precue_reaction_time;
    json_trial["impulsive_nosepokes"] = trial_struct.impulsive_nosepokes;
    json_trial["correct"] = trial_struct.correct;
    json_trial["reaction_time"] = trial_struct.reaction_time;
    
    // Reward consumption
    json_trial["licked"] = trial_struct.licked;
    json_trial["time_to_lick"] = trial_struct.time_to_lick;
    json_trial["total_lick_t"] = trial_struct.total_lick_t;
    
    // Additional timing data
    json_trial["precue_nosepoke_wait"] = trial_struct.precue_nosepoke_wait;
    
    // Sensor readings for diagnostic purposes
    json_trial["iti_max_touchread"] = trial_struct.iti_max_touchread;
    json_trial["iti_mean_touchread"] = trial_struct.iti_mean_touchread;
    json_trial["precue_max_touchread"] = trial_struct.precue_max_touchread;
    json_trial["precue_mean_touchread"] = trial_struct.precue_mean_touchread;
    json_trial["cue_max_touchread"] = trial_struct.cue_max_touchread;
    json_trial["cue_mean_touchread"] = trial_struct.cue_mean_touchread;
    json_trial["reward_max_touchread"] = trial_struct.reward_max_touchread;
    json_trial["reward_mean_touchread"] = trial_struct.reward_mean_touchread;
    
    // Allocate buffer and serialize
    if (prettify) {
        buffsize = measureJsonPretty(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJsonPretty(doc, buffer, buffsize);
        return buffer;
    } else {
        buffsize = measureJson(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJson(doc, buffer, buffsize);
        return buffer;
    }
}

/**
 * Serialize event structure to JSON string
 * @param event_struct Event data to serialize
 * @param prettify Whether to format JSON with indentation and newlines
 * @param buffsize Reference to store the buffer size
 * @return Pointer to dynamically allocated JSON string (caller must delete[])
 */
char* serialize_event_t(const event_t& event_struct, bool prettify, size_t& buffsize) {
    StaticJsonDocument<384> doc;
    JsonObject json_event = doc.createNestedObject("event");
    
    // Convert timestamp to ISO format
    char isotime[27];
    epoch_to_timestamp(event_struct.start_s, event_struct.start_us, isotime, 27);
    json_event["t_start"] = isotime;
    
    // Event context
    json_event["state"] = state_char_to_string(event_struct.state);
    
    // Event type mapping
    switch (event_struct.event_name) {
        case 'P':
            json_event["event_name"] = "EVENT:NOSEPOKE";
            break;
        case 'L':
            json_event["event_name"] = "EVENT:BOTTLELICK";
            break;
        default:
            json_event["event_name"] = "EVENT:UNKNOWN";
            break;
    }
    
    // Allocate buffer and serialize
    if (prettify) {
        buffsize = measureJsonPretty(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJsonPretty(doc, buffer, buffsize);
        return buffer;
    } else {
        buffsize = measureJson(doc) + 1;
        char* buffer = new char[buffsize];
        serializeJson(doc, buffer, buffsize);
        return buffer;
    }
}