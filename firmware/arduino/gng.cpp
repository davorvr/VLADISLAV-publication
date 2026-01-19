/**
 * VLADISLAV Operant Conditioning Apparatus - Main Application
 * 
 * This is the main Arduino firmware for a home cage operant conditioning system
 * designed for behavioural neuroscience research. The system implements a VDS-like
 * paradigm in the home cage with the following components:
 * 
 * Hardware Components:
 * - ESP32-S2 microcontroller with WiFi capability
 * - Servo motor controlling water bottle position (reward delivery)
 * - Beam break sensor for nosepoke detection (main response)
 * - Capacitive touch sensor for lick detection (reward consumption)
 * - Addressable RGB LED (on ESP32 dev board) for visual cue presentation
 * 
 * Behavioral Paradigm:
 * The system implements a multi-phase training protocol:
 * 1. Basic reward association (long cue, easy response requirements)
 * 2. Shortened cue, introduction of ITI (inter-trial interval)
 * 3. Addition of precue requiring specific response timing
 * 4. Full VDS protocol with impulse control requirements
 * 5. Test sessions with varied timing parameters
 * 
 * Communication:
 * - Wireless (WiFi):
 *      - MQTT protocol for real-time data streaming and remote control (protocol loading and adjustments)
 *      - HTTP server for reading current config and uploading new compiled code (over-the-air firmware updates)
 * - Wired (USB): Serial debugging
 * 
 * Note:
 * Initially, this code was developed for a Go/No-Go paradigm. It has been adapted
 * for a VDS-like paradigm in the home cage. Some variable names and comments may still
 * reflect the original Go/No-Go design.
 * By the same token, the device initially raised or lowered the water bottle to grant or
 * withhold rewards based on the animal's behaviour. In the current design, the servo opens
 * and closes a gate to a water bottle with a fixed position. This is now referred to as
 * allowing or denying access, but some variable names may still refer to lowering or
 * raising the bottle respectively.  This is also true for the nosepoke opening using a
 * beam break sensor to detect responses to cues, which was initially intended to be a lever.
 * 
 * @author Davor Virag
 * @version 0.1
 * @date 2023
 */

// Standard library includes
#include <WiFi.h>              // ESP32 WiFi functionality
#include <AsyncMqttClient.h>   // Asynchronous MQTT client
#include <ArduinoJson.h>       // JSON parsing and serialization
#include <WebServer.h>         // HTTP server for web interface
#include <HTTPUpdateServer.h>  // Over-the-air firmware updates
#include <Adafruit_NeoPixel.h> // LED control library
#include <time.h>              // Time and date functions

// FreeRTOS includes for real-time task management
extern "C" {
    #include "freertos/FreeRTOS.h"
    #include "freertos/timers.h"
    #include "freertos/task.h"
}

// Project-specific includes
#include "gnglib.h"                      // Core behavioural apparatus functions
#include "device_id.h"                   // Unique device identifier
#include "network_config_stala.h"        // Network configuration
#include "experiment_config.h"           // Experimental protocol parameters

//=============================================================================
// CONFIGURATION AND PIN DEFINITIONS
//=============================================================================

// Debug and logging configuration
#define DEBUG                 // Enable output of debugging messages
#define LOG_TO_SERIAL         // Log messages to serial port (USB)
#define LOG_TO_MQTT           // Log messages to MQTT broker (WiFi - standard)

// Hardware pin assignments
#define LICK_PIN T14          // Capacitive touch pin for lick detection
#define SERVO_PIN 17          // PWM pin for servo motor control
#define LED_PIN 18            // Onboard addressable LED data pin
#define NOSEPOKE_PIN 1        // Analog pin for nosepoke detection

// Sensor thresholds
#define NOSEPOKE_THRESH 800   // ADC threshold for nosepoke detection
#define LICK_THRESH 35000     // Touch sensor threshold for lick detection

// Timing parameters
// #define POLLRATE_PIR 100      // PIR sensor polling rate (ms) - unused in current version
// #define POLLRATE_ENV 1000     // Environment sensor polling rate (ms) - unused
#define REWARD_DELAY 500      // Delay before reward delivery (ms)

// Servo control parameters using LEDC (LED Controller) for PWM
#define LEDC_CHANNEL 7           // LEDC channel for servo control
#define LEDC_TIMER_PRECISION 10  // PWM resolution in bits
#define LEDC_BASE_FREQ 50        // PWM frequency in Hz (standard for servos)

// Servo position values (determined empirically for specific hardware)
#define SERVO_MIN 34    // Minimum servo position value
#define SERVO_MAX 116   // Maximum servo position value

// Calculate servo positions based on degrees and hardware limits
#define BOTTLE_ALLOW_VAL SERVO_MIN + (SERVO_MAX - SERVO_MIN) * BOTTLE_ALLOW_DEG / 180
#define BOTTLE_DENYOVERSHOOT_VAL SERVO_MIN + (SERVO_MAX - SERVO_MIN) * (BOTTLE_DENY_DEG + BOTTLE_DENYOVERSHOOT_DEG) / 180
#define BOTTLE_DENY_VAL SERVO_MIN + (SERVO_MAX - SERVO_MIN) * BOTTLE_DENY_DEG / 180

// Event character definitions for behavioural events
#define LEVER_PULL 'P'        // Nosepoke event identifier
#define LICK 'L'              // Lick event identifier

// Debouncing parameters to prevent multiple triggers
#define LEVER_DEBOUNCE_T 1000 // Nosepoke debounce time (ms)
#define LICK_DEBOUNCE_T 1000  // Lick debounce time (ms)

//=============================================================================
// FREERTOS EVENT GROUP BIT DEFINITIONS
//=============================================================================

/**
 * FreeRTOS event group bits for experiment control
 * These bits coordinate between different tasks to control experiment flow
 * Up to 24 bits can be used in an event group
 */

// Session status bit (set by Experimenter task)
#define EVG_SESSION_RUNNING (1 << 0)  // Indicates if a session is currently active

// Condition bits (set by monitoring tasks, read by Experimenter)
// If any of these becomes 0, the current session will terminate cleanly
#define EVG_CND_NO_HALT_MSG (1 << 1)  // Set to 0 when halt command received via MQTT
#define EVG_CND_TIME_OF_DAY (1 << 2)  // Set to 0 when outside scheduled session time

// Aggregate of all conditions that must be true for session to continue
#define EVG_ALL_CNDS (EVG_CND_NO_HALT_MSG | EVG_CND_TIME_OF_DAY)

//=============================================================================
// GLOBAL VARIABLES AND OBJECTS
//=============================================================================

// Compilation timestamp for version tracking
const char compile_time[] = __DATE__ " " __TIME__;

// Task name array for debugging and monitoring
const char *const tasks[] = { "TMQTT", "TExp", "TEvents", "THttp" };

// Network and communication objects
AsyncMqttClient mqttClient;      // MQTT client for data transmission
WebServer httpServer(80);        // HTTP server for web interface
HTTPUpdateServer httpUpdater;    // OTA update handler

// FreeRTOS timer handles for various system functions
TimerHandle_t TimerBlinkOn;      // Timer for LED blink on phase
TimerHandle_t TimerBlinkOff;     // Timer for LED blink off phase
TimerHandle_t mqttReconnectTimer;// Timer for MQTT reconnection attempts
TimerHandle_t wifiReconnectTimer;// Timer for WiFi reconnection attempts

// FreeRTOS task handles for task management and monitoring
TaskHandle_t handleBlinkLed = NULL;      // LED blinking task
TaskHandle_t handleEventListener = NULL; // Event detection task
TaskHandle_t handleExperimenter = NULL;  // Main experiment control task
TaskHandle_t handleHttpHandler = NULL;   // HTTP server task
TaskHandle_t handleMQTTMessenger = NULL; // MQTT data transmission task
TaskHandle_t handleMQTTController = NULL;// MQTT command processing task
TaskHandle_t handleLickMonitor = NULL;   // Lick detection monitoring task

// Event group for coordinating experiment execution
EventGroupHandle_t evgroupExpRun;

// Task creation response storage
BaseType_t task_create_response;

// Stack high water mark variables for debugging memory usage
uint32_t shwmExperimenter;  // Experimenter task stack usage
uint32_t shwmBlinkLed;      // Blink LED task stack usage
uint32_t shwmLickMonitor;   // Lick monitor task stack usage

// FreeRTOS queues for inter-task communication
QueueHandle_t controlmsg_queue;  // MQTT control messages
QueueHandle_t sessions_queue;    // Session data for logging
QueueHandle_t trials_queue;      // Trial data for logging
QueueHandle_t events_queue;      // Behavioral events for logging

// Semaphores for synchronization between interrupt handlers and tasks
SemaphoreHandle_t lickISR_to_exp;    // Lick detection to experimenter task
SemaphoreHandle_t lickISR_to_msg;    // Lick detection to message task
SemaphoreHandle_t leverISR_to_exp;   // Nosepoke detection to experimenter task
SemaphoreHandle_t leverISR_to_msg;   // Nosepoke detection to message task
SemaphoreHandle_t stop_lick_task;    // Signal to stop lick monitoring task

// Mutexes for thread-safe access to shared data structures
SemaphoreHandle_t http_protocol_mutex;    // Protocol structure access via HTTP
SemaphoreHandle_t modified_protocol_mutex;// Modified protocol structure access

// Timestamp buffer (minimum 27 chars for ISO format: YYYY-MM-DDTHH:MM:SS.ssssss\0)
char isotime[27];

//=============================================================================
// STATE AND CONTROL VARIABLES
//=============================================================================

// Hardware state variables
volatile char bottle_state;           // 'U' for denied ("up", raised), 'D' for allowed ("down", lowered)
volatile int32_t servo_position = BOTTLE_ALLOW_VAL;  // Current servo position

// System state variables
volatile char global_state = char_states.idle;  // Current behavioural state
volatile uint32_t global_total_experiments = 0; // Total experiment counter
volatile uint32_t total_sessions = 1;           // Session counter

// Control flags
volatile bool msg_halt = false;              // MQTT halt command received
volatile bool run_modified_protocol = false; // Use modified protocol flag
volatile uint32_t protocol_phase_load = 0;   // Protocol phase to load (0=default, 1-5=phases, 255=modified)
volatile bool use_nvs;                       // Whether to use non-volatile storage

// LED control structure for blinking behaviour
led_blink_params_t blink_params;

// Hardware objects
Adafruit_NeoPixel led(1, LED_PIN, NEO_GRB + NEO_KHZ800);  // Single NeoPixel LED

//=============================================================================
// PROTOCOL DEFINITIONS
//=============================================================================

/**
 * Hardcoded default protocol parameters
 * This serves as the baseline configuration and fallback if NVS fails
 * Parameters are loaded from experiment_config.h
 */
const protocol_t hardcoded_default_protocol = {
    PHASE,                      // uint8:  Default phase number
    TEST_PHASE,                 // bool:   Whether this is a test phase (as opposed to training)
    START_DELAY_BUP,            // uint32: Startup delay after closing bottle access
    START_DELAY_BDOWN,          // uint32: Startup delay after opening bottle access
    EXPCTRL_START_TIME_OF_DAY,  // bool:   Use scheduled start time
    HOUR_START,                 // uint32: Session start hour
    MINUTE_START,               // uint32: Session start minute
    EXPCTRL_END_TIME_OF_DAY,    // bool:   Use scheduled end time
    HOUR_END,                   // uint32: Session end hour
    MINUTE_END,                 // uint32: Session end minute
    EXPCTRL_N_SESSIONS,         // uint32: Limit number of sessions
    TOTAL_SESSION_N,            // uint32: Maximum sessions
    EXPCTRL_N_TRIALS,           // bool:   Limit trials per session
    TOTAL_TRIAL_N,              // uint32: Maximum trials per session
    DO_PRECUE,                  // bool:   Include precue phase
    T_PRECUE,                   // uint32: Precue duration
    DELAY_PRECUE_CUE_MIN,       // uint32: Minimum precue-cue delay
    DELAY_PRECUE_CUE_MAX,       // uint32: Maximum precue-cue delay
    COLOR_PRECUE,               // uint32: Precue LED color
    CUE_BLINK,                  // bool:   Enable cue blinking
    BLINK_T_ON,                 // int32:  Blink on duration
    BLINK_T_OFF,                // int32:  Blink off duration
    T_CUE,                      // uint32: Cue duration
    COLOR_CUE,                  // uint32: Cue LED color
    T_REWARD,                   // uint32: Reward availability duration
    COLOR_REWARD,               // uint32: Reward LED color
    T_ITI_MIN,                  // uint32: Minimum ITI duration
    T_ITI_MAX,                  // uint32: Maximum ITI duration
    COLOR_ITI,                  // uint32: ITI LED color
    INTERSESSION_DELAY,         // uint32: Delay between sessions
    COLOR_IDLE,                 // uint32: Idle LED color
    LED_BRIGHTNESS,             // uint8:  Active LED brightness
    LED_IDLE_BRIGHTNESS,        // uint8:  Idle LED brightness
    USE_ENTIRE_RANGE,           // bool:   Randomization method
    false                       // bool:   Currently running flag
};

// Protocol instances for different purposes
protocol_t protocol;           // Currently active protocol
protocol_t default_protocol;   // Default protocol loaded at startup
protocol_t protocol_duplicate; // Copy for HTTP interface (thread safety)
protocol_t modified_protocol;  // Modified protocol from MQTT commands

//=============================================================================
// SERVO CONTROL FUNCTIONS
//=============================================================================

/**
 * Move servo smoothly from current position to new position
 * Uses step-wise movement to reduce mechanical stress and noise
 * 
 * @param current_position Current servo position value
 * @param new_position Target servo position value
 * @return Final servo position (should equal new_position)
 */
int32_t set_servo(int32_t current_position, int32_t new_position) {
    int32_t travel_length = abs(new_position - current_position);
    int32_t n_steps = travel_length / SERVO_STEP_SIZE;
    int32_t sign = (current_position < new_position) ? 1 : -1;
    
    // Move in steps if distance is significant
    if (n_steps) {
        for (uint32_t i = 0; i < n_steps; i++) {
            current_position = current_position + (SERVO_STEP_SIZE * sign);
            ledcWrite(LEDC_CHANNEL, current_position);
            delay(SERVO_STEP_LATENCY);
        }
    }
    
    // Set final position precisely
    current_position = new_position;
    ledcWrite(LEDC_CHANNEL, current_position);
    
    return current_position;
}

/**
 * Deny access to the water bottle
 * Updates global servo position and bottle state
 */
void bottle_deny() {
    servo_position = set_servo(servo_position, BOTTLE_DENY_VAL);
    delay(10);  // Allow servo to settle
    bottle_state = 'U';
}

/**
 * Allow access to the water bottle
 * Updates global servo position and bottle state
 */
void bottle_allow() {
    servo_position = set_servo(servo_position, BOTTLE_ALLOW_VAL);
    delay(10);  // Allow servo to settle
    bottle_state = 'D';
}

//=============================================================================
// UTILITY FUNCTIONS
//=============================================================================

/**
 * Get current time and format as ISO 8601 timestamp
 * Format: YYYY-MM-DDTHH:MM:SS.ssssss
 * 
 * @param ts Buffer to store timestamp (must be at least 27 characters)
 * @param length Size of the timestamp buffer
 * @return 0 on success, error code on failure
 */
int curtime_to_timestamp(char* ts, size_t length) {
    if (length < 27) {
        send_log("ERROR: Timestamp buffer too small!");
        return 2;
    }
    
    struct tm timeinfo;
    struct timeval tv;
    
    // Get current local time
    if (!getLocalTime(&timeinfo)) {
        send_log("Failed to obtain time");
        return 1;
    }
    
    // Get microsecond precision
    if (gettimeofday(&tv, NULL) != 0) {
        send_log("Failed to obtain time (us)");
        return 1;
    }
    
    // Format timestamp
    strftime(ts, 20, "%Y-%m-%dT%H:%M:%S", &timeinfo);
    snprintf(ts + 19, 8, ".%06d", tv.tv_usec);
    
    return 0;
}

//=============================================================================
// NETWORK CONNECTION FUNCTIONS
//=============================================================================

/**
 * Connect to WiFi network with device-specific SSID
 * Attempts connection for up to 60 seconds, then restarts device on failure
 */
void connectToWifi() {
    // Construct SSID with subnet identifier for multiple networks
    const String ssid = String(WIFI_SSID_PREFIX) + String(THIRD_SUBNET_FIELD);
    
    #ifdef LOG_TO_SERIAL
    Serial.println("Connecting to Wi-Fi: ");
    Serial.println(ssid);
    #endif
    
    WiFi.begin(ssid.c_str(), WIFI_PASS);
    
    // Wait up to 60 seconds for connection
    for (int i = 0; i < 600; i++) {
        if (WiFi.status() == WL_CONNECTED) {
            #ifdef LOG_TO_SERIAL
            Serial.println("Connected.");
            #endif
            return;
        }
        delay(100);
    }
    
    // Connection failed - restart device
    #ifdef LOG_TO_SERIAL
    Serial.println("Failed to connect. Restarting...");
    #endif
    ESP.restart();
}

/**
 * Initiate connection to MQTT broker
 */
void connectToMqtt() {
    #ifdef LOG_TO_SERIAL
    Serial.println("Connecting to MQTT...");
    #endif
    mqttClient.connect();
}

//=============================================================================
// NETWORK EVENT HANDLERS
//=============================================================================

/**
 * WiFi event handler for connection state management
 * Automatically handles reconnection attempts
 */
void WiFiEvent(WiFiEvent_t event) {
    #ifdef LOG_TO_SERIAL
    Serial.printf("[WiFi-event] event: %d\n", event);
    #endif
    
    switch (event) {
        case SYSTEM_EVENT_STA_GOT_IP:
            #ifdef LOG_TO_SERIAL
            Serial.println("WiFi connected");
            Serial.println("IP address: ");
            Serial.println(WiFi.localIP());
            #endif
            // Start MQTT connection attempt
            xTimerStart(mqttReconnectTimer, 0);
            break;
            
        case SYSTEM_EVENT_STA_DISCONNECTED:
            #ifdef LOG_TO_SERIAL
            Serial.println("WiFi lost connection");
            #endif
            // Stop MQTT reconnection and start WiFi reconnection
            xTimerStop(mqttReconnectTimer, 0);
            xTimerStart(wifiReconnectTimer, 0);
            break;
    }
}

/**
 * MQTT connection established handler
 * Subscribe to control topics and announce device presence
 */
void onMqttConnect(bool sessionPresent) {
    send_log("Connected to MQTT.");
    
    // Subscribe to control channels
    mqttClient.subscribe(TOPIC_CONTROL_ALL, 1);  // Commands for all devices
    mqttClient.subscribe(TOPIC_CONTROL, 1);      // Commands for this device
    
    // Announce device presence
    mqttClient.publish(TOPIC_STATUS_ALL, 1, false, "Hello, I am " DEVICE_NAME);
}

/**
 * MQTT disconnection handler
 * Attempt reconnection if WiFi is still connected
 */
void onMqttDisconnect(AsyncMqttClientDisconnectReason reason) {
    #ifdef LOG_TO_SERIAL
    Serial.println("Disconnected from MQTT.");
    #endif
    
    if (WiFi.isConnected()) {
        xTimerStart(mqttReconnectTimer, 0);
    }
}

/**
 * MQTT subscription acknowledgment handler (currently unused)
 */
void onMqttSubscribe(uint16_t packetId, uint8_t qos) {
    // Optional: Add subscription confirmation logging
}

/**
 * MQTT publish acknowledgment handler (currently unused)
 */
void onMqttPublish(uint16_t packetId) {
    // Optional: Add publish confirmation logging
}

/**
 * MQTT message received handler
 * Processes incoming control commands and queues them for the controller task
 */
void onMqttMessage(char* topic, char* payload, AsyncMqttClientMessageProperties properties, size_t len, size_t index, size_t total) {
    // Check if this is a complete message (not fragmented)
    if ((len != total) || (index != 0)) {
        send_log("Message too long to receive, discarded!");
        return;
    }
    
    // Allocate buffer and copy message content
    char* buffer = new char[len + 1];
    size_t buffer_size = len + 1;
    memcpy(buffer, payload, len);
    buffer[len] = '\0';  // Null terminate
    
    // Package message for queue
    controlmsg_t controlmsg = {
        buffer,
        buffer_size
    };
    
    // Queue message for processing by controller task
    xQueueSend(controlmsg_queue, &controlmsg, 0);
}

//=============================================================================
// LED CONTROL FUNCTIONS
//=============================================================================

/**
 * Set LED color and brightness according to current behavioural state
 * Different colors indicate different phases of the behavioural paradigm
 * 
 * @param x Character representing the current behavioural state
 * @param protocol Pointer to current protocol for brightness settings
 */
void set_led(char x, const protocol_t *protocol) {
    led.setBrightness(protocol->led_brightness);
    
    if (x == char_states.cue) {
        // Yellow for main cue (Go signal)
        led.setPixelColor(0, 255, 255, 0);
    }
    else if (x == char_states.precue) {
        // Blue for precue signal
        led.setPixelColor(0, 0, 0, 255);
    }
    else {
        // Turn off LED for all other states (ITI, reward, etc.)
        led.clear();
    }
    
    led.show();
    delay(10);  // Allow LED to update
}

//=============================================================================
// LOGGING FUNCTIONS
//=============================================================================

/**
 * Send log message via Serial and/or MQTT
 * Dual logging allows both local debugging and remote monitoring
 * 
 * @param text String message to log
 */
void send_log(const String text) {
    String msg = String("{\"message\":\"") + text + String("\"}");
    
    #ifdef LOG_TO_SERIAL
    Serial.println(msg);
    #endif
    
    #ifdef LOG_TO_MQTT
    mqttClient.publish(TOPIC_STATUS, 1, false, msg.c_str());
    #endif
}

/**
 * Same as above, but can take numeric values directly to convert to String
 * @param number Numeric value to log
 */
void send_log(const int number) {
    String msg = String("{\"message\":") + String(number) + String("}");
    
    #ifdef LOG_TO_SERIAL
    Serial.println(msg);
    #endif
    
    #ifdef LOG_TO_MQTT
    mqttClient.publish(TOPIC_STATUS, 1, false, msg.c_str());
    #endif
}

// ============================================================================
// MAIN BEHAVIORAL TASK IMPLEMENTATIONS
// ============================================================================

/**
 * Task: Event Listener
 * 
 * This task continuously monitors hardware sensors for behavioural events:
 * - Nosepoke detection (main behavioural response)
 * - Lick detection (reward consumption)
 * 
 * Events are debounced to prevent multiple triggers and then:
 * 1. Signalled to the Experimenter task via semaphores
 * 2. Logged to the events queue for data transmission
 * 
 * @param pvParameters Unused task parameter
 */
void TaskEventListener(void* pvParameters) {
    (void)pvParameters;  // Suppress unused parameter warning
    
    // Set up periodic task timing
    TickType_t xLastWakeTime = xTaskGetTickCount();
    const TickType_t xFrequency = pdMS_TO_TICKS(10);  // 10ms polling rate
    
    // Initialize debouncing timers
    uint32_t last_lever = millis() - LEVER_DEBOUNCE_T;
    uint32_t last_lick = millis() - LICK_DEBOUNCE_T;
    
    while (1) {
        // Check for nosepoke events
        if ((analogRead(NOSEPOKE_PIN) >= NOSEPOKE_THRESH) && 
            ((millis() - last_lever) >= LEVER_DEBOUNCE_T)) {
            
            // Signal to experimenter task
            xSemaphoreGive(leverISR_to_exp);
            last_lever = millis();
            
            // Create event record for logging
            event_t event;
            timeval tv_now;
            gettimeofday(&tv_now, NULL);
            event.start_s = tv_now.tv_sec;
            event.start_us = tv_now.tv_usec;
            event.state = global_state;
            event.event_name = LEVER_PULL;
            
            // Queue event for transmission
            xQueueSend(events_queue, &event, 0);
        }
        
        // Check for lick events (signaled by lick monitoring task)
        if (xSemaphoreTake(lickISR_to_msg, 0) == pdTRUE) {
            // Create event record for logging
            event_t event;
            timeval tv_now;
            gettimeofday(&tv_now, NULL);
            event.start_s = tv_now.tv_sec;
            event.start_us = tv_now.tv_usec;
            event.state = global_state;
            event.event_name = LICK;
            
            // Queue event for transmission
            xQueueSend(events_queue, &event, 0);
        }
        
        // Maintain precise timing
        vTaskDelayUntil(&xLastWakeTime, xFrequency);
    }
}

/**
 * Task: MQTT Controller
 * 
 * This task processes incoming MQTT control messages and updates system parameters.
 * It handles commands for:
 * - Protocol loads from templates
 * - Individual parameter modifications
 * - System control signals (halt, resume, restart)
 * - Non-volatile storage operations to store protocol modifications across power cycles
 * - Manual hardware control (bottle gate)
 * 
 * @param pvParameters Unused task parameter
 */
void TaskMQTTController(void* pvParameters) {
    while (1) {
        controlmsg_t controlmsg;
        
        // Process all queued control messages
        while (xQueueReceive(controlmsg_queue, &controlmsg, 0) == pdTRUE) {
            StaticJsonDocument<768> doc;
            DeserializationError error = deserializeJson(doc, controlmsg.buffer, controlmsg.buffer_size);
            
            if (error) {
                Serial.println(controlmsg.buffer);
                send_log("deserializeJson() failed: ");
                send_log(error.c_str());
                delete[] controlmsg.buffer;
                break;
            }
            
            // If both protocol parameters and a phase preset are provided, the preset is set and the params ignored
            // Warn the user about this
            if ((!doc["phase"].isNull() || !doc["schedule"].isNull()) && !doc["protocol"].isNull()) {
                send_log("Warning: Phase presets take precedence over specific protocol params.");
                send_log("Setting only presets, ignoring protocol params entirely...");
            }
            
            // Enable/disable non-volatile storage
            if (!doc["set_useNVS"].isNull()) {
                bool read_signal = doc["set_useNVS"].as<bool>();
                use_nvs = read_signal;
                nvs_set_useNVS(use_nvs);
                send_log("use_nvs is set");
            }
            
            // Load predefined phase configuration
            if (!doc["phase"].isNull()) {
                JsonVariant phase = doc["phase"];
                protocol_phase_load = phase.as<uint32_t>();
                
                // Signal protocol change by stopping current session
                xSemaphoreTake(http_protocol_mutex, pdMS_TO_TICKS(10000));
                protocol_duplicate.is_currently_running = false;
                xSemaphoreGive(http_protocol_mutex);
                
                send_log(String("Successfully set phase ") + phase.as<String>() + 
                        String(". Will be loaded upon next halt-resume cycle."));
            }
            
            // Handle custom protocol modifications
            if (!doc["protocol"].isNull() && doc["phase"].isNull() && doc["schedule"].isNull()) {
                // Stop current session to apply changes
                xSemaphoreTake(http_protocol_mutex, pdMS_TO_TICKS(10000));
                protocol_duplicate.is_currently_running = false;
                xSemaphoreGive(http_protocol_mutex);
                
                xSemaphoreTake(modified_protocol_mutex, pdMS_TO_TICKS(10000));
                modified_protocol.phase = 255;  // Mark as custom protocol
                
                // Update only specified parameters (allows partial updates)
                if (!doc["protocol"]["test_phase"].isNull()) 
                    modified_protocol.test_phase = doc["protocol"]["test_phase"];
                if (!doc["protocol"]["start_delay_bup"].isNull()) 
                    modified_protocol.start_delay_bup = doc["protocol"]["start_delay_bup"];
                if (!doc["protocol"]["start_delay_bdown"].isNull()) 
                    modified_protocol.start_delay_bdown = doc["protocol"]["start_delay_bdown"];
                if (!doc["protocol"]["expctrl_start_time_of_day"].isNull()) 
                    modified_protocol.expctrl_start_time_of_day = doc["protocol"]["expctrl_start_time_of_day"];
                if (!doc["protocol"]["hour_start"].isNull()) 
                    modified_protocol.hour_start = doc["protocol"]["hour_start"];
                if (!doc["protocol"]["minute_start"].isNull()) 
                    modified_protocol.minute_start = doc["protocol"]["minute_start"];
                if (!doc["protocol"]["expctrl_end_time_of_day"].isNull()) 
                    modified_protocol.expctrl_end_time_of_day = doc["protocol"]["expctrl_end_time_of_day"];
                if (!doc["protocol"]["hour_end"].isNull()) 
                    modified_protocol.hour_end = doc["protocol"]["hour_end"];
                if (!doc["protocol"]["minute_end"].isNull()) 
                    modified_protocol.minute_end = doc["protocol"]["minute_end"];
                if (!doc["protocol"]["expctrl_n_sessions"].isNull()) 
                    modified_protocol.expctrl_n_sessions = doc["protocol"]["expctrl_n_sessions"];
                if (!doc["protocol"]["total_session_n"].isNull()) 
                    modified_protocol.total_session_n = doc["protocol"]["total_session_n"];
                if (!doc["protocol"]["expctrl_n_trials"].isNull()) 
                    modified_protocol.expctrl_n_trials = doc["protocol"]["expctrl_n_trials"];
                if (!doc["protocol"]["total_trial_n"].isNull()) 
                    modified_protocol.total_trial_n = doc["protocol"]["total_trial_n"];
                if (!doc["protocol"]["do_precue"].isNull()) 
                    modified_protocol.do_precue = doc["protocol"]["do_precue"];
                if (!doc["protocol"]["t_precue"].isNull()) 
                    modified_protocol.t_precue = doc["protocol"]["t_precue"];
                if (!doc["protocol"]["delay_precue_cue_min"].isNull()) 
                    modified_protocol.delay_precue_cue_min = doc["protocol"]["delay_precue_cue_min"];
                if (!doc["protocol"]["delay_precue_cue_max"].isNull()) 
                    modified_protocol.delay_precue_cue_max = doc["protocol"]["delay_precue_cue_max"];
                if (!doc["protocol"]["color_precue"].isNull()) 
                    modified_protocol.color_precue = doc["protocol"]["color_precue"];
                if (!doc["protocol"]["cue_blink"].isNull()) 
                    modified_protocol.cue_blink = doc["protocol"]["cue_blink"];
                if (!doc["protocol"]["blink_t_on"].isNull()) 
                    modified_protocol.blink_t_on = doc["protocol"]["blink_t_on"];
                if (!doc["protocol"]["blink_t_off"].isNull()) 
                    modified_protocol.blink_t_off = doc["protocol"]["blink_t_off"];
                if (!doc["protocol"]["t_cue"].isNull()) 
                    modified_protocol.t_cue = doc["protocol"]["t_cue"];
                if (!doc["protocol"]["color_cue"].isNull()) 
                    modified_protocol.color_cue = doc["protocol"]["color_cue"];
                if (!doc["protocol"]["t_reward"].isNull()) 
                    modified_protocol.t_reward = doc["protocol"]["t_reward"];
                if (!doc["protocol"]["color_reward"].isNull()) 
                    modified_protocol.color_reward = doc["protocol"]["color_reward"];
                if (!doc["protocol"]["t_iti_min"].isNull()) 
                    modified_protocol.t_iti_min = doc["protocol"]["t_iti_min"];
                if (!doc["protocol"]["t_iti_max"].isNull()) 
                    modified_protocol.t_iti_max = doc["protocol"]["t_iti_max"];
                if (!doc["protocol"]["color_iti"].isNull()) 
                    modified_protocol.color_iti = doc["protocol"]["color_iti"];
                if (!doc["protocol"]["intersession_delay"].isNull()) 
                    modified_protocol.intersession_delay = doc["protocol"]["intersession_delay"];
                if (!doc["protocol"]["color_idle"].isNull()) 
                    modified_protocol.color_idle = doc["protocol"]["color_idle"];
                if (!doc["protocol"]["led_brightness"].isNull()) 
                    modified_protocol.led_brightness = doc["protocol"]["led_brightness"];
                if (!doc["protocol"]["led_idle_brightness"].isNull()) 
                    modified_protocol.led_idle_brightness = doc["protocol"]["led_idle_brightness"];
                if (!doc["protocol"]["use_entire_range"].isNull()) 
                    modified_protocol.use_entire_range = doc["protocol"]["use_entire_range"];
                
                xSemaphoreGive(modified_protocol_mutex);
                send_log("Successfully updated protocol. Will be loaded upon next halt-resume cycle.");
            }
            
            // Handle system control signals
            if (!doc["signal"].isNull()) {
                String signal = doc["signal"].as<String>();
                
                if (signal == "halt") {
                    if (msg_halt) send_log("I am already halted.");
                    else send_log("Halting!");
                    msg_halt = true;
                }
                else if (signal == "resume") {
                    if (!msg_halt) send_log("I am already un-halted.");
                    else send_log("Resuming...");
                    msg_halt = false;
                }
                else if (signal == "restart") {
                    send_log("REBOOTING");
                    ESP.restart();
                }
                else if (signal == "get_state") {
                    send_log(state_char_to_string(global_state));
                }
                // Prevent bottle control unless system is halted
                else if (signal == "bottle_deny") {
                    if (!msg_halt) {
                        send_log("I'm afraid I cannot do that. Please halt me first.");
                    } else {
                        send_log("Raising the bottle...");
                        bottle_deny();
                    }
                }
                else if (signal == "bottle_allow") {
                    if (!msg_halt) {
                        send_log("I'm afraid I cannot do that. Please halt me first.");
                    } else {
                        send_log("Lowering the bottle...");
                        bottle_allow();
                    }
                }
                else if (signal == "get_useNVS") {
                    StaticJsonDocument<16> use_nvs_json;
                    String use_nvs_json_str;
                    use_nvs = nvs_get_useNVS();
                    use_nvs_json["use_nvs"] = use_nvs;
                    serializeJson(use_nvs_json, use_nvs_json_str);
                    send_log(use_nvs_json_str);
                }
                else if (signal == "nvs_store_protocol") {
                    nvs_store_protocol(protocol);
                    send_log("Stored protocol to NVS. Currently running protocol (phase " + 
                           String(protocol.phase) + String(") will be loaded by default on startup."));
                }
                else {
                    send_log("MQTT signal not recognized.");
                }
            }
            
            // Clean up message buffer
            delete[] controlmsg.buffer;
        }
        
        delay(100);  // Prevent task from consuming too much CPU
    }
}

/**
 * Task: MQTT Messenger
 * 
 * This task handles outgoing MQTT data transmission.
 * It processes queues containing:
 * - Session summary data
 * - Individual trial data  
 * - Behavioral event data
 * 
 * All data is serialized to JSON format before transmission.
 * 
 * @param pvParameters Unused task parameter
 */
void TaskMQTTMessenger(void* pvParameters) {
    (void)pvParameters;
    
    // Send startup message
    mqttClient.publish(TOPIC_EVENTS, 1, false, "Booted!");
    
    while (1) {
        session_t session;
        trial_t trial;
        event_t event;
        
        // Process all queued behavioural events
        while (xQueueReceive(events_queue, &event, 0) == pdTRUE) {
            size_t msg_size;
            char* msg = serialize_event_t(event, false, msg_size);
            mqttClient.publish(TOPIC_EVENTS, 1, false, msg);
            
            #ifdef DEBUG
            Serial.println(msg);
            #endif
            
            delete[] msg;
        }
        
        delay(10);
        
        // Process all queued trial data
        while (xQueueReceive(trials_queue, &trial, 0) == pdTRUE) {
            size_t msg_size;
            char* msg = serialize_trial_t(trial, false, msg_size);
            mqttClient.publish(TOPIC_TRIALS, 1, false, msg);
            
            #ifdef DEBUG
            Serial.println(msg);
            #endif
            
            delete[] msg;
        }
        
        delay(10);
        
        // Process all queued session data
        while (xQueueReceive(sessions_queue, &session, 0) == pdTRUE) {
            size_t msg_size;
            char* msg = serialize_session_t(session, false, msg_size);
            mqttClient.publish(TOPIC_SESSIONS, 1, false, msg);
            
            #ifdef DEBUG
            Serial.println(msg);
            #endif
            
            delete[] msg;
        }
        
        delay(10);  // Small delay to prevent overwhelming the network
    }
}

/**
 * LED Blink Timer Callbacks
 * These functions are called by FreeRTOS timers to control LED blinking
 */

/**
 * Timer callback to turn LED on during blink sequence
 */
void TimerCallbackLedOn() {
    led.setBrightness(LED_BRIGHTNESS);
    led.setPixelColor(0, 255, 255, 0);  // Yellow for cue
    led.show();
    delay(2);
    blink_params.led_state = 1;
}

/**
 * Timer callback to turn LED off during blink sequence
 */
void TimerCallbackLedOff() {
    led.clear();
    led.show();
    delay(2);
    blink_params.led_state = 0;
}

/**
 * Task: LED Blink Controller
 * 
 * This task manages LED blinking during cue presentation.
 * It uses FreeRTOS timers to create precise on/off timing patterns
 * based on the protocol's blink parameters.
 * 
 * @param pvParameters Unused task parameter
 */
void TaskBlinkLed(void* pvParameters) {
    blink_params.task_running = true;
    (void)pvParameters;
    
    // Create timers for blink control
    TimerBlinkOn = xTimerCreate("TimerBlinkOn", pdMS_TO_TICKS(blink_params.time_off), 
                               pdFALSE, (void *)0, reinterpret_cast<TimerCallbackFunction_t>(TimerCallbackLedOn));
    TimerBlinkOff = xTimerCreate("TimerBlinkOff", pdMS_TO_TICKS(blink_params.time_on), 
                                pdFALSE, (void *)0, reinterpret_cast<TimerCallbackFunction_t>(TimerCallbackLedOff));
    
    led.setBrightness(LED_BRIGHTNESS);
    
    // Main blink loop
    while (!blink_params.stop_flag) {
        // Start "on" timer when LED is off and timer is not active
        if ((blink_params.led_state == 0) && (xTimerIsTimerActive(TimerBlinkOn) == pdFALSE)) {
            xTimerStart(TimerBlinkOn, 0);
            xTimerStop(TimerBlinkOff, 0);  // Ensure off timer is stopped
        }
        
        // Start "off" timer when LED is on and timer is not active
        if ((blink_params.led_state == 1) && (xTimerIsTimerActive(TimerBlinkOff) == pdFALSE)) {
            xTimerStart(TimerBlinkOff, 0);
            xTimerStop(TimerBlinkOn, 0);   // Ensure on timer is stopped
        }
        
        delay(2);  // Small delay to prevent excessive CPU usage
    }
    
    // Cleanup timers and exit
    xTimerStop(TimerBlinkOn, pdMS_TO_TICKS(100));
    xTimerDelete(TimerBlinkOn, pdMS_TO_TICKS(100));
    xTimerStop(TimerBlinkOff, pdMS_TO_TICKS(100));
    xTimerDelete(TimerBlinkOff, pdMS_TO_TICKS(100));
    
    blink_params.task_running = false;
    
    // Store stack usage for debugging
    shwmBlinkLed = uxTaskGetStackHighWaterMark(NULL);
    vTaskDelete(NULL);
}

/**
 * Task: Lick Monitor
 * 
 * This task monitors the lick sensor during reward periods.
 * It samples the capacitive touch sensor rapidly and:
 * - Detects licks above threshold
 * - Signals lick events to other tasks
 * - Automatically lowers bottle after timeout to prevent overdrinking
 * 
 * @param pvParameters Unused task parameter
 */
void TaskLickMonitor(void* pvParameters) {
    xSemaphoreTake(stop_lick_task, 0);  // Clear stop signal
    int n_readings = 0;
    
    while (true) {
        // Check for stop signal
        if (xSemaphoreTake(stop_lick_task, 0) == pdTRUE) break;
        
        // Safety mechanism: lower bottle after extended monitoring
        if (n_readings >= 60) {
            bottle_allow();
            n_readings = 0;
        }
        
        // Sample lick sensor multiple times to find maximum reading
        uint32_t max_reading = 0;
        for (uint32_t i = 0; i < 10; i++) {
            uint32_t r = touchRead(LICK_PIN);
            if (r > max_reading) max_reading = r;
            delay(100);
        }
        
        // Signal lick detection if threshold exceeded
        if (max_reading >= LICK_THRESH) {
            xSemaphoreGive(lickISR_to_msg);
        }
        
        n_readings++;
    }
    
    // Store stack usage and cleanup
    shwmLickMonitor = uxTaskGetStackHighWaterMark(NULL);
    handleLickMonitor = NULL;
    vTaskDelete(NULL);
}

/**
 * Task: HTTP Handler
 * 
 * Simple task to handle HTTP server requests for the web interface.
 * This provides a web-based monitoring and configuration interface.
 * 
 * @param pvParameters Unused task parameter
 */
void TaskHttpHandler(void *pvParameters) {
    while (1) {
        httpServer.handleClient();
        delay(100);  // Prevent excessive CPU usage
    }
}

/**
 * HTTP Request Handler: Root page
 * 
 * Serves the current protocol configuration as JSON via HTTP GET request.
 * This allows web-based monitoring of the current experimental parameters.
 */
void handleRoot() {
    if (xSemaphoreTake(http_protocol_mutex, pdMS_TO_TICKS(1000)) == pdTRUE) {
        size_t msg_size;
        char* msg = serialize_protocol_t(protocol_duplicate, true, msg_size);
        httpServer.send(200, "text/plain", msg);
        delete[] msg;
        xSemaphoreGive(http_protocol_mutex);
    } else {
        httpServer.send(500, "text/plain", "Timeout reading protocol struct. Please try again.");
    }
}

// ============================================================================
// PROTOCOL CONFIGURATION FUNCTIONS
// ============================================================================

/**
 * Configure protocol for all-night session schedule (17:00 - 05:00)
 * Used for extended behavioural training sessions
 */
void pr_sched_allnight(protocol_t& pr) {
    pr.expctrl_start_time_of_day = true;
    pr.hour_start = 17;
    pr.minute_start = 0;
    pr.expctrl_end_time_of_day = true;
    pr.hour_end = 5;
    pr.minute_end = 0;
    pr.expctrl_n_sessions = false;  // No session limit
    pr.expctrl_n_trials = false;    // No trial limit
}

/**
 * Configure protocol for 4-hour session schedule (19:00 - 23:00)
 * Used for shorter, more intensive training sessions
 */
void pr_sched_fourhours(protocol_t& pr) {
    pr.expctrl_start_time_of_day = true;
    pr.hour_start = 19;
    pr.minute_start = 0;
    pr.expctrl_end_time_of_day = true;
    pr.hour_end = 23;
    pr.minute_end = 0;
    pr.expctrl_n_sessions = false;
    pr.expctrl_n_trials = false;
}

/**
 * Configure protocol for 6-hour session schedule (19:00 - 01:00)
 * Moderate duration training sessions
 */
void pr_sched_sixhours(protocol_t& pr) {
    pr.expctrl_start_time_of_day = true;
    pr.hour_start = 19;
    pr.minute_start = 0;
    pr.expctrl_end_time_of_day = true;
    pr.hour_end = 1;
    pr.minute_end = 0;
    pr.expctrl_n_sessions = false;
    pr.expctrl_n_trials = false;
}

/**
 * Training Phase 1: Basic reward association
 * - Long cue duration for easy learning
 * - Extended reward availability
 * - Minimal ITI to maximize reward opportunities
 * - No precue requirement
 * 
 * Goal: Establish nosepoke-water association
 */
void pr_phase_one(protocol_t& pr) {
    pr.phase = 1;
    pr.test_phase = false;
    pr.do_precue = false;           // No precue required
    pr.t_cue = 30*1000;            // Long 30-second cue
    pr.t_reward = 20*1000;         // Extended reward availability
    pr.t_iti_min = 10*1000;        // Short ITI
    pr.t_iti_max = 10*1000;
}

/**
 * Training Phase 2: Cue-water association with ITI
 * - Shorter cue duration increases difficulty
 * - Reduced reward time encourages faster response
 * - Longer ITI introduces timing control
 * 
 * Goal: Learn that ITI nosepokes delay cue presentation
 */
void pr_phase_two(protocol_t& pr) {
    pr.phase = 2;
    pr.test_phase = false;
    pr.do_precue = false;
    pr.t_cue = 10*1000;            // Shorter 10-second cue
    pr.t_reward = 7*1000;          // Reduced reward time
    pr.t_iti_min = 15*1000;        // Longer ITI
    pr.t_iti_max = 15*1000;
}

/**
 * Training Phase 3: Introduction of precue
 * - Adds precue requirement for impulse control
 * - Very short ITI since precue provides timing structure
 * - Fixed precue-to-cue delay for predictability
 * 
 * Goal: Learn to wait for precue before responding
 */
void pr_phase_three(protocol_t& pr) {
    pr.phase = 3;
    pr.test_phase = false;
    pr.do_precue = true;           // Introduce precue
    pr.t_precue = 0;               // Infinite precue (wait for response)
    pr.delay_precue_cue_min = 3*1000;  // Fixed 3-second delay
    pr.delay_precue_cue_max = 3*1000;
    pr.t_cue = 30*1000;            // Generous cue duration
    pr.t_reward = 7*1000;
    pr.t_iti_min = 1*1000;         // Minimal ITI
    pr.t_iti_max = 1*1000;
}

/**
 * Test Protocol: Easy version with extended times
 * Used for testing system function or giving easier sessions
 */
void pr_test_easy(protocol_t& pr) {
    pr = (protocol_t)hardcoded_default_protocol;
    pr.t_cue *= 10;        // 10x longer cue
    pr.t_reward *= 5;      // 5x longer reward
}

/**
 * Test Protocol: Standard testing parameters
 * Uses default protocol settings from configuration
 */
void pr_test(protocol_t& pr) {
    pr = (protocol_t)hardcoded_default_protocol;
}

/**
 * Test Protocol Part 1: Fixed short delay (first 20% of session)
 * Used in multi-part test sessions with varying difficulty
 */
void pr_test_firstpart(protocol_t& pr) {
    pr.delay_precue_cue_min = 3*1000;   // 3-second delay
    pr.delay_precue_cue_max = 3*1000;
    pr.use_entire_range = false;        // Use fixed values only
}

/**
 * Test Protocol Part 2: Variable long delays (middle 60% of session)
 * Increases difficulty with unpredictable timing
 */
void pr_test_secondpart(protocol_t& pr) {
    pr.delay_precue_cue_min = 6*1000;   // 6-12 second range
    pr.delay_precue_cue_max = 12*1000;
    pr.use_entire_range = false;        // Min or max only
}

/**
 * Test Protocol Part 3: Return to fixed short delay (final 20% of session)
 * Provides easier ending to session
 */
void pr_test_thirdpart(protocol_t& pr) {
    pr.delay_precue_cue_min = 3*1000;   // Back to 3-second delay
    pr.delay_precue_cue_max = 3*1000;
    pr.use_entire_range = false;
}

// [File continues with session management functions...]/**
 * Go/No-Go Apparatus - Core Experimental Logic (Part 3)
 * The main experimenter task and system initialization
 */

/**
 * Task: Main Experimenter
 * 
 * This is the heart of the behavioural system. It implements the complete
 * Go/No-Go behavioural paradigm with the following sequence:
 * 
 * 1. Inter-Trial Interval (ITI): Wait period, extended if premature responses occur
 * 2. Precue (optional): Blue light, animal must nosepoke to continue
 * 3. Precue-Cue Delay: Variable delay period, extended if impulsive responses occur
 * 4. Main Cue: Yellow light, animal must nosepoke within time limit for reward
 * 5. Reward: Water bottle lowered, lick detection monitored
 * 
 * The task continuously runs trials within sessions, respecting timing constraints
 * and protocol parameters. All behavioural data is logged for analysis.
 * 
 * @param pvParameters Unused task parameter
 */
void TaskExperimenter(void* pvParameters) {
    (void)pvParameters;
    
    #if START_DELAY
    delay(DEVICE_ID*10);  // Stagger device startup if multiple devices
    #endif
    
    send_log("TaskExperimenter starting...");
    
    bool fatal_error = false;
    bool runcnd = test_evgroup(evgroupExpRun, EVG_ALL_CNDS);
    xEventGroupSetBits(evgroupExpRun, EVG_SESSION_RUNNING);
    
    // Main session loop - continues until conditions are no longer met
    while (runcnd && check_n_sessions(protocol, total_sessions)) {
        uint32_t session_start = millis();
        global_total_experiments += 1;
        total_sessions += 1;
        
        // Initialize session data structure
        session_t session = {
            -2, -2,  // timestamps set below
            0,       // duration
            global_total_experiments,  // experiment number
            total_sessions,           // session number
            0, 0, 0, 0, 0, 0, 0, 0,  // performance counters
            0, 0, 0, 0               // trial type counters
        };
        
        // Initialize current state
        current_state_t current_state = { char_states.idle, 0 };
        global_state = char_states.idle;
        
        // Record session start time with microsecond precision
        timeval tv_now;
        gettimeofday(&tv_now, NULL);
        session.start_s = tv_now.tv_sec;
        session.start_us = tv_now.tv_usec;
        
        // Main trial loop - continues until trial limit reached or conditions not met
        while (runcnd && check_n_trials(protocol, current_state.trial_n)) {
            
            //================================================================
            // PHASE 1: INTER-TRIAL INTERVAL (ITI)
            //================================================================
            
            current_state.state_name = char_states.interval_intertrial;
            global_state = char_states.interval_intertrial;
            current_state.trial_n += 1;  // Human-readable: 1-based indexing
            
            // Initialize trial data structure
            trial_t trial = {
                -2, -2,              // timestamps
                current_state.trial_n, // trial number
                0, 0,                // ITI duration and extensions
                -1, -2,              // precue performance (-1 = N/A)
                0, 0,                // delay and impulsive count
                -2, -2,              // main response performance
                -2, -2, -2,          // lick data
                0,                   // precue wait time
                0, 0, 0, 0, 0, 0, 0, 0,  // sensor readings
                'G'                  // trial type (always Go in this version)
            };
            
            set_led(char_states.interval_intertrial, &protocol);
            
            // Wait for nosepoke sensor to be released before starting trial
            uint32_t timeout_start = millis();
            uint32_t adjusted_timeout_start = timeout_start;
            
            while (analogRead(NOSEPOKE_PIN) >= NOSEPOKE_THRESH) {
                delay(10);
                trial.iti += 10;
                
                // Safety check: prevent infinite loop if sensor is stuck
                if (difftime(adjusted_timeout_start, timeout_start) > 15*60*1000) {
                    send_log("FATAL: ITI has been extended for over 15 minutes - check the nosepoke. Halting...");
                    msg_halt = true;
                    fatal_error = true;
                    break;
                }
            }
            
            // Clear any pending nosepoke signals
            xSemaphoreTake(leverISR_to_exp, 0);
            
            // Record actual trial start time
            gettimeofday(&tv_now, NULL);
            trial.start_s = tv_now.tv_sec;
            trial.start_us = tv_now.tv_usec;
            
            // Determine ITI duration
            uint32_t current_iti;
            if ((protocol.t_iti_min == protocol.t_iti_max) || (current_state.trial_n == 1)) {
                // Fixed ITI or first trial gets average
                current_iti = (protocol.t_iti_min + protocol.t_iti_max) / 2;
            } else {
                if (protocol.use_entire_range) {
                    // Random value within range
                    current_iti = random(protocol.t_iti_min, protocol.t_iti_max);
                } else {
                    // Binary choice: min or max only
                    current_iti = random(0,2) ? protocol.t_iti_min : protocol.t_iti_max;
                }
            }
            
            // Execute ITI with extension for premature responses
            timeout_start = millis();
            adjusted_timeout_start = timeout_start;
            uint32_t time_interval = current_iti;
            uint64_t touchread_iteration = 0;
            
            // ITI loop: restart timing if nosepoke detected
            while ((millis()-adjusted_timeout_start < time_interval) && (!fatal_error)) {
                if (xSemaphoreTake(leverISR_to_exp, pdMS_TO_TICKS(10)) == pdTRUE) {
                    // Premature response detected - restart ITI
                    session.count_lever_total += 1;
                    session.count_lever_iti += 1;
                    trial.iti_extension += 1;
                    adjusted_timeout_start = millis();  // Restart timer
                    
                    // Safety check for excessive extensions
                    if (difftime(adjusted_timeout_start, timeout_start) > 15*60*1000) {
                        send_log("FATAL: ITI has been extended for over 15 minutes - check the nosepoke. Halting...");
                        msg_halt = true;
                        fatal_error = true;
                    }
                }
                
                // Monitor lick sensor during ITI (for diagnostic purposes)
                touchread_iteration++;
                uint64_t r = touchRead(LICK_PIN);
                trial.iti_mean_touchread += r;
                if (r > trial.iti_max_touchread) {
                    trial.iti_max_touchread = r;
                }
            }
            
            trial.iti = millis() - timeout_start;  // Record total ITI duration
            if (touchread_iteration) trial.iti_mean_touchread /= touchread_iteration;
            
            //================================================================
            // PHASE 2: PRECUE (if enabled)
            //================================================================
            
            if (protocol.do_precue) {
                current_state.state_name = char_states.precue;
                global_state = char_states.precue;
                
                xSemaphoreTake(leverISR_to_exp, 0);  // Clear signals
                set_led(char_states.precue, &protocol);  // Blue LED
                
                timeout_start = millis();
                time_interval = protocol.t_precue;
                BaseType_t response = pdFALSE;
                touchread_iteration = 0;
                
                delay(2);  // Ensure loop executes even with t_precue == 0
                
                // Wait for precue response or timeout
                while ((millis()-timeout_start <= time_interval) && (!fatal_error)) {
                    // Monitor lick sensor
                    touchread_iteration++;
                    uint64_t r = touchRead(LICK_PIN);
                    trial.precue_mean_touchread += r;
                    if (r > trial.precue_max_touchread) {
                        trial.precue_max_touchread = r;
                    }
                    
                    // Check for nosepoke response
                    response = xSemaphoreTake(leverISR_to_exp, pdMS_TO_TICKS(10));
                    if (response == pdTRUE) break;
                    
                    // For infinite precue (t_precue == 0), check run conditions
                    if (time_interval == 0) {
                        runcnd = test_evgroup(evgroupExpRun, EVG_ALL_CNDS);
                        if (!runcnd) break;
                    }
                }
                
                session.trial_count_total += 1;
                
                // Record precue performance
                if (response == pdFALSE) {
                    // No response - precue failed
                    trial.precue_reaction_time = -1;
                    trial.precue_correct = 0;
                    session.count_precue_fail += 1;
                } else {
                    // Response detected - precue succeeded
                    trial.precue_reaction_time = millis() - timeout_start;
                    trial.precue_correct = 1;
                    session.count_lever_total += 1;
                    session.count_precue_correct += 1;
                }
                
                if (touchread_iteration) trial.precue_mean_touchread /= touchread_iteration;
                set_led(char_states.idle, &protocol);  // Turn off LED
                
                //============================================================
                // PHASE 3: PRECUE-CUE DELAY (if precue was successful)
                //============================================================
                
                if (trial.precue_correct) {
                    // Determine delay duration
                    if (protocol.delay_precue_cue_min == protocol.delay_precue_cue_max) {
                        trial.delay_precue_cue = protocol.delay_precue_cue_min;
                    } else {
                        if (protocol.use_entire_range) {
                            trial.delay_precue_cue = (uint32_t)random(protocol.delay_precue_cue_min, 
                                                                    protocol.delay_precue_cue_max);
                        } else {
                            trial.delay_precue_cue = random(0,2) ? protocol.delay_precue_cue_min : 
                                                                  protocol.delay_precue_cue_max;
                        }
                    }
                    
                    current_state.state_name = char_states.interval_precue_cue;
                    global_state = char_states.interval_precue_cue;
                    
                    // Execute delay with extension for impulsive responses
                    timeout_start = millis();
                    adjusted_timeout_start = timeout_start;
                    time_interval = trial.delay_precue_cue;
                    
                    while ((millis()-adjusted_timeout_start < time_interval) && (!fatal_error)) {
                        if (xSemaphoreTake(leverISR_to_exp, pdMS_TO_TICKS(10)) == pdTRUE) {
                            // Impulsive response - restart delay
                            adjusted_timeout_start = millis();
                            trial.impulsive_nosepokes += 1;
                            session.count_impulsive_nosepokes += 1;
                        }
                        
                        // Safety check
                        if (difftime(adjusted_timeout_start, timeout_start) > 15*60*1000) {
                            send_log("FATAL: Delay has been extended for over 15 minutes - check the nosepoke. Halting...");
                            msg_halt = true;
                            fatal_error = true;
                        }
                    }
                    
                    trial.precue_nosepoke_wait = millis() - timeout_start;
                    
                    if (trial.impulsive_nosepokes > 0) {
                        session.trial_count_impulsive += 1;
                    }
                }
            }
            
            //================================================================
            // PHASE 4: MAIN CUE (if precue passed or not required)
            //================================================================
            
            if ((!protocol.do_precue) || (protocol.do_precue && trial.precue_correct)) {
                current_state.state_name = char_states.cue;
                global_state = char_states.cue;
                
                // Set up LED blinking if enabled
                if (protocol.cue_blink) {
                    blink_params = {
                        false,           // task_running
                        510,             // color_on (placeholder)
                        protocol.blink_t_on,
                        0,               // color_off
                        protocol.blink_t_off,
                        false,           // stop_flag
                        0                // led_state
                    };
                    
                    // Create LED blinking task
                    task_create_response = xTaskCreate(TaskBlinkLed, "TBlink", 1024, NULL, 2, &handleBlinkLed);
                    if (task_create_response != pdPASS) {
                        send_log("ERROR while creating TaskBlinkLed: ");
                        send_log(task_create_response);
                    }
                } else {
                    // Static LED
                    set_led(trial.trial_type, &protocol);
                }
                
                xSemaphoreTake(leverISR_to_exp, 0);  // Clear signals
                timeout_start = millis();
                time_interval = protocol.t_cue;
                BaseType_t response;
                touchread_iteration = 0;
                
                // Wait for cue response or timeout
                while ((millis()-timeout_start < time_interval) && (!fatal_error)) {
                    // Monitor lick sensor
                    touchread_iteration++;
                    uint64_t r = touchRead(LICK_PIN);
                    trial.cue_mean_touchread += r;
                    if (r > trial.cue_max_touchread) {
                        trial.cue_max_touchread = r;
                    }
                    
                    // Check for response
                    response = xSemaphoreTake(leverISR_to_exp, pdMS_TO_TICKS(10));
                    if (response == pdTRUE) break;
                    
                    // For infinite cue (t_cue == 0), check run conditions
                    if (protocol.t_cue == 0) {
                        runcnd = test_evgroup(evgroupExpRun, EVG_ALL_CNDS);
                        if (!runcnd) break;
                    }
                }
                
                if (touchread_iteration) trial.cue_mean_touchread /= touchread_iteration;
                
                // Evaluate response (this version only has Go trials)
                if (response == pdTRUE) {
                    // Response detected
                    trial.reaction_time = millis() - timeout_start;
                    session.count_lever_total += 1;
                    
                    if (trial.trial_type == 'G') {
                        // Correct response for Go trial
                        trial.correct = 1;
                        session.count_correct += 1;
                    } else {
                        // Incorrect response for NoGo trial (not implemented)
                        trial.correct = 0;
                        session.count_fail += 1;
                    }
                } else {
                    // No response (timeout)
                    trial.reaction_time = -1;
                    
                    if (trial.trial_type == 'G') {
                        // Incorrect non-response for Go trial
                        trial.correct = 0;
                        session.count_fail += 1;
                    } else {
                        // Correct non-response for NoGo trial (not implemented)
                        trial.correct = 1;
                        session.count_correct += 1;
                    }
                }
                
                // Stop LED blinking if active
                if (protocol.cue_blink) {
                    blink_params.stop_flag = true;
                    while (blink_params.task_running) delay(2);  // Wait for task to finish
                    delay(2);
                }
                
                //============================================================
                // PHASE 5: REWARD (if response was correct)
                //============================================================
                
                if (trial.correct == 1) {
                    session.trial_count_good += 1;
                    
                    current_state.state_name = char_states.reward;
                    global_state = char_states.reward;
                    set_led(char_states.reward, &protocol);
                    delay(REWARD_DELAY);
                    
                    // Lower bottle to provide water access
                    bottle_allow();
                    timeout_start = millis();
                    time_interval = protocol.t_reward;
                    
                    // Monitor lick sensor during reward period
                    uint64_t last_lick_timestamp = 0;
                    touchread_iteration = 0;
                    trial.total_lick_t = 0;
                    uint64_t r_previous = 0;
                    
                    while ((millis()-timeout_start < time_interval) && (!fatal_error)) {
                        touchread_iteration++;
                        uint64_t r = touchRead(LICK_PIN);
                        trial.reward_mean_touchread += r;
                        if (r > trial.reward_max_touchread) {
                            trial.reward_max_touchread = r;
                        }
                        
                        // Detect licking behaviour
                        if (r > LICK_THRESH) {
                            trial.total_lick_t += 1;
                            
                            if (r_previous < LICK_THRESH) {
                                // New lick detected
                                session.count_licks += 1;
                            } else {
                                // Continued licking
                                trial.total_lick_t += millis() - last_lick_timestamp;
                            }
                            
                            last_lick_timestamp = millis();
                            
                            // Record time to first lick
                            if (trial.time_to_lick == -2) {
                                trial.time_to_lick = millis() - timeout_start;
                            }
                        }
                        
                        r_previous = r;
                        delay(1);
                    }
                    
                    // Raise bottle to end reward access
                    bottle_deny();
                    
                    if (touchread_iteration) trial.reward_mean_touchread /= touchread_iteration;
                    
                    // Finalize lick data
                    if (trial.time_to_lick == -2) trial.time_to_lick = -1;  // No lick detected
                    
                    if (trial.reward_max_touchread >= LICK_THRESH) {
                        trial.licked = 1;
                        xSemaphoreGive(lickISR_to_msg);  // Signal lick event
                    } else {
                        trial.licked = 0;
                    }
                } else {
                    // Trial abandoned (incorrect or no response)
                    session.trial_count_abandoned += 1;
                    trial.time_to_lick = -2;  // Not applicable
                    trial.licked = -2;        // Not applicable
                }
            }
            
            // End of trial - return to ITI state and log data
            current_state.state_name = char_states.interval_intertrial;
            global_state = char_states.interval_intertrial;
            set_led(char_states.interval_intertrial, &protocol);
            
            // Queue trial data for transmission
            xQueueSend(trials_queue, &trial, 0);
            
            // Check if session should continue
            runcnd = test_evgroup(evgroupExpRun, EVG_ALL_CNDS);
        }
        
        // End of session
        uint32_t session_end = millis();
        session.duration = session_end - session_start;
        
        // Queue session data for transmission
        xQueueSend(sessions_queue, &session, 0);
        
        // Inter-session delay
        current_state.state_name = char_states.interval_intersession;
        global_state = char_states.interval_intersession;
        
        if (runcnd) {
            while ((millis() - session_end) < protocol.intersession_delay) {
                delay(100);
            }
        }
    }
    
    // Experiment finished - clean up
    xEventGroupClearBits(evgroupExpRun, EVG_SESSION_RUNNING);
    global_state = char_states.idle;
    
    // Store stack usage for debugging
    shwmExperimenter = uxTaskGetStackHighWaterMark(NULL);
    
    #if START_DELAY
    delay(DEVICE_ID*10);
    #endif
    
    send_log("TaskExperimenter finished.");
    vTaskDelete(NULL);
}

//=============================================================================
// SESSION MANAGEMENT FUNCTIONS
//=============================================================================

/**
 * Execute a single behavioural session
 * 
 * This function manages the complete lifecycle of a behavioural session:
 * - Waits for scheduled start time (if configured)
 * - Creates and starts the main experimenter task
 * - Monitors for end conditions (time of day, halt commands)
 * - Ensures clean shutdown when session ends
 * 
 * @param epoch_time_start Session start time (epoch seconds)
 * @param epoch_time_end Session end time (epoch seconds)  
 * @param n_session Session number (default = 1)
 */
void run_session(time_t epoch_time_start, time_t epoch_time_end, uint32_t n_session = 1) {
    
    // Start lick monitoring task
    xTaskCreate(TaskLickMonitor, "TLick", 1024, NULL, 2, &handleLickMonitor);
    
    // Check for halt command
    if (msg_halt) {
        xEventGroupClearBits(evgroupExpRun, EVG_CND_NO_HALT_MSG);
        xSemaphoreGive(stop_lick_task);
        while (handleLickMonitor != NULL) delay(100);
        return;
    }
    
    time_t epoch_time_now;
    time(&epoch_time_now);
    
    // Skip if we're past the end time
    if (difftime(epoch_time_end, epoch_time_now) < 0) {
        send_log("I'm late. Skipping this phase...");
        return;
    }
    
    // Wait for start time if configured
    if (protocol.expctrl_start_time_of_day) {
        while (difftime(epoch_time_start, epoch_time_now) > 0) {
            if (msg_halt) {
                xEventGroupClearBits(evgroupExpRun, EVG_CND_NO_HALT_MSG);
                xSemaphoreGive(stop_lick_task);
                while (handleLickMonitor != NULL) delay(100);
                return;
            }
            time(&epoch_time_now);
            delay(1000);
        }
        xEventGroupSetBits(evgroupExpRun, EVG_CND_TIME_OF_DAY);
    } else {
        xEventGroupSetBits(evgroupExpRun, EVG_CND_TIME_OF_DAY);
    }
    
    // Stop lick monitor before starting experiment
    xSemaphoreGive(stop_lick_task);
    while (handleLickMonitor != NULL) delay(100);
    
    Serial.println("Starting TaskExperimenter");
    bottle_deny();  // Prepare hardware
    total_sessions = n_session;
    
    // Create main experimenter task
    task_create_response = xTaskCreate(TaskExperimenter, "TExp", 4096, NULL, 1, &handleExperimenter);
    if (task_create_response != pdPASS) {
        send_log("ERROR while creating TaskExperimenter: ");
        send_log(task_create_response);
    }
    
    // Wait for task to start
    while (global_state == char_states.idle) {
        delay(100);
    }
    
    // Monitor for end conditions
    if (protocol.expctrl_end_time_of_day) {
        while (difftime(epoch_time_end, epoch_time_now) > 0) {
            if (msg_halt) {
                xEventGroupClearBits(evgroupExpRun, EVG_CND_NO_HALT_MSG);
                break;
            }
            time(&epoch_time_now);
            delay(1000);
        }
        xEventGroupClearBits(evgroupExpRun, EVG_CND_TIME_OF_DAY);
    }
    
    // Wait for task to finish cleanly
    while (global_state != char_states.idle) {
        if (msg_halt) {
            xEventGroupClearBits(evgroupExpRun, EVG_CND_NO_HALT_MSG);
        }
        delay(100);
    }
}

/**
 * Execute a multi-part test session
 * 
 * Test sessions are divided into three parts with different timing parameters:
 * - First 20%: Short, fixed delays (easy)
 * - Middle 60%: Long, variable delays (difficult)  
 * - Final 20%: Short, fixed delays (easy ending)
 * 
 * @param epoch_time_start Test session start time
 * @param epoch_time_end Test session end time
 */
void run_test_session(time_t epoch_time_start, time_t epoch_time_end) {
    uint32_t current_session = 1;
    double test_duration = difftime(epoch_time_end, epoch_time_start);
    
    // Calculate transition times
    time_t et_test_secondpart_start = epoch_time_start + (time_t)(test_duration * 0.2);
    time_t et_test_thirdpart_start = epoch_time_start + (time_t)(test_duration * 0.8);
    
    #ifdef DEBUG
    Serial.print(epoch_time_start);
    Serial.print("\t");
    Serial.print(et_test_secondpart_start);
    Serial.print("\t");
    Serial.print(et_test_thirdpart_start);
    Serial.print("\t");
    Serial.println(epoch_time_end);
    #endif
    
    // Wait for any pending data transmission
    while (uxQueueMessagesWaiting(sessions_queue) != 0) delay(100);
    mqttClient.publish(TOPIC_SESSIONS, 1, false, "{\"message\":\"TEST_SESSION_START\"}");
    
    // Part 1: Easy phase
    pr_test_firstpart(protocol);
    update_web_protocol();
    #ifdef DEBUG
    Serial.println("Loaded first part.");
    #endif
    
    run_session(epoch_time_start, et_test_secondpart_start, current_session);
    if (msg_halt) {
        while (uxQueueMessagesWaiting(sessions_queue) != 0) delay(100);
        mqttClient.publish(TOPIC_SESSIONS, 1, false, "{\"message\":\"TEST_SESSION_INTERRUPTED\"}");
        return;
    }
    
    // Part 2: Difficult phase
    pr_test_secondpart(protocol);
    update_web_protocol();
    #ifdef DEBUG
    Serial.println("Loaded second part.");
    #endif
    
    current_session++;
    run_session(et_test_secondpart_start, et_test_thirdpart_start, current_session);
    if (msg_halt) {
        while (uxQueueMessagesWaiting(sessions_queue) != 0) delay(100);
        mqttClient.publish(TOPIC_SESSIONS, 1, false, "{\"message\":\"TEST_SESSION_INTERRUPTED\"}");
        return;
    }
    
    // Part 3: Easy ending
    pr_test_thirdpart(protocol);
    update_web_protocol();
    #ifdef DEBUG
    Serial.println("Loaded third part.");
    #endif
    
    current_session++;
    run_session(et_test_thirdpart_start, epoch_time_end, current_session);
    if (msg_halt) {
        while (uxQueueMessagesWaiting(sessions_queue) != 0) delay(100);
        mqttClient.publish(TOPIC_SESSIONS, 1, false, "{\"message\":\"TEST_SESSION_INTERRUPTED\"}");
        return;
    }
    
    // Test session completed
    while (uxQueueMessagesWaiting(sessions_queue) != 0) delay(100);
    mqttClient.publish(TOPIC_SESSIONS, 1, false, "{\"message\":\"TEST_SESSION_END\"}");
}

/**
 * Load protocol configuration based on phase selection
 * 
 * @param phase_load Protocol phase to load (0=default, 1-5=training phases, 255=custom)
 */
void load_protocol() {
    switch (protocol_phase_load) {
        case 0:
            protocol = default_protocol;
            break;
        case 1:
            pr_phase_one(protocol);
            pr_sched_sixhours(protocol);
            break;
        case 2:
            pr_phase_two(protocol);
            pr_sched_sixhours(protocol);
            break;
        case 3:
            pr_phase_three(protocol);
            pr_sched_fourhours(protocol);
            break;
        case 4:
            pr_test(protocol);
            pr_sched_fourhours(protocol);
            break;
        case 5:
            pr_test_easy(protocol);
            pr_sched_fourhours(protocol);
            break;
        case 255:
            // Load custom modified protocol
            xSemaphoreTake(modified_protocol_mutex, pdMS_TO_TICKS(10000));
            protocol = modified_protocol;
            xSemaphoreGive(modified_protocol_mutex);
            break;
        default:
            break;
    }
}

/**
 * Update the web interface protocol copy (thread-safe)
 */
void update_web_protocol() {
    xSemaphoreTake(http_protocol_mutex, pdMS_TO_TICKS(10000));
    protocol_duplicate = protocol;
    protocol_duplicate.is_currently_running = true;
    xSemaphoreGive(http_protocol_mutex);
}

//=============================================================================
// MAIN SETUP AND LOOP FUNCTIONS
//=============================================================================

/**
 * Arduino setup function
 * Initializes all hardware, network connections, and FreeRTOS tasks
 */
void setup() {
    #if START_DELAY
    delay(DEVICE_ID * 500);  // Stagger startup for multiple devices
    #endif
    
    // Initialize LED
    Serial.println("Starting LED");
    led.begin();
    led.setBrightness(LED_BRIGHTNESS);
    led.clear();
    led.setPixelColor(0, 0, 255, 0);  // Green startup indicator
    led.show();
    delay(1000);
    led.clear();
    led.show();
    Serial.println("LED initialized");
    
    // Initialize FreeRTOS synchronization objects
    evgroupExpRun = xEventGroupCreate();
    xEventGroupClearBits(evgroupExpRun, (EVG_SESSION_RUNNING | EVG_CND_TIME_OF_DAY));
    xEventGroupSetBits(evgroupExpRun, EVG_CND_NO_HALT_MSG);
    
    http_protocol_mutex = xSemaphoreCreateMutex();
    modified_protocol_mutex = xSemaphoreCreateMutex();
    
    // Load protocol configuration
    use_nvs = nvs_get_useNVS();
    if (use_nvs) {
        protocol_t nvs_protocol;
        nvs_load_protocol(nvs_protocol, hardcoded_default_protocol);
        default_protocol = nvs_protocol;
    } else {
        default_protocol = (protocol_t)hardcoded_default_protocol;
    }
    
    protocol = default_protocol;
    modified_protocol = default_protocol;
    
    xSemaphoreTake(http_protocol_mutex, pdMS_TO_TICKS(1000));
    protocol_duplicate = protocol;
    xSemaphoreGive(http_protocol_mutex);
    
    // Initialize serial communication
    Serial.begin(115200);
    Serial.print("DEVICE_ID: ");
    Serial.println(DEVICE_ID);
    Serial.println("Booting");
    
    // Configure ADC resolution
    analogReadResolution(12);
    
    // Initialize WiFi
    WiFi.mode(WIFI_STA);
    if (!WiFi.config(LOCAL_IP, GATEWAY, SUBNET_MASK, DNS_PRIM, DNS_SECOND)) {
        Serial.println("STA Failed to configure");
    }
    
    // Create network timers
    mqttReconnectTimer = xTimerCreate("mqttTimer", pdMS_TO_TICKS(10000), pdFALSE, 
                                     (void *)0, reinterpret_cast<TimerCallbackFunction_t>(connectToMqtt));
    wifiReconnectTimer = xTimerCreate("wifiTimer", pdMS_TO_TICKS(10000), pdFALSE, 
                                     (void *)0, reinterpret_cast<TimerCallbackFunction_t>(connectToWifi));
    
    // Set up network event handlers
    WiFi.onEvent(WiFiEvent);
    mqttClient.onMessage(onMqttMessage);
    mqttClient.onConnect(onMqttConnect);
    mqttClient.onDisconnect(onMqttDisconnect);
    
    // Configure MQTT client
    mqttClient.setServer(MQTT_HOST, MQTT_PORT);
    mqttClient.setClientId(MQTT_CLIENTID);
    mqttClient.setCredentials(MQTT_USER, MQTT_PASS);
    
    // Connect to network
    connectToWifi();
    Serial.print("IP address: ");
    Serial.println(WiFi.localIP());
    
    // Set up HTTP server
    httpUpdater.setup(&httpServer);
    httpServer.on("/", handleRoot);
    httpServer.begin();
    Serial.printf("HTTPUpdateServer ready at http://%s/update\n", WiFi.localIP().toString().c_str());
    
    // Configure time synchronization
    configTime(NTP_OFFSET_GMT, NTP_OFFSET_DST, NTP_SERVER);
    
    // Wait for time synchronization
    uint8_t exit_signal;
    for (uint8_t i = 0; i < 10; i++) {
        exit_signal = curtime_to_timestamp(isotime, sizeof(isotime));
        if (exit_signal == 0) break;
    }
    
    if (exit_signal > 0) ESP.restart();
    Serial.print("Time synchronized: ");
    Serial.println(isotime);
    
    connectToMqtt();
    
    // Initialize semaphores
    Serial.println("Initializing interrupt semaphores");
    lickISR_to_exp = xSemaphoreCreateBinary();
    lickISR_to_msg = xSemaphoreCreateBinary();
    leverISR_to_exp = xSemaphoreCreateBinary();
    leverISR_to_msg = xSemaphoreCreateBinary();
    stop_lick_task = xSemaphoreCreateBinary();
    
    // Create communication queues
    Serial.println("Creating queues");
    controlmsg_queue = xQueueCreate(10, sizeof(controlmsg_t));
    sessions_queue = xQueueCreate(10, sizeof(session_t));
    trials_queue = xQueueCreate(100, sizeof(trial_t));
    events_queue = xQueueCreate(1000, sizeof(event_t));
    
    // Initialize servo control
    #if DEVICE_ID == 1
    pinMode(19, OUTPUT);
    digitalWrite(19, HIGH);  // Enable servo power
    #endif
    
    ledcSetup(LEDC_CHANNEL, LEDC_BASE_FREQ, LEDC_TIMER_PRECISION);
    pinMode(SERVO_PIN, OUTPUT);
    digitalWrite(SERVO_PIN, LOW);
    ledcAttachPin(SERVO_PIN, LEDC_CHANNEL);
    
    // Initialize servo position
    Serial.println("Initializing servo position");
    bottle_allow();
    delay(protocol.start_delay_bdown);
    bottle_deny();  
    delay(protocol.start_delay_bup);
    
    // Initialize sensors
    touchRead(LICK_PIN);  // Initialize touch sensor
    delay(100);
    pinMode(NOSEPOKE_PIN, INPUT);
    
    // Create tasks
    Serial.println("Creating tasks");
    
    task_create_response = xTaskCreate(TaskEventListener, "TEvents", 2048, NULL, 2, &handleEventListener);
    if (task_create_response != pdPASS) {
        send_log("ERROR creating TaskEventListener: ");
        send_log(task_create_response);
    }
    
    task_create_response = xTaskCreate(TaskMQTTController, "Tctr", 4096, NULL, 1, &handleMQTTController);
    if (task_create_response != pdPASS) {
        send_log("ERROR creating TaskMQTTController: ");
        send_log(task_create_response);
    }
    
    task_create_response = xTaskCreate(TaskMQTTMessenger, "TMQTT", 4096, NULL, 1, &handleMQTTMessenger);
    if (task_create_response != pdPASS) {
        send_log("ERROR creating TaskMQTTMessenger: ");
        send_log(task_create_response);
    }
    
    task_create_response = xTaskCreate(TaskHttpHandler, "THttp", 4096, NULL, 1, &handleHttpHandler);
    if (task_create_response != pdPASS) {
        send_log("ERROR creating TaskHttpHandler: ");
        send_log(task_create_response);
    }
    
    set_led(char_states.idle, &protocol);
    Serial.println("Setup completed - entering main loop");
}

/**
 * Arduino main loop function
 * 
 * This function runs continuously and manages the high-level experiment flow:
 * - Handles halt/resume commands
 * - Loads protocol configurations
 * - Calculates session timing
 * - Launches appropriate session types (training vs. test)
 * - Provides debug information about task memory usage
 */
void loop() {
    // Lower bottle to safe position
    bottle_allow();
    
    // Load current protocol configuration
    load_protocol();
    update_web_protocol();
    
    bool sent_halted = false;
    
    // Handle halt state
    while (msg_halt) {
        if (!sent_halted) {
            send_log("HALTED");
            sent_halted = true;
            global_state = char_states.halted;
            
            // Start lick monitoring during halt
            xTaskCreate(TaskLickMonitor, "TLick", 1024, NULL, 2, &handleLickMonitor);
        }
        
        delay(100);
    }
    
    // Clean up halt state
    xSemaphoreGive(stop_lick_task);
    while (handleLickMonitor != NULL) delay(100);
    xEventGroupClearBits(evgroupExpRun, EVG_CND_NO_HALT_MSG);
    delay(100);
    
    // Resume normal operation
    global_state = char_states.idle;
    xEventGroupSetBits(evgroupExpRun, EVG_CND_NO_HALT_MSG);
    
    // Calculate session timing
    time_t epoch_time_start, epoch_time_end, epoch_time_now;
    time(&epoch_time_now);
    
    struct tm datetime_start, datetime_end;
    localtime_r(&epoch_time_now, &datetime_start);
    localtime_r(&epoch_time_now, &datetime_end);
    
    // Set start time
    datetime_start.tm_hour = protocol.hour_start;
    datetime_start.tm_min = protocol.minute_start;
    datetime_start.tm_sec = 0;
    epoch_time_start = mktime(&datetime_start);
    
    // Set end time
    datetime_end.tm_hour = protocol.hour_end;
    datetime_end.tm_min = protocol.minute_end;
    datetime_end.tm_sec = 0;
    epoch_time_end = mktime(&datetime_end);
    
    // Handle overnight sessions (end time < start time)
    if (difftime(epoch_time_end, epoch_time_start) < 0) {
        epoch_time_end += 24 * 60 * 60;  // Add 24 hours
    }
    
    // If we're past today's end time, schedule for tomorrow
    if (difftime(epoch_time_now, epoch_time_end) > 0) {
        epoch_time_start += 24 * 60 * 60;
        epoch_time_end += 24 * 60 * 60;
    }
    
    // Launch appropriate session type
    if (protocol.test_phase) {
        run_test_session(epoch_time_start, epoch_time_end);
    } else {
        run_session(epoch_time_start, epoch_time_end);
    }
    
    // Debug output: task memory usage
    #ifdef DEBUG
    Serial.printf("TaskEventListener stack high water mark = %d\n", uxTaskGetStackHighWaterMark(handleEventListener));
    Serial.printf("TaskHttpHandler stack high water mark = %d\n", uxTaskGetStackHighWaterMark(handleHttpHandler));
    Serial.printf("TaskMQTTMessenger stack high water mark = %d\n", uxTaskGetStackHighWaterMark(handleMQTTMessenger));
    Serial.printf("TaskBlinkLed stack high water mark = %d\n", shwmBlinkLed);
    Serial.printf("TaskExperimenter stack high water mark = %d\n", shwmExperimenter);
    Serial.printf("TaskMQTTController stack high water mark = %d\n", uxTaskGetStackHighWaterMark(handleMQTTController));
    Serial.printf("TaskLickMonitor stack high water mark = %d\n", shwmLickMonitor);
    Serial.printf("Timer task stack high water mark = %d\n", uxTaskGetStackHighWaterMark(xTimerGetTimerDaemonTaskHandle()));
    Serial.printf("Main task stack high water mark = %d\n", uxTaskGetStackHighWaterMark(NULL));
    #endif
}
