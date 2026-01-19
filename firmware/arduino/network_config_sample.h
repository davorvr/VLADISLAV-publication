// Device identifier used for MQTT client and topic naming
#define DEVICE_ID "gng_test"

// Password for Over-the-Air (OTA) firmware updates
#define OTA_PASS "password_for_ota_upload"

// MQTT broker authentication credentials
#define MQTT_USER "mqtt_username"
#define MQTT_PASS "mqtt_password"

// Static IP address assigned to the device on the local network
#define LOCAL_IP IPAddress(192, 168, 1, 10) // of the device

// Wi-Fi network credentials for connection
#define WIFI_SSID "ssid"
#define WIFI_PASS "pass"

// Local network settings
#define GATEWAY IPAddress(192, 168, 1, 1) // router IP
#define SUBNET IPAddress(255, 255, 255, 0) // subnet mask
#define DNS_PRIM IPAddress(1, 1, 1, 1) // primary DNS server
#define DNS_SECOND IPAddress(8, 8, 8, 8) // secondary DNS server

// MQTT broker network settings
#define MQTT_HOST IPAddress(192, 168, 1, 2) // IP of the MQTT server
#define MQTT_PORT 1883 // network port for MQTT communication

// MQTT client identifier, typically matches device ID above
#define MQTT_CLIENTID DEVICE_ID

// Network Time Protocol settings for accurate timekeeping
#define NTP_SERVER "hr.pool.ntp.org" // NTP server address
#define NTP_OFFSET_GMT 3600 // GMT time zone offset in seconds
#define NTP_OFFSET_DST 3600 // Daylight saving time offset in seconds
