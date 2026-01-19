import json

max_char_size = 128+1
maximum_values = {
    "uint64_t": int(0xFFFFFFFFFFFFFFFF),
    "int64_t": int(-0x8000000000000000),
    "uint32_t": int(0xFFFFFFFF),
    "int32_t": int(-0x80000000),
    "uint16_t": int(0xFFFF),
    "int16_t": int(-0x8000),
    "uint8_t": int(0xFF),
    "int8_t": int(-0x80),
    "bool": "false",
    "char": "A",
    "time_t": "2023-01-03T09:43:42.574906",
    "char[]": "A"*(max_char_size)
    }

def remove_commented_lines(struct: str):
    for sym_start, sym_end in [ ("/*", "*/"), ("//", "\n") ]:
        while sym_start in struct:
            start = struct.index(sym_start)
            end = struct.find(sym_end, start)+len(sym_end)
            struct = struct[:start]+struct[end:]
    return struct

def struct_to_dict(struct: str):
    struct = struct[struct.index("{")+1:struct.index("}")].strip()
    struct_list = struct.split(";")
    struct_list = [
        x.strip()
        for x in struct_list
        if x.strip()
        ]
    struct_dict = {}
    for member in struct_list:
        member = member.split(" ", 1)
        if member[0] == "char" and all([ x in member[1] for x in ["[", "]"] ]): ## if it's a char, check if it's actually a char array
            v = member[0]+"[]" ## append the brackets so it matches the char array key in maximum values
            k = member[1]
            k = k[:k.index("[")] ## strip the array declaration part from the char array variable's name
        else:
            v = member[0]
            k = member[1]
        struct_dict[k] = v
        
    return struct_dict

def fill_structdict(struct_dict: dict):
    filled_dict = {}
    for k, v in struct_dict.items():
        filled_dict[k] = maximum_values[v]
    return filled_dict

structs = {
"protocol_t" : """
struct protocol_t {
  uint8_t phase;
  bool test_phase;
  uint32_t start_delay_bup;
  uint32_t start_delay_bdown;
  /* if the experiment begins at a certain time of day.
     this makes it wait for the specified time of day to start.
     the experiment will start every day at this time. */
  bool expctrl_start_time_of_day; // if experiment begins at a certain time of day
  uint32_t hour_start;
  uint32_t minute_start;
  /* if the end criterion is a certain time of day. */
  bool expctrl_end_time_of_day; // if experiment ends at a certain time of day
  uint32_t hour_end;
  uint32_t minute_end;
  bool expctrl_n_sessions; // if number of sessions is used as an end criterion
  uint32_t total_session_n;
  /* if there is a defined number of trials in a session.
     if this is used in conjuction with expctrl_end_time_of_day, time of day
     takes precedence - i.e. a session will be stopped when the specified
     time of day is reached, even if total_trial_n hasn't been reached yet.
     (the currently running trial will still be run to completion.) */
  bool expctrl_n_trials; // if number of trials in a session is defined/limited
  uint32_t total_trial_n;
  bool do_precue;
  uint32_t t_precue; // if 0, wait indefinitely (until session end)
  uint32_t delay_precue_cue_min;
  uint32_t delay_precue_cue_max;
  uint32_t color_precue;
  bool cue_blink;
  int32_t blink_t_on;
  int32_t blink_t_off;
  uint32_t t_cue; // if 0, wait indefinitely (until session end)
  uint32_t color_cue;
  uint32_t t_reward;
  uint32_t color_reward;
  uint32_t t_iti_min;
  uint32_t t_iti_max;
  uint32_t color_iti;
  uint32_t intersession_delay;
  uint32_t color_idle;
  uint8_t led_brightness;
  uint8_t led_idle_brightness;
  bool use_entire_range; // if 0, the program will choose between either the min, or the max value. if 1, the program will pick a random value from [min,max>
};
""",
"session_t" : """
struct session_t {
//   int64_t start_s;
  time_t start_s;
  int32_t start_us;
  uint32_t duration;
  uint32_t experiment_number;
  uint32_t session_number;
  uint32_t count_licks;
  uint32_t count_lever_total;
  uint32_t count_lever_iti;
  uint32_t count_correct;
  uint32_t count_fail;
  uint32_t count_precue_correct;
  uint32_t count_precue_fail;
};
""",
"trial_t" : """
struct trial_t { // -2 is a placeholder value, meaning empty/not written yet, or not applicable
//   int64_t start_s;
  time_t start_s;
  int32_t start_us;
  uint32_t trial_n;
  char trial_type; // G or N
  int32_t iti; // in ms
  uint32_t iti_extension; // iti extension due to lever press
  int8_t precue_correct; // -1 placeholder, 0 or 1
  int32_t precue_reaction_time; // -2 placeholder, -1 timeout
  uint32_t delay_precue_cue;
  int8_t correct; // -1 placeholder (if precue failed), 0 if failed, 1 if correct
  int32_t reaction_time; // -1 if timeout, -2 if precue failed
  int8_t licked; // 0 or 1, or -2 if not applicable (no reward attained)
  int32_t time_to_lick; // time, or -1 if not licked (timeout), or -2 if not applicable (no reward attained)
  int32_t total_lick_t; // cycles spent licking (should last a little bit more than 1 ms), 0 if not licked (timeout), or -2 if not applicable (no reward attained)
  uint32_t precue_nosepoke_wait;
  uint32_t iti_max_touchread;
  uint64_t iti_mean_touchread;
  uint32_t precue_max_touchread;
  uint64_t precue_mean_touchread;
  uint32_t cue_max_touchread;
  uint64_t cue_mean_touchread;
  uint32_t reward_max_touchread;
  uint64_t reward_mean_touchread;
};
""",
"event_t" : """
struct event_t {
  time_t start_s;
  int32_t start_us;
  char[] state;
  char event_name;
};
"""
}

dicts = {}
for k, v in structs.items():
    dicts[k] = fill_structdict(
               struct_to_dict(
               remove_commented_lines(
               v
               )))
    # with open(k+".json", "w") as f:
    #     json.dump(v, f, indent=4)
with open("fulldict.json", "w") as f:
    json.dump(dicts, f, indent=0)
