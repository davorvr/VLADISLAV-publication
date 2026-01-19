#!/usr/bin/python3
import paho.mqtt.client as mqtt
from datetime import datetime
import gzip
from pathlib import Path

devices = [ "vds_"+str(i) for i in range(1,41) ]
topics = [ "sessions", "trials", "events", "status", "control" ]
topic_files = {}

# The callback for when the client receives a CONNACK response from the server.
def on_connect(client, userdata, flags, rc):
    print("Connected with result code "+str(rc))

    # Subscribing in on_connect() means that if we lose the connection and
    # reconnect then subscriptions will be renewed.
    timenow = datetime.now().isoformat().replace(":","-").replace(".","-")
    for dev in devices:
        for i, t in enumerate(topics):
            logdir = "./"+dev+"/"+t+"/"
            full_topic_name = "gng/"+dev+"/"+t
            Path(logdir).mkdir(parents=True, exist_ok=True)
            filename = full_topic_name.replace("/", "-")+"."+timenow+".txt"
            topic_files[full_topic_name] = open(logdir+filename, "wt", buffering=1)
            client.subscribe(full_topic_name, qos=1)
    for t in ["status", "control"]:
        all_logdir = "./all/"+t+"/"
        full_topic_name = "gng/all/"+t
        Path(all_logdir).mkdir(parents=True, exist_ok=True)
        filename = full_topic_name.replace("/", "-")+"."+timenow+".txt"
        topic_files[full_topic_name] = open(all_logdir+filename, "wt", buffering=1)
        client.subscribe(full_topic_name, qos=1)

# The callback for when a PUBLISH message is received from the server.
def on_message(client, userdata, msg):
    #print(msg.topic+" "+str(msg.payload))
    line = '{"timestamp": "'+datetime.now().isoformat()+'", "message":'+msg.payload.decode("utf-8")+'}\n'
    topic_files[msg.topic].write(line)

client = mqtt.Client()
client.on_connect = on_connect
client.on_message = on_message
client.username_pw_set(username="mqtt_username", password="mqtt_password")
client.connect("192.168.1.2", 1883, 60)

# Blocking call that processes network traffic, dispatches callbacks and
# handles reconnecting.
# Other loop*() functions are available that give a threaded interface and a
# manual interface.
try:
    client.loop_forever()
except KeyboardInterrupt:
    pass

for f in topic_files:
    topic_files[f].close()
