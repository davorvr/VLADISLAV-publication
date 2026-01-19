import subprocess
from pathlib import Path
import time

# Batch-compiles firmware in ./arduino for devices from 1 to 40 using arduino-cli.

devices = range(1, 41)
#devices = [6, 41]
sketch_path = Path("./arduino")
binaryout_path = sketch_path / "compiled"
binary_path = binaryout_path / "gng.ino.bin"
buildartifacts_path = sketch_path / "build_artifacts"

binaryout_path.mkdir(exist_ok=True)
buildartifacts_path.mkdir(exist_ok=True)

start_time = time.time()
for dev_id in devices:
    with open(sketch_path / "device_id.h", "w") as header_file:
        header_file.write(f"#define DEVICE_ID {dev_id}\n")
    start_time_onebin = time.time()
    completedprocess = subprocess.run(["arduino-cli", "compile", "-b", "esp32:esp32:esp32s2", "--output-dir", str(binaryout_path.resolve()), str(sketch_path.resolve())],  check=True)
    print(time.time()-start_time_onebin)
    if not binary_path.is_file():
        raise FileNotFoundError("Output binary doesn't exist!")
    binary_path.rename(binaryout_path/f"vds{dev_id}.bin")

total_duration = time.time()-start_time
print(total_duration)
