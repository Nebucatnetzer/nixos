# Raspberry Pi microSD card benchmark script.
#
# A script I use to automate the running and reporting of benchmarks I compile
# for: http://www.pidramble.com/wiki/benchmarks/microsd-cards
#
# Usage:
#   # Run it locally.
#   $ su do ./microsd-benchmarks.sh
#
#   # Run it straight from GitHub.
#   $ curl https://raw.githubusercontent.com/geerlingguy/raspberry-pi-dramble/master/setup/benchmarks/microsd-benchmarks.sh | sudo bash
#
# Another good benchmark:
#   $ curl http://www.nmacleod.com/public/sdbench.sh | sudo bash
#
# Author: Jeff Geerling, 2016 (last updated 2020)

printf "\n"
printf "Raspberry Pi Dramble microSD benchmarks\n"

CLOCK="$(grep "actual clock" /sys/kernel/debug/mmc0/ios 2>/dev/null | awk '{printf("%0.3f MHz", $3/1000000)}')"
if [ -n "$CLOCK" ]; then
    echo "microSD clock: $CLOCK"
fi
printf "\n"

# Fail if $SUDO_USER is empty.
if [ -z "$SUDO_USER" ]; then
    printf "This script must be run with sudo.\n"
    exit 1
fi

# Variables.
USER_HOME_PATH=$(getent passwd "$SUDO_USER" | cut -d: -f6)

# Run benchmarks.
printf "Running hdparm test...\n"
hdparm -t /dev/mmcblk1
printf "\n"

printf "Running dd test...\n\n"
dd if=/dev/zero of="$USER_HOME_PATH"/test bs=8k count=50k conv=fsync
rm -f "$USER_HOME_PATH"/test
printf "\n"

printf "Running iozone test...\n"
iozone -e -I -a -s 100M -r 4k -i 0 -i 1 -i 2
printf "\n"

printf "microSD card benchmark complete!\n\n"
