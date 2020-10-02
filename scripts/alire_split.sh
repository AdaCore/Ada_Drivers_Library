#!/bin/sh

# Split the Ada Drivers Library project into multiple crates for alire.ada.dev

HAL_REPO="git@github.com:Fabien-Chouteau/hal.git"
CORTEX_M_REPO="git@github.com:Fabien-Chouteau/cortex-m.git"
COMP_REPO="git@github.com:Fabien-Chouteau/embedded-components.git"
MID_REPO="git@github.com:Fabien-Chouteau/adl-middleware.git"
NRF_REPO="git@github.com:Fabien-Chouteau/nrf5x-hal.git"
MICROBIT_REPO="git@github.com:Fabien-Chouteau/microbit_bsp.git"
MICROBIT_EXAMPLES_REPO="git@github.com:Fabien-Chouteau/microbit_examples.git"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ADL_DIR=${SCRIPT_DIR}/../

echo "ADL Directory" ${ADL_DIR}

WORK_DIR=${ADL_DIR}/scripts/"alire_split_work_dir"
rm -rf ${WORK_DIR}
mkdir ${WORK_DIR}

# clone the repos
git clone ${HAL_REPO} ${WORK_DIR}/hal
git clone ${CORTEX_M_REPO} ${WORK_DIR}/cortex-m
git clone ${COMP_REPO} ${WORK_DIR}/embedded-components
git clone ${MID_REPO} ${WORK_DIR}/adl-middleware
git clone ${NRF_REPO} ${WORK_DIR}/nrf5x-hal
git clone ${MICROBIT_REPO} ${WORK_DIR}/microbit_bsp
git clone ${MICROBIT_EXAMPLES_REPO} ${WORK_DIR}/microbit_examples

# HAL
cp ${ADL_DIR}/hal/src/* ${WORK_DIR}/hal/src/
cd ${WORK_DIR}/hal
git status

# CORTEX-M
cp -r ${ADL_DIR}/arch/ARM/cortex_m/src/* ${WORK_DIR}/cortex-m/src/
cd ${WORK_DIR}/cortex-m
git status

# Components
mkdir -p ${WORK_DIR}/embedded-components/src
cp -r ${ADL_DIR}/components/src/* ${WORK_DIR}/embedded-components/src/
cd ${WORK_DIR}/embedded-components
git status

# Middleware
mkdir -p ${WORK_DIR}/adl-middleware/src
cp -r ${ADL_DIR}/middleware/src/* ${WORK_DIR}/adl-middleware/src/
rm -r ${WORK_DIR}/adl-middleware/src/ravenscar-common/
cd ${WORK_DIR}/adl-middleware
git status

# nrf5x
mkdir -p ${WORK_DIR}/nrf5x-hal/src
cp -r ${ADL_DIR}/arch/ARM/Nordic/* ${WORK_DIR}/nrf5x-hal/src/
cd ${WORK_DIR}/nrf5x-hal
git status

# micro:bit BSP
mkdir -p ${WORK_DIR}/microbit_bsp/src
cp ${ADL_DIR}/boards/MicroBit/src/* ${WORK_DIR}/microbit_bsp/src/
cp ${ADL_DIR}/boards/MicroBit/src/zfp/link.ld ${WORK_DIR}/microbit_bsp/src/
cp ${ADL_DIR}/boards/MicroBit/src/zfp/crt0.S ${WORK_DIR}/microbit_bsp/src/
cd ${WORK_DIR}/microbit_bsp
git status

# micro:bit examples
mkdir -p ${WORK_DIR}/microbit_examples/
cp -rf ${ADL_DIR}/examples/MicroBit/* ${WORK_DIR}/microbit_examples/

for gpr in $(find ${WORK_DIR}/microbit_examples/ -name '*.gpr'); do
    echo "-->" $gpr
    sed -i 's/MicroBit_ZFP/MicroBit_BSP/g' $gpr
    sed -i 's/..\/..\/..\/boards\/MicroBit\/microbit_zfp\.gpr/microbit_bsp\.gpr/g' $gpr
done
