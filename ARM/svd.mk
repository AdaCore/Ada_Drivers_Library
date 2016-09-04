SVD2ADA_DIR?=~/src/svd2ada
CORTEX_DIR=$(PWD)/cortex_m/src
STM_DIR=$(PWD)/STM32/svd

all: svd

svd:
	rm -rf $(STM_DIR)/stm32*
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F40x.svd --boolean -o $(STM_DIR)/stm32f40x -p STM32_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F429x.svd --boolean -o $(STM_DIR)/stm32f429x -p STM32_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F46_79x.svd --boolean -o $(STM_DIR)/stm32f46_79x -p STM32_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F7x.svd --boolean -o $(STM_DIR)/stm32f7x -p STM32_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/ST/STM32F7x9.svd --boolean -o $(STM_DIR)/stm32f7x9 -p STM32_SVD --base-types-package HAL

	rm -rf $(CORTEX_DIR)/cm*
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Cortex_M/cm7.svd --boolean -o $(CORTEX_DIR)/cm7 -p Cortex_M_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Cortex_M/cm4.svd --boolean -o $(CORTEX_DIR)/cm4 -p Cortex_M_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/Cortex_M/cm4f.svd --boolean -o $(CORTEX_DIR)/cm4f -p Cortex_M_SVD --base-types-package HAL
