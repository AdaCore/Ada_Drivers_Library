SVD2ADA_DIR=~/src/svd2ada

all: svd

svd:
	rm -rf stm32*
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/STM32F40x.svd --boolean -o stm32f40x -p STM32_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/STM32F429x.svd --boolean -o stm32f429x -p STM32_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/STM32F46_79x.svd --boolean -o stm32f46_79x -p STM32_SVD --base-types-package HAL
	$(SVD2ADA_DIR)/svd2ada $(SVD2ADA_DIR)/CMSIS-SVD/STM32F7x.svd --boolean -o stm32f7x -p STM32_SVD --base-types-package HAL
