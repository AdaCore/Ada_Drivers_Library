# BME280

> Humidity sensor measuring relative humidity, barometric pressure and
> ambient temperature

* [Datasheet](https://www.bosch-sensortec.com/products/environmental-sensors/humidity-sensors-bme280/#documents)

The sensor is available as a module for DIY projects from various
manufacturers, such as [Adafruit](https://www.adafruit.com/product/2652)
and [SparkFun](https://www.sparkfun.com/products/13676). It boasts high
accuracy, a compact size, and the flexibility to connect via both I2C and
SPI interfaces.

The BME280 driver enables the following functionalities:

* Detect the presence of the sensor.
* Perform a reset operation.
* Configure the parameters of the IRR filter and oversampling for each channel.
* Read calibration coefficients.
* Conduct measurements and calibrate the obtained values.
* Calculate the time required for measurements.

## Examples

* [Simple example for STM32 F4VE board](../../../../examples/stm32_f4ve/bme280)
* [Advanced example for STM32 F4VE board and LCD & touch panel](../../../../examples/stm32_f4ve/bme280)
