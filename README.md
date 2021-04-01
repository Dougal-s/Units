# Unit Library
The header `units.hpp` provides compile time unit checking and conversion for SI
units.

## Usage

The header provides two main classes; the Unit class; and the Quantity class.

The Unit class is not designed to be interacted with directly. Instead, the
header provides predefined units that can be combined using the `*`, `/`, and
`^` operators (e.g. joule has been defined using `newton*metre`). The unit
variables can also be combined with c++'s `std::ratios`. For example, the
kilometre unit has been defined as:
```c++
constexpr auto kilometre = std::kilo * metre;
```

The Unit class on its own does not provide any meaningful functionality. Instead
it is meant to be used in conjunction with the Quantity class.

The Quantity class has the following signature:
```c++
template <auto Units, typename T = float>
class Quantity;
```
Units - represents the stored quantity's units.  
T - the number type used by the Quantity.

Once a quantity variable has been declared, it can mostly be used just like any
other arithmetic type.

Example code:
```c++
#include <cmath>
#include <iostream>

#include "units.hpp"

using namespace units;
using namespace units::abbreviations;
using namespace units::unit_literals;

constexpr Quantity<N*m*m/(kg*kg), float> G = 6.67408e-11f;

int main() {
	auto x = 5519.2f * km;
	auto y = 3186.5_km;
	auto r = std::sqrt(x*x + y*y);

	auto mass = 5.97e24_kg;

	constexpr auto acc = m/(s*s);
	Quantity<acc, double> potential = G * mass/(r*r);

	// prints 9.81015
	std::cout << potential.as<acc>() << std::endl;
}
```

### Namespaces
The library defines all classes, functions and variables inside the `units`
namespace.

**units::abbreviations**  
``units::abbreviations`` contains abbreviated versions of every predefined unit.

**units::units_literals**: _incomplete_  
``units::units_literals`` contains literal operators for every predefined unit.


## Limitations
While the library can convert between different units related by a constant
factor, it does not have the ability to convert between units that contain an
offset (e.g. Kelvin and Celsius).

Since all the units are checked at compile time, functions that would result in
runtime dependent unit changes such as `pow` are not available. Instead the
library provides templated alternatives (e.g. `units::power`).

Radians are a also bit of a weird one as a dimensionless unit. At the moment,
the library mostly ignores angle units when performing unit checking. This means
that code such as `(20_rad).as<rad*rad>()` is perfectly valid despite not making
sense. The exceptions are the trigonometric function, which still require angle
units.
