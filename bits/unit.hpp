#include <ratio>
#include <type_traits>

namespace units {


	// Custom Ratio Operations

	template <typename T>
	struct IsRatio : std::false_type {};

	template <auto num, auto den>
	struct IsRatio<std::ratio<num, den>> : std::true_type {};

	template <typename R>
	concept Ratio = IsRatio<R>::value;

	// Performs ratio exponentiation
	template <Ratio base, intmax_t power, bool>
	class ratio_power_impl {
		using power_minus = typename ratio_power_impl<base, -power, false>::type;
	public:
		using type = std::ratio_divide<std::ratio<1,1>, power_minus>;
	};

	template <Ratio base, intmax_t power>
	class ratio_power_impl<base, power, false> {
		using power_half = typename ratio_power_impl<base, power/2, false>::type;
		using power_floor_even = std::ratio_multiply<power_half, power_half>;
		using odd = std::conditional<power%2, base, std::ratio<1>>::type;
	public:
		using type = std::ratio_multiply<power_floor_even, odd>;
	};

	template <Ratio base>
	struct ratio_power_impl<base, 0, false> {
		using type = std::ratio<1>;
	};

	// the type representing the result of raising a ratio to the power of an integer
	template <Ratio base, intmax_t power>
	using ratio_power = typename ratio_power_impl<base, power, (power<0)>::type;



	// Unit class

	template <Ratio coefficient, Ratio rad, Ratio s, Ratio m, Ratio kg, Ratio A, Ratio K, Ratio mol, Ratio cd>
	struct Unit {
		using radians = rad;
		using seconds = s;
		using metres = m;
		using kilograms = kg;
		using amperes = A;
		using kelvin = K;
		using mole = mol;
		using candela = cd;

		using coef = coefficient;
	};


	template <class T>
	concept CUnit = requires(T unit) {
		{ Unit{unit} } -> std::derived_from<T>;
	};

	template <class>
	struct IsUnit : public std::false_type {};

	template <CUnit unit>
	struct IsUnit<unit> : public std::true_type {};


	template <class A, class B>
	concept CEquivalentUnit =
		std::ratio_equal_v<typename A::seconds, typename B::seconds> &&
		std::ratio_equal_v<typename A::metres, typename B::metres> &&
		std::ratio_equal_v<typename A::kilograms, typename B::kilograms> &&
		std::ratio_equal_v<typename A::amperes, typename B::amperes> &&
		std::ratio_equal_v<typename A::kelvin, typename B::kelvin> &&
		std::ratio_equal_v<typename A::mole, typename B::mole> &&
		std::ratio_equal_v<typename A::candela, typename B::candela>;

	template <class A, class B>
	struct EquivalentUnit : public std::false_type {};

	template <CUnit A, CUnit B> requires CEquivalentUnit<A,B>
	struct EquivalentUnit<A, B> : public std::true_type {};

	template <CUnit lhs, CUnit rhs>
	consteval bool operator==(lhs, rhs) {
		return EquivalentUnit<lhs, rhs>::value;
	}

	//	Unit Operations

	// with other units

	template <Ratio coef1, Ratio... units1, Ratio coef2, Ratio... units2>
	consteval Unit<
		std::ratio_multiply<coef1, coef2>,
		std::ratio_add<units1, units2>...
	> operator*(Unit<coef1, units1...>, Unit<coef2, units2...>) {
		return {};
	}

	template <Ratio coef1, Ratio... units1, Ratio coef2, Ratio... units2>
	consteval Unit<
		std::ratio_divide<coef1, coef2>,
		std::ratio_subtract<units1, units2>...
	> operator/(Unit<coef1, units1...>, Unit<coef2, units2...>) {
		return {};
	}

	// with scalars

	// Multiplication
	template <Ratio coef, Ratio... units, Ratio scalar>
	consteval auto operator*(scalar, Unit<coef, units...>) {
		return Unit<std::ratio_multiply<coef, scalar>, units...>{};
	}
	template <Ratio coef, Ratio... units, Ratio scalar>
	consteval auto operator*(Unit<coef, units...>, scalar) {
		return Unit<std::ratio_multiply<coef, scalar>, units...>{};
	}

	// Division
	template <Ratio coef, Ratio... units, Ratio scalar>
	consteval auto operator/(scalar, Unit<coef, units...>) {
		return Unit<std::ratio_divide<scalar, coef>, std::ratio_subtract<std::ratio<0>, units>...>{};
	}
	template <Ratio coef, Ratio... units, Ratio scalar>
	consteval auto operator/(Unit<coef, units...>, scalar) {
		return Unit<std::ratio_divide<coef, scalar>, units...>{};
	}

	template <Ratio coef, Ratio... units, auto power>
	consteval Unit<
		ratio_power<coef, power>,
		std::ratio_multiply<std::ratio<power>, units>...
	> operator^(Unit<coef, units...>, std::ratio<power, 1>) {
		return {};
	}

	template <Ratio coef, Ratio... units, Ratio ratio>
	consteval Unit<
		std::ratio<1>,
		std::ratio_multiply<ratio, units>...
	> operator^(Unit<coef, units...>, ratio) {
		static_assert(std::ratio_equal_v<coef, std::ratio<1>>,
			"Cannot perform unit exponentiation on non-base units");
		return {};
	}


	//
	// Base Units (+ angles)
	//

	// Nonunit
	constexpr Unit<std::ratio<1>, std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>> nonunit = {};
	// Angle
	template <Ratio ratio>
	using AngleUnit = Unit<ratio, std::ratio<1>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>>;

	constexpr AngleUnit<std::ratio<2*3141592653589793238, 1000000000000000000>> revolution = {};
	constexpr AngleUnit<std::ratio<31415926535897932, 180*10000000000000000>> degree = {};
	constexpr AngleUnit<std::ratio<1>> radian = {};

	// time
	template <Ratio ratio>
	using TimeUnit = Unit<ratio, std::ratio<0>,std::ratio<1>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>>;

	constexpr TimeUnit<std::ratio<1>> second = {};
	constexpr TimeUnit<std::milli> millisecond = {};
	constexpr TimeUnit<std::micro> microsecond = {};
	constexpr TimeUnit<std::nano> nanosecond = {};

	// Distance
	template <Ratio ratio>
	using DistanceUnit = Unit<ratio, std::ratio<0>,std::ratio<0>,std::ratio<1>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>>;

	constexpr DistanceUnit<std::kilo> kilometre = {};
	constexpr DistanceUnit<std::deca> decametre = {};
	constexpr DistanceUnit<std::ratio<1>> metre = {};
	constexpr DistanceUnit<std::deci> decimetre = {};
	constexpr DistanceUnit<std::centi> centimetre = {};
	constexpr DistanceUnit<std::milli> millimeter = {};
	constexpr DistanceUnit<std::micro> micrometer = {};
	constexpr DistanceUnit<std::nano> nanometer = {};

	// Mass
	template <Ratio ratio>
	using MassUnit = Unit<ratio, std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<1>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>>;

	constexpr MassUnit<std::ratio<1>> kilogram = {};
	constexpr MassUnit<std::milli> gram = {};
	constexpr MassUnit<std::micro> milligram = {};
	constexpr MassUnit<std::nano> microgram = {};

	// Current
	template <Ratio ratio>
	using CurrentUnit = Unit<ratio, std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<1>,std::ratio<0>,std::ratio<0>,std::ratio<0>>;

	constexpr CurrentUnit<std::ratio<1>> ampere = {};
	constexpr CurrentUnit<std::milli> milliampere = {};

	// Temperature
	template <Ratio ratio>
	using TemperatureUnit = Unit<ratio, std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<1>,std::ratio<0>,std::ratio<0>>;

	constexpr TemperatureUnit<std::ratio<1>> kelvin = {};

	// Amount
	template <Ratio ratio>
	using AmountUnit = Unit<ratio, std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<1>,std::ratio<0>>;

	constexpr AmountUnit<std::ratio<1>> mole = {};
	// entities is unavailable as Avogadro constant is too large to
	// fit in a 64bit integer

	// Intensity
	template <Ratio ratio>
	using IntensityUnit = Unit<ratio, std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<0>,std::ratio<1>>;

	constexpr IntensityUnit<std::ratio<1>> candela = {};

	// Derived units
	constexpr auto steradian = radian*radian;
	constexpr auto hertz = second^std::ratio<-1>{};
	constexpr auto newton = kilogram*metre/(second*second);
	constexpr auto pascal = newton/(metre*metre);
	constexpr auto joule = newton*metre;
	constexpr auto watt = joule/second;
	constexpr auto coulomb = ampere*second;
	constexpr auto volt = joule/coulomb;
	constexpr auto farad = coulomb/volt;
	constexpr auto ohm = volt/ampere;
	constexpr auto siemens = std::ratio<1>{}/ohm;
	constexpr auto weber = volt*second;
	constexpr auto tesla = weber/(metre*metre);
	constexpr auto henry = weber/ampere;
	constexpr auto lumen = candela*steradian;
	constexpr auto lux = lumen/(metre*metre);
	constexpr auto becquerel = second^std::ratio<-1>{};
	constexpr auto gray = joule/kilogram;
	constexpr auto seivert = joule/kilogram;
	constexpr auto katal = mole/second;

	// non-si units
	constexpr auto liter = decimetre^std::ratio<3>{};
	constexpr auto tonne = std::kilo{}*kilogram;
	constexpr auto minute = std::ratio<60>{}*second;
	constexpr auto hour = std::ratio<60>{}*minute;

	namespace abbreviations {
		constexpr auto rev = revolution;
		constexpr auto deg = degree;
		constexpr auto rad = radian;

		constexpr auto s = second;
		constexpr auto ms = millisecond;
		constexpr auto us = microsecond;
		constexpr auto ns = nanosecond;

		constexpr auto km = kilometre;
		constexpr auto dam = decametre;
		constexpr auto m = metre;
		constexpr auto dm = decimetre;
		constexpr auto cm = centimetre;
		constexpr auto mm = millimeter;
		constexpr auto um = micrometer;
		constexpr auto nm = nanometer;

		constexpr auto kg = kilogram;
		constexpr auto g = gram;
		constexpr auto mg = milligram;
		constexpr auto ug = microgram;
		constexpr auto A = ampere;
		constexpr auto mA = milliampere;
		constexpr auto K = kelvin;
		constexpr auto mol = mole;
		constexpr auto cd = candela;
		constexpr auto sr = steradian;
		constexpr auto Hz = hertz;
		constexpr auto N = newton;
		constexpr auto Pa = pascal;
		constexpr auto J = joule;
		constexpr auto W = watt;
		constexpr auto C = coulomb;
		constexpr auto V = volt;
		constexpr auto F = farad;
		constexpr auto Ω = ohm;
		constexpr auto S = siemens;
		constexpr auto Wb = weber;
		constexpr auto T = tesla;
		constexpr auto H = henry;
		constexpr auto lm = lumen;
		constexpr auto lx = lux;
		constexpr auto Bq = becquerel;
		constexpr auto Gy = gray;
		constexpr auto Sv = seivert;
		constexpr auto kat = katal;
	};
}
