#include <cmath>
#include <ratio>
#include <type_traits>
#include <concepts>

#include "bits/unit.hpp"

namespace units {

	template <auto template_units, typename T = float>
		requires CUnit<decltype(template_units)>
	class Quantity {
	public:
		using value_type = T;
		using units_t = decltype(template_units);
		static constexpr units_t units = template_units;

		// Constructors
		constexpr Quantity() = default;
		template <typename U> requires std::constructible_from<T, U>
		constexpr Quantity(U val) : value{val} {}

		template <auto other_units, typename U> requires (units == other_units)
		constexpr Quantity(const Quantity<other_units, U>& other) :
			Quantity(other.template as<units>()) {}

		// Assignment
		template <typename U> requires std::assignable_from<T, U>
		constexpr Quantity& operator=(U val) {
			value = val;
			return *this;
		}

		template <auto other_units, typename U> requires (units == other_units)
		constexpr Quantity& operator=(const Quantity<other_units, U>& other) {
			return *this = other.template as<units>();
		}

		// Destructors
		constexpr ~Quantity() = default;

		// returns a dimensionless object representing the current quantity converted to out_units
		template <const auto out_units> requires (units == out_units)
		constexpr T as() const {
			using coef1 = typename units_t::coef;
			using coef2 = typename decltype(out_units)::coef;
			using value_coef = std::ratio_divide<coef1, coef2>;
			return value * static_cast<T>(value_coef::num) / static_cast<T>(value_coef::den);
		}
		template <typename out_units_t> requires CEquivalentUnit<units_t, out_units_t>
		constexpr T as(out_units_t out_units) const {
			using coef1 = typename units_t::coef;
			using coef2 = typename decltype(out_units)::coef;
			using value_coef = std::ratio_divide<coef1, coef2>;
			return value * static_cast<T>(value_coef::num) / static_cast<T>(value_coef::den);
		}

		T& get() noexcept { return value; }
		constexpr const T& get() const noexcept { return value; }

		// Operations
		template <auto other_units, typename U> requires (units == other_units)
		constexpr Quantity& operator+=(const Quantity<other_units, U>& other) {
			value += other.template as<units>();
			return *this;
		}
		template <auto other_units, typename U> requires (units == other_units)
		constexpr Quantity& operator-=(const Quantity<other_units, U>& other) {
			value -= other.template as<units>();
			return *this;
		}

		template <typename U> requires
			requires(T t, U&& u) {{t += std::forward<U>(u)} -> std::same_as<T>;}
		constexpr Quantity& operator+=(U other) {
			value += other;
			return *this;
		}
		template <typename U> requires
			requires(T t, U&& u) {{t -= std::forward<U>(u)} -> std::same_as<T>;}
		constexpr Quantity& operator-=(U other) {
			value -= other;
			return *this;
		}

		// Multiplication and Division assignment operators are
		// not available with Quantities as they would change the current units

		template <typename U> requires
			requires(T t, U&& u) {{t *= std::forward<U>(u)} -> std::same_as<T>;}
		constexpr Quantity& operator*=(U other) {
			value *= other;
			return *this;
		}
		template <typename U> requires
			requires(T t, U&& u) {{t /= std::forward<U>(u)} -> std::same_as<T>;}
		constexpr Quantity& operator/=(U other) {
			value /= other;
			return *this;
		}

	private:
		T value{};
	};

	// Addition/Subtraction
	template <typename T1, auto units1, typename T2, auto units2>
		requires (units1 == units2)
	constexpr auto operator+(
		const Quantity<units1, T1>& lhs,
		const Quantity<units2, T2>& rhs
	) -> Quantity<units1, decltype(T1{}+T2{})> {
		return {lhs.get() + rhs.template as<units1>()};
	}
	template <typename T1, auto units1, typename T2, auto units2>
		requires (units1 == units2)
	constexpr auto operator-(
		const Quantity<units1, T1>& lhs,
		const Quantity<units2, T2>& rhs
	) -> Quantity<units1, decltype(T1{}-T2{})> {
		return {lhs.get() - rhs.template as<units1>()};
	}

	template <typename T, auto units> requires CUnit<decltype(units)>
	constexpr auto operator+(const Quantity<units, T>& rhs) {
		return rhs;
	}
	template <typename T, auto units> requires CUnit<decltype(units)>
	constexpr auto operator-(const Quantity<units, T>& rhs) {
		return T{-1}*rhs;
	}

	// Multiplication/Division
	template <typename T1, auto units1, typename T2, auto units2>
		requires CUnit<decltype(units1)> && CUnit<decltype(units2)>
	constexpr auto operator*(
		const Quantity<units1, T1>& lhs,
		const Quantity<units2, T2>& rhs
	) -> Quantity<units1*units2, decltype(T1{}*T2{})> {
		return {lhs.get() * rhs.get()};
	}
	template <typename T1, auto units1, typename T2, auto units2>
		requires CUnit<decltype(units1)> && CUnit<decltype(units2)>
	constexpr auto operator/(
		const Quantity<units1, T1>& lhs,
		const Quantity<units2, T2>& rhs
	) -> Quantity<units1/units2, decltype(T1{}/T2{})> {
		return {lhs.get() / rhs.get()};
	}

	// Addition/Subtraction with dimensionless values
	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U&& u) {{t + u};}
	constexpr auto operator+(
		const Quantity<units, T>& lhs, U rhs
	) -> Quantity<units, decltype(T{}+U{})> {
		return lhs.get() + rhs;
	}

	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U&& u) {{t + u};}
	constexpr auto operator+(
		U lhs, const Quantity<units, T>& rhs
	) -> Quantity<units, decltype(T{}+U{})> {
		return lhs + rhs.get();
	}

	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U u) {{t - u};}
	constexpr auto operator-(
		const Quantity<units, T>& lhs, U rhs
	) -> Quantity<units, decltype(T{}-U{})> {
		return lhs.get() - rhs;
	}

	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U u) {{t - u};}
	constexpr auto operator-(
		U lhs, const Quantity<units, T>& rhs
	) -> Quantity<units, decltype(T{}-U{})> {
		return lhs - rhs.get();
	}

	// Multiplication/Division with dimensionless values
	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U&& u) {{t * u};}
	constexpr auto operator*(
		const Quantity<units, T>& lhs, U rhs
	) -> Quantity<units, decltype(T{}*U{})> {
		return lhs.get() * rhs;
	}

	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U&& u) {{t * u};}
	constexpr auto operator*(
		U lhs, const Quantity<units, T>& rhs
	) -> Quantity<units, decltype(T{}*U{})> {
		return lhs * rhs.get();
	}

	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U u) {{t / u};}
	constexpr auto operator/(
		const Quantity<units, T>& lhs, U rhs
	) -> Quantity<units, decltype(T{}/U{})> {
		return lhs.get() / rhs;
	}

	template <typename T, auto units, typename U>
		requires CUnit<decltype(units)> &&
		requires(T t, U u) {{t / u};}
	constexpr auto operator/(
		U lhs, const Quantity<units, T>& rhs
	) -> Quantity<units, decltype(T{}/U{})> {
		return lhs / rhs.get();
	}

	// Comparison
	template <typename T1, auto units1, typename T2, auto units2> requires (units1 = units2)
	constexpr auto operator<=>(const Quantity<units1, T1>& lhs, const Quantity<units2, T2>& rhs) {
		return lhs.template as<units1>() <=> rhs.template as<units1>();
	}

	template <typename T1, auto units1, typename T2, auto units2> requires (units1 = units2)
	constexpr auto operator<(const Quantity<units1, T1>& lhs, const Quantity<units2, T2>& rhs) {
		return lhs.template as<units1>() < rhs.template as<units1>();
	}

	template <typename T1, auto units1, typename T2, auto units2> requires (units1 = units2)
	constexpr auto operator==(const Quantity<units1, T1>& lhs, const Quantity<units2, T2>& rhs) {
		return lhs.template as<units1>() == rhs.template as<units1>();
	}

	template <typename T1, auto units1, typename T2, auto units2> requires (units1 = units2)
	constexpr auto operator>(const Quantity<units1, T1>& lhs, const Quantity<units2, T2>& rhs) {
		return lhs.template as<units1>() > rhs.template as<units1>();
	}



	namespace unit_literals {
		// angle units
		constexpr auto operator""_revf(long double val) {
			return Quantity<revolution, float>{static_cast<float>(val)};
		}
		constexpr auto operator""_rev(long double val) {
			return Quantity<revolution, double>{static_cast<double>(val)};
		}
		constexpr auto operator""_revl(long double val) {
			return Quantity<revolution, long double>{static_cast<long double>(val)};
		}

		constexpr auto operator""_degf(long double val) {
			return Quantity<degree, float>{static_cast<float>(val)};
		}
		constexpr auto operator""_deg(long double val) {
			return Quantity<degree, double>{static_cast<double>(val)};
		}
		constexpr auto operator""_degl(long double val) {
			return Quantity<degree, long double>{static_cast<long double>(val)};
		}

		constexpr auto operator""_radf(long double val) {
			return Quantity<radian, float>{static_cast<float>(val)};
		}
		constexpr auto operator""_rad(long double val) {
			return Quantity<radian, double>{static_cast<double>(val)};
		}
		constexpr auto operator""_radl(long double val) {
			return Quantity<radian, long double>{static_cast<long double>(val)};
		}

		// TODO: Add more unit literals
		constexpr auto operator""_km(long double val) {
			return Quantity<kilometre, double>{static_cast<double>(val)};
		}
		constexpr auto operator""_kg(long double val) {
			return Quantity<kilogram, double>{static_cast<double>(val)};
		}
	}

	// Custom mathematical functions


	template <Ratio exponent, auto unit, typename T>
	auto power(const units::Quantity<unit, T>& q) {
		using unit_t = decltype(unit);
		constexpr auto in_units = unit / typename unit_t::coef{};
		constexpr auto out_units = in_units^exponent{};

		return units::Quantity<out_units, T>{std::pow(q.template as<in_units>(), static_cast<T>(exponent::num)/exponent::den)};
	}
}

namespace std {
	template <auto units1, auto units2, typename T> requires units::CEquivalentUnit<decltype(units1), decltype(units2)>
	void swap(units::Quantity<units1, T>& lhs, units::Quantity<units2, T>& rhs) {
		swap(lhs.get(), rhs.get());
		lhs = lhs.template as<units1>();
		rhs = rhs.template as<units2>();
	}

	// Trigonemetric function overloads
	template <auto unit, typename T>
	units::Quantity<units::nonunit, T> sin(const units::Quantity<unit, T>& q) {
		return sin(q.template as<units::radian>());
	}
	template <auto unit, typename T>
	units::Quantity<units::nonunit, T> cos(const units::Quantity<unit, T>& q) {
		return cos(q.template as<units::radian>());
	}
	template <auto unit, typename T>
	units::Quantity<units::nonunit, T> tan(const units::Quantity<unit, T>& q) {
		return tan(q.template as<units::radian>());
	}
	template <auto unit, typename T> requires units::CEquivalentUnit<decltype(unit), decltype(units::nonunit)>
	units::Quantity<units::radian, T> asin(const units::Quantity<unit, T>& q) {
		return asin(q.template as<units::nonunit>());
	}
	template <auto unit, typename T> requires units::CEquivalentUnit<decltype(unit), decltype(units::nonunit)>
	units::Quantity<units::radian, T> acos(const units::Quantity<unit, T>& q) {
		return acos(q.template as<units::nonunit>());
	}
	template <auto unit, typename T> requires units::CEquivalentUnit<decltype(unit), decltype(units::nonunit)>
	units::Quantity<units::radian, T> atan(const units::Quantity<unit, T>& q) {
		return atan(q.template as<units::nonunit>());
	}

	// exponentiation

	template <auto unit, typename T>
	auto sqrt(const units::Quantity<unit, T>& q) {
		using unit_t = decltype(unit);
		constexpr auto in_units = unit / typename unit_t::coef{};
		constexpr auto out_units = in_units^std::ratio<1,2>{};

		return units::Quantity<out_units, T>{sqrt(q.template as<in_units>())};
	}

	template <auto unit, typename T>
	auto cbrt(const units::Quantity<unit, T>& q) {
		using unit_t = decltype(unit);
		constexpr auto in_units = unit / typename unit_t::coef{};
		constexpr auto out_units = in_units^std::ratio<1,3>{};

		return units::Quantity<out_units, T>{cbrt(q.template as<in_units>())};
	}

	template <auto unit, typename T> requires (unit == units::nonunit)
	auto exp(const units::Quantity<unit, T>& q) {
		return units::Quantity<units::nonunit, T>{exp(q.template as<units::nonunit>())};
	}

	template <auto unit, typename T> requires (unit == units::nonunit)
	auto exp2(const units::Quantity<unit, T>& q) {
		return units::Quantity<units::nonunit, T>{exp2(q.template as<units::nonunit>())};
	}

	template <auto unit, typename T> requires (unit == units::nonunit)
	auto expm1(const units::Quantity<unit, T>& q) {
		return units::Quantity<units::nonunit, T>{expm1(q.template as<units::nonunit>())};
	}

	template <auto unit, typename T> requires (unit == units::nonunit)
	auto log(const units::Quantity<unit, T>& q) {
		return units::Quantity<units::nonunit, T>{log(q.template as<units::nonunit>())};
	}

	template <auto unit, typename T> requires (unit == units::nonunit)
	auto log10(const units::Quantity<unit, T>& q) {
		return units::Quantity<units::nonunit, T>{log10(q.template as<units::nonunit>())};
	}

	template <auto unit, typename T> requires (unit == units::nonunit)
	auto log2(const units::Quantity<unit, T>& q) {
		return units::Quantity<units::nonunit, T>{log2(q.template as<units::nonunit>())};
	}

	template <auto unit, typename T> requires (unit == units::nonunit)
	auto log1p(const units::Quantity<unit, T>& q) {
		return units::Quantity<units::nonunit, T>{log1p(q.template as<units::nonunit>())};
	}
}
