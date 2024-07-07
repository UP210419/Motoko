import Debug "mo:base/Debug";
import Float "mo:base/Float";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Char "mo:base/Char";
import Text "mo:base/Text";

actor Calculator {
    public func add(x : Float, y : Float) : async Float {
        return x + y;
    };

    public func subtract(x : Float, y : Float) : async Float {
        return x - y;
    };

    public func multiply(x : Float, y : Float) : async Float {
        return x * y;
    };

    public func divide(x : Float, y : Float) : async ?Float {
        if (y != 0.0) {
            return ?(x / y);
        } else {
            Debug.print("Error: Division by zero");
            return null;
        };
    };

    // Función para calcular la raíz cuadrada de un número
    public func sqrt(x : Float) : async ?Float {
        if (x >= 0.0) {
            return ?(Float.sqrt(x));
        } else {
            Debug.print("Error: Square root of negative number");
            return null;
        };
    };

    // Función para calcular el porcentaje (x% de y)
    public func percentage(x : Float, y : Float) : async Float {
        return (x / 100.0) * y;
    };

    // Función para calcular la potencia (x^y)
    public func power(x : Float, y : Float) : async Float {
        return Float.pow(x, y);
    };

    // Función para resolver la ecuación cuadrática ax^2 + bx + c = 0
    public func quadraticFormula(a : Float, b : Float, c : Float) : async ?(Float, Float) {
        let discriminant = (b * b) - (4.0 * a * c);
        if (discriminant < 0.0) {
            Debug.print("Error: No real roots");
            return null;
        } else {
            let sqrtDiscriminant = Float.sqrt(discriminant);
            let root1 = (-b + sqrtDiscriminant) / (2.0 * a);
            let root2 = (-b - sqrtDiscriminant) / (2.0 * a);
            return ?(root1, root2);
        };
    };

    // Función para calcular el seno de un ángulo en grados
    public func sinDegrees(degrees : Float) : async Float {
        let radians = degreesToRadians(degrees);
        return sinApproximation(radians);
    };

    // Función para calcular el coseno de un ángulo en grados
    public func cosDegrees(degrees : Float) : async Float {
        let radians = degreesToRadians(degrees);
        return cosApproximation(radians);
    };

    // Función para calcular la tangente de un ángulo en grados
    public func tanDegrees(degrees : Float) : async Float {
        let radians = degreesToRadians(degrees);
        return tanApproximation(radians);
    };

    // Función para calcular el arcoseno de un valor en grados
    public func asinDegrees(x : Float) : async ?Float {
        if (x >= -1.0 and x <= 1.0) {
            let radians = asinApproximation(x);
            return ?radiansToDegrees(radians);
        } else {
            Debug.print("Error: asin input out of range");
            return null;
        };
    };

    // Función para calcular el arcocoseno de un valor en grados
    public func acosDegrees(x : Float) : async ?Float {
        if (x >= -1.0 and x <= 1.0) {
            let radians = acosApproximation(x);
            return ?radiansToDegrees(radians);
        } else {
            Debug.print("Error: acos input out of range");
            return null;
        };
    };

    // Función para calcular el arcotangente de un valor en grados
    public func atanDegrees(x : Float) : async Float {
        let radians = atanApproximation(x);
        return radiansToDegrees(radians);
    };

    // Aproximaciones de funciones trigonométricas y auxiliares
    private func sinApproximation(x : Float) : Float {
        var term = x;
        var sum = term;
        var n = 1;
        while (Float.abs(term) > 0.00001) {
            term *= -1.0 * x * x / (2.0 * Float.fromInt(n) * (2.0 * Float.fromInt(n) + 1.0));
            sum += term;
            n += 1;
        };
        return sum;
    };

    private func cosApproximation(x : Float) : Float {
        var term = 1.0;
        var sum = term;
        var n = 1;
        while (Float.abs(term) > 0.00001) {
            term *= -1.0 * x * x / (2.0 * Float.fromInt(n - 1) * 2.0 * Float.fromInt(n));
            sum += term;
            n += 1;
        };
        return sum;
    };

    private func tanApproximation(x : Float) : Float {
        // Serie de Taylor para la tangente:
        // tan(x) = x + x^3/3 + 2x^5/15 + 17x^7/315 + ...
        var term = x;
        var sum = term;
        var n = 1;
        while (Float.abs(term) > 0.00001) {
            term *= x * x * (2.0 * Float.fromInt(n) - 1.0) / (2.0 * Float.fromInt(n) + 1.0);
            sum += term;
            n += 1;
        };
        return sum;
    };

    private func asinApproximation(x : Float) : Float {
        var sum = x;
        var term = x;
        var n = 1;
        while (Float.abs(term) > 0.00001) {
            term *= (Float.fromInt(2 * n - 1) * x * x) / Float.fromInt(2 * n);
            sum += term / Float.fromInt(2 * n + 1);
            n += 1;
        };
        return sum;
    };

    private func acosApproximation(x : Float) : Float {
        return Float.pi / 2.0 - asinApproximation(x);
    };

    private func atanApproximation(x : Float) : Float {
        var sum = x;
        var term = x;
        var n = 1;
        while (Float.abs(term) > 0.00001) {
            term *= -x * x;
            sum += term / (2.0 * Float.fromInt(n) + 1.0);
            n += 1;
        };
        return sum;
    };

    private func sqrtApproximation(x : Float) : Float {
        var guess = x / 2.0;
        var epsilon = 0.00001;
        while (Float.abs(guess * guess - x) > epsilon) {
            guess := (guess + x / guess) / 2.0;
        };
        return guess;
    };

    private func powApproximation(x : Float, y : Float) : Float {
        if (y == 0.0) {
            return 1.0;
        } else if (y > 0.0) {
            var result = x;
            for (_ in Iter.range(1, Float.toInt(y) - 1)) {
                result *= x;
            };
            return result;
        } else {
            var result = 1.0 / x;
            for (_ in Iter.range(1, Float.toInt(-y) - 1)) {
                result /= x;
            };
            return result;
        };
    };

    // Función para convertir grados a radianes
    private func degreesToRadians(degrees : Float) : Float {
        return degrees * Float.pi / 180.0;
    };

    public func convertDegreToRadians(n : Float) : async Float {
        return degreesToRadians(n);
    };
    // Función para convertir radianes a grados
    private func radiansToDegrees(radians : Float) : Float {
        return radians * 180.0 / Float.pi;
    };

    public func convertRadianToRDegres(n : Float) : async Float {
        return radiansToDegrees(n);
    };

    // Función para calcular el logaritmo natural (ln)
    public func ln(x : Float) : async ?Float {
        if (x > 0.0) {
            var term = (x - 1.0) / (x + 1.0);
            var sum = 0.0;
            var n = 1;
            var term2 = term * term;
            while (Float.abs(term) > 0.00001) {
                sum += term / Float.fromInt(2 * n - 1);
                term *= term2;
                n += 1;
            };
            return ?(2.0 * sum);
        } else {
            Debug.print("Error: Logarithm of non-positive number");
            return null;
        };
    };

    // Función para calcular el logaritmo en base 10 (log)
    public func log(x : Float) : async ?Float {
        let ln10 = 2.302585092994046; // ln(10)
        let lnX = await ln(x);
        switch (lnX) {
            case (?lnValue) { return ?(lnValue / ln10) };
            case null { return null };
        };
    };

    // Función para calcular el exponencial (exp)
    public func exp(x : Float) : async Float {
        var term = 1.0;
        var sum = term;
        var n = 1;
        while (Float.abs(term) > 0.00001) {
            term *= x / Float.fromInt(n);
            sum += term;
            n += 1;
        };
        return sum;
    };

    // Función para calcular el logaritmo en una base arbitraria
    public func logBase(x : Float, base : Float) : async ?Float {
        if (x > 0.0 and base > 0.0 and base != 1.0) {
            let lnX = await ln(x);
            let lnBase = await ln(base);
            switch (lnX, lnBase) {
                case (?lnXValue, ?lnBaseValue) {
                    return ?(lnXValue / lnBaseValue);
                };
                case (_, _) { return null }; // Handle null cases if ln(x) or ln(base) is null
            };
        } else {
            Debug.print("Error: Invalid arguments for logarithm calculation");
            return null;
        };
    };

    // Función para calcular el factorial de un número
    private func factorial(n : Nat) : Nat {
        if (n <= 1) {
            return 1;
        } else {
            return n * factorial(n - 1);
        };
    };

    public func calculateFactorial(n : Nat) : async Nat {
        return factorial(n);
    };

    // Función para calcular las permutaciones P(n, k)
    public func permutations(n : Nat, k : Nat) : async Nat {
        return factorial(n) / factorial(n - k);
    };

    // Función para calcular las combinaciones C(n, k)
    public func combinations(n : Nat, k : Nat) : async Nat {
        return factorial(n) / (factorial(k) * factorial(n - k));
    };

    // Función para calcular la raíz n-ésima de un número
    public func nthRoot(x : Float, n : Float) : async ?Float {
        if (x >= 0.0 and n != 0.0) {
            return ?(Float.pow(x, 1.0 / n));
        } else {
            Debug.print("Error: Invalid input for nth root calculation");
            return null;
        };
    };

    // Función para calcular el factorial parcial P(n, k)
    public func partialFactorial(n : Nat, k : Nat) : async Nat {
        if (k > n) {
            Debug.print("Error: k cannot be greater than n for partial factorial");
            return 0;
        } else if (k == 0) {
            return 1;
        } else {
            var result = 1;
            for (i in Iter.range(1, k)) {
                result *= n - (i - 1);
            };
            return result;
        };
    };

    // Función para convertir un número decimal a binario
    public func decimalToBinary(decimal : Nat) : async Nat {
        var binary = 0;
        var base = 1;
        var temp = decimal;
        while (temp > 0) {
            var remainder = temp % 2;
            temp /= 2;
            binary += remainder * base;
            base *= 10;
        };
        return binary;
    };

    // Función para convertir un número decimal a octal
    public func decimalToOctal(decimal : Nat) : async Nat {
        var octal = 0;
        var base = 1;
        var temp = decimal;
        while (temp > 0) {
            var remainder = temp % 8;
            temp /= 8;
            octal += remainder * base;
            base *= 10;
        };
        return octal;
    };

    // Función para convertir un número octal a decimal
    public func octalToDecimal(octal : Nat) : async Nat {
        var decimal = 0;
        var base = 1;
        var temp = octal;
        while (temp > 0) {
            var remainder = temp % 10;
            temp /= 10;
            decimal += remainder * base;
            base *= 8;
        };
        return decimal;
    };

    // Función para convertir un número binario a decimal
    public func binaryToDecimal(binary : Nat) : async Nat {
        var decimal = 0;
        var base = 1;
        var temp = binary;
        while (temp > 0) {
            var remainder = temp % 10;
            temp /= 10;
            decimal += remainder * base;
            base *= 2;
        };
        return decimal;
    };

    // Función para calcular el valor absoluto de un número
    public func abs(x : Float) : async Float {
        if (x >= 0.0) {
            return x;
        } else {
            return -x;
        };
    };

    // Función para calcular la hipotenusa de un triángulo rectángulo
    public func hypot(a : Float, b : Float) : async Float {
        return Float.sqrt(a * a + b * b);
    };

    // Función para calcular el cateto a de un triángulo rectángulo
    public func cateto(c : Float, b : Float) : async Float {
        return Float.sqrt(c * c - b * b);
    };

    // Función para convertir segundos a minutos
    public func secondsToMinutes(seconds : Float) : async Float {
        return seconds / 60.0;
    };

    // Función para convertir minutos a horas
    public func minutesToHours(minutes : Float) : async Float {
        return minutes / 60.0;
    };

    // Función para convertir horas a días
    public func hoursToDays(hours : Float) : async Float {
        return hours / 24.0;
    };

    // Función para convertir metros cuadrados a pies cuadrados
    public func squareMetersToSquareFeet(squareMeters : Float) : async Float {
        return squareMeters * 10.7639; // 1 metro cuadrado = 10.7639 pies cuadrados
    };

    // Función para convertir litros a galones
    public func litersToGallons(liters : Float) : async Float {
        return liters * 0.264172; // 1 litro = 0.264172 galones
    };

    // Función para convertir kilogramos a libras
    public func kilogramsToPounds(kilograms : Float) : async Float {
        return kilograms * 2.20462; // 1 kilogramo = 2.20462 libras
    };

    // Función para convertir julios a calorías
    public func joulesToCalories(joules : Float) : async Float {
        return joules * 0.239006; // 1 julio = 0.239006 calorías
    };

    // Función para convertir pascales a atmósferas
    public func pascalsToAtmospheres(pascals : Float) : async Float {
        return pascals * 0.00000986923; // 1 pascal = 0.00000986923 atmósferas
    };

    // Función para convertir Celsius a Kelvin
    public func celsiusToKelvin(celsius : Float) : async Float {
        return celsius + 273.15;
    };

    // Función para convertir millas por hora a kilómetros por hora
    public func mphToKmph(mph : Float) : async Float {
        return mph * 1.60934; // 1 milla por hora = 1.60934 kilómetros por hora
    };

    // Función para convertir grados Celsius a Fahrenheit
    public func celsiusToFahrenheit(celsius : Float) : async Float {
        return (celsius * 9.0/5.0) + 32.0;
    };

    // Función para convertir grados Fahrenheit a Celsius
    public func fahrenheitToCelsius(fahrenheit : Float) : async Float {
        return (fahrenheit - 32.0) * 5.0/9.0;
    };

    // Función para convertir metros a pies
    public func metersToFeet(meters : Float) : async Float {
        return meters * 3.28084;
    };

    // Función para convertir pies a metros
    public func feetToMeters(feet : Float) : async Float {
        return feet / 3.28084;
    };

    // Función para convertir minutos a segundos
    public func minutesToSeconds(minutes : Float) : async Float {
        return minutes * 60.0;
    };

    // Función para convertir horas a minutos
    public func hoursToMinutes(hours : Float) : async Float {
        return hours * 60.0;
    };

    // Función para convertir días a horas
    public func daysToHours(days : Float) : async Float {
        return days * 24.0;
    };

    // Función para convertir pies cuadrados a metros cuadrados
    public func squareFeetToSquareMeters(squareFeet : Float) : async Float {
        return squareFeet / 10.7639; // 1 pie cuadrado = 0.092903 metros cuadrados
    };

    // Función para convertir galones a litros
    public func gallonsToLiters(gallons : Float) : async Float {
        return gallons / 0.264172; // 1 galón = 3.78541 litros
    };

    // Función para convertir libras a kilogramos
    public func poundsToKilograms(pounds : Float) : async Float {
        return pounds / 2.20462; // 1 libra = 0.453592 kilogramos
    };

    // Función para convertir calorías a julios
    public func caloriesToJoules(calories : Float) : async Float {
        return calories / 0.239006; // 1 caloría = 4.184 julios
    };

    // Función para convertir atmósferas a pascales
    public func atmospheresToPascals(atmospheres : Float) : async Float {
        return atmospheres / 0.00000986923; // 1 atmósfera = 101325 pascales
    };

    // Función para convertir Kelvin a Celsius
    public func kelvinToCelsius(kelvin : Float) : async Float {
        return kelvin - 273.15;
    };

    // Función para convertir kilómetros por hora a millas por hora
    public func kmphToMph(kmph : Float) : async Float {
        return kmph / 1.60934; // 1 kilómetro por hora = 0.621371 millas por hora
    };

    // Función para convertir grados Fahrenheit a Kelvin
    public func fahrenheitToKelvin(fahrenheit : Float) : async Float {
        return (fahrenheit - 32.0) * 5.0/9.0 + 273.15;
    };
};
