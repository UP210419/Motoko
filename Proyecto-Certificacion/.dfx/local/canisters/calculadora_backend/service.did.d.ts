import type { Principal } from '@dfinity/principal';
import type { ActorMethod } from '@dfinity/agent';
import type { IDL } from '@dfinity/candid';

export interface _SERVICE {
  'abs' : ActorMethod<[number], number>,
  'acosDegrees' : ActorMethod<[number], [] | [number]>,
  'add' : ActorMethod<[number, number], number>,
  'asinDegrees' : ActorMethod<[number], [] | [number]>,
  'atanDegrees' : ActorMethod<[number], number>,
  'atmospheresToPascals' : ActorMethod<[number], number>,
  'binaryToDecimal' : ActorMethod<[bigint], bigint>,
  'calculateFactorial' : ActorMethod<[bigint], bigint>,
  'caloriesToJoules' : ActorMethod<[number], number>,
  'cateto' : ActorMethod<[number, number], number>,
  'celsiusToFahrenheit' : ActorMethod<[number], number>,
  'celsiusToKelvin' : ActorMethod<[number], number>,
  'combinations' : ActorMethod<[bigint, bigint], bigint>,
  'convertDegreToRadians' : ActorMethod<[number], number>,
  'convertRadianToRDegres' : ActorMethod<[number], number>,
  'cosDegrees' : ActorMethod<[number], number>,
  'daysToHours' : ActorMethod<[number], number>,
  'decimalToBinary' : ActorMethod<[bigint], bigint>,
  'decimalToOctal' : ActorMethod<[bigint], bigint>,
  'divide' : ActorMethod<[number, number], [] | [number]>,
  'exp' : ActorMethod<[number], number>,
  'fahrenheitToCelsius' : ActorMethod<[number], number>,
  'fahrenheitToKelvin' : ActorMethod<[number], number>,
  'feetToMeters' : ActorMethod<[number], number>,
  'gallonsToLiters' : ActorMethod<[number], number>,
  'hoursToDays' : ActorMethod<[number], number>,
  'hoursToMinutes' : ActorMethod<[number], number>,
  'hypot' : ActorMethod<[number, number], number>,
  'joulesToCalories' : ActorMethod<[number], number>,
  'kelvinToCelsius' : ActorMethod<[number], number>,
  'kilogramsToPounds' : ActorMethod<[number], number>,
  'kmphToMph' : ActorMethod<[number], number>,
  'litersToGallons' : ActorMethod<[number], number>,
  'ln' : ActorMethod<[number], [] | [number]>,
  'log' : ActorMethod<[number], [] | [number]>,
  'logBase' : ActorMethod<[number, number], [] | [number]>,
  'metersToFeet' : ActorMethod<[number], number>,
  'minutesToHours' : ActorMethod<[number], number>,
  'minutesToSeconds' : ActorMethod<[number], number>,
  'mphToKmph' : ActorMethod<[number], number>,
  'multiply' : ActorMethod<[number, number], number>,
  'nthRoot' : ActorMethod<[number, number], [] | [number]>,
  'octalToDecimal' : ActorMethod<[bigint], bigint>,
  'partialFactorial' : ActorMethod<[bigint, bigint], bigint>,
  'pascalsToAtmospheres' : ActorMethod<[number], number>,
  'percentage' : ActorMethod<[number, number], number>,
  'permutations' : ActorMethod<[bigint, bigint], bigint>,
  'poundsToKilograms' : ActorMethod<[number], number>,
  'power' : ActorMethod<[number, number], number>,
  'quadraticFormula' : ActorMethod<
    [number, number, number],
    [] | [[number, number]]
  >,
  'secondsToMinutes' : ActorMethod<[number], number>,
  'sinDegrees' : ActorMethod<[number], number>,
  'sqrt' : ActorMethod<[number], [] | [number]>,
  'squareFeetToSquareMeters' : ActorMethod<[number], number>,
  'squareMetersToSquareFeet' : ActorMethod<[number], number>,
  'subtract' : ActorMethod<[number, number], number>,
  'tanDegrees' : ActorMethod<[number], number>,
}
export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];