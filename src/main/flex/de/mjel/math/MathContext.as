/*
 * Copyright (C) 2013 - bigdecimal-as (http://evan.mjel.de/)
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software is
 * freely granted, provided that this notice is preserved.
 */
package de.mjel.math {

   /**
    * The <code>MathContext</code> immutable class encapsulates the
    * settings understood by the operator methods of the <code>BigDecimal</code>
    * class (and potentially other classes). Operator methods are those
    * that effect an operation on a number or a pair of numbers.
    *
    * <p>The settings, which are not base-dependent, comprise:
    *   <ol>
    *     <li><code>digits</code>: the number of digits (precision) to be used for an operation
    *     <li><code>form</code>: the form of any exponent that results from the operation
    *     <li><code>lostDigits</code>: whether checking for lost digits is enabled
    *     <li><code>roundingMode</code>: the algorithm to be used for rounding.
    *   </ol><p>
    *
    * When provided, a <code>MathContext</code> object supplies the settings for an operation directly.
    *
    * <p>When <code>MathContext.DEFAULT</code> is provided for a <code>MathContext</code>
    *    parameter then the default settings are used (<code>9, SCIENTIFIC, false, ROUND_HALF_UP</code>).<p>
    *
    * In the <code>BigDecimal</code> class, all methods which accept a <code>MathContext</code>
    * object defaults) also have a version of the method which does not accept a MathContext parameter.
    * These versions carry out unlimited precision fixed point arithmetic
    * (as though the settings were (<code>0, PLAIN, false, ROUND_HALF_UP</code>).<p>
    *
    * The instance variables are shared with default access (so they are directly accessible to
    * the <code>BigDecimal</code> class), but must never be changed.
    *
    * <p>The rounding mode constants have the same names and values as the constants of
    *    the same name in <code>java.math.BigDecimal</code>, to maintain compatibility
    *    with earlier versions of <code>BigDecimal</code>.</p>
    */
   public class MathContext {

      /**
       * Rounding mode to round to a more positive number.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>If any of the discarded digits are non-zero then the result
       *    should be rounded towards the next more positive digit.</p>
       */
      public static const ROUND_CEILING:int = 2;

      /**
       * Rounding mode to round towards zero.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>All discarded digits are ignored (truncated). The result is
       *    neither incremented nor decremented.</p>
       */
      public static const ROUND_DOWN:int = 1;

      /**
       * Rounding mode to round to a more negative number.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>If any of the discarded digits are non-zero then the result
       *    should be rounded towards the next more negative digit.</p>
       */
      public static const ROUND_FLOOR:int = 3;

      /**
       * Rounding mode to round to nearest neighbor, where an equidistant
       * value is rounded down.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>If the discarded digits represent greater than half (0.5 times)
       *    the value of a one in the next position then the result should be
       *    rounded up (away from zero). Otherwise the discarded digits are
       *    ignored.
       */
      public static const ROUND_HALF_DOWN:int = 5;

      /**
       * Rounding mode to round to nearest neighbor, where an equidistant
       * value is rounded to the nearest even neighbor.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>If the discarded digits represent greater than half (0.5 times)
       *    the value of a one in the next position then the result should be
       *    rounded up (away from zero). If they represent less than half,
       *    then the result should be rounded down.</p>
       *
       * <p>Otherwise (they represent exactly half) the result is rounded
       *    down if its rightmost digit is even, or rounded up if its
       *    rightmost digit is odd (to make an even digit).</p>
       */
      public static const ROUND_HALF_EVEN:int = 6;

      /**
       * Rounding mode to round to nearest neighbor, where an equidistant
       * value is rounded up.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>If the discarded digits represent greater than or equal to half
       *    (0.5 times) the value of a one in the next position then the result
       *    should be rounded up (away from zero). Otherwise the discarded
       *    digits are ignored.</p>
       */
      public static const ROUND_HALF_UP:int = 4;

      /**
       * Rounding mode to assert that no rounding is necessary.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>Rounding (potential loss of information) is not permitted.
       *    If any of the discarded digits are non-zero then an
       *    <code>ArithmeticException</code> should be thrown.</p>
       */
      public static const ROUND_UNNECESSARY:int = 7;

      /**
       * Rounding mode to round away from zero.
       * Used as a setting to control the rounding mode used during a
       * <code>BigDecimal</code> operation.
       *
       * <p>If any of the discarded digits are non-zero then the result will
       *    be rounded up (away from zero).</p>
       */
      public static const ROUND_UP:int = 0;

      /**
       * Get a new MathContext whose rounding mode is ROUND_UNNECESSARY and
       * defaults for other properties
       *
       * @return a new MathContext
       */
      public static function roundUnnecessary():MathContext {
         return new MathContext(DEFAULT_DIGITS, DEFAULT_LOSTDIGITS, ROUND_UNNECESSARY);
      }

      private var _digits:int;

      /**
       * The number of digits (precision) to be used for an operation.
       * A value of 0 indicates that unlimited precision (as many digits
       * as are required) will be used.
       *
       * <p>The <code>BigDecimal</code> operator methods use this value to
       *    determine the precision of results.
       *    Note that leading zeros (in the integer part of a number) are
       *    never significant.
       *
       * <p><code>digits</code> will always be non-negative.</p>
       */
      public function get digits():int {
         return _digits;
      }

      private function setDigits(value:int):void {
         if (value < MIN_DIGITS) {
            throw new ArgumentError("Digits too small: " + value);
         }
         if (value > MAX_DIGITS) {
            throw new ArgumentError("Digits too large: " + value);
         }
         _digits = value;
      }

      private var _lostDigits:Boolean;

      /**
       * Controls whether lost digits checking is enabled for an operation.
       * Set to <code>true</code> to enable checking, or to <code>false</code> to disable checking.
       *
       * <p>When enabled, the <code>BigDecimal</code> operator methods check
       *    the precision of their operand or operands, and throw an
       *    <code>ArithmeticException</code> if an operand is more precise
       *    than the digits setting (that is, digits would be lost).
       *    When disabled, operands are rounded to the specified digits.</p>
       */
      public function get lostDigits():Boolean {
         return _lostDigits;
      }

      private var _roundingMode:int;

      /**
       * The rounding algorithm to be used for an operation.
       *
       * <p>The <code>BigDecimal</code> operator methods use this value to
       *    determine the algorithm to be used when non-zero digits have to
       *    be discarded in order to reduce the precision of a result.
       *    The value must be one of the public constants whose name starts
       *    with <code>ROUND_</code>.
       *
       * @see #ROUND_CEILING
       * @see #ROUND_DOWN
       * @see #ROUND_FLOOR
       * @see #ROUND_HALF_DOWN
       * @see #ROUND_HALF_EVEN
       * @see #ROUND_HALF_UP
       * @see #ROUND_UNNECESSARY
       * @see #ROUND_UP
       */
      public function get roundingMode():int {
         return _roundingMode;
      }

      private function setRoundingMode(value:int):void {
         if (ROUNDS.indexOf(value) == -1) {
            throw new ArgumentError("Bad roundingMode value: " + value);
         }
         _roundingMode = value;
      }

      // default settings
      private static const DEFAULT_DIGITS:int = 9;
      private static const DEFAULT_LOSTDIGITS:Boolean = false;
      private static const DEFAULT_ROUNDINGMODE:int = ROUND_HALF_UP;

      private static const MIN_DIGITS:int = 0; // smallest value for DIGITS.
      private static const MAX_DIGITS:int = 999999999; // largest value for DIGITS. If increased,

      // the BigDecimal class may need update.
      // list of valid rounding mode values, most common two first
      private static const ROUNDS:Array = [ROUND_HALF_UP, ROUND_UNNECESSARY, ROUND_CEILING, ROUND_DOWN, ROUND_FLOOR, ROUND_HALF_DOWN, ROUND_HALF_EVEN, ROUND_UP];

      private static const ROUNDWORDS:Array = ["ROUND_HALF_UP", "ROUND_UNNECESSARY", "ROUND_CEILING", "ROUND_DOWN", "ROUND_FLOOR", "ROUND_HALF_DOWN", "ROUND_HALF_EVEN", "ROUND_UP"]; // matching names of the ROUNDS values

      /**
       * A <code>MathContext</code> object initialized to the default
       * settings for general-purpose arithmetic. That is,
       * <code>digits=9 form=SCIENTIFIC lostDigits=false
       * roundingMode=ROUND_HALF_UP</code>.
       *
       * @see #ROUND_HALF_UP
       */
      public static const DEFAULT:MathContext = new MathContext(DEFAULT_DIGITS, DEFAULT_LOSTDIGITS, DEFAULT_ROUNDINGMODE);

      public static const PLAIN:MathContext = new MathContext(0); // context for plain unlimited math

      /**
       * Constructs a new <code>MathContext</code> with a specified
       * precision, form, lostDigits, and roundingMode setting.
       *
       * <p>An <code>IllegalArgumentException</code> is thrown if the
       *    <code>digits</code> parameter is out of range
       *    (&lt;0 or &gt;999999999), or if the value given for the
       *    <code>form</code> or <code>roundingMode</code> parameters is
       *    not one of the appropriate constants.</p>
       *
       * @param digits The <code>int</code> digits setting for this <code>MathContext</code>.
       * @param lostDigits The <code>boolean</code> lostDigits setting for this <code>MathContext</code>.
       * @param roundingMode The <code>int</code> roundingMode setting for this <code>MathContext</code>.
       * @throws ArgumentError parameter out of range.
       */
      public function MathContext(digits:int, lostDigits:Boolean = DEFAULT_LOSTDIGITS, roundingMode:int = DEFAULT_ROUNDINGMODE) {
         // set values, after checking
         setDigits(digits);
         setRoundingMode(roundingMode);
         _lostDigits = lostDigits; // [no bad value possible]
      }

      /**
       * Returns the <code>MathContext</code> as a readable string.
       * The <code>String</code> returned represents the settings of the
       * <code>MathContext</code> object as four blank-delimited words
       * separated by a single blank and with no leading or trailing blanks,
       * as follows:
       * <ol>
       *   <li><code>digits=</code>, immediately followed by the value of the digits setting as a numeric word.</li>
       *   <li><code>form=</code>, immediately followed by the value of the form setting as an uppercase word
       *       (one of <code>SCIENTIFIC</code>, <code>PLAIN</code>, or <code>ENGINEERING</code>).</li>
       *   <li><code>lostDigits=</code>, immediately followed by the value of the lostDigits setting
       *       (<code>1</code> if enabled, <code>0</code> if disabled).</li>
       *   <li><code>roundingMode=</code>, immediately followed by the value of the roundingMode setting as a word.
       *        This word will be the same as the name of the corresponding public constant.</li>
       * </ol>
       *
       * <p>For example: <code>digits=9 form=SCIENTIFIC lostDigits=0 roundingMode=ROUND_HALF_UP</code></p>
       *
       * <p>Additional words may be appended to the result of
       *    <code>toString</code> in the future if more properties are added
       *    to the class.</p>
       *
       * @return a <code>String</code> representing the context settings.
       */
      public function toString():String {
         const r:int = ROUNDS.indexOf(_roundingMode);
         var roundWord:String = ROUNDWORDS[r];
         return "digits=" + digits + " " + "lostDigits=" + (lostDigits ? "1" : "0") + " " + "roundingMode=" + roundWord;
      }
   }
}
