/*
 * Copyright (C) 2013 - bigdecimal-as (http://evan.mjel.de/)
 * All rights reserved.
 *
 * Based on BigDecimal for Java.
 * ActionScript port by Jean-Francois Larouche.
 * Improved by Maurice Nicholson, forked from https://github.com/maurice/BigDecimal.as
 *
 * Permission to use, copy, modify, and distribute this software is
 * freely granted, provided that this notice is preserved.
 */
package de.mjel.math {
   import flash.system.Capabilities;
   import flash.utils.IDataInput;
   import flash.utils.IDataOutput;
   import flash.utils.IExternalizable;
   import flash.utils.getQualifiedClassName;

   /**
    * The <code>BigDecimal</code> class implements immutable
    * arbitrary-precision decimal numbers.
    */
   public class BigDecimal implements IExternalizable {
      /**
       * Used by BigDecimal internally to call its own constructor with a "null" parameter.
       * Note since statics are initialized in declaration order, top to bottom,
       * this needs to come before the public constants defined with createStatic
       */
      private static const NULL:Object = {};

      /**
       * The <code>BigDecimal</code> constant "0".
       *
       * @see #ONE
       * @see #TEN
       */
      public static const ZERO:BigDecimal = createStatic(0); // use long as we want the int constructor
      // .. to be able to use this, for speed

      /**
       * The <code>BigDecimal</code> constant "1".
       *
       * @see #TEN
       * @see #ZERO
       */
      public static const ONE:BigDecimal = createStatic(1); // use long as we want the int constructor
      // .. to be able to use this, for speed

      /**
       * The <code>BigDecimal</code> constant "10".
       *
       * @see #ONE
       * @see #ZERO
       */
      public static const TEN:BigDecimal = createStatic(10);

      private static const ispos:int = 1; // ind: indicates positive (must be 1)
      private static const iszero:int = 0; // ind: indicates zero (must be 0)
      private static const isneg:int = -1; // ind: indicates negative (must be -1)
      // [later could add NaN, +/- infinity, here]

      // ActionScript 3 Port
      // In AS3 there is no char, byte or native arrays
      // So lets create the int value of each char we need in the algorythms
      private static const CODE_0:int = "0".charCodeAt(0);
      private static const CODE_9:int = "9".charCodeAt(0);
      private static const CODE_e:int = "e".charCodeAt(0);
      private static const CODE_E:int = "E".charCodeAt(0);
      private static const CODE_dot:int = ".".charCodeAt(0);
      private static const CODE_plus:int = "+".charCodeAt(0);
      private static const CODE_minus:int = "-".charCodeAt(0);

      // Precalculated constant arrays (used by byteaddsub)
      private static const bytecar:Vector.<int> = new Vector.<int>((90 + 99) + 1, true); // carry/borrow array
      private static const bytedig:Vector.<int> = diginit(); // next digit array

      // If true, we need to explicitly set lengthened vectors extra digits to 0
      private static const vectorPadFix:Boolean = needVectorPadFix();

      /**
       * The indicator. This may take the values:
       * <ul>
       *   <li>ispos -- the number is positive
       *   <li>iszero -- the number is zero
       *   <li>isneg -- the number is negative
       * </ul>
       */
      private var ind:int; // assumed undefined
      // Note: some code below assumes IND = Sign [-1, 0, 1], at present.
      // We only need two bits for this, but use a byte [also permits smooth future extension].

      /**
       * The value of the mantissa.
       *
       * <p>Once constructed, this may become shared between several BigDecimal objects, so must not be altered.
       * <p>For efficiency (speed), this is a byte array, with each byte taking a value of 0 -> 9.<p>
       *
       * If the first byte is 0 then the value of the number is zero
       * (and mant.length=1, except when constructed from a plain number, for example, 0.000).
       */
      private var mant:Vector.<int>; // assumed null

      /**
       * The exponent.
       *
       * <p>For fixed point arithmetic, scale is <code>-exp</code>, and can apply to zero.</p<
       *
       * Note that this property can have a value less than MinExp when the mantissa has more than one digit.
       */
      private var exp:int; // assumed 0

      /**
       * Caches toString after first built.
       */
      private var string:String;

      /**
       * Constructs a new <code>BigDecimal</code> instance from a given unscaled value
       * <code>unscaledVal</code> and a given scale. The value of this instance is
       * <code>unscaledVal</code> (<code>10^-scale</code>). The result is rounded according
       * to the specified math context.
       *
       * @param unscaledValue <code>String</code> representing the unscaled integer value of this
       *                      <code>BigDecimal</code> instance.
       * @param scale scale of this <code>BigDecimal</code> instance.
       * @param context rounding mode and precision for the result of this operation.
       * @throws ArgumentError if <code>mc.precision > 0</code> and <code>context.roundingMode == UNNECESSARY</code>
       *                       and the new big decimal cannot be represented
       *                       within the given precision without rounding.
       * @throws TypeError if <code>unscaledValue == null</code>.
       */
      public static function createFromUnscaledInteger(unscaledValue:String, scale:int=0, context:MathContext=null):BigDecimal {
         const instance:BigDecimal = new BigDecimal(unscaledValue);
         if (instance.exp != 0 && scale) {
            throw new ArgumentError("The unscaledValue already has a scale: " + unscaledValue);
         }
         instance.exp = -scale;
         return instance.plus(context);
      }

      /**
       * Constructs a <code>BigDecimal</code> object from the given input.
       *
       * <p>The given <code>value</code> may be a <code>String</code> (in which
       *    case <code>offset</code> and <code>length</code> describe the part
       *    of the String to use), an <code>int</code>, a <code>Number</code>.</p>
       *
       * @param value An <code>Object</code> to create the <code>BigDecimal</code> from;
       *        should not be null, NaN, +Infinity or -Infinity.
       * @param offset If <code>value</code> is a <code>String</code>, the offset into
       *        character array of the start of the number to be converted.
       * @param length If <code>value</code> is a <code>String</code>, the length of the number.
       */
      public function BigDecimal(value:Object=0, offset:int=0, length:int=-1) {
         if (value is String) {
            createFromString(value as String, offset, length);
         }
         else if (value is int) {
            createFromInt(value as int);
         }
         else if (value is Number) {
            if (isNaN(Number(value)) || !isFinite(Number(value))) {
               throw new ArgumentError("Infinite or NaN");
            }
            createFromString(String(value as Number), offset, length);
         }
         else if (value == null) {
            throw new ArgumentError("The value parameter cannot be null");
         }
         else if (value != NULL) { //ActionScript 3 : allow internal code to bypass constructor
            throw new ArgumentError("Unhandled parameter type: " + getQualifiedClassName(value));
         }
      }

      /**
       * Populates this BigDecimal from the given string.
       *
       * @param str string to build BigDecimal from.
       * @param offset offset to start parsing at.
       * @param length length of the substring to parse, if any, otherwise -1.
       * @throws ArgumentError if the given string, offset or length are invalid.
       */
      private function createFromString(str:String, offset:int=0, length:int=-1):void {
         if (length == -1) {
            length = str.length;
         }

         if (length <= 0 || length - offset > str.length) {
            throw new ArgumentError("Invalid offset/length: " + offset + "/" + length + " for string: " + str);
         }

         var scientific:Boolean; // true for XXXeYYY numbers
         var dot:int = -1; // index of decimal point, if any
         var c:int; // current character code

         // check for sign
         ind = 1; // assume positive
         c = str.charCodeAt(offset);
         if (c == CODE_minus) {
            // negative number
            ind = -1;
            offset++
         }
         else if (c == CODE_plus) {
            offset++
         }

         // locate mantissa, dot and exponent indicator
         var nz:int = offset;
         var zero:Boolean = true; // true if the last mantissa digit was zero
         for (var i:int = offset; i < length; i++) {
            c = str.charCodeAt(i);
            if (c >= CODE_0 && c <= CODE_9) {
               if (zero) {
                  offset = i; // offset is index of first number, ignoring leading zeroes
               }
               if (c != CODE_0) {
                  nz = i; // index of last *non-zero* number in mantissa
                  zero = false;
               }
               continue;
            }

            if (c == CODE_dot) {
               if (dot != -1) {
                  throw new ArgumentError("Not a number: " + str);
               }
               // save dot index and continue
               dot = i;
               continue;
            }

            if (c == CODE_e || c == CODE_E) {
               // process exponent in a different loop
               scientific = true;
               break;
            }

            throw new ArgumentError("Not a number: " + str);
         }

         const end:int = i - 1; // index of final mantissa digit

         // determine mantissa storage length
         const len:int = i - offset - (dot >= offset ? 1 : 0);
         if (len == 0) {
            throw new ArgumentError("Not a number: " + str);
         }

         // allocate and populate mantissa
         mant = new Vector.<int>(len, true);
         for (i = 0; offset <= nz; offset++) {
            c = str.charCodeAt(offset);
            if (offset != dot) {
               mant[i++] = (c - CODE_0);
            }
         }

         // zero?
         if (mant[0] == 0) {
            ind = 0;
         }

         // parse exponent
         var exponent:Number = 0;
         if (scientific) {
            offset = end + 2; // offset is the index of first exponent digit

            var neg:Boolean;
            c = str.charCodeAt(offset);
            if (c == CODE_minus) {
               // negative exponent
               neg = true;
               offset++
            }
            else if (c == CODE_plus) {
               offset++
            }

            if (length - offset > 10) {
               throw new ArgumentError("Exponent too large: " + str);
            }

            for (i = offset; i < length; i++) {
               c = str.charCodeAt(i);
               if (c >= CODE_0 && c <= CODE_9) {
                  exponent = (exponent * 10) + (c - CODE_0);
                  continue;
               }

               throw new ArgumentError("Not a number: " + str);
            }

            if (i == offset) { // no exponent digits 
               throw new ArgumentError("Not a number: " + str);
            }
            if (exponent > int.MAX_VALUE) {
               throw new ArgumentError("Exponent too large: " + str);
            }

            if (neg) {
               exponent = -exponent;
            }
         }

         // whole number? If so, dot is implicit
         if (dot == -1) {
            dot = end;
         }

         // set exponent, adjusting for dot position if any
         exp = exponent - (end - dot);
      }

      /**
       * Constructs a <code>BigDecimal</code> object directly from a <code>int</code>.
       *
       * <p>Constructs a <code>BigDecimal</code> which is the exact decimal
       *    representation of the 32-bit signed binary integer parameter.
       *    The <code>BigDecimal</code> will contain only decimal digits,
       *    prefixed with a leading minus sign (hyphen) if the parameter is negative.
       *    A leading zero will be present only if the parameter is zero.</p>
       *
       * @param num The <code>int</code> to be converted.
       */
      private function createFromInt(num:int=0):void {
         var mun:int;
         var i:int;

         // We fastpath commoners
         if (num <= 9) {
            if (num >= -9) { /*singledigit*/
               // very common single digit case
               if (num == 0) {
                  mant = ZERO.mant;
                  ind = iszero;
               }
               else if (num == 1) {
                  mant = ONE.mant;
                  ind = ispos;
               }
               else if (num == -1) {
                  mant = ONE.mant;
                  ind = isneg;
               }
               else {
                  {
                     mant = new Vector.<int>(1);
                     if (num > 0) {
                        mant[0] = num;
                        ind = ispos;
                     }
                     else {
                        // num<-1
                        mant[0] = -num;
                        ind = isneg;
                     }
                  }
               }

               return;
            }
            /*singledigit*/
         }

         // We work on negative numbers so we handle the most negative number
         if (num > 0) {
            ind = ispos;
            num = -num;
         }
         else
         {
            ind = isneg;
            /* negative */ // [0 case already handled]
         }

         // [it is quicker, here, to pre-calculate the length with
         // one loop, then allocate exactly the right length of byte array,
         // then re-fill it with another loop]
         mun = num; // working copy

         for (i = 9; ; i--) {
            mun = int(mun / 10);
            if (mun == 0) {
               break;
            }
         }

         // i is the position of the leftmost digit placed
         mant = new Vector.<int>(10 - i);
         i = (10 - i) - 1;
         for (; ; i--) {
            mant[i] = -(int(num % 10));
            num = int(num / 10);
            if (num == 0) {
               break;
            }
         }
      }

      /**
       * Constructs a <code>BigDecimal</code> object directly from a <code>int</code>.
       * <p>Constructs a <code>BigDecimal</code> which is the exact decimal
       *    representation of the 64-bit signed binary integer parameter.
       *    The <code>BigDecimal</code> will contain only decimal digits,
       *    prefixed with a leading minus sign (hyphen) if the parameter is negative.
       *    A leading zero will be present only if the parameter is zero.</p>
       *
       * @param num The <code>long</code> to be converted.
       */
      //ActionScript 3 : This is to patch the fact that AS3 dont support Overriding
      //of Methods.  We need a way to construct the static constants without using 
      //them in the 1st place with the normal int constructor.
      private static function createStatic(num:int):BigDecimal {
         const r:BigDecimal = new BigDecimal(NULL);

         // Not really worth fastpathing commoners in this constructor [also,
         // we use this to construct the static constants].
         // This is much faster than: this(String.valueOf(num).toCharArray())
         /* We work on negative num so we handle the most negative number */

         if (num > 0) {
            r.ind = ispos;
            num = -num;
         }
         else if (num == 0) {
            r.ind = iszero;
         }
         else {
            r.ind = isneg;
         }

         var mun:int = num;
         var i:int = 18;
         for (; ; i--) {
            mun = int(mun / 10);
            if (mun == 0) {
               break;
            }
         }

         // i is the position of the leftmost digit placed
         r.mant = new Vector.<int>(19 - i);
         i = (19 - i) - 1;
         for (; ; i--) {
            r.mant[i] = -((num % 10));
            num = int(num / 10);
            if (num == 0) {
               break;
            }
         }

         return r;
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is the absolute value of <code>this</code>.
       * The result is rounded according to the passed context <code>context</code>.
       *
       * @param context rounding mode and precision for the result of this operation.
       * @return <code>abs(this)</code>.
       */
      public function abs(context:MathContext=null):BigDecimal {
         if (context == null) {
            context = MathContext.PLAIN;
         }
         if (ind == isneg) {
            return negate(context);
         }
         return plus(context);
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is <code>this + rhs</code>.
       * The result is rounded according to the passed context <code>context</code>.
       *
       * @param rhs value to be added to <code>this</code>.
       * @param context rounding mode and precision for the result of this operation.
       * @return <code>this + rhs</code>.
       */
      public function add(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         var newlen:int = 0;
         var tlen:int = 0;
         var mult:int = 0;
         var t:Vector.<int> = null;
         var ia:int = 0;
         var ib:int = 0;
         var ea:int = 0;
         var eb:int = 0;
         var ca:int = 0;
         var cb:int = 0;

         if (context == null) {
            context = MathContext.PLAIN;
         }

         // Determine requested digits and form
         if (context.lostDigits) {
            checkdigits(rhs, context.digits);
         }

         var lhs:BigDecimal = this; // name for clarity and proxy

         /* Quick exit for add floating 0 */
         // plus() will optimize to return same object if possible
         if (lhs.ind == iszero) {
            return rhs.plus(context);
         }
         if (rhs.ind == iszero) {
            return lhs.plus(context);
         }

         /* Prepare numbers (round, unless unlimited precision) */
         var reqdig:int = context.digits; // local copy (heavily used)
         if (reqdig > 0) {
            if (lhs.mant.length > reqdig) {
               lhs = clone(lhs).roundContext(context);
            }
            if (rhs.mant.length > reqdig) {
               rhs = clone(rhs).roundContext(context);
               // [we could reuse the new LHS for result in this case]
            }
         }

         const res:BigDecimal = new BigDecimal(); // build result here

         /* Now see how much we have to pad or truncate lhs or rhs in order
            to align the numbers. If one number is much larger than the
            other, then the smaller cannot affect the answer [but we may
            still need to pad with up to DIGITS trailing zeros]. */
         // Note sign may be 0 if digits (reqdig) is 0
         // usel and user will be the byte arrays passed to the adder; we'll
         // use them on all paths except quick exits
         var usel:Vector.<int> = lhs.mant;
         var usellen:int = lhs.mant.length;
         var user:Vector.<int> = rhs.mant;
         var userlen:int = rhs.mant.length;

         /*padder*/
         if (lhs.exp == rhs.exp) {
            /* no padding needed */
            // This is the most common, and fastest, path
            res.exp = lhs.exp;
         }
         else if (lhs.exp > rhs.exp) {
            // need to pad lhs and/or truncate rhs
            newlen = (usellen + lhs.exp) - rhs.exp;
            /* If, after pad, lhs would be longer than rhs by digits+1 or
               more (and digits>0) then rhs cannot affect answer, so we only
               need to pad up to a length of DIGITS+1. */
            if (newlen >= ((userlen + reqdig) + 1)) {
               if (reqdig > 0) {
                  // LHS is sufficient
                  res.mant = usel;
                  res.exp = lhs.exp;
                  res.ind = lhs.ind;

                  if (usellen < reqdig) {
                     // need 0 padding
                     res.mant = padBy(lhs.mant, reqdig - lhs.mant.length);
                     res.exp = res.exp - ((reqdig - usellen));
                  }

                  return res.finish(context);
               }
            }

            // RHS may affect result
            res.exp = rhs.exp; // expected final exponent
            if (newlen > (reqdig + 1)) {
               if (reqdig > 0) {
                  // LHS will be max; RHS truncated
                  tlen = (newlen - reqdig) - 1; // truncation length
                  userlen = userlen - tlen;
                  res.exp = res.exp + tlen;
                  newlen = reqdig + 1;
               }
            }
            if (newlen > usellen) {
               usellen = newlen; // need to pad LHS
            }
         }
         else {
            // need to pad rhs and/or truncate lhs
            newlen = (userlen + rhs.exp) - lhs.exp;
            if (newlen >= ((usellen + reqdig) + 1)) {
               if (reqdig > 0) {
                  // RHS is sufficient
                  res.mant = user;
                  res.exp = rhs.exp;
                  res.ind = rhs.ind;
                  if (userlen < reqdig) {
                     // need 0 padding
                     res.mant = padBy(rhs.mant, reqdig - rhs.mant.length);
                     res.exp = res.exp - ((reqdig - userlen));
                  }
                  return res.finish(context);
               }
            }
            // LHS may affect result
            res.exp = lhs.exp; // expected final exponent
            if (newlen > (reqdig + 1)) {
               if (reqdig > 0) {
                  // RHS will be max; LHS truncated
                  tlen = (newlen - reqdig) - 1; // truncation length
                  usellen = usellen - tlen;
                  res.exp = res.exp + tlen;
                  newlen = reqdig + 1;
               }
            }
            if (newlen > userlen) {
               userlen = newlen; // need to pad RHS
            }
         }
         /*padder*/

         /* OK, we have aligned mantissas. Now add or subtract. */
         // 1998.06.27 Sign may now be 0 [e.g., 0.000] .. treat as positive
         // 1999.05.27 Allow for 00 on lhs [is not larger than 2 on rhs]
         // 1999.07.10 Allow for 00 on rhs [is not larger than 2 on rhs]
         if (lhs.ind == iszero) {
            res.ind = ispos;
         }
         else {
            res.ind = lhs.ind; // likely sign, all paths
         }
         if (( (lhs.ind == isneg) ? 1 : 0) == ((rhs.ind == isneg) ? 1 : 0)) {
            // same sign, 0 non-negative
            mult = 1;
         }
         else {
            /*signdiff*/
            // different signs, so subtraction is needed
            mult = -1; // will cause subtract
            /* Before we can subtract we must determine which is the larger,
               as our add/subtract routine only handles non-negative results
               so we may need to swap the operands. */
            /*swaptest*/
            if (rhs.ind == iszero) {
               // original A bigger
            }
            else if ((usellen < userlen) || (lhs.ind == iszero)) {
               // original B bigger
               t = usel;
               usel = user;
               user = t; // swap
               tlen = usellen;
               usellen = userlen;
               userlen = tlen; // ..
               res.ind = -res.ind; // and set sign
            }
            else if (usellen > userlen) {
               // original A bigger
            }
            else {
               /* logical lengths the same */ // need compare
               /* may still need to swap: compare the strings */
               ia = 0;
               ib = 0;
               ea = usel.length - 1;
               eb = user.length - 1;
               for (; ;) { /*compare*/
                  if (ia <= ea) {
                     ca = usel[ia];
                  }
                  else {
                     if (ib > eb) {
                        // [we must do the subtract, in case of 0.000 results]
                        break /*compare*/;
                     }
                     ca = 0;
                  }

                  if (ib <= eb) {
                     cb = user[ib];
                  }
                  else {
                     cb = 0;
                  }
                  if (ca != cb) {
                     if (ca < cb) {
                        /* swap needed */
                        t = usel;
                        usel = user;
                        user = t; // swap
                        tlen = usellen;
                        usellen = userlen;
                        userlen = tlen; // ..
                        res.ind = -res.ind;
                     }
                     break /*compare*/;
                  }
                  /* mantissas the same, so far */
                  ia++;
                  ib++;
               }
               /*compare*/
               // lengths the same
            }
         }
         /*swaptest*/
         /*signdiff*/

         /* here, A is > B if subtracting */
         // add [A+B*1] or subtract [A+(B*-1)]
         res.mant = byteaddsub(usel, usellen, user, userlen, mult, false);
         // [reuse possible only after chop; accounting makes not worthwhile]

         // Finish() rounds before stripping leading 0's, then sets form, etc.
         return res.finish(context);
      }

      /**
       * Compares this <code>BigDecimal</code> with <code>rhs</code>. Returns one of the
       * three values <code>1</code>, <code>0</code>, or <code>-1</code>. The method behaves as
       * if <code>this.subtract(rhs)</code> is computed. If this difference is > 0 then
       * 1 is returned, if the difference is < 0 then -1 is returned, and if the
       * difference is 0 then 0 is returned. This means, that if two decimal
       * instances are compared which are equal in value but differ in scale, then
       * these two instances are considered as equal.
       *
       * @param rhs value to be compared with <code>this</code>.
       * @return <code>1</code> if <code>this > rhs</code>, <code>-1</code> if <code>this < rhs</code>,
       *         <code>0</code> if <code>this == rhs</code>.
       */
      public function compareTo(rhs:BigDecimal, context:MathContext=null):int {
         var thislength:int = 0;
         var i:int = 0;
         var newrhs:BigDecimal;

         if (context == null) {
            context = MathContext.PLAIN;
         }

         // rhs=null will raise NullPointerException, as per Comparable interface
         if (context.lostDigits) {
            checkdigits(rhs, context.digits);
         }

         // [add will recheck in slowpath cases .. but would report -rhs]
         if (this.ind == rhs.ind && this.exp == rhs.exp) {
            /* sign & exponent the same [very common] */
            thislength = mant.length;
            if (thislength < rhs.mant.length) {
               return -ind;
            }
            if (thislength > rhs.mant.length) {
               return ind;
            }
            /* lengths are the same; we can do a straight mantissa compare
               unless maybe rounding [rounding is very unusual] */
            if (thislength <= context.digits || context.digits == 0) {
               var $6:int = thislength;
               i = 0;
               for (; $6 > 0; $6--, i++) {
                  if (mant[i] < rhs.mant[i]) {
                     return -ind;
                  }
                  if (mant[i] > rhs.mant[i]) {
                     return ind;
                  }
               }
               return 0; // identical
            }
            /* drop through for full comparison */
         }
         else {
            /* More fastpaths possible */
            if (this.ind < rhs.ind) {
               return -1;
            }
            if (this.ind > rhs.ind) {
               return 1;
            }
         }
         /* carry out a subtract to make the comparison */
         newrhs = clone(rhs); // safe copy
         newrhs.ind = -newrhs.ind; // prepare to subtract
         return add(newrhs, context).ind; // add, and return sign of result
      }

      /**
       * Returns a plain <code>BigDecimal</code> whose value is <code>this / rhs</code>,
       * using fixed point arithmetic and a rounding mode.
       *
       * <p>The length of the decimal part (the scale) of the result will
       *    therefore be the same as the scale of the current object, if the
       *    latter were formatted without exponential notation.</p>
       *
       * @param rhs The <code>BigDecimal</code> for the right hand side of the division.
       * @param round The <code>int</code> rounding mode to be used for the division
       *              (see <code>MathContext</code>).
       * @return A plain <code>BigDecimal</code> whose value is <code>this / rhs</code>,
       *         using fixed point arithmetic and the specified rounding mode.
       * @throws Error if <code>round</code> is not a valid rounding mode.
       * @throws ArithmeticError if <code>rhs</code> is zero.
       * @throws ArithmeticError if <code>round</code> is <code>MathContext.ROUND_UNNECESSARY</code>
       *                         and <code>this.scale()</code> is insufficient to represent the result exactly.
       */
      public function divideRound(rhs:BigDecimal, round:int):BigDecimal {
         var context:MathContext;
         context = new MathContext(0, false, round); // [checks round, too]
         return dodivide('D', rhs, context, -1); // take scale from LHS
      }

      /**
       * Returns a plain <code>BigDecimal</code> whose value is <code>this / rhs</code>,
       * using fixed point arithmetic and a given scale and rounding mode.
       *
       * <p>The same as <code>divide(BigDecimal, MathContext)</code>,
       *    where the <code>BigDecimal</code> is <code>rhs</code>,
       *    <code>new MathContext(0, MathContext.PLAIN, false, round)</code>,
       *    except that the length of the decimal part (the scale) to be used
       *    for the result is explicit rather than being taken from
       *    <code>this</code>.
       *
       * <p>The length of the decimal part (the scale) of the result will be
       *    the same as the scale of the current object, if the latter were
       *    formatted without exponential notation.<p>
       *
       * @param rhs The <code>BigDecimal</code> for the right hand side of the division.
       * @param scale The <code>int</code> scale to be used for the result.
       * @param round The <code>int</code> rounding mode to be used for the division
       *              (see <code>MathContext</code>).
       * @return A plain <code>BigDecimal</code> whose value is <code>this / rhs</code>,
       *         using fixed point arithmetic and the specified rounding mode.
       * @throws Error if <code>round</code> is not a valid rounding mode.
       * @throws ArithmeticError if <code>rhs</code> is zero.
       * @throws ArithmeticError if <code>scale</code> is negative.
       * @throws ArithmeticError if <code>round</code> is <code>MathContext.ROUND_UNNECESSARY</code>
       *                         and <code>scale</code> is insufficient to represent the result exactly.
       */
      public function divideScaleRound(rhs:BigDecimal, scale:int, round:int):BigDecimal {
         var context:MathContext;
         if (scale < 0) {
            throw new ArithmeticError("Negative scale: " + scale);
         }
         context = new MathContext(0, false, round); // [checks round]
         return dodivide('D', rhs, context, scale);
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is <code>this / rhs</code>.
       * The result is rounded according to the passed context <code>context</code>. If the
       * passed math context specifies precision <code>0</code>, then this call is
       * equivalent to <code>this.divide(rhs)</code>.
       *
       * @param rhs value by which <code>this</code> is divided.
       * @param context rounding mode and precision for the result of this operation.
       * @return <code>this / rhs</code>.
       */
      public function divide(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         if (context == null) {
            context = MathContext.PLAIN;
         }
         return dodivide('D', rhs, context, -1);
      }

      /**
       * Returns a <code>BigDecimal</code> whose value is the integer part of <code>this / rhs</code>.
       *
       * @param rhs The <code>BigDecimal</code> for the right hand side of the integer division.
       * @param context The <code>MathContext</code> arithmetic settings.
       * @return A <code>BigDecimal</code> whose value is the integer part of <code>this / rhs</code>.
       * @throws ArithmeticError if <code>rhs</code> is zero.
       * @throws ArithmeticError if the result will not fit in the number of digits specified for the context.
       */
      public function divideInteger(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         if (context == null) {
            context = MathContext.PLAIN;
         }
         // scale 0 to drop .000 when plain
         return dodivide('I', rhs, context, 0);
      }

      /**
       * Returns the maximum of this <code>BigDecimal</code> and <code>rhs</code>.
       *
       * @param rhs value to be used to compute the maximum with this.
       * @return <code>max(this, rhs)</code>.
       */
      public function max(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         var max:BigDecimal;

         if (context == null) {
            context = MathContext.PLAIN;
         }

         if (compareTo(rhs, context) >= 0) {
            max = this.plus(context);
         }
         else {
            max = rhs.plus(context);
         }

         return max;
      }

      /**
       * Returns the minimum of this <code>BigDecimal</code> and <code>rhs</code>.
       *
       * @param rhs value to be used to compute the minimum with this.
       * @return <code>min(this, rhs</code>.
       */
      public function min(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         var min:BigDecimal;

         if (context == null) {
            context = MathContext.PLAIN;
         }

         if (compareTo(rhs, context) <= 0) {
            min = this.plus(context);
         }
         else {
            min = rhs.plus(context);
         }

         return min;
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is <code>this * rhs</code>.
       * The result is rounded according to the passed context <code>context</code>.
       *
       * @param rhs value to be multiplied with <code>this</code>.
       * @param context rounding mode and precision for the result of this operation.
       * @return <code>this * rhs</code>.
       */
      public function multiply(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         var lhs:BigDecimal;
         var reqdig:int;
         var multer:Vector.<int> = null;
         var multand:Vector.<int> = null;
         var multandlen:int;
         var acclen:int = 0;
         var res:BigDecimal;
         var acc:Vector.<int>;
         var n:int;
         var mult:int = 0;

         if (context == null) {
            context = MathContext.PLAIN;
         }

         if (context.lostDigits) {
            checkdigits(rhs, context.digits);
         }

         lhs = this; // name for clarity and proxy

         /* Prepare numbers (truncate, unless unlimited precision) */
         reqdig = context.digits; // local copy

         if (reqdig > 0) {
            if (lhs.mant.length > reqdig) {
               lhs = clone(lhs).roundContext(context);
            }
            if (rhs.mant.length > reqdig) {
               rhs = clone(rhs).roundContext(context);
            }
            // [we could reuse the new LHS for result in this case]
         }

         // For best speed, as in DMSRCN, we use the shorter number as the
         // multiplier and the longer as the multiplicand.
         // 1999.12.22: We used to special case when the result would fit in
         // a long, but with Java 1.3 this gave no advantage.
         if (lhs.mant.length < rhs.mant.length) {
            multer = lhs.mant;
            multand = rhs.mant;
         }
         else {
            multer = rhs.mant;
            multand = lhs.mant;
         }

         /* Calculate how long result byte array will be */
         multandlen = (multer.length + multand.length) - 1; // effective length
         // optimize for 75% of the cases where a carry is expected...
         if ((multer[0] * multand[0]) > 9) {
            acclen = multandlen + 1;
         }
         else {
            acclen = multandlen;
         }

         /* Now the main long multiplication loop */
         res = new BigDecimal(); // where we'll build result
         acc = new Vector.<int>(acclen); // accumulator, all zeros
         // 1998.07.01: calculate from left to right so that accumulator goes
         // to likely final length on first addition; this avoids a one-digit
         // extension (and object allocation) each time around the loop.
         // Initial number therefore has virtual zeros added to right.
         var $7:int = multer.length;
         for (n = 0; $7 > 0; $7--, n++) {
            mult = multer[n];
            if (mult != 0) {
               // [optimization]
               // accumulate [accumulator is reusable array]
               acc = byteaddsub(acc, acc.length, multand, multandlen, mult, true);
            }
            // divide multiplicand by 10 for next digit to right
            multandlen--; // 'virtual length'
         }

         res.ind = (lhs.ind * rhs.ind); // final sign
         res.exp = (lhs.exp + rhs.exp); // final exponent
         res.mant = acc;
         return res.finish(context);
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is the <code>-this</code>. The
       * result is rounded according to the passed context <code>context</code>.
       *
       * @param context rounding mode and precision for the result of this operation.
       * @return <code>-this</code>
       */
      public function negate(context:MathContext=null):BigDecimal {
         if (context == null) {
            context = MathContext.PLAIN;
         }

         var res:BigDecimal;
         // Originally called minus(), changed to matched Java precedents
         // This simply clones, flips the sign, and possibly rounds
         if (context.lostDigits) {
            checkdigits(null as BigDecimal, context.digits);
         }
         res = clone(this); // safe copy
         res.ind = -res.ind;

         return res.finish(context);
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is <code>+this</code>.
       * The result is rounded according to the passed context <code>context</code>.
       *
       * @param context rounding mode and precision for the result of this operation.
       * @return <code>this</code> rounded.
       */
      public function plus(context:MathContext=null):BigDecimal {
         if (context == null) {
            context = MathContext.PLAIN;
         }

         // This clones and forces the result to the new settings
         // May return same object
         if (context.lostDigits) {
            checkdigits(null as BigDecimal, context.digits);
         }
         // Optimization: returns same object for some common cases
         if (mant.length <= context.digits) {
            return this;
         }
         if (context.digits == 0) {
            return this;
         }

         return clone(this).finish(context);
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is <code>this ^ n</code>.
       * The scale of the result is <code>n</code> times the scales of <code>this</code>.
       *
       * <p><code>x.pow(0)</code> returns <code>1</code>, even if <code>x == 0</code>.<p>
       *
       * Implementation Note: The implementation is based on the ANSI standard X3.274-1996 algorithm.
       *
       * @param n exponent to which <code>this</code> is raised.
       * @return <code>this ^ n</code>.
       * @throws ArithmeticError if <code>n < 0</code> or <code>n > 999999999</code>.
       */
      public function pow(n:int, context:MathContext=null):BigDecimal {
         if (n == 0) {
            return ONE; // x**0 == 1
         }

         if (n < 0 || n > 999999999) {
            throw new ArithmeticError("Invalid Operation");
         }

         if (context == null) {
            context = MathContext.PLAIN;
         }

         if (context.lostDigits) {
            checkdigits(null, context.digits);
         }

         var lhs:BigDecimal = this; // clarified name
         var workdigits:int = 0;
         const reqdig:int = context.digits; // local copy (heavily used)
         if (reqdig == 0) {
            workdigits = 0;
         }
         else {
            /* Round the lhs to DIGITS if need be */
            if (lhs.mant.length > reqdig) {
               lhs = clone(lhs).roundContext(context);
            }

            /* L for precision calculation [see ANSI X3.274-1996] */
            var L:int = String(n).length; // length without decimal zeros/exp

            /* non-0 digits */
            if (L > reqdig) {
               throw Error("Too many digits: " + n);
            }

            workdigits = (reqdig + L) + 1; // calculate the working DIGITS
         }

         /* Create a copy of context for working settings */
         // Note: no need to check for lostDigits again.
         // 1999.07.17 Note: this construction must follow RHS check
         const workset:MathContext = new MathContext(workdigits, false, context.roundingMode);

         var seenbit:Boolean = false; // set once we've seen a 1-bit
         var res:BigDecimal = ONE; // accumulator
         for (var i:int = 1; ; i++) {
            // for each bit [top bit ignored]
            n = n + n; // shift left 1 bit
            if (n < 0) {
               // top bit is set
               seenbit = true; // OK, we're off
               res = res.multiply(lhs, workset); // acc=acc*x
            }
            if (i == 31) {
               break; // that was the last bit
            }
            if (!seenbit) {
               continue; // we don't have to square 1
            }
            res = res.multiply(res, workset); // acc=acc*acc [square]
         }
         // 32 bits

         return res.finish(context); // round [original digits]
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is <code>this % rhs</code>.
       *
       * <p>The remainder is defined as <code>this - this.divideToIntegralValue(rhs) * rhs</code>.
       *
       * @param rhs value by which <code>this</code> is divided.
       * @return <code>this % rhs</code>.
       */
      public function remainder(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         if (context == null) {
            context = MathContext.PLAIN;
         }
         return dodivide('R', rhs, context, -1);
      }

      /**
       * Returns a new <code>BigDecimal</code> whose value is <code>this - rhs</code>.
       * The result is rounded according to the passed context <code>context</code>.
       *
       * @param rhs value to be subtracted from <code>this</code>.
       * @param context rounding mode and precision for the result of this operation.
       * @return <code>this - rhs</code>.
       */
      public function subtract(rhs:BigDecimal, context:MathContext=null):BigDecimal {
         var newrhs:BigDecimal;

         if (context == null) {
            context = MathContext.PLAIN;
         }

         if (context.lostDigits) {
            checkdigits(rhs, context.digits);
         }
         // [add will recheck .. but would report -rhs]
         /* carry out the subtraction */
         // we could fastpath -0, but it is too rare.
         newrhs = clone(rhs); // safe copy
         newrhs.ind = -newrhs.ind; // prepare to subtract

         return add(newrhs, context); // arithmetic
      }

      /**
       * Converts this <code>BigDecimal</code> to a <code>Number</code>
       * (with possible loose of precision).
       *
       * @return A <code>Number</code> corresponding to <code>this</code>.
       */
      public function toNumber():Number {
         // We go via a String [as does BigDecimal in JDK 1.2]
         // Next line could possibly raise NumberFormatException
         return Number(toString());
      }

      /**
       * Returns <code>true</code> if <code>obj</code> is a <code>BigDecimal</code> instance and if
       * this instance is equal to this big decimal. Two big decimals are equal if
       * their unscaled value and their scale is equal. For example, 1.0
       * (10*10^(-1)) is not equal to 1.00 (100*10^(-2)). Similarly, zero
       * instances are not equal if their scale differs.
       *
       * @param obj object to be compared with <code>this</code>.
       * @return true if <code>obj</code> is a <code>BigDecimal</code> and <code>this == obj</code>.
       */
      public function equals(obj:Object):Boolean {
         // We are equal iff toString of both are exactly the same
         if (obj == null) {
            return false; // not equal
         }

         if (!(obj is BigDecimal)) {
            return false; // not a decimal
         }

         const rhs:BigDecimal = obj as BigDecimal; // cast; we know it will work
         if (this.ind != rhs.ind) {
            return false; // different signs never match
         }

         if (this.mant.length == rhs.mant.length && this.exp == rhs.exp) {
            // mantissas say all
            // here with equal-length byte arrays to compare
            const len:int = mant.length;
            for (var i:int = 0; i < len; i++) {
               if (this.mant[i] != rhs.mant[i]) {
                  return false;
               }
            }
            return true; // arrays have identical content
         }
         return false;
      }

      /**
       * Converts this <code>BigDecimal</code> to an <code>int</code>.
       * If the <code>BigDecimal</code> has a non-zero decimal part it is
       * discarded. If the <code>BigDecimal</code> is out of the possible
       * range for an <code>int</code> (32-bit signed integer) result then
       * only the low-order 32 bits are used. (That is, the number may be
       * <i>decapitated</i>.) To avoid unexpected errors when these
       * conditions occur, use the <code>intValueExact</code> method.
       *
       * @return An <code>int</code> converted from <code>this</code>,
       *         truncated and decapitated if necessary.
       */
      // TODO
      public function toInt():int {
         throw new Error("Not implemented");
      }

      /**
       * Returns this <code>BigDecimal</code> as a int value if it has no fractional
       * part and if its value fits to the int range ([-2^{31}..2^{31}-1]). If
       * these conditions are not met, an <code>ArithmeticException</code> is thrown.
       *
       * @return this <code>BigDecimal</code> as a int value.
       * @throws Error if rounding is necessary or the number doesn't fit in a int.
       */
      public function intValueExact():int {
         var lodigit:int;
         var useexp:int = 0;
         var result:int;
         var i:int;
         var topdig:int = 0;

         // This does not use longValueExact() as the latter can be much
         // slower.
         // intcheck (from pow) relies on this to check decimal part
         if (ind == iszero) {
            return 0; // easy, and quite common
         }
         /* test and drop any trailing decimal part */

         lodigit = mant.length - 1;
         if (exp < 0) {
            lodigit = lodigit + exp; // reduces by -(-exp)
            /* all decimal places must be 0 */

            if (!allzero(mant, lodigit + 1))
            {
               throw new Error("Decimal part non-zero:" + " " + toString());
            }
            if (lodigit < 0)
            {
               return 0; // -1<this<1
            }
            useexp = 0;
         }
         else {
            /* >=0 */

            if ((exp + lodigit) > 9) {
               // early exit
               throw new Error("Conversion overflow:" + " " + toString());
            }
            useexp = exp;
         }
         /* convert the mantissa to binary, inline for speed */

         result = 0;
         const $16:int = lodigit + useexp;
         i = 0;
         for (; i <= $16; i++) {
            result = result * 10;
            if (i <= lodigit) {
               result = result + mant[i];
            }
         }

         /* Now, if the risky length, check for overflow */

         if ((lodigit + useexp) == 9) {
            // note we cannot just test for -ve result, as overflow can move a
            // zero into the top bit [consider 5555555555]
            topdig = int(result / 1000000000); // get top digit, preserving sign
            if (topdig != mant[0]) {
               // digit must match and be positive
               // except in the special case ...
               if (result == int.MIN_VALUE) {
                  // looks like the special
                  if (ind == isneg) {
                     // really was negative
                     if (mant[0] == 2) {
                        return result; // really had top digit 2
                     }
                  }
               }
               throw new Error("Conversion overflow:" + " " + toString());
            }
         }

         /* Looks good */

         if (ind == ispos) {
            return result;
         }

         return -result;
      }

      /**
       * Returns a new <code>BigDecimal</code> instance where the decimal point has
       * been moved <code>n</code> places to the left. If <code>n < 0</code> then the
       * decimal point is moved <code>-n</code> places to the right.
       *
       * <p>The result is obtained by changing its scale. If the scale of the result
       *    becomes negative, then its precision is increased such that the scale is zero.
       *
       * <p>Note, that <code>movePointLeft(0)</code> returns a result which is
       *    mathematically equivalent, but which has <code>scale >= 0</code>.
       *
       * @param n number of placed the decimal point has to be moved.
       * @return <code>this * 10^(-n</code>).
       */
      public function movePointLeft(n:int):BigDecimal {
         return movePoint(-n);
      }

      /**
       * Returns a new <code>BigDecimal</code> instance where the decimal point has
       * been moved <code>n</code> places to the right. If <code>n < 0</code> then the
       * decimal point is moved <code>-n</code> places to the left.
       *
       * <p>The result is obtained by changing its scale. If the scale of the result
       *    becomes negative, then its precision is increased such that the scale is zero.
       *
       * <p>Note, that <code>movePointRight(0)</code> returns a result which is
       *    mathematically equivalent, but which has scale >= 0.</p>
       *
       * @param n number of placed the decimal point has to be moved.
       * @return <code>this * 10^n</code>.
       */
      public function movePointRight(n:int):BigDecimal {
         return movePoint(n);
      }

      private function movePoint(n:int):BigDecimal {
         var res:BigDecimal = clone(this);
         res.exp = toIntExponent(exp + n);
         res = res.finish(MathContext.PLAIN);
         return res.exp < 0 ? res : res.setScale(0, MathContext.ROUND_UNNECESSARY);
      }

      /**
       * Returns the scale of this <code>BigDecimal</code>.
       *
       * The scale is the number of digits behind the decimal point.
       * The value of this <code>BigDecimal</code> is the unsignedValue * 10^(-scale).
       * If the scale is negative, then this <code>BigDecimal</code> represents a big integer.
       *
       * @return the scale of this <code>BigDecimal</code>.
       */
      public function scale():int {
         return -exp;
      }

      /**
       * Returns a new <code>BigDecimal</code> instance with the specified scale.
       * <p>
       * If the new scale is greater than the old scale, then additional zeros are
       * added to the unscaled value. In this case no rounding is necessary.
       * <p>
       * If the new scale is smaller than the old scale, then trailing digits are
       * removed. If these trailing digits are not zero, then the remaining
       * unscaled value has to be rounded. For this rounding operation the
       * specified rounding mode is used.
       *
       * @param scale scale of the result returned.
       * @param round rounding mode to be used to round the result.
       * @return a new <code>BigDecimal</code> instance with the specified scale.
       */
      public function setScale(scale:int, round:int=-1):BigDecimal {
         //ActionScript 3
         //Correct the default parameter patch because of 
         //Compiler bug for the compile time constants
         if (round == -1) {
            round = MathContext.ROUND_UNNECESSARY;
         }

         // at present this naughtily only checks the round value if it is
         // needed (used), for speed
         const ourscale:int = this.scale();
         if (ourscale == scale) {
            // already correct scale
            return this;
         }

         var res:BigDecimal = clone(this); // need copy
         if (ourscale <= scale) {
            // simply zero-padding/changing form
            if (ind != iszero) { //no need to pad; adding zeroes just inflates the mantissa
               // if ourscale is 0 we may have lots of 0s to add
               var padding:int = 0;
               if (ourscale == 0) {
                  padding = res.exp + scale;
               }
               else {
                  padding = scale - ourscale;
               }
               res.mant = padBy(res.mant, padding);
               res.exp = -scale; // as requested
            }
         }
         else {
            /* ourscale>scale: shortening, probably */
            if (scale < 0) {
               throw new Error("Negative scale:" + " " + scale);
            }
            // [round() will raise exception if invalid round]
            const newlen:int = res.mant.length - ((ourscale - scale)); // [<=0 is OK]
            res = res.round(newlen, round); // round to required length
            // This could have shifted left if round (say) 0.9->1[.0]
            // Repair if so by adding a zero and reducing exponent
            if (res.exp != (-scale)) {
               res.mant = padBy(res.mant, 1);
               res.exp = res.exp - 1;
            }
         }
         return res;
      }

      /**
       * Returns the sign of this <code>BigDecimal</code>.
       *
       * @return <code>-1</code> if <code>this < 0</code>,
       *         <code>0</code> if <code>this == 0</code>,
       *         <code>1</code> if <code>this > 0</code>.
       */
      public function get sign():int {
         return ind; // [note this assumes values for ind.]
      }

      /**
       * Returns a canonical string representation of this <code>BigDecimal</code>. If
       * necessary, scientific notation is used. This representation always prints
       * all significant digits of this value.
       *
       * <p>If the scale is negative or if <code>scale - precision >= 6</code> then
       *    scientific notation is used.</p>
       *
       * @return a string representation of <code>this</code> in scientific notation if necessary.
       */
      public function toCanonicalString():String {
         const buf:Vector.<String> = Vector.<String>(mant);
         if (ind < 0) {
            buf.splice(0, 0, "-");
         }
         if (scale() == 0) {
            return buf.join("");
         }

         const begin:int = (ind < 0) ? 2 : 1;
         var end:int = buf.length;
         const exponent:int = -scale() + end - begin;

         if (scale() > 0 && exponent >= -6) {
            if (exponent >= 0) {
               buf.splice(end - scale(), 0, '.');
            }
            else {
               buf.splice(begin - 1, 0, "0", ".");
               for (var i:int = 0; i < -exponent - 1; i++) {
                  buf.splice(begin + 1, 0, "0");
               }
            }
         }
         else {
            if (end - begin >= 1) {
               buf.splice(begin, 0, '.');
            }
            buf[buf.length] = 'E';
            if (exponent > 0) {
               buf[buf.length] = '+';
            }
            buf[buf.length] = String(exponent);
         }
         return buf.join("");
      }

      /**
       * Returns a string representation of this <code>BigDecimal</code>. This
       * representation always prints all significant digits of this value.
       *
       * <p>If the scale is negative or if <code>scale - precision >= 6</code> then
       *    engineering notation is used. Engineering notation is similar to the
       *    scientific notation except that the exponent is made to be a multiple of
       *    3 such that the integer part is >= 1 and < 1000.</p>
       *
       * @return a string representation of <code>this</code> in engineering notation if necessary.
       */
      public function toEngineeringString():String {
         const buf:Vector.<String> = Vector.<String>(mant);

         if (ind < 0) {
            buf.splice(0, 0, "-");
         }
         if (exp == 0) {
            return buf.join("");
         }

         var begin:int = (ind < 0) ? 2 : 1;
         var end:int = buf.length;
         var exponent:int = exp + end - begin;

         if (-exp > 0 && exponent >= -6) {
            if (exponent >= 0) {
               buf.splice(end - scale(), 0, '.');
            }
            else {
               buf.splice(begin - 1, 0, "0", "."); //$NON-NLS-1$
               for (var i:int = 0; i < -exponent - 1; i++) {
                  buf.splice(begin + 1, 0, "0");
               }
            }
         }
         else {
            var delta:int = end - begin;
            var rem:int = int(exponent % 3);

            if (rem != 0) {
               // adjust exponent so it is a multiple of three
               if (ind == 0) {
                  // zero value
                  rem = (rem < 0) ? -rem : 3 - rem;
                  exponent += rem;
               }
               else {
                  // nonzero value
                  rem = (rem < 0) ? rem + 3 : rem;
                  exponent -= rem;
                  begin += rem;
               }
               if (delta < 3) {
                  for (i = rem - delta; i > 0; i--)
                  {
                     buf.splice(end++, 0, '0');
                  }
               }
            }
            if (end - begin >= 1) {
               buf.splice(begin, 0, '.');
            }
            if (exponent != 0) {
               buf[buf.length] = 'E';
               if (exponent > 0) {
                  buf[buf.length] = '+';
               }
               buf[buf.length] = String(exponent);
            }
         }
         return buf.join("");
      }

      public function toPlainString():String {
         return toString();
      }

      /**
       * Returns a string representation of this <code>BigDecimal</code>. No scientific
       * notation is used. This methods adds zeros where necessary.
       *
       * <p>If this string representation is used to create a new instance,
       *    this instance is generally not identical to <code>this</code> as the precision changes.</p>
       * <p><code>x.equals(new BigDecimal(x.toPlainString())</code> usually returns <code>false</code>.</p>
       * <p><code>x.compareTo(new BigDecimal(x.toPlainString())</code> returns <code>0</code>.</p>
       *
       * @return a string representation of <code>this</code> without exponent part.
       */
      public function toString():String {
         if (string != null) {
            return string;
         }

         const len:int = mant.length;
         const buf:Vector.<String> = new Vector.<String>();

         if (ind < 0) {
            buf[buf.length] = "-";
         }
         var dot:int = -1;
         if (exp < 0) {
            if (-exp >= len) {
               buf[buf.length] = "0.";
               var pos:int = buf.length;
               var pad:int = -exp - len;
               buf.length += pad;
               for ( ; pad-- > 0; pos++) {
                  buf[pos] = "0";
               }
            }
            else {
               dot = len + exp;
            }
         }
         for (var i:int = 0; i < len; i++) {
            if (i == dot) {
               buf[buf.length] = ".";
            }
            buf[buf.length] = String(mant[i]);
         }
         if (exp > 0 && ind != iszero) {
            pos = buf.length;
            pad = exp;
            buf.length += pad;
            for ( ; pad-- > 0; pos++)
            {
               buf[pos] = "0";
            }
         }
         string = buf.join("");
         return string;
      }

      /* <sgml> Carry out division operations. </sgml> */
      /*
         Arg1 is operation code: D=divide, I=integer divide, R=remainder
         Arg2 is the rhs.
         Arg3 is the context.
         Arg4 is explicit scale iff code='D' or 'I' (-1 if none).

         Underlying algorithm (complications for Remainder function and
         scaled division are omitted for clarity):

         Test for x/0 and then 0/x
         Exp =Exp1 - Exp2
         Exp =Exp +len(var1) -len(var2)
         Sign=Sign1 * Sign2
         Pad accumulator (Var1) to double-length with 0's (pad1)
         Pad Var2 to same length as Var1
         B2B=1st two digits of var2, +1 to allow for roundup
         have=0
         Do until (have=digits+1 OR residue=0)
         if exp<0 then if integer divide/residue then leave
         this_digit=0
         Do forever
         compare numbers
         if <0 then leave inner_loop
         if =0 then (- quick exit without subtract -) do
         this_digit=this_digit+1; output this_digit
         leave outer_loop; end
         Compare lengths of numbers (mantissae):
         If same then CA=first_digit_of_Var1
         else CA=first_two_digits_of_Var1
         mult=ca*10/b2b -- Good and safe guess at divisor
         if mult=0 then mult=1
         this_digit=this_digit+mult
         subtract
         end inner_loop
         if have\=0 | this_digit\=0 then do
         output this_digit
         have=have+1; end
         var2=var2/10
         exp=exp-1
         end outer_loop
         exp=exp+1 -- set the proper exponent
         if have=0 then generate answer=0
         Return to FINISHED
         Result defined by MATHV1

         For extended commentary, see DMSRCN.
       */
      private function dodivide(code:String, rhs:BigDecimal, context:MathContext, scale:int):BigDecimal {
         var lhs:BigDecimal;
         var reqdig:int;
         var newexp:int;
         var newlen:int;
         var var1len:int;
         var var2:Vector.<int>;
         var var2len:int;
         var b2b:int;
         var have:int;
         var thisdigit:int;
         var i:int = 0;
         var v2:int = 0;
         var ba:int = 0;
         var mult:int;
         var start:int;
         var padding:int = 0;
         var d:int = 0;
         var lasthave:int = 0;
         var actdig:int = 0;

         if (context.lostDigits) {
            checkdigits(rhs, context.digits);
         }

         lhs = this; // name for clarity

         // [note we must have checked lostDigits before the following checks]
         if (rhs.ind == 0) {
            throw new ArithmeticError("Division by zero"); // includes 0/0
         }

         if (lhs.ind == 0) {
            if (scale == -1) {
               return lhs;
            }
            return lhs.setScale(scale);
         }

         /* Prepare numbers according to BigDecimal rules */
         reqdig = context.digits; // local copy (heavily used)

         if (reqdig > 0) {
            if (lhs.mant.length > reqdig) {
               lhs = clone(lhs).roundContext(context);
            }
            if (rhs.mant.length > reqdig) {
               rhs = clone(rhs).roundContext(context);
            }
         }
         else {
            /* scaled divide */
            if (scale == (-1)) {
               scale = lhs.scale();
            }
            // set reqdig to be at least large enough for the computation
            reqdig = lhs.mant.length; // base length
            // next line handles both positive lhs.exp and also scale mismatch
            if (scale != (-lhs.exp)) {
               reqdig = (reqdig + scale) + lhs.exp;
            }
            reqdig = (reqdig - ((rhs.mant.length - 1))) - rhs.exp; // reduce by RHS effect
            if (reqdig < lhs.mant.length) {
               reqdig = lhs.mant.length; // clamp
            }
            if (reqdig < rhs.mant.length) {
               reqdig = rhs.mant.length; // ..
            }
         }

         /* precalculate exponent */
         newexp = ((lhs.exp - rhs.exp) + lhs.mant.length) - rhs.mant.length;
         /* If new exponent -ve, then some quick exits are possible */
         if (newexp < 0) {
            if (code != "D") {
               if (code == "I") {
                  return ZERO; // easy - no integer part
               }
               /* Must be 'R'; remainder is [finished clone of] input value */
               return clone(lhs).finish(context);
            }
         }

         /* We need slow division */
         const res:BigDecimal = new BigDecimal(); // where we'll build result
         res.ind = (lhs.ind * rhs.ind); // final sign (for D/I)
         res.exp = newexp; // initial exponent (for D/I)
         res.mant = new Vector.<int>(reqdig + 1); // where build the result

         /* Now [virtually pad the mantissae with trailing zeros */
         // Also copy the LHS, which will be our working array
         newlen = (reqdig + reqdig) + 1;

         var var1:Vector.<int> = padBy(lhs.mant, newlen - lhs.mant.length);
         //        var var1:Vector.<int> = lhs.mant.slice();
         //        var1.length = newlen; // always makes longer, so new safe array
         var1len = newlen; // [remaining digits are 0]

         var2 = rhs.mant;
         var2len = newlen;

         /* Calculate first two digits of rhs (var2), +1 for later estimations */
         b2b = (var2[0] * 10) + 1;
         if (var2.length > 1) {
            b2b = b2b + var2[1];
         }

         /* start the long-division loops */
         have = 0;

outer:   for (; ;)
         {
            thisdigit = 0;
            /* find the next digit */
inner:      for (; ;) /*inner*/
            {
               if (var1len < var2len) {
                  break /*inner*/; // V1 too low
               }
               if (var1len == var2len) {
                  // compare needed
compare:          {
                     // comparison
                     var $22:int = var1len;
                     i = 0;
                     for (; $22 > 0; $22--, i++) {
                        // var1len is always <= var1.length
                        if (i < var2.length) {
                           v2 = var2[i];
                        }
                        else {
                           v2 = 0;
                        }
                        if (var1[i] < v2) {
                           break inner; // V1 too low
                        }
                        if (var1[i] > v2) {
                           break compare; // OK to subtract
                        }
                     }

                     /* reach here if lhs and rhs are identical; subtraction will
                        increase digit by one, and the residue will be 0 so we
                        are done; leave the loop with residue set to 0 (in case
                        code is 'R' or ROUND_UNNECESSARY or a ROUND_HALF_xxxx is
                        being checked) */
                     thisdigit++;
                     res.mant[have] = thisdigit;
                     have++;
                     var1[0] = 0; // residue to 0 [this is all we'll test]
                     // var1len=1 -- [optimized out]
                     break outer;
                  }
                  // end compare

                  /* prepare for subtraction. Estimate BA (lengths the same) */
                  ba = var1[0]; // use only first digit
               } /* lengths the same */
               else {
                  /* lhs longer than rhs */
                  /* use first two digits for estimate */
                  ba = var1[0] * 10;
                  if (var1len > 1) {
                     ba = ba + var1[1];
                  }
               }

               /* subtraction needed; V1>=V2 */
               mult = int(ba * 10) / b2b;
               if (mult == 0) {
                  mult = 1;
               }
               thisdigit = thisdigit + mult;
               // subtract; var1 reusable
               var1 = byteaddsub(var1, var1len, var2, var2len, -mult, true);

               if (var1[0] != 0) {
                  continue /*inner*/; // maybe another subtract needed
               }
               /* V1 now probably has leading zeros, remove leading 0's and try
                  again. (It could be longer than V2) */
               const $23:int = var1len - 2;
               start = 0;
               for (; start <= $23; start++) { /* start */
                  if (var1[start] != 0) {
                     break;
                     /* start */
                  }
                  var1len--;
               }
               /*start*/

               if (start == 0) {
                  continue /*inner*/;
               }
               // shift left
               for (i = 0; i < var1len; i++) {
                  var1[i] = var1[start + i];
               }
            }
            // end inner

            /* We have the next digit */
            if (have != 0 || thisdigit != 0) {
               // put the digit we got
               res.mant[have] = thisdigit;
               have++;
               if (have == (reqdig + 1)) {
                  break /*outer*/; // we have all we need
               }
               if (var1[0] == 0) {
                  break /*outer*/; // residue now 0
               }
            }
            /* can leave now if a scaled divide and exponent is small enough */
            if (scale >= 0) {
               if ((-res.exp) > scale) {
                  break /*outer*/;
               }
            }

            /* can leave now if not Divide and no integer part left */
            if (code != "D") {
               if (res.exp <= 0) {
                  break /*outer*/;
               }
            }
            res.exp = res.exp - 1; // reduce the exponent
            /* to get here, V1 is less than V2, so divide V2 by 10 and go for
               the next digit */
            var2len--;
         }
         // end outer

         /* here when we have finished dividing, for some reason */
         // have is the number of digits we collected in res.mant
         if (have == 0) {
            have = 1; // res.mant[0] is 0; we always want a digit
         }

         if (code == "I" || code == "R") {
            /* check for integer overflow needed */
            if ((have + res.exp) > reqdig) {
               throw new Error("Integer overflow");
            }

            if (code == "R") { /*remainder*/
               /* We were doing Remainder -- return the residue */
               if (res.mant[0] == 0) {
                  // no integer part was found
                  return clone(lhs).finish(context); // .. so return lhs, canonical
               }
               if (var1[0] == 0) {
                  return ZERO; // simple 0 residue
               }
               res.ind = lhs.ind; // sign is always as LHS
               /* Calculate the exponent by subtracting the number of padding zeros
                  we added and adding the original exponent */
               padding = ((reqdig + reqdig) + 1) - lhs.mant.length;
               res.exp = (res.exp - padding) + lhs.exp;

               /* strip insignificant padding zeros from residue, and create/copy
                  the resulting mantissa if need be */
               d = var1len;
               i = d - 1;
               for (; i >= 1; i--) {
                  if (!((res.exp < lhs.exp) && (res.exp < rhs.exp))) {
                     break;
                  }
                  if (var1[i] != 0) {
                     break;
                  }
                  d--;
                  res.exp = res.exp + 1;
               }

               if (d < var1.length) {
                  /* need to reduce */
                  var1.length = d;
               }
               res.mant = var1;
               return res.finish(context);
            }
            /*remainder*/
         }
         else {
            /* 'D' -- no overflow check needed */
            // If there was a residue then bump the final digit (iff 0 or 5)
            // so that the residue is visible for ROUND_UP, ROUND_HALF_xxx and
            // ROUND_UNNECESSARY checks (etc.) later.
            // [if we finished early, the residue will be 0]
            if (var1[0] != 0) {
               // residue not 0
               lasthave = res.mant[have - 1];
               if ((lasthave % 5) == 0) {
                  res.mant[have - 1] = (lasthave + 1);
               }
            }
         }

         /* Here for Divide or Integer Divide */
         // handle scaled results first ['I' always scale 0, optional for 'D']
         if (scale >= 0) { /*scaled*/
            // say 'scale have res.exp len' scale have res.exp res.mant.length
            if (have != res.mant.length) {
               // already padded with 0's, so just adjust exponent
               res.exp = res.exp - (res.mant.length - have);
            }
            // calculate number of digits we really want [may be 0]
            actdig = res.mant.length - (-res.exp - scale);
            res.round(actdig, context.roundingMode); // round to desired length
            // This could have shifted left if round (say) 0.9->1[.0]
            // Repair if so by adding a zero and reducing exponent
            if (res.exp != -scale) {
               res.mant[res.mant.length] = 0; // extend by 1
               res.exp = res.exp - 1;
            }
            return res.finish(context);
         }
         /*scaled*/

         // reach here only if a non-scaled
         if (have == res.mant.length) {
            // got digits+1 digits
            res.roundContext(context);
         }
         else {
            /* have<=reqdig */
            if (res.mant[0] == 0) {
               return ZERO; // fastpath
            }
            // make the mantissa truly just 'have' long
            // [we could let finish do this, during strip, if we adjusted
            // the exponent; however, truncation avoids the strip loop]
            res.mant.length = have;
         }

         return res.finish(context);
      }

      /**
       * Returns the given exponent as an int, throwing an error if too large or small
       * @param exponent the value to convert to int
       * @return exponent as int
       */
      private static function toIntExponent(exponent:Number):int {
         if (exponent < int.MIN_VALUE) {
            throw new ArithmeticError("Underflow");
         }
         else if (exponent > int.MAX_VALUE) {
            throw new ArithmeticError("Overflow");
         }
         return exponent;
      }

      /* <sgml> Add or subtract two >=0 integers in byte arrays
         <p>This routine performs the calculation:
         <pre>
         C=A+(B*M)
         </pre>
         Where M is in the range -9 through +9
         <p>
         If M<0 then A>=B must be true, so the result is always
         non-negative.

         Leading zeros are not removed after a subtraction. The result is
         either the same length as the longer of A and B, or 1 longer than
         that (if a carry occurred).

         A is not altered unless Arg6 is 1.
         B is never altered.

         Arg1 is A
         Arg2 is A length to use (if longer than A, pad with 0's)
         Arg3 is B
         Arg4 is B length to use (if longer than B, pad with 0's)
         Arg5 is M, the multiplier
         Arg6 is 1 if A can be used to build the result (if it fits)

         This routine is severely performance-critical; *any* change here
         must be measured (timed) to assure no performance degradation.
       */
      // 1996.02.20 -- enhanced version of DMSRCN algorithm (1981)
      // 1997.10.05 -- changed to byte arrays (from char arrays)
      // 1998.07.01 -- changed to allow destructive reuse of LHS
      // 1998.07.01 -- changed to allow virtual lengths for the arrays
      // 1998.12.29 -- use lookaside for digit/carry calculation
      // 1999.08.07 -- avoid multiply when mult=1, and make db an int
      // 1999.12.22 -- special case m=-1, also drop 0 special case

      private static function byteaddsub(a:Vector.<int>, avlen:int, b:Vector.<int>, bvlen:int, m:int, reuse:Boolean):Vector.<int> {
         // We'll usually be right if we assume no carry
         const alength:int = a.length; // physical lengths
         const blength:int = b.length; // ..
         var ap:int = avlen - 1; // -> final (rightmost) digit
         var bp:int = bvlen - 1; // ..
         var maxarr:int = bp;

         if (maxarr < ap) {
            maxarr = ap;
         }

         var reb:Vector.<int>; // result byte array
         if (reuse) {
            if ((maxarr + 1) == alength) {
               reb = a; // OK to reuse A
            }
         }
         if (reb == null) {
            reb = new Vector.<int>(maxarr + 1); // need new array
         }

         var quickm:Boolean = false; // 1 if no multiply needed
         if (m == 1) {
            quickm = true; // most common
         }
         else if (m == (-1)) {
            quickm = true; // also common
         }

         var digit:int = 0; // digit, with carry or borrow
         var op:int = maxarr;
         for (; op >= 0; op--) { /*op*/
            if (ap >= 0) {
               if (ap < alength) {
                  digit = digit + a[ap]; // within A
               }
               ap--;
            }
            if (bp >= 0) {
               if (bp < blength) {
                  // within B
                  if (quickm) {
                     if (m > 0) {
                        digit = digit + b[bp]; // most common
                     }
                     else {
                        digit = digit - b[bp]; // also common
                     }
                  }
                  else {
                     digit = digit + (b[bp] * m);
                  }
               }
               bp--;
            }

            /* result so far (digit) could be -90 through 99 */
            if (digit < 10) {
               if (digit >= 0) { /*quick*/
                  // 0-9
                  reb[op] = digit;
                  digit = 0; // no carry
                  continue /*op*/;
               }
               /*quick*/
            }

            var dp90:int = digit + 90;
            reb[op] = bytedig[dp90]; // this digit
            digit = bytecar[dp90]; // carry or borrow
         }
         /*op*/

         if (digit == 0) {
            return reb; // no carry
         }
         // following line will become an Assert, later
         // if digit<0 then signal ArithmeticException("internal.error ["digit"]")

         /* We have carry -- need to make space for the extra digit */
         reb.splice(0, 0, digit);
         return reb;
      }

      /* <sgml> Initializer for digit array properties (lookaside). </sgml>
         Returns the digit array, and initializes the carry array. */

      private static function diginit():Vector.<int> {
         const work:Vector.<int> = new Vector.<int>((90 + 99) + 1, true);
         var op:int = 0;
         for (; op <= (90 + 99); op++) { /*op*/
            var digit:int = op - 90;
            if (digit >= 0) {
               work[op] = (digit % 10);
               bytecar[op] = int(digit / 10); // calculate carry
               continue /*op*/;
            }

            // borrowing...
            digit = digit + 100; // yes, this is right [consider -50]
            work[op] = (digit % 10);
            bytecar[op] = (int(digit / 10) - 10); // calculate borrow [NB: - after %]
         }
         /*op*/
         return work;
      }

      /* <sgml> Create a copy of BigDecimal object for local use.
         <p>This does NOT make a copy of the mantissa array.
         </sgml>
         Arg1 is the BigDecimal to clone (non-null)
       */

      private static function clone(dec:BigDecimal):BigDecimal {
         const copy:BigDecimal = new BigDecimal(NULL);
         copy.ind = dec.ind;
         copy.exp = dec.exp;
         copy.mant = dec.mant;
         return copy;
      }

      /* <sgml> Check one or two numbers for lost digits. </sgml>
         Arg1 is RHS (or null, if none)
         Arg2 is current DIGITS setting
         returns quietly or throws an exception */

      private function checkdigits(rhs:BigDecimal, dig:int):void {
         if (dig == 0) {
            return; // don't check if digits=0
         }
         // first check lhs...
         if (mant.length > dig) {
            if (!allzero(mant, dig)) {
               throw new Error("Too many digits: " + toString());
            }
         }
         if (rhs == null) {
            return; // monadic
         }
         if (rhs.mant.length > dig) {
            if (!allzero(rhs.mant, dig)) {
               throw new Error("Too many digits: " + rhs.toString());
            }
         }
      }

      /* <sgml> Round to specified digits, if necessary. </sgml>
         Arg1 is requested MathContext [with length and rounding mode]
         returns this, for convenience */

      private function roundContext(context:MathContext):BigDecimal {
         return round(context.digits, context.roundingMode);
      }

      /* <sgml> Round to specified digits, if necessary.
         Arg1 is requested length (digits to round to)
         [may be <=0 when called from format, dodivide, etc.]
         Arg2 is rounding mode
         returns this, for convenience

         ind and exp are adjusted, but not cleared for a mantissa of zero

         The length of the mantissa returned will be Arg1, except when Arg1
         is 0, in which case the returned mantissa length will be 1.
         </sgml>
       */

      private function round(len:int, mode:int):BigDecimal {
         var adjust:int;
         var sign:int;
         var oldmant:Vector.<int>;
         var reuse:Boolean = false;
         var first:int = 0;
         var increment:int;
         var newmant:Vector.<int> = null;

         adjust = mant.length - len;
         if (adjust <= 0) {
            return this; // nowt to do
         }

         exp = toIntExponent(exp + adjust); // exponent of result
         sign = ind; // save [assumes -1, 0, 1]
         oldmant = mant; // save

         if (len > 0) {
            // remove the unwanted digits
            mant = mant.slice(0, len);
            reuse = true; // can reuse mantissa
            first = oldmant[len]; // first of discarded digits
         }
         else {
            /* len<=0 */
            mant = ZERO.mant;
            ind = iszero;
            reuse = false; // cannot reuse mantissa
            if (len == 0)
               first = oldmant[0];
            else
               first = 0; // [virtual digit]
         }

         // decide rounding adjustment depending on mode, sign, and discarded digits
         increment = 0; // bumper

         /*modes*/
         if (mode == MathContext.ROUND_HALF_UP) {
            // default first [most common]
            if (first >= 5) {
               increment = sign;
            }
         }
         else if (mode == MathContext.ROUND_UNNECESSARY) {
            // default for setScale()
            // discarding any non-zero digits is an error
            if (!allzero(oldmant, len)) {
               throw new ArithmeticError("Rounding necessary");
            }
         }
         else if (mode == MathContext.ROUND_HALF_DOWN) {
            // 0.5000 goes down
            if (first > 5) {
               increment = sign;
            }
            else if (first == 5) {
               if (!allzero(oldmant, len + 1)) {
                  increment = sign;
               }
            }
         }
         else if (mode == MathContext.ROUND_HALF_EVEN) {
            // 0.5000 goes down if left digit even
            if (first > 5) {
               increment = sign;
            }
            else if (first == 5) {
               if (!allzero(oldmant, len + 1)) {
                  increment = sign;
               }
               else /* 0.5000 */ if (((mant[mant.length - 1]) % 2) == 1) {
                  increment = sign;
               }
            }
         }
         else if (mode == MathContext.ROUND_DOWN) {
            // never increment
         }
         else if (mode == MathContext.ROUND_UP) {
            // increment if discarded non-zero
            if (!allzero(oldmant, len)) {
               increment = sign;
            }
         }
         else if (mode == MathContext.ROUND_CEILING) {
            // more positive
            if (sign > 0) {
               if (!allzero(oldmant, len)) {
                  increment = sign;
               }
            }
         }
         else if (mode == MathContext.ROUND_FLOOR) {
            // more negative
            if (sign < 0) {
               if (!allzero(oldmant, len)) {
                  increment = sign;
               }
            }
         }
         else {
            throw new ArgumentError("Bad round value: " + mode);
         }
         /*modes*/

         if (increment != 0) { /*bump*/
            if (ind == iszero) {
               // we must not subtract from 0, but result is trivial anyway
               mant = ONE.mant;
               ind = increment;
            }
            else {
               // mantissa is non-0; we can safely add or subtract 1
               if (ind == isneg) {
                  increment = -increment;
               }
               newmant = byteaddsub(mant, mant.length, ONE.mant, 1, increment, reuse);
               if (newmant.length > mant.length) {
                  // had a carry
                  // drop rightmost digit and raise exponent
                  exp++;
                  // mant is already the correct length
                  newmant.length = mant.length;
               }
               mant = newmant;
            }
         }
         /*bump*/
         return this;
      }

      /* <sgml> Test if rightmost digits are all 0.
         Arg1 is a mantissa array to test
         Arg2 is the offset of first digit to check
         [may be negative; if so, digits to left are 0's]
         returns 1 if all the digits starting at Arg2 are 0

         Arg2 may be beyond array bounds, in which case 1 is returned
         </sgml> */

      private static function allzero(array:Vector.<int>, i:int):Boolean {
         if (i < 0) {
            i = 0;
         }

         const len:int = array.length;
         for (; i < len; i++) {
            if (array[i] != 0) {
               return false;
            }
         }

         return true;
      }

      /**
       * Returns a copy of the given vector padded by pad extra zeroes
       * @param vec the vector to extend
       * @param pad the amount to pad by
       * @return a new vector
       */
      private static function padBy(vec:Vector.<int>, pad:uint):Vector.<int> {
         const r:Vector.<int> = vec.slice();
         r.length += pad;
         if (vectorPadFix) {
            const last:int = r.length - 1;
            for (; pad-- > 0; ) {
               r[last - pad] = 0;
            }
         }
         return r;
      }

      /**
       * Returns true if we need to fix-up lengthened vectors with extra zeroes
       */
      private static function needVectorPadFix():Boolean {
         const version:Array = Capabilities.version.split(' ')[1].split(',');
         return version[0] == "10" && int(version[1]) < 2;
      }

      /* <sgml> Carry out final checks and canonicalization
         <p>
         This finishes off the current number by:
         1. Rounding if necessary (NB: length includes leading zeros)
         2. Stripping trailing zeros (if requested and \PLAIN)
         3. Stripping leading zeros (always)
         4. Selecting exponential notation (if required)
         5. Converting a zero result to just '0' (if \PLAIN)
         In practice, these operations overlap and share code.
         It always sets form.
         </sgml>
         Arg1 is requested MathContext (length to round to, trigger, and FORM)
         returns this, for convenience
       */

      private function finish(context:MathContext):BigDecimal {
         /* Round if mantissa too long and digits requested */
         if (context.digits != 0) {
            if (mant.length > context.digits) {
               roundContext(context);
            }
         }

         /* Now check for leading- and all- zeros in mantissa */
         const len:int = mant.length;
         for (var i:int = 0; i < len; i++) { /*i*/
            if (mant[i] != 0) {
               // non-0 result; ind will be correct
               // remove leading zeros [e.g., after subtract]
               if (i > 0) { /*delead*/
                  mant = mant.slice(i);
               }
               /*delead*/

               return this;
            }
         }
         /*i*/

         // Drop through to here only if mantissa is all zeros
         ind = iszero;
         mant = ZERO.mant; // canonical mantissa
         return this;
      }

      /**
       * @copy IExternalizable#writeExternal
       */
      public function writeExternal(output:IDataOutput):void {
         output.writeObject(mant);
         output.writeInt(exp);
         output.writeInt(ind);
      }

      /**
       * @copy IExternalizable#readExternal
       */
      public function readExternal(input:IDataInput):void {
         mant = input.readObject();
         exp = input.readInt();
         ind = input.readInt();
      }
   }
}
