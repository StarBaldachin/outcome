/++
 + A result type built upon [`std.sumtype`](https://dlang.org/phobos/std_sumtype).
 +
 + License: MIT
 +/
module starcanopy.outcome;

import std.sumtype: isSumType, SumType;

/+++/
private enum compareNegTypes(A, B) = A.stringof[0] < B.stringof[0];

/++
 + Detects whether a type is an `Outcome`.
 +/
template isOutcome(Type) {
	import std.meta: isSorted = staticIsSorted;
	import std.traits: Unqual;
	enum isOutcome = isSumType!Type
		&& isSumType!(Type.Types[1])
		&& isSorted!(compareNegTypes, Type.Types[1].Types)
		&& is(Unqual!(Type.Types[$-1]) == OutcomeMarker);
}

/+++/
@nogc nothrow pure @safe unittest {
	alias Outcome_ = Outcome!(int, string, wstring);
	alias SumType_ = SumType!(int, string, wstring);

	static assert(isOutcome!Outcome_);
	static assert(isSumType!Outcome_);

	// Every Outcome is a SumType, but the reverse is not true.
	static assert(!isOutcome!SumType_);
}

/++
 + Resolves to an alias sequence of the types an outcome may hold, where the first element is the
 + positive type, and the subsequent elements are the negative types.
 +
 + This template ignores the `OutcomeMarker`, resolving to just the types specified by the user.
 +
 + Params:
 +	Outcome_	   = The outcome from which to extract the types.
 +	withQualifiers = Whether to return the types with the qualifiers of `Outcome_` applied to them.
 +					 The default behavior is with qualifiers.
 +/
template AllTypes(Outcome_, bool withQualifiers = true)
if (isOutcome!Outcome_) {
	import std.meta: AliasSeq;

	static if (withQualifiers) {
		import std.meta: ApplyLeft, Map = staticMap;
		import std.traits: CopyTypeQualifiers;

		alias AllTypes = Map!(
			ApplyLeft!(CopyTypeQualifiers, Outcome_),
			AliasSeq!(Outcome_.Types[0], Outcome_.Types[1].Types)
		);
	}

	else
		alias AllTypes = AliasSeq!(Outcome_.Types[0], Outcome_.Types[1].Types);
}

/+++/
@nogc nothrow pure @safe unittest {
	import std.meta: AliasSeq;

	static assert(
		is(AllTypes!(shared Outcome!(int, string)) == AliasSeq!(shared int, shared string))
	);
	static assert(is(AllTypes!(shared Outcome!(int, string), false) == AliasSeq!(int, string)));
}

/++
 + Resolves to the negative type wrapper of an outcome. It's a `SumType`.
 +/
template AnyNegative(Outcome_, bool withQualifiers = true)
if (isOutcome!Outcome_) {
	static if (withQualifiers) {
		import std.traits: CopyTypeQualifiers;
		alias AnyNegative = CopyTypeQualifiers!(Outcome_, Outcome_.Types[1]);
	}
	else
		alias AnyNegative = Outcome_.Types[1];
}

/+++/
@nogc nothrow pure @safe unittest {
	static assert(
		is(AnyNegative!(shared Outcome!(int, string, wstring)) == shared SumType!(string, wstring))
	);
	static assert(
		is(AnyNegative!(shared Outcome!(int, string, wstring), false) == SumType!(string, wstring))
	);
}

/++
 + A `Maybe` type implemented as an `Outcome`.
 +
 + Params:
 +	Value = The expected value.
 +/
alias Maybe(Value) = Outcome!(Value, Nothing);

/+++/
@nogc nothrow pure @safe unittest {
	Maybe!int(10)
		.handle!(
			(int val) => val,
			(Nothing _) => -1
		)
		.mustBe(10);
}

/++
 + Resolves to an alias sequence of the negative types an outcome may hold.
 +
 + Params:
 +	Outcome_	   = The outcome from which to extract the types.
 +	withQualifiers = Whether to return the types with the qualifiers of `Outcome_` applied to them.
 +					 The default behavior is with qualifiers.
 +/
alias NegativeTypes(Outcome_, bool withQualifiers = true)
	= AllTypes!(Outcome_, withQualifiers)[1..$];

/+++/
@nogc nothrow pure @safe unittest {
	import std.meta: AliasSeq;

	static assert(
		is(
			NegativeTypes!(shared Outcome!(int, string, wstring))
			== AliasSeq!(shared string, shared wstring)
		)
	);
	static assert(is(NegativeTypes!(shared Outcome!(int, string), false) == AliasSeq!string));
}

/++
 + A type that represents the absence of another.
 +/
struct Nothing {}

/++
 + A result type
 +
 + Params:
 +	PositiveType  = The positive type that may be held.
 +	NegativeTypes = The negative types that may be held. There must be at least one.
 +/
template Outcome(PositiveType, NegativeTypes...)
if (NegativeTypes.length > 0) {
	import std.meta: Sort = staticSort;
	alias Outcome = SumType!(
		PositiveType, SumType!(Sort!(compareNegTypes, NegativeTypes)), OutcomeMarker
	);
}

/+++/
@nogc nothrow pure @safe unittest {
	// An int as a positive type and a string as the negative type.
	static assert(__traits(compiles, Outcome!(int, string)));
	// Ditto, except with wstring as an additional negative type.
	static assert(__traits(compiles, Outcome!(int, string, wstring)));
	// A negative type can be the same as the positive one.
	static assert(__traits(compiles, Outcome!(int, int)));
	// There cannot be two or more negative types that are identical to each other.
	static assert(!__traits(compiles, Outcome!(int, string, string)));
	// The order in which the negative types are passed doesn't affect the outcome type.
	static assert(is(Outcome!(int, string, wstring) == Outcome!(int, wstring, string)));
}

/++
 + A type used to distinguish an `Outcome` from a `SumType`.
 +/
struct OutcomeMarker {}

/++
 + Resolves to the positive type an outcome may hold.
 +
 + Params:
 +	Outcome_	   = The outcome from which to extract the type.
 +	withQualifiers = Whether to return the type with the qualifiers of `Outcome_` applied to it.
 +					 The default behavior is with qualifiers.
 +/
template PositiveType(Outcome_, bool withQualifiers = true)
if (isOutcome!Outcome_) {
	static if (withQualifiers) {
		import std.traits: CopyTypeQualifiers;
		alias PositiveType = CopyTypeQualifiers!(Outcome_, Outcome_.Types[0]);
	}

	else
		alias PositiveType = Outcome_.Types[0];
}

/+++/
@nogc nothrow pure @safe unittest {
	static assert(is(PositiveType!(shared Outcome!(int, string)) == shared int));
	static assert(is(PositiveType!(shared Outcome!(int, string), false) == int));
}

/++
 + Matches a given function to an outcome's currently held value, and calls it.
 +
 + `handle` is an alias to `std.sumtype.match`, but it's designed to ignore the `OutcomeMarker`,
 + and it correctly handles the negative types.
 +
 + Params:
 +	positiveHandler  = The function that will be called if the outcome is positive. It must be able
 +					   to take the positive value as an argument.
 +	negativeHandlers = The functions of which one will be called if the outcome is negative and
 +					   the negative value may be passed to it.
 +
 + Returns: The return value of the matched function.
 +/
template handle(alias positiveHandler, negativeHandlers...) {
	import std.sumtype: match;
	alias handle = match!(
		(OutcomeMarker marker) => noreturn.init,
		match!negativeHandlers,
		positiveHandler
	);
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	FileOutcome(File())
		.handle!(
			(File file) => 0,
			(FileAccessErr err) => 1,
			(FileNotFoundErr err) => 2,
		)
		.mustBe(0);

	FileOutcome(File())
		.handle!(
			file => true,
			err  => false
		)
		.mustBeTrue;
}

/+++/
bool isNegative(Outcome_)(auto ref Outcome_ outcome)
if (isOutcome!Outcome_) {
	return outcome.handle!(
		(in val) => false,
		(in err) => true
	);
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	neg!FileOutcome(FileAccessErr()).isNegative.mustBeTrue;
	neg!FileOutcome(FileNotFoundErr()).isNegative.mustBeTrue;
	pos!FileOutcome(File()).isNegative.mustBeFalse;
}

/+++/
template isNegative(ExpectedErr) {
	bool isNegative(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_) {
		import std.meta: AliasSeq;

		static if (NegativeTypes!Outcome_.length > 1)
			alias otherNegHandler = (in _) => false;
		else
			alias otherNegHandler = AliasSeq!();

		return outcome.handle!(
			(in val) => false,
			(in ExpectedErr err) => true,
			otherNegHandler
		);
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	neg!FileOutcome(FileAccessErr()).isNegative!FileAccessErr.mustBeTrue;
	neg!FileOutcome(FileNotFoundErr()).isNegative!FileNotFoundErr.mustBeTrue;
	neg!FileOutcome(FileAccessErr()).isNegative!FileNotFoundErr.mustBeFalse;
}

/+++/
bool isNegative(Outcome_, ExpectedValue)(
	auto ref Outcome_ outcome,
	auto ref ExpectedValue expectedValue
)
if (isOutcome!Outcome_) {
	import std.meta: AliasSeq;

	static if (NegativeTypes!Outcome_.length > 1)
		alias otherNegHandler = (in _) => false;
	else
		alias otherNegHandler = AliasSeq!();

	return outcome.handle!(
		(in val) => false,
		(in ExpectedValue err) => err == expectedValue,
		otherNegHandler
	);
}

/+++/
@nogc nothrow pure @safe unittest {
	alias Outcome_ = Outcome!(int, string, wstring);

	neg!Outcome_("Error!").isNegative("Error!").mustBeTrue;
	neg!Outcome_("Different error!").isNegative("Error!").mustBeFalse;
	neg!Outcome_("Error!"w).isNegative("Error!").mustBeFalse;
	pos!Outcome_(10).isNegative("Error!").mustBeFalse;
}

/+++/
bool isPositive(Outcome_)(auto ref Outcome_ outcome)
if (isOutcome!Outcome_) {
	return outcome.handle!(
		(in val) => true,
		(in err) => false
	);
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	pos!FileOutcome(File()).isPositive.mustBeTrue;
	neg!FileOutcome(FileAccessErr()).isPositive.mustBeFalse;
	neg!FileOutcome(FileNotFoundErr()).isPositive.mustBeFalse;
}

/+++/
bool isPositive(Outcome_, ExpectedValue)(
	auto ref Outcome_ outcome,
	auto ref ExpectedValue expectedValue
)
if (isOutcome!Outcome_ && is(ExpectedValue : PositiveType!Outcome_)) {
	return outcome.handle!(
		(in val) => val == expectedValue,
		(in err) => false
	);
}

/+++/
@nogc nothrow pure @safe unittest {
	alias Outcome_ = Outcome!(int, float, string);

	pos!Outcome_(10).isPositive(10).mustBeTrue;
	pos!Outcome_(20).isPositive(10).mustBeFalse;
	neg!Outcome_("Error!").isPositive(10).mustBeFalse;
}

/+++/
alias mapAny(alias handler) = handle!(handler, handler);

/+++/
@nogc nothrow pure @safe unittest {
	alias Outcome_ = Outcome!(int, string, wstring);

	pos!Outcome_(10).mapAny!(any => typeof(any).stringof).mustBe("int");
	neg!Outcome_("").mapAny!(any => typeof(any).stringof).mustBe("string");
	neg!Outcome_(""w).mapAny!(any => typeof(any).stringof).mustBe("wstring");
}

/+++/
template neg(Outcome_)
if (isOutcome!Outcome_) {
	/+++/
	Outcome_ neg(Err)(auto ref Err err) {
		return typeof(return)(typeof(return).Types[1](err));
	}
}

/++
 + Returns a negative outcome holding a specified value.
 +
 + Params:
 +	PositiveType  = The positive type the returned outcome may hold.
 +	NegativeTypes = The other negative types the returned outcome may hold.
 +	value		  = The negative value the returned outcome will hold.
 +/
template neg(PositiveType, NegativeTypes...) {
	import std.meta: NoDuplicates;
	/+++/
	Outcome!(PositiveType, NoDuplicates!(NegativeTypes, NegativeValue))
	neg(NegativeValue)(auto ref NegativeValue value) {
		return typeof(return)(typeof(return).Types[1](value));
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	immutable outcome1 = neg!(int, wstring)("Error!");
	immutable outcome2 = neg!(int, string)("Error!"w);

	outcome1.isNegative("Error!").mustBeTrue;
	outcome2.isNegative("Error!"w).mustBeTrue;

	static assert(is(typeof(outcome1) == typeof(outcome2)));
}

/+++/
template pos(Outcome_)
if (isOutcome!Outcome_) {
	/+++/
	Outcome_ pos(Val)(auto ref Val val) {
		return typeof(return)(val);
	}
}

/+++/
template pos(Errs...) {
	/+++/
	Outcome!(Val, Errs) pos(Val)(auto ref Val val) {
		return typeof(return)(val);
	}
}

/++
 + Assigns a negative value to an outcome.
 +
 + Params:
 +	outcome = The outcome to hold the negative value.
 +	value = The negative value to be held.
 +/
ref Outcome_ negate(Outcome_, NegativeValue)(ref Outcome_ outcome, auto ref NegativeValue value)
if (isOutcome!Outcome_ && is(typeof(Outcome_.Types[1](value)))) {
	return outcome = outcome.Types[1](value);
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	auto outcome = pos!(FileAccessErr, FileNotFoundErr)(File());
	assert(outcome.isPositive);
	outcome.negate(FileAccessErr());
	assert(outcome.isNegative!FileAccessErr);
}

/++
 + Resolves an expression that may throw, returning a positive outcome holding the result of the
 + aforementioned expression. However, if the expression throws, then the returned outcome will
 + hold one of the specified exceptions or `Exception`.
 +
 + Params:
 +	Exceptions = A sequence of exceptions to catch. `Exception` is implicitly included.
 +	expression = The expression to resolve. If it's a void expression (e.g. calling a void
 +	function), then `Nothing` is assumed to be its type.
 +
 + Returns: An `Outcome!(Expression|Nothing, Exceptions, Exception)`.
 +/
template stash(Exceptions...) {
	/+++/
	nothrow
	auto stash(Expression)(lazy Expression expression) {
		import std.algorithm: castSwitch;
		import std.meta: NoDuplicates, Map = staticMap;

		static if (is(Expression == void))
			alias Expression_ = Nothing;

		else
			alias Expression_ = Expression;

		alias StashOutcome = Outcome!(Expression_, NoDuplicates!(Exceptions, Exception));
		alias wrap = e => StashOutcome(StashOutcome.Types[1](e));

		try {
			static if (is(Expression == void)) {
				expression();
				return StashOutcome(Nothing());
			}
			else
				return StashOutcome(expression());
		}

		catch (Exception e)
			return e.castSwitch!(Map!(wrap, Exceptions, Exception));
	}
}

/+++/
nothrow @safe unittest {
	import std.conv: ConvException, to;

	"1o"
		.to!int
		.stash!ConvException
		.handle!(
			(int val) => false,
			(ConvException e) => true,
			(Exception e) => false,
		)
		.mustBeTrue;

	"10"
		.to!int
		.stash!ConvException
		.isPositive(10)
		.mustBeTrue;
}

/+++/
nothrow @safe unittest {
	static void enforce(bool condition, string msg) {
		if (!condition)
			throw new Exception(msg);
	}

	enforce(true, "I'm flying!")
		.stash
		.handle!(
			(Nothing _) => true,
			(Exception e) => false
		)
		.mustBeTrue;

	enforce(false, "I'm flying!")
		.stash
		.handle!(
			(Nothing _) => false,
			(Exception e) => e.msg == "I'm flying!"
		)
		.mustBeTrue;
}

/+++/
nothrow @safe unittest {
	static struct File {}
	static class FileAccessException : Exception {
		@nogc nothrow pure @safe
		this(string msg_) { super(msg_); }
	}
	static class FileNotFoundException : Exception {
		@nogc nothrow pure @safe
		this(string msg_) { super(msg_); }
	}

	static File openFile(string path, string mode) {
		if (path != "test")
			throw new FileNotFoundException("File not found");
		else if (mode != "r")
			throw new FileAccessException("Cannot open file in write mode");
		else
			return File();
	}

	openFile("test", "r")
		.stash!(FileAccessException, FileNotFoundException)
		.handle!(
			(File file) => true,
			(FileAccessException e) => false,
			(FileNotFoundException e) => false,
			(Exception e) => false
		)
		.mustBeTrue;

	openFile("test", "w")
		.stash!(FileAccessException, FileNotFoundException)
		.isNegative!FileAccessException
		.mustBeTrue;

	openFile("nonexistent file", "r")
		.stash!(FileAccessException, FileNotFoundException)
		.isNegative!FileNotFoundException
		.mustBeTrue;
}

version (unittest) {
private:
	/+++/
	@nogc nothrow pure @safe
	void mustBeFalse(bool condition) {
		assert(!condition);
	}

	/+++/
	@nogc nothrow pure @safe
	void mustBeTrue(bool condition) {
		assert(condition);
	}

	/+++/
	@nogc nothrow pure @safe
	void mustBe(Expression)(auto ref Expression expression, Expression expectedValue) {
		import std.conv: text;
		debug assert(
			expression == expectedValue,
			"Expected ".text(expectedValue, ", but got ", expression)
		);
	}
}

/+
/++
 + Detects whether `fun` takes any of the negative values of an outcome as its only argument.
 +
 + Params:
 +	Outcome_ = The `Outcome` type.
 +	fun		 = The function to test.
 +/
template isNegativeHandler(Outcome_, alias fun)
if (isOutcome!Outcome_) {
	import std.meta: anySatisfy;
	import std.traits: lvalueOf;

	enum takesValue(Type) = is(typeof(fun(lvalueOf!Type)));
	enum isNegativeHandler = anySatisfy!(takesValue, Outcome_.Types[1..$-1]);
}

/++
 + Detects whether `fun` takes any of the negative values of an outcome as its only argument and
 + has a specified return type.
 +
 + Params:
 +	Outcome_  = The `Outcome` type.
 +	fun		  = The function to test.
 +	FunResult = The expected return type.
 +/
template isNegativeHandler(Outcome_, alias fun, FunResult)
if (isOutcome!Outcome_) {
	import std.meta: anySatisfy;
	import std.traits: CopyTypeQualifiers, lvalueOf;

	enum takesValue(Type) = is(
		typeof(fun(lvalueOf!Type)) == CopyTypeQualifiers!(Outcome_, FunResult)
	);
	enum isNegativeHandler = anySatisfy!(takesValue, Outcome_.Types[1..$-1]);
}

/++
 + Detects whether `fun` takes the positive value of an outcome as its only argument.
 +
 + Params:
 +	Outcome_ = The `Outcome` type.
 +	fun		 = The function to test.
 +/
template isPositiveHandler(Outcome_, alias fun)
if (isOutcome!Outcome_) {
	import std.traits: lvalueOf;
	enum isPositiveHandler = is(typeof(fun(lvalueOf!(Outcome_.Types[0]))));
}

/++
 + Detects whether `fun` takes the positive value of an outcome as its only argument and has a
 + specified return type.
 +
 + Params:
 +	Outcome_  = The `Outcome` type.
 +	fun		  = The function to test.
 +	FunResult = The expected return type.
 +/
template isPositiveHandler(Outcome_, alias fun, FunResult)
if (isOutcome!Outcome_) {
	import std.traits: CopyTypeQualifiers, lvalueOf;
	enum isPositiveHandler = is(
		typeof(fun(lvalueOf!(Outcome_.Types[0])))
		== CopyTypeQualifiers!(Outcome_, FunResult)
	);
}

/++
 + Returns the outcome of the application of `fun` on the positive value of an outcome, but if the
 + given outcome is negative, then `fun` is not called.
 +
 + Params:
 +	fun = The function to apply.
 +/
template andThen(alias fun) {
	/++
	 + Params:
	 +	outcome = The outcome upon which to operate.
	 +
	 + Returns:
	 + 1. If `fun` returns an outcome, then this function returns one whose positive type is the
	 +	  the positive type of `fun`, and whose negative types are the union of the negative types of
	 +	  `Outcome_` and `fun`.
	 + 2. If `fun` returns a non-outcome then this function returns one whose positive type is the
	 +	  return type of `fun`, and whose negative types are identical to `Outcome_`.
	 + 3. If `fun` doesn't return anything, then this function returns an `Outcome_`.
	 +/
	auto andThen(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_ && isPositiveHandler!(Outcome_, fun)) {
		import std.functional: pipe;
		import std.traits: CopyTypeQualifiers, lvalueOf;

		alias FunResult = typeof(fun(lvalueOf!(Outcome_.Types[0])));

		static if (is(FunResult == void)) {
			alias ThenOutcome = Outcome_;
			alias wrapValue = (ref CopyTypeQualifiers!(Outcome_, Outcome_.Types[0]) val) {
				fun(val);
				return Outcome_(val);
			};
		}

		else {
			static if (isOutcome!FunResult) {
				import std.meta: NoDuplicates;

				alias ThenOutcome = CopyTypeQualifiers!(
					Outcome_,
					CopyTypeQualifiers!(
						FunResult,
						Outcome!(
							FunResult.Types[0],
							NoDuplicates!(Outcome_.Types[1..$-1],
							FunResult.Types[1..$-1])
						)
					)
				);

				alias wrapValue = pipe!(fun, matchWith!ThenOutcome);
			}

			else {
				alias ThenOutcome = CopyTypeQualifiers!(
					Outcome_,
					Outcome!(FunResult, Outcome_.Types[1..$-1])
				);

				alias wrapValue = pipe!(fun, ThenOutcome);
			}
		}

		return outcome.handle!(wrapValue, ThenOutcome);
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}
	static struct FileReadErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	alias isOpen = (File file) => true;
	alias readFile = (File file, bool succeed) => succeed
		? pos!FileReadErr(cast(string)null) : neg!string(FileReadErr());

	assert(
		FileOutcome(File())
			.andThen!isOpen
			.matchWith!(
				(bool isOpen) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2
			)
			== 0
	);

	assert(
		FileOutcome(File())
			.andThen!(file => readFile(file, true))
			.matchWith!(
				(string contents) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2,
				(FileReadErr err) => 3
			)
			== 0
	);

	assert(
		FileOutcome(File())
			.andThen!(file => readFile(file, false))
			.matchWith!(
				(string contents) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2,
				(FileReadErr err) => 3
			)
			== 3
	);

	assert(
		FileOutcome(File())
			.andThen!((file) {})
			.matchWith!(
				(File file) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2
			)
			== 0
	);
}

/+++/
alias andThenEnsure(alias pred, alias whenNeg) = andThen!(ensure!(pred, whenNeg));

/+++/
template andThenIf(alias pred, alias whenTrue, alias whenFalse) {
	/+++/
	auto andThenIf(Outcome_)(auto ref Outcome_ outcome)
	if (
		isOutcome!Outcome_
		&& (isPositiveHandler!(Outcome_, pred, bool) || is(typeof(pred()) == bool))
		&& isPositiveHandler!(Outcome_, whenTrue)
		&& isPositiveHandler!(Outcome_, whenFalse)
	) {
		alias ThenOutcomeOnTrue = typeof(andThen!whenTrue(outcome));
		alias ThenOutcomeOnFalse = typeof(andThen!whenFalse(outcome));

		import std.functional: pipe;
		import std.meta: NoDuplicates;

		// The positive type of andThenIf's outcome will be the SumType of the handlers' positive
		// types.
		static if (!is(ThenOutcomeOnTrue.Types[0] == ThenOutcomeOnFalse.Types[0])) {
			alias AndThenIfOutcome = Outcome!(
				SumType!(ThenOutcomeOnTrue.Types[0], ThenOutcomeOnFalse.Types[0]),
				NoDuplicates!(ThenOutcomeOnTrue.Types[1..$-1], ThenOutcomeOnFalse.Types[1..$-1])
			);

			alias andThenIfOutcome = handle!(
				pipe!(AndThenIfOutcome.Types[0], AndThenIfOutcome), AndThenIfOutcome
			);
		}

		else {
			alias AndThenIfOutcome = Outcome!(
				ThenOutcomeOnTrue.Types[0],
				NoDuplicates!(ThenOutcomeOnTrue.Types[1..$-1], ThenOutcomeOnFalse.Types[1..$-1])
			);

			alias andThenIfOutcome = matchWith!AndThenIfOutcome;
		}

		// Obviate binding since pred doesn't take any arguments.
		static if (is(typeof(pred()))) {
			return pred()
				? pipe!(andThen!whenTrue, andThenIfOutcome)(outcome)
				: pipe!(andThen!whenFalse, andThenIfOutcome)(outcome);
		}

		else {
			return outcome
				.andThen!pred
				.handle!(
					predResult => predResult
						? pipe!(andThen!whenTrue, andThenIfOutcome)(outcome)
						: pipe!(andThen!whenFalse, andThenIfOutcome)(outcome),
					AndThenIfOutcome
				);
		}
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	static int countBytes(in File file) { return 32; }
	static int countLines(in File file) { return 16; }

	auto countBytesNotLines = true;

	assert(
		FileOutcome(File())
			.andThenIf!(
				() => countBytesNotLines,
				countBytes,
				countLines
			)
			.matchWith!(
				(uint n) => n,
				(FileAccessErr err) => -1,
				(FileNotFoundErr err) => -2
			)
			== 32
	);

	countBytesNotLines = false;

	assert(
		FileOutcome(File())
			.andThenIf!(
				() => countBytesNotLines,
				countBytes,
				countLines
			)
			.matchWith!(
				(uint n) => n,
				(FileAccessErr err) => -1,
				(FileNotFoundErr err) => -2
			)
			== 16
	);
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}
	static struct ProcessingErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	static Outcome!(ubyte[], ProcessingErr) processZipFile(bool fail)(in File file) {
		return fail ? typeof(return)(ProcessingErr()) : typeof(return)(cast(ubyte[])null);
	}

	static string readMagicNumber(string magicNumber)(in File file) { return magicNumber; }

	import std.sumtype: match, SumType;

	assert(
		FileOutcome(File())
			.andThenIf!(
				file => readMagicNumber!`PK\x03\x04`(file) == `PK\x03\x04`,
				processZipFile!false,
				file => "Not a zip file"
			)
			.handle!(
				match!(
					(ubyte[] data) => 0,
					(string errMsg) => 1
				),
				(FileAccessErr err) => 2,
				(FileNotFoundErr err) => 3,
				(ProcessingErr err) => 4
			)
			== 0
	);

	assert(
		FileOutcome(File())
			.andThenIf!(
				file => readMagicNumber!`PK\x03\x04`(file) == `PK\x03\x04`,
				processZipFile!true,
				file => "Not a zip file"
			)
			.handle!(
				match!(
					(ubyte[] data) => 0,
					(string errMsg) => 1
				),
				(FileAccessErr err) => 2,
				(FileNotFoundErr err) => 3,
				(ProcessingErr err) => 4
			)
			== 4
	);

	assert(
		FileOutcome(File())
			.andThenIf!(
				file => readMagicNumber!`7`(file) == `PK\x03\x04`,
				processZipFile!false,
				file => "Not a zip file"
			)
			.handle!(
				match!(
					(ubyte[] data) => 0,
					(string errMsg) => 1
				),
				(FileAccessErr err) => 2,
				(FileNotFoundErr err) => 3,
				(ProcessingErr err) => 4
			)
			== 1
	);
}

/+++/
template ensure(alias pred, alias whenNeg) {
	/+++/
	auto ensure(Value)(auto ref Value value)
	if (is(typeof(pred(value)) == bool) && (is(typeof(whenNeg(value))) || is(typeof(whenNeg())))) {
		import std.functional: partial;

		static if (is(typeof(whenNeg(value))))
			alias whenNeg_ = partial!(whenNeg, value);
		else
			alias whenNeg_ = whenNeg;

		alias EnsureOutcome = Outcome!(Value, typeof(whenNeg_()));

		return pred(value) ? EnsureOutcome(value) : EnsureOutcome(whenNeg_());
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct NullPtrErr { string id; }
	static struct Ptr { int* ptr; string id; }

	auto x = 10;
	auto p = Ptr((() @trusted => &x)(), "p");

	assert(
		p.ensure!(ptr => ptr.ptr !is null, () => NullPtrErr())
			.matchWith!(
				(Ptr ptr) => true,
				(NullPtrErr err) => false
			)
	);

	p.ptr = null;

	assert(
		p.ensure!(ptr => ptr.ptr !is null, ptr => NullPtrErr(ptr.id))
			.matchWith!(
				(Ptr ptr) => false,
				(NullPtrErr err) => err.id == "p"
			)
	);
}

/+++/
template handle(alias positiveHandler, negativeHandlers...) {
	auto ref handle(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_) {
		import std.sumtype: match;
		import std.traits: CopyTypeQualifiers;

		return outcome.match!(
			(Outcome_.Types[$-1] marker) => noreturn.init,
			(ref CopyTypeQualifiers!(Outcome_, Outcome_.Types[0]) val) => positiveHandler(val),
			negativeHandlers,
		);
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	assert(
		FileOutcome(File())
			.handle!(
				(File file) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2,
			)
			== 0
	);

	assert(
		FileOutcome(File())
			.handle!(
				file => true,
				err  => false
			)
	);
}

/+++/
auto just(Value)(auto ref Value value) {
	return Maybe!Value(value);
}

/+++/
@nogc nothrow pure @safe unittest {
	assert(
		just(10)
			.matchWith!(
				(int val) => val,
				(None _) => 0
			)
			== 10
	);
}

/+++/
template neg(Val, OtherErrs...) {
	/+++/
	Outcome!(Val, OtherErrs, Err) neg(Err)(auto ref Err err) {
		return typeof(return)(err);
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	assert(
		neg!(File, FileAccessErr)(FileNotFoundErr())
			.matchWith!(
				(FileNotFoundErr err) => true,
				other => false
			)
	);

	static assert(
		!is(
			typeof(neg!(File, FileAccessErr)(FileNotFoundErr()))
			== typeof(neg!(File, FileNotFoundErr)(FileAccessErr()))
		)
	);
}

/+++/
auto none(Value)() {
	return Maybe!Value(None());
}

/+++/
@nogc nothrow pure @safe unittest {
	assert(
		none!int
			.matchWith!(
				(int val) => false,
				(None _) => true
			)
	);
}

/+++/
template orElse(alias fun) {
	/+++/
	auto orElse(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_ && isNegativeHandler!(Outcome_, fun, Outcome_.Types[0])) {
		import std.functional: pipe;
		import std.meta: Filter, staticIndexOf;
		import std.traits: CopyTypeQualifiers, lvalueOf;

		enum passableToFun(Type) = is(typeof(fun(lvalueOf!Type)));
		alias HandledErrs = Filter!(passableToFun, Outcome_.Types[1..$-1]);
		alias FunResult = typeof(fun(lvalueOf!(HandledErrs[0])));
		enum isUnhandledErr(Type) = staticIndexOf!(Type, HandledErrs) == -1;

		static if (isOutcome!FunResult) {
			import std.meta: NoDuplicates;

			alias OrElseOutcome = CopyTypeQualifiers!(
				Outcome_,
				CopyTypeQualifiers!(
					FunResult,
					Outcome!(
						Outcome_.Types[0],
						Filter!(
							isUnhandledErr,
							NoDuplicates!(Outcome_.Types[1..$-1], FunResult.Types[1..$-1])
						)
					)
				)
			);

			alias wrapErr = pipe!(fun, matchWith!OrElseOutcome);
		}

		else {
			alias OrElseOutcome = CopyTypeQualifiers!(
				Outcome_,
				Outcome!(Outcome_.Types[0], Filter!(isUnhandledErr, Outcome_.Types[1..$-1]))
			);

			alias wrapErr = pipe!(fun, OrElseOutcome);
		}

		return outcome.matchWith!(wrapErr, OrElseOutcome);
	}
}

/+++/
template pos(Errs...) {
	/+++/
	Outcome!(Val, Errs) pos(Val)(auto ref Val val) {
		return typeof(return)(val);
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	assert(
		pos!(FileAccessErr, FileNotFoundErr)(File())
			.handle!(
				val => true,
				err => false
			)
	);
}

/++
 + Returns the positive value of an outcome.
 +
 + Params:
 +	outcome = The outcome to unwrap.
 +
 + Throws: An `Error` if `outcome` is negative.
 +/
auto unwrap(Outcome_)(auto ref Outcome_ outcome)
if (isOutcome!Outcome_) {
	return outcome
		.handle!(
			val => val,
			err => (() {
				throw new Error("Attempted to unwrap negative outcome");
				return noreturn.init;
			})()
		);
}

/+++/
nothrow pure @safe unittest {
	assert(pos!string(10).unwrap == 10);
}

/+++/
pure unittest {
	import std.exception: assertThrown;
	assertThrown!Error(neg!int("Whoops!").unwrap);
}

/++
 + Returns the positive value of an outcome if the latter is indeed holding one; otherwise, the
 + result of a supplied function is returned.
 +
 + Params:
 +	whenNeg = The function whose return value to use if `outcome` is negative.
 +/
template unwrap(alias whenNeg) {
	/++
	 + Params:
	 +	outcome = The outcome to unwrap.
	 +/
	auto unwrap(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_ && is(typeof(whenNeg()) == Outcome_.Types[0])) {
		return outcome
			.handle!(
				val => val,
				err => whenNeg()
			);
	}
}

/+++/
@nogc nothrow pure @safe unittest {
	alias whenNeg = () => -1;
	assert(pos!string(10).unwrap!whenNeg == 10);
	assert(neg!int("Whoops!").unwrap!whenNeg == -1);
}
+/