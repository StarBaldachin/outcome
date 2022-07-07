/++
 + A result type built upon [`std.sumtype`](https://dlang.org/phobos/std_sumtype).
 +
 + License: MIT
 +/
module starcanopy.outcome;

import std.sumtype: isSumType, SumType;

/+++/
alias Maybe(Value) = Outcome!(Value, None);

/+++/
@nogc nothrow pure @safe unittest {
	assert(
		Maybe!int(10)
			.matchWith!(
				(int val) => val,
				(None _) => 0
			)
			== 10
	);

	assert(
		Maybe!int(10)
			.handle!(
				val => val,
			 	_ => 0
			)
			== 10
	);
}

/+++/
struct None {}

/++
 + A result type
 +
 + Params:
 +	Value = The positive type that may be held.
 +	Errs  = The negative types that may be held. There must be at least one.
 +/
template Outcome(Value, Errs...)
if (Errs.length > 0) {
	alias Outcome = SumType!(Value, Errs, OutcomeMarker);
}

/++
 + Used to distinguish `Outcome` from `SumType`.
 +/
struct OutcomeMarker {}

/++
 + Resolves to the types an `Outcome` may hold, where the first element of the alias sequence is the
 + positive type, and the rest are the negative types.
 +
 + This template ignores the `OutcomeMarker`, resolving to just the types specified by the user.
 +/
template OutcomeTypes(Outcome)
if (isOutcome!Outcome) {
	alias OutcomeTypes = Outcome.Types[0..$-1];
}

/+++/
@nogc nothrow pure @safe unittest {
	import std.meta: AliasSeq;
	static assert(is(OutcomeTypes!(Outcome!(int, string)) == AliasSeq!(int, string)));
}

/++
 + Detects whether a type is an `Outcome`.
 +/
template isOutcome(Type) {
	import std.traits: Unqual;
	enum isOutcome = isSumType!Type && is(Unqual!(Type.Types[$-1]) == OutcomeMarker);
}

/+++/
@nogc nothrow pure @safe unittest {
	static struct File {}
	static struct FileAccessErr {}
	static struct FileNotFoundErr {}

	alias Outcome_ = Outcome!(File, FileAccessErr, FileNotFoundErr);
	alias SumType_ = SumType!(File, FileAccessErr, FileNotFoundErr);

	static assert(isOutcome!Outcome_);
	static assert(isSumType!Outcome_);

	// Every Outcome is a SumType, but the reverse is not true.
	static assert(!isOutcome!SumType_);
}

/+++/
template andThen(alias fun) {
	/+++/
	auto andThen(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_) {
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
						Outcome!(FunResult.Types[0], NoDuplicates!(Outcome_.Types[1..$-1], FunResult.Types[1..$-1]))
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

	alias FileOutcome = Outcome!(File, FileAccessErr, FileNotFoundErr);

	assert(
		FileOutcome(File())
			.andThen!(file => "")
			.matchWith!(
				(string contents) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2
		)
		== 0
	);

	assert(
		FileOutcome(File())
			.andThen!(file => Outcome!(int, string)(10))
			.matchWith!(
				(int numLines)		  => 0,
				(FileAccessErr err)	  => 1,
				(FileNotFoundErr err) => 2,
				(string err)		  => 3
		)
		== 0
	);

	assert(
		FileOutcome(File())
			.andThen!(file => Outcome!(int, string)("Empty"))
			.matchWith!(
				(int numLines) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2,
				(string err) => 3
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
alias andThenEnsure(alias pred, alias onErr) = andThen!(ensure!(pred, onErr));

/+++/
template andThenIf(alias pred, alias onTrue, alias onFalse) {
	/+++/
	auto andThenIf(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_) {
		alias ThenOutcomeOnTrue = typeof(andThen!onTrue(outcome));
		alias ThenOutcomeOnFalse = typeof(andThen!onFalse(outcome));

		import std.functional: pipe;
		import std.meta: NoDuplicates;

		// The positive type of andThenIf's outcome will be the SumType of the handlers' positive
		// types.
		static if (!is(ThenOutcomeOnTrue.Types[0] == ThenOutcomeOnFalse.Types[0])) {
			alias EitherOutcome = Outcome!(
				SumType!(ThenOutcomeOnTrue.Types[0], ThenOutcomeOnFalse.Types[0]),
				NoDuplicates!(ThenOutcomeOnTrue.Types[1..$-1], ThenOutcomeOnFalse.Types[1..$-1])
			);

			alias eitherOutcome = handle!(
				pipe!(EitherOutcome.Types[0], EitherOutcome), EitherOutcome
			);
		}

		else {
			alias EitherOutcome = Outcome!(
				ThenOutcomeOnTrue.Types[0],
				NoDuplicates!(ThenOutcomeOnTrue.Types[1..$-1], ThenOutcomeOnFalse.Types[1..$-1])
			);

			alias eitherOutcome = matchWith!EitherOutcome;
		}

		// Obviate binding since pred doesn't take any arguments.
		static if (is(typeof(pred()))) {
			return pred()
				? pipe!(andThen!onTrue, eitherOutcome)(outcome)
				: pipe!(andThen!onFalse, eitherOutcome)(outcome);
		}

		else {
			return outcome
				.andThen!pred
				.handle!(
					predResult => predResult
						? pipe!(andThen!onTrue, eitherOutcome)(outcome)
						: pipe!(andThen!onFalse, eitherOutcome)(outcome),
					EitherOutcome
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
template ensure(alias pred, alias onErr) {
	/+++/
	auto ensure(Value)(auto ref Value value) {
		static if (__traits(isTemplate, onErr))
			alias onErr_ = onErr!Value;
		else
			alias onErr_ = onErr;

		import std.traits: ReturnType;

		alias EnsureOutcome = Outcome!(Value, ReturnType!onErr_);

		if (pred(value))
			return EnsureOutcome(value);
		else {
			static if (is(typeof(onErr_())))
				return EnsureOutcome(onErr_());
			else
				return EnsureOutcome(onErr_(value));
		}
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

	assert(FileOutcome(FileAccessErr()).isNegative);
	assert(FileOutcome(FileNotFoundErr()).isNegative);
	assert(!FileOutcome(File()).isNegative);
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

	assert(FileOutcome(File()).isPositive);
	assert(!FileOutcome(FileAccessErr()).isPositive);
	assert(!FileOutcome(FileNotFoundErr()).isPositive);
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
template matchWith(handlers...) {
	/+++/
	auto ref matchWith(Outcome_)(auto ref Outcome_ outcome)
	if (isOutcome!Outcome_) {
		import std.sumtype: match;

		return outcome.match!(
			(Outcome_.Types[$-1] marker) => noreturn.init,
			handlers
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
			.matchWith!(
				(File file) => 0,
				(FileAccessErr err) => 1,
				(FileNotFoundErr err) => 2,
			)
			== 0
	);

	assert(
		FileOutcome(File())
			.matchWith!(
				(File file) => true,
				err => false
			)
	);

	assert(
		FileOutcome(File()).matchWith!(any => true)
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

// Need to add a test for when the user passes more than one exception type.
/+++/
template stash(Exceptions...) {
	/+++/
	nothrow
	auto stash(Expression)(lazy Expression expression) {
		import std.algorithm: castSwitch;
		import std.meta: NoDuplicates, Map = staticMap;

		static if (is(Expression == void))
			alias Expression_ = None;

		else
			alias Expression_ = Expression;

		alias StashOutcome = Outcome!(Expression_, NoDuplicates!(Exceptions, Exception));
		alias wrap = e => StashOutcome(e);

		try {
			static if (is(Expression == void)) {
				expression();
				return StashOutcome(None());
			}
			else
				return StashOutcome(expression());
		}
		catch (Exception e)
			return e.castSwitch!(Map!(wrap, StashOutcome.Types[1..$-1]));
	}
}

/+++/
nothrow @safe unittest {
	import std.conv: ConvException, to;

	assert(
		"1o"
			.to!int
			.stash!ConvException
			.matchWith!(
				(ConvException e) => true,
				(Exception e) => false,
				_ => false
			)
	);

	assert(
		"10"
			.to!int
			.stash!ConvException
			.handle!(
				val => val == 10,
				err => false
			)
	);
}

/+++/
nothrow @safe unittest {
	static void enforce(bool expression, string msg) {
		if (!expression)
			throw new Exception(msg);
	}

	assert(
		enforce(true, "I'm flying!")
			.stash
			.matchWith!(
				(None _) => true,
				(Exception e) => false
			)
	);

	assert(
		enforce(false, "I'm flying!")
			.stash
			.matchWith!(
				(None _) => false,
				(Exception e) => true
			)
	);
}

/+++/
auto ref unwrap(Outcome_)(auto ref Outcome_ outcome)
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