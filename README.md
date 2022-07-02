A result type built upon [`std.sumtype`](https://dlang.org/phobos/std_sumtype).

# Examples
```d
// sample/countln.d
nothrow @safe
void main() {
    import starcanopy.outcome;
    import std.algorithm: count, splitter;
    import std.file: read;

    immutable numLines = "data/lines.txt"
        .read
        .stash
        .andThen!(
            (data) @trusted => (cast(char[])data)
                .splitter("\n")
                .count
        )
        .handle!(
            numLines => numLines,
            e => -1
        );

    assert(numLines == 10);
}
```

# Documentation

`outcome` uses plain ol' DDOC to generate its documentation. But there is the `pretty-docs` DUB
build for a slightly nicer presentation. Do note that the latter requires `sh` and `sed` to be
in the system's path.