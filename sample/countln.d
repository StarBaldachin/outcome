/+ dub.sdl:
    dependency "outcome" path=".."
+/

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