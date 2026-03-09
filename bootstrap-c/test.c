#ifdef testing
void assert(u64 predicate) {
    if (!predicate) {
        exit(1);
    }
}

void test() {
    assert(16 == sizeof(Token));
    assert(16 == sizeof(TokenInfoT));
    assert(128 == sizeof(Tokenizer));
    assert(176 == sizeof(Module));
    assert(104 == sizeof(Asm));
}
#endif
