void strdup(u8* to, u8* from, u64 len) {
    if (len == 0) {
        return;
    }
    u8* end = from + len;
    while (from != end) {
        *to = *from;
        to++;
        from++;
    }
}
