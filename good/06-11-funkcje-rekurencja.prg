int f(int begin, int end) {
    if (begin == end) {
        return end;
    }

    println(begin);

    return f(begin + 1, end);
}

boolean g() {
    return true;
}

string h() {
    return "Hello";
}

int main() {

    println(f(0, 3));

    println(g());
    println(h());


    return 0;
}