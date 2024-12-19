int main() {

    int i = 0;

    while (i < 3) {
        println(i);
        i = i + 1;
    }

    if (true) {
        println("if");
    }

    if (false) {
        println("won't be printed");
    } else {
        println("else");
    }

    return 0;
}