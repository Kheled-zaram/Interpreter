boolean f(var int ref, int val) {

    ref = ref + 1;
    val = val + 1;

    return true;
}

int main() {

    int x = 1, y = 1;

    println(x); // 1
    println(y); // 1

    f(x, y);

    println(x); // 2
    println(y); // 1


    return 0;
}