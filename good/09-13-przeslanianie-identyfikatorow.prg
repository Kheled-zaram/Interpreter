int main(){  // procedura / podprocedura - OK
    int x = 500;

    int zmglob() {
        x = x + 1;
        println(x); // zewnętrzny x: 501
        return 1;
    }

    int zmlok() {
        int x = 10;
        zmglob();
        println(x); // lokalny x: 10
        return 2;
    }

    zmlok();
    println(x);    // zewnętrzny x: 501
    return 0;
}

