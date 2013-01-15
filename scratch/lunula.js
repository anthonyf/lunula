(function () {
    // intern all symbols
    window.Lunula = window.Lunula || {};
    Lunula.packages = Lunula.packages || {};
    Lunula.packages["KEYWORD"] = Lunula.packages["KEYWORD"] || {};
    Lunula.packages["KEYWORD"].symbols = Lunula.packages["KEYWORD"].symbols || {};
    Lunula.packages["COMMON-LISP"] = Lunula.packages["COMMON-LISP"] || {};
    Lunula.packages["COMMON-LISP"].symbols = Lunula.packages["COMMON-LISP"].symbols || {};
    Lunula.packages["LUNULA"] = Lunula.packages["LUNULA"] || {};
    Lunula.packages["LUNULA"].symbols = Lunula.packages["LUNULA"].symbols || {};
    Lunula.packages["LUNULA"].symbols["FOO5"] = Lunula.packages["LUNULA"].symbols["FOO5"] || { type: "symbol", name: "FOO5" };
    var FOO5 = Lunula.packages["LUNULA"].symbols["FOO5"] = Lunula.packages["LUNULA"].symbols["FOO5"];
    Lunula.packages["LUNULA"].symbols["FUNCALL"] = Lunula.packages["LUNULA"].symbols["FUNCALL"] || { type: "symbol", name: "FUNCALL" };
    var FUNCALL = Lunula.packages["LUNULA"].symbols["FUNCALL"] = Lunula.packages["LUNULA"].symbols["FUNCALL"];
    Lunula.packages["LUNULA"].symbols["FDEFINITION"] = Lunula.packages["LUNULA"].symbols["FDEFINITION"] || { type: "symbol", name: "FDEFINITION" };
    var FDEFINITION = Lunula.packages["LUNULA"].symbols["FDEFINITION"] = Lunula.packages["LUNULA"].symbols["FDEFINITION"];
    Lunula.packages["LUNULA"].symbols["SETF"] = Lunula.packages["LUNULA"].symbols["SETF"] || { type: "symbol", name: "SETF" };
    var SETF = Lunula.packages["LUNULA"].symbols["SETF"] = Lunula.packages["LUNULA"].symbols["SETF"];
    Lunula.packages["LUNULA"].symbols["FOO4"] = Lunula.packages["LUNULA"].symbols["FOO4"] || { type: "symbol", name: "FOO4" };
    var FOO4 = Lunula.packages["LUNULA"].symbols["FOO4"] = Lunula.packages["LUNULA"].symbols["FOO4"];
    Lunula.packages["COMMON-LISP"].symbols["NIL"] = Lunula.packages["COMMON-LISP"].symbols["NIL"] || { type: "symbol", name: "NIL" };
    var NIL = Lunula.packages["COMMON-LISP"].symbols["NIL"] = Lunula.packages["COMMON-LISP"].symbols["NIL"];
    Lunula.packages["LUNULA"].symbols["FOO3"] = Lunula.packages["LUNULA"].symbols["FOO3"] || { type: "symbol", name: "FOO3" };
    var FOO3 = Lunula.packages["LUNULA"].symbols["FOO3"] = Lunula.packages["LUNULA"].symbols["FOO3"];
    Lunula.packages["LUNULA"].symbols["FOO2"] = Lunula.packages["LUNULA"].symbols["FOO2"] || { type: "symbol", name: "FOO2" };
    var FOO2 = Lunula.packages["LUNULA"].symbols["FOO2"] = Lunula.packages["LUNULA"].symbols["FOO2"];
    Lunula.packages["LUNULA"].symbols["FOO"] = Lunula.packages["LUNULA"].symbols["FOO"] || { type: "symbol", name: "FOO" };
    var FOO = Lunula.packages["LUNULA"].symbols["FOO"] = Lunula.packages["LUNULA"].symbols["FOO"];
    Lunula.packages["LUNULA"].symbols["C"] = Lunula.packages["LUNULA"].symbols["C"] || { type: "symbol", name: "C" };
    var C = Lunula.packages["LUNULA"].symbols["C"] = Lunula.packages["LUNULA"].symbols["C"];
    Lunula.packages["LUNULA"].symbols["B"] = Lunula.packages["LUNULA"].symbols["B"] || { type: "symbol", name: "B" };
    var B = Lunula.packages["LUNULA"].symbols["B"] = Lunula.packages["LUNULA"].symbols["B"];
    Lunula.packages["LUNULA"].symbols["A"] = Lunula.packages["LUNULA"].symbols["A"] || { type: "symbol", name: "A" };
    var A = Lunula.packages["LUNULA"].symbols["A"] = Lunula.packages["LUNULA"].symbols["A"];
    Lunula.packages["LUNULA"].symbols["LIST"] = Lunula.packages["LUNULA"].symbols["LIST"] || { type: "symbol", name: "LIST" };
    var LIST = Lunula.packages["LUNULA"].symbols["LIST"] = Lunula.packages["LUNULA"].symbols["LIST"];
    Lunula.packages["LUNULA"].symbols["CDR"] = Lunula.packages["LUNULA"].symbols["CDR"] || { type: "symbol", name: "CDR" };
    var CDR = Lunula.packages["LUNULA"].symbols["CDR"] = Lunula.packages["LUNULA"].symbols["CDR"];
    Lunula.packages["LUNULA"].symbols["ERROR"] = Lunula.packages["LUNULA"].symbols["ERROR"] || { type: "symbol", name: "ERROR" };
    var ERROR = Lunula.packages["LUNULA"].symbols["ERROR"] = Lunula.packages["LUNULA"].symbols["ERROR"];
    Lunula.packages["LUNULA"].symbols["CAR"] = Lunula.packages["LUNULA"].symbols["CAR"] || { type: "symbol", name: "CAR" };
    var CAR = Lunula.packages["LUNULA"].symbols["CAR"] = Lunula.packages["LUNULA"].symbols["CAR"];
    Lunula.packages["LUNULA"].symbols["CONSP"] = Lunula.packages["LUNULA"].symbols["CONSP"] || { type: "symbol", name: "CONSP" };
    var CONSP = Lunula.packages["LUNULA"].symbols["CONSP"] = Lunula.packages["LUNULA"].symbols["CONSP"];
    Lunula.packages["LUNULA"].symbols["CONS"] = Lunula.packages["LUNULA"].symbols["CONS"] || { type: "symbol", name: "CONS" };
    var CONS = Lunula.packages["LUNULA"].symbols["CONS"] = Lunula.packages["LUNULA"].symbols["CONS"];
    Lunula.packages["LUNULA"].symbols["*"] = Lunula.packages["LUNULA"].symbols["*"] || { type: "symbol", name: "*" };
    var $star$ = Lunula.packages["LUNULA"].symbols["*"] = Lunula.packages["LUNULA"].symbols["*"];
    Lunula.packages["LUNULA"].symbols["REDUCE"] = Lunula.packages["LUNULA"].symbols["REDUCE"] || { type: "symbol", name: "REDUCE" };
    var REDUCE = Lunula.packages["LUNULA"].symbols["REDUCE"] = Lunula.packages["LUNULA"].symbols["REDUCE"];
    Lunula.packages["KEYWORD"].symbols["INITIAL-VALUE"] = Lunula.packages["KEYWORD"].symbols["INITIAL-VALUE"] || { type: "symbol", name: "INITIAL-VALUE" };
    var INITIAL_VALUE = Lunula.packages["KEYWORD"].symbols["INITIAL-VALUE"] = Lunula.packages["KEYWORD"].symbols["INITIAL-VALUE"];
    Lunula.packages["LUNULA"].symbols["+"] = Lunula.packages["LUNULA"].symbols["+"] || { type: "symbol", name: "+" };
    var $plus$ = Lunula.packages["LUNULA"].symbols["+"] = Lunula.packages["LUNULA"].symbols["+"];
    Lunula.packages["LUNULA"].symbols["2-ARG-*"] = Lunula.packages["LUNULA"].symbols["2-ARG-*"] || { type: "symbol", name: "2-ARG-*" };
    var $2_ARG_$star$ = Lunula.packages["LUNULA"].symbols["2-ARG-*"] = Lunula.packages["LUNULA"].symbols["2-ARG-*"];
    Lunula.packages["LUNULA"].symbols["2-ARG-+"] = Lunula.packages["LUNULA"].symbols["2-ARG-+"] || { type: "symbol", name: "2-ARG-+" };
    var $2_ARG_$plus$ = Lunula.packages["LUNULA"].symbols["2-ARG-+"] = Lunula.packages["LUNULA"].symbols["2-ARG-+"];
    Lunula.packages["LUNULA"].symbols["NULL"] = Lunula.packages["LUNULA"].symbols["NULL"] || { type: "symbol", name: "NULL" };
    var NULL = Lunula.packages["LUNULA"].symbols["NULL"] = Lunula.packages["LUNULA"].symbols["NULL"];
    Lunula.packages["LUNULA"].symbols["EQ"] = Lunula.packages["LUNULA"].symbols["EQ"] || { type: "symbol", name: "EQ" };
    var EQ = Lunula.packages["LUNULA"].symbols["EQ"] = Lunula.packages["LUNULA"].symbols["EQ"];
    Lunula.packages["LUNULA"].symbols["*PACKAGE*"] = Lunula.packages["LUNULA"].symbols["*PACKAGE*"] || { type: "symbol", name: "*PACKAGE*" };
    var $star$PACKAGE$star$ = Lunula.packages["LUNULA"].symbols["*PACKAGE*"] = Lunula.packages["LUNULA"].symbols["*PACKAGE*"];
    Lunula.packages["LUNULA"].symbols["T"] = Lunula.packages["LUNULA"].symbols["T"] || { type: "symbol", name: "T" };
    var T = Lunula.packages["LUNULA"].symbols["T"] = Lunula.packages["LUNULA"].symbols["T"];
    Lunula.packages["LUNULA"].symbols["NIL"] = Lunula.packages["LUNULA"].symbols["NIL"] || { type: "symbol", name: "NIL" };
    var NIL = Lunula.packages["LUNULA"].symbols["NIL"] = Lunula.packages["LUNULA"].symbols["NIL"];
    var $v;

    // (LUNULA::DEFPACKAGE "LUNULA")

    // (LUNULA::DEFPARAMETER LUNULA::NIL 'LUNULA::NIL)
    NIL.value = NIL;

    // (LUNULA::DEFPARAMETER LUNULA::T 'LUNULA::T)
    T.value = T;

    // (LUNULA::DEFPARAMETER LUNULA::*PACKAGE* LUNULA::NIL)
    $star$PACKAGE$star$.value = NIL.value;

    // (LUNULA::IN-PACKAGE "LUNULA")
    $star$PACKAGE$star$.value = Lunula.packages["LUNULA"];

    // (DEFUN EQ (A B) (JS-INLINE (A B T NIL) "$v = $p1 === $p2 ? $p3 : $p4;"))
    $v = function () {
        var $p1, $p2, $p3, $p4, $v, A, B;
        A = arguments[0];
        B = arguments[1];
        $v = NIL.value;
        $p1 = A;
        $p2 = B;
        $p3 = T.value;
        $p4 = NIL.value;
        $v = $p1 === $p2 ? $p3 : $p4;
        return $v;
    };
    EQ.function = $v;

    // (DEFUN NULL (THING) (EQ NIL THING))
    $v = function () {
        var THING;
        THING = arguments[0];
        return EQ.function(NIL.value, THING);
    };
    NULL.function = $v;

    // (DEFUN 2-ARG-+ (A B) (JS-INLINE (A B) "$v = $p1 + $p2;"))
    $v = function () {
        var $p1, $p2, $v, A, B;
        A = arguments[0];
        B = arguments[1];
        $v = NIL.value;
        $p1 = A;
        $p2 = B;
        $v = $p1 + $p2;
        return $v;
    };
    $2_ARG_$plus$.function = $v;

    // (DEFUN 2-ARG-* (A B) (JS-INLINE (A B) "$v = $p1 + $p2;"))
    $v = function () {
        var $p1, $p2, $v, A, B;
        A = arguments[0];
        B = arguments[1];
        $v = NIL.value;
        $p1 = A;
        $p2 = B;
        $v = $p1 + $p2;
        return $v;
    };
    $2_ARG_$star$.function = $v;

    // (DEFUN + (&REST ARGS) (REDUCE '2-ARG-+ ARGS :INITIAL-VALUE 0))
    $v = function () {
        var ARGS;
        var $n = 0;
        ARGS = NIL.value;
        for($n = arguments.length-1; $n >= 0; $n--) {
            ARGS = { type: "cons", car: arguments[$n], cdr: ARGS };
        }
        return REDUCE.function($2_ARG_$plus$, ARGS, INITIAL_VALUE.value, 0);
    };
    $plus$.function = $v;

    // (DEFUN * (&REST ARGS) (REDUCE '2-ARG-+ ARGS :INITIAL-VALUE 0))
    $v = function () {
        var ARGS;
        var $n = 0;
        ARGS = NIL.value;
        for($n = arguments.length-1; $n >= 0; $n--) {
            ARGS = { type: "cons", car: arguments[$n], cdr: ARGS };
        }
        return REDUCE.function($2_ARG_$plus$, ARGS, INITIAL_VALUE.value, 0);
    };
    $star$.function = $v;

    // (DEFUN CONS (A B)
    //  (JS-INLINE (A B) "$v = { type: \"cons\"," "       car: $p1,"
    //   "       cdr: $p2 };"))
    $v = function () {
        var $p1, $p2, $v, A, B;
        A = arguments[0];
        B = arguments[1];
        $v = NIL.value;
        $p1 = A;
        $p2 = B;
        $v = { type: "cons",
               car: $p1,
               cdr: $p2 };
        return $v;
    };
    CONS.function = $v;

    // (DEFUN CONSP (X)
    //  (JS-INLINE (X T NIL)
    //   "$v = (typeof ($p1) === 'object' && $p1.type === 'cons') ? $p2 : $p3;"))
    $v = function () {
        var $p1, $p2, $p3, $v, X;
        X = arguments[0];
        $v = NIL.value;
        $p1 = X;
        $p2 = T.value;
        $p3 = NIL.value;
        $v = (typeof ($p1) === 'object' && $p1.type === 'cons') ? $p2 : $p3;
        return $v;
    };
    CONSP.function = $v;

    // (DEFUN CAR (X)
    //  (IF (CONSP X) (JS-INLINE (X) "$v = $p1.car;")
    //   (IF (NULL X) NIL (ERROR "Cannot take car of ~A" X))))
    $v = function () {
        var $p1, $v, X;
        X = arguments[0];
        if (NIL.value != CONSP.function(X)) {
            $v = NIL.value;
            $p1 = X;
            $v = $p1.car;
        }
        else {
            if (NIL.value != NULL.function(X)) {
                $v = NIL.value;
            }
            else {
                $v = ERROR.function("Cannot take car of ~A", X);
            }
        }
        return $v;
    };
    CAR.function = $v;

    // (DEFUN CDR (X)
    //  (IF (CONSP X) (JS-INLINE (X) "$v = $p1.cdr;")
    //   (IF (NULL X) NIL (ERROR "Cannot take cdr of ~A" X))))
    $v = function () {
        var $p1, $v, X;
        X = arguments[0];
        if (NIL.value != CONSP.function(X)) {
            $v = NIL.value;
            $p1 = X;
            $v = $p1.cdr;
        }
        else {
            if (NIL.value != NULL.function(X)) {
                $v = NIL.value;
            }
            else {
                $v = ERROR.function("Cannot take cdr of ~A", X);
            }
        }
        return $v;
    };
    CDR.function = $v;

    // (DEFUN LIST (&REST ARGS) ARGS)
    $v = function () {
        var ARGS;
        var $n = 0;
        ARGS = NIL.value;
        for($n = arguments.length-1; $n >= 0; $n--) {
            ARGS = { type: "cons", car: arguments[$n], cdr: ARGS };
        }
        return ARGS;
    };
    LIST.function = $v;

    // (DEFPARAMETER A NIL)
    A.value = NIL.value;

    // (DEFPARAMETER B NIL)
    B.value = NIL.value;

    // (DEFPARAMETER C NIL)
    C.value = NIL.value;

    // (PROGN (SETQ A 100) (SETQ B 200) (SETQ C NIL))
    A.value = 100;
    B.value = 200;
    C.value = NIL.value;

    // ((LAMBDA (A) (SETQ A 100) (SETQ B 200)))
    $v = function () {
        var A;
        A = arguments[0];
        A = 100;
        B.value = 200;
        return 200;
    };
    $v();

    // (DEFUN FOO (A) (2-ARG-+ (2-ARG-+ A A) (2-ARG-+ A A))
    //  (2-ARG-+ (2-ARG-+ A A) (2-ARG-+ A A)))
    $v = function () {
        var A;
        A = arguments[0];
        $2_ARG_$plus$.function($2_ARG_$plus$.function(A, A), $2_ARG_$plus$.function(A, A));
        return $2_ARG_$plus$.function($2_ARG_$plus$.function(A, A), $2_ARG_$plus$.function(A, A));
    };
    FOO.function = $v;

    // (DEFUN FOO2 COMMON-LISP:NIL (2-ARG-+ (2-ARG-+ A B) (2-ARG-+ A B))
    //  (2-ARG-+ (2-ARG-+ A B) (2-ARG-+ A B)))
    $v = function () {
        $2_ARG_$plus$.function($2_ARG_$plus$.function(A.value, B.value), $2_ARG_$plus$.function(A.value, B.value));
        return $2_ARG_$plus$.function($2_ARG_$plus$.function(A.value, B.value), $2_ARG_$plus$.function(A.value, B.value));
    };
    FOO2.function = $v;

    // (DEFUN FOO3 (A B &OPTIONAL C) (LIST A B C))
    $v = function () {
        var A, B, C;
        A = arguments[0];
        B = arguments[1];
        C = arguments.length > 2 ? arguments[2] : NIL.value;
        return LIST.function(A, B, C);
    };
    FOO3.function = $v;

    // (DEFUN FOO4 (A B C) (SETF A (LIST 1 2 3)) (SETF (CAR A) 4) (SETF B 20 C 30))
    $v = function () {
        var A, B, C;
        A = arguments[0];
        B = arguments[1];
        C = arguments[2];
        A = LIST.function(1, 2, 3);
        LIST.function(1, 2, 3);
        FUNCALL.function(FDEFINITION.function({ type: 'cons', car: SETF, cdr: { type: 'cons', car: CAR, cdr: NIL } }), 4, A);
        B = 20;
        C = 30;
        return 30;
    };
    FOO4.function = $v;

    // (DEFUN FOO5 COMMON-LISP:NIL '(1 2 3))
    $v = function () {
        return { type: 'cons', car: 1, cdr: { type: 'cons', car: 2, cdr: { type: 'cons', car: 3, cdr: NIL } } };
    };
    FOO5.function = $v;
})();
