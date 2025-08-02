%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
R = evalc('A = string(NaN)');
REF =  '
A =

    <missing>

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = string([])');
REF = '
A =

  0×0 empty string array

';
assert_isequal(R, REF)
%=============================================================================
A = ["d","d","d";
    "ddd","dddd","dd";
    "cc","cc","cc"];
R = evalc('A');
REF = '
A =

  3×3 string array

      "d"       "d"     "d"
    "ddd"    "dddd"    "dd"
     "cc"      "cc"    "cc"

';
assert_isequal(R, REF)
%=============================================================================
A = ["d","d","d";
    "ds", NaN,"ds";
    "c","c","c"];
R = evalc('A');
REF = '
A =

  3×3 string array

     "d"          "d"     "d"
    "ds"    <missing>    "ds"
     "c"          "c"     "c"

';
assert_isequal(R, REF)
%=============================================================================
A = ["dddlll" "dd" "1234567890ABCDEFGHIJKLMNOPQRSTVUWXY0123";"dddlll" "dd" "ddddddddddddddd";"d" "dd" ""];
R = evalc('A');
REF = '
A =

  3×3 string array

    "dddlll"    "dd"    "1234567890ABCDE…"
    "dddlll"    "dd"     "ddddddddddddddd"
         "d"    "dd"                    ""

';
assert_isequal(R, REF)
%=============================================================================
A = ["Mercury1" "Gemini2" "Apollo3" "Smith4","Chung5","Morales6" "Mercury7" "Gemini8" "Apollo9" "Smith10","Chung11","Morales12" "Mercury13" "Gemini14" "Apollo15" "Smith16","Chung17","Morales18";
       "Skylab19" "Skylab B20" "ISS21" "Sanchez22","Peterson23","Adams24" "Mercury25" "Gemini26" "Apollo27" "Smith28","Chung29","Morales30" "Mercury31" "Gemini32" "Apollo33" "Smith34","Chung35","Morales36"];
R = evalc('A');
REF =  '
A =

  2×18 string array

  Columns 1 through 5

    "Mercury1"       "Gemini2"    "Apollo3"       "Smith4"        "Chung5"
    "Skylab19"    "Skylab B20"      "ISS21"    "Sanchez22"    "Peterson23"

  Columns 6 through 10

    "Morales6"     "Mercury7"     "Gemini8"     "Apollo9"    "Smith10"
     "Adams24"    "Mercury25"    "Gemini26"    "Apollo27"    "Smith28"

  Columns 11 through 15

    "Chung11"    "Morales12"    "Mercury13"    "Gemini14"    "Apollo15"
    "Chung29"    "Morales30"    "Mercury31"    "Gemini32"    "Apollo33"

  Columns 16 through 18

    "Smith16"    "Chung17"    "Morales18"
    "Smith34"    "Chung35"    "Morales36"

';
assert_isequal(R, REF)
%=============================================================================
A = ["Smith","Chung","Morales"; 
       "Sanchez","Peterson","Adams"];
R = evalc('A');
REF = '
A =

  2×3 string array

      "Smith"       "Chung"    "Morales"
    "Sanchez"    "Peterson"      "Adams"

';
assert_isequal(R, REF)
%=============================================================================
A = ["dd", "dddddddddddddddddddddddddddddddddddddddddddddddddd"];
R = evalc('A');
REF = '
A =

  1×2 string array

    "dd"    "ddddddddddddddd…"

';
assert_isequal(R, REF)
%=============================================================================
A = ["ffffffffffffff", "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd", "dddddddddddddddddddddddddddddd","ddddddddddddddddddddddddddddddddddddddddddddddddddd"];
R = evalc('A');
REF =  '
A =

  1×4 string array

  Columns 1 through 3

    "ffffffffffffff"    "ddddddddddddddd…"    "ddddddddddddddd…"

  Column 4

    "ddddddddddddddd…"

';
assert_isequal(R, REF)
%=============================================================================
A=1:10:27*10;
B = string(reshape(A,3,3,3));
B(2,1, 2) = "ffffffffffffffffffdddddddddddffffffffffffffffffffff";
B(1,1, 3) = NaN;
R = evalc('B');
REF =  '
  3×3×3 string array

B(:,:,1) =

     "1"    "31"    "61"
    "11"    "41"    "71"
    "21"    "51"    "81"


B(:,:,2) =

                  "91"    "121"    "151"
    "fffffffffffffff…"    "131"    "161"
                 "111"    "141"    "171"


B(:,:,3) =

    <missing>    "211"    "241"
        "191"    "221"    "251"
        "201"    "231"    "261"

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('disp(B)');
REF =  '
(:,:,1) =

     "1"    "31"    "61"
    "11"    "41"    "71"
    "21"    "51"    "81"


(:,:,2) =

                  "91"    "121"    "151"
    "fffffffffffffff…"    "131"    "161"
                 "111"    "141"    "171"


(:,:,3) =

    <missing>    "211"    "241"
        "191"    "221"    "251"
        "201"    "231"    "261"

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('disp("hello")');
REF = 'hello
';
assert_isequal(R, REF)
%=============================================================================
R = evalc('disp(string(NaN))');
REF =  char([32    32    32    32    60   109   105   115   115   105   110   103    62    10]);
assert_isequal(R, REF)
%=============================================================================
R = evalc('display(string(NaN))');
REF = char([32    32    32    32    60   109   105   115   115   105   110   103    62    10    10]);
assert_isequal(R, REF)
%=============================================================================
R = evalc('display(string(NaN), ''A'')');
REF = char([ 10    65    32    61    10    10    32    32    32    32    60   109   105   115   115   105   110   103    62    10    10]);
assert_isequal(R, REF)
%=============================================================================
R = evalc('display("hello")');
REF = char([32    32    32    32    34   104   101   108   108   111    34    10    10]);
assert_isequal(R, REF)
%=============================================================================
R = evalc('display("hello", ''A'')');
REF = char([ 10    65    32    61    10    10    32    32    32    32    34   104   101   108   108   111    34    10    10]);
assert_isequal(R, REF)
%=============================================================================
A = ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";"vvvvvvvvvvvvvvvvvvvvvvvvvv";"ddddddddddddddddddd"];
R = evalc('A');
REF = '
A =

  3×1 string array

    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    "vvvvvvvvvvvvvvvvvvvvvvvvvv"
    "ddddddddddddddddddd"

' ;
assert_isequal(R, REF)
%=============================================================================
