%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = ["A", "C", NaN, "E", "D"];
B = sort(A);
REF = ["A"    "C"    "D"    "E"    NaN];
assert_isequal(B, REF);
%=============================================================================
B = sort(A, 'ascend');
REF = ["A"    "C"    "D"    "E"    NaN];
assert_isequal(B, REF);
%=============================================================================
A = ["A", "C", NaN, "E", NaN, "D"];
B = sort(A, 'descend');
REF = [NaN,  NaN,  "E",    "D",    "C",    "A"];
assert_isequal(B, REF);
%=============================================================================
A = ["A", "C", NaN, "E", "D"];
B = sort(A,'MissingPlacement', 'last');
REF = ["A"    "C"    "D"    "E"    NaN];
assert_isequal(B, REF);
%=============================================================================
B = sort(A,'MissingPlacement', 'first');
REF = [NaN,    "A",    "C",    "D",    "E"];
assert_isequal(B, REF);
%=============================================================================
B = sort(A,'MissingPlacement', 'auto');
REF = ["A"    "C"    "D"    "E"    NaN];
assert_isequal(B, REF);
%=============================================================================
B = sort(A, 'ascend','MissingPlacement', 'last');
REF = ["A"    "C"    "D"    "E"    NaN];
assert_isequal(B, REF);
%=============================================================================
B = sort(A, 'ascend','MissingPlacement', 'first');
REF = [NaN,    "A",    "C",    "D",    "E"];
assert_isequal(B, REF);
%=============================================================================
B = sort(A, 'ascend','MissingPlacement', 'auto');
REF = ["A"    "C"    "D"    "E"    NaN];
assert_isequal(B, REF);
%=============================================================================
B = sort(A, 'descend','MissingPlacement', 'last');
REF = ["E"    "D"    "C"    "A"    NaN];
assert_isequal(B, REF);
%=============================================================================
B = sort(A, 'descend','MissingPlacement', 'first');
REF = [NaN, "E",    "D",    "C",    "A"];
assert_isequal(B, REF);
%=============================================================================
B = sort(A, 'descend','MissingPlacement', 'auto');
REF = [NaN, "E",    "D",    "C",    "A"];
assert_isequal(B, REF);
%=============================================================================
A = ["A", "C", NaN, "E", "D"];
B = sort(A');
REF = ["A"    "C"    "D"    "E"    NaN]';
assert_isequal(B, REF);
%=============================================================================
B = sort(A', 'ascend');
REF = ["A"    "C"    "D"    "E"    NaN]';
assert_isequal(B, REF);
%=============================================================================
A = ["A", "C", NaN, "E", NaN, "D"];
B = sort(A', 'descend');
REF = [NaN,  NaN,  "E",    "D",    "C",    "A"]';
assert_isequal(B, REF);
%=============================================================================
A = ["A", "C", NaN, "E", "D"];
B = sort(A', 'MissingPlacement', 'last');
REF = ["A"    "C"    "D"    "E"    NaN]';
assert_isequal(B, REF);
%=============================================================================
B = sort(A','MissingPlacement', 'first');
REF = [NaN,    "A",    "C",    "D",    "E"]';
assert_isequal(B, REF);
%=============================================================================
B = sort(A','MissingPlacement', 'auto');
REF = ["A"    "C"    "D"    "E"    NaN]';
assert_isequal(B, REF);
%=============================================================================
A = ["A", NaN , "D" ;"B", NaN, "C"; "F", "E", NaN];
B = sort(A, 'ascend');
REF = ["A", "E", "C";
"B", NaN,  "D";
"F", NaN, NaN];
assert_isequal(B, REF);
%=============================================================================
A = ["A", NaN , "D" ;"B", NaN, "C"; "F", "E", NaN];
B = sort(A, 'ascend', 'MissingPlacement', 'last');
REF = ["A", "E", "C";
"B", NaN, "D";
"F", NaN, NaN];
assert_isequal(B, REF);
%=============================================================================
A = ["A", NaN , "D" ;"B", NaN, "C"; "F", "E", NaN];
B = sort(A, 'descend');
REF = ["F", NaN, NaN;
"B", NaN, "D";
"A", "E", "C"  ];
assert_isequal(B, REF);
%=============================================================================
A = ["A", NaN , "D" ;"B", NaN, "C"; "F", "E", NaN];
B = sort(A, 'MissingPlacement', 'first');
REF = ["A", NaN, NaN;
"B", NaN, "C";
"F", "E", "D"  ];
assert_isequal(B, REF);
%=============================================================================
A = ["A", NaN , "D" ;"B", NaN, "C"; "F", "E", NaN];
B = sort(A, 'MissingPlacement', 'last');
REF = ["A", "E", "C"
"B", NaN, "D"
"F", NaN, NaN ];
assert_isequal(B, REF);
%=============================================================================
A = ["A", NaN , "D" ;"B", NaN, "C"; "F", "E", NaN];
B = sort(A, 'ascend','MissingPlacement', 'last');
REF = ["A", "E", "C"
"B", NaN, "D"
"F", NaN, NaN ];
assert_isequal(B, REF);
%=============================================================================
A = string({'Smith','Burns'; 'Jones','Matthews'; 'Peterson','Adams'});
B = sort(A, 'ascend');
REF = [  "Jones","Adams";
"Peterson","Burns";
"Smith","Matthews"];
assert_isequal(B, REF);
%=============================================================================
A = string({'Smith','Burns'; 'Jones','Matthews'; 'Peterson','Adams'});
B = sort(A, 'descend');
REF = ["Smith", "Matthews";
"Peterson", "Burns";
"Jones", "Adams" ];
assert_isequal(B, REF);
%=============================================================================
A = ["A", "C", NaN, "E", "D"];
[B, I] = sort(A);
REF = [ 1     2     5     4     3];
assert_isequal(I, REF);
%=============================================================================
A = ["A", NaN , "D" ;"B", NaN, "C"; "F", "E", NaN];
[B, I] = sort(A, 'ascend','MissingPlacement', 'last');
REF = [     1     3     2;
2     1     1;
3     2     3 ];
assert_isequal(I, REF);
%=============================================================================
A = ["A", "C", NaN, "E", NaN, "D"];
[B, I] = sort(A, 'descend');
REF = [     3,     5,     4,     6,     2,     1];
assert_isequal(I, REF);
%=============================================================================
A = ["A", "C", NaN, "E", NaN, "D"];
[B, I] = sort(A, 'ascend');
REF = [ 1     2     6     4     3     5];
assert_isequal(I, REF);
%=============================================================================
A = ["A", "C", NaN, "E", NaN, "D"];
[B, I] = sort(A, 'ascend','MissingPlacement', 'last');
REF = [1     2     6     4     3     5];
assert_isequal(I, REF);
%=============================================================================
A = ["A", "C", NaN, "E", NaN, "D"];
[B, I] = sort(A, 'ascend','MissingPlacement', 'first');
REF = [3     5     1     2     6     4];
assert_isequal(I, REF);
%=============================================================================
A = ["A", "C", NaN, "E", NaN, "D"];
[B, I] = sort(A, 'descend','MissingPlacement', 'first');
REF = [ 3     5     4     6     2     1];
assert_isequal(I, REF);
%=============================================================================
A = ["A", "C"; NaN, "E"; NaN, "D"];
[B, I] = sort(A, 'descend','MissingPlacement', 'first');
REF = [     2     2;
3     3;
1     1];
assert_isequal(I, REF);
%=============================================================================
A = ["A", "C"; NaN, "E"; NaN, "D"];
[B, I] = sort(A, 'descend','MissingPlacement', 'last');
REF = [     1     2;
2     3;
3     1];
assert_isequal(I, REF);
%=============================================================================
