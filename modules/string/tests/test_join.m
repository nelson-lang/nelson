%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.isequal(nargin('join'), -2);
asserts.isequal(nargout('join'), 1);
%=============================================================================
R = join('add', " ");
REF = 'add';
asserts.isequal(R, REF);
%=============================================================================
R = join('', "ab");
REF = '';
asserts.isequal(R, REF);
%=============================================================================
str = ["a", "b", "c"];
R = join(str);
REF = "a b c";
asserts.isequal(R, REF);
%=============================================================================
str = ["a", "b", "c"];
R = join(str, "-");
REF = "a-b-c";
asserts.isequal(R, REF);
%=============================================================================
C = {'a', 'b', 'c'};
R = join(C);
REF = {'a b c'};
asserts.isequal(R, REF);
%=============================================================================
C = {'a', 'b', 'c'};
R = join(C, '+');
REF = {'a+b+c'};
asserts.isequal(R, REF);
%=============================================================================
M = ["a" "b"; "c" "d"];
R = join(M, "-");
REF = ["a-b"; "c-d"];
asserts.isequal(R, REF);
%=============================================================================
M = ["a" "b"; "c" "d"];
R = join(M, "-", 1);
REF = ["a-c" "b-d"];
asserts.isequal(R, REF);
%=============================================================================
M = ["a" "b"; "c" "d"];
R = join(M, "-", 2);
REF = ["a-b"; "c-d"];
asserts.isequal(R, REF);
%=============================================================================
R = join(string([]), "-");
REF = string(zeros(0, 1));
asserts.isequal(R, REF);
%=============================================================================
R = join({}, "-");
REF = cell(0, 1);
asserts.isequal(R, REF);
%=============================================================================
str = ["a", "b", "c"];
R = join(str, [":", ";"]);
REF = "a:b;c";
asserts.isequal(R, REF);
%=============================================================================
str = ["x", "y", "z"; "a", "b", "c"];
delimiters = [" + ", " = "; " - ", " = "];
R = join(str, delimiters);
REF = ["x + y = z"; "a - b = c"];
asserts.isequal(R, REF);
%=============================================================================
M = ["x", "y", "z";
 "a", "b", "c";
 "d", "e", "f";
 "g", "h", "i"];
L = ["A", "B";
  "C", "D";
  "E", "F";
  "G", "H"];
R = join(M, L);
REF = ["xAyBz";
  "aCbDc";
  "dEeFf";
  "gGhHi"];
asserts.isequal(R, REF);
%=============================================================================
M = ["x", "y", "z";
 "a", "b", "c";
 "d", "e", "f";
 "g", "h", "i"];
L = ["A", "B"];
R = join(M, L);
REF = ["xAyBz";
    "aAbBc";
    "dAeBf";
    "gAhBi"];
asserts.isequal(R, REF);
%=============================================================================
M = ["x", "y";
 "a", "b";
 "d", "e";
 "g", "h"];
L = ["A"; "B"; "C"; "D"];
R = join(M, L);
REF = ["xAy";
  "aAb";
  "dAe";
  "gAh"];
asserts.isequal(R, REF);
%=============================================================================
M = [string(NaN), "b", "c"];
R = join(M);
REF = string(NaN);
asserts.isequal(R, REF);
%=============================================================================
M = [string(NaN), "b"; "c", string(NaN)];
R = join(M);
REF = [string(NaN); string(NaN)];
asserts.isequal(R, REF);
%=============================================================================
m = ["a", string(NaN), "b"; "c", "e", "d"];
R = join(m, "-");
REF = [string(NaN); "c-e-d"];
asserts.isequal(R, REF);
%=============================================================================
m = [string(NaN), string(NaN), string(NaN)];
R = join(m, 1);
REF = [string(NaN), string(NaN), string(NaN)];
asserts.isequal(R, REF);
%=============================================================================
m = [string(NaN), string(NaN), string(NaN)];
R = join(m, 2);
REF = string(NaN);
asserts.isequal(R, REF);
%=============================================================================
str = ["x", "y", "z"; "a", "b", "c"];
R = join(str, string(NaN), 1);
REF = [string(NaN), string(NaN), string(NaN)];
asserts.isequal(R, REF);
%=============================================================================
str = ["x", "y", "z"; "a", "b", "c"];
R = join(str, string(NaN), 2);
REF = [string(NaN); string(NaN)];
asserts.isequal(R, REF);
%=============================================================================
M = ["x", "y", "z";
 "a", "b", "c";
 "d", "e", "f";
 "g", "h", "i"];
%=============================================================================
R = join(M, "A");
REF = ["xAyAz";
  "aAbAc";
  "dAeAf";
  "gAhAi"];
asserts.isequal(R, REF);
%=============================================================================
cmd = "R = join(M, [""A"";""B""])";
msg = _('Invalid delimiter dimensions.');
asserts.checkerror(cmd, msg);
%=============================================================================
R = join(M, ["A"; "B"; "C"], 1);
REF = ["xAaBdCg", "yAbBeCh", "zAcBfCi"];
asserts.isequal(R, REF);
%=============================================================================
R = "join(M, [""A""; ""B"";""C""], 2)";
msg = _('Invalid delimiter dimensions.');
asserts.checkerror(cmd, msg);
%=============================================================================
R = join(M, ["A", "B", "C"], 1);
REF = ["xAaBdCg", "yAbBeCh", "zAcBfCi"];
asserts.isequal(R, REF);
%=============================================================================
R = "join(M, [""A"", ""B"",""C""], 2)";
msg = _('Invalid delimiter dimensions.');
asserts.checkerror(cmd, msg);
%=============================================================================
R = "join(M, [""A"",""B"";""C"",""D""], 1)";
msg = _('Invalid delimiter dimensions.');
asserts.checkerror(cmd, msg);
%=============================================================================
R = "join(M, [""A"",""B"";""C"",""D""], 2)";
msg = _('Invalid delimiter dimensions.');
asserts.checkerror(cmd, msg);
%=============================================================================
M = ["x", "y", "z", "c";
     "a", "b", "c", "c";
     "d", "e", "f", "c";
     "g", "h", "i", "c"];
L = ["5", "6", "7", "8";
     "9", "10", "11", "12";
     "13", "14", "15", "16"];
R = join(M, L, 1);
REF = ["x5a9d13g", "y6b10e14h", "z7c11f15i", "c8c12c16c"];
asserts.isequal(R, REF);
%=============================================================================
M = {'x', 'y', 'z', 'c';
     'a', 'b', 'c', 'c';
     'd', 'e', 'f', 'c';
     'g', 'h', 'i', 'c'};
L = {'5', '6', '7', '8';
     '9', '10', '11', '12';
     '13', '14', '15', '16'};
R = join(M, L, 1);
REF = {'x5a9d13g', 'y6b10e14h', 'z7c11f15i', 'c8c12c16c'};
asserts.isequal(R, REF);
%=============================================================================
str = {'x', 'y', 'z'; 'a', 'b', 'c'};
R = join(str, string(NaN), 2);
REF = {''; ''};
asserts.isequal(R, REF);
%=============================================================================
M = {'x', 'y', 'z';
 'a', 'b', 'c';
 'd', 'e', 'f';
 'g', 'h', 'i'};
L = ["A", "B";
  "C", "D";
  string(NaN), "F";
  "G", "H"];
R = join(M, L);
REF = {'xAyBz';
  'aCbDc';
  '';
  'gGhHi'};
asserts.isequal(R, REF);
%=============================================================================
% a cell holding something else than text is rejected
msg = 'First argument must be a string array or a cell array of character vectors.';
asserts.checkerror('R = join({''a'', 5});', msg, 'Nelson:string:join:invalidInput');
asserts.checkerror('R = join({1, 2});', msg, 'Nelson:string:join:invalidInput');
%=============================================================================
