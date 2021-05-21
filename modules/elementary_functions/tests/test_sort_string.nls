%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
