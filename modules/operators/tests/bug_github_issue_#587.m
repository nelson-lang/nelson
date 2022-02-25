%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
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
% <-- Issue URL -->
% https://github.com/Nelson-numerical-software/nelson/issues/587
% <-- Short Description -->
% implicit cast to string array for horzcat and vertcat operators
%=============================================================================
A = {'a'; 'b';'c'};
B = ["d"; "e"; "f"];
%=============================================================================
R = [B; A];
REF = ["d";"e";"f";"a";"b";"c"];
assert_isequal(R, REF);
%=============================================================================
R = [A; B];
REF = ["a";"b";"c";"d";"e";"f"];
assert_isequal(R, REF);
%=============================================================================
A = {'a'; 2;'c'};
B = ["d"; "e"; "f"];
%=============================================================================
R = [A, B];
REF = [ "a",    "d";
"2",    "e";
"c",    "f"];
assert_isequal(R, REF);
%=============================================================================
R = [B, A];
REF = ["d",    "a";
"e",    "2";
"f",    "c"];
assert_isequal(R, REF);
%=============================================================================
R = [1; "A"];
REF = ["1"; "A"];
assert_isequal(R, REF);
%=============================================================================
R = ["A"; 1];
REF = ["A"; "1"];
assert_isequal(R, REF);
%=============================================================================
R = [100; 'A'];
REF = ['d';'A'];
assert_isequal(R, REF);
%=============================================================================
R = ['A'; 100];
REF = ['A';'d'];
assert_isequal(R, REF);
%=============================================================================
