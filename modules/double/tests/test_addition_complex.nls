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
% complex
A = 3;
B = 4i;
assert_isequal(A+B, 3+4i);
%=============================================================================
assert_isequal(3i + [], []);
%=============================================================================
assert_isequal([] + 3i, []);
%=============================================================================
assert_isequal(3i + ones(3,0), zeros(3, 0));
%=============================================================================
assert_isequal(ones(3,0) + 3i, zeros(3, 0));
%=============================================================================
A = [1 2; 3 4] * i;
B = [5 6; 7 8];
R = A + B;
REF = [5.0000 + 1.0000i,   6.0000 + 2.0000i;
   7.0000 + 3.0000i,   8.0000 + 4.0000i];
assert_isequal(R, REF);
%=============================================================================
A = [1 2; 3 4];
B = 10i;
R1 = A + B;
R2 = B + A;
REF = [1.0000+10.0000i,   2.0000+10.0000i;
   3.0000+10.0000i   4.0000+10.0000i];
assert_isequal(R1, R2);
assert_isequal(R1, REF);
%=============================================================================
A = [0 10; 10 0];
R = A + 20i;
REF = [  0.0000+20.0000i,  10.0000+20.0000i;
  10.0000+20.0000i,   0.0000+20.0000i];
assert_isequal(R, REF);
%=============================================================================
A = [10 0; 20 40];
B = [50 90; 20 10];
R = A + (B * i);
REF = [10.0000+50.0000i, 0.0000+90.0000i;
  20.0000+20.0000i, 40.0000+10.0000i];
assert_isequal(R, REF);
%=============================================================================
A = [10, 20, 30, 40];
B = [50; 60; 70] * i;
R1 = A + B;
R2 = B + A;
assert_isequal(R1, R2);
REF = [ 10.0000+50.0000i,  20.0000+50.0000i,  30.0000+50.0000i,  40.0000+50.0000i;
  10.0000+60.0000i,  20.0000+60.0000i,  30.0000+60.0000i,  40.0000+60.0000i;
  10.0000+70.0000i,  20.0000+70.0000i,  30.0000+70.0000i, 40.0000+70.0000i];
assert_isequal(R1, REF);
%=============================================================================
A = [10 20 30; 40 50 60];
B = [10; 10] * i;
R2 = B + A;
R1 = A + B;
assert_isequal(R1, R2);
REF = [  10.0000+10.0000i, 20.0000+10.0000i,  30.0000+10.0000i;
  40.0000+10.0000i, 50.0000+10.0000i,  60.0000+10.0000i];
assert_isequal(R1, REF);
%=============================================================================
A = [10 20 30; 40 50 60];
B = [1 2 3] * i;
R = A + B;
REF = [10.0000 + 1.0000i,  20.0000 + 2.0000i,  30.0000 + 3.0000i;
  40.0000 + 1.0000i,  50.0000 + 2.0000i,  60.0000 + 3.0000i];
assert_isequal(R, REF);
%=============================================================================
B = [10 20 30; 40 50 60];
A = [1 2 3] * i;
R = A + B;
REF = [10.0000 + 1.0000i,  20.0000 + 2.0000i,  30.0000 + 3.0000i;
  40.0000 + 1.0000i,  50.0000 + 2.0000i,  60.0000 + 3.0000i];
assert_isequal(R, REF);
%=============================================================================
