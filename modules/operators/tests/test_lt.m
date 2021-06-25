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
assert_isequal(nargin('lt'), 2)
assert_isequal(nargout('lt'), 1)
%=============================================================================
% Nelson does not compare only real part for complex
A = 2 + i;
B = -3-2;
R = A < B;
assert_istrue(R);
%=============================================================================
R = "Nel" < "Son";
assert_istrue(R);
%=============================================================================
R = 2 < 3;
assert_istrue(R);
%=============================================================================
R = 2 > 3;
assert_isfalse(R);
%=============================================================================
A = ["apple", "orange","tomato"];
B = 'orange';
R = (A <= B);
REF = logical([1 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = [1 2; 3 4];
B = [1; 3];
R = A > B;
REF = logical([0, 1;
    0, 1]);
assert_isequal(R, REF);
%=============================================================================
B = [1 2; 3 4];
A = [1; 3];
R = A > B;
REF = logical([0, 0;
    0, 0]);
assert_isequal(R, REF);
%=============================================================================
