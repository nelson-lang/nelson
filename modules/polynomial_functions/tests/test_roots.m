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
p = [3 -2 -4];
R = roots(p);
REF = [1.5352; -0.8685];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
p = [1 0 0 0 -1];
R = roots(p);
REF = [  -1.0000 + 0.0000i;
0.0000 + 1.0000i;
0.0000 - 1.0000i;
1.0000 + 0.0000i];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
P = [1, 0, -5];
R = roots (P);
REF = [2.2361; -2.2361];
assert_isapprox(abs(R), abs(REF), 1e-4);
%=============================================================================
P = [1.0000 0.1818 -31.1818 6.4786] 
R = roots(P);
REF = [-5.7753; 5.3851;  0.2083];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = roots([]);
REF = [];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = roots ([1, -6, 11, -6])
REF = [3; 2; 1];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = roots([1.0000   -9.4248   29.6088  -31.0063]);
REF = [3.2080 + 0.0000i;3.1084 + 0.0561i;3.1084 - 0.0561i];
assert_isapprox(real(R), real(REF), 1e-4);
assert_isapprox(imag(R), imag(REF), 1e-3);
%=============================================================================
R = roots([- 6.    11.  - 6.    1.  ]);
REF = [1.0000; -0.1667];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
