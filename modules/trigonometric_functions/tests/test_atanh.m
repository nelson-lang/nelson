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
assert_isequal(nargin('atanh'), 1);
assert_isequal(nargout('atanh'), 1);
%=============================================================================
assert_isequal(atanh(NaN),  NaN);
assert_isequal(atanh(-NaN), NaN);
assert_isequal(atanh(Inf), complex(0,pi * 0.5));
assert_isequal(atanh(-Inf), complex(0,-pi * 0.5));
%=============================================================================
v = [1.   0.8660254   0.7071068   0.5   0.   0.  -0.5  -0.7071068  -0.8660254  -1];
r = atanh(v);
ref = [Inf,  1.3170, 0.8814, 0.5493, 0, 0, -0.5493, -0.8814, -1.3170, -Inf];
assert_isapprox(real(r), ref, 1e-4);
%=============================================================================
x = atanh(0i);
ref = 0;
assert_isequal(x, ref);
%=============================================================================
X = atanh(zeros(3, 3, 3));
REF = zeros(3, 3, 3);
assert_isequal(X, REF);
%=============================================================================
R = atanh([12, NaN]);
REF = [0.0835+1.5708i, complex(NaN, 0)];
assert_isapprox(R, REF, 1e-3);
%=============================================================================
assert_isequal(atanh([]), []);
%=============================================================================
assert_checkerror('atanh(''a'')', [_('Undefined function ''atanh'' for input arguments of type '''), class('a'), '''.']);
%=============================================================================
