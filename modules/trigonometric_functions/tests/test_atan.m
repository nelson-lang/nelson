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
assert_isequal(nargin('atan'), 1);
assert_isequal(nargout('atan'), 1);
%=============================================================================
assert_isequal(atan(NaN),  NaN);
assert_isequal(atan(-NaN), NaN);
assert_isequal(atan(Inf), pi * 0.5);
assert_isequal(atan(-Inf), -pi * 0.5);
%=============================================================================
if ~ismac()
  A = rand(100, 100);
  assert_isapprox(atan(-A), -atan(A));
end
%=============================================================================
v = [1.   0.8660254   0.7071068   0.5   0.   0.  -0.5  -0.7071068  -0.8660254  -1];
r = atan(v);
ref = [ 0.7854    0.7137    0.6155    0.4636         0         0   -0.4636   -0.6155   -0.7137   -0.7854];
assert_isapprox(real(r), ref, 1e-4);
%=============================================================================
x = atan(0i);
ref = 0;
assert_isequal(x, ref);
%=============================================================================
X = atan(zeros(3, 3, 3));
REF = zeros(3, 3, 3);
assert_isequal(X, REF);
%=============================================================================
S = sparse(zeros(3, 3));
X = atan(S);
REF = sparse(3, 3);
assert_isequal(X, REF);
%=============================================================================
assert_isequal(atan([]), []);
%=============================================================================
assert_checkerror('atan(''a'')', [_('Undefined function ''atan'' for input arguments of type '''), class('a'), '''.']);
%=============================================================================
