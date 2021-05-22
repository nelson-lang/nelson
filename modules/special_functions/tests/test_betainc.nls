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
assert_isequal(nargin('betainc'), 3)
assert_isequal(nargout('betainc'), 1)
%=============================================================================
R = betainc(1, [1, 1.5, 2, 3], 4:-1:1);
REF = ones(1, 4);
assert_isequal(R, REF);
%=============================================================================
R = betainc(1, [1, 1.5, 2, 3], 4:-1:1, 'upper');
REF = zeros(1, 4);
assert_isequal(R, REF);
%=============================================================================
R = betainc(1, [1, 1.5, 2, 3], 4:-1:1, 'lower');
REF = ones(1, 4);
assert_isequal(R, REF);
%=============================================================================
R = betainc([.2, .4, .6, .8], [1, 1.5, 2, 3], 4:-1:1);
REF = [0.59040   0.65143   0.64800   0.51200];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = betainc(single(1), single([1, 1.5, 2, 3]), single(4:-1:1));
REF = single(ones(1, 4));
assert_isequal(R, REF);
%=============================================================================
R = betainc(single(1), double([1, 1.5, 2, 3]), double(4:-1:1));
REF = single(ones(1, 4));
assert_isequal(R, REF);
%=============================================================================
R = betainc(0.5, 1, Inf);
assert_isequal(R, 1);
%=============================================================================
R = betainc(0.5, 1:10, 3);
REF = [0.8750    0.6875    0.5000    0.3438    0.2266    0.1445    0.0898    0.0547    0.0327    0.0193];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
assert_checkerror('betainc (-3, 1, 2)',_('Wrong value for #1 argument. [0, 1] values expected.'));
%=============================================================================