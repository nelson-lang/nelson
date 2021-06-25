%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software: you can redistribute it and/or remify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('corrcoef'), 1);
assert_isequal(nargout('corrcoef'), 1);
%=============================================================================
R = corrcoef(magic(4));
REF = [   1.0000   -0.9776   -0.9069    0.7978;
-0.9776    1.0000    0.9753   -0.9069;
-0.9069    0.9753    1.0000   -0.9776;
 0.7978   -0.9069   -0.9776    1.0000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = corrcoef(magic(4)+2i);
REF = [   1.0000   -0.9776   -0.9069    0.7978;
-0.9776    1.0000    0.9753   -0.9069;
-0.9069    0.9753    1.0000   -0.9776;
 0.7978   -0.9069   -0.9776    1.0000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = corrcoef(single(magic(4)));
REF = [   1.0000   -0.9776   -0.9069    0.7978;
-0.9776    1.0000    0.9753   -0.9069;
-0.9069    0.9753    1.0000   -0.9776;
 0.7978   -0.9069   -0.9776    1.0000];
assert_isapprox(R, single(REF), 1e-4);
%=============================================================================
R = corrcoef([]);
assert_istrue(isnan(R));
%=============================================================================
R = corrcoef(zeros(0, 3));
REF = zeros(3, 3) * NaN;
assert_isequal(R, REF);
%=============================================================================
R = corrcoef(1);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = corrcoef([1, 2, 3]);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = corrcoef(ones(3,4,3));', _('Inputs must be 2-D.'));
%=============================================================================
