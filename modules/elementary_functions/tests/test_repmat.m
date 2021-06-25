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
assert_isequal(nargin('repmat'), -1)
assert_isequal(nargout('repmat'), 1)
%=============================================================================
R = repmat(1:3, 2, 2);
REF = [1, 2, 3, 1, 2, 3;1, 2, 3, 1, 2, 3];
assert_isequal(R, REF);
%=============================================================================
R = repmat(1:3, 2);
REF = [1, 2, 3, 1, 2, 3;1, 2, 3, 1, 2, 3];
assert_isequal(R, REF);
%=============================================================================
R = repmat(1+20i, 2);
REF = [1+20*i, 1+20*i; 1+20*i, 1+20*i];
assert_isequal(R, REF);
%=============================================================================
R = repmat('Nelson', 3, 2);
REF = ['NelsonNelson';
'NelsonNelson';
'NelsonNelson']
assert_isequal(R, REF);
%=============================================================================
R = repmat(eps, 3, 2);
REF = eps * ones(3, 2);
assert_isequal(R, REF);
%=============================================================================
X = [10 20; 30 40];
R = repmat(X, [2 3 2]);
%=============================================================================
X = [10 20; 30 40];
R = repmat(X, [2 -3 2]);
REF = zeros(4, 0, 2);
assert_isequal(R, REF);
%=============================================================================
