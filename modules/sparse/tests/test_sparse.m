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
A = sparse([]);
assert_isequal(size(A), [0 0]);
assert_isequal(class(A), 'sparsedouble');
assert_istrue(issparse(A));
A(10, 11) = 1;
assert_isequal(size(A), [10 11]);
[a, b, c, d, e] = IJV(A);
ref_a = 10;
ref_b = 11;
ref_c = 1;
ref_d = 10;
ref_e = 11;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
a = logical(sparse(1));
assert_isequal(class(a), 'sparselogical');
assert_isequal(size(a), [1 1]);
assert_istrue(full(a(1, 1)));
%=============================================================================
assert_checkerror('S = sparse(zeros(3, 3), 3, 3)', _('in I, J, V format, all three vectors must be the same size or be scalars.'));
%=============================================================================
R = sparse([], 4, 4);
REF = sparse([]);
assert_isequal(R, REF);
%=============================================================================
R = sparse([], 4, 4, 5, 6);
S = size(R);
assert_isequal(S, [5, 6]);
%=============================================================================
