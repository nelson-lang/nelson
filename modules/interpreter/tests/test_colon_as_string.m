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
A = [ 1 2 3; 4 5 6; 7 8 9];
R = A(':');
REF = A(:);
assert_isequal(R, REF);
REF2 = [1; 4; 7; 2; 5; 8; 3; 6; 9];
assert_isequal(R, REF2);
%=============================================================================
R = A(2,':');
REF = A(2, ':');
assert_isequal(R, REF);
%=============================================================================
R = A(':', 3);
REF = A(:, 3);
assert_isequal(R, REF);
%=============================================================================
R = A(':', ':');
REF = A(:, :);
assert_isequal(R, REF);
%=============================================================================
R = A(':', ':', ':');
REF = A(:, :, :);
assert_isequal(R, REF);
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests']);
%=============================================================================
A = { 1 2 3; 4 5 6; 7 8 9};
assert_istrue(compare_colon(1, A{:}));
%=============================================================================
A = { 1 2 3; 4 5 6; 7 8 9};
assert_istrue(compare_colon(2, A{1, ':'}));
%=============================================================================
A = { 1 2 3; 4 5 6; 7 8 9};
assert_istrue(compare_colon(3, A{':', 2}));
%=============================================================================
A = { 1 2 3; 4 5 6; 7 8 9};
assert_istrue(compare_colon(4, A{':', ':'}));
%=============================================================================
A = { 1 2 3; 4 5 6; 7 8 9};
assert_istrue(compare_colon(5, A{':', ':', ':'}));
%=============================================================================
A = [ 1 2 3; 4 5 6; 7 8 9];
A(1,':') = [];
assert_isequal(A, [4 5 6; 7 8 9]);
%=============================================================================
A = [ 1 2 3; 4 5 6; 7 8 9];
A(':', 2) = [];
assert_isequal(A, [ 1 3; 4 6; 7 9]);
%=============================================================================
A = { 1 2 3; 4 5 6; 7 8 9};
assert_checkerror('A{'':''} = [];',_('Cannot convert string data types to indices.'));
%=============================================================================
A = [ 1 2 3; 4 5 6; 7 8 9];
A(':',1) = [10; 11; 12]
REF =[ 10 2 3; 11 5 6; 12 8 9];
assert_isequal(A, REF);
%=============================================================================
A = [ 1 2 3; 4 5 6; 7 8 9];
A(2,':') = [10; 11; 12]
REF =[     1     2     3;
10    11    12
 7     8     9];
assert_isequal(A, REF);
%=============================================================================
A = { 1 2 3; 4 5 6; 7 8 9};
A(':',1) = {10; 11; 12}
REF = { 10 2 3; 11 5 6; 12 8 9};
assert_isequal(A, REF);
%=============================================================================
