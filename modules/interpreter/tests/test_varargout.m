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
addpath([nelsonroot(), '/modules/interpreter/tests/']);
nolhs_function()
assert_checkerror('a = nolhs_function();', _('Wrong number of output arguments.'));
assert_checkerror('disp(nolhs_function())', _('Wrong number of output arguments.'));
onelhs_function()
assert_istrue(onelhs_function());
twolhs_function()
a = twolhs_function();
assert_isequal(a, 1);
[a, b] = twolhs_function();
assert_isequal(a, 1);
assert_isequal(b, 10);
varargoutlhs_function(0)
varargoutlhs_function(1)
assert_checkerror('A = varargoutlhs_function(0)', _('Not enough outputs in varargout to satisfy call.'));
A = varargoutlhs_function(1)
assert_isequal(A, 1);
A = varargoutlhs_function(2);
assert_isequal(A, 1);
[A, B] = varargoutlhs_function(2);
assert_isequal(A, 1);
assert_isequal(B, 2);
%=============================================================================
