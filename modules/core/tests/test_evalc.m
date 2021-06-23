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
assert_isequal(nargin('evalc'), 1);
assert_isequal(nargout('evalc'), -1);
%=============================================================================
R = evalc('1');
REF = '
ans =

  1
';
assert_isequal(R, REF);
%=============================================================================
[R, B] = evalc('1');
assert_isequal(R, '');
assert_isequal(B, 1);
%=============================================================================
R = evalc('1;');
REF = '';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = 32');
REF = 'A =

  32
';
assert_isequal(R, REF);
%=============================================================================
R = evalc ('A = 32;');
assert_isequal(R, '');
%=============================================================================
assert_checkerror('evalc(''cos(1,2)'')', _('Wrong number of input arguments.'));
%=============================================================================
assert_checkerror('evalc(''input(''''prompt'''')'')', _('input function not allowed from evalc.'));
%=============================================================================
assert_checkerror('R = evalc(''error (''''AA'''')'', ''error(''''BB'''')'')', 'BB');
%=============================================================================
R = evalc('warning(''this a warning'')');
REF = 'this a warning';
assert_isequal(R, REF);
%=============================================================================
expected_msg = sprintf(_('Expecting %s'), _('an expression after ''+'''));
assert_checkerror('evalc(''A++'')', expected_msg);
%=============================================================================
