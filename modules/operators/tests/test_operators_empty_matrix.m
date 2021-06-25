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
R = ones(0, 1) + ones(1, 0);
REF = ones(0, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1) + ones(0, 0);
REF = ones(0, 0);
assert_isequal(R, REF);
%=============================================================================
R =  ones(0,1,0,1) + ones(0,0,0,0);
REF = zeros(0, 0, 0, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1,0,1) + ones(0,0,0,30);
REF = zeros(0, 0, 0, 30);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1,0,1) + ones(0,0,1,30);
REF = zeros(0, 0, 0, 30);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1,0,1) + ones(0,0,0,0);
REF = zeros(0, 0, 0, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1,0,1) + ones(0,0,0,0);
REF = zeros(0, 0, 0, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1) + ones(0, 0);
REF = zeros(0, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(30, 0) + ones(30, 0);
REF = ones(30, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(30,0) + ones(1, 0);
REF = ones(30, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(1,0)+ones(30,0);
REF = ones(30, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1) + ones(0,30);
REF = ones(0, 30);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,1,0,1) + ones(0,0,0,0);
REF = ones(0,0,0,0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0, 0, 0, 0) + ones(0, 1, 0, 1);
REF = ones(0, 0, 0, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0,0,0) + ones(0,1,0,1);
REF = ones(0,0,0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0, 1, 0, 1) + ones(0, 30);
REF = ones(0, 30, 0);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R =  ones(0,1,0,1) + ones(3,0,0,0)', [_('Size mismatch on arguments to arithmetic operator'), ' ', '+']);
%=============================================================================
assert_checkerror('R = ones(30, 0) + ones(0, 0);', [_('Size mismatch on arguments to arithmetic operator'), ' ', '+']);
%=============================================================================
assert_checkerror('R = ones(30, 0, 0, 0) + ones(0, 1, 0, 1);', [_('Size mismatch on arguments to arithmetic operator'), ' ', '+']);
%=============================================================================
assert_checkerror('R = ones(0,1) + ones(30, 0);', [_('Size mismatch on arguments to arithmetic operator'), ' ', '+']);
%=============================================================================
R = ones(0, 1) - ones(1, 0);
REF = ones(0, 0);
assert_isequal(R, REF);
%=============================================================================
R = ones(0, 4) / 3;
REF = ones(0, 4);
assert_isequal(R, REF);
%=============================================================================
R = ones(3, 0) / ones(1, 0);
R = [0;0;0];
REF = zeros(3, 1);
assert_isequal(R, REF);
%=============================================================================
R = ones(3, 0) / ones(6, 0);
REF = zeros(3, 6);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = ones(3, 0) / ones(3, 1);',  [_('Size mismatch on arguments to arithmetic operator'), ' ', '/']);
%=============================================================================
assert_checkerror('R = ones(1,0,0) / ones(1,0,30)', _('Wrong size for input arguments: 2D matrix expected.'));
%=============================================================================
assert_checkerror('R = 3 / ones(0, 4);',  [_('Size mismatch on arguments to arithmetic operator'), ' ', '/']);
%=============================================================================
assert_checkerror('R = 3 / ones(0, 1, 2)',  [_('Size mismatch on arguments to arithmetic operator'), ' ', '/']);
%=============================================================================
R = zeros(2,0) * zeros(0, 4);
REF = zeros(2, 4);
assert_isequal(R, REF);
%=============================================================================
p = 0; A = ones(3,2); B = ones(3,p); C = ones(2,3); D = ones(p,3);
R = [A B]*[C; D];
REF = ones(3, 3) * 2;
assert_isequal(R, REF);
%=============================================================================
