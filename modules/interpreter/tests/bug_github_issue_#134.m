%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
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
% <-- Issue URL -->
% https://github.com/Nelson-numerical-software/nelson/issues/134
% <-- Short Description -->
% Evaluation of Non-Scalar If-Condition Expression was not managed.
%=============================================================================
R = [];
if [false, false], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [true, false], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [false, true], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [true, true], R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
if [], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
if ones(3,0), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([false]), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([true]), R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
R = [];
if sparse([false, false]), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([true, false]), R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if sparse([true, true]), R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
R = [];
if [true, false, true], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if [false, true, false], R = true; else, R = false; end
assert_isfalse(R);
%=============================================================================
R = [];
if eps, R = true; else, R = false; end
assert_istrue(R);
%=============================================================================
R = [];
cmd = 'if sparse([0+i, 0+i]), R = true; else, R = false; end';
assert_checkerror(cmd, _('Complex cannot be converted to logical.'));
%=============================================================================
R = [];
cmd = 'if [0+i, 0+i], R = true; else, R = false; end';
assert_checkerror(cmd, _('Complex cannot be converted to logical.'));
%=============================================================================

