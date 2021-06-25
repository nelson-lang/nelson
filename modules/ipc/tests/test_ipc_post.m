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
% <--ADV-CLI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
clear('R');
REF = 3333;
ipc(getpid, 'post', 'R = 3333')
q = 0; while(~ipc(getpid, 'isvar', 'R', 'base') && q < 10), sleep(1), q = q + 1; end
assert_isequal(acquirevar('base', 'R'), REF);
%=============================================================================
clear('R');
REF = 3333;
ipc(getpid, 'post', 'R = 3333', 'local')
q = 0; while(~ipc(getpid, 'isvar', 'R', 'local') && q < 10), sleep(1), q = q + 1; end
assert_isequal(acquirevar('local', 'R'), REF);
%=============================================================================
