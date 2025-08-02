%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
