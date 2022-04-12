%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
REF = 3;
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = 3;
ipc(getpid, 'put', REF, 'R', 'local')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = rand(1, 100);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = [1+2i; 3+4i];
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = ones(1000, 1000);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(REF, R);
%=============================================================================
