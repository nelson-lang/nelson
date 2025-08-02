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
REF = uint8([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = int8([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = uint16([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = int16([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = uint32([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = int32([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = uint64([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = int64([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
