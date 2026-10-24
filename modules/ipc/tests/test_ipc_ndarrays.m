%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
clear('R');
REF = reshape(1:24, [2, 3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = reshape(int32(1:12), [2, 2, 3]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = reshape({1, 'two', single(3), int8(4), 5, "six", 7, uint16(8)}, [2, 2, 2]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = struct('a', {1, 2, 3; 4, 5, 6}, 'b', {'x', 'y', 'z'; 'u', 'v', 'w'});
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
