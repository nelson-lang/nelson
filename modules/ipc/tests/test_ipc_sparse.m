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
k = 10;
clear('R');
REF = sparse(rand(k, k) + i * rand(k,k));
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
k = 10;
clear('R');
REF = sparse(eye(k, k));
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
k = 10;
clear('R');
REF = sparse(logical(eye(k, k)));
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
