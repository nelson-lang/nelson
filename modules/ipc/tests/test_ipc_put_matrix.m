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
k = 1000;
A = rand(k, k);
tic()
ipc(getpid, 'put', A, 'R')
clear('A');
toc()
tic()
q = 0;while(~isvar('R') && q < 20), sleep(1), q = q + 1; end
toc()
assert_isequal(size(R), [k, k])
clear('R')
%=============================================================================
