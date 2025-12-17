%=============================================================================
% Copyright (c) 2016-present Allan CORNET
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
clear all;
%=============================================================================
N_ITER  = 50000;            % Number of iterations
STABILIZATION_DELAY = 5;   % Seconds to wait before second memory check
%=============================================================================
% INITIAL MEMORY SNAPSHOT
%=============================================================================
[u1, s1] = memory();
%=============================================================================
addpath(fullfile(modulepath('stream_manager', 'tests'), 'loadsavebin'));
for i = 1:N_ITER
  typeofbin(201);
end
% Allow garbage collector and system allocator to settle
sleep(STABILIZATION_DELAY);
%=============================================================================
% MEMORY MEASUREMENT
%=============================================================================
[u2, s2] = memory();
delta_mem = u2.MemUsedNelson - u1.MemUsedNelson;
disp(['Memory difference after ', num2str(N_ITER), ...
' iterations: ', num2str(delta_mem), ' bytes']);
%=============================================================================
max_allowed = 126.0 * N_ITER;  % ~6300000 for 50000 iterations
%=============================================================================
assert_istrue(delta_mem < max_allowed);
%=============================================================================