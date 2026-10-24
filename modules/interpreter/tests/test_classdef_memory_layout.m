%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% memory() is a coarse smoke test. Warm up classdef parsing and generated
% functions first, then verify repeated value construction does not keep growing.
%=============================================================================
warmup = ClassdefPoint(0, 0); %#ok<NLS0004>
clear warmup;
%=============================================================================
N_ITER = 5000;
[u0, s0] = memory(); %#ok<NLS0004>
for k = 1:N_ITER
  p = ClassdefPoint(k, k + 1); %#ok<NLS0004>
end
clear p;
[u1, s1] = memory(); %#ok<NLS0004>
for k = 1:N_ITER
  p = ClassdefPoint(k, k + 1);
end
clear p;
[u2, s2] = memory(); %#ok<NLS0004>
%=============================================================================
delta_first = u1.MemUsedNelson - u0.MemUsedNelson;
delta_second = u2.MemUsedNelson - u1.MemUsedNelson;
disp(['Classdef value first pass memory difference: ', num2str(delta_first), ' bytes']);
disp(['Classdef value second pass memory difference: ', num2str(delta_second), ' bytes']);
%=============================================================================
asserts.istrue(delta_second < 5 * 1024 * 1024);
%=============================================================================
rmpath(path_test);
%=============================================================================
