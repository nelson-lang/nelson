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
clear functions;
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
mkdir([path_test, '/private']);
%=============================================================================
filewrite([path_test, '/private_cache_public.m'], ...
  ["function y = private_cache_public(n)"; ...
  "  y = 0;"; ...
  "  for k = 1:n"; ...
  "    y = private_cache_helper(y);"; ...
  "  end"; ...
  "end"]);
filewrite([path_test, '/private/private_cache_helper.m'], ...
  ["function y = private_cache_helper(x)"; ...
  "  y = x + 1;"; ...
  "end"]);
%=============================================================================
addpath(path_test);
asserts.isequal(private_cache_public(1), 1);
%=============================================================================
N_ITER = 10000;
MAX_BYTES_PER_CALL = 1200;
STABILIZATION_DELAY = 2;
%=============================================================================
asserts.isequal(private_cache_public(N_ITER), N_ITER);
sleep(STABILIZATION_DELAY);
[u1, s1] = memory();
asserts.isequal(private_cache_public(N_ITER), N_ITER);
sleep(STABILIZATION_DELAY);
[u2, s2] = memory();
%=============================================================================
delta_mem = u2.MemUsedNelson - u1.MemUsedNelson;
disp(['Memory difference after private function cache regression test: ', ...
  num2str(delta_mem), ' bytes']);
%=============================================================================
asserts.istrue(delta_mem < (MAX_BYTES_PER_CALL * N_ITER));
%=============================================================================
rmpath(path_test);
rmdir(path_test, 's');
clear functions;
%=============================================================================
