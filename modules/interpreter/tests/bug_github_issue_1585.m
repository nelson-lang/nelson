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
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1585
% <-- Short Description -->
% Memory leak during scalar assignments in tight/nested loops after scalar inline-data optimization
%=============================================================================
clear all;
%=============================================================================
N_ITER = 100000;
%=============================================================================
% Lightweight leak smoke tests. memory() is not a precise RAM usage oracle, so
% these checks only guard against obvious repeated growth after warm-up.
%=============================================================================
[u0, s0] = memory();
for k = 1:N_ITER
  x = k;
end
[u1, s1] = memory();
for k = 1:N_ITER
  x = k;
end
[u2, s2] = memory();
%=============================================================================
delta_first = u1.MemUsedNelson - u0.MemUsedNelson;
delta_second = u2.MemUsedNelson - u1.MemUsedNelson;
disp(['First pass memory difference: ', num2str(delta_first), ' bytes']);
disp(['Second pass memory difference: ', num2str(delta_second), ' bytes']);
%=============================================================================
assert_istrue(delta_second < 1024 * 1024);
%=============================================================================
% A(:) = B used to replace the internal data pointer without releasing the
% previous value. Use two warm-up passes, then verify that another pass does not
% keep growing linearly.
%=============================================================================
A = zeros(10, 10);
B = ones(10, 10);
[u0, s0] = memory();
for k = 1:N_ITER
  A(:) = B;
end
[u1, s1] = memory();
for k = 1:N_ITER
  A(:) = B;
end
[u2, s2] = memory();
for k = 1:N_ITER
  A(:) = B;
end
[u3, s3] = memory();
%=============================================================================
delta_first = u1.MemUsedNelson - u0.MemUsedNelson;
delta_second = u2.MemUsedNelson - u1.MemUsedNelson;
delta_third = u3.MemUsedNelson - u2.MemUsedNelson;
disp(['A(:)=B first pass memory difference: ', num2str(delta_first), ' bytes']);
disp(['A(:)=B second pass memory difference: ', num2str(delta_second), ' bytes']);
disp(['A(:)=B third pass memory difference: ', num2str(delta_third), ' bytes']);
%=============================================================================
assert_istrue(delta_third < 1024 * 1024);
%=============================================================================
