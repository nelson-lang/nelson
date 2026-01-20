%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Bench: measure reduction fast-path vs fallback
N = 2e6;
fprintf('bench_for_reduction_fastpath: N=%d\n', int32(N));
%=============================================================================
% Ensure scalar accumulator exists (fast-path precondition)
s = 0;
tic();
for i = 1:N
  s = s + i;
end
t1 = toc();
assert_isequal(s, (N*(N+1))/2);
fprintf('  simple acc loop: %0.4f s\n', t1);
%=============================================================================
% Fallback: function-call in body
s = 0;
identity = @(x) x;
tic();
for i = 1:N
  s = s + identity(i);
end
t2 = toc();
assert_isequal(s, (N*(N+1))/2);
fprintf('  function-call loop: %0.4f s\n', t2);
fprintf('  ratio (func/simple) = %0.2f\n', t2 / t1);
%=============================================================================
