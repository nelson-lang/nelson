%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: cellfun/arrayfun with a capture-free anonymous callback.
% Such callbacks run scope-free: the callback bytecode is resolved once and
% executed directly per element, skipping the per-call scope push and the
% AnonymousMacroFunctionDef lock/recheck round-trip.
%=============================================================================
N = 10000;
nums = num2cell(1:N);
%=============================================================================
% correctness first: fast path must match the interpreted result
asserts.isequal(cellfun(@(v) v + 1, nums), 2:(N + 1));
asserts.isequal(arrayfun(@(v) v * 2, 1:N), 2:2:(2 * N));
% a captured variable must fall back to the scoped path and stay correct
k = 7;
asserts.isequal(arrayfun(@(v) v + k, 1:5), (1:5) + k);
% non-uniform output and error handler stay correct
c = cellfun(@(v) v * [1 1], num2cell(1:4), 'UniformOutput', false);
asserts.isequal(c{4}, [4 4]);
%=============================================================================
cellfun(@(v) v + 1, nums); % warmup
tic();
for r = 1:5
  out = cellfun(@(v) v + 1, nums);
end
t = toc();
asserts.isequal(out(end), N + 1);
fprintf('  cellfun anon 10k:  %0.2f us/elem\n', t / 5 / N * 1e6);
%=============================================================================
arrayfun(@(v) v * 2, 1:N); % warmup
tic();
for r = 1:5
  out2 = arrayfun(@(v) v * 2, 1:N);
end
t = toc();
asserts.isequal(out2(end), 2 * N);
fprintf('  arrayfun anon 10k: %0.2f us/elem\n', t / 5 / N * 1e6);
%=============================================================================
% guard against a regression to the per-call scope-push cost
asserts.istrue(t / 5 / N < 12e-6);
%=============================================================================
