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
% Bench: per-call overhead of the arguments block
% (bare block, size/type specification, named validator).
path_test = [modulepath('interpreter', 'tests'), '/arguments'];
addpath(path_test);
%=============================================================================
R = 5000;
%=============================================================================
% warmup
y = arguments_bench_identity(1);
y = arguments_bench_bare(1);
y = arguments_bench_scalar_double(1);
y = arguments_bench_validator(1);
%=============================================================================
tic();
for i = 1:R
  y = arguments_bench_identity(i);
end
t = toc();
asserts.isequal(y, R);
fprintf('  no arguments block:   %0.1f ns/call\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  y = arguments_bench_bare(i);
end
t = toc();
asserts.isequal(y, R);
fprintf('  bare arguments block: %0.1f ns/call\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  y = arguments_bench_scalar_double(i);
end
t = toc();
asserts.isequal(y, R);
fprintf('  (1,1) double spec:    %0.1f ns/call\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  y = arguments_bench_validator(i);
end
t = toc();
asserts.isequal(y, R);
fprintf('  named validator:      %0.1f ns/call\n', t / R * 1e9);
%=============================================================================
% default value path
tic();
for i = 1:R
  y = arguments_bench_scalar_double();
end
t = toc();
asserts.isequal(y, 0);
fprintf('  default value used:   %0.1f ns/call\n', t / R * 1e9);
%=============================================================================
rmpath(path_test);
%=============================================================================
