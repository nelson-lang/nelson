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
if iswasm()
  skip_testsuite('native classdef handle performance thresholds are not stable in WebAssembly yet.');
end
%=============================================================================
% Bench: classdef handle object hot paths.
% - plain stored property get/set skip the generated get/set wrapper call
% - mutating method calls stay on the memoized dispatch path
% - object creation must stay O(1) per object: the handle registry indexes
%   ids and native pointers, a linear registry scan made creation O(n)
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
R = 20000;
%=============================================================================
h = ClassdefBenchHandle(1);
%=============================================================================
tic();
for i = 1:R
  h.Id = i;
end
t = toc();
asserts.isequal(h.Id, R);
fprintf('  handle property set:     %0.1f ns/op\n', t / R * 1e9);
asserts.istrue(t / R < 8e-6);
%=============================================================================
acc = 0;
tic();
for i = 1:R
  acc = acc + h.Id;
end
t = toc();
asserts.isequal(acc, R * R);
fprintf('  handle property get:     %0.1f ns/op\n', t / R * 1e9);
asserts.istrue(t / R < 8e-6);
%=============================================================================
h.Id = 0;
tic();
for i = 1:R
  bump(h);
end
t = toc();
asserts.isequal(h.Id, R);
fprintf('  handle mutating method:  %0.1f ns/op\n', t / R * 1e9);
asserts.istrue(t / R < 15e-6);
%=============================================================================
N = 1000;
hv = cell(1, N);
tic();
for i = 1:N
  hv{i} = ClassdefBenchHandle(i);
end
t = toc();
asserts.isequal(hv{N}.Id, N);
fprintf('  handle creation (kept):  %0.1f ns/op\n', t / N * 1e9);
% creation must not degrade with the number of live handles
asserts.istrue(t / N < 80e-6);
%=============================================================================
% validated handle property still rejects wrong values through the fast path
ok = false;
try
  h.Checked = [1, 2];
catch
  ok = true;
end
asserts.istrue(ok);
h.Checked = 42;
asserts.isequal(h.Checked, 42);
%=============================================================================
% private property stays unreachable from outside
ok = false;
try
  x = h.Secret;
catch
  ok = true;
end
asserts.istrue(ok);
%=============================================================================
clear hv h
rmpath(path_test);
%=============================================================================
