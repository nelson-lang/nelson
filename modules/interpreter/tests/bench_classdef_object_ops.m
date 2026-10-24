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
% Bench: core classdef object operations (create, extract, swap,
% overloaded plus, property get/set) on a small value class.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
N_CREATE = 1000;
R_FAST = 20000;
R_SWAP = 2000;
%=============================================================================
% warmup
a = ClassdefBenchPoint(1);
b = ClassdefBenchPoint(2);
c = a + b;
asserts.isequal(c.Id, 3);
%=============================================================================
% create N objects in a growing array
tic();
for i = 1:N_CREATE
  v(i) = ClassdefBenchPoint(i); %#ok<SAGROW>
end
t = toc();
asserts.isequal(v(N_CREATE).Id, N_CREATE);
fprintf('  create %d objects:  %0.4f s\n', int32(N_CREATE), t);
%=============================================================================
% extract one object from the array
tic();
for i = 1:R_FAST
  x = v(500);
end
t = toc();
asserts.isequal(x.Id, 500);
fprintf('  extract object:     %0.1f ns/op\n', t / R_FAST * 1e9);
%=============================================================================
% swap two objects inside the array
tic();
for i = 1:R_SWAP
  tmp = v(1);
  v(1) = v(2);
  v(2) = tmp;
end
t = toc();
asserts.isequal(v(1).Id, 1);
asserts.isequal(v(2).Id, 2);
fprintf('  swap two objects:   %0.1f ns/op\n', t / R_SWAP * 1e9);
%=============================================================================
% overloaded plus on scalar objects
tic();
for i = 1:R_FAST
  c = a + b;
end
t = toc();
asserts.isequal(c.Id, 3);
fprintf('  overloaded plus:    %0.1f ns/op\n', t / R_FAST * 1e9);
%=============================================================================
% property read
tic();
for i = 1:R_FAST
  y = a.Id;
end
t = toc();
asserts.isequal(y, 1);
fprintf('  property get:       %0.1f ns/op\n', t / R_FAST * 1e9);
%=============================================================================
% property write
tic();
for i = 1:R_FAST
  a.Id = i;
end
t = toc();
asserts.isequal(a.Id, R_FAST);
fprintf('  property set:       %0.1f ns/op\n', t / R_FAST * 1e9);
%=============================================================================
rmpath(path_test);
%=============================================================================
