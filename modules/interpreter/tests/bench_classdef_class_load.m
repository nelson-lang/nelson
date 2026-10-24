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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Bench: classdef class LOADING (parse + generation + rewrite + re-parse).
%
% This is the cost the native front-end migration targets: today loading a
% class runs the text scanner, rewrites each method source, renames accessors
% and re-parses every generated function. It is paid once per class per
% session (cleared by clear('classes')). This bench forces a reload each
% iteration to measure that one-shot cost -- the migration gate for P1-P3 is
% "load time does not regress" (ideally improves).
%
% Baseline (pre-migration, 2026-08-11, x64 Release): class load cold full
% pipeline ~3400 us/load; warm instantiate ~31 us/inst. Absolute values are
% machine-dependent; the gate is relative to a run on the same machine.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
N = 200;
% warm up once so path indexing is not counted
o = ClassdefMigLoadBench(1);
asserts.isequal(o.A, 1);
clear('classes');
%=============================================================================
tic();
for i = 1:N
  o = ClassdefMigLoadBench(i);
  clear('classes');
end
t = toc();
fprintf('  class load (cold, full pipeline): %8.1f us/load\n', t / N * 1e6);
%=============================================================================
% Warm instantiation cost (class already loaded) for contrast.
o = ClassdefMigLoadBench(1);
M = 20000;
tic();
for i = 1:M
  o = ClassdefMigLoadBench(i);
end
t = toc();
asserts.isequal(o.A, M);
fprintf('  instantiate (warm):               %8.1f ns/inst\n', t / M * 1e9);
%=============================================================================
