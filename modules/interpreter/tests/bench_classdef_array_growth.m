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
% Bench: growing classdef object arrays.
% Hole filling reuses a cached value-class default object instead of
% running the default constructor on every expansion.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
N = 1000;
R_EXPAND = 500;
%=============================================================================
% element-wise growth
tic();
for i = 1:N
  w(i) = ClassdefBenchPoint(i); %#ok<SAGROW>
end
t = toc();
asserts.isequal(w(N).Id, N);
fprintf('  grow %d objects one by one: %0.4f s\n', int32(N), t);
%=============================================================================
% struct baseline: the classdef growth should stay close to it
clear sg
tic();
for i = 1:N
  sg(i).Id = i; %#ok<SAGROW>
end
tStruct = toc();
fprintf('  grow %d struct baseline:    %0.4f s\n', int32(N), tStruct);
%=============================================================================
% expansion with default-filled holes
tic();
for i = 1:R_EXPAND
  clear h
  h = ClassdefBenchPoint(1);
  h(50) = ClassdefBenchPoint(2);
end
t = toc();
asserts.isequal(h(50).Id, 2);
asserts.isequal(h(25).Id, 0);
fprintf('  expand 1 -> 50 with holes:    %0.1f ns/op\n', t / R_EXPAND * 1e9);
%=============================================================================
rmpath(path_test);
%=============================================================================
