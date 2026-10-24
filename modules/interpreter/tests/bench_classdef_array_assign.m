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
% Bench: in-bounds indexed assignment into a classdef object array.
% Must not rebuild the whole array (default constructor + full copy);
% the cost has to stay independent of the array length.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
N = 1000;
R = 2000;
%=============================================================================
w = ClassdefBenchPoint(0);
for i = 2:N
  w(i) = ClassdefBenchPoint(i); %#ok<SAGROW>
end
small = ClassdefBenchPoint(0);
for i = 2:10
  small(i) = ClassdefBenchPoint(i); %#ok<SAGROW>
end
obj = ClassdefBenchPoint(-1);
%=============================================================================
tic();
for i = 1:R
  small(5) = obj;
end
tSmall = toc();
asserts.isequal(small(5).Id, -1);
fprintf('  assign in 10-element array:    %0.1f ns/op\n', tSmall / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  w(500) = obj;
end
tLarge = toc();
asserts.isequal(w(500).Id, -1);
fprintf('  assign in %d-element array:  %0.1f ns/op\n', int32(N), tLarge / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  tmp = w(1);
  w(1) = w(2);
  w(2) = tmp;
end
t = toc();
fprintf('  swap two elements:             %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
rmpath(path_test);
%=============================================================================
