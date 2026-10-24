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
% Bench: chained method dispatch on an expression result.
% A fluent chain (obj.m(a).m(b)) reuses the per-level SUBIDX_DOT dispatch; the
% only cost over two separate calls is keeping the intermediate object on the
% stack, so the chained form should track the stored-intermediate form closely.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
N = 20000;
%=============================================================================
tic();
for i = 1:N
  c = ClassdefFluentHandle(0);
  c.add(1);
  c.add(2);
end
t = toc();
asserts.isequal(c.total(), 3);
fprintf('  two separate calls:        %8.1f ns/iter\n', t / N * 1e9);
%=============================================================================
tic();
for i = 1:N
  c = ClassdefFluentHandle(0);
  c.add(1).add(2);
end
t = toc();
asserts.isequal(c.total(), 3);
fprintf('  chained obj.add(1).add(2): %8.1f ns/iter\n', t / N * 1e9);
%=============================================================================
tic();
for i = 1:N
  c = ClassdefFluentHandle(0);
  v = c.add(1).add(2).Value;
end
t = toc();
asserts.isequal(v, 3);
fprintf('  chain then .property:      %8.1f ns/iter\n', t / N * 1e9);
%=============================================================================
