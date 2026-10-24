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
% Bench: classdef method dispatch.
% Method resolution (mangled overload name, registry walk, function lookup)
% is memoized against the classdef and function-table generations, so a call
% costs close to a plain macro call instead of re-resolving every time.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
N = 20000;
p = ClassdefBenchPoint(7);
q = ClassdefBenchPoint(35);
%=============================================================================
tic();
for i = 1:N
  id = getId(p);
end
t = toc();
asserts.isequal(id, 7);
fprintf('  method call getId(obj):    %8.1f ns/call\n', t / N * 1e9);
%=============================================================================
tic();
for i = 1:N
  id = p.getId();
end
t = toc();
asserts.isequal(id, 7);
fprintf('  method call obj.getId():   %8.1f ns/call\n', t / N * 1e9);
%=============================================================================
tic();
for i = 1:N
  v = addTo(p, i);
end
t = toc();
asserts.isequal(v, 7 + N);
fprintf('  method call addTo(obj, k): %8.1f ns/call\n', t / N * 1e9);
%=============================================================================
tic();
for i = 1:N
  r = p + q;
end
t = toc();
asserts.isequal(r.Id, 42);
fprintf('  overloaded plus:           %8.1f ns/call\n', t / N * 1e9);
%=============================================================================
