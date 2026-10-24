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
% Bench: classdef property read paths.
% - plain stored property (direct fast path in LOAD_DOT)
% - property with a declared get.X accessor (interpreted getter call)
% - struct field read as lower-bound baseline
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
R = 20000;
%=============================================================================
p = ClassdefBenchPoint(42);
s = ClassdefBenchSetter(42);
st = struct('Id', 42);
%=============================================================================
tic();
for i = 1:R
  y = p.Id;
end
t = toc();
asserts.isequal(y, 42);
fprintf('  plain property get:      %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  y = s.Id;
end
t = toc();
asserts.isequal(y, 42);
fprintf('  get.X accessor get:      %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  y = st.Id;
end
t = toc();
asserts.isequal(y, 42);
fprintf('  struct field baseline:   %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
rmpath(path_test);
%=============================================================================
