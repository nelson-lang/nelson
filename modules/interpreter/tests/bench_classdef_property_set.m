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
% Bench: classdef property assignment paths.
% - plain stored property (direct fast path, no path-indexer probe for
%   undeclared set.X accessors)
% - property with a size/type validation (memoized validation parse)
% - property with a declared set.X accessor (interpreted setter call)
% - struct field assignment as lower-bound baseline
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
R = 20000;
%=============================================================================
p = ClassdefBenchPoint(1);
v = ClassdefBenchValidated(1);
s = ClassdefBenchSetter(1);
st = struct('Id', 1);
%=============================================================================
tic();
for i = 1:R
  p.Id = i;
end
t = toc();
asserts.isequal(p.Id, R);
fprintf('  plain property set:      %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  v.Id = i;
end
t = toc();
asserts.isequal(v.Id, R);
fprintf('  validated property set:  %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  s.Id = i;
end
t = toc();
asserts.isequal(s.Id, R);
fprintf('  set.X accessor set:      %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
tic();
for i = 1:R
  st.Id = i;
end
t = toc();
asserts.isequal(st.Id, R);
fprintf('  struct field baseline:   %0.1f ns/op\n', t / R * 1e9);
%=============================================================================
% validation must still reject wrong values through the fast path
ok = false;
try
  v.Id = [1, 2];
catch
  ok = true;
end
asserts.istrue(ok);
%=============================================================================
rmpath(path_test);
%=============================================================================
