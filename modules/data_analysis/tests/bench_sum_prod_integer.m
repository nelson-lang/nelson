%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Timings of integer sum/prod: default (double) output and 'native' output.
N = 4e7;
classes = {'int8', 'int32', 'uint16', 'int64', 'double', 'single'};
nRuns = 5;
for c = 1:numel(classes)
  cls = classes{c};
  V = cast(mod(1:N, 7) - 3, cls);
  M = reshape(V, 4000, []);
  P = cast(1 + mod(1:N, 2), cls);
  cases = {@() sum(V), @() sum(M), @() sum(M, 2), @() sum(V, 'native'), ...
           @() sum(M, 'native'), @() sum(M, 2, 'native'), @() sum(M, [1 2]), @() prod(M), ...
           @() prod(P, 'native')};
  names = {'sum(V)', 'sum(M)', 'sum(M,2)', 'sum(V,native)', ...
           'sum(M,native)', 'sum(M,2,native)', 'sum(M,[1 2])', 'prod(M)', 'prod(P,native)'};
  for k = 1:numel(cases)
    f = cases{k};
    f();
    t = inf;
    for r = 1:nRuns
      tic(); f(); t = min(t, toc());
    end
    fprintf('%-7s %-16s %8.2f ms\n', cls, names{k}, t * 1000);
  end
end
%=============================================================================
