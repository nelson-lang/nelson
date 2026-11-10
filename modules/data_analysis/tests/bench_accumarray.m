%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Timings of accumarray on 1e6 values: default sum per class, fill, 2-D subscripts, @max.
N = 1e6;
subs = mod((1:N)' * 7919, 1000) + 1;
subs2 = [mod((1:N)' * 7919, 300) + 1, mod((1:N)' * 104729, 300) + 1];
vals = mod((1:N)', 97) / 97;
vs = single(vals);
vi = int8(vals * 100);
nRuns = 5;
cases = {@() accumarray(subs, vals), @() accumarray(subs, vs), @() accumarray(subs, vi), ...
         @() accumarray(subs2, vals), @() accumarray(subs, vals, [1000 1], [], 5), ...
         @() accumarray(subs, vals, [], @max), @() accumarray(subs, vs, [], @max)};
names = {'sum double', 'sum single', 'sum int8', 'sum 2-D', 'sum fill', '@max double', '@max single'};
for k = 1:numel(cases)
  f = cases{k};
  f();
  t = inf;
  for r = 1:nRuns
    tic(); f(); t = min(t, toc());
  end
  fprintf('%-12s %8.2f ms\n', names{k}, t * 1e3);
end
%=============================================================================
