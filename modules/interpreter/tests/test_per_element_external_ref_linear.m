%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: calling a function per element in a loop, when that function's
% body references an external (loadable) builtin, must not force the whole
% caller workspace to be co-owned by a context-scope snapshot. When it did,
% an in-place indexed assignment to a growing variable (out{k} = ...) in the
% same loop deep-copied the entire array on every call, turning an O(n) loop
% into O(n^2). The dead 'regexp' reference below is never executed but is
% enough to mark the helper as caller-context touching; the loop must still
% stay linear. See seedAssignBaseFromContext / syncAssignedLocalsToContext.
%=============================================================================
N = 8000;
%=============================================================================
% Baseline: identical loop whose helper references no external function. This
% is inherently linear, so it tracks the machine speed and makes the bound
% below adaptive instead of a brittle absolute wall-clock threshold.
best_core = min(timeCoreLoop(N), timeCoreLoop(N));
%=============================================================================
% The pattern under test: the helper carries a dead external reference.
best_ext = min(timeExtLoop(N), timeExtLoop(N));
%=============================================================================
% Linear: ext/core stays a small constant. Before the fix this ratio was in
% the hundreds (seconds vs milliseconds); 20x leaves a wide margin for noise
% while a reintroduced O(n^2) (two orders of magnitude) fails decisively.
% The additive floor absorbs timer jitter when both measurements are tiny.
ratio_bound = best_core * 20 + 0.5;
disp(['per-element external-ref loop: ext = ', num2str(best_ext), ' s, core = ', ...
  num2str(best_core), ' s, bound = ', num2str(ratio_bound), ' s']);
asserts.istrue(best_ext < ratio_bound);
%=============================================================================
function elapsed = timeExtLoop(N)
  items = repmat({'foobar'}, N, 1);
  n = numel(items);
  tic();
  out = cell(n, 1);
  for k = 1:n
    out{k} = withExtRef(items{k});
  end
  elapsed = toc();
  asserts.isequal(numel(out), n);
  asserts.isequal(out{1}, 'foo');
end
%=============================================================================
function elapsed = timeCoreLoop(N)
  items = repmat({'foobar'}, N, 1);
  n = numel(items);
  tic();
  out = cell(n, 1);
  for k = 1:n
    out{k} = noExtRef(items{k});
  end
  elapsed = toc();
  asserts.isequal(numel(out), n);
end
%=============================================================================
function v = withExtRef(txt)
  if true
    v = txt(1:3);
    return;
  end
  v = regexp(txt, 'x'); % dead branch: never executed, reference present
end
%=============================================================================
function v = noExtRef(txt)
  if true
    v = txt(1:3);
    return;
  end
  v = txt; % dead branch, no external reference
end
%=============================================================================
