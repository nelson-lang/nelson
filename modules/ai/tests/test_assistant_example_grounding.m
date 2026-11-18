%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Example-grounding helpers: the NFlow example matcher, the Nelson demo matcher,
% and the accent fold they share. These read catalog data shipped in the binary,
% so each block is guarded on the relevant files being present.
%=============================================================================
privateDir = [modulepath('ai', 'root'), '/functions/+nelson/+ai/+internal/private'];
addpath(privateDir);
cleaner = onCleanup(@() rmpath(privateDir)); %#ok<NASGU>
%=============================================================================
% accent fold: 'REPONSE' with an accented E (code 201) folds to ascii 'reponse'
assert_isequal(nelson.ai.internal.aiFoldAccents(char([82, 201, 80, 79, 78, 83, 69])), 'reponse');
assert_isequal(nelson.ai.internal.aiFoldAccents('PLOT'), 'plot');
%=============================================================================
% NFlow example grounding (needs the shipped .nflow examples)
nflowRoot = [modulepath('nflow_blocks', 'root'), '/examples'];
if isfolder(nflowRoot)
  % a clear French request reaches the English-named example via the synonym map
  r = aiNflowExampleContext('--- Request: construire un systeme masse ressort amortisseur', 'nflow');
  assert_istrue(~isempty(r));
  assert_istrue(contains(r, 'Closest shipped NFlow example'));
  assert_istrue(contains(lower(r), 'spring') || contains(lower(r), 'mass'));
  assert_istrue(contains(r, 'solver:'));
  % only the NFlow surface gets it
  assert_istrue(isempty(aiNflowExampleContext('masse ressort', 'web')));
  % a vague request matches nothing (no misleading example)
  assert_istrue(isempty(aiNflowExampleContext('--- Request: fais quelque chose', 'nflow')));
end
%=============================================================================
% Nelson demo grounding (needs at least one shipped examples/index.json)
if isfile([modulepath('control_system', 'root'), '/examples/index.json'])
  d = aiNelsonDemoContext('plot the frequency response gain and phase', 'web');
  assert_istrue(~isempty(d));
  assert_istrue(contains(d, 'runnable example'));
  % cross-language: an unaccented French query matches the shipped fr_FR metadata
  % (the index "reponse indicielle" is folded the same way) even in an en_US run
  d2 = aiNelsonDemoContext('tracer la reponse indicielle du systeme du second ordre', 'web');
  assert_istrue(~isempty(d2));
  % the NFlow surface has its own example grounding, not this one
  assert_istrue(isempty(aiNelsonDemoContext('frequency response', 'nflow')));
  % a greeting matches nothing
  assert_istrue(isempty(aiNelsonDemoContext('bonjour', 'web')));
end
%=============================================================================
% Live session context: reports the base workspace (names/sizes/classes, never
% values) and the last error, so the model answers about the running session.
assignin('base', 'aiCtxMatrix', magic(3));
assignin('base', 'aiCtxText', 'hello');
try
  error('Nelson:test:ctxBoom', 'context boom message');
catch
end
ctx = aiLiveSessionContext('qt');
assert_istrue(contains(ctx, 'aiCtxMatrix'));
assert_istrue(contains(ctx, '3x3'));
assert_istrue(contains(ctx, 'double'));
assert_istrue(contains(ctx, 'aiCtxText'));
% the value itself is never leaked, only the shape/class
assert_isfalse(contains(ctx, 'hello'));
assert_istrue(contains(ctx, 'context boom message'));
evalin('base', 'clear aiCtxMatrix aiCtxText');
%=============================================================================
