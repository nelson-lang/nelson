%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% Simulation-progress plan (P1/P2/P3): wall-clock publish cadence and O(N)
% incremental serialization, measured through the opt-in progressStats
% instrumentation ('profileProgress' diagram flag, no production logging).
%=============================================================================
% A small feedback-free diagram with one scope.
d = struct();
d.blocks = {struct('id', 'src', 'type', 'sine', 'params', struct()), ...
            struct('id', 'sc', 'type', 'scope', 'params', struct())};
d.connections = {struct('from', 'src', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0)};
d.duration = 10;
d.dt = 0.001;
%=============================================================================
% 1) Default cadence: progress work stays bounded by the simulation steps.
d.profileProgress = true;
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert(strcmp(r.status, 'done'));
s = r.progressStats;
assert(s.steps > 0);
assert(s.publishesSent <= s.publishChecks);
assert(s.samplesSerialized <= s.steps);
%=============================================================================
% 2) Real-time pacing makes wall-clock publication deterministic. Interim
% frames flow, and the O(N) invariant holds;
% each sample is serialized AT MOST ONCE across all partial frames, so the
% cumulative partial volume stays linear in the final result size.
d2 = d;
d2.duration = 0.25;
d2.realTime = true;
d2.progressPeriodMs = 50;
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert(strcmp(r2.status, 'done'));
s2 = r2.progressStats;
assert(s2.publishesSent >= 1);
assert(s2.samplesSerialized <= s2.steps);
assert(s2.partialBytes < 3 * s2.finalBytes);
%=============================================================================
% 3) The final result must be byte-identical with and without progress
% profiling (progress reporting must never alter the numerical results).
d3 = d2;
d3 = rmfield(d3, 'profileProgress');
d3 = rmfield(d3, 'progressPeriodMs');
rref = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert(strcmp(rref.status, 'done'));
assert(isequal(rref.scopes(1).times, r2.scopes(1).times));
assert(isequal(rref.scopes(1).series, r2.scopes(1).series));
%=============================================================================
