%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% sim() forwards the model's / call's Solver (and tolerances) to the engine.
% Previously sim() always ran the default fixed-step engine, ignoring Solver.
%=============================================================================
examples_dir = [modulepath('nflow_blocks'), '/examples'];
model = [examples_dir, '/workspace/To_Workspace_Demo.nflow'];

% Default (no solver) keeps the fixed-step path and runs fine.
out = sim(model);
asserts.istrue(isstruct(out));
asserts.istrue(isfield(out, 'tout'));

% A continuous solver is now forwarded. This model has no continuous state, so
% the engine rejects a VARIABLE-step solver with a clear message, which proves
% the Solver option reached the engine instead of being silently dropped.
% ode1 / ode4 no longer serve as that proof: with nothing to integrate they are
% the fixed-step loop to the bit, so they run rather than refuse.
msg = '';
try
  sim(model, 'Solver', 'ode45');
catch e
  msg = lower(e.message);
end
asserts.istrue(~isempty(msg));
asserts.istrue(~isempty(strfind(msg, 'continuous')) || ~isempty(strfind(msg, 'solver')));

% 'discrete' is treated as no solver (keeps the fixed-step path) and runs fine.
out2 = sim(model, 'Solver', 'discrete');
asserts.istrue(isstruct(out2));

% A malformed Solver value is rejected at the argument level.
bad = '';
try
  sim(model, 'Solver', 42);
catch e
  bad = e.identifier;
end
asserts.isequal(bad, 'Nelson:nflow_engine:simArgs:solverAString');
%=============================================================================
