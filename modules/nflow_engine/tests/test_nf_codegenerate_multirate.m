%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Code generation honors a per-block SampleTime on a unitDelay:
% the generated step function advances the block's stored state only on its
% sample hit, matching the simulator's zero-order-hold sub-rate. The generators
% emit a major-step index (__nflow_step) and guard the state update with
% "__nflow_step % rate == 0". A single-rate model emits no such guard.
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'ud', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0, 'SampleTime', 0.2)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {...
  struct('from', 'c', 'to', 'ud', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ud', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
%=============================================================================
% C backend: the state advance is gated on the 2x rate.
stem = 'mr_ud_c';
f = [tempdir(), stem, '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
srcC = fileread([tempdir(), stem, filesep, 'model.c']);
assert_istrue(~isempty(strfind(srcC, '__nflow_step')));
assert_istrue(~isempty(strfind(srcC, '% 2 == 0')));
%=============================================================================
% Rust backend: same gating.
stem = 'mr_ud_rust';
f = [tempdir(), stem, '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), stem, filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(srcR, '__nflow_step')));
assert_istrue(~isempty(strfind(srcR, '% 2 == 0')));
%=============================================================================
% A single-rate unit delay (no SampleTime) emits no rate guard.
d2 = d;
d2.blocks{2}.params = struct('InitialCondition', 0);
stem = 'mr_ud_single';
f = [tempdir(), stem, '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d2));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
srcS = fileread([tempdir(), stem, filesep, 'model.c']);
assert_istrue(isempty(strfind(srcS, '% 2 == 0')));
%=============================================================================
% The same honoring applies to another state-holding block, difference, through
% the shared codegenSampleRate helper: a 2x difference gates its stored-input
% advance on the major-step index.
dd = struct();
dd.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 1)), ...
  struct('id', 'df', 'type', 'difference', 'inputs', 1, 'outputs', 1, ...
    'params', struct('SampleTime', 0.2)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
dd.connections = {...
  struct('from', 'r', 'to', 'df', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'df', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
dd.sampleTime = 0.1;
dd.duration = 1.0;
dd.variables = struct();
stem = 'mr_diff_c';
f = [tempdir(), stem, '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(dd));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
srcD = fileread([tempdir(), stem, filesep, 'model.c']);
assert_istrue(~isempty(strfind(srcD, '__nflow_step')));
assert_istrue(~isempty(strfind(srcD, '% 2 == 0')));
%=============================================================================
% A block with its OWN sample time (Ts), e.g. zoh, is a DIFFERENT mechanism: it
% self-schedules in the generated code via a time-based next-sample marker
% (zoh_next), independent of the base step -- so it is not rejected and needs no
% __nflow_step %% rate guard. This confirms the codegen multi-rate is already
% complete for the Ts-based discrete blocks (zoh/ddelay/dtf/dstateSpace/foh).
zz = struct();
zz.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'z', 'type', 'zoh', 'inputs', 1, 'outputs', 1, 'params', struct('Ts', 0.3)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
zz.connections = {...
  struct('from', 'c', 'to', 'z', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'z', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
zz.sampleTime = 0.1;
zz.duration = 1.0;
zz.variables = struct();
stem = 'mr_zoh_c';
f = [tempdir(), stem, '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(zz));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C'); % must NOT reject
srcZ = fileread([tempdir(), stem, filesep, 'model.c']);
assert_istrue(~isempty(strfind(srcZ, 'zoh_next'))); % time-based self-scheduling
assert_istrue(isempty(strfind(srcZ, '__nflow_step'))); % not the base-step gate
%=============================================================================
% An edge detector with a slower SampleTime: its emitter gates on the major
% step through codegenSampleRate, so the multi-rate gate must accept it (it
% used to over-reject every type outside unitDelay/difference).
re = struct();
re.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'e', 'type', 'risingEdge', 'inputs', 1, 'outputs', 1, ...
    'params', struct('SampleTime', 0.2)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
re.connections = {...
  struct('from', 'c', 'to', 'e', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'e', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
re.sampleTime = 0.1;
re.duration = 1.0;
re.variables = struct();
stem = 'mr_redge_c';
f = [tempdir(), stem, '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(re));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C'); % must NOT reject
srcE = fileread([tempdir(), stem, filesep, 'model.c']);
assert_istrue(~isempty(strfind(srcE, '__nflow_step'))); % major-step gating emitted
nflow_codegenerate(f, tempdir(), false, 'rust');
srcER = fileread([tempdir(), stem, filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(srcER, '__nflow_step')));
%=============================================================================
% ode4 + multirate: the simulator honors BOTH (RK4 backend + per-block
% update rates); generated code demotes to fused Euler. That divergence must
% surface as a solverDowngrade warning (fatal under failOnWarning), never
% silently.
re4 = re;
re4.solver = 'ode4';
f4 = [tempname(), '.nflow'];
fid = fopen(f4, 'wt');
fwrite(fid, jsonencode(re4));
fclose(fid);
ok = false;
try
  nflow_codegenerate(f4, tempdir(), false, 'C', struct('failOnWarning', true));
catch e
  ok = ~isempty(strfind(e.message, 'multi-rate'));
end
assert_istrue(ok);
nflow_codegenerate(f4, tempdir(), false, 'C'); % without the option: succeeds
rmfile(f4);
%=============================================================================
