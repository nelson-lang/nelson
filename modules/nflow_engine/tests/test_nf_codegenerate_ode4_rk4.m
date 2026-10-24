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
% Unified variable-step code generation. When a model is
% integrated with the fixed-step RK4 solver (solver = 'ode4') and every stateful
% block is a plain unbounded scalar integrator, the C generator emits a global
% RK4 scheme: a pure rhs() function (scatter the packed state, evaluate the
% graph, gather each integrator's derivative) plus a 4-stage nflow_step. This
% matches the simulator's ode4 path instead of the per-block forward Euler that
% the historical (fixed-step / Euler) generator emits. Any other shape falls
% back to that byte-identical Euler emission.
%=============================================================================
function d = ramp_integrator_model(solver, minmax)
  ig = struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct());
  if nargin > 1 && ~isempty(minmax)
    ig.params = struct('LowerSaturationLimit', minmax(1), ...
      'UpperSaturationLimit', minmax(2));
  end
  d = struct();
  d.blocks = {...
    struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
      'params', struct('slope', 2)), ...
    ig, ...
    struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, ...
      'params', struct('label', 'y'))};
  d.connections = {...
    struct('from', 'r', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1;
  d.duration = 1.0;
  d.solver = solver;
  d.variables = struct();
end
%=============================================================================
function src = gen_c(d, stem)
  f = [tempdir(), stem, '.nflow'];
  fid = fopen(f, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  nflow_codegenerate(f, tempdir(), false, 'C');
  src = fileread([tempdir(), stem, filesep, 'model.c']);
end
%=============================================================================
% ode4 + a plain unbounded integrator: the RK4 scheme is emitted.
srcO = gen_c(ramp_integrator_model('ode4'), 'rk4_ode4_t');
assert_istrue(~isempty(strfind(srcO, '__nflow_rhs')));
% The pure rhs() scatters the packed state into the block, writes x' = u into
% the global xdot, and the step combines the four stages with the RK4 weights.
assert_istrue(~isempty(strfind(srcO, 's->int_ig = __x[0];')));
assert_istrue(~isempty(strfind(srcO, '__xdot[0] = out_r;')));
assert_istrue(~isempty(strfind(srcO, '(dt / 6.0) * (__k1[__i] + 2.0 * __k2[__i] + 2.0 * __k3[__i] + __k4[__i])')));
% The integrator output is read from the scattered global state, not fused.
assert_istrue(~isempty(strfind(srcO, 'out_ig = s->int_ig;')));
%=============================================================================
% ode4 with SEVERAL state-carrying families in one model: they pack into a
% single global state vector at distinct, contiguous offsets (integrator at 0,
% the 2nd-order tf at 1..2, lpf at 3). Exercises the multi-block offset layout.
dmix = struct();
dmix.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 2)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'g', 'type', 'tf', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Numerator', {{1}}, 'Denominator', {{1, 2, 1}})), ...
  struct('id', 'lp', 'type', 'lpf', 'inputs', 1, 'outputs', 1, 'params', struct('Cutoff', 1)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('label', 'y'))};
dmix.connections = {...
  struct('from', 'r', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'lp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'lp', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
dmix.sampleTime = 0.05;
dmix.duration = 1.0;
dmix.solver = 'ode4';
dmix.variables = struct();
srcMix = gen_c(dmix, 'rk4_mix_t');
assert_istrue(~isempty(strfind(srcMix, '__nflow_rhs')));
% Integrator packs at offset 0, the order-2 tf at 1..2, the lpf at 3.
assert_istrue(~isempty(strfind(srcMix, '__x0[0] = s->int_ig;')));
assert_istrue(~isempty(strfind(srcMix, '__x[1 + __i] = s->tf_g[__i]')) ...
  || ~isempty(strfind(srcMix, 's->tf_g[__i] = __x[1 + __i]')));
assert_istrue(~isempty(strfind(srcMix, '__x0[3] = s->lpf_x_lp;')));
% Four packed states: the RK arrays are sized [4].
assert_istrue(~isempty(strfind(srcMix, 'double __x0[4]')));
%=============================================================================
% Default solver (no ode4): the historical per-block Euler emission, no rhs().
srcD = gen_c(ramp_integrator_model('discrete'), 'rk4_default_t');
assert_isequal(isempty(strfind(srcD, '__nflow_rhs')), true);
% Fused forward-Euler state update.
assert_istrue(~isempty(strfind(srcD, 's->int_ig = fmin(fmax(s->int_ig +')));
%=============================================================================
% ode4 + a transfer function: its n companion states join the packed global
% state and it integrates through the shared RK4 rhs (tf_out / tf_deriv), not
% its own per-block local stepper (tf_step).
dtf = struct();
dtf.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 2)), ...
  struct('id', 'g', 'type', 'tf', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Numerator', {{1}}, 'Denominator', {{1, 1}})), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, ...
    'params', struct('label', 'y'))};
dtf.connections = {...
  struct('from', 'r', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
dtf.sampleTime = 0.1;
dtf.duration = 1.0;
dtf.solver = 'ode4';
dtf.variables = struct();
srcT = gen_c(dtf, 'rk4_tf_t');
assert_istrue(~isempty(strfind(srcT, '__nflow_rhs')));
% Output via tf_out (algebraic y = D*u + C.x) and derivative via tf_deriv into
% the global xdot; the local tf_step must NOT be called from the rhs body.
assert_istrue(~isempty(strfind(srcT, 'tf_out(&tf_params_g')));
assert_istrue(~isempty(strfind(srcT, 'tf_deriv(tf_params_g.n')));
%=============================================================================
% ode4 + a scalar SISO state-space block: x' = A*x + B*u and y = C*x + D*u join
% the RK4 rhs from the scattered scalar state.
dss = struct();
dss.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 2)), ...
  struct('id', 'ss', 'type', 'stateSpace', 'inputs', 1, 'outputs', 1, ...
    'params', struct('A', -2, 'B', 1, 'C', 1, 'D', 0)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, ...
    'params', struct('label', 'y'))};
dss.connections = {...
  struct('from', 'r', 'to', 'ss', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ss', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
dss.sampleTime = 0.1;
dss.duration = 1.0;
dss.solver = 'ode4';
dss.variables = struct();
srcS2 = gen_c(dss, 'rk4_ss_t');
assert_istrue(~isempty(strfind(srcS2, '__nflow_rhs')));
assert_istrue(~isempty(strfind(srcS2, 's->ss_x_ss = __x[0];')));
% A MULTI-STATE scalar-I/O state-space (nx>1, nu=ny=1) now joins the RK4 rhs
% too: it packs nx states and emits the A*x + B*u rows into __xdot.
dmisso = dss;
dmisso.blocks{2}.params = struct('A', {{-1, 0, 0, -2}}, 'B', {{1, 1}}, 'C', {{1, 0}}, 'D', 0);
srcMisso = gen_c(dmisso, 'rk4_ss_multi_t');
assert_istrue(~isempty(strfind(srcMisso, '__nflow_rhs')));
assert_istrue(~isempty(strfind(srcMisso, 's->ss_x_ss_0 = __x[0];')));
assert_istrue(~isempty(strfind(srcMisso, 's->ss_x_ss_1 = __x[1];')));
% A true VECTOR-I/O MIMO (vector output, ny>1) now joins the RK4 rhs as ONE
% multi-port block: nx packed states, one output variable per row
% (out_<id>, out_<id>_p1, ...), no marker, no Euler downgrade.
dmimo = dss;
dmimo.blocks{2}.params = struct('A', {{-1, 0, 0, -2}}, 'B', {{1, 1}}, ...
  'C', {{1, 0, 0, 1}}, 'D', {{0, 0}});
srcMimo = gen_c(dmimo, 'rk4_ss_mimo_t');
assert_istrue(isempty(strfind(srcMimo, 'not supported')));
assert_istrue(~isempty(strfind(srcMimo, '__nflow_rhs')));
assert_istrue(~isempty(strfind(srcMimo, 's->ss_x_ss_0 = __x[0];')));
assert_istrue(~isempty(strfind(srcMimo, 's->ss_x_ss_1 = __x[1];')));
assert_istrue(~isempty(strfind(srcMimo, 'out_ss_p1 = ')));
%=============================================================================
% ode4 + a first-order low-pass filter: one continuous state x' = wc*(u - x),
% output y = x, joins the RK4 rhs.
dlp = struct();
dlp.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 2)), ...
  struct('id', 'lp', 'type', 'lpf', 'inputs', 1, 'outputs', 1, 'params', struct('Cutoff', 1)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('label', 'y'))};
dlp.connections = {...
  struct('from', 'r', 'to', 'lp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'lp', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
dlp.sampleTime = 0.05;
dlp.duration = 1.0;
dlp.solver = 'ode4';
dlp.variables = struct();
srcLp = gen_c(dlp, 'rk4_lpf_t');
assert_istrue(~isempty(strfind(srcLp, '__nflow_rhs')));
assert_istrue(~isempty(strfind(srcLp, 'out_lp = s->lpf_x_lp;')));
%=============================================================================
% ode4 + a first-order high-pass filter: under the solver it is a single ODE
% state x' = wc*(u - x) with feedthrough output y = u - x (the fixed-step
% uprev latch is unused on the RK4 path), matching the simulator's ode4.
dhp = struct();
dhp.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 2)), ...
  struct('id', 'hp', 'type', 'hpf', 'inputs', 1, 'outputs', 1, 'params', struct('Cutoff', 1)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('label', 'y'))};
dhp.connections = {...
  struct('from', 'r', 'to', 'hp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'hp', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
dhp.sampleTime = 0.05;
dhp.duration = 1.0;
dhp.solver = 'ode4';
dhp.variables = struct();
srcHp = gen_c(dhp, 'rk4_hpf_t');
assert_istrue(~isempty(strfind(srcHp, '__nflow_rhs')));
% Feedthrough output y = u - x from the scattered state (no uprev latch).
assert_istrue(~isempty(strfind(srcHp, 'out_hp = out_r - s->hpf_x_hp;')));
%=============================================================================
% ode4 + a derivative block: it keeps state through the declarative `state`
% vector (der_prev), which is NOT a continuous ODE state; running its emitStep
% inside rhs() would mutate der_prev every sub-stage. The generator must detect
% this stateful block and fall back to the Euler emission (no rhs).
dder = struct();
dder.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 2)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'dv', 'type', 'derivative', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('label', 'y'))};
dder.connections = {...
  struct('from', 'r', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'dv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dv', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
dder.sampleTime = 0.1;
dder.duration = 1.0;
dder.solver = 'ode4';
dder.variables = struct();
srcDv = gen_c(dder, 'rk4_deriv_t');
assert_isequal(isempty(strfind(srcDv, '__nflow_rhs')), true);
%=============================================================================
% ode4 + a PID block: its integral is one continuous state that joins the RK4
% rhs (output = P + I from the scattered integral, integral' = input). The
% derivative term is a discrete finite difference with no continuous form, so it
% is dropped under variable-step -- matching the interpreter's OUTPUT phase
% (kp*u + ki*x). Numerically validated: the generated C compiled and run matches
% the ode4 simulation exactly (y = 3.0 for ramp -> integrator -> pid P=2,I=3).
dpid = dder;
dpid.blocks{3} = struct('id', 'dv', 'type', 'pid', 'inputs', 1, 'outputs', 1, ...
  'params', struct('P', 2, 'I', 3, 'D', 1));
srcPid = gen_c(dpid, 'rk4_pid_t');
assert_istrue(~isempty(strfind(srcPid, '__nflow_rhs')));
% Output is P + I from the scattered continuous integral; the derivative of the
% integral is the block input, packed into the global xdot.
assert_istrue(~isempty(strfind(srcPid, ...
  'pid_params_dv.kp * out_ig + pid_params_dv.ki * s->pid_dv.integ')));
assert_istrue(~isempty(strfind(srcPid, 's->pid_dv.integ = __x')));
%=============================================================================
% ode4 + a transport delay: a history buffer, not an ODE state, so it forces
% the Euler fallback too.
ddel = dder;
ddel.blocks{3} = struct('id', 'dv', 'type', 'delay', 'inputs', 1, 'outputs', 1, ...
  'params', struct('Delay', 0.2));
srcDel = gen_c(ddel, 'rk4_delay_t');
assert_isequal(isempty(strfind(srcDel, '__nflow_rhs')), true);
%=============================================================================
% ode4 + a LIMITED (saturated) integrator: it joins the RK4 rhs too, with a
% hold-at-limit derivative and a post-step clamp. Exact while within its limits;
% only the crossing instant differs from the simulator's zero-crossing by O(dt).
srcL = gen_c(ramp_integrator_model('ode4', [-1, 1]), 'rk4_limited_t');
assert_istrue(~isempty(strfind(srcL, '__nflow_rhs')));
% Output clamped, derivative held at the engaged limit, state clamped post-step.
assert_istrue(~isempty(strfind(srcL, 'out_ig = fmin(fmax(s->int_ig')));
assert_istrue(~isempty(strfind(srcL, 's->int_ig >= 1 && out_r > 0.0')));
%=============================================================================
% ode4 but with a discrete (stateful, non-integrator) block present: fall back.
dd = ramp_integrator_model('ode4');
dd.blocks{end + 1} = struct('id', 'ud', 'type', 'unitDelay', 'inputs', 1, ...
  'outputs', 1, 'params', struct('InitialCondition', 0));
dd.connections{end + 1} = struct('from', 'r', 'to', 'ud', 'fromIndex', 0, 'toIndex', 0);
srcDisc = gen_c(dd, 'rk4_discrete_t');
assert_isequal(isempty(strfind(srcDisc, '__nflow_rhs')), true);
%=============================================================================
% The Rust backend emits the same global RK4 scheme: a pure __nflow_rhs plus a
% 4-stage step over the packed state, with the model output written only on the
% reporting stage (__report).
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(ramp_integrator_model('ode4')));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(srcR, 'fn __nflow_rhs')));
assert_istrue(~isempty(strfind(srcR, 's.int_ig = __x[0];')));
assert_istrue(~isempty(strfind(srcR, 'if __report { out.y')));
% A default-solver Rust model keeps the per-block Euler (no rhs).
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(ramp_integrator_model('discrete')));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'rust');
srcRE = fileread([tempdir(), fstem, filesep, 'src', filesep, 'lib.rs']);
assert_isequal(isempty(strfind(srcRE, '__nflow_rhs')), true);
%=============================================================================
