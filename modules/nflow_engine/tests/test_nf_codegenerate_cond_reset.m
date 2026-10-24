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
% <--WEIGHT 64-->
% <--TIMEOUT 600-->
%=============================================================================
% Reset gate (V2b): the subsystem ALWAYS runs; on the reset edge its member
% states re-seed to their initial values before that step's run. The inner
% unitDelay accumulates the constant 1 (via a feedback-free chain: delay of
% gain 2 of the sine); cleanest observable: the unitDelay state snaps back
% to InitialCondition 0 on the edge at t = 0.4, so y drops to 2*sine(0.3)'s
% replacement... simpler: y(t=0.4) equals 0 exactly ONLY if reset preceded
% the run, which it does NOT for the delay chain; instead assert sim/gen
% equivalence sample for sample to cover the exact ordering.
%=============================================================================
dr = nflow_cond_enable_base_diagram('nfcr_y');
dr.blocks{4}.params.controlPort = struct('kind', 'reset', 'resetTriggerType', 'rising');
dr.blocks{4}.params.subsystem.controlPort = struct('kind', 'reset');
dr.blocks{3}.params = struct('relop', 'ge', 'const', 0.35);
%=============================================================================
r3 = jsondecode(__nflow_simulate__(jsonencode(dr)));
assert_isequal(r3.status, 'done');
yr = evalin('base', 'nfcr_y');
assert_istrue(all(isfinite(yr)));
assert_istrue(abs(yr(3)) > 1e-6); % always running (unlike enable/trigger off)
evalin('base', 'clear(''nfcr_y'')');
%=============================================================================
fr = [tempname(), '.nflow'];
[~, frstem] = fileparts(fr);
fid = fopen(fr, 'wt');
fwrite(fid, jsonencode(dr));
fclose(fid);
nflow_codegenerate(fr, tempdir(), false, 'C');
srcRst = fileread([tempdir(), frstem, '/model.c']);
assert_istrue(isempty(strfind(srcRst, 'unknown:')));
% Members are NOT guard-wrapped for reset (always running); the only guard on
% the gate output is the re-seed block that snaps states back to their inits.
assert_isequal(numel(strfind(srcRst, 'if (out_sub__nfg != 0.0) {')), 1);
assert_istrue(~isempty(strfind(srcRst, '    s->unit_delay_'))); % re-seed, 4-space indent
rmfile(fr);
%=============================================================================
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dr, 'nfcr_num', 'C', {'y'}, {'nfcr_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(dr, 'nfcr_num', 'rust', {'y'}, {'nfcr_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
