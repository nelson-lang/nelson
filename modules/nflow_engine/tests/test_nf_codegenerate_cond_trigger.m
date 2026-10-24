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
% Trigger gate (V2): fires ONLY on the step where the control crosses zero in
% the configured direction, measured against the value latched at the END of
% the previous step (no fire before the first latch, so never at t0). Model:
% control = (t >= 0.35) rises once at t = 0.4; the subsystem samples the
% sine exactly there and holds it before and after.
%=============================================================================
d = nflow_cond_enable_base_diagram('nfct_y');
inner2 = struct();
inner2.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
inner2.connections = {...
  struct('from', 'in1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner2.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner2.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
inner2.controlPort = struct('kind', 'trigger');
dt2 = d;
dt2.blocks{4}.params.controlPort = struct('kind', 'trigger', 'triggerType', 'rising');
dt2.blocks{4}.params.subsystem = inner2;
dt2.blocks{3}.params = struct('relop', 'ge', 'const', 0.35);
%=============================================================================
r2 = jsondecode(__nflow_simulate__(jsonencode(dt2)));
assert_isequal(r2.status, 'done');
yt = evalin('base', 'nfct_y');
assert_istrue(all(abs(yt(1:4)) < 1e-12)); % held at init 0 before the edge
assert_istrue(abs(yt(5)) > 1e-3); % fired once at t = 0.4
assert_istrue(all(abs(yt(6:end) - yt(5)) < 1e-12)); % held after
evalin('base', 'clear(''nfct_y'')');
%=============================================================================
ft = [tempname(), '.nflow'];
[~, ftstem] = fileparts(ft);
fid = fopen(ft, 'wt');
fwrite(fid, jsonencode(dt2));
fclose(fid);
nflow_codegenerate(ft, tempdir(), false, 'C');
srcT = fileread([tempdir(), ftstem, '/model.c']);
assert_istrue(isempty(strfind(srcT, 'unknown:')));
assert_istrue(~isempty(strfind(srcT, 'nfgp_sub__nfg'))); % edge memory
assert_istrue(~isempty(strfind(srcT, 'nfgh_sub__nfg')));
rmfile(ft);
%=============================================================================
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dt2, 'nfct_num', 'C', {'y'}, {'nfct_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(dt2, 'nfct_num', 'rust', {'y'}, {'nfct_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
