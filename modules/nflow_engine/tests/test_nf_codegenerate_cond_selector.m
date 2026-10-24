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
% The action-selector shape, generated: a selector (if or switchCase) drives
% one action subsystem at a time and a Merge recombines them. All three blocks
% were refused outright, on the grounds that a selector has no straight-line
% lowering. It has:
%
%   - an `if` clause is a small grammar over u1..un. It is EVALUATED at
%     simulation time and TRANSLATED here, by a walk over the same productions
%     in the same order, into an expression worth 1.0 / 0.0. A clause the
%     parser refuses becomes 0.0, which is what the evaluator answers for it.
%   - a switchCase is a chain of comparisons against the truncated control.
%   - a Merge takes the input whose SOURCE ran this step, which in generated
%     code is the input whose gate is open; it reads that gate off the name of
%     the boundary hold feeding it, and holds its own last value otherwise.
%
% Each shape is compared with the simulation sample by sample, in C and in Rust.
%=============================================================================
function s = sel_arm(gainValue)
  s = struct();
  s.blocks = {...
    struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
      'params', struct('label', 'in1')), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Gain', gainValue)), ...
    struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('label', 'out1'))};
  s.connections = {...
    struct('from', 'in1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
  s.externalInputs = {struct('id', 'in1', 'name', 'in1')};
  s.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
  s.controlPort = struct('kind', 'action');
end
%=============================================================================
function d = sel_doc(varName, selector, gains)
  arms = {};
  conns = {struct('from', 'u', 'to', 'f', 'fromIndex', 0, 'toIndex', 0)};
  for k = 1:numel(gains)
    id = ['a', int2str(k)];
    arms{end + 1} = struct('id', id, 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
      'params', struct('subsystem', sel_arm(gains{k}), ...
        'controlPort', struct('kind', 'action')));
    conns{end + 1} = struct('from', 'u', 'to', id, 'fromIndex', 0, 'toIndex', 0);
    conns{end + 1} = struct('from', 'f', 'to', id, 'fromIndex', k - 1, 'toIndex', 1);
    conns{end + 1} = struct('from', id, 'to', 'm', 'fromIndex', 0, 'toIndex', k - 1);
  end
  conns{end + 1} = struct('from', 'm', 'to', 'y', 'fromIndex', 0, 'toIndex', 0);
  conns{end + 1} = struct('from', 'm', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0);
  d = struct();
  d.blocks = [{...
    struct('id', 'u', 'type', 'sine', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Amplitude', 2.0, 'Frequency', 0.5, 'Phase', 0.0)), ...
    selector}, arms, {...
    struct('id', 'm', 'type', 'merge', 'inputs', numel(gains), 'outputs', 1, ...
      'params', struct('InitialOutput', 0)), ...
    struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', varName, 'SaveFormat', 'Array', ...
        'Decimation', 1, 'MaxDataPoints', 'inf'))}];
  d.connections = conns;
  d.sampleTime = 0.01;
  d.duration = 2.0;
  d.variables = struct();
end
%=============================================================================
% if / else on the sign of a sine: x2 on the positive half, x1 on the negative
% one, so the two arms are told apart by the sign of what comes out.
selIf = struct('id', 'f', 'type', 'if', 'inputs', 1, 'outputs', 2, ...
  'params', struct('IfExpression', 'u1 > 0', 'ElseIfExpressions', '', 'ShowElse', 'on'));
dIf = sel_doc('nfcs_if', selIf, {2.0, 1.0});
rIf = jsondecode(__nflow_simulate__(jsonencode(dIf)));
assert_isequal(rIf.status, 'done');
yIf = evalin('base', 'nfcs_if');
assert_istrue(all(isfinite(yIf)));
assert_istrue(max(yIf) > 3.9); % 2 * 2, the x2 arm on the positive half
assert_istrue(min(yIf) < -1.9); % 1 * -2, the x1 arm on the negative one
evalin('base', 'clear(''nfcs_if'')');
%=============================================================================
% Three arms through an elseif clause, to pin the first-match-wins order.
selElif = struct('id', 'f', 'type', 'if', 'inputs', 1, 'outputs', 3, ...
  'params', struct('IfExpression', 'u1 > 1', 'ElseIfExpressions', 'u1 > 0', ...
    'ShowElse', 'on'));
dElif = sel_doc('nfcs_elif', selElif, {10.0, 1.0, -1.0});
rElif = jsondecode(__nflow_simulate__(jsonencode(dElif)));
assert_isequal(rElif.status, 'done');
yElif = evalin('base', 'nfcs_elif');
assert_istrue(all(isfinite(yElif)));
% u > 1 takes the x10 arm; between 0 and 1 the x1 arm; below zero the -1 arm.
assert_istrue(max(yElif) > 19.0);
evalin('base', 'clear(''nfcs_elif'')');
%=============================================================================
% switchCase on the truncated control.
selSw = struct('id', 'f', 'type', 'switchCase', 'inputs', 1, 'outputs', 2, ...
  'params', struct('CaseConditions', '{1}', 'ShowDefaultCase', 'on'));
dSw = sel_doc('nfcs_sw', selSw, {2.0, 1.0});
rSw = jsondecode(__nflow_simulate__(jsonencode(dSw)));
assert_isequal(rSw.status, 'done');
ySw = evalin('base', 'nfcs_sw');
assert_istrue(all(isfinite(ySw)));
evalin('base', 'clear(''nfcs_sw'')');
%=============================================================================
% Structure: the selector drives one port at a time, and the Merge reads a gate
% per input rather than a wire.
fS = [tempname(), '.nflow'];
[~, stemS] = fileparts(fS);
fid = fopen(fS, 'wt');
fwrite(fid, jsonencode(dIf));
fclose(fid);
nflow_codegenerate(fS, tempdir(), false, 'C');
srcS = fileread([tempdir(), stemS, '/model.c']);
assert_istrue(isempty(strfind(srcS, 'unknown:')));
assert_istrue(~isempty(strfind(srcS, 'out_f = '))); % clause port 0
assert_istrue(~isempty(strfind(srcS, 'out_f_p1 = '))); % the else port
assert_istrue(~isempty(strfind(srcS, 's->mg_m'))); % the Merge holds
assert_istrue(~isempty(strfind(srcS, 'out_a1__nfg != 0.0) { out_m')));
assert_istrue(~isempty(strfind(srcS, 'out_a2__nfg != 0.0) { out_m')));
rmfile(fS);
%=============================================================================
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dIf, 'nfcs_ifn', 'C', {'y'}, {'nfcs_if'}, 5e-6, 1e-9);
  nflow_codegen_check_numeric(dElif, 'nfcs_elifn', 'C', {'y'}, {'nfcs_elif'}, 5e-6, 1e-9);
  nflow_codegen_check_numeric(dSw, 'nfcs_swn', 'C', {'y'}, {'nfcs_sw'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(dIf, 'nfcs_ifn', 'rust', {'y'}, {'nfcs_if'}, 5e-6, 1e-9);
    nflow_codegen_check_numeric(dSw, 'nfcs_swn', 'rust', {'y'}, {'nfcs_sw'}, 5e-6, 1e-9);
  end
end
%=============================================================================
