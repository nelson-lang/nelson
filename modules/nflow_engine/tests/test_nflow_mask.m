%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Mask parameters: a masked / reusable subsystem declares
% params.maskParameters (name -> value) that scope over the parent variables, so
% each instance resolves its inner block params (a Gain, a time constant, ...) to
% its own values. Verified for both simulation (per-instance scoping) and code
% generation (the flattener substitutes each instance's values into the inlined
% inner params). Models are written as raw JSON.
%=============================================================================
function p = mask_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w');
  fwrite(fid, json);
  fclose(fid);
end
%=============================================================================
function s = mask_instance(id, k)
  % A masked subsystem whose inner gain reads the mask parameter K.
  s = ['{"id":"', id, '","type":"subsystem","inputs":1,"outputs":1,"params":{', ...
    '"masked":true,"maskParameters":{"K":', num2str(k), '},"subsystem":{', ...
    '"blocks":[', ...
      '{"id":"in1","type":"labelSource","inputs":0,"outputs":1,"params":{"label":"in1"}},', ...
      '{"id":"g","type":"gain","inputs":1,"outputs":1,"params":{"Gain":"K"}},', ...
      '{"id":"o","type":"labelSink","inputs":1,"outputs":1,"params":{"label":"o"}}],', ...
    '"connections":[', ...
      '{"from":"in1","to":"g","fromIndex":0,"toIndex":0},', ...
      '{"from":"g","to":"o","fromIndex":0,"toIndex":0}],', ...
    '"externalInputs":[{"id":"in1","name":"in1"}],', ...
    '"externalOutputs":[{"id":"o","name":"o"}]}}}'];
end
%=============================================================================
% Two instances of the SAME masked block with different K resolve independently:
% a constant 2 through K=5 gives 10, through K=3 gives 6.
model = ['{"blocks":[', ...
  '{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":2}},', ...
  mask_instance('sub1', 5), ',', ...
  mask_instance('sub2', 3), ',', ...
  '{"id":"tw1","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"y1"}},', ...
  '{"id":"tw2","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"y2"}}],', ...
  '"connections":[', ...
    '{"from":"c","to":"sub1","fromIndex":0,"toIndex":0},', ...
    '{"from":"c","to":"sub2","fromIndex":0,"toIndex":0},', ...
    '{"from":"sub1","to":"tw1","fromIndex":0,"toIndex":0},', ...
    '{"from":"sub2","to":"tw2","fromIndex":0,"toIndex":0}],', ...
  '"runtime":0.2,"sampleTime":0.1}'];
mfile = mask_write(model, 'test_nflow_mask_sim');
out = sim(mfile);
% logsout order follows the To Workspace blocks; match by name to be safe.
names = {out.logsout.name};
i1 = find(strcmp(names, 'y1'), 1);
i2 = find(strcmp(names, 'y2'), 1);
assert_istrue(abs(out.logsout(i1).values(end) - 10) < 1e-9); % 2 * 5
assert_istrue(abs(out.logsout(i2).values(end) - 6) < 1e-9); % 2 * 3
%=============================================================================
% Code generation substitutes each instance's mask value into the inlined inner
% params: sub1 -> * 5, sub2 -> * 3 (not the unresolved * 0).
nflow_codegenerate(mfile, tempdir(), false);
[~, mstem] = fileparts(mfile);
cc = fileread([tempdir(), '/', mstem, '/model.c']);
assert_istrue(~isempty(strfind(cc, 'out_c * 5')));
assert_istrue(~isempty(strfind(cc, 'out_c * 3')));
assert_istrue(isempty(strfind(cc, 'out_c * 0')));
rmfile(mfile);
if isdir([tempdir(), '/', mstem])
  rmdir([tempdir(), '/', mstem], 's');
end
%=============================================================================
