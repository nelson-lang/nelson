%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A per-block SampleTime is honored by the generators only for state-holding
% blocks that gate their state advance on the major-step index (unitDelay, see
% test_nf_codegenerate_multirate). A SampleTime on any other block type has no
% lowering and would run every step -- silently different from the simulation --
% so it is rejected with a clear diagnostic rather than emitted.
%=============================================================================
model = [tempname(), '.nflow'];
json = ['{', ...
  '"version":"1.0.0","format":"nflow-2","name":"MultirateReject","sampleTime":0.1,', ...
  '"blocks":[', ...
  '{"id":"c","type":"constant","x":100,"y":100,"rotation":0,"params":{"Value":1}},', ...
  '{"id":"g","type":"gain","x":300,"y":100,"rotation":0,"params":{"Gain":2,"SampleTime":0.2}},', ...
  '{"id":"s","type":"scope","x":500,"y":100,"rotation":0,"params":{}}', ...
  '],', ...
  '"connections":[{"from":"c","fromPort":0,"to":"g","toPort":0},', ...
  '{"from":"g","fromPort":0,"to":"s","toPort":0}]', ...
  '}'];
fid = fopen(model, 'wt');
fwrite(fid, json);
fclose(fid);
%=============================================================================
expected = _(['Code generation is not yet supported for a multi-rate ', ...
  'SampleTime on this block type (block ''g''). It is supported on discrete state-holding ', ...
  'blocks (unit delay, difference, counters, edge and change detectors); for ', ...
  'other blocks remove the per-block SampleTime or simulate with the solver instead.']);
cmd = ['nflow_codegenerate(''', model, ''', tempdir(), false, ''C'')'];
assert_checkerror(cmd, expected);
%=============================================================================
% Same rejection for the Rust backend.
cmd = ['nflow_codegenerate(''', model, ''', tempdir(), false, ''rust'')'];
assert_checkerror(cmd, expected);
%=============================================================================
rmfile(model);
%=============================================================================
% A SampleTime that is not an integer multiple of the base step is rejected
% with the simulator's own rule (integer sub-rates only) -- even on a block
% whose emitter honors multi-rate.
model = [tempname(), '.nflow'];
json = ['{', ...
  '"version":"1.0.0","format":"nflow-2","name":"MultirateNonInt","sampleTime":0.1,', ...
  '"blocks":[', ...
  '{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', ...
  '{"id":"ud","type":"unitDelay","inputs":1,"outputs":1,"params":{"SampleTime":0.25}},', ...
  '{"id":"s","type":"scope","inputs":1,"outputs":1,"params":{}}', ...
  '],', ...
  '"connections":[{"from":"c","to":"ud","fromIndex":0,"toIndex":0},', ...
  '{"from":"ud","to":"s","fromIndex":0,"toIndex":0}]', ...
  '}'];
fid = fopen(model, 'wt');
fwrite(fid, json);
fclose(fid);
msg = '';
try
  nflow_codegenerate(model, tempdir(), false, 'C');
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'integer multiple of the base step')));
rmfile(model);
%=============================================================================
% Self-scheduled block types (zoh/ddelay/foh/dtf/dstateSpace) name their own
% sample time 'SampleTime' -- it is the field the editor writes -- and their
% emitted step gates on it. They are NOT a generic multi-rate request, so they
% must go through instead of being rejected: rejecting them left every one of
% those blocks unusable above the base step (which is the only rate at which
% they do anything).
model = [tempname(), '.nflow'];
json = ['{', ...
  '"version":"1.0.0","format":"nflow-2","name":"MultirateSelfSched","sampleTime":0.1,', ...
  '"blocks":[', ...
  '{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', ...
  '{"id":"z","type":"zoh","inputs":1,"outputs":1,"params":{"SampleTime":0.2}},', ...
  '{"id":"s","type":"scope","inputs":1,"outputs":1,"params":{}}', ...
  '],', ...
  '"connections":[{"from":"c","to":"z","fromIndex":0,"toIndex":0},', ...
  '{"from":"z","to":"s","fromIndex":0,"toIndex":0}]', ...
  '}'];
fid = fopen(model, 'wt');
fwrite(fid, json);
fclose(fid);
msg = '';
try
  nflow_codegenerate(model, tempdir(), false, 'C');
catch e
  msg = e.message;
end
assert_isequal(msg, '');
[~, stem] = fileparts(model);
srcSelfSched = fileread([tempdir(), stem, filesep, 'model.c']);
% The hold period reaches the emitted step as the block's own schedule, not as
% a generic sub-rate gate.
assert_istrue(~isempty(strfind(srcSelfSched, 'zoh_next_z')));
rmfile(model);
%=============================================================================
