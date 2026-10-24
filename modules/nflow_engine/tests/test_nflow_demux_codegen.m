%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A standalone demux fed by a real (non-mux) vector source must lower to real
% code: each demux output slice has to reach its consumer. Regression: the
% codegen flattener used to drop every connection leaving a demux, so consumers
% fell back to 0.0 (e.g. "out_g0 = 0.0 * 2"). Only the genuine mux -> demux
% pass-through may be collapsed.
%=============================================================================
outdir = [tempdir, filesep, 'nflow_demux_codegen_test'];
if isdir(outdir)
  rmdir(outdir, 's');
end
mkdir(outdir);
%=============================================================================
% constant [10 20] -> demux (2 outputs) -> gain g0 (x2) / gain g1 (x3) -> sinks.
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [10 20])), ...
  struct('id', 'dm', 'type', 'demux', 'inputs', 1, 'outputs', 2, 'params', struct()), ...
  struct('id', 'g0', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 'g1', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 3)), ...
  struct('id', 'y0', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('GotoTag', 'y0')), ...
  struct('id', 'y1', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('GotoTag', 'y1'))};
d.connections = {struct('from', 'c', 'to', 'dm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 'g0', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 'g1', 'fromIndex', 1, 'toIndex', 0), ...
  struct('from', 'g0', 'to', 'y0', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g1', 'to', 'y1', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
model = [outdir, filesep, 'demux.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(model, outdir, false, 'C');
gen = fileread([outdir, filesep, 'demux', filesep, 'model.c']);
% Each gain must read its demux slice (the constant element), not 0.0.
assert_istrue(~isempty(strfind(gen, 'out_g0 = out_c__e0 * 2')));
assert_istrue(~isempty(strfind(gen, 'out_g1 = out_c__e1 * 3')));
% The old dropped-connection bug emitted these; they must be gone.
assert_isequal(strfind(gen, 'out_g0 = 0.0 * 2'), []);
assert_isequal(strfind(gen, 'out_g1 = 0.0 * 3'), []);
%=============================================================================
% A genuine mux -> demux pair is still collapsed to a pass-through: the demux
% outputs resolve straight to the mux inputs (constants a and b).
d2 = struct();
d2.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 7)), ...
  struct('id', 'm', 'type', 'mux', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'dm', 'type', 'demux', 'inputs', 1, 'outputs', 2, 'params', struct()), ...
  struct('id', 'g0', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 'g1', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 3)), ...
  struct('id', 'y0', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('GotoTag', 'y0')), ...
  struct('id', 'y1', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('GotoTag', 'y1'))};
d2.connections = {struct('from', 'a', 'to', 'm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'm', 'to', 'dm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 'g0', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 'g1', 'fromIndex', 1, 'toIndex', 0), ...
  struct('from', 'g0', 'to', 'y0', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g1', 'to', 'y1', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.01; d2.duration = 0.05; d2.variables = struct();
model2 = [outdir, filesep, 'mxdm.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(d2));
fclose(fid);
nflow_codegenerate(model2, outdir, false, 'C');
gen2 = fileread([outdir, filesep, 'mxdm', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen2, 'out_g0 = out_a * 2')));
assert_istrue(~isempty(strfind(gen2, 'out_g1 = out_b * 3')));
%=============================================================================
