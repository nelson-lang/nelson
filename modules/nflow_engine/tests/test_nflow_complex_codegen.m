%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Complex code generation: the complex-lowering pass rewrites complex wires
% into re/im pairs of real blocks, so the scalar C/Rust generators emit
% plain real arithmetic (complex product = 4 mults + 2 sums).
%=============================================================================
outdir = [tempdir, filesep, 'nflow_cplx_codegen_test'];
if isdir(outdir)
  rmdir(outdir, 's');
end
mkdir(outdir);
% (1+3i) * (5+6i) = -13 + 21i, complex product then magnitude and output.
d = struct();
d.blocks = {struct('id', 'ra', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'ia', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 3)), ...
  struct('id', 'rb', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'ib', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 6)), ...
  struct('id', 'ca', 'type', 'realImagToComplex', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'cb', 'type', 'realImagToComplex', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'm', 'type', 'mult', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('GotoTag', 'y'))};
d.connections = {struct('from', 'ra', 'to', 'ca', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ia', 'to', 'ca', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'rb', 'to', 'cb', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ib', 'to', 'cb', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'ca', 'to', 'm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cb', 'to', 'm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'm', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'm', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
% Reference: simulation of the complex diagram.
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_isequal(r.signals.m, -13);
assert_isequal(r.signalsImag.m, 21);
%=============================================================================
% C generation: the lowered diagram carries the 4-mult/2-sum product and the
% complex output splits into y__re / y__im fields.
model = [outdir, filesep, 'cplx.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(model, outdir, false, 'C');
gen = fileread([outdir, filesep, 'cplx', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen, 'out_m__rr')));
assert_istrue(~isempty(strfind(gen, 'out_m__ii')));
assert_istrue(~isempty(strfind(gen, 'out_m__re')));
assert_istrue(~isempty(strfind(gen, 'out_m__im')));
assert_istrue(~isempty(strfind(gen, 'double y__re;')));
assert_istrue(~isempty(strfind(gen, 'double y__im;')));
%=============================================================================
% Rust mirror.
nflow_codegenerate(model, outdir, false, 'rust');
genr = fileread([outdir, filesep, 'cplx', filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(genr, 'out_m__re')));
assert_istrue(~isempty(strfind(genr, 'out_m__im')));
assert_istrue(~isempty(strfind(genr, 'pub y__re: f64,')));
assert_istrue(~isempty(strfind(genr, 'pub y__im: f64,')));
%=============================================================================
% The angle output lowers to atan2(im, re): full mag/angle round-trip
% generates C code carrying both ports.
d15 = struct();
d15.blocks = {struct('id', 'mm', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2)), ...
  struct('id', 'aa', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', pi / 3)), ...
  struct('id', 'mk', 'type', 'magnitudeAngleToComplex', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sp', 'type', 'complexToMagnitudeAngle', 'inputs', 1, 'outputs', 2, 'params', struct()), ...
  struct('id', 'ym', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('GotoTag', 'ym')), ...
  struct('id', 'ya', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('GotoTag', 'ya'))};
d15.connections = {struct('from', 'mm', 'to', 'mk', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'aa', 'to', 'mk', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'mk', 'to', 'sp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sp', 'to', 'ym', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sp', 'to', 'ya', 'fromIndex', 1, 'toIndex', 0)};
d15.sampleTime = 0.01; d15.duration = 0.05; d15.variables = struct();
model15 = [outdir, filesep, 'cang.nflow'];
fid = fopen(model15, 'wt');
fwrite(fid, jsonencode(d15));
fclose(fid);
nflow_codegenerate(model15, outdir, false, 'C');
gen15 = fileread([outdir, filesep, 'cang', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen15, 'atan2')));
assert_istrue(~isempty(strfind(gen15, 'sqrt')));
assert_istrue(~isempty(strfind(gen15, 'double ym;')));
assert_istrue(~isempty(strfind(gen15, 'double ya;')));
%=============================================================================
% abs of a complex lowers to sqrt(re^2 + im^2) through an expression block.
d2 = d;
d2.blocks{end} = struct('id', 'mag', 'type', 'abs', 'inputs', 1, 'outputs', 1, 'params', struct());
d2.blocks{end + 1} = struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('GotoTag', 'y'));
d2.connections{end} = struct('from', 'm', 'to', 'mag', 'fromIndex', 0, 'toIndex', 0);
d2.connections{end + 1} = struct('from', 'mag', 'to', 'y', 'fromIndex', 0, 'toIndex', 0);
model2 = [outdir, filesep, 'cabs.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(d2));
fclose(fid);
nflow_codegenerate(model2, outdir, false, 'C');
gen2 = fileread([outdir, filesep, 'cabs', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen2, 'out_mag__m2r')));
assert_istrue(~isempty(strfind(gen2, 'sqrt')));
assert_istrue(~isempty(strfind(gen2, 'double y;')));
%=============================================================================
