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
% Feedback through a non-feedthrough stateful block in generated code. A lag
% (sum -> integrator -> back into the sum) breaks its cycle at the integrator:
% the sum is emitted BEFORE the integrator in topological order, so it must
% read the integrator's pre-loaded output-from-state, not the 0.0 declaration
% default (which silently dropped the whole feedback term: the exported code
% integrated the open loop). Both generators are checked textually, no C/Rust
% toolchain needed.
%=============================================================================
% Lag: x' = 1 - x (constant forcing, unit feedback), y = x.
lag = ['{"blocks":[', ...
  '{"id":"k1","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', ...
  '{"id":"s","type":"sum","inputs":2,"outputs":1,"params":{"Inputs":"+-"}},', ...
  '{"id":"ig","type":"integrator","inputs":1,"outputs":1,"params":{}},', ...
  '{"id":"y","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"y","isExternalPort":true}}],', ...
  '"connections":[', ...
    '{"from":"k1","to":"s","fromIndex":0,"toIndex":0},', ...
    '{"from":"ig","to":"s","fromIndex":0,"toIndex":1},', ...
    '{"from":"s","to":"ig","fromIndex":0,"toIndex":0},', ...
    '{"from":"ig","to":"y","fromIndex":0,"toIndex":0}],', ...
  '"runtime":1.0,"sampleTime":0.01}'];
mfile = [tempname(), '.nflow'];
[~, mfilestem] = fileparts(mfile);
fid = fopen(mfile, 'wt'); fwrite(fid, lag); fclose(fid);
%=============================================================================
% C: the integrator's output is pre-loaded from its state before the sum reads
% it, and the sum consumes the feedback term.
nflow_codegenerate(mfile, tempdir(), false);
cc = fileread([tempdir(), mfilestem, '/model.c']);
pre = strfind(cc, 'out_ig = s->int_ig;');
sumLine = strfind(cc, 'out_s = out_k1 + -out_ig;');
assert_istrue(~isempty(pre));
assert_istrue(~isempty(sumLine));
% The pre-load appears BEFORE the sum consumes it.
assert_istrue(min(pre) < min(sumLine));
%=============================================================================
% Rust mirror: same pre-load, same ordering.
nflow_codegenerate(mfile, tempdir(), false, 'rust');
rr = fileread([tempdir(), mfilestem, '/src/lib.rs']);
preR = strfind(rr, 'out_ig = s.int_ig;');
sumR = strfind(rr, 'out_s = out_k1 - out_ig;');
assert_istrue(~isempty(preR));
assert_istrue(~isempty(sumR));
assert_istrue(min(preR) < min(sumR));
%=============================================================================
% A discrete feedback loop (unit delay in a counter) pre-loads too.
cnt = ['{"blocks":[', ...
  '{"id":"one","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', ...
  '{"id":"acc","type":"sum","inputs":2,"outputs":1,"params":{"Inputs":"++"}},', ...
  '{"id":"del","type":"unitDelay","inputs":1,"outputs":1,"params":{}},', ...
  '{"id":"y","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"y","isExternalPort":true}}],', ...
  '"connections":[', ...
    '{"from":"one","to":"acc","fromIndex":0,"toIndex":0},', ...
    '{"from":"del","to":"acc","fromIndex":0,"toIndex":1},', ...
    '{"from":"acc","to":"del","fromIndex":0,"toIndex":0},', ...
    '{"from":"acc","to":"y","fromIndex":0,"toIndex":0}],', ...
  '"runtime":1.0,"sampleTime":0.1}'];
cfile = [tempname(), '.nflow'];
[~, cfilestem] = fileparts(cfile);
fid = fopen(cfile, 'wt'); fwrite(fid, cnt); fclose(fid);
nflow_codegenerate(cfile, tempdir(), false);
dc = fileread([tempdir(), cfilestem, '/model.c']);
preD = strfind(dc, 'out_del = s->unit_delay_del;');
sumD = strfind(dc, 'out_acc = out_one + out_del;');
assert_istrue(~isempty(preD));
assert_istrue(~isempty(sumD));
assert_istrue(min(preD) < min(sumD));
%=============================================================================
% An acyclic chain gains no pre-load: emission stays byte-identical to the
% historical output (the fix is scoped to broken feedback cycles).
chain = ['{"blocks":[', ...
  '{"id":"k1","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', ...
  '{"id":"ig","type":"integrator","inputs":1,"outputs":1,"params":{}},', ...
  '{"id":"y","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"y","isExternalPort":true}}],', ...
  '"connections":[', ...
    '{"from":"k1","to":"ig","fromIndex":0,"toIndex":0},', ...
    '{"from":"ig","to":"y","fromIndex":0,"toIndex":0}],', ...
  '"runtime":1.0,"sampleTime":0.01}'];
hfile = [tempname(), '.nflow'];
[~, hfilestem] = fileparts(hfile);
fid = fopen(hfile, 'wt'); fwrite(fid, chain); fclose(fid);
nflow_codegenerate(hfile, tempdir(), false);
hc = fileread([tempdir(), hfilestem, '/model.c']);
% Exactly ONE integrator output emission (the block body's own), no pre-load.
assert_isequal(numel(strfind(hc, 'out_ig = s->int_ig;')), 1);
%=============================================================================
