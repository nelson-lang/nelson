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
% Bus signals through the simulation engine: busCreator groups
% heterogeneous signals into a packed bus, busSelector extracts members by
% path (nested a.b supported); non-bus-aware blocks reject buses cleanly.
%=============================================================================
% Round-trip: ([1 2], 7) -> busCreator -> busSelector('count','pos') and the
% extracted member feeds plain math again.
d = struct();
d.blocks = {struct('id', 'v', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2])), ...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 7)), ...
  struct('id', 'bc', 'type', 'busCreator', 'inputs', 2, 'outputs', 1, 'params', struct('MemberNames', {{'pos', 'count'}})), ...
  struct('id', 'sel', 'type', 'busSelector', 'inputs', 1, 'outputs', 2, 'params', struct('SelectedSignals', {{'count', 'pos'}})), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 3)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct()), ...
  struct('id', 's2', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {struct('from', 'v', 'to', 'bc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'k', 'to', 'bc', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'bc', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 's2', 'fromIndex', 1, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
% bus packs [pos(2), count(1)]
assert_isequal(nflow_test_sig(r, 'bc'), [1 2 7]);
% signalBus reports the member layout of the bus port
bi = r.signalBus.bc;
if iscell(bi)
  bi = bi{1};
end
assert_isequal(bi.members(1).name, 'pos');
assert_isequal(bi.members(1).offset, 0);
assert_isequal(bi.members(1).width, 2);
assert_isequal(bi.members(2).name, 'count');
assert_isequal(bi.members(2).offset, 2);
% selector port 1 = count -> gain 3 = 21 ; port 2 = pos
assert_isequal(nflow_test_sig(r, 'g'), 21);
sel = r.signals.sel;
if iscell(sel)
  p2 = sel{2}(:)';
else
  p2 = sel(2, :);
end
assert_isequal(p2, [1 2]);
%=============================================================================
% Nested bus: inner(pos) inside outer(sub, flag); dotted path extraction.
d2 = struct();
d2.blocks = {struct('id', 'x', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [4 5])), ...
  struct('id', 'inner', 'type', 'busCreator', 'inputs', 1, 'outputs', 1, 'params', struct('MemberNames', {{'a'}})), ...
  struct('id', 'y', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 9)), ...
  struct('id', 'outer', 'type', 'busCreator', 'inputs', 2, 'outputs', 1, 'params', struct('MemberNames', {{'sub', 'flag'}})), ...
  struct('id', 'sel', 'type', 'busSelector', 'inputs', 1, 'outputs', 2, 'params', struct('SelectedSignals', {{'sub.a', 'flag'}})), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct()), ...
  struct('id', 's2', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d2.connections = {struct('from', 'x', 'to', 'inner', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'inner', 'to', 'outer', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'y', 'to', 'outer', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'outer', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 's2', 'fromIndex', 1, 'toIndex', 0)};
d2.sampleTime = 0.01; d2.duration = 0.05; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
sel2 = r2.signals.sel;
if iscell(sel2)
  a = sel2{1}(:)'; f = sel2{2}(:)';
else
  a = sel2(1, :); f = sel2(2);
end
assert_isequal(a, [4 5]);
assert_isequal(f(1), 9);
%=============================================================================
% A bus into a plain math block is a clean compile error.
d3 = struct();
d3.blocks = {struct('id', 'v', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'bc', 'type', 'busCreator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d3.connections = {struct('from', 'v', 'to', 'bc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'bc', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.01; d3.duration = 0.05; d3.variables = struct();
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'error');
assert_istrue(~isempty(strfind(r3.reason, 'busSelector')));
%=============================================================================
% Unknown member path names the available members.
d4 = d;
d4.blocks{4} = struct('id', 'sel', 'type', 'busSelector', 'inputs', 1, 'outputs', 1, 'params', struct('SelectedSignals', {{'missing'}}));
d4.connections = d.connections(1:4);
d4.connections{4} = struct('from', 'sel', 'to', 'g', 'fromIndex', 0, 'toIndex', 0);
d4.connections{5} = struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 0);
d4.blocks = d4.blocks([1 2 3 4 5 6]);
r4 = jsondecode(__nflow_simulate__(jsonencode(d4)));
assert_isequal(r4.status, 'error');
assert_istrue(~isempty(strfind(r4.reason, 'missing')));
assert_istrue(~isempty(strfind(r4.reason, 'pos')));
%=============================================================================
% Code generation: the bus collapses to direct element wires (virtual bus
% semantics); the expanded diagram simulates identically and the generated
% C carries the member value through to the output.
ex = __nflow_expand_vector__(jsonencode(d));
exj = jsondecode(ex);
assert_isfalse(isfield(exj, 'error'));
re = jsondecode(__nflow_simulate__(ex));
assert_isequal(re.status, 'done');
% the gain fed by the selected 'count' member still computes 21
assert_isequal(nflow_test_sig(re, 'g'), 21);
%=============================================================================
outdir = [tempdir, filesep, 'nflow_bus_codegen_test'];
if isdir(outdir)
  rmdir(outdir, 's');
end
mkdir(outdir);
db = d;
db.blocks{end + 1} = struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('GotoTag', 'y'));
db.connections{end + 1} = struct('from', 'g', 'to', 'y', 'fromIndex', 0, 'toIndex', 0);
model = [outdir, filesep, 'bus.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(db));
fclose(fid);
nflow_codegenerate(model, outdir, false, 'C');
gen = fileread([outdir, filesep, 'bus', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen, 'out_g')));
assert_istrue(~isempty(strfind(gen, 'double y;')));
nflow_codegenerate(model, outdir, false, 'rust');
genr = fileread([outdir, filesep, 'bus', filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(genr, 'out_g')));
assert_istrue(~isempty(strfind(genr, 'pub y: f64,')));
%=============================================================================
% A complex bus member generates code: the lowering splits it into
% m__re/m__im members and the selector paths follow. abs(3+4i) = 5.
d5 = struct();
d5.blocks = {struct('id', 'rr', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 3)), ...
  struct('id', 'ii', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 4)), ...
  struct('id', 'ck', 'type', 'realImagToComplex', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'kk', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 10)), ...
  struct('id', 'bc', 'type', 'busCreator', 'inputs', 2, 'outputs', 1, 'params', struct('MemberNames', {{'z', 'k'}})), ...
  struct('id', 'sel', 'type', 'busSelector', 'inputs', 1, 'outputs', 2, 'params', struct('SelectedSignals', {{'z', 'k'}})), ...
  struct('id', 'mag', 'type', 'abs', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct()), ...
  struct('id', 'ym', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('GotoTag', 'ym')), ...
  struct('id', 'yk', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('GotoTag', 'yk'))};
d5.connections = {struct('from', 'rr', 'to', 'ck', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ii', 'to', 'ck', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'ck', 'to', 'bc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'kk', 'to', 'bc', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'bc', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 'mag', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'mag', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'mag', 'to', 'ym', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 'yk', 'fromIndex', 1, 'toIndex', 0)};
d5.sampleTime = 0.01; d5.duration = 0.05; d5.variables = struct();
% simulation reference: |3+4i| = 5, k = 10
r5 = jsondecode(__nflow_simulate__(jsonencode(d5)));
assert_isequal(r5.status, 'done');
assert_isequal(nflow_test_sig(r5, 'mag'), 5);
% code generation succeeds and carries the split members
model5 = [outdir, filesep, 'cbus.nflow'];
fid = fopen(model5, 'wt');
fwrite(fid, jsonencode(d5));
fclose(fid);
nflow_codegenerate(model5, outdir, false, 'C');
gen5 = fileread([outdir, filesep, 'cbus', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen5, 'sqrt')));
assert_istrue(~isempty(strfind(gen5, 'double ym;')));
assert_istrue(~isempty(strfind(gen5, 'double yk;')));
%=============================================================================
% Nonvirtual bus at the generated interface: a labelSink fed by a named
% nonvirtual bus emits a recursive struct (C typedef / Rust #[repr(C)])
% and one struct-typed ModelOutput field instead of per-element fields.
% Vector members become arrays, int64 members keep their exact type,
% nested buses become nested structs.
% Raw JSON so the busTypes member dims stay arrays (jsonencode collapses
% one-element vectors to scalars).
dnv = ['{"busTypes":[', ...
  '{"name":"Inner","nonvirtual":true,"members":[{"name":"a","dims":[2]}]},', ...
  '{"name":"Pose","nonvirtual":true,"members":[', ...
  '{"name":"pos","dims":[2]},{"name":"count","type":"int64"},{"name":"sub","bus":"Inner"}]}],', ...
  '"blocks":[', ...
  '{"id":"v","type":"constant","inputs":0,"outputs":1,"params":{"Value":[1,2]}},', ...
  '{"id":"k","type":"constant","inputs":0,"outputs":1,"params":{"Value":7,"OutDataType":"int64"}},', ...
  '{"id":"w","type":"constant","inputs":0,"outputs":1,"params":{"Value":[4,5]}},', ...
  '{"id":"bi","type":"busCreator","inputs":1,"outputs":1,"params":{"BusType":"Inner"}},', ...
  '{"id":"bc","type":"busCreator","inputs":3,"outputs":1,"params":{"BusType":"Pose"}},', ...
  '{"id":"y","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"y"}},', ...
  '{"id":"s","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[', ...
  '{"from":"v","to":"s","fromIndex":0,"toIndex":0},', ...
  '{"from":"v","to":"bc","fromIndex":0,"toIndex":0},', ...
  '{"from":"k","to":"bc","fromIndex":0,"toIndex":1},', ...
  '{"from":"w","to":"bi","fromIndex":0,"toIndex":0},', ...
  '{"from":"bi","to":"bc","fromIndex":0,"toIndex":2},', ...
  '{"from":"bc","to":"y","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":0.05,"variables":{}}'];
% The bus terminates on the label sink and still simulates (packed copy).
rnv = jsondecode(__nflow_simulate__(dnv));
assert_isequal(rnv.status, 'done');
assert_isequal(nflow_test_sig(rnv, 'bc'), [1 2 7 4 5]);
% C: recursive typedefs (nested type first), struct-typed output field and
% per-member assignments.
modelnv = [outdir, filesep, 'posebus.nflow'];
fid = fopen(modelnv, 'wt');
fwrite(fid, dnv);
fclose(fid);
nflow_codegenerate(modelnv, outdir, true, 'C');
gnv = fileread([outdir, filesep, 'posebus', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gnv, 'typedef struct {')));
assert_istrue(~isempty(strfind(gnv, 'double a[2];')));
assert_istrue(~isempty(strfind(gnv, '} Inner_t;')));
assert_istrue(~isempty(strfind(gnv, 'double pos[2];')));
assert_istrue(~isempty(strfind(gnv, 'int64_t count;')));
assert_istrue(~isempty(strfind(gnv, 'Inner_t sub;')));
assert_istrue(~isempty(strfind(gnv, '} Pose_t;')));
assert_istrue(~isempty(strfind(gnv, 'Pose_t y;')));
assert_istrue(~isempty(strfind(gnv, 'out->y.pos[0] = ')));
assert_istrue(~isempty(strfind(gnv, 'out->y.count = ')));
assert_istrue(~isempty(strfind(gnv, 'out->y.sub.a[1] = ')));
% the flattened per-element fields are gone from the interface
assert_istrue(isempty(strfind(gnv, 'double y_e0;')));
% the CSV runner prints one column per packed element
mnv = fileread([outdir, filesep, 'posebus', filesep, 'main.c']);
assert_istrue(~isempty(strfind(mnv, 'out.y.sub.a[1]')));
% Rust mirror: #[repr(C)] structs, typed members, struct-typed output.
nflow_codegenerate(modelnv, outdir, true, 'rust');
gnvr = fileread([outdir, filesep, 'posebus', filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(gnvr, '#[repr(C)]')));
assert_istrue(~isempty(strfind(gnvr, 'pub struct Inner {')));
assert_istrue(~isempty(strfind(gnvr, 'pub a: [f64; 2],')));
assert_istrue(~isempty(strfind(gnvr, 'pub struct Pose {')));
assert_istrue(~isempty(strfind(gnvr, 'pub pos: [f64; 2],')));
assert_istrue(~isempty(strfind(gnvr, 'pub count: i64,')));
assert_istrue(~isempty(strfind(gnvr, 'pub sub: Inner,')));
assert_istrue(~isempty(strfind(gnvr, 'pub y: Pose,')));
assert_istrue(~isempty(strfind(gnvr, 'out.y.sub.a[1] = ')));
mnvr = fileread([outdir, filesep, 'posebus', filesep, 'src', filesep, 'main.rs']);
assert_istrue(~isempty(strfind(mnvr, 'out.y.count')));
%=============================================================================
% Struct-typed external INPUT: a labelSource declared with a named
% nonvirtual bus type emits one struct-typed ModelInput field; the CSV
% runner reads one column per packed element. Members keep their exact
% type (int64 leaf -> int64_t / i64 field).
din = ['{"busTypes":[', ...
  '{"name":"Inner","nonvirtual":true,"members":[{"name":"a","dims":[2]}]},', ...
  '{"name":"Pose","nonvirtual":true,"members":[', ...
  '{"name":"pos","dims":[2]},{"name":"count","type":"int64"},{"name":"sub","bus":"Inner"}]}],', ...
  '"blocks":[', ...
  '{"id":"u","type":"labelSource","inputs":0,"outputs":1,"params":{"GotoTag":"u","BusType":"Pose"}},', ...
  '{"id":"sel","type":"busSelector","inputs":1,"outputs":3,"params":{"SelectedSignals":["pos","count","sub.a"]}},', ...
  '{"id":"yp","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"yp"}},', ...
  '{"id":"yk","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"yk"}},', ...
  '{"id":"ya","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"ya"}},', ...
  '{"id":"s","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[', ...
  '{"from":"sel","to":"yp","fromIndex":0,"toIndex":0},', ...
  '{"from":"sel","to":"yk","fromIndex":1,"toIndex":0},', ...
  '{"from":"sel","to":"ya","fromIndex":2,"toIndex":0},', ...
  '{"from":"u","to":"sel","fromIndex":0,"toIndex":0},', ...
  '{"from":"sel","to":"s","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":0.05,"variables":{}}'];
% The bus-typed external input simulates (all-zero source, members selectable).
rin = jsondecode(__nflow_simulate__(din));
assert_isequal(rin.status, 'done');
assert_isequal(nflow_test_sig(rin, 'u'), [0 0 0 0 0]);
modelin = [outdir, filesep, 'poseinput.nflow'];
fid = fopen(modelin, 'wt');
fwrite(fid, din);
fclose(fid);
nflow_codegenerate(modelin, outdir, true, 'C');
gin = fileread([outdir, filesep, 'poseinput', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gin, 'Pose_t u;')));
assert_istrue(~isempty(strfind(gin, 'in->u.pos[0]')));
assert_istrue(~isempty(strfind(gin, 'in->u.count : 0;')));
assert_istrue(~isempty(strfind(gin, 'in->u.sub.a[1]')));
assert_istrue(~isempty(strfind(gin, '"u.pos[0]",')));
% the flattened per-element input fields are gone from the interface
assert_istrue(isempty(strfind(gin, 'double u_e0;')));
min_ = fileread([outdir, filesep, 'poseinput', filesep, 'main.c']);
assert_istrue(~isempty(strfind(min_, 'in->u.pos[0] = ')));
assert_istrue(~isempty(strfind(min_, 'in->u.count = (int64_t)')));
% Rust mirror: struct-typed ModelInput field + per-column typed filling.
nflow_codegenerate(modelin, outdir, true, 'rust');
ginr = fileread([outdir, filesep, 'poseinput', filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(ginr, 'pub u: Pose,')));
assert_istrue(~isempty(strfind(ginr, 'inp.u.count;')));
minr = fileread([outdir, filesep, 'poseinput', filesep, 'src', filesep, 'main.rs']);
assert_istrue(~isempty(strfind(minr, 'inp.u.pos[0] = row[0];')));
assert_istrue(~isempty(strfind(minr, 'inp.u.count = row[2] as i64;')));
%=============================================================================
% goto/from carries the bus flavor in simulation: a busSelector can read
% members after a labelSink/labelSource hop.
dgf = ['{"busTypes":[', ...
  '{"name":"Pair","nonvirtual":false,"members":[', ...
  '{"name":"a","dims":[2]},{"name":"k","type":"int64"}]}],', ...
  '"blocks":[', ...
  '{"id":"v","type":"constant","inputs":0,"outputs":1,"params":{"Value":[1,2]}},', ...
  '{"id":"k","type":"constant","inputs":0,"outputs":1,"params":{"Value":7,"OutDataType":"int64"}},', ...
  '{"id":"bc","type":"busCreator","inputs":2,"outputs":1,"params":{"BusType":"Pair"}},', ...
  '{"id":"t","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"b"}},', ...
  '{"id":"f","type":"labelSource","inputs":0,"outputs":1,"params":{"GotoTag":"b"}},', ...
  '{"id":"sel","type":"busSelector","inputs":1,"outputs":1,"params":{"SelectedSignals":["k"]}},', ...
  '{"id":"s","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[', ...
  '{"from":"v","to":"bc","fromIndex":0,"toIndex":0},', ...
  '{"from":"k","to":"bc","fromIndex":0,"toIndex":1},', ...
  '{"from":"bc","to":"t","fromIndex":0,"toIndex":0},', ...
  '{"from":"f","to":"sel","fromIndex":0,"toIndex":0},', ...
  '{"from":"sel","to":"s","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":0.05,"variables":{}}'];
rgf = jsondecode(__nflow_simulate__(dgf));
assert_isequal(rgf.status, 'done');
assert_isequal(nflow_test_sig(rgf, 'sel'), 7);
%=============================================================================
% Regression: an INTERNAL vector goto/from pair stays internal in generated
% code (the per-element source tags follow the per-element sink renaming;
% previously every element copy was misclassified as an external input).
dvec = ['{"blocks":[', ...
  '{"id":"v","type":"constant","inputs":0,"outputs":1,"params":{"Value":[1,2,3]}},', ...
  '{"id":"t","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"w"}},', ...
  '{"id":"f","type":"labelSource","inputs":0,"outputs":1,"params":{"GotoTag":"w"}},', ...
  '{"id":"g","type":"gain","inputs":1,"outputs":1,"params":{"Gain":2}},', ...
  '{"id":"y","type":"labelSink","inputs":1,"outputs":1,"params":{"GotoTag":"y"}},', ...
  '{"id":"s","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[', ...
  '{"from":"v","to":"t","fromIndex":0,"toIndex":0},', ...
  '{"from":"f","to":"g","fromIndex":0,"toIndex":0},', ...
  '{"from":"g","to":"y","fromIndex":0,"toIndex":0},', ...
  '{"from":"v","to":"s","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":0.05,"variables":{}}'];
modelvec = [outdir, filesep, 'vecgoto.nflow'];
fid = fopen(modelvec, 'wt');
fwrite(fid, dvec);
fclose(fid);
nflow_codegenerate(modelvec, outdir, false, 'C');
gvec = fileread([outdir, filesep, 'vecgoto', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gvec, 'INPUT_COUNT = 0;')));
assert_istrue(~isempty(strfind(gvec, 'double y_e0;')));
%=============================================================================
% A VIRTUAL named bus keeps the flattened per-element interface.
dvv = strrep(dnv, '"nonvirtual":true', '"nonvirtual":false');
modelvv = [outdir, filesep, 'posevirt.nflow'];
fid = fopen(modelvv, 'wt');
fwrite(fid, dvv);
fclose(fid);
nflow_codegenerate(modelvv, outdir, false, 'C');
gvv = fileread([outdir, filesep, 'posevirt', filesep, 'model.c']);
assert_istrue(isempty(strfind(gvv, 'Pose_t')));
assert_istrue(~isempty(strfind(gvv, 'double y_e0;')));
%=============================================================================
function v = nflow_test_sig(r, id)
  v = r.signals.(id);
  if iscell(v)
    v = cell2mat(v);
  end
  v = v(:)';
end
%=============================================================================
