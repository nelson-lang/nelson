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
% __nflow_codegen_support__(type) reports whether the C and Rust generators can
% lower a block type (a codegen template is registered and not notImplemented).
% This is the authoritative source for the per-block "supports code generation"
% help indicator (NFlow.internal.generateCodegenHelp).
%=============================================================================
% A continuous block with both C and Rust seams.
[c, r] = __nflow_codegen_support__('integrator');
assert_istrue(islogical(c) && islogical(r));
assert_istrue(c);
assert_istrue(r);
%=============================================================================
% Single-output form returns the C support only.
c = __nflow_codegen_support__('integrator');
assert_istrue(c);
%=============================================================================
% A block with no code-generation lowering (FMU wraps a precompiled binary).
[c, r] = __nflow_codegen_support__('fmu');
assert_isfalse(c);
assert_isfalse(r);
%=============================================================================
% An unknown / non-existent block type reports no support (not an error).
[c, r] = __nflow_codegen_support__('this_block_type_does_not_exist');
assert_isfalse(c);
assert_isfalse(r);
%=============================================================================
% Native / pass-lowered paths (no registered template, or an unimplemented
% placeholder) generate code and must report support: subsystems are
% flattened, expression has a native AST emitter, mux/demux/selector are
% erased by the vector expansion, matmul lowers to scalar blocks, the complex
% bridge blocks are erased by the re/im lowering, dashboards become tunable
% state fields.
for t = {'subsystem', 'expression', 'mux', 'demux', 'selector', 'matmul', ...
         'conjugate', 'complexToRealImag', 'dashboardSlider', 'labelSource', 'labelSink'}
  [c, r] = __nflow_codegen_support__(t{1});
  assert_istrue(c, t{1});
  assert_istrue(r, t{1});
end
%=============================================================================
% Codegen-only helpers with NO simulation handler must not report support:
% luScalar exists only inside expanded diagrams.
for t = {'luScalar'}
  [c, r] = __nflow_codegen_support__(t{1});
  assert_isfalse(c, t{1});
  assert_isfalse(r, t{1});
end
%=============================================================================
% fileSink gained both halves: a streaming CSV simulation handler and a
% codegen lowering (the pipeline rewrites it into an external labelSink).
[c, r] = __nflow_codegen_support__('fileSink');
assert_istrue(c);
assert_istrue(r);
%=============================================================================
% Registry symmetry: every block type registered for one codegen target must
% be registered for the other (a one-sided registration is a class of bug;
% the fileSink alias once shipped that way). Always empty when healthy.
asym = __nflow_codegen_asymmetries__();
assert_istrue(iscell(asym));
assert_isequal(numel(asym), 0);
%=============================================================================
% Simulable types with no lowering at all keep reporting no support.
for t = {'iteratorNumber'}
  [c, r] = __nflow_codegen_support__(t{1});
  assert_isfalse(c, t{1});
  assert_isfalse(r, t{1});
end
%=============================================================================
% planarMechanicalIsland gained a joint-free lowering (jointed islands are
% rejected per model by the non-generatable gate, but the TYPE has support).
[c, r] = __nflow_codegen_support__('planarMechanicalIsland');
assert_istrue(c);
assert_istrue(r);
%=============================================================================
% Cross-check the reported support against an actual generation: a one-block
% diagram of a native-path type must produce clean code (no unknown marker).
d = struct();
d.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2)), ...
  struct('id', 'e', 'type', 'expression', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Expr', 'u * 3')), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'k', 'to', 'e', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'e', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 0.5;
d.variables = struct();
wd = tempname();
mkdir(wd);
cleaner = onCleanup(@() rmdir(wd, 's')); %#ok<NASGU>
f = fullfile(wd, 'nf_support_xcheck.nflow');
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, wd, false, 'C');
src = fileread([wd, '/nf_support_xcheck/model.c']);
assert_istrue(isempty(strfind(src, 'unknown:')));
%=============================================================================
