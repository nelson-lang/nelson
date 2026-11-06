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
% A signal chain that ends only in a visualization sink (scope) must still be
% emitted: dead-code elimination keeps the blocks feeding an observed sink.
%=============================================================================
function src = gen_scope_chain(expr, lang)
  d = struct();
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
    struct('id', 'f', 'type', 'expression', 'inputs', 1, 'outputs', 1, 'params', struct('Expr', expr)), ...
    struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
  d.connections = {...
    struct('from', 'c', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'f', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.01;
  d.duration = 1;
  d.variables = struct();
  wd = tempname();
  mkdir(wd);
  f = fullfile(wd, 'scope_chain.nflow');
  fid = fopen(f, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  nflow_codegenerate(f, wd, false, lang);
  if strcmp(lang, 'C')
    src = fileread([wd, '/scope_chain/model.c']);
  else
    src = fileread([wd, '/scope_chain/src/lib.rs']);
  end
end
%=============================================================================
% C: the expression feeding the scope is emitted verbatim, not pruned to an
% empty model.
c = gen_scope_chain('sin(u) + u * 2', 'C');
assert_istrue(~isempty(strfind(c, 'sin(')));
assert_istrue(~isempty(regexp(c, 'out_\w+\s*=.*sin\(', 'once')));
%=============================================================================
% The "^" operator is power in the expression grammar: it must become pow(),
% never a bare "^" (which is XOR in C).
cp = gen_scope_chain('u ^ 2', 'C');
assert_istrue(~isempty(strfind(cp, 'pow(')));
assert_istrue(isempty(regexp(cp, 'out_\w+\s*=[^;]*\^', 'once')));
%=============================================================================
% Rust emits the same chain through libm.
r = gen_scope_chain('sin(u) + u * 2', 'rust');
assert_istrue(~isempty(strfind(r, 'libm::sin(')));
%=============================================================================
