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
% constraint blocks carry an algebraic DAE state solved by the IDAS runtime:
% there is no explicit C / Rust lowering for a differential-algebraic system,
% so code generation must reject the model with the typed message rather than
% emit a wrong constant-0 output.
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 1)), ...
  struct('id', 'c', 'type', 'constraint', 'inputs', 1, 'outputs', 1, ...
    'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'k', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'c', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
f = [tempname(), '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
msg = '';
try
  nflow_codegenerate(f, tempdir(), false, 'C');
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'constraint')));
assert_istrue(~isempty(strfind(msg, 'IDAS')));
rmfile(f);
%=============================================================================
