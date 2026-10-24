%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Code generation must reject nelsonFunction blocks (interpreted Nelson code),
% pointing the user at the expression block.
%=============================================================================
d = struct();
d.name = 'nf_reject';
d.blocks = {struct('id', 'c', 'type', 'constant', 'x', 100, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 1)), ...
  struct('id', 'f', 'type', 'nelsonFunction', 'x', 300, 'y', 100, 'rotation', 0, ...
    'params', struct('Fcn', 'sin', 'OutputDimensions', '-1')), ...
  struct('id', 's', 'type', 'scope', 'x', 500, 'y', 100, 'rotation', 0, 'params', struct())};
d.connections = {struct('from', 'c', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'f', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d.variables = ''; d.runtime = 1; d.sampleTime = 0.1; d.autoRoute = true; d.locked = false;
nflow_file = [tempdir, filesep, 'nf_reject.nflow'];
fw = fopen(nflow_file, 'w');
fprintf(fw, '%s', jsonencode(d));
fclose(fw);
cmd = ['nflow_codegenerate(''', nflow_file, ''', tempdir, false)'];
msg = '';
try
  eval(cmd);
catch e
  msg = e.message;
end
asserts.istrue(~isempty(msg));
asserts.istrue(~isempty(strfind(msg, 'nelsonFunction')));
%=============================================================================
