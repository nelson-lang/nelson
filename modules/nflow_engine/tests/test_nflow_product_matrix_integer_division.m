%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Matrix inversion requires floating-point inputs; integer matrix products remain valid.
d = struct();
d.blocks = {struct('id','a','type','constant','inputs',0,'outputs',1,'params',struct('Value',[1 3 2 4], 'OutDataType','int8')), ...
  struct('id','ashape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2])), ...
  struct('id','b','type','constant','inputs',0,'outputs',1,'params',struct('Value',[5 7 6 8], 'OutDataType','int8')), ...
  struct('id','bshape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2])), ...
  struct('id','p','type','mult','inputs',2,'outputs',1,'params',struct('Inputs','*/','Multiplication','Matrix(*)')), ...
  struct('id','s','type','scope','inputs',1,'outputs',0,'params',struct())};
d.connections = {struct('from','a','to','ashape'), struct('from','b','to','bshape'), ...
  struct('from','ashape','to','p','toIndex',0), struct('from','bshape','to','p','toIndex',1), ...
  struct('from','p','to','s')};
d.sampleTime = 0.01; d.duration = 0.1; d.variables = struct();
for ty = {'int8', 'int64', 'uint64'}
  d.blocks{1}.params.OutDataType = ty{1};
  d.blocks{3}.params.OutDataType = ty{1};
  raw = __nflow_simulate__(jsonencode(d));
  result = jsondecode(raw);
  assert_isequal(result.status, 'error');
  assert_istrue(~isempty(strfind(raw, 'integer matrix division is not supported')));
end
% Both generated targets must reject the same unsupported model.
d.blocks{1}.params.OutDataType = 'int8';
d.blocks{3}.params.OutDataType = 'int8';
model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
for target = {'C', 'rust'}
  message = '';
  try
    nflow_codegenerate(model, tempdir(), false, target{1});
  catch err
    message = err.message;
  end
  assert_istrue(~isempty(strfind(message, 'integer matrix division is not supported')));
end
rmfile(model);
% A floating-point divisor must also be rejected when the result is integer.
d.blocks{1}.params.OutDataType = 'int8';
d.blocks{3}.params.OutDataType = 'double';
result = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(result.status, 'error');
% Converting both integer operands to double enables matrix division.
d.blocks{3}.params.OutDataType = 'int8';
d.blocks{end + 1} = struct('id','aconv','type','convert','inputs',1,'outputs',1,'params',struct('OutDataType','double'));
d.blocks{end + 1} = struct('id','bconv','type','convert','inputs',1,'outputs',1,'params',struct('OutDataType','double'));
d.connections = {struct('from','a','to','ashape'), struct('from','b','to','bshape'), ...
  struct('from','ashape','to','aconv'), struct('from','bshape','to','bconv'), ...
  struct('from','aconv','to','p','toIndex',0), struct('from','bconv','to','p','toIndex',1), ...
  struct('from','p','to','s')};
result = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(result.status, 'done');
values = result.signals.p; if iscell(values), values = cell2mat(values); end
assert_isapprox(values(:)', [3 2 -2 -1], 1e-10);
%=============================================================================
