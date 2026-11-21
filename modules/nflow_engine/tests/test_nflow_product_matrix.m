%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Matrix-mode Product must use matrix multiplication instead of elementwise operations.
d = struct();
d.blocks = {struct('id','a','type','constant','inputs',0,'outputs',1,'params',struct('Value',[1 3 2 4])), ...
  struct('id','ashape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2])), ...
  struct('id','b','type','constant','inputs',0,'outputs',1,'params',struct('Value',[5 7 6 8])), ...
  struct('id','bshape','type','reshape','inputs',1,'outputs',1,'params',struct('OutputDimensions',[2 2])), ...
  struct('id','p','type','mult','inputs',2,'outputs',1,'params',struct('Inputs','**','Multiplication','Matrix(*)')), ...
  struct('id','s','type','scope','inputs',1,'outputs',0,'params',struct())};
d.connections = {struct('from','a','to','ashape'), struct('from','b','to','bshape'), ...
  struct('from','ashape','to','p','toIndex',0), struct('from','bshape','to','p','toIndex',1), ...
  struct('from','p','to','s')};
d.sampleTime = 0.01; d.duration = 0.1; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_isequal(r.signalDims.p(:)', [2 2]);
v = r.signals.p; if iscell(v), v = cell2mat(v); end
assert_isapprox(v(:)', [19 43 22 50], 1e-10);
% Matrix division A / B.
d.blocks{5}.params.Inputs = '*/';
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
v = r.signals.p; if iscell(v), v = cell2mat(v); end
assert_isapprox(v(:)', [3 2 -2 -1], 1e-10);
% Non-square multiplication checks dimension inference and column-major order.
d.blocks{1}.params.Value = [1 4 2 5 3 6];
d.blocks{2}.params.OutputDimensions = [2 3];
d.blocks{3}.params.Value = [7 9 11 8 10 12];
d.blocks{4}.params.OutputDimensions = [3 2];
d.blocks{5}.params.Inputs = '**';
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_isequal(r.signalDims.p(:)', [2 2]);
v = r.signals.p; if iscell(v), v = cell2mat(v); end
assert_isapprox(v(:)', [58 139 64 154], 1e-10);
% A single inverse needs row pivoting when the leading diagonal is zero.
d.blocks{1}.params.Value = [0 2 1 3];
d.blocks{2}.params.OutputDimensions = [2 2];
d.blocks{5}.inputs = 1;
d.blocks{5}.params.Inputs = '/';
d.connections = {struct('from','a','to','ashape'), struct('from','ashape','to','p'), ...
  struct('from','p','to','s')};
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
v = r.signals.p; if iscell(v), v = cell2mat(v); end
assert_isapprox(v(:)', [-1.5 1 0.5 0], 1e-10);
% A singular divisor is diagnosed instead of producing a silent result.
d.blocks{1}.params.Value = [1 2 2 4];
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'error');
% A mismatched matrix product is rejected.
d.blocks{1}.params.Value = [1 4 2 5 3 6];
d.blocks{2}.params.OutputDimensions = [2 3];
d.blocks{3}.params.Value = [1 3 2 4];
d.blocks{4}.params.OutputDimensions = [2 2];
d.blocks{5}.inputs = 2;
d.blocks{5}.params.Inputs = '**';
d.connections = {struct('from','a','to','ashape'), struct('from','b','to','bshape'), ...
  struct('from','ashape','to','p','toIndex',0), struct('from','bshape','to','p','toIndex',1), ...
  struct('from','p','to','s')};
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'error');
% Integer matrix multiplication saturates each product and accumulation.
d.blocks{1}.params.Value = [100 0 100 0];
d.blocks{1}.params.OutDataType = 'int8';
d.blocks{2}.params.OutputDimensions = [2 2];
d.blocks{3}.params.Value = [2 -2 0 0];
d.blocks{3}.params.OutDataType = 'int8';
d.blocks{4}.params.OutputDimensions = [2 2];
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
v = r.signals.p; if iscell(v), v = cell2mat(v); end
assert_isequal(v(:)', [-1 0 0 0]);
%=============================================================================