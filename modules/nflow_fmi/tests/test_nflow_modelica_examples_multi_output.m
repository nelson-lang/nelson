%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~modelicaInfo().capable
  return
end
exdir = [modulepath('nflow_fmi'), '/examples/modelica'];
%=============================================================================
% Multi-output Modelica model: a `modelica` block with two output ports (the FMU
% orders them itself, not by .mo declaration order). One port settles to the
% source voltage (vC -> 1), the other to zero DC current (iL -> 0); the check is
% order-agnostic to reflect that the FMU owns the port order.
mo2 = [exdir, '/RLC_MultiOutput.mo'];
m2 = struct('id', 'phys', 'type', 'modelica', 'inputs', 0, 'outputs', 2, ...
             'params', struct('file', mo2, 'modelName', 'RLC_MultiOutput'));
w1 = struct('id', 'w1', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'p0'));
w2 = struct('id', 'w2', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'p1'));
e1 = struct('from', 'phys', 'to', 'w1', 'fromIndex', 0, 'toIndex', 0);
e2 = struct('from', 'phys', 'to', 'w2', 'fromIndex', 1, 'toIndex', 0);
d2 = struct('blocks', {{m2, w1, w2}}, 'connections', {{e1, e2}}, 'runtime', 12.0, 'sampleTime', 0.05);
pp = [tempname(), '.nflow'];
fid = fopen(pp, 'w'); fwrite(fid, jsonencode(d2)); fclose(fid);
o2 = sim(pp);
n2 = {o2.logsout.name};
p0 = o2.logsout(find(strcmp(n2, 'p0'), 1)).values;
p1 = o2.logsout(find(strcmp(n2, 'p1'), 1)).values;
assert_istrue(~isequal(p0(:), p1(:))); % two distinct ports mapped
ends = sort([p0(end), p1(end)]);
assert_istrue(abs(ends(1) - 0.0) < 5e-2); % iL -> 0 (no DC current)
assert_istrue(abs(ends(2) - 1.0) < 5e-2); % vC -> V = 1
%=============================================================================
