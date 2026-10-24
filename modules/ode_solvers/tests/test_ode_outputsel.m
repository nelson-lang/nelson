%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear global ODE_OUTPUT_SEL_LOG;
global ODE_OUTPUT_SEL_LOG;
ODE_OUTPUT_SEL_LOG = {};
f = @(t, y)[y(1); -y(2)];
options = odeset('OutputFcn', @odeOutputSelRecorder, 'OutputSel', 2, 'MaxStep', 0.1);
[t, y] = ode45(f, [0 0.3], [1; 2], options);
asserts.istrue(size(y, 2) == 2);
asserts.istrue(length(ODE_OUTPUT_SEL_LOG) >= 3);
asserts.isequal(size(ODE_OUTPUT_SEL_LOG{1}.y), [1 1]);
for k = 2:length(ODE_OUTPUT_SEL_LOG) - 1
  asserts.isequal(size(ODE_OUTPUT_SEL_LOG{k}.y), [1 1]);
end
lastIndex = length(ODE_OUTPUT_SEL_LOG);
asserts.istrue(isempty(ODE_OUTPUT_SEL_LOG{lastIndex}.y));
%=============================================================================
