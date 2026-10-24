%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
ballodeVisible = 'off';
ballodeUseOutputFcn = false;
run([modulepath('ode_solvers', 'root'), '/examples/ballode.m']);
run([modulepath('ode_solvers', 'root'), '/examples/ballode.m']);
asserts.istrue(~isempty(timeOutput));
asserts.istrue(~isempty(stateOutput));
asserts.istrue(~isempty(eventTimeOutput));
asserts.isequal(size(stateOutput, 2), 2);
asserts.istrue(timeOutput(length(timeOutput)) <= 30);
asserts.istrue(eventTimeOutput(length(eventTimeOutput)) > 25);
ax = gca();
asserts.isequal(size(ax.Children), [2 1]);
asserts.isequal(ax.XLim, [0 30]);
close all
%=============================================================================
