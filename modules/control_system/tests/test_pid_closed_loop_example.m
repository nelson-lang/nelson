%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
pidExampleFigureVisible = 'off';
run([modulepath('control_system', 'root'), '/examples/pid_closed_loop.m']);

assert(all(real(closedLoopPoles) < 0));
assert(abs(responseValues(end) - 1) < 0.03);
assert(isequal(size(responseValues), size(responseTimes)));
assert(isgraphics(figureHandle));
close(figureHandle);
%=============================================================================
