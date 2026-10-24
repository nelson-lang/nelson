%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
lorenzFigureVisible = 'off';
lorenzTimeSpan = 0:0.05:5;
run([modulepath('ode_solvers', 'root'), '/examples/lorenz_attractor.m']);

assert(isequal(size(timeValues), [101 1]));
assert(isequal(size(stateValues), [101 3]));
assert(all(isfinite(stateValues), 'all'));
assert(max(abs(stateValues(:, 1))) > 10);
assert(strcmp(get(figureHandle, 'Visible'), 'off'));
close(figureHandle);
%=============================================================================
