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
explorerFigureVisible = 'off';
explorerAnimationEnabled = true;
explorerAnimationFrames = 12;
explorerFramePause = 0;
explorerAnimationContinuous = false;
run([modulepath('function_handle', 'root'), ...
  '/examples/function_explorer.m']);

assert_istrue(isgraphics(explorerFigure));
assert_istrue(isgraphics(explorerCurve));
% Every expression text became a callable handle through str2func.
assert_isequal(numel(explorerHandles), numel(explorerExpressions));
assert_istrue(isa(explorerHandles{1}, 'function_handle'));
% The handle evaluates on the plotted grid and fills the curve.
sample = explorerHandles{1}(explorerX, 2);
assert_isequal(numel(sample), numel(explorerX));
assert_isequal(numel(get(explorerCurve, 'YData')), numel(explorerX));
delete(explorerFigure);
drawnow();
%=============================================================================
