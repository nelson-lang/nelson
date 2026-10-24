%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [previousPosition, previousVisible] = graphics_test_reference_figure_position()
  root = groot();
  previousPosition = get(root, 'DefaultFigurePosition');
  previousVisible = get(root, 'DefaultFigureVisible');
  set(root, 'DefaultFigurePosition', [338 144 859 517], 'DefaultFigureVisible', 'off');
end
