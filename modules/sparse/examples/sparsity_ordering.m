%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Reduce the bandwidth of a shuffled sparse Poisson matrix.
gridSide = 20;
gridPoints = gridSide ^ 2;
diagonalValues = ones(gridSide, 1);
oneDimensionalOperator = spdiags(...
  [-diagonalValues, 2 * diagonalValues, -diagonalValues], ...
  [-1 0 1], gridSide, gridSide);
poissonMatrix = kron(speye(gridSide), oneDimensionalOperator) ...
  + kron(oneDimensionalOperator, speye(gridSide));
shuffleOrder = mod((0:gridPoints - 1) * 37, gridPoints) + 1;
shuffledMatrix = poissonMatrix(shuffleOrder, shuffleOrder);
reverseCuthillMcKeeOrder = symrcm(shuffledMatrix);
reorderedMatrix = shuffledMatrix(...
  reverseCuthillMcKeeOrder, reverseCuthillMcKeeOrder);
shuffledBandwidth = bandwidth(shuffledMatrix);
reorderedBandwidth = bandwidth(reorderedMatrix);

if ~exist('sparsityOrderingFigureVisible', 'var')
  sparsityOrderingFigureVisible = 'on';
end
figureHandle = figure(...
  'Name', 'Sparse matrix ordering', ...
  'NumberTitle', 'off', ...
  'Visible', sparsityOrderingFigureVisible);

shuffledAxes = subplot(1, 2, 1, 'Parent', figureHandle);
axes(shuffledAxes);
spy(shuffledMatrix);
title(shuffledAxes, sprintf('Shuffled: bandwidth %d', shuffledBandwidth));

reorderedAxes = subplot(1, 2, 2, 'Parent', figureHandle);
axes(reorderedAxes);
spy(reorderedMatrix);
title(reorderedAxes, sprintf('Reordered: bandwidth %d', reorderedBandwidth));
%=============================================================================
