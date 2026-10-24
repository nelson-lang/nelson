%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compare conjugate-gradient convergence with and without preconditioning.
matrixSize = 160;
offDiagonal = -ones(matrixSize - 1, 1);
systemMatrix = spdiags([offDiagonal; 0], -1, matrixSize, matrixSize) + ...
  spdiags(4 * ones(matrixSize, 1), 0, matrixSize, matrixSize) + ...
  spdiags([0; offDiagonal], 1, matrixSize, matrixSize);
expectedSolution = ones(matrixSize, 1);
rightHandSide = systemMatrix * expectedSolution;

tolerance = 1e-10;
maximumIterations = 200;
[plainSolution, plainFlag, plainResidual, plainIterations] = ...
  pcg(systemMatrix, rightHandSide, tolerance, maximumIterations);

factor = ichol(systemMatrix);
[preparedSolution, preparedFlag, preparedResidual, preparedIterations] = ...
  pcg(systemMatrix, rightHandSide, tolerance, maximumIterations, factor, factor');

fprintf('Without preconditioner: %d iterations, residual %.3e, flag %d\n', ...
  plainIterations, plainResidual, plainFlag);
fprintf('With incomplete Cholesky: %d iterations, residual %.3e, flag %d\n', ...
  preparedIterations, preparedResidual, preparedFlag);
fprintf('Maximum solution errors: %.3e without, %.3e with preconditioner\n', ...
  norm(plainSolution - expectedSolution, Inf), ...
  norm(preparedSolution - expectedSolution, Inf));
%=============================================================================
