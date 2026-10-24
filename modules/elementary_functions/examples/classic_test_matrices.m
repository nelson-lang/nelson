%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Construct classic matrices used in numerical experiments.
matrixOrder = 5;
hilbertMatrix = hilb(matrixOrder);
inverseHilbertMatrix = invhilb(matrixOrder);
hilbertInverseError = norm(hilbertMatrix * inverseHilbertMatrix - eye(matrixOrder), 1);
pascalMatrix = pascal(matrixOrder);
magicMatrix = magic(matrixOrder);
magicRowSums = sum(magicMatrix, 2);

disp('Hilbert inverse residual:');
disp(hilbertInverseError);
disp('Pascal matrix:');
disp(pascalMatrix);
disp('Magic-square row sums:');
disp(magicRowSums);
