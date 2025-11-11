%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function subspaceAngle = subspace(varargin)
    % Validate number of inputs/outputs
    narginchk(2,2);
    nargoutchk(0, 1);

    matrixA = varargin{1};
    matrixB = varargin{2};
    
    % Validate that inputs are matrices
    mustBeMatrix(matrixA, 1);
    mustBeMatrix(matrixB, 2);

    % Compute orthonormal bases for the column spaces of the inputs
    orthBasisA = orth(matrixA);
    orthBasisB = orth(matrixB);

    % Ensure orthBasisA has at least as many columns as orthBasisB.
    % If not, swap them so we always project the smaller onto the larger.
    if size(orthBasisA,2) < size(orthBasisB,2)
        [orthBasisA, orthBasisB] = deal(orthBasisB, orthBasisA);
    end

    % Project orthBasisB onto the orthogonal complement of orthBasisA.
    % This removes the components of orthBasisB lying in the span of orthBasisA.
    projectedBasisB = orthBasisB - orthBasisA * (orthBasisA' * orthBasisB);

    % The subspace angle is the arcsine of the largest singular value (2-norm)
    % of the projection. Clip to 1 to avoid numerical issues from rounding.
    subspaceAngle = asin(min(1, norm(projectedBasisB)));
end
%=============================================================================

