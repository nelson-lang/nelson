%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tform = projective2d(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);
  if nargin == 0
    T = eye(3);
  else
    T = varargin{1};
  end
  if ~isnumeric(T) || ~isreal(T)
    error('Nelson:image_processing:transformationMatrixNumericReal', 'Transformation matrix must be numeric and real.');
  end
  T = double(T);
  if ~isequal(size(T), [3 3])
    error('Nelson:image_processing:projectiveTransformationMatrix', 'Projective transformation matrix must be 3-by-3.');
  end
  if any(~isfinite(T(:))) || abs(det(T)) <= eps
    error('Nelson:image_processing:transformationMatrixFiniteNonsingular', 'Transformation matrix must be finite and nonsingular.');
  end
  tform = struct('Type', 'projective', 'Dimensionality', 2, 'T', T);
end
%=============================================================================
