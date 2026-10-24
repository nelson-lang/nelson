%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function eccentricity = axes2ecc(semimajor, semiminor)
  narginchk(1, 2);
  if nargin == 1
    v = semimajor;
    if ~isnumeric(v)
      error('Nelson:axes2ecc:inputsMustBeNumeric', 'Semi-axes must be numeric.');
    end
    if ndims(v) == 2 && size(v, 2) == 2
      semimajor = v(:, 1);
      semiminor = v(:, 2);
    elseif numel(v) == 2
      semimajor = v(1);
      semiminor = v(2);
    else
      error('Nelson:axes2ecc:invalidInput', ...
        'A single input must be a two-column matrix or a two-element vector.');
    end
  else
    if ~isnumeric(semimajor) || ~isnumeric(semiminor)
      error('Nelson:axes2ecc:inputsMustBeNumeric', 'Semi-axes must be numeric.');
    end
  end
  % Eccentricity of an ellipse from its semimajor and semiminor axes.
  eccentricity = sqrt(1 - (semiminor ./ semimajor) .^ 2);
end
%=============================================================================
