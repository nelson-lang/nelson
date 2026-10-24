%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function vertices = stream3(varargin)
  % vertices = stream3(X, Y, Z, U, V, W, startx, starty, startz)
  % vertices = stream3(U, V, W, startx, starty, startz)
  % vertices = stream3(..., options)
  narginchk(6, 10);
  nargoutchk(0, 1);
  dataArguments = varargin;
  rejectStreamPropertyArguments(dataArguments);
  [vertices, field] = streamFieldVertices(dataArguments);
  if (field.Dimension ~= 3)
    error('Nelson:stream3:threeDimensionalFieldExpected', ...
      'A three dimensional vector field is expected.');
  end
  % One row cell per start point, as the start points were given.
  vertices = vertices(:).';
end
%=============================================================================
