%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function vertices = stream2(varargin)
  % vertices = stream2(X, Y, U, V, startx, starty)
  % vertices = stream2(U, V, startx, starty)
  % vertices = stream2(..., options)
  narginchk(4, 7);
  nargoutchk(0, 1);
  dataArguments = varargin;
  rejectStreamPropertyArguments(dataArguments);
  [vertices, field] = streamFieldVertices(dataArguments);
  if (field.Dimension ~= 2)
    error('Nelson:stream2:twoDimensionalFieldExpected', ...
      'A two dimensional vector field is expected.');
  end
  % One row cell per start point, as the start points were given.
  vertices = vertices(:).';
end
%=============================================================================
