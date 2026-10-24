%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, z, V, vertices, patchHandle, negate] = parseIsonormalsInputs(args)
  [args, negate] = parseIsonormalsOption(args);
  patchHandle = [];
  if numel(args) == 2
    V = validateIsosurfaceVolume(args{1});
    [x, y, z] = defaultIsonormalsGrid(V);
    [vertices, patchHandle] = parseIsonormalsTarget(args{2});
  elseif numel(args) == 5
    V = validateIsosurfaceVolume(args{4});
    [x, y, z] = validateIsosurfaceGrid(args{1}, args{2}, args{3}, V);
    [vertices, patchHandle] = parseIsonormalsTarget(args{5});
  elseif numel(args) < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  elseif numel(args) > 5
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  else
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  validateIsonormalsVertices(vertices);
end
%=============================================================================
