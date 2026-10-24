%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [faces, vertices] = shrinkfacesDataFromStruct(fv)
  faces = shrinkfacesStructField(fv, 'faces', 'Faces');
  vertices = shrinkfacesStructField(fv, 'vertices', 'Vertices');
end
%=============================================================================
function value = shrinkfacesStructField(fv, lowerName, upperName)
  if isfield(fv, lowerName)
    value = fv.(lowerName);
  elseif isfield(fv, upperName)
    value = fv.(upperName);
  else
    error('Nelson:shrinkfacesDataFromStruct:invalidStructure', 'Structure must contain faces and vertices fields.');
  end
end
%=============================================================================
