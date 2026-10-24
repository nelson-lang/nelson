%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [faces, vertices, sf, patchHandle] = parseShrinkfacesInputs(args)
  sf = 0.3;
  patchHandle = [];
  if isscalar(args{1}) && isgraphics(args{1}, 'patch')
    [faces, vertices, sf, patchHandle] = parseShrinkfacesPatchInput(args);
  elseif isstruct(args{1})
    [faces, vertices, sf] = parseShrinkfacesStructInput(args);
  elseif numel(args) == 3
    faces = args{1};
    vertices = args{2};
    sf = validateShrinkFactor(args{3});
  else
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  validateShrinkfacesData(faces, vertices);
end
%=============================================================================
function [faces, vertices, sf, patchHandle] = parseShrinkfacesPatchInput(args)
  patchHandle = args{1};
  faces = patchHandle.Faces;
  vertices = patchHandle.Vertices;
  sf = 0.3;
  if numel(args) == 2
    sf = validateShrinkFactor(args{2});
  elseif numel(args) > 2
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
end
%=============================================================================
function [faces, vertices, sf] = parseShrinkfacesStructInput(args)
  [faces, vertices] = shrinkfacesDataFromStruct(args{1});
  sf = 0.3;
  if numel(args) == 2
    sf = validateShrinkFactor(args{2});
  elseif numel(args) > 2
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
end
%=============================================================================
