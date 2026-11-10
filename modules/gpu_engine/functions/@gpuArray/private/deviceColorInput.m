%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [onDevice, isMap] = deviceColorInput(rgb)
  % real float m-by-3 colormaps and m-by-n-by-3 images
  s = size(rgb);
  isMap = numel(s) == 2 && s(2) == 3;
  isImage = numel(s) == 3 && s(3) == 3;
  isFloat = any(strcmp(underlyingType(rgb), {'single', 'double'}));
  onDevice = ~isempty(rgb) && isFloat && (isMap || isImage) && isreal(rgb);
end
%=============================================================================
