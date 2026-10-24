%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function newCodes = convertCodes(codes, oldNames, newNames)
  % Position of each old category name in the new list (0 when it is gone);
  % newNames is unique, so ismember's match index is unambiguous.
  [~, map] = ismember(oldNames(:), newNames(:));
  map = map(:);
  % Remap every code in one gather instead of a per-element loop.
  newCodes = zeros(size(codes));
  valid = codes > 0;
  newCodes(valid) = map(codes(valid));
end
%=============================================================================
