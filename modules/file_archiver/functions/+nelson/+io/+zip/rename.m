%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rename(filename, oldNames, newNames)
  % Renames entries of a ZIP file without recompressing them.
  narginchk(3, 3);
  oldList = string(oldNames);
  newList = string(newNames);
  if numel(oldList) ~= numel(newList)
    error('Nelson:io:zip:invalidOption', 'Old and new names must have the same number of elements.');
  end
  __zip_rewrite__(char(filename), strings(0, 1), oldList, newList);
end
%=============================================================================
