%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = standardizeMissing(A, indicators)
  narginchk(2, 2);
  if ~iscell(indicators)
    indicators = {indicators};
  end
  B = A;
  if istable(A)
    names = A.Properties.VariableNames;
    for k = 1:length(names)
      B.(names{k}) = standardizeArray(A.(names{k}), indicators);
    end
  else
    B = standardizeArray(A, indicators);
  end
end
%=============================================================================
function B = standardizeArray(A, indicators)
  B = A;
  mask = missingValueLocal(A, indicators);
  if isnumeric(B) && isfloat(B)
    B(mask) = NaN;
  elseif isstring(B)
    B(mask) = "";
  elseif iscell(B)
    B(mask) = {[]};
  end
end
%=============================================================================
function tf = missingValueLocal(value, indicators)
  if iscell(value)
    tf = false(size(value, 1), size(value, 2));
    for k = 1:numel(value)
      localMask = missingValueLocal(value{k}, indicators);
      tf(k) = any(localMask(:));
    end
    return
  end
  if isstring(value)
    tf = value == "";
  elseif ischar(value)
    tf = isempty(value);
  elseif isnumeric(value)
    if isfloat(value)
      tf = isnan(value);
    else
      tf = false(size(value, 1), size(value, 2));
    end
  elseif islogical(value)
    tf = false(size(value, 1), size(value, 2));
  else
    tf = false(size(value, 1), size(value, 2));
  end
  for k = 1:length(indicators)
    try
      tf = tf | (value == indicators{k});
    catch
      try
        tf = tf | strcmp(value, indicators{k});
      catch
      end
    end
  end
end
%=============================================================================
