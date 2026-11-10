%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isUnderlyingType(X, typename)
  % true when the underlying type of X is typename or belongs to the 'numeric', 'float' or 'integer' category
  narginchk(2, 2);
  mustBeTextScalar(typename, 2);
  typename = char(typename);
  t = underlyingType(X);
  integers = {'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64'};
  floats = {'double', 'single'};
  switch typename
    case 'numeric'
      tf = any(strcmp(t, [integers, floats]));
    case 'float'
      tf = any(strcmp(t, floats));
    case 'integer'
      tf = any(strcmp(t, integers));
    otherwise
      tf = strcmp(t, typename);
  end
end
%=============================================================================
