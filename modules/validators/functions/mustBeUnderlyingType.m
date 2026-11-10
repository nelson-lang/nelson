%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mustBeUnderlyingType(A, typename)
  narginchk(2, 2);
  mustBeTextScalar(typename, 2);
  typename = char(typename);
  if ~strcmp(underlyingType(A), typename)
    error('Nelson:validators:mustBeUnderlyingType', 'Value must be of underlying type ''%s''.', typename);
  end
end
%=============================================================================
