%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('getdynlibext'), 0);
asserts.isequal(nargout('getdynlibext'), 1);
%=============================================================================
if ispc()
  asserts.isequal(getdynlibext(), '.dll');
else
  if ismac()
    asserts.isequal(getdynlibext(), '.dylib');
  else
    asserts.isequal(getdynlibext(), '.so');
  end
end
%=============================================================================
