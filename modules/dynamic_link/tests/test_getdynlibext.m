%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('getdynlibext'), 0);
assert_isequal(nargout('getdynlibext'), 1);
%=============================================================================
if ispc()
  assert_isequal(getdynlibext(), '.dll');
else
  if ismac()
    assert_isequal(getdynlibext(), '.dylib');
  else
    assert_isequal(getdynlibext(), '.so');
  end
end
%=============================================================================
