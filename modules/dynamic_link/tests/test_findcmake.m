%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[status, pathcmake] = findcmake();
if status
  assert_istrue(status);
  assert_istrue(isfile(pathcmake));
else
  assert_isfalse(status);
  assert_isequal(pathcmake, '');
end
%=============================================================================