%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stop = odeComplexOutputCheck(t, y, flag)
  stop = false;
  if isempty(flag)
    asserts.isequal(size(y, 1), 1);
    asserts.isfalse(isreal(y));
  elseif strcmp(flag, 'init')
    asserts.isequal(size(y, 1), 2);
    asserts.isfalse(isreal(y));
  elseif strcmp(flag, 'done')
    asserts.istrue(isempty(y));
  end
end
%=============================================================================
