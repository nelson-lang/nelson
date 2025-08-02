%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = ismethod(H, methodName)
  if ~isscalar(H)
    error(_('Wrong size for argument #1: scalar expected.'));
  end
  if (strcmp(class(H), 'MPI_Comm') == false)
    error(_('Wrong type for argument #1: MPI_Comm expected.'));
  end
  if ~ischar(methodName)
    error(_('Wrong type for argument #2: string expected.'));
  end
  r = false;
end
%=============================================================================
