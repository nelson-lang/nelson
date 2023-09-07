%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = isprop(H, propertyName)
  if ~isscalar(H)
    error(_('Wrong size for argument #1: scalar expected.'));
  end
  if (strcmp(class(H), 'MPI_Comm') == false)
    error(_('Wrong type for argument #1: MPI_Comm expected.'));
  end
  if ~ischar(propertyName)
    error(_('Wrong type for argument #2: string expected.'));
  end
  r = false;
end
%=============================================================================
