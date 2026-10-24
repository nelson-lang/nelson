%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function native_mex_entry()
  value = nativepkg.nc_native_mex(7);
  if value ~= 17
    error('Incorrect native function result.');
  end
  fprintf('MEX_VALUE=%d\n', value);
  pause(2);
end
