%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_get_dimension_n_as_string(VARIABLE)
  if isnumeric(VARIABLE.DIMENSION_N)
    txt = int2str(VARIABLE.DIMENSION_N);
  else
    txt = VARIABLE.DIMENSION_N;
  end
end
%=============================================================================
