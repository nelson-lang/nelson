%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_get_dimension_m_as_string(VARIABLE)
  if isnumeric(VARIABLE.DIMENSION_M)
    txt = int2str(VARIABLE.DIMENSION_M);
  else
    txt = VARIABLE.DIMENSION_M;
  end
end
%=============================================================================
