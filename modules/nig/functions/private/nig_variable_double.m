%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_variable_double(VARIABLE, withLocal)
  txt = '';
  if strcmp(VARIABLE.MODE, 'input')
    txt = [txt, ' double *', toupper(VARIABLE.NAME)];
  end
  if strcmp(VARIABLE.MODE, 'output')
    txt = [txt, ' double *', toupper(VARIABLE.NAME)];
  end
  if strcmp(VARIABLE.MODE, 'in_out')
    txt = [txt, ' double *', toupper(VARIABLE.NAME)];
  end
  if withLocal
    if strcmp(VARIABLE.MODE, 'local')
      txt = [txt, ' double *', toupper(VARIABLE.NAME)];
    end
  end
end
%=============================================================================
