%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nig_language_function_declaration(NIG_FUNCTION)
  txt = '';
  if strcmp(NIG_FUNCTION.LANGUAGE, 'fortran') == true
    txt = ['int ', NIG_FUNCTION.SYMBOL, '_'];
  else
    txt = ['int ', NIG_FUNCTION.SYMBOL];
  end
  txt = [txt, ' ('];
  for k = NIG_FUNCTION.VARIABLES(:)'
    txt = [txt, nig_variable_by_type(k, true), ','];
  end
  if txt(end) == ','
    txt = txt(1:end - 1);
  end
  txt = [txt, ')'];
end
%=============================================================================
