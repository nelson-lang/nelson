%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = nig_check_struct_function(NIG_FUNCTION)
  r = false;
  names = fieldnames(NIG_FUNCTION);
  supported_names = {'NELSON_NAME'; 'SYMBOL'; 'LANGUAGE'; 'VARIABLES'};
  r = all(isfield(NIG_FUNCTION, supported_names));
  if r == true
    if ~ischar(NIG_FUNCTION.NELSON_NAME)
      r = false;
      return;
    end
    if ~ischar(NIG_FUNCTION.SYMBOL)
      r = false;
      return;
    end
    if ~ischar(NIG_FUNCTION.LANGUAGE)
      r = false;
      return;
    end
    if ~isstruct(NIG_FUNCTION.VARIABLES)
      r = false;
      return;
    end
  end
end
%=============================================================================
