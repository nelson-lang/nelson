%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_build_help(_MODULE_NAME, _MODULE_PATH)
  if ~ismodule(_MODULE_NAME)
    try
      run([_MODULE_PATH '/loader.m']);
      loaded = true;
    catch
      loaded = false;
    end
  else
    loaded = true;
  end
  if loaded
    buildhelp(_MODULE_NAME);
    removemodule(_MODULE_NAME);
  end
end
%=============================================================================
