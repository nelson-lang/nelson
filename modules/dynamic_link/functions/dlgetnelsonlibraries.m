%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = dlgetnelsonlibraries()
  nelsonLibrariesPath = modulepath('nelson', 'builtin');
  c = {[nelsonLibrariesPath, '/libnlsInterpreter']; ...
  [nelsonLibrariesPath, '/libnlsError_manager']; ...
  [nelsonLibrariesPath, '/libnlsI18n']; ...
  [nelsonLibrariesPath, '/libnlsTypes']};
  
  if (ismodule('validators'))
    c = [c; [nelsonLibrariesPath, '/libnlsValidators']];
  end
  
  if (ismodule('f2c'))
    c = [c; [nelsonLibrariesPath, '/libnlsF2C']];
  end
  
  if (ispc())
    c = [c; [nelsonLibrariesPath, '/libnlsblaslapack']];
  end
end
%=============================================================================
