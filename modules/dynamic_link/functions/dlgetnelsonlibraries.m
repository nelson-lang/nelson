%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = dlgetnelsonlibraries()
  c = {[modulepath(nelsonroot(),'interpreter','bin'), '/libnlsInterpreter']; ...
  [modulepath(nelsonroot(),'error_manager','bin'), '/libnlsError_manager']; ...
  [modulepath(nelsonroot(),'i18n','bin'), '/libnlsI18n']; ...
  [modulepath(nelsonroot(),'types','bin'), '/libnlsTypes']};

  if (ismodule('validators'))
    c = [c; [modulepath(nelsonroot(),'validators','bin'), '/libnlsValidators']];
  end

  if (ismodule('f2c'))
    c = [c; [modulepath(nelsonroot(),'f2c','bin'), '/libnlsf2c']];
  end

  if (ispc())
    c = [c; [modulepath(nelsonroot(),'nelson','bin'), '/libnlsblaslapack']];
  end
end
%=============================================================================
