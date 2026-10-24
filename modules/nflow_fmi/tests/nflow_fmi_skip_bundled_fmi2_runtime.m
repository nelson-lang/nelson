%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
function tf = nflow_fmi_skip_bundled_fmi2_runtime()
  % the bundled FMI 2.0 reference FMUs only carry x86-64 binaries (win64, darwin64, linux64)
  if ismac()
    tf = strcmp(computer('arch'), 'maca64');
  elseif isunix()
    [status, machine] = system('uname -m');
    tf = (status == 0) && ~strcmp(strtrim(machine), 'x86_64');
  else
    tf = false;
  end
end
%=============================================================================
