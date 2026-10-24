%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = is_native_platform() %#ok<NLS0026>
  persistent is_native %#ok<NLS0005>
  if isempty(is_native)
    if ispc()
      ver_comp = version('-compiler');
      targetAMD64 = startsWith(ver_comp{3}, '64');
      targetX86 = startsWith(ver_comp{3}, '32');
      arch = getenv('PROCESSOR_ARCHITECTURE');
      archW6432 = getenv('PROCESSOR_ARCHITEW6432');
      if targetX86
        is_native = strcmp(arch, 'x86') && strcmp(archW6432, '');
      elseif targetAMD64
        is_native = strcmp(arch, 'AMD64') && strcmp(archW6432, '');
      else
        is_native = false;
      end
    else
      is_native = true;
    end
  end
  r = is_native;
end
