%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function je = tryFindJuliaEnvironment(je) %#ok<NLS0026>
  isempty(je);
  isJuliaCompatible = (computer('arch') ~= "woa64") && (computer('arch') ~= "win32");
  try
    if isJuliaCompatible
      [status, msg] = unix('julia -e "print(Base.julia_cmd()[1])"'); %#ok<NLS0004>
      if status == 0
        juliaExecutable = strtrim(msg);
        if isfile(juliaExecutable)
          je = jlenv('Version', juliaExecutable);
        end
      end
    end
  catch
  end
end
