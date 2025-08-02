%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('julia_engine')
  exit(0);
end
je = jlenv();
if ~(je.Version == "")
 je
 exit(0);
end

juliaExecutable = getenv('NLS_DEFAULT_JULIA_EXECUTABLE_TESTS');
if ~isempty(juliaExecutable) && isfile(juliaExecutable)
  try
    je = jlenv('Version', juliaExecutable);
  catch
  end
end
if ~(je.Version == "")
 exit(0);
end

if (je.Version == "")
  [status, msg] = unix('julia -e "print(Base.julia_cmd()[1])"');
  if status == 0
    juliaExecutable = strtrim(msg);
    if isfile(juliaExecutable)
      try
        je = jlenv('Version', juliaExecutable);
      catch
      end
    end
  end
end

je
exit(je.Version == "");
%=============================================================================
