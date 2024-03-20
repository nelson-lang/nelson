%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('python_engine')
  exit(0);
end
pe = pyenv();
if ~(pe.Version == "")
 pe
 exit(0);
end

pythonExecutable = getenv('NLS_DEFAULT_PYTHON_EXECUTABLE_TESTS');
if ~isempty(pythonExecutable) && isfile(pythonExecutable)
  try
    pe = pyenv('Version', pythonExecutable);
  catch
  end
end
if ~(pe.Version == "")
 exit(0);
end

Python_ROOT_DIR = getenv('Python_ROOT_DIR');
if ispc()
  name = 'python';
else
  name = 'python3';
end
pythonExecutable = [Python_ROOT_DIR, '/', name];
if isfile(pythonExecutable)
  try
    pe = pyenv('Version', pythonExecutable);
  catch
  end
end
if (pe.Version == "")
  if ispc()
    [status, msg] = unix('python -c "import sys;print(sys.executable)"');
  else
    [status, msg] = unix("python3 -c 'import sys;print(sys.executable)'");
  end

  if status == 0
    pythonExecutable = strtrim(msg);
    if isfile(pythonExecutable)
      try
        pe = pyenv('Version', pythonExecutable);
      catch
      end
    end
  end
end
pe
exit(pe.Version == "");


