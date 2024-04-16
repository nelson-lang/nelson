%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pyenv(varargin)
  nargoutchk(0, 1);
  narginchk(0, 2);

  currentPythonEnvironment = __pyenv__();

  switch nargin
    case 0
      varargout{1} = currentPythonEnvironment;
    case 2
      pythonScriptsPath = [modulepath('python_engine','root'), '/resources/python/'];
      if ~isdir(pythonScriptsPath)
        error(_('Python scripts are missing.'));
      end

      name = varargin{1};
      mustBeTextScalar(name, 1); 
      mustBeMember(convertStringsToChars(name), {'Version'}, 1);
      pythonExecutablePath = varargin{2};
      mustBeTextScalar(pythonExecutablePath, 2);

      if currentPythonEnvironment.Status ~= "NotLoaded"
        error(_('The Python path cannot be modified if Python library is loaded.'));
      end

      if isempty(pythonExecutablePath)
        varargout{1} = __pyenv__('', '', '', '');    
        return
      end
      if ~isfile(pythonExecutablePath)
        if ispc()
          pythonExecutablePath = getExecutablePathFromWindowsRegistry(pythonExecutablePath);
          if isempty(pythonExecutablePath) || ~isfile(pythonExecutablePath)
            error(_('Cannot find specified version.'));
          end
        else
          error(_('valid executable filename expected.'));
        end
      end
      python_version = getPythonCommandResult(pythonExecutablePath, [pythonScriptsPath, 'findPythonVersion.py']);        
      if ~semver(python_version, '>=3.10.0')
        error(_('Python version is not supported (at least 3.10.0 expected).'));
      end
      python_architecture = getPythonCommandResult(pythonExecutablePath, [pythonScriptsPath, 'findPythonArchitecture.py']);
      python_is64bit = strcmpi(python_architecture, 'true');
      compiler_info = version('-compiler');
      nelson_is64bit = strcmpi(compiler_info(3), '64');
      if (~python_is64bit || ~nelson_is64bit) && (python_is64bit || nelson_is64bit)
        error(_('Python and Nelson must use the same architecture.'))
      end
      python_executable = getPythonCommandResult(pythonExecutablePath, [pythonScriptsPath, 'findPythonExecutable.py']);
      python_home = getPythonCommandResult(pythonExecutablePath, [pythonScriptsPath, 'findPythonHome.py']);
      python_library = getPythonCommandResult(pythonExecutablePath, [pythonScriptsPath, 'findPythonLibrary.py']);
      
      varargout{1} = __pyenv__(python_version, python_executable, python_library, python_home);
    otherwise
      error(_('Wrong number of input arguments.'));
  end
end
%=============================================================================
function result = getPythonCommandResult(python, pythonScript)
  if ispc()
    cmd = ['"', convertStringsToChars(python), '"', ' ', '"', convertStringsToChars(pythonScript), '"'];
  else
    cmd = [convertStringsToChars(python), ' ', convertStringsToChars(pythonScript)];
  end
  [status, result] = unix(cmd);
  if (status ~= 0)
    error(_('Impossible to call Python. Check path.'));
  end
  result = strtrim(result);
  if isempty(result) || strcmpi(result, 'none')
    error(_('Python returns no value.'));
  end
end
%=============================================================================
function executablePath = getExecutablePathFromWindowsRegistry(vstr)
  executablePath = [];
  V = sscanf(vstr, '%d.%d');
  if isempty(V) || isscalar(V)
    error(_('valid version or executable filename expected.'));
  end
  pyversion = sprintf('%d.%d', V(1:2));
  try
    executablePath = winqueryreg('HKCU', ['SOFTWARE\Python\PythonCore\', pyversion, '\InstallPath'], 'ExecutablePath');
  catch
    executablePath = [];
  end
  if isempty(executablePath)
    try
      executablePath = winqueryreg('HKLM', ['SOFTWARE\Python\PythonCore\', pyversion, '\InstallPath'], 'ExecutablePath');
    catch
      executablePath = [];
    end
  end
end
%=============================================================================
