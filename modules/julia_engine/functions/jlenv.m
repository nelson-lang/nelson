%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = jlenv(varargin)
  nargoutchk(0, 1);
  narginchk(0, 2);

  currentJuliaEnvironment = __jlenv__();

  switch nargin
    case 0
      varargout{1} = currentJuliaEnvironment;
    case 2
      juliaScriptsPath = [modulepath('julia_engine','root'), '/resources/julia/'];
      if ~isdir(juliaScriptsPath)
        error(_('Julia scripts are missing.'));
      end

      name = varargin{1};
      mustBeTextScalar(name, 1); 
      mustBeMember(convertStringsToChars(name), {'Version', 'version'}, 1);
      juliaExecutablePath = varargin{2};
      mustBeTextScalar(juliaExecutablePath, 2);

      if currentJuliaEnvironment.Status ~= "NotLoaded"
        error(_('The Julia path cannot be modified if Julia library is loaded.'));
      end

      if isempty(juliaExecutablePath)
        varargout{1} = __jlenv__('', '', '', '');    
        return
      end
      julia_version = getJuliaCommandResult(juliaExecutablePath, [juliaScriptsPath, 'findJuliaVersion.jl']);        
      if ~semver(julia_version, '>=1.10.0')
        error(_('Julia version is not supported (at least 1.10.0 expected).'));
      end
      julia_architecture = getJuliaCommandResult(juliaExecutablePath, [juliaScriptsPath, 'findJuliaArchitecture.jl']);
      julia_is64bit = strcmpi(julia_architecture, 'true');
      compiler_info = version('-compiler');
      nelson_is64bit = strcmpi(compiler_info(3), '64');
      if (~julia_is64bit || ~nelson_is64bit) && (julia_is64bit || nelson_is64bit)
        error(_('Julia and Nelson must use the same architecture.'))
      end
      julia_executable = getJuliaCommandResult(juliaExecutablePath, [juliaScriptsPath, 'findJuliaExecutable.jl']);
      julia_home = getJuliaCommandResult(juliaExecutablePath, [juliaScriptsPath, 'findJuliaHome.jl']);
      julia_library = getJuliaCommandResult(juliaExecutablePath, [juliaScriptsPath, 'findJuliaLibrary.jl']);
      
      varargout{1} = __jlenv__(julia_version, julia_executable, julia_library, julia_home);
    otherwise
      error(_('Wrong number of input arguments.'));
  end
end
%=============================================================================
function result = getJuliaCommandResult(julia, juliaScript)
  if ispc()
    cmd = ['"', convertStringsToChars(julia), '"', ' ', '"', convertStringsToChars(juliaScript), '"'];
  else
    cmd = [convertStringsToChars(julia), ' ', convertStringsToChars(juliaScript)];
  end
  [status, result] = unix(cmd);
  if (status ~= 0)
    error(_('Impossible to call Julia. Check path.'));
  end
  result = strtrim(result);
  if isempty(result) || strcmpi(result, 'none')
    error(_('Julia returns no value.'));
  end
end
%=============================================================================
