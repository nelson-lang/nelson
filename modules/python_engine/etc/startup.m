%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addgateway(modulepath('python_engine', 'builtin'), 'python_engine');
addpath(modulepath('python_engine', 'functions'), '-frozen');
%=============================================================================
% use embedded python on windows by default
if ispc()
  pe = pyenv();
  if (pe.Executable == "")
    pythonExecutable = [modulepath('python_engine', 'root'), '/thirdparty/python/python.exe'];
    if isfile(pythonExecutable)
        pyenv('Version', pythonExecutable);
    end
  end
end
%=============================================================================
