%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, message] = cmake(varargin)
  status = false;
  message = '';
  [status, pathcmake] = findcmake();
  if status == false
    error('Nelson:cmake:notFound', 'cmake not found.');
  end
  cmd = ['"', pathcmake, '"'];
  for k = [1:length(varargin)]
    if ~ischar(varargin{k})
      error('Nelson:types:onlyStringExpected', 'only string expected as input argument.');
    end
    cmd = [cmd, ' ', varargin{k}];
  end
  % Guard against a polluted RC environment variable. cmake reads RC as the
  % resource compiler; if an install script leaks a robocopy exit code (e.g.
  % RC=2) configuration aborts with "Could not find the compiler specified in
  % the environment variable RC". When RC does not point to an existing file,
  % clear it for this call so cmake auto-detects rc.exe from the toolchain.
  previousRC = getenv('RC');
  if ~isempty(previousRC) && ~isfile(previousRC)
    setenv('RC', '');
    restoreRC = onCleanup(@() setenv('RC', previousRC)); %#ok<NASGU>
  end
  [status, message] = unix(cmd);
  status = (status == 0);
  message = cleanup(message);
end
%=============================================================================
function msgout = cleanup(msgin)
  msgout = replace(msgin, ['Failed to create ConsoleBuf!', char(10)], '');
  msgout = replace(msgout, ['setActiveInputCodepage failed!', char(10)], '');
end
%=============================================================================
