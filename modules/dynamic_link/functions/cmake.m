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
    error(_('cmake not found.'));
  end
  cmd = ['"', pathcmake, '"'];
  for k = [1:length(varargin)]
    if ~ischar(varargin{k})
      error(_('only string expected as input argument.'));
    end
    cmd = [cmd, ' ', varargin{k}];
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
