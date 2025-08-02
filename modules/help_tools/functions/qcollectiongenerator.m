%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function qcollectiongenerator(varargin)
  % internal function not documented
  if nargin() == 0
    src_file = [nelsonroot(), '/modules/help_tools/resources/', 'nelson_help_collection.qhcp'];
    dst_file = [nelsonroot(), '/modules/help_tools/resources/', 'nelson_help_collection.qhc'];
  else
    if (nargin() ~= 2)
      error(_('Wrong number of input arguments.'));
    end
    src_file = varargin{1};
    dst_file = varargin{2};
  end
  
  cmd = ['qcollectiongenerator', ' "', src_file, '"', ' -o "', dst_file, '"'];
  [res, msg] = unix(cmd);
  if res
    error(msg)
  end
end
