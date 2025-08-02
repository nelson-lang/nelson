%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = h5dump(varargin)
  if (nargin > 2 || nargin == 0)
    error(_('Wrong number of input arguments.'));
  end
  if nargout > 1
    error(_('Wrong number of output arguments.'));
  end
  h5filename = string(varargin{1});
  if nargin == 2
    h5path = string(varargin{2});
  else
    h5path = "";
  end
  if ispc()
    exe = ['"', modulepath('nelson', 'bin'), '/', 'h5dump', '"'];
  else
    exe = "h5dump";
  end
  if h5path == ""
    if ispc()
      cmd = [exe + " " + """" + h5filename + """"];
    else
      cmd = [exe + " " + h5filename];
    end
  else
    if ispc()
      cmd = [exe + " -d " + h5path + " " + """" + h5filename + """"];
    else
      cmd = [exe + " -d " + h5path + " " + h5filename];
    end
  end
  cmd = cmd{1};
  [status, msg] = unix(cmd);
  if status ~= 0
    error(msg);
  else
    if (nargout == 0)
      disp(msg)
    else
      varargout{1} = msg;
    end
  end
end
%=============================================================================
