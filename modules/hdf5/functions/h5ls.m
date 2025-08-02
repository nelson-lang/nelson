%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = h5ls(varargin)
  if (nargin > 2 || nargin == 0)
    error(_('Wrong number of input arguments.'));
  end
  if nargout > 1
    error(_('Wrong number of output arguments.'));
  end
  h5filename = string(varargin{1});
  if nargin == 2
    h5path = string(varargin{2});
    if ~startsWith(h5path, '/')
      error(_('Second argument must start with ''/''.'));
    end
  else
    h5path = "";
  end
  if ispc()
    exe = ['"', modulepath('nelson', 'bin'), '/', 'h5ls', '"'];
  else
    exe = "h5ls";
  end
  if (h5path == "")
    if ispc()
      cmd = [exe + " -r -f -v " + """" + h5filename + """"];
    else
      cmd = [exe + " -r -f -v " + h5filename];
    end
  else
    if ispc()
      cmd = [exe + " -r -f -v " + """" + h5filename + h5path + """"];
    else
      cmd = [exe + " -r -f -v " + h5filename + h5path];
    end
  end
  cmd = cmd{1};
  [status, msg] = unix(cmd);
  if status ~= 0
    error(msg);
  else
    if (nargout == 0)
      disp(msg);
    else
      msg = charToCell(msg);
      % remove first line
      msg = msg(2: length(msg), 1);
      msg = filterOutput(msg);
      varargout{1} = splitAsCell(msg);
    end
  end
end
%=============================================================================
function ce = charToCell(str)
  tokens = strfind(str, newline());
  ce = {};
  f = 1;
  len = length(str);
  for q = tokens
    if (f > len)
      s = len;
    else
      s = f;
    end
    if (q - 1 > len)
      e = len;
    else
      e = q - 1;
    end
    ce = [ce; str(s:e)];
    f = q + 1;
  end
end
%=============================================================================
function res = filterOutput(ce)
  res = {};
  for k = 1:length(ce)
    line = ce{k};
    if startsWith(line, '/') || contains(line, 'Group') || contains(line, 'Attribute:')
      tokens = strfind(line, ' {');
      if ~isempty(tokens)
        line = line(1: tokens(1) - 1);
      end
      line = strtrim(line);
      if startsWith(line, 'Attribute: ')
        line = replace(line, 'Attribute: ', '');
        line = [line, ' ', 'Attribute'];
      end
      res = [res; line];
    end
  end
end
%=============================================================================
function res = splitAsCell(ce)
  res = {};
  for k = 1:length(ce)
    line = ce{k};
    element = {};
    if endsWith(line, ' Group')
      line = replace(line, ' Group', '');
      element = {strtrim(line), 'Group'};
    end
    if endsWith(line, ' Dataset')
      line = replace(line, ' Dataset', '');
      element = {strtrim(line), 'Dataset'};
    end
    if endsWith(line, ' Attribute')
      line = replace(line, ' Attribute', '');
      if endsWith(line, ' scalar')
        line = replace(line, ' scalar', '');
      end
      element = {strtrim(line), 'Attribute'};
    end
    if ~isempty(element)
      res = [res; element];
    end
  end
end
%=============================================================================
