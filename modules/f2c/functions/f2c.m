%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = f2c(varargin)
  path_f2c = [modulepath('f2c','bin'), '/nelson_f2c'];
  if ispc()
    path_f2c = [path_f2c, '.exe'];
  end
  if ~isfile(path_f2c)
    error(_('nelson_f2c not found.'))
  end
  if nargin < 2 || nargin > 3
    error(_('Wrong number of input arguments.'));
  end
  if nargout > 2
    error(_('Wrong number of output arguments.'));
  end
  src = varargin{1};
  if ~isfile(src)
    error(_('Input argument #1: existing file expected.'));
  end
  dst = varargin{2};
  if ~isdir(dst)
    error(_('Input argument #2: existing directory expected.'));
  end
  options = '';
  if nargin == 3
    options = varargin{3};
  end
  F2C_OPTIONS = ['-E -I"', modulepath('f2c','root'), 'src/include"', ' -d'];
  cmd = ['"', path_f2c, '" "', src, '"', ' ', F2C_OPTIONS, dst, ' ', options];
  [res, msg] = unix(cmd);
  if res == 0
    % replace first definition to export
    fname_f = fileparts(src, 'filename');
    fname_c = [dst , '/', fname_f, '.c'];
    txt = fileread(fname_c);
    txt_to_replace = '/* Subroutine */ int ';
    index = strfind(txt, txt_to_replace);
    if ~isempty(index)
      txt = [txt(1:index(1) - 1), 'EXPORTSYMBOL ', txt_to_replace, txt(index(1) + length(txt_to_replace): end)];
    fw = fopen(fname_c, 'w');
    fwrite(fw, txt);
    fclose(fw);
  end
end
if nargout > 0
  if res ~= 0
    varargout{1} = false;
    if nargout > 1
      varargout{2} = msg;
    end
  else
    varargout{1} = true;
    if nargout > 1
      varargout{2} = '';
    end
  end
else
  if res ~= 0
    error(msg);
  end
end
end
%=============================================================================

