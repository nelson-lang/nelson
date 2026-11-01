%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opentoline(filename, linenumber, columnnumber)
  % opentoline(filename, linenumber[, columnnumber]) opens the file in the editor at that position
  if nargin < 2 || nargin > 3
    error('Nelson:opentoline:nargin', _('Wrong number of input arguments.'));
  end
  if ~((ischar(filename) && isrow(filename)) || isStringScalar(filename))
    error('Nelson:opentoline:filename', _('A file name is expected as first argument.'));
  end
  if ~(isnumeric(linenumber) && isscalar(linenumber) && linenumber >= 1)
    error('Nelson:opentoline:line', _('A line number of 1 or more is expected as second argument.'));
  end
  if nargin == 2
    editor(char(filename), double(linenumber));
    return
  end
  if ~(isnumeric(columnnumber) && isscalar(columnnumber) && columnnumber >= 1)
    error('Nelson:opentoline:column', _('A column number of 1 or more is expected as third argument.'));
  end
  editor(char(filename), double(linenumber), double(columnnumber));
end
%=============================================================================
