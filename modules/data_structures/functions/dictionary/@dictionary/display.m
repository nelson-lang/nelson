%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(varargin)
  narginchk(1, 2);
  fmt = format();
  LineSpacing = fmt.LineSpacing;
  obj = varargin{1};
  if (nargin == 1)
    name = inputname(1);
  else
    name = varargin{2};
  end
  if strcmp(LineSpacing, 'loose')
    msgfmt = '\n%s = \n';
  else
    msgfmt = '%s = \n';
  end
  msg = sprintf(msgfmt, name);
  fprintf(msg);
  disp(obj);
end
