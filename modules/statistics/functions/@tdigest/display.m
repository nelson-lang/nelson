%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(varargin)
  narginchk(1, 2);
  nargoutchk(0, 0);
  fmt = format();
  LineSpacing = fmt.LineSpacing;
  td = varargin{1};
  if ~isa(td, 'tdigest')
    error(_("First argument must be a 'tdigest' object."));
  end
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
  disp(td);
end
%=============================================================================
