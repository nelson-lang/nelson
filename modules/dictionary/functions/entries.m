%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = entries(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  obj = varargin{1};
  mustBeA(obj, 'dictionary', 1);
  if (nargin < 2)
    fmt = 'table'; 
  else
    fmt = varargin{2};
    mustBeTextScalar(fmt, 2);
  end
  k = keys(obj, 'cell');
  v = values(obj, 'cell');
  if (startsWith('cell', fmt, 'IgnoreCase', true))
    r = [k, v];
  elseif (startsWith ('struct', fmt, 'IgnoreCase', true))
    r = struct('Key', k, 'Value', v);
  elseif (startsWith('table', fmt, 'IgnoreCase', true))
    error(_("'table' format not yet implemented."));
  else
    error("Wrong value for 'format' option. 'struct', 'cell' or 'table' expected.");
    end
    varargout{1} = r;
  end
  %=============================================================================
