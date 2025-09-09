%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = properties(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  obj = varargin{1};
  if ~isa(obj, 'tdigest')
    msg = _("'tdigest' class expected.");
    error(msg);
  end
  properties_list = {'compression', 'totalWeight'};
  
  varargout = {};
  if nargout == 1
    varargout{1} = properties_list';
    return
  end
  fmt = format();
  LineSpacing = fmt.LineSpacing;
  if strcmp(LineSpacing, 'loose')
    fprintf('\n')
  end
  fmtmsg = _('Properties for class %s:\n');
  msg = sprintf(fmtmsg, class(obj));
  fprintf(msg)
  if strcmp(LineSpacing, 'loose')
    fprintf('\n')
  end
  for e = properties_list
    fprintf('    %s\n', e{1});
  end
  if strcmp(LineSpacing, 'loose')
    fprintf('\n')
  end
end
%=============================================================================
