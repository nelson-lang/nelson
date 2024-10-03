%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = table2array(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  T = varargin{1};
  if ~istable(T)
    error(_('T must be a table.'));
  end
  c = [];
  st = struct(T);
  for j=1:width(T)
    name = st.Properties.VariableNames{j};
    a = st.data.(name);
    if (~isnumeric(a) && ~islogical(a))
      error(_('numeric or logical expected.'));
    end
    c = [c, a];
  end
  varargout{1} = c;
end
%=============================================================================
