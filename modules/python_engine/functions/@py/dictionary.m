%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = dictionary(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1)
  obj = varargin{1};
  if ~isa(obj, 'py.dict')
    error(_('py.dict expected.'));
  end
  st = struct(obj);
  names = string(fieldnames(st));
  values = cell(size(names));
  for i = 1:numel(names)
    values{i} = st.(names{i});
  end
  varargout{1} = dictionary(names, values);
end
