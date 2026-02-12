%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ismethod(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  method_name = varargin{2};
  obj = varargin{1};
  if ~ischar(method_name) && ~isstring(method_name)
    error(_('Method name must be a string or character array.'));
  end
  method_name = convertStringsToChars(method_name);
  methods_list = methods(obj);
  varargout{1} = any(strcmp(method_name, methods_list));
end
%=============================================================================
