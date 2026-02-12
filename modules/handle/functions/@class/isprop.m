%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isprop(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
   property_name = varargin{2};
  obj = varargin{1};
  if ~ischar(property_name) && ~isstring(property_name)
    error(_('Property name must be a string or character array.'));
  end
  property_name = convertStringsToChars(property_name);
  properties_list = properties(obj);
  varargout{1} = any(strcmp(property_name, properties_list));
end
%=============================================================================