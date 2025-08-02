%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function f = setfield(s, varargin)
  if ~(isstruct(s) || (iscell(s) && isempty(s)) || isempty(s))
    error(_('Invalid Input struct expected.'));
  end
  if isempty(varargin)
    error(_('Wrong numbers of input arguments.'));
  end
  f = s;
  for i = 1:2:length(varargin)
    fieldname = varargin{i};
    if isstring(fieldname) && isscalar(fieldname)
      fieldname = convertStringsToChars(fieldname);
    end
    if ~ischar(fieldname)
      error(_('Input should be a string or character array.'));
    end
    fieldvalue = varargin{i + 1};
    f.(fieldname) = fieldvalue;
  end
end
%=============================================================================
