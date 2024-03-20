%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pyargs(varargin)
  nargoutchk(0, 1);
  if (mod (nargin, 2) ~= 0)
    error(_('NAME, VALUE pairs of arguments expected.'));
  end
  try
    st = struct(varargin{:});
   catch ex
     error (_('Field names must be string scalars or character vectors.'));
   end
   varargout{1} = class(st, 'pyargs');
end