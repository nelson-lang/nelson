%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = __subsref__(varargin)
  A = varargin{1};
  S = varargin{2};
  for k = 1:numel(S)
    switch (S(k).type)
      case '()'
        R = {A(S(k).subs{:})};
      case '{}'
        R = {A{S(k).subs{:}}};
      case '.'
        R = {A.(S(k).subs)};
      otherwise
        error(_('Illegal indexing structure argument: type ''.'', ''{}'' or ''()'' expected.'));
    end
   end
   varargout = R;
end
%=============================================================================
