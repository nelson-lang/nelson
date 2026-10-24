%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = __subsref__(varargin)
  A = varargin{1};
  S = varargin{2};
  % each indexing step applies to the result of the previous one
  R = {A};
  for k = 1:numel(S)
    switch (S(k).type)
      case '()'
        R = {A(S(k).subs{:})};
      case '{}'
        R = {A{S(k).subs{:}}};
      case '.'
        R = {A.(S(k).subs)};
      otherwise
        error('Nelson:__subsref__:expected2', '%s expected.', 'Illegal indexing structure argument: type ''.'', ''{}'' or ''()''');
    end
    if k < numel(S)
      A = R{1};
    end
  end
  varargout = R;
end
%=============================================================================
