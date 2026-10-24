%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefIndexingHooks
  methods
    function varargout = subsref(obj, S)
      values = [4, 5, 6, 7];
      if strcmp(S(1).type, '()')
        indices = S(1).subs;
        value = values(indices{:});
      elseif strcmp(S(1).type, '.') && strcmp(S(1).subs, 'first')
        value = values(1);
      else
        error('Unexpected subsref.');
      end
      if numel(S) > 1
        value = subsref(value, S(2:end));
      end
      varargout{1} = value;
    end
    function obj = subsasgn(obj, S, value)
      if strcmp(S(1).type, '()')
        if ~isequal(S(1).subs, {2}) || ~isequal(value, 50)
          error('Unexpected subsasgn.');
        end
      elseif strcmp(S(1).type, '.') && strcmp(S(1).subs, 'first')
        if ~isequal(value, 40)
          error('Unexpected subsasgn.');
        end
      else
        error('Unexpected subsasgn.');
      end
    end
    function value = end(obj, index, count)
      if count == 1
        value = 4;
      else
        value = size([4, 5, 6, 7], index);
      end
    end
  end
end
