%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Fixture: a container whose brace indexing yields a comma-separated list whose
% length is reported by numArgumentsFromSubscript.
classdef ClassdefNumArguments
  properties
    items
  end
  methods
    function obj = ClassdefNumArguments(c)
      obj.items = c;
    end
    function varargout = subsref(obj, s)
      if strcmp(s(1).type, '{}')
        idx = s(1).subs{1};
        for k = 1:numel(idx)
          varargout{k} = obj.items{idx(k)};
        end
        return
      end
      [varargout{1:nargout}] = builtin('subsref', obj, s);
    end
    function n = numArgumentsFromSubscript(obj, s, context)
      if strcmp(s(1).type, '{}')
        n = numel(s(1).subs{1});
      else
        n = 1;
      end
    end
  end
end
%=============================================================================
