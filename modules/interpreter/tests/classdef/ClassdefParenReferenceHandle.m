%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefParenReferenceHandle < handle
  properties
    Values = [10, 20, 30, 40]
    LastValue = 0
  end
  methods
    function obj = ClassdefParenReferenceHandle(values)
      if nargin > 0
        obj.Values = values;
      end
    end
    function value = parenReference(obj, indices)
      if numel(indices) == 1
        values = obj.Values;
        value = values(indices{1});
      else
        error('Unexpected parenReference.');
      end
    end
    function parenAssign(obj, indices, value)
      if numel(indices) == 1
        values = obj.Values;
        values(indices{1}) = value;
        obj.Values = values;
        obj.LastValue = value;
      else
        error('Unexpected parenAssign.');
      end
    end
    function delete(obj)
      if isvalid(obj)
        value = obj(2);
        if value ~= obj.Values(2)
          error('Unexpected delete parenReference.');
        end
      end
    end
  end
end
