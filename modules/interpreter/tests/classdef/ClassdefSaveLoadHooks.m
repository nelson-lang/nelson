%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefSaveLoadHooks
  properties
    Value = 0
    Label = 'default'
  end

  properties (Dependent)
    Twice
  end

  methods
    function obj = ClassdefSaveLoadHooks(value, label)
      if nargin > 0
        obj.Value = value;
      end
      if nargin > 1
        obj.Label = label;
      end
    end

    function value = get.Twice(obj)
      value = obj.Value * 2;
    end

    function data = saveObjectImpl(obj)
      data = struct('Value', obj.Value + 100, 'Label', [obj.Label, '_saved'], 'Twice', obj.Twice);
    end
  end

  methods (Static)
    function obj = loadObjectImpl(data)
      obj = ClassdefSaveLoadHooks(data.Value - 90, [data.Label, '_loaded']);
    end
  end
end
