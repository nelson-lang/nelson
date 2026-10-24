%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef Item
  properties
    Value
  end
  methods
    function obj = Item()
      obj.Value = 8;
    end
  end
  methods (Static)
    function value = visible()
      value = 17;
    end
    function value = reveal()
      value = NcByteAccess.nested.Item.secret();
    end
  end
  methods (Static, Access = private)
    function value = secret()
      value = 999;
    end
  end
  methods (Static, Access = protected)
    function value = guarded()
      value = 777;
    end
  end
end
