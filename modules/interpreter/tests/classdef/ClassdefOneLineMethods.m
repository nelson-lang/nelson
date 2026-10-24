%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefOneLineMethods
  properties
    Value = 3
  end

  methods
    function r = value(obj), r = obj.Value + 1; end
  end

  methods (Static)
    function r = staticValue(), r = 9; end
  end

  methods (Access = private)
    function r = privateValue(obj), r = obj.Value + 2; end
  end
end
