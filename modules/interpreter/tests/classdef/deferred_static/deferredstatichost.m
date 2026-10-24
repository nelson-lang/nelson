%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A lowercase-named class whose constructor refuses a zero-argument call. It is
% used to check that ClassName.staticMethod(...) dispatches to the static method
% at script top level / immediate mode. If the call were lowered to a construct
% then index, the guarded constructor would raise instead of returning a value.
classdef deferredstatichost
  properties
    value
  end
  methods
    function obj = deferredstatichost(x)
      if nargin < 1
        error('deferredstatichost:noArgCtor', ...
          'deferredstatichost must not be default-constructed');
      end
      obj.value = x;
    end
  end
  methods (Static = true)
    function r = scale(x)
      r = x * 10;
    end
  end
end
%=============================================================================
