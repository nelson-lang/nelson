%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefLocalfunctionsFixture
  methods
    function handles = localHandles(obj)
      handles = localfunctions();
    end

    function value = callFirstLocal(obj, inputValue)
      handles = localfunctions();
      value = handles{1}(inputValue);
    end
  end

  methods (Static)
    function names = staticLocalNames()
      handles = localfunctions();
      names = {func2str(handles{1}); func2str(handles{2})};
    end
  end
end
%=============================================================================
function value = classdef_local_one(inputValue)
  value = inputValue + 10;
end
%=============================================================================
function value = classdef_local_two(inputValue)
  value = inputValue * 5;
end
%=============================================================================
