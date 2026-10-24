%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefMigKeywordVars
  properties
    v = 0
  end
  methods
    function r = useKeywordsAsVars(obj)
      events = 42;
      methods = events + 1;
      properties = methods * 2;
      r = properties;
    end
    function r = callProperties(obj)
      r = properties(obj);
    end
  end
end
