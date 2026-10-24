%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefGetSetMethodProperty < handle
  properties (Access = public)
    counter = 42;
  end
  properties (Access = private)
    secret = 7;
  end
  methods
    function s = get(obj, key)
      s = 999;
    end
    function set(obj, key, value)
    end
    function v = readCounter(obj)
      v = obj.counter;
    end
    function bumpSecret(obj)
      obj.secret = obj.secret + 1;
    end
    function v = readSecret(obj)
      v = obj.secret;
    end
  end
end
%=============================================================================
