%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Value class whose property has a set method that rejects the empty default.
% Construction must apply the (implicit []) default without calling the set
% method, otherwise the assert below would abort the constructor.
classdef ClassdefSetterGuardedBase
  properties (SetAccess = protected, GetAccess = public)
    s
  end
  methods
    function q = ClassdefSetterGuardedBase(value)
      if nargin == 1
        q.s = value;
      else
        q.s = 0;
      end
    end
    function q = set.s(q, value)
      assert(isreal(value) && isscalar(value), 'ClassdefSetterGuarded:badarg', 's must be real scalar');
      q.s = value;
    end
  end
end
%=============================================================================
