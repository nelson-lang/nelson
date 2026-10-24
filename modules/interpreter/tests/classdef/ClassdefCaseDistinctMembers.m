%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefCaseDistinctMembers
  properties (Access = 'private')
    foo;
  end
  properties (Dependent)
    Foo;
  end
  methods
    function obj = ClassdefCaseDistinctMembers()
      obj.foo = 42;
    end
    function v = get.Foo(obj)
      v = obj.foo;
    end
  end
end
%=============================================================================
