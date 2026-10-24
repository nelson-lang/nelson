%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Subclass inheriting the overloaded subsref/subsasgn of ClassdefHiddenSubsref:
% its own methods also see the inherited properties directly.
classdef ClassdefHiddenSubsrefChild < ClassdefHiddenSubsref
  methods
    function obj = ClassdefHiddenSubsrefChild()
      obj@ClassdefHiddenSubsref();
    end
    function R = childReadStore(obj)
      R = obj.store;
    end
    function obj = childWriteStore(obj, v)
      obj.store = v;
    end
  end
end
%=============================================================================
