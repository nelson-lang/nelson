%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef WeakReference < handle
  properties (Access = private)
    Storage
  end

  properties (Dependent)
    Handle
  end

  properties (Dependent, SetAccess = private)
    ValidHandle
  end

  methods
    function obj = WeakReference(handleObject)
      obj.Storage = __classdef_weak_reference_new__();
      if nargin > 0
        obj.Handle = handleObject;
      end
    end

    function value = get.Handle(obj)
      value = __classdef_weak_reference_get__(obj.Storage);
    end

    function set.Handle(obj, value)
      __classdef_weak_reference_set__(obj.Storage, value);
    end

    function value = get.ValidHandle(obj)
      value = __classdef_weak_reference_valid_get__(obj.Storage);
    end
  end
end
