%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefLifecycleParent < handle
  % Destructor that reads a handle property: the child is still alive then.
  properties
    Name = ''
    Kid = []
  end

  methods
    function obj = ClassdefLifecycleParent(name)
      obj.Name = name;
    end

    function delete(obj)
      global CLASSDEF_LIFECYCLE_LOG
      if isempty(obj.Kid)
        CLASSDEF_LIFECYCLE_LOG{end + 1} = ['parent:', obj.Name, ':nokid'];
      else
        CLASSDEF_LIFECYCLE_LOG{end + 1} = sprintf('parent:%s:kid=%s:%d', obj.Name, obj.Kid.Name, isvalid(obj.Kid));
      end
    end
  end
end
