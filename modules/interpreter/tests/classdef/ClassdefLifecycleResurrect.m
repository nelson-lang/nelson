%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefLifecycleResurrect < handle
  % Destructor that stores the object in a global: the handle stays invalid.
  properties
    Name = ''
  end

  methods
    function obj = ClassdefLifecycleResurrect(name)
      obj.Name = name;
    end

    function delete(obj)
      global CLASSDEF_LIFECYCLE_LOG
      global CLASSDEF_LIFECYCLE_RESURRECTED
      CLASSDEF_LIFECYCLE_RESURRECTED = obj;
      CLASSDEF_LIFECYCLE_LOG{end + 1} = ['resurrect:', obj.Name];
    end
  end
end
