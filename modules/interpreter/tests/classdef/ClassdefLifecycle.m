%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefLifecycle < handle
  % Handle class whose destructor records "<Name>:<isvalid>" in a global log.
  properties
    Name = ''
    Child = []
  end

  methods
    function obj = ClassdefLifecycle(name)
      if nargin > 0
        obj.Name = name;
      end
    end

    function delete(obj)
      global CLASSDEF_LIFECYCLE_LOG
      CLASSDEF_LIFECYCLE_LOG{end + 1} = sprintf('%s:%d', obj.Name, isvalid(obj));
    end
  end
end
