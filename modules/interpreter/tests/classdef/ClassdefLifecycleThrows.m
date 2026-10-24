%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefLifecycleThrows < handle
  % Destructor that errors: the teardown must be contained and continue.
  properties
    Name = ''
  end

  methods
    function obj = ClassdefLifecycleThrows(name)
      obj.Name = name;
    end

    function delete(obj)
      global CLASSDEF_LIFECYCLE_LOG
      CLASSDEF_LIFECYCLE_LOG{end + 1} = ['throws:', obj.Name];
      error('ClassdefLifecycle:boom', 'boom in delete');
    end
  end
end
