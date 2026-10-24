%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefVarargoutUnset
  properties
    Tag
  end

  methods
    function obj = ClassdefVarargoutUnset()
      obj.Tag = 1;
    end

    % Declares 'varargout' but never assigns it. Calling with nargout == 0
    % must run without error (OUTPUT: none).
    function [varargout] = doit(obj)
      obj.Tag;
    end

    % Assigns varargout: returns a value when an output is requested.
    function [varargout] = give(obj)
      varargout{1} = obj.Tag + 41;
    end
  end
end
