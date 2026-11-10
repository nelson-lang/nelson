%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefHandleDotIndex < handle
  % handle with hidden subsref/subsasgn that records the subscript types it receives
  properties
    Data = struct();
    Types = {};
    ReadCount = 0;
    WriteCount = 0;
  end
  methods (Hidden)
    function varargout = subsref(obj, s)
      obj.Types = {s.type};
      obj.ReadCount = obj.ReadCount + 1;
      value = obj.Data.(s(1).subs);
      if numel(s) > 1
        value = subsref(value, s(2:end));
      end
      varargout{1} = value;
    end
    function obj = subsasgn(obj, s, value)
      obj.Types = {s.type};
      obj.WriteCount = obj.WriteCount + 1;
      name = s(1).subs;
      if numel(s) > 1
        if isfield(obj.Data, name)
          current = obj.Data.(name);
        else
          current = [];
        end
        value = subsasgn(current, s(2:end), value);
      end
      obj.Data.(name) = value;
    end
  end
  methods
    function t = receivedTypes(obj)
      t = obj.Types;
    end
    function n = reads(obj)
      n = obj.ReadCount;
    end
    function n = writes(obj)
      n = obj.WriteCount;
    end
  end
end
%=============================================================================
