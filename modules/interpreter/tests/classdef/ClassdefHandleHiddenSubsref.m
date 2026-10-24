%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Handle class overloading subsref/subsasgn, with a Hidden and a public
% property. A dot access from outside the class methods must reach the
% overloads, even when the name matches a declared property; its methods see
% the properties.
classdef ClassdefHandleHiddenSubsref < handle
  properties (Hidden)
    store = 'internal'
  end
  properties
    Pub = 'public'
    Inner
  end
  properties (Dependent)
    InnerKeys
  end
  methods
    function obj = ClassdefHandleHiddenSubsref()
      obj.Inner = containers.Map({'k1', 'k2'}, {1, 2});
    end
    function R = get.InnerKeys(obj)
      % keys must dispatch to the containers.Map method
      R = keys(obj.Inner);
    end
    function R = subsref(obj, s)
      if strcmp(s(1).type, '.') && ~strcmp(s(1).subs, 'InnerKeys')
        R = ['subsref:', s(1).subs];
      else
        R = builtin('subsref', obj, s);
      end
    end
    function obj = subsasgn(obj, s, v)
      if strcmp(s(1).type, '.')
        obj.Pub = ['subsasgn:', s(1).subs, '=', v];
      else
        obj = builtin('subsasgn', obj, s, v);
      end
    end
    function R = readStore(obj)
      R = obj.store;
    end
    function R = readStoreDynamic(obj)
      R = obj.('store');
    end
    function R = readPub(obj)
      R = obj.Pub;
    end
    function writeStore(obj, v)
      obj.store = v;
    end
    function writePub(obj, v)
      obj.Pub = v;
    end
    function R = readViaBuiltin(obj, name)
      R = builtin('subsref', obj, struct('type', '.', 'subs', name));
    end
    function obj = writeViaBuiltin(obj, name, v)
      obj = builtin('subsasgn', obj, struct('type', '.', 'subs', name), v);
    end
  end
end
%=============================================================================
