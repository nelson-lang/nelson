%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Overloaded subsref/subsasgn on a class with a Hidden and a public property.
% A dot access from outside the class methods must reach the overloads, even
% when the name matches a declared property; its methods see the properties.
classdef ClassdefHiddenSubsref
  properties (Hidden)
    store
  end
  properties
    Pub
  end
  methods
    function obj = ClassdefHiddenSubsref()
      obj.store = 'internal';
      obj.Pub = 'public';
    end
    function R = subsref(obj, s)
      if strcmp(s(1).type, '.')
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
    function obj = writeStore(obj, v)
      obj.store = v;
    end
    function obj = writePub(obj, v)
      obj.Pub = v;
    end
  end
end
%=============================================================================
