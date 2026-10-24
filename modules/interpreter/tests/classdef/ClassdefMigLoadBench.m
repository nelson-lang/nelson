%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefMigLoadBench < handle
  % Representative shape for stressing the class-load pipeline: several
  % properties with defaults, get/set accessors, ordinary methods, a static
  % method and local functions. Loading it exercises the text scanner, the
  % per-method source rewrites, the accessor renaming and the re-parse of
  % every generated function -- exactly the machinery the migration replaces.
  properties
    A = 1
    B = [1 2 3]
    C = sort([3 1 2])
    Label = 'bench'
  end
  properties (Dependent)
    Sum
  end
  methods
    function obj = ClassdefMigLoadBench(a)
      if nargin > 0
        obj.A = a;
      end
    end
    function r = get.Sum(obj)
      r = obj.A + sum(obj.B);
    end
    function set.Label(obj, value)
      obj.Label = char(value);
    end
    function r = scaled(obj, k)
      r = mig_helper(obj.A, k);
    end
    function r = combine(obj, other)
      r = obj.A + other.A;
    end
  end
  methods (Static)
    function r = make(a)
      r = ClassdefMigLoadBench(a);
    end
  end
end
%=============================================================================
function r = mig_helper(a, k)
  r = a * k + 1;
end
%=============================================================================
