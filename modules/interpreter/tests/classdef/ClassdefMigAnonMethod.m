%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefMigAnonMethod
  methods
    function r = pick(obj, names, target)
      % An anonymous function assigned in a non-constructor method must
      % not be misread as a misplaced x@Super(...) superclass call: the
      % text between '@' and '(' is empty here, which is not a superclass
      % name. Mirrors the '@(nm) find(strcmp({s.name}, nm), 1)' shape.
      finder = @(nm) find(strcmp(names, nm), 1);
      r = finder(target);
    end
  end
end
