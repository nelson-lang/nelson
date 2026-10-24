%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A method body that builds a matrix mixing string literals and function calls
% separated by whitespace (e.g. ['str' func(...)]). The classdef tokenizer must
% treat the space before the quote as a matrix-element separator, not a
% transpose, otherwise the class fails to register.
classdef ClassdefMethodStringMatrix
  properties
    v = [0 0 0]
  end
  methods
    function obj = ClassdefMethodStringMatrix(values)
      if nargin == 1
        obj.v = values;
      end
    end
    function s = render(obj)
      s = [num2str(obj.v(1)), ' << ' num2str(obj.v(2)) ', ' num2str(obj.v(3)) ' >>'];
    end
  end
end
%=============================================================================
